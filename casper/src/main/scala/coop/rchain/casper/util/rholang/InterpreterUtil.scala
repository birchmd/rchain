package coop.rchain.casper.util.rholang

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import cats.Id
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.{trace, Checkpoint}
import monix.execution.Scheduler
import scodec.Codec
import coop.rchain.shared.AttemptOps._
import scodec.bits.BitVector

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

import RuntimeManager.DeployError

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint(b: BlockMessage,
                              genesis: BlockMessage,
                              dag: BlockDag,
                              internalMap: Map[BlockHash, BlockMessage],
                              emptyStateHash: StateHash,
                              knownStateHashes: Set[StateHash],
                              runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (Option[StateHash], Set[StateHash]) = {
    val tsHash        = ProtoUtil.tuplespace(b)
    val serializedLog = b.body.fold(Seq.empty[Event])(_.commReductions)
    val log           = serializedLog.map(EventConverter.toRspaceEvent).toList
    val attempt = Try(
      computeBlockCheckpointFromDeploys(b,
                                        genesis,
                                        dag,
                                        internalMap,
                                        emptyStateHash,
                                        knownStateHashes,
                                        runtimeManager.replayComputeState(log))
    )
    val (computedCheckpoint, updatedStateHashes, cost) = attempt match {
      case Success(x) => x
      case Failure(ex) =>
        println("Bad log:")
        log.foreach(println)
        println("---------------")
        throw new Exception("Uhohs!")
    }
    val computedStateHash = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
    if (tsHash.contains(computedStateHash)) {
      // state hash in block matches computed hash!
      Some(computedStateHash) -> updatedStateHashes
    } else {
      // state hash in block does not match computed hash -- invalid!
      // return no state hash, do not update the state hash set
      None -> knownStateHashes
    }
  }

  def computeDeploysCheckpoint(
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      genesis: BlockMessage,
      dag: BlockDag,
      internalMap: Map[BlockHash, BlockMessage],
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : (Checkpoint, Set[StateHash], Vector[DeployCost]) = {
    //TODO: Revisit how the deployment cost should be handled for multiparent blocks
    //for time being we ignore the `postStateCost`
    val (postStateHash, updatedStateHashes, postStateCost) =
      computeParentsPostState(parents,
                              genesis,
                              dag,
                              internalMap,
                              emptyStateHash,
                              knownStateHashes,
                              computeState)

    val Right((postDeploysCheckpoint, deployCost)) = computeState(postStateHash, deploys)
    val postDeploysStateHash                       = ByteString.copyFrom(postDeploysCheckpoint.root.bytes.toArray)
    (postDeploysCheckpoint, updatedStateHashes + postDeploysStateHash, deployCost)
  }

  private def computeParentsPostState(
      parents: Seq[BlockMessage],
      genesis: BlockMessage,
      dag: BlockDag,
      internalMap: Map[BlockHash, BlockMessage],
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : (StateHash, Set[StateHash], Vector[DeployCost]) = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (emptyStateHash, knownStateHashes, Vector.empty)
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      assert(
        knownStateHashes.contains(parentStateHash),
        "We should have already computed parent state hash when we added the parent to our blockDAG.")
      (parentStateHash, knownStateHashes, Vector.empty)
    } else {
      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all histories since the greatest
      //common ancestor in order to reach the current
      //state.
      val gca =
        parentTuplespaces
          .map(_._1)
          .reduce(DagOperations.greatestCommonAncestor(_, _, genesis, dag, internalMap))

      val gcaStateHash = ProtoUtil.tuplespace(gca).get
      assert(
        knownStateHashes.contains(gcaStateHash),
        "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

      // TODO: Fix so that all search branches reach GCA before quitting
      val deploys = DagOperations
        .bfTraverse[BlockMessage](parentTuplespaces.map(_._1))(
          ProtoUtil.parents(_).iterator.map(internalMap.apply))
        .takeWhile(_ != gca)
        .flatMap(ProtoUtil.deploys(_).reverse)
        .toIndexedSeq
        .reverse

      //TODO: figure out what casper should do with errors in deploys
      val Right((resultStateCheckpoint, deployCost)) =
        computeState(gcaStateHash, deploys)
      val resultStateHash = ByteString.copyFrom(resultStateCheckpoint.root.bytes.toArray)
      (resultStateHash, knownStateHashes + resultStateHash, deployCost)
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      internalMap: Map[BlockHash, BlockMessage],
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : (Checkpoint, Set[StateHash], Vector[DeployCost]) = {
    val parents = ProtoUtil
      .parents(b)
      .map(internalMap.apply)

    val deploys = ProtoUtil.deploys(b)

    assert(parents.nonEmpty || (parents.isEmpty && b == genesis),
           "Received a different genesis block.")

    computeDeploysCheckpoint(
      parents,
      deploys,
      genesis,
      dag,
      internalMap,
      emptyStateHash,
      knownStateHashes,
      computeState
    )
  }
}
