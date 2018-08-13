package coop.rchain.casper.helper

import cats.Monad
import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.{
  MultiParentCasper,
  MultiParentCasperConstructor,
  SafetyOracle,
  ValidatorIdentity
}
import coop.rchain.catscontrib._
import coop.rchain.comm._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.{Connect, HandleMessages}
import Connect._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.shared.Cell
import java.nio.file.Files

import coop.rchain.casper.util.rholang.RuntimeManager
import monix.execution.Scheduler
import monix.eval.Task
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.collection.mutable
import coop.rchain.shared.PathOps.RichPath
import scala.util.Random

trait AntiMonad[F[_]] {
  def extract[A](fa: F[A]): A
}
object AntiMonad {
  def apply[F[_]](implicit instance: AntiMonad[F]): AntiMonad[F] = instance
}

class EffectNode[F[_]: Monad: Capture: Sync: ErrorHandler: AntiMonad](
    val local: PeerNode,
    msgQueuesF: Ref[F, Map[PeerNode, mutable.Queue[Protocol]]],
    val genesis: BlockMessage,
    sk: Array[Byte],
    storageSize: Long = 1024L * 1024 * 10)(implicit scheduler: Scheduler) {

  val blockStoreDir              = BlockStoreTestFixture.dbDir
  val runtimeDir                 = BlockStoreTestFixture.dbDir
  implicit val logEff            = new LogStub[F]
  implicit val timeEff           = new LogicalTime[F]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[F]()
  implicit val transportLayerEff = new TransportLayerTestImpl[F](local, msgQueuesF)
  implicit val metricEff         = new Metrics.MetricsNOP[F]
  implicit val blockStoreEff =
    LMDBBlockStore.create[F](LMDBBlockStore.Config(blockStoreDir, storageSize))
  implicit val turanOracleEffect = SafetyOracle.turanOracle[F]
  implicit val rpConfAsk         = createRPConfAsk[F](local)

  val activeRuntime  = Runtime.create(runtimeDir, storageSize)
  val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
  val validatorId    = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

  def tearDown(): Unit = {
    activeRuntime.close()
    blockStoreEff.close()
    runtimeDir.recursivelyDelete()
    blockStoreDir.recursivelyDelete()
  }

  implicit val connectionsCell = Cell.const[F, Connections](Connect.Connections.empty)
  private val defaultTimeout   = FiniteDuration(1000, MILLISECONDS)

  val casperEff = AntiMonad[F].extract(
    for {
      _        <- blockStoreEff.put(genesis.blockHash, genesis)
      blockMap <- blockStoreEff.asMap()
    } yield
      MultiParentCasper
        .hashSetCasper[F](runtimeManager, Some(validatorId), genesis, blockMap)
  )

  implicit val constructor = MultiParentCasperConstructor.successCasperConstructor[F](
    ApprovedBlock(candidate = Some(ApprovedBlockCandidate(block = Some(genesis)))),
    casperEff)

  implicit val packetHandler = PacketHandler.pf[F](casperPacketHandler[F])

  def receive(): F[Unit] =
    transportLayerEff.receive(p => HandleMessages.handle[F](p, defaultTimeout))
}

object EffectNode {
  type Effect[A] = EitherT[Task, CommError, A]

  def standalone[F[_]: Monad: Capture: Sync: ErrorHandler: AntiMonad](
      sk: Array[Byte],
      genesis: BlockMessage)(implicit scheduler: Scheduler): EffectNode[F] = {
    val local     = HashSetCasperTestNode.peerNode("taskNode", 40400)
    val msgQueues = Map.empty[PeerNode, mutable.Queue[Protocol]]

    new EffectNode[F](local, Ref.unsafe(msgQueues), genesis, sk)
  }

  def network[F[_]: Monad: Capture: Sync: ErrorHandler: AntiMonad](
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage)(implicit scheduler: Scheduler): F[List[EffectNode[F]]] = {
    val n     = sks.length
    val names = (1 to n).map(i => s"node-$i")
    val peers = names.map(HashSetCasperTestNode.peerNode(_, 40400))
    val msgQueuesF = Ref.unsafe[F, Map[PeerNode, mutable.Queue[Protocol]]](
      peers.map(_ -> new mutable.Queue[Protocol]()).toMap)

    val nodes =
      peers.zip(sks).map {
        case (p, sk) =>
          new EffectNode[F](p, msgQueuesF, genesis, sk)
      }

    //make sure all nodes know about each other
    val peerPairs = (for {
      n <- nodes
      m <- nodes
      if n.local != m.local
    } yield (n, m)).toList

    peerPairs
      .traverse {
        case (n, m) => n.nodeDiscoveryEff.addNode(m.local)
      }
      .map(_ => nodes.toList)
  }

  def antiMonadEffectInstance(implicit scheduler: Scheduler) = new AntiMonad[Effect] {
    def extract[A](fa: Effect[A]): A = fa.value.unsafeRunSync.right.get
  }

  val syncEffectInstance = SyncInstances.syncEffect[CommError](commError => {
    new Exception(s"CommError: $commError")
  }, e => { UnknownCommError(e.getMessage) })
}
