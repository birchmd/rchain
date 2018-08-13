package coop.rchain.casper

import cats.data.EitherT
import cats.{Id, Monad}
import cats.implicits._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.casper.helper.{EffectNode, HashSetCasperTestNode}
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm.CommError
import coop.rchain.models.Par
import monix.execution.Scheduler
import monix.eval.Task
import scala.concurrent.duration._
import cats.effect.{IO, LiftIO, Timer}

/* USAGE:
 * import coop.rchain.catscontrib.TaskContrib._
 * import coop.rchain.casper.Rhol591Bug
 * val rhol591 = new Rhol591Bug
 * import rhol591._
 * val testRun = testProgram.unsafeRunSync
 */

class Rhol591Bug {
  type Effect[A] = EitherT[Task, CommError, A]

  implicit val scheduler               = Scheduler.io("test")
  implicit val antiMonadEffectInstance = EffectNode.antiMonadEffectInstance
  implicit val syncEffectInstance      = EffectNode.syncEffectInstance
  implicit val errorHandlerId          = HashSetCasperTestNode.errorHandler
  implicit val liftIOEffect = new LiftIO[Effect] {
    def liftIO[A](ioa: IO[A]): Effect[A] =
      EitherT.liftF[Task, CommError, A](LiftIO[Task].liftIO(ioa))
  }
  implicit val timerEff                        = Timer.derive[Effect]
  val (validatorKeys, validators)              = (1 to 2).map(_ => Ed25519.newKeyPair).unzip
  val genesis                                  = HashSetCasperTest.createGenesis(validators)
  val nodesF: Effect[List[EffectNode[Effect]]] = EffectNode.network[Effect](validatorKeys, genesis)
  val nodes: List[EffectNode[Effect]]          = nodesF.value.unsafeRunSync.right.get
  //val term                                     = mkTerm(scala.io.Source.fromFile("./rholang/examples/tut-hello.rho").mkString).right.get
  val term0 = mkTerm("@0!(0) | for(@x <- @0){ @0!(x) }").right.get
  val term1 = mkTerm("@1!(1)").right.get

  def repDeployPropose[F[_]: Monad: Timer](base: EffectNode[F], term: Par, n: Int = 10): F[Unit] =
    if (n == 0) ().pure[F]
    else {
      val casper = base.casperEff
      for {
        _     <- casper.deploy(ProtoUtil.termDeploy(term))
        block <- casper.createBlock.map(_.get)
        _     <- casper.addBlock(block)
        _     <- Timer[F].sleep(200.milliseconds)
        _     <- repDeployPropose[F](base, term, n - 1)
      } yield ()
    }

  def receiveAll[F[_]: Monad: Timer](nodes: List[EffectNode[F]]) = MonadOps.forever(
    nodes.traverse(_.receive).void.flatMap(_ => Timer[F].sleep(100.milliseconds))
  )

  val testProgram = for {
    recFiber <- receiveAll[Effect](nodes).value.fork
    f0       <- repDeployPropose[Effect](nodes(0), term0).value.fork
    f1       <- repDeployPropose[Effect](nodes(1), term1).value.fork
    _        <- f0.join
    _        <- f1.join
    _        <- recFiber.cancel
    _        <- Task.delay { println("testProgram done!") }
  } yield ()
}
