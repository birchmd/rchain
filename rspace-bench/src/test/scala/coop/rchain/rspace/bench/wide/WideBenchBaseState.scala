package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rholang.interpreter.ChargingReducer
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.StoreType
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

abstract class WideBenchBaseState {
  val rhoSetupScriptPath: String = "/casper/walletSeup.rho"
  val rhoScriptSource: String    = "/casper/transfers.rho"

  implicit val scheduler: Scheduler = Scheduler.fixedPool(name = "wide-1", poolSize = 100)
  lazy val dbDir: Path              = Files.createTempDirectory(BenchStorageDirPrefix)
  val mapSize: Long                 = 1024L * 1024L * 1024L * 10L

  var runtime: Runtime       = null
  var setupTerm: Option[Par] = None
  var term: Option[Par]      = None

  var runTask: Task[Vector[Throwable]] = null

  implicit def readErrors = () => runtime.readAndClearErrorVector()

  def createRuntime(): Runtime = Runtime.create(dbDir, mapSize)

  private def evalFile(path: String, runtime: Runtime)(
      implicit rand: Blake2b512Random
  ): Task[Unit] =
    for {
      term <- Interpreter.buildNormalizedTerm(resourceFileReader(path)).task
      _    <- runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE))
      _    <- runtime.replayReducer.setAvailablePhlos(Cost(Integer.MAX_VALUE))
      _    <- runtime.reducer.inj(term)
    } yield ()

  @Setup(value = Level.Iteration)
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)
    setupTerm =
      Interpreter.buildNormalizedTerm(resourceFileReader(rhoSetupScriptPath)).runAttempt match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }

    term = Interpreter.buildNormalizedTerm(resourceFileReader(rhoScriptSource)).runAttempt match {
      case Right(par) => Some(par)
      case Left(err)  => throw err
    }
    runtime = createRuntime()
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    runtime.replayReducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)

    implicit val rand = Blake2b512Random(128)
    (for {
      emptyCheckpoint <- runtime.space.createCheckpoint()
      //make sure we always start from clean rspace & trie
      _ <- runtime.replaySpace.clear()
      _ <- runtime.replaySpace.reset(emptyCheckpoint.root)
      _ <- runtime.space.clear()
      _ <- runtime.space.reset(emptyCheckpoint.root)
      _ <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
      _ <- evalFile("casper/src/main/rholang/NonNegativeNumber.rho", runtime)
      _ <- evalFile("casper/src/main/rholang/MakeMint.rho", runtime)
      _ <- evalFile("casper/src/main/rholang/BasicWallet.rho", runtime)
      _ <- evalFile("casper/src/main/rholang/WalletCheck.rho", runtime)
      _ <- evalFile("casper/src/main/rholang/SystemInstancesRegistry.rho", runtime)
      _ <- evalFile("casper/src/main/rholang/MakePoS.rho", runtime)
      _ <- evalFile("casper/walletSeup.rho", runtime)
    } yield ()).unsafeRunSync
  }

  @TearDown
  def tearDown(): Unit =
    runtime.close().unsafeRunSync
}
