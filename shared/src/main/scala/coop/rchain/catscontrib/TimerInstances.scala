package coop.rchain.catscontrib

import cats.Functor
import cats.data.EitherT
import cats.effect.Timer
import cats.implicits._

import scala.concurrent.duration._

trait TimerInstances {
  def eitherTTimer[F[_]: Functor, B](implicit timerF: Timer[F]): Timer[EitherT[F, B, ?]] = {
    type Effect[A] = EitherT[F, B, A]

    new Timer[Effect] {
      override def clockRealTime(unit: TimeUnit): Effect[Long] =
        EitherT.liftF(timerF.clockRealTime(unit))
      override def clockMonotonic(unit: TimeUnit): Effect[Long] =
        EitherT.liftF(timerF.clockMonotonic(unit))
      override def sleep(duration: FiniteDuration): Effect[Unit] =
        EitherT.liftF(timerF.sleep(duration))
      override def shift: Effect[Unit] = EitherT.liftF(timerF.shift)
    }
  }

}
