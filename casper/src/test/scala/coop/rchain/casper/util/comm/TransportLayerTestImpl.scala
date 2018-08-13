package coop.rchain.casper.util.comm

import cats.{Id, Monad}
import cats.effect.concurrent.Ref
import cats.implicits._

import coop.rchain.comm.protocol.routing._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.{peerNodeNotFound, CommErr}
import coop.rchain.comm.PeerNode

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable
import coop.rchain.comm.transport._

class TransportLayerTestImpl[F[_]: Monad: Capture](
    identity: PeerNode,
    val msgQueuesF: Ref[F, Map[PeerNode, mutable.Queue[Protocol]]])
    extends TransportLayer[F] {

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]] = ???

  def send(peer: PeerNode, msg: Protocol): F[Unit] =
    msgQueuesF.update(msgQueues => {
      val _ = msgQueues.get(peer).foreach(_.enqueue(msg))
      msgQueues
    })

  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Unit] =
    peers.toList.traverse(send(_, msg)).void

  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit] =
    for {
      maybeProto <- msgQueuesF.modify(msgQueues => {
                     val mp = TransportLayerTestImpl.dequeueOption(msgQueues(identity))
                     msgQueues -> mp
                   })
      _ <- maybeProto.fold(().pure[F]) { proto =>
            dispatch(proto).flatMap(_ => receive(dispatch))
          }
    } yield ()

  def disconnect(peer: PeerNode): F[Unit] = ???

  def shutdown(msg: Protocol): F[Unit] = ???
}

object TransportLayerTestImpl {
  def dequeueOption[A](q: mutable.Queue[A]): Option[A] =
    if (q.nonEmpty) q.dequeue().some
    else none[A]
}
