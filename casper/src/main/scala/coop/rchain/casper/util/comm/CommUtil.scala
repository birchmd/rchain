package coop.rchain.casper.util.comm

import cats.Monad
import cats.effect.Timer
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Capture
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp._
import coop.rchain.comm.transport.CommMessages.{packet, toPacket}
import coop.rchain.comm.transport.{PacketType, TransportLayer}
import coop.rchain.comm.{transport, PeerNode, ProtocolHelper}
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects._
import coop.rchain.shared._

import scala.concurrent.duration._
import scala.util.Try

object CommUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def sendBlock[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      b: BlockMessage): F[Unit] = {
    val serializedBlock = b.toByteString
    val hashString      = PrettyPrinter.buildString(b.blockHash)
    for {
      _ <- Log[F].info(s"CASPER: Beginning send of ${PrettyPrinter.buildString(b)} to peers...")
      _ <- sendToPeers[F](transport.BlockMessage, serializedBlock)
      _ <- Log[F].info(s"CASPER: Sent $hashString to peers")
    } yield ()
  }

  def sendBlockRequest[
      F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      r: BlockRequest): F[Unit] = {
    val serialized = r.toByteString
    val hashString = PrettyPrinter.buildString(r.hash)
    for {
      _ <- Log[F].info(s"CASPER: Beginning request of missing block $hashString from peers...")
      _ <- sendToPeers[F](transport.BlockRequest, serialized)
      _ <- Log[F].info(s"CASPER: Requested $hashString from peers")
    } yield ()
  }

  def sendToPeers[F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: RPConfAsk](
      pType: PacketType,
      serializedMessage: ByteString): F[Unit] =
    for {
      peers       <- NodeDiscovery[F].peers
      local       <- RPConfAsk[F].reader(_.local)
      currentTime <- Time[F].currentMillis
      msg         = packet(local, pType, serializedMessage, currentTime)
      _           <- TransportLayer[F].broadcast(peers, msg)
    } yield ()

  def requestApprovedBlock[
      F[_]: Monad: Capture: LastApprovedBlock: Log: Time: Timer: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: PacketHandler: RPConfAsk](
      delay: FiniteDuration): F[Unit] = {
    val request = ApprovedBlockRequest("PleaseSendMeAnApprovedBlock").toByteString

    def askPeers(peers: List[PeerNode], local: PeerNode): F[Unit] = peers match {
      case peer :: rest =>
        for {
          _           <- Log[F].info(s"CASPER: Sending request for ApprovedBlock to $peer")
          currentTime <- Time[F].currentMillis
          send <- TransportLayer[F]
                   .roundTrip(peer,
                              packet(local, transport.ApprovedBlockRequest, request, currentTime),
                              5.seconds)
          _ <- send match {
                case Left(err) =>
                  Log[F].info(s"CASPER: Failed to get response from $peer because: $err") *>
                    askPeers(rest, local)

                case Right(response) =>
                  Log[F]
                    .info(s"CASPER: Received response from $peer! Processing...")
                    .flatMap(_ => {
                      val maybeSender = ProtocolHelper.sender(response)
                      val maybePacket = toPacket(response).toOption

                      (maybeSender, maybePacket) match {
                        case (Some(sender), Some(_)) =>
                          for {
                            _ <- HandleMessages.handlePacket[F](sender, maybePacket)
                            l <- LastApprovedBlock[F].get
                            _ <- l.fold(askPeers(rest, local))(_ => ().pure[F])
                          } yield ()
                        case (None, _) =>
                          Log[F].error(
                            s"CASPER: Response from $peer invalid. The sender of the message could not be determined.") *> askPeers(
                            rest,
                            local)
                        case (Some(_), None) =>
                          Log[F].error(
                            s"CASPER: Response from $peer invalid. A packet was expected, but received ${response.message}.") *> askPeers(
                            rest,
                            local)
                      }
                    })

              }
        } yield ()

      case Nil => Timer[F].sleep(delay) >> requestApprovedBlock[F](delay)
    }

    for {
      a     <- LastApprovedBlock[F].get
      _     = println(s"LastApprovedBlock is $a")
      peers <- NodeDiscovery[F].peers
      _     = println(s"Peers: $peers")
      local <- RPConfAsk[F].reader(_.local)
      _     <- a.fold(askPeers(peers.toList, local))(_ => ().pure[F])
    } yield ()
  }
}
