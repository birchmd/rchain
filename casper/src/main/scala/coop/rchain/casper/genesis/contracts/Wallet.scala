package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rholang.interpreter.Registry.buildURI

import scala.util.{Failure, Success, Try}

case class Wallet(algorithm: String, pk: String, initRevBalance: Int)

object Wallet {
  def rhoLookupId(w: Wallet): String = {
    val keyBytes     = Base16.decode(w.pk)
    val hashKeyBytes = Blake2b256.hash(keyBytes)
    buildURI(hashKeyBytes)
  }

  /**
    * Produces Rholang code which adds a wallet to the blockchain based on the
    * given Wallet case class.
    * @param w the Wallet object containing the information which will go
    *          on the blockchain.
    * @return  Rholang code to add the wallet to the blockchain.
    */
  def rhoCode(w: Wallet): String = s"""
    |new purseCh, walletCh, rs(`rho:registry:insertSigned:ed25519`) in {
    |  @revMint!("makePurse", ${w.initRevBalance}, *purseCh) |
    |  for(@purse <- purseCh) {
    |    @BasicWallet!(purse, "${w.algorithm}", "${w.pk}", *walletCh) |
    |    for(@maybeWallet <- walletCh) {
    |      match maybeWallet {
    |        [wallet] => { rs!("${w.pk}".hexToBytes(), (9223372036854775807, wallet), "abc".hexToBytes(), Nil) }
    |        _        => { rs!("${w.pk}".hexToBytes(), (0, "Error during wallet creation!"), "abc".hexToBytes(), Nil) }
    |      }
    |    }
    |  }
    |}""".stripMargin

  def fromLine(line: String): Either[String, Wallet] = line.split(" ").filter(_.nonEmpty) match {
    case Array(algorithm, pk, initRevBalanceStr) =>
      Try(initRevBalanceStr.toInt) match {
        case Success(initRevBalance) => Right(Wallet(algorithm, pk, initRevBalance))
        case Failure(_) =>
          Left(s"Failed to parse given initial balance $initRevBalanceStr as int.")
      }

    case _ => Left(s"Invalid wallet specification:\n$line")
  }
}
