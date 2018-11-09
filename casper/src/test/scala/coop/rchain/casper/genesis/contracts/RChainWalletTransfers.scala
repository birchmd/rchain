package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath
import java.io.PrintWriter
import java.nio.file.Files
import monix.execution.Scheduler.Implicits.global
import Wallet._

object RChainWalletTransfers {

  //give a simple address for a wallet
  def depositAddress(wallet: Wallet): String = "0x" + wallet.pk

  //generate the rholang code for accepting purses at an address
  def depositForwarder(wallet: Wallet): String = {
    val address = depositAddress(wallet)
    s"""
    contract @"$address"(@purse) = {
      new purseAmountCh, walletCh, return(`rho:io:stdout`), rl(`rho:registry:lookup`) in {
        @purse!("getBalance", *purseAmountCh) |
        rl!(`${rhoLookupId(wallet)}`, *walletCh) |
        for(@(_, wallet) <- walletCh; @amount <- purseAmountCh) {
          @wallet!("deposit", amount, purse, *return)
        }
      }
    }
    """
  }

  //generate rholang code for transferring rev between wallets
  def transfer(from: Wallet, to: Wallet, amount: Int, nonce: Int, sk: Array[Byte])(
      implicit rm: RuntimeManager
  ): String = {
    val destName = depositAddress(to)
    val deploy = ProtoUtil.termDeployNow(
      mkTerm(s""" @"__SCALA__"!([$nonce, $amount, "$destName"].toByteArray()) """).right.get
    )
    val sigData = rm.captureResults(rm.emptyStateHash, deploy).head.exprs.head.getGByteArray
    val sig     = Base16.encode(Ed25519.sign(Blake2b256.hash(sigData.toByteArray), sk))

    s"""new walletCh, status(`rho:io:stdout`), rl(`rho:registry:lookup`) in {
       |  rl!(`${rhoLookupId(from)}`, *walletCh) |
       |  for(@(_, wallet) <- walletCh) {
       |    @wallet!("transfer", $amount, $nonce, "$sig", "$destName", *status)
       |  }
       |}""".stripMargin
  }

  def writeToFile(code: String, name: String): Unit = {
    val out = new PrintWriter(s"./$name.rho")
    out.println(code)
    out.close()
  }

  def main(args: Array[String]): Unit = {
    //main method to actually generate the rholang code
    val runtime                 = TestSetUtil.runtime
    implicit val runtimeManager = RuntimeManager.fromRuntime(runtime)

    val n: Int     = args.head.toInt //number of wallets to make
    val (sks, pks) = (1 to n).map(_ => Ed25519.newKeyPair).unzip
    val wallets    = pks.map(pk => Wallet("ed25519", Base16.encode(pk), 100))
    val rev = new Rev[Wallet](
      Wallet.rhoCode,
      wallets,
      Faucet.noopFaucet,
      ProofOfStakeParams(1L, Long.MaxValue, List(ProofOfStakeValidator(Array.empty[Byte], 0L)))
    )
    val depositForwarders = wallets.map(depositForwarder)
    val transfers = { //set up transfers between wallets; could be modified to do more txns
      val wsks = wallets.zip(sks)
      wsks.zip(wallets.tail).map {
        case ((from, sk), to) => transfer(from, to, 11, 0, sk)
      }
    }

    //Note that this generated code assumes NonNegativeNumber.rho, MakeMint.rho and BasicWallet.rho
    //are already deployed. This is is the case for node instances because those contracts are in
    //the genesis block.
    val walletSeupCode = (rev.code +: depositForwarders).mkString(" |\n")
    val transferCode   = transfers.mkString(" |\n")

    writeToFile(walletSeupCode, "walletSeup")
    writeToFile(transferCode, "transfers")

    runtime.close()
  }

}
