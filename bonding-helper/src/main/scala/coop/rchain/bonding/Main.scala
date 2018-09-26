package coop.rchain.bonding

import coop.rchain.casper.protocol.{Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath

import java.io.PrintWriter
import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global

object Main {

  def bondingForwarderDeploy(bondKey: String, ethAddress: String): String = {
    val bondingStatusOut        = s"${ethAddress}_bondingOut"
    val bondingForwarderAddress = s"${ethAddress}_bondingForwarder"
    s"""for(@purse <- @"$bondingForwarderAddress"; @pos <- @"proofOfStake"){
       |  @(pos, "bond")!("$bondKey".hexToBytes(), "ed25519Verify", purse, "$ethAddress", "$bondingStatusOut")
       |}""".stripMargin
  }

  def unlockDeploy(ethAddress: String, pubKey: String, secKey: String)(
      implicit runtimeManager: RuntimeManager
  ): String =
    preWalletUnlockDeploy(ethAddress, pubKey, Base16.decode(secKey), s"${ethAddress}_unlockOut").term

  def bondDeploy(amount: Long, ethAddress: String, pubKey: String, secKey: String)(
      implicit runtimeManager: RuntimeManager
  ): String = {
    val bondingForwarderAddress = s"${ethAddress}_bondingForwarder"
    val transferStatusOut       = s"${ethAddress}_transferOut"
    walletTransferDeploy(
      0, //nonce
      amount,
      bondingForwarderAddress,
      transferStatusOut,
      pubKey,
      Base16.decode(secKey)
    ).term
  }

  def preWalletUnlockDeploy(
      ethAddress: String,
      pubKey: String,
      secKey: Array[Byte],
      statusOut: String
  )(implicit runtimeManager: RuntimeManager): DeployData = {
    assert(Base16.encode(Keccak256.hash(Base16.decode(pubKey)).drop(12)) == ethAddress.drop(2))
    val unlockSigDataTerm =
      mkTerm(s""" @"__SCALA__"!(["$pubKey", "$statusOut"].toByteArray())  """).right.get
    val unlockSigData = Keccak256.hash(
      runtimeManager
        .captureResults(runtimeManager.emptyStateHash, unlockSigDataTerm)
        .head
        .exprs
        .head
        .getGByteArray
        .toByteArray
    )
    val unlockSig = Secp256k1.sign(unlockSigData, secKey)
    assert(Secp256k1.verify(unlockSigData, unlockSig, Base16.decode("04" + pubKey)))

    ProtoUtil.sourceDeploy(
      s"""@"$ethAddress"!(["$pubKey", "$statusOut"], "${Base16.encode(unlockSig)}")""",
      System.currentTimeMillis()
    )
  }

  def walletTransferDeploy(
      nonce: Int,
      amount: Long,
      destination: String,
      transferStatusOut: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager): DeployData = {
    val transferSigDataTerm =
      mkTerm(s""" @"__SCALA__"!([$nonce, $amount, "$destination"].toByteArray())  """).right.get
    val transferSigData = Blake2b256.hash(
      runtimeManager
        .captureResults(runtimeManager.emptyStateHash, transferSigDataTerm)
        .head
        .exprs
        .head
        .getGByteArray
        .toByteArray
    )
    val transferSig = Secp256k1.sign(transferSigData, secKey)

    ProtoUtil.sourceDeploy(
      s"""
       |for(@wallet <- @"$pubKey") {
       |  @(wallet, "transfer")!($amount, $nonce, "${Base16
           .encode(transferSig)}", "$destination", "$transferStatusOut")
       |}
     """.stripMargin,
      System.currentTimeMillis()
    )
  }

  def writeFile(name: String, content: String): Unit = {
    val out = new PrintWriter(name)
    out.println(content)
    out.close()
  }

  def main(args: Array[String]): Unit = {
    val runtimeDir              = Files.createTempDirectory("casper-bonding-helper-")
    val activeRuntime           = Runtime.create(runtimeDir, 1024L * 1024 * 1024)
    implicit val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)

    val Array(bondKey, ethAddress, amount, secKey, pubKey) = args
    val unlockCode                                         = unlockDeploy(ethAddress, pubKey, secKey)
    val forwardCode                                        = bondingForwarderDeploy(bondKey, ethAddress)
    val bondCode                                           = bondDeploy(amount.toLong, ethAddress, pubKey, secKey)

    writeFile(s"unlock_${ethAddress}.rho", unlockCode)
    writeFile(s"forward_${ethAddress}_${bondKey}.rho", forwardCode)
    writeFile(s"bond_${ethAddress}.rho", bondCode)

    runtimeDir.recursivelyDelete()
  }
}
