package matmul

import spinal.core.sim._

import scala.collection.mutable

final case class CommandDesc(
    cmdId: Int,
    aBase: BigInt,
    bBase: BigInt,
    dBase: BigInt,
    m: Int,
    n: Int,
    k: Int,
    lda: Int,
    ldb: Int,
    ldd: Int,
    primM: Int,
    primN: Int,
    primK: Int,
    flags: Int = 0
)

/**
 * Decoupled command descriptor driver with queueing and cmd_ready backpressure handling.
 */
final class CmdDriver(dut: SystolicMatmul) {
  private val pending = mutable.Queue.empty[CommandDesc]
  val acceptedCmdOrder: mutable.Queue[Int] = mutable.Queue.empty[Int]

  def enqueue(cmd: CommandDesc): Unit = pending.enqueue(cmd)

  def start(): Unit = {
    dut.io.cmd_valid #= false
    driveZeros()

    fork {
      var active: Option[CommandDesc] = None

      while (true) {
        active match {
          case None if pending.nonEmpty =>
            active = Some(pending.dequeue())
          case _ =>
        }

        active match {
          case Some(cmd) =>
            driveCmd(cmd)
            dut.io.cmd_valid #= true
          case None =>
            dut.io.cmd_valid #= false
            driveZeros()
        }

        dut.clockDomain.waitSampling()

        if (active.nonEmpty && dut.io.cmd_valid.toBoolean && dut.io.cmd_ready.toBoolean) {
          acceptedCmdOrder.enqueue(active.get.cmdId)
          active = None
        }
      }
    }
  }

  def waitAccepted(cmdId: Int, timeoutCycles: Int): Boolean = {
    var waited = 0
    while (waited < timeoutCycles) {
      if (acceptedCmdOrder.contains(cmdId)) return true
      dut.clockDomain.waitSampling()
      waited += 1
    }
    false
  }

  private def driveZeros(): Unit = {
    dut.io.cmd_desc_cmd_id #= 0
    dut.io.cmd_desc_a_base #= 0
    dut.io.cmd_desc_b_base #= 0
    dut.io.cmd_desc_d_base #= 0
    dut.io.cmd_desc_m #= 0
    dut.io.cmd_desc_n #= 0
    dut.io.cmd_desc_k #= 0
    dut.io.cmd_desc_lda #= 0
    dut.io.cmd_desc_ldb #= 0
    dut.io.cmd_desc_ldd #= 0
    dut.io.cmd_desc_prim_m #= 0
    dut.io.cmd_desc_prim_n #= 0
    dut.io.cmd_desc_prim_k #= 0
    dut.io.cmd_desc_flags #= 0
  }

  private def driveCmd(cmd: CommandDesc): Unit = {
    dut.io.cmd_desc_cmd_id #= cmd.cmdId
    dut.io.cmd_desc_a_base #= cmd.aBase
    dut.io.cmd_desc_b_base #= cmd.bBase
    dut.io.cmd_desc_d_base #= cmd.dBase
    dut.io.cmd_desc_m #= cmd.m
    dut.io.cmd_desc_n #= cmd.n
    dut.io.cmd_desc_k #= cmd.k
    dut.io.cmd_desc_lda #= cmd.lda
    dut.io.cmd_desc_ldb #= cmd.ldb
    dut.io.cmd_desc_ldd #= cmd.ldd
    dut.io.cmd_desc_prim_m #= cmd.primM
    dut.io.cmd_desc_prim_n #= cmd.primN
    dut.io.cmd_desc_prim_k #= cmd.primK
    dut.io.cmd_desc_flags #= cmd.flags
  }
}
