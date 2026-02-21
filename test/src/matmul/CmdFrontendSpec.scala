package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class CmdFrontendSpec extends AnyFunSuite {
  private val cfg = SystolicMatmulConfig(clBits = 128, maxOutstandingRd = 4, cmdqDepth = 2)

  private def driveCmd(
      dut: CmdFrontend,
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
      pm: Int,
      pn: Int,
      pk: Int,
      flags: Int = 0
  ): Unit = {
    dut.io.cmdDesc.cmdId #= cmdId
    dut.io.cmdDesc.aBase #= aBase
    dut.io.cmdDesc.bBase #= bBase
    dut.io.cmdDesc.dBase #= dBase
    dut.io.cmdDesc.m #= m
    dut.io.cmdDesc.n #= n
    dut.io.cmdDesc.k #= k
    dut.io.cmdDesc.lda #= lda
    dut.io.cmdDesc.ldb #= ldb
    dut.io.cmdDesc.ldd #= ldd
    dut.io.cmdDesc.primM #= pm
    dut.io.cmdDesc.primN #= pn
    dut.io.cmdDesc.primK #= pk
    dut.io.cmdDesc.flags #= flags
  }

  test("CmdFrontend accepts valid commands and rejects invalid ones") {
    SpinalSimConfig().withVerilator.compile(CmdFrontend(cfg)).doSim("cmd_frontend") { dut =>
      dut.clockDomain.forkStimulus(2)

      dut.io.cmdValid #= false
      dut.io.outReady #= true
      dut.io.rejReady #= true

      val e = cfg.clElems

      // Valid command.
      driveCmd(
        dut = dut,
        cmdId = 10,
        aBase = 0x1000,
        bBase = 0x2000,
        dBase = 0x3000,
        m = 2 * e,
        n = 2 * e,
        k = 2 * e,
        lda = 2 * e,
        ldb = 2 * e,
        ldd = 2 * e,
        pm = 2 * e,
        pn = 2 * e,
        pk = 2 * e
      )
      dut.io.cmdValid #= true
      dut.clockDomain.waitSampling()
      dut.io.cmdValid #= false

      var sawValidOut = false
      var cycles = 0
      while (!sawValidOut && cycles < 10) {
        dut.clockDomain.waitSampling()
        if (dut.io.outValid.toBoolean) {
          sawValidOut = true
          assert(dut.io.outDesc.cmdId.toBigInt.intValue == 10)
        }
        cycles += 1
      }
      assert(sawValidOut, "valid command was not forwarded")

      // Invalid: unaligned base address.
      driveCmd(
        dut = dut,
        cmdId = 11,
        aBase = 0x1004,
        bBase = 0x2000,
        dBase = 0x3000,
        m = e,
        n = e,
        k = e,
        lda = e,
        ldb = e,
        ldd = e,
        pm = e,
        pn = e,
        pk = e
      )
      dut.io.cmdValid #= true
      dut.clockDomain.waitSampling()
      dut.io.cmdValid #= false

      var sawReject = false
      cycles = 0
      while (!sawReject && cycles < 10) {
        dut.clockDomain.waitSampling()
        if (dut.io.rejValid.toBoolean) {
          sawReject = true
          assert(dut.io.rejCmdId.toBigInt.intValue == 11)
          assert(dut.io.rejErrCode.toBigInt.intValue != 0)
        }
        cycles += 1
      }
      assert(sawReject, "invalid command was not rejected")
    }
  }
}
