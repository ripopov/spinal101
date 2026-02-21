package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable

object SystolicMatmulTb {
  sealed trait CommandRunResult
  final case class CommandSuccess(cyclesWaited: Int) extends CommandRunResult
  final case class CommandFailure(reason: String) extends CommandRunResult

  final case class HarnessTimeouts(
      acceptCycles: Int = 100,
      statusCycles: Int = 2000
  )

  final case class HarnessConfig(
      dutCfg: SystolicMatmulConfig,
      memoryCfg: MemoryAgentConfig,
      outputCfg: OutputAgentConfig,
      timeouts: HarnessTimeouts = HarnessTimeouts()
  )

  def defaultHarnessConfigS4: HarnessConfig = {
    val dutCfg = SystolicMatmulConfig(
      clBits = 128,
      maxOutstandingRd = 4
    )
    HarnessConfig(
      dutCfg = dutCfg,
      memoryCfg = MemoryAgentConfig(clBytes = dutCfg.clBytes),
      outputCfg = OutputAgentConfig(clBytes = dutCfg.clBytes)
    )
  }
}

/**
 * Top-level testbench harness for SystolicMatmul + memory/output/status/cmd agents.
 */
final class SystolicMatmulTb(dut: SystolicMatmul, cfg: SystolicMatmulTb.HarnessConfig) {
  import SystolicMatmulTb._

  private val expectedCompletionOrder = mutable.Queue.empty[Int]

  val cmdDriver = new CmdDriver(dut)
  val aMem = new MemoryAgent(
    name = "A",
    cfg = cfg.memoryCfg,
    clockDomain = dut.clockDomain,
    reqValid = dut.io.a_rd_req_valid,
    reqReady = dut.io.a_rd_req_ready,
    reqAddr = dut.io.a_rd_req_addr,
    reqTag = dut.io.a_rd_req_tag,
    rspValid = dut.io.a_rd_rsp_valid,
    rspReady = dut.io.a_rd_rsp_ready,
    rspData = dut.io.a_rd_rsp_data,
    rspTag = dut.io.a_rd_rsp_tag,
    rspErr = dut.io.a_rd_rsp_err
  )
  val bMem = new MemoryAgent(
    name = "B",
    cfg = cfg.memoryCfg,
    clockDomain = dut.clockDomain,
    reqValid = dut.io.b_rd_req_valid,
    reqReady = dut.io.b_rd_req_ready,
    reqAddr = dut.io.b_rd_req_addr,
    reqTag = dut.io.b_rd_req_tag,
    rspValid = dut.io.b_rd_rsp_valid,
    rspReady = dut.io.b_rd_rsp_ready,
    rspData = dut.io.b_rd_rsp_data,
    rspTag = dut.io.b_rd_rsp_tag,
    rspErr = dut.io.b_rd_rsp_err
  )
  val out = new OutputAgent(
    cfg = cfg.outputCfg,
    clockDomain = dut.clockDomain,
    dWrValid = dut.io.d_wr_valid,
    dWrReady = dut.io.d_wr_ready,
    dWrAddr = dut.io.d_wr_addr,
    dWrData = dut.io.d_wr_data,
    dWrLast = dut.io.d_wr_last,
    dWrCmdId = dut.io.d_wr_cmd_id
  )
  val sts = new StatusAgent(
    clockDomain = dut.clockDomain,
    stsValid = dut.io.sts_valid,
    stsReady = dut.io.sts_ready,
    stsCmdId = dut.io.sts_cmd_id,
    stsOk = dut.io.sts_ok,
    stsErrCode = dut.io.sts_err_code,
    expectedCmdOrder = expectedCompletionOrder
  )

  def startAgents(): Unit = {
    cmdDriver.start()
    aMem.start()
    bMem.start()
    out.start()
    sts.start(alwaysReady = true)
  }

  def runCommand(
      cmd: CommandDesc,
      aRows: Seq[Seq[Int]],
      bRows: Seq[Seq[Int]]
  ): CommandRunResult = {
    populateMatrixRows(aMem, cmd.aBase, cmd.lda, cmd.k, aRows)
    populateMatrixRows(bMem, cmd.bBase, cmd.ldb, cmd.n, bRows)

    val expectedD = RefMatmul.compute(
      A = flattenMatrixRows(aRows, cmd.lda),
      B = flattenMatrixRows(bRows, cmd.ldb),
      M = cmd.m,
      N = cmd.n,
      K = cmd.k,
      lda = cmd.lda,
      ldb = cmd.ldb,
      ldd = cmd.ldd,
      primM = cmd.primM,
      primN = cmd.primN,
      primK = cmd.primK,
      S = cfg.dutCfg.s
    )

    cmdDriver.enqueue(cmd)
    val accepted = cmdDriver.waitAccepted(cmd.cmdId, cfg.timeouts.acceptCycles)
    if (!accepted) {
      return CommandFailure(s"command handshake timeout for cmd_id=${cmd.cmdId}")
    }

    expectedCompletionOrder.enqueue(cmd.cmdId)
    val statusOpt = sts.waitFor(cmd.cmdId, cfg.timeouts.statusCycles, dut.clockDomain)
    statusOpt match {
      case None =>
        CommandFailure(s"status timeout for cmd_id=${cmd.cmdId}")
      case Some(status) if !status.ok =>
        CommandFailure(s"command failed with err_code=${status.errCode} for cmd_id=${cmd.cmdId}")
      case Some(_) =>
        val mismatch = compareOutputAgainstExpected(cmd, expectedD)
        mismatch match {
          case Some(msg) => CommandFailure(msg)
          case None => CommandSuccess(cfg.timeouts.statusCycles)
        }
    }
  }

  private def compareOutputAgainstExpected(cmd: CommandDesc, expectedD: Array[Int]): Option[String] = {
    val expectedByAddr = packExpectedDCacheLines(cmd, expectedD)

    expectedByAddr.collectFirst {
      case (addr, expectedData) if out.writesByAddr.getOrElse(addr, BigInt(0)) != expectedData =>
        val actual = out.writesByAddr.getOrElse(addr, BigInt(0))
        f"D mismatch at addr=0x${addr.toString(16)} expected=0x$expectedData%x actual=0x$actual%x"
    }
  }

  private def flattenMatrixRows(rows: Seq[Seq[Int]], ld: Int): Array[Int] = {
    val out = Array.fill(rows.length * ld)(0)
    for (r <- rows.indices; c <- rows(r).indices) {
      out(r * ld + c) = rows(r)(c)
    }
    out
  }

  private def populateMatrixRows(
      mem: MemoryAgent,
      baseAddr: BigInt,
      ld: Int,
      usedCols: Int,
      rows: Seq[Seq[Int]]
  ): Unit = {
    val elemsPerCl = cfg.dutCfg.clElems
    require(ld % elemsPerCl == 0, s"ld=$ld must be a multiple of CL elements=$elemsPerCl")

    for (r <- rows.indices) {
      val row = rows(r)
      val padded = Array.fill(ld)(0)
      for (c <- row.indices) padded(c) = row(c)

      val clsPerRow = ld / elemsPerCl
      for (cl <- 0 until clsPerRow) {
        val elems = padded.slice(cl * elemsPerCl, (cl + 1) * elemsPerCl)
        val data = packElemsToCacheLine(elems)
        val addr = baseAddr + BigInt((r * clsPerRow + cl) * cfg.dutCfg.clBytes)
        mem.writeCacheLine(addr, data)
      }
    }
  }

  private def packExpectedDCacheLines(cmd: CommandDesc, d: Array[Int]): Map[BigInt, BigInt] = {
    val elemsPerCl = cfg.dutCfg.clElems
    val clsPerRow = cmd.ldd / elemsPerCl
    val byAddr = mutable.HashMap.empty[BigInt, BigInt]

    for (r <- 0 until cmd.m) {
      for (cl <- 0 until clsPerRow) {
        val elems = Array.fill(elemsPerCl)(0)
        var i = 0
        while (i < elemsPerCl) {
          val col = cl * elemsPerCl + i
          if (col < cmd.n) {
            elems(i) = d(r * cmd.ldd + col)
          }
          i += 1
        }
        val data = packElemsToCacheLine(elems)
        val addr = cmd.dBase + BigInt((r * clsPerRow + cl) * cfg.dutCfg.clBytes)
        byAddr(addr) = data
      }
    }

    byAddr.toMap
  }

  private def packElemsToCacheLine(elems: Array[Int]): BigInt = {
    var out = BigInt(0)
    var i = 0
    while (i < elems.length) {
      val bits = BigInt(elems(i).toLong & 0xffffffffL)
      out |= (bits << (i * 32))
      i += 1
    }
    out
  }
}

class SystolicMatmulTbSpec extends AnyFunSuite {
  import SystolicMatmulTb._

  private def f2i(f: Float): Int = java.lang.Float.floatToRawIntBits(f)

  test("systolic harness smoke test detects stub DUT failure clearly") {
    val harnessCfg = defaultHarnessConfigS4
    var observedFailure: Option[String] = None

    SpinalSimConfig().withVerilator.compile(SystolicMatmul(harnessCfg.dutCfg)).doSim("systolic_tb_smoke") { dut =>
      dut.clockDomain.forkStimulus(period = 2)

      val tb = new SystolicMatmulTb(dut, harnessCfg)
      tb.startAgents()

      val s = harnessCfg.dutCfg.s
      val a = Seq.tabulate(s, s) { (r, c) => f2i((r * s + c + 1).toFloat) }
      val b = Seq.tabulate(s, s) { (r, c) => f2i((if (r == c) 1.0f else 0.0f)) }

      val cmd = CommandDesc(
        cmdId = 1,
        aBase = BigInt(0x1000),
        bBase = BigInt(0x2000),
        dBase = BigInt(0x3000),
        m = s,
        n = s,
        k = s,
        lda = s,
        ldb = s,
        ldd = s,
        primM = s,
        primN = s,
        primK = s,
        flags = 0
      )

      tb.runCommand(cmd, a, b) match {
        case CommandSuccess(_) =>
          fail("expected smoke test to fail for stub DUT, but command completed successfully")
        case CommandFailure(reason) =>
          observedFailure = Some(reason)
      }
    }

    assert(observedFailure.nonEmpty, "smoke test did not report failure reason")
    assert(
      observedFailure.get.contains("handshake timeout") || observedFailure.get.contains("status timeout"),
      s"unexpected failure reason: ${observedFailure.get}"
    )
  }
}
