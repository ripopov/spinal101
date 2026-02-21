package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

object SystolicMatmulTb {
  sealed trait CommandRunResult
  final case class CommandSuccess(cyclesWaited: Int) extends CommandRunResult
  final case class CommandFailure(reason: String) extends CommandRunResult

  final case class HarnessTimeouts(
      acceptCycles: Int = 500,
      statusCycles: Int = 200000
  )

  final case class HarnessConfig(
      dutCfg: SystolicMatmulConfig,
      memoryCfg: MemoryAgentConfig,
      outputCfg: OutputAgentConfig,
      timeouts: HarnessTimeouts = HarnessTimeouts()
  )

  def defaultHarnessConfigS4(
      memoryCfg: Option[MemoryAgentConfig] = None,
      outputCfg: Option[OutputAgentConfig] = None
  ): HarnessConfig = {
    val dutCfg = SystolicMatmulConfig(
      clBits = 128,
      maxOutstandingRd = 4
    )
    HarnessConfig(
      dutCfg = dutCfg,
      memoryCfg = memoryCfg.getOrElse(MemoryAgentConfig(clBytes = dutCfg.clBytes)),
      outputCfg = outputCfg.getOrElse(OutputAgentConfig(clBytes = dutCfg.clBytes))
    )
  }

  def defaultHarnessConfigS16: HarnessConfig = {
    val dutCfg = SystolicMatmulConfig(clBits = 512)
    HarnessConfig(
      dutCfg = dutCfg,
      memoryCfg = MemoryAgentConfig(clBytes = dutCfg.clBytes),
      outputCfg = OutputAgentConfig(clBytes = dutCfg.clBytes)
    )
  }
}

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

  def runCommand(cmd: CommandDesc, aRows: Seq[Seq[Int]], bRows: Seq[Seq[Int]]): CommandRunResult = {
    populateMatrixRows(aMem, cmd.aBase, cmd.lda, aRows)
    populateMatrixRows(bMem, cmd.bBase, cmd.ldb, bRows)

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
    if (!accepted) return CommandFailure(s"command handshake timeout for cmd_id=${cmd.cmdId}")

    expectedCompletionOrder.enqueue(cmd.cmdId)
    val statusOpt = sts.waitFor(cmd.cmdId, cfg.timeouts.statusCycles, dut.clockDomain)
    statusOpt match {
      case None => CommandFailure(s"status timeout for cmd_id=${cmd.cmdId}")
      case Some(status) if !status.ok => CommandFailure(s"command failed with err_code=${status.errCode} for cmd_id=${cmd.cmdId}")
      case Some(_) =>
        compareOutputAgainstExpected(cmd, expectedD) match {
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
        val observed = out.writeBeats.take(8).map(b => f"0x${b.addr.toString(16)}->0x${b.data}%x").mkString(",")
        val expectedPreview = expectedByAddr.toSeq.sortBy(_._1).take(8).map { case (a, d) => f"0x${a.toString(16)}->0x$d%x" }.mkString(",")
        f"D mismatch at addr=0x${addr.toString(16)} expected=0x$expectedData%x actual=0x$actual%x writes=${out.writeBeats.size} observed=[$observed] expectedPreview=[$expectedPreview]"
    }
  }

  private def flattenMatrixRows(rows: Seq[Seq[Int]], ld: Int): Array[Int] = {
    val out = Array.fill(rows.length * ld)(0)
    for (r <- rows.indices; c <- rows(r).indices) out(r * ld + c) = rows(r)(c)
    out
  }

  private def populateMatrixRows(mem: MemoryAgent, baseAddr: BigInt, ld: Int, rows: Seq[Seq[Int]]): Unit = {
    val elemsPerCl = cfg.dutCfg.clElems
    require(ld % elemsPerCl == 0)

    for (r <- rows.indices) {
      val padded = Array.fill(ld)(0)
      for (c <- rows(r).indices) padded(c) = rows(r)(c)
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

    for (r <- 0 until cmd.m; cl <- 0 until clsPerRow) {
      val elems = Array.fill(elemsPerCl)(0)
      var i = 0
      while (i < elemsPerCl) {
        val col = cl * elemsPerCl + i
        if (col < cmd.n) elems(i) = d(r * cmd.ldd + col)
        i += 1
      }
      val data = packElemsToCacheLine(elems)
      val addr = cmd.dBase + BigInt((r * clsPerRow + cl) * cfg.dutCfg.clBytes)
      byAddr(addr) = data
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

  private def mkRows(rows: Int, cols: Int, ld: Int, seed: Int): Seq[Seq[Int]] = {
    val rng = new Random(seed)
    Seq.tabulate(rows, ld) { (_, c) =>
      if (c < cols) f2i((rng.nextFloat() - 0.5f) * 6.0f) else 0
    }
  }

  private def runCase(name: String, cfg: HarnessConfig)(body: SystolicMatmulTb => Unit): Unit = {
    SpinalSimConfig().withVerilator.compile(SystolicMatmul(cfg.dutCfg)).doSim(name) { dut =>
      dut.clockDomain.forkStimulus(2)
      val tb = new SystolicMatmulTb(dut, cfg)
      tb.startAgents()
      body(tb)
    }
  }

  test("integration smoke single SxS command") {
    val cfg = defaultHarnessConfigS4()
    runCase("systolic_int_smoke", cfg) { tb =>
      val s = cfg.dutCfg.s
      val cmd = CommandDesc(1, 0x1000, 0x2000, 0x3000, s, s, s, s, s, s, s, s, s)
      val a = mkRows(s, s, s, 1)
      val b = mkRows(s, s, s, 2)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess])
    }
  }

  test("integration all primitive sizes on S=4") {
    val cfg = defaultHarnessConfigS4()
    runCase("systolic_int_all_prims", cfg) { tb =>
      val s = cfg.dutCfg.s
      val primMN = Seq(1, 2, 4, 8).map(_ * s)
      val primK = Seq(1, 2, 4, 8, 16).map(_ * s)
      var cmdId = 10
      for (pm <- primMN; pn <- primMN; pk <- primK.take(2)) {
        val m = pm; val n = pn; val k = pk
        val cmd = CommandDesc(cmdId, 0x1000 + cmdId * 0x1000L, 0x2000 + cmdId * 0x1000L, 0x3000 + cmdId * 0x1000L,
          m, n, k, k, n, n, pm, pn, pk)
        val a = mkRows(m, k, k, cmdId)
        val b = mkRows(k, n, n, cmdId + 1)
        assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess], s"failed cmdId=$cmdId pm=$pm pn=$pn pk=$pk")
        cmdId += 1
      }
    }
  }

  test("integration multi-tile and k-reduction") {
    val cfg = defaultHarnessConfigS4()
    runCase("systolic_int_multitile_kred", cfg) { tb =>
      val s = cfg.dutCfg.s
      val m = 2 * s; val n = 2 * s; val k = 4 * s
      val cmd = CommandDesc(100, 0x10000, 0x20000, 0x30000, m, n, k, k, n, n, 2 * s, 2 * s, s)
      val a = mkRows(m, k, k, 11)
      val b = mkRows(k, n, n, 22)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess])
    }
  }

  test("integration out-of-order memory and output backpressure") {
    val base = defaultHarnessConfigS4()
    val cfg = base.copy(
      memoryCfg = MemoryAgentConfig(
        clBytes = base.dutCfg.clBytes,
        latencyModel = MemoryLatencyModel.RandomRange(1, 20),
        reorderWindow = 4,
        seed = 7
      ),
      outputCfg = OutputAgentConfig(
        clBytes = base.dutCfg.clBytes,
        backpressureMode = BackpressureMode.RandomReady(0.7),
        seed = 9
      )
    )
    runCase("systolic_int_ooo_bp", cfg) { tb =>
      val s = cfg.dutCfg.s
      val cmd = CommandDesc(200, 0x40000, 0x50000, 0x60000, 2 * s, 2 * s, 2 * s, 2 * s, 2 * s, 2 * s, 2 * s, 2 * s, s)
      val a = mkRows(2 * s, 2 * s, 2 * s, 33)
      val b = mkRows(2 * s, 2 * s, 2 * s, 44)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess])
    }
  }

  test("integration back-to-back commands") {
    val cfg = defaultHarnessConfigS4()
    runCase("systolic_int_back_to_back", cfg) { tb =>
      val s = cfg.dutCfg.s
      val cmd0 = CommandDesc(300, 0x70000, 0x80000, 0x90000, s, s, s, s, s, s, s, s, s)
      val cmd1 = CommandDesc(301, 0xa0000, 0xb0000, 0xc0000, s, s, s, s, s, s, s, s, s)
      val a0 = mkRows(s, s, s, 55); val b0 = mkRows(s, s, s, 56)
      val a1 = mkRows(s, s, s, 57); val b1 = mkRows(s, s, s, 58)
      assert(tb.runCommand(cmd0, a0, b0).isInstanceOf[CommandSuccess])
      assert(tb.runCommand(cmd1, a1, b1).isInstanceOf[CommandSuccess])
    }
  }

  test("integration read error injection returns error status") {
    val base = defaultHarnessConfigS4()
    val cfg = base.copy(
      memoryCfg = MemoryAgentConfig(
        clBytes = base.dutCfg.clBytes,
        latencyModel = MemoryLatencyModel.Fixed(0),
        reorderWindow = 0,
        injectErrEvery = 3,
        seed = 2
      )
    )
    runCase("systolic_int_err", cfg) { tb =>
      val s = cfg.dutCfg.s
      val cmd = CommandDesc(400, 0xd0000, 0xe0000, 0xf0000, s, s, s, s, s, s, s, s, s)
      val res = tb.runCommand(cmd, mkRows(s, s, s, 60), mkRows(s, s, s, 61))
      assert(res.isInstanceOf[CommandFailure])
      assert(res.asInstanceOf[CommandFailure].reason.contains("err_code"))
    }
  }

  test("integration invalid command rejected") {
    val cfg = defaultHarnessConfigS4()
    runCase("systolic_int_bad_cmd", cfg) { tb =>
      val s = cfg.dutCfg.s
      val bad = CommandDesc(500, 0x1000, 0x2000, 0x3000, s, s, s, s, s, s, s, s, s, flags = 1)
      val res = tb.runCommand(bad, mkRows(s, s, s, 70), mkRows(s, s, s, 71))
      assert(res.isInstanceOf[CommandFailure])
      assert(res.asInstanceOf[CommandFailure].reason.contains("err_code"))
    }
  }

  test("integration S=16 smoke") {
    val cfg = defaultHarnessConfigS16
    runCase("systolic_int_s16", cfg) { tb =>
      val s = cfg.dutCfg.s
      val cmd = CommandDesc(600, 0x100000, 0x200000, 0x300000, s, s, s, s, s, s, s, s, s)
      val a = mkRows(s, s, s, 80)
      val b = mkRows(s, s, s, 81)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess])
    }
  }
}
