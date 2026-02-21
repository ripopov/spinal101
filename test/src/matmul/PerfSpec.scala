package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

class PerfSpec extends AnyFunSuite {
  import SystolicMatmulTb._

  private def f2i(f: Float): Int = java.lang.Float.floatToRawIntBits(f)

  private def mkRows(rows: Int, cols: Int, ld: Int, seed: Int): Seq[Seq[Int]] = {
    val rng = new Random(seed)
    Seq.tabulate(rows, ld) { (_, c) =>
      if (c < cols) f2i((rng.nextFloat() - 0.5f) * 4.0f) else 0
    }
  }

  private def burstLengths(cycles: Seq[Long]): Seq[Int] = {
    if (cycles.isEmpty) return Seq.empty
    val lengths = mutable.ArrayBuffer.empty[Int]
    var run = 1
    var i = 1
    while (i < cycles.length) {
      if (cycles(i) - cycles(i - 1) == 1) run += 1
      else {
        lengths += run
        run = 1
      }
      i += 1
    }
    lengths += run
    lengths.toSeq
  }

  private def strideRunLengths(cycles: Seq[Long], stride: Int): Seq[Int] = {
    if (cycles.length < 2) return Seq.empty
    val runs = mutable.ArrayBuffer.empty[Int]
    var run = 0
    var i = 1
    while (i < cycles.length) {
      if (cycles(i) - cycles(i - 1) == stride) {
        run += 1
      } else if (run > 0) {
        runs += (run + 1)
        run = 0
      }
      i += 1
    }
    if (run > 0) runs += (run + 1)
    runs.toSeq
  }

  private def injectWindows(cycles: Seq[Long]): Seq[(Long, Long)] = {
    if (cycles.isEmpty) return Seq.empty
    val windows = mutable.ArrayBuffer.empty[(Long, Long)]
    var start = cycles.head
    var prev = cycles.head
    var i = 1
    while (i < cycles.length) {
      val c = cycles(i)
      if (c - prev != 1) {
        windows += ((start, prev))
        start = c
      }
      prev = c
      i += 1
    }
    windows += ((start, prev))
    windows.toSeq
  }

  private def runPerfContracts(base: HarnessConfig, tag: String): Unit = {
    val compiled = SpinalSimConfig().withVerilator.compile(SystolicMatmul(base.dutCfg))
    val s = base.dutCfg.s

    def runCase(name: String, cfg: HarnessConfig)(body: (SystolicMatmul, SystolicMatmulTb) => Unit): Unit = {
      compiled.doSim(name) { dut =>
        dut.clockDomain.forkStimulus(2)
        val tb = new SystolicMatmulTb(dut, cfg)
        tb.startAgents()
        body(dut, tb)
      }
    }

    runCase(
      s"perf_${tag}_drain_throughput",
      base.copy(
        memoryCfg = MemoryAgentConfig(
          clBytes = base.dutCfg.clBytes,
          latencyModel = MemoryLatencyModel.Fixed(0),
          reorderWindow = 0,
          seed = 101
        ),
        outputCfg = OutputAgentConfig(
          clBytes = base.dutCfg.clBytes,
          backpressureMode = BackpressureMode.AlwaysReady,
          seed = 102
        )
      )
    ) { (_, tb) =>
      val cmd = CommandDesc(
        cmdId = 1000 + s,
        aBase = 0x100000,
        bBase = 0x200000,
        dBase = 0x300000,
        m = 8 * s,
        n = 8 * s,
        k = s,
        lda = s,
        ldb = 8 * s,
        ldd = 8 * s,
        primM = 8 * s,
        primN = 8 * s,
        primK = s
      )

      val a = mkRows(cmd.m, cmd.k, cmd.lda, seed = 10 + s)
      val b = mkRows(cmd.k, cmd.n, cmd.ldb, seed = 20 + s)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess], s"throughput command failed for S=$s")

      val fireCycles = tb.out.fireCycles.toSeq
      val expectedBeats = cmd.m * (cmd.ldd / s)
      assert(fireCycles.length == expectedBeats, s"unexpected D beats for S=$s")
      assert(fireCycles.nonEmpty, s"no D beats observed for S=$s")
      val diffs = fireCycles.sliding(2).collect { case Seq(a0, a1) => (a1 - a0).toInt }.toSeq
      val maxGap = if (diffs.nonEmpty) diffs.max else 0
      val avgGap = if (diffs.nonEmpty) diffs.sum.toDouble / diffs.length else 0.0
      val stride2Runs = strideRunLengths(fireCycles, stride = 2)
      val longestStride2 = if (stride2Runs.nonEmpty) stride2Runs.max else 0

      assert(maxGap <= 2, s"drain exceeded expected no-stall interval for S=$s (max gap=$maxGap)")
      assert(longestStride2 >= 16, s"insufficient sustained drain run for S=$s (longest=$longestStride2)")

      info(
        f"[perf S=$s throughput] beats=$expectedBeats first=${fireCycles.head} last=${fireCycles.last} maxGap=$maxGap avgGap=$avgGap%.2f longestStride2=$longestStride2"
      )
    }

    runCase(
      s"perf_${tag}_overlap_gap",
      base.copy(
        memoryCfg = MemoryAgentConfig(
          clBytes = base.dutCfg.clBytes,
          latencyModel = MemoryLatencyModel.Fixed(0),
          reorderWindow = 0,
          seed = 103
        ),
        outputCfg = OutputAgentConfig(
          clBytes = base.dutCfg.clBytes,
          backpressureMode = BackpressureMode.AlwaysReady,
          seed = 104
        )
      )
    ) { (dut, tb) =>
      val reqIssueCycles = mutable.ArrayBuffer.empty[Long]
      fork {
        var cycle = 0L
        while (true) {
          val aFire = dut.io.a_rd_req_valid.toBoolean && dut.io.a_rd_req_ready.toBoolean
          if (aFire) reqIssueCycles += cycle
          dut.clockDomain.waitSampling()
          cycle += 1
        }
      }

      val cmd = CommandDesc(
        cmdId = 1100 + s,
        aBase = 0x400000,
        bBase = 0x500000,
        dBase = 0x600000,
        m = s,
        n = s,
        k = 2 * s,
        lda = 2 * s,
        ldb = s,
        ldd = s,
        primM = s,
        primN = s,
        primK = 2 * s
      )

      val a = mkRows(cmd.m, cmd.k, cmd.lda, seed = 30 + s)
      val b = mkRows(cmd.k, cmd.n, cmd.ldb, seed = 40 + s)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess], s"overlap command failed for S=$s")

      val windows = injectWindows(reqIssueCycles.toSeq)
      assert(windows.length >= 2, s"expected at least two inject windows for S=$s")

      val firstLen = (windows.head._2 - windows.head._1 + 1).toInt
      val secondLen = (windows(1)._2 - windows(1)._1 + 1).toInt
      val gap = (windows(1)._1 - windows.head._2 - 1).toInt

      assert(firstLen > 0, s"invalid first inject window length for S=$s")
      assert(secondLen > 0, s"invalid second inject window length for S=$s")
      val overlapGapBudget = s + 14
      assert(
        gap <= overlapGapBudget,
        s"excessive primitive gap for S=$s: gap=$gap budget=$overlapGapBudget"
      )

      info(s"[perf S=$s overlap] firstLen=$firstLen secondLen=$secondLen reqGap=$gap")
    }

    runCase(
      s"perf_${tag}_backpressure_absorb",
      base.copy(
        memoryCfg = MemoryAgentConfig(
          clBytes = base.dutCfg.clBytes,
          latencyModel = MemoryLatencyModel.RandomRange(1, 6),
          reorderWindow = 2,
          seed = 105
        ),
        outputCfg = OutputAgentConfig(
          clBytes = base.dutCfg.clBytes,
          backpressureMode = BackpressureMode.PeriodicStall(periodCycles = 9, stallCycles = 3),
          seed = 106
        )
      )
    ) { (_, tb) =>
      val cmd = CommandDesc(
        cmdId = 1200 + s,
        aBase = 0x700000,
        bBase = 0x800000,
        dBase = 0x900000,
        m = 4 * s,
        n = 4 * s,
        k = s,
        lda = s,
        ldb = 4 * s,
        ldd = 4 * s,
        primM = 4 * s,
        primN = 4 * s,
        primK = s
      )

      val a = mkRows(cmd.m, cmd.k, cmd.lda, seed = 50 + s)
      val b = mkRows(cmd.k, cmd.n, cmd.ldb, seed = 60 + s)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess], s"backpressure command failed for S=$s")

      val fireCycles = tb.out.fireCycles.toSeq
      assert(fireCycles.nonEmpty, s"no D beats observed under backpressure for S=$s")

      val gaps = fireCycles.sliding(2).collect { case Seq(a0, a1) => (a1 - a0).toInt }.toSeq
      val burstMax = burstLengths(fireCycles).max
      val stride2Runs = strideRunLengths(fireCycles, stride = 2)
      val longestStride2 = if (stride2Runs.nonEmpty) stride2Runs.max else 0
      val maxGap = if (gaps.nonEmpty) gaps.max else 0

      assert(maxGap > 2, s"expected at least one stalled interval under periodic backpressure for S=$s")
      assert(
        longestStride2 >= 2,
        s"drain did not resume with near-1 CL/cycle bursts after backpressure for S=$s"
      )

      info(
        s"[perf S=$s backpressure] beats=${fireCycles.length} maxGap=$maxGap maxBurst=$burstMax longestStride2=$longestStride2"
      )
    }

    runCase(
      s"perf_${tag}_outstanding_depth",
      base.copy(
        memoryCfg = MemoryAgentConfig(
          clBytes = base.dutCfg.clBytes,
          latencyModel = MemoryLatencyModel.Fixed(base.dutCfg.maxOutstandingRd + 8),
          reorderWindow = 0,
          seed = 107
        ),
        outputCfg = OutputAgentConfig(
          clBytes = base.dutCfg.clBytes,
          backpressureMode = BackpressureMode.AlwaysReady,
          seed = 108
        )
      )
    ) { (_, tb) =>
      val cmd = CommandDesc(
        cmdId = 1300 + s,
        aBase = 0xa00000,
        bBase = 0xb00000,
        dBase = 0xc00000,
        m = s,
        n = s,
        k = s,
        lda = s,
        ldb = s,
        ldd = s,
        primM = s,
        primN = s,
        primK = s
      )

      val a = mkRows(cmd.m, cmd.k, cmd.lda, seed = 70 + s)
      val b = mkRows(cmd.k, cmd.n, cmd.ldb, seed = 80 + s)
      assert(tb.runCommand(cmd, a, b).isInstanceOf[CommandSuccess], s"outstanding-depth command failed for S=$s")

      val expected = base.dutCfg.maxOutstandingRd
      val aMax = tb.aMem.maxOutstandingObserved
      val bMax = tb.bMem.maxOutstandingObserved
      assert(aMax == expected, s"A outstanding depth did not reach $expected for S=$s (got $aMax)")
      assert(bMax == expected, s"B outstanding depth did not reach $expected for S=$s (got $bMax)")

      info(s"[perf S=$s outstanding] Amax=$aMax Bmax=$bMax expected=$expected")
    }
  }

  test("performance contracts S=4") {
    runPerfContracts(defaultHarnessConfigS4(), tag = "s4")
  }

  test("performance contracts S=16") {
    runPerfContracts(defaultHarnessConfigS16, tag = "s16")
  }
}
