package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._
import scala.util.Random

class UtilizationSpec extends AnyFunSuite {
  import SystolicMatmulTb._

  private final case class UtilizationMetrics(
      tag: String,
      s: Int,
      cmdCount: Int,
      totalCycles: BigInt,
      injectWindowCycles: BigInt,
      injectFullCycles: BigInt,
      stallNoStepCycles: BigInt,
      stallANotReadyCycles: BigInt,
      stallBNotReadyCycles: BigInt,
      stallBankHazardCycles: BigInt,
      stallDrainBlockedCycles: BigInt,
      stallOutputBackpressureCycles: BigInt,
      stallErrorFlushCycles: BigInt,
      traceCmdAcceptedCount: BigInt,
      traceFeedStartCount: BigInt,
      traceDrainStartCount: BigInt,
      traceDrainDoneCount: BigInt
  ) {
    def arrayUtilization: Double =
      if (injectWindowCycles == 0) 0.0 else injectFullCycles.toDouble / injectWindowCycles.toDouble

    def stallSum: BigInt =
      stallNoStepCycles +
        stallANotReadyCycles +
        stallBNotReadyCycles +
        stallBankHazardCycles +
        stallDrainBlockedCycles +
        stallOutputBackpressureCycles +
        stallErrorFlushCycles
  }

  private def f2i(f: Float): Int = java.lang.Float.floatToRawIntBits(f)

  private def mkRows(rows: Int, cols: Int, ld: Int, seed: Int): Seq[Seq[Int]] = {
    val rng = new Random(seed)
    Seq.tabulate(rows, ld) { (_, c) =>
      if (c < cols) f2i((rng.nextFloat() - 0.5f) * 6.0f) else 0
    }
  }

  private def findWorkspaceRoot(): Path = {
    val start = Paths.get(System.getProperty("user.dir")).toAbsolutePath
    var cur: Path = start
    while (cur != null && !Files.exists(cur.resolve("build.mill"))) {
      cur = cur.getParent
    }
    if (cur != null) cur else start
  }

  private def writeMetrics(metrics: UtilizationMetrics): Path = {
    val outDir = findWorkspaceRoot().resolve("build").resolve("utilization_baseline")
    Files.createDirectories(outDir)
    val outPath = outDir.resolve(s"util_${metrics.tag}.metrics.env")

    val lines = Seq(
      s"tag=${metrics.tag}",
      s"s=${metrics.s}",
      s"cmd_count=${metrics.cmdCount}",
      s"total_cycles=${metrics.totalCycles}",
      s"inject_window_cycles=${metrics.injectWindowCycles}",
      s"inject_full_cycles=${metrics.injectFullCycles}",
      f"array_utilization=${metrics.arrayUtilization}%.6f",
      s"stall_no_step_cycles=${metrics.stallNoStepCycles}",
      s"stall_a_not_ready_cycles=${metrics.stallANotReadyCycles}",
      s"stall_b_not_ready_cycles=${metrics.stallBNotReadyCycles}",
      s"stall_bank_hazard_cycles=${metrics.stallBankHazardCycles}",
      s"stall_drain_blocked_cycles=${metrics.stallDrainBlockedCycles}",
      s"stall_output_backpressure_cycles=${metrics.stallOutputBackpressureCycles}",
      s"stall_error_flush_cycles=${metrics.stallErrorFlushCycles}",
      s"stall_sum_cycles=${metrics.stallSum}",
      s"trace_cmd_accepted_count=${metrics.traceCmdAcceptedCount}",
      s"trace_feed_start_count=${metrics.traceFeedStartCount}",
      s"trace_drain_start_count=${metrics.traceDrainStartCount}",
      s"trace_drain_done_count=${metrics.traceDrainDoneCount}"
    )

    Files.write(outPath, lines.asJava, StandardCharsets.UTF_8)
    outPath
  }

  private def runBaseline(tag: String, baseCfg: HarnessConfig): UtilizationMetrics = {
    val cfg = baseCfg.copy(
      memoryCfg = MemoryAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        latencyModel = MemoryLatencyModel.Fixed(0),
        reorderWindow = 0,
        seed = 31 + baseCfg.dutCfg.s
      ),
      outputCfg = OutputAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        backpressureMode = BackpressureMode.AlwaysReady,
        seed = 41 + baseCfg.dutCfg.s
      )
    )

    var captured: Option[UtilizationMetrics] = None

    SpinalSimConfig().withVerilator.compile(SystolicMatmul(cfg.dutCfg)).doSim(s"util_${tag}") { dut =>
      dut.clockDomain.forkStimulus(2)
      val tb = new SystolicMatmulTb(dut, cfg)
      tb.startAgents()

      val s = cfg.dutCfg.s
      val cmdCount = if (s <= 4) 8 else 4
      val cmds = (0 until cmdCount).map { i =>
        val aBase = BigInt(0x100000) + BigInt(i) * 0x500000
        val bBase = aBase + BigInt(0x100000)
        val dBase = aBase + BigInt(0x200000)
        CommandDesc(
          cmdId = 1000 + i,
          aBase = aBase,
          bBase = bBase,
          dBase = dBase,
          m = 2 * s,
          n = 2 * s,
          k = s,
          lda = s,
          ldb = 2 * s,
          ldd = 2 * s,
          primM = s,
          primN = s,
          primK = s
        )
      }

      val queued = cmds.zipWithIndex.map { case (cmd, i) =>
        val a = mkRows(cmd.m, cmd.k, cmd.lda, 100 + i * 2 + s)
        val b = mkRows(cmd.k, cmd.n, cmd.ldb, 101 + i * 2 + s)
        tb.enqueueCommand(cmd, a, b)
      }

      queued.foreach { q =>
        assert(tb.awaitCommand(q).isInstanceOf[CommandSuccess], s"util baseline failed cmdId=${q.cmd.cmdId} for tag=$tag")
      }

      dut.clockDomain.waitSampling(8)

      val metrics = UtilizationMetrics(
        tag = tag,
        s = s,
        cmdCount = cmdCount,
        totalCycles = dut.utilTotalCycles.toBigInt,
        injectWindowCycles = dut.utilInjectWindowCycles.toBigInt,
        injectFullCycles = dut.utilInjectFullCycles.toBigInt,
        stallNoStepCycles = dut.utilStallNoStepCycles.toBigInt,
        stallANotReadyCycles = dut.utilStallANotReadyCycles.toBigInt,
        stallBNotReadyCycles = dut.utilStallBNotReadyCycles.toBigInt,
        stallBankHazardCycles = dut.utilStallBankHazardCycles.toBigInt,
        stallDrainBlockedCycles = dut.utilStallDrainBlockedCycles.toBigInt,
        stallOutputBackpressureCycles = dut.utilStallOutputBackpressureCycles.toBigInt,
        stallErrorFlushCycles = dut.utilStallErrorFlushCycles.toBigInt,
        traceCmdAcceptedCount = dut.traceCmdAcceptedCount.toBigInt,
        traceFeedStartCount = dut.traceFeedStartCount.toBigInt,
        traceDrainStartCount = dut.traceDrainStartCount.toBigInt,
        traceDrainDoneCount = dut.traceDrainDoneCount.toBigInt
      )

      val outPath = writeMetrics(metrics)
      info(s"[util $tag] metrics_file=${outPath.toAbsolutePath}")
      info(
        f"[util $tag] util=${metrics.arrayUtilization}%.6f window=${metrics.injectWindowCycles} full=${metrics.injectFullCycles} " +
          s"stall_no_step=${metrics.stallNoStepCycles} stall_a=${metrics.stallANotReadyCycles} " +
          s"stall_b=${metrics.stallBNotReadyCycles} stall_bank=${metrics.stallBankHazardCycles} " +
          s"stall_drain=${metrics.stallDrainBlockedCycles} stall_out_bp=${metrics.stallOutputBackpressureCycles} " +
          s"stall_err=${metrics.stallErrorFlushCycles}"
      )

      assert(metrics.injectWindowCycles > 0, s"inject window did not open for $tag")
      assert(metrics.injectFullCycles > 0, s"no full inject cycles recorded for $tag")
      assert(metrics.traceCmdAcceptedCount == cmdCount, s"accepted command count mismatch for $tag")
      assert(metrics.traceFeedStartCount > 0, s"no feed-start trace events for $tag")
      assert(metrics.traceDrainStartCount == metrics.traceDrainDoneCount, s"drain start/done mismatch for $tag")
      assert(metrics.stallDrainBlockedCycles == 0, s"drain_blocked must be zero in no-backpressure baseline for $tag")
      assert(
        metrics.injectFullCycles + metrics.stallSum <= (metrics.injectWindowCycles + 1),
        s"stall + full cycles exceed window for $tag"
      )

      captured = Some(metrics)
    }

    captured.getOrElse(fail(s"failed to capture utilization metrics for $tag"))
  }

  test("utilization baseline S=4") {
    runBaseline(tag = "s4", baseCfg = defaultHarnessConfigS4())
  }

  test("utilization baseline S=16") {
    runBaseline(tag = "s16", baseCfg = defaultHarnessConfigS16)
  }
}
