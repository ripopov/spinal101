package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

class UtilizationSpec extends AnyFunSuite {
  import SystolicMatmulTb._

  private final case class CounterSnapshot(
      injectWindowCycles: BigInt,
      injectFullCycles: BigInt,
      stallNoStepCycles: BigInt,
      stallANotReadyCycles: BigInt,
      stallBNotReadyCycles: BigInt,
      stallBankHazardCycles: BigInt,
      stallDrainBlockedCycles: BigInt,
      stallOutputBackpressureCycles: BigInt,
      stallErrorFlushCycles: BigInt,
      stallPrefetchSetupCycles: BigInt,
      stallBankHazardWaitCycles: BigInt,
      stallDrainEnqueueBookkeepingCycles: BigInt
  ) {
    def -(rhs: CounterSnapshot): CounterSnapshot =
      CounterSnapshot(
        injectWindowCycles = injectWindowCycles - rhs.injectWindowCycles,
        injectFullCycles = injectFullCycles - rhs.injectFullCycles,
        stallNoStepCycles = stallNoStepCycles - rhs.stallNoStepCycles,
        stallANotReadyCycles = stallANotReadyCycles - rhs.stallANotReadyCycles,
        stallBNotReadyCycles = stallBNotReadyCycles - rhs.stallBNotReadyCycles,
        stallBankHazardCycles = stallBankHazardCycles - rhs.stallBankHazardCycles,
        stallDrainBlockedCycles = stallDrainBlockedCycles - rhs.stallDrainBlockedCycles,
        stallOutputBackpressureCycles = stallOutputBackpressureCycles - rhs.stallOutputBackpressureCycles,
        stallErrorFlushCycles = stallErrorFlushCycles - rhs.stallErrorFlushCycles,
        stallPrefetchSetupCycles = stallPrefetchSetupCycles - rhs.stallPrefetchSetupCycles,
        stallBankHazardWaitCycles = stallBankHazardWaitCycles - rhs.stallBankHazardWaitCycles,
        stallDrainEnqueueBookkeepingCycles = stallDrainEnqueueBookkeepingCycles - rhs.stallDrainEnqueueBookkeepingCycles
      )
  }

  private final case class CycleSample(
      cycle: Long,
      feed: Boolean,
      stallSample: Boolean,
      aReqFire: Boolean,
      bReqFire: Boolean,
      bucketPrefetchSetup: Boolean,
      bucketBankHazardWait: Boolean,
      bucketDrainEnqueueBookkeeping: Boolean,
      bucketOutputBackpressure: Boolean
  )

  private final case class WindowMetrics(
      startCycle: Long,
      endCycle: Long,
      measuredWindowCycles: BigInt,
      measuredFeedCycles: BigInt,
      measuredStallSampleCycles: BigInt,
      reqFireACycles: BigInt,
      reqFireBCycles: BigInt,
      bucketPrefetchSetupCycles: BigInt,
      bucketBankHazardWaitCycles: BigInt,
      bucketDrainEnqueueBookkeepingCycles: BigInt,
      bucketOutputBackpressureCycles: BigInt,
      feedRunCount: BigInt,
      feedRunLongestCycles: BigInt,
      feedRunAverageCycles: Double,
      feedBreakCount: BigInt,
      feedBreakPrefetchSetup: BigInt,
      feedBreakBankHazardWait: BigInt,
      feedBreakDrainEnqueueBookkeeping: BigInt,
      feedBreakOutputBackpressure: BigInt,
      feedBreakOther: BigInt
  ) {
    def nonFeedCycles: BigInt = measuredStallSampleCycles

    def bucketedNonFeedCycles: BigInt =
      bucketPrefetchSetupCycles +
        bucketBankHazardWaitCycles +
        bucketDrainEnqueueBookkeepingCycles +
        bucketOutputBackpressureCycles

    def feedDutyWindowCycles: BigInt =
      measuredFeedCycles +
        bucketBankHazardWaitCycles +
        bucketDrainEnqueueBookkeepingCycles +
        bucketOutputBackpressureCycles

    def feedDuty: Double =
      if (feedDutyWindowCycles == 0) 0.0 else measuredFeedCycles.toDouble / feedDutyWindowCycles.toDouble

    def reqDutyA: Double =
      if (measuredWindowCycles == 0) 0.0 else reqFireACycles.toDouble / measuredWindowCycles.toDouble

    def reqDutyB: Double =
      if (measuredWindowCycles == 0) 0.0 else reqFireBCycles.toDouble / measuredWindowCycles.toDouble

    def feedBreaksClassified: BigInt =
      feedBreakPrefetchSetup +
        feedBreakBankHazardWait +
        feedBreakDrainEnqueueBookkeeping +
        feedBreakOutputBackpressure +
        feedBreakOther
  }

  private final case class UtilizationMetrics(
      scenario: String,
      tag: String,
      s: Int,
      cmdCount: Int,
      expectedSteadyFullCycles: BigInt,
      observedSteadyFullCycles: BigInt,
      steadyWindowCycles: BigInt,
      steadyStalls: CounterSnapshot,
      totalStalls: CounterSnapshot,
      window: WindowMetrics,
      outputBeatCount: Int,
      expectedOutputBeatCount: Int,
      outputFirstCycle: Long,
      outputLastCycle: Long,
      maxOutstandingA: Int,
      maxOutstandingB: Int
  ) {
    def steadyArrayUtilization: Double =
      if (expectedSteadyFullCycles == 0) 0.0 else observedSteadyFullCycles.toDouble / expectedSteadyFullCycles.toDouble

    def streamUtilization: Double = {
      if (expectedOutputBeatCount <= 1 || outputBeatCount <= 1) {
        1.0
      } else {
        val idealDuration = ((expectedOutputBeatCount - 1) * 2 + 1).toDouble
        val actualDuration = (outputLastCycle - outputFirstCycle + 1).toDouble
        if (actualDuration <= 0.0) 0.0 else math.min(1.0, idealDuration / actualDuration)
      }
    }

    def feedDuty: Double = window.feedDuty
    def reqDutyA: Double = window.reqDutyA
    def reqDutyB: Double = window.reqDutyB
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

  private def writeMetrics(metrics: UtilizationMetrics, fileTag: String): Path = {
    val outDir = findWorkspaceRoot().resolve("build").resolve("utilization_gates")
    Files.createDirectories(outDir)
    val outPath = outDir.resolve(s"$fileTag.metrics.env")

    val lines = Seq(
      s"scenario=${metrics.scenario}",
      s"tag=${metrics.tag}",
      s"s=${metrics.s}",
      s"cmd_count=${metrics.cmdCount}",
      s"expected_steady_full_cycles=${metrics.expectedSteadyFullCycles}",
      s"observed_steady_full_cycles=${metrics.observedSteadyFullCycles}",
      s"steady_window_cycles=${metrics.steadyWindowCycles}",
      f"array_utilization=${metrics.steadyArrayUtilization}%.6f",
      f"stream_utilization=${metrics.streamUtilization}%.6f",
      s"measured_window_start_cycle=${metrics.window.startCycle}",
      s"measured_window_end_cycle=${metrics.window.endCycle}",
      s"measured_window_cycles=${metrics.window.measuredWindowCycles}",
      s"measured_feed_cycles=${metrics.window.measuredFeedCycles}",
      s"measured_stall_sample_cycles=${metrics.window.measuredStallSampleCycles}",
      s"feed_duty_window_cycles=${metrics.window.feedDutyWindowCycles}",
      f"feed_duty=${metrics.feedDuty}%.6f",
      s"req_fire_a_cycles=${metrics.window.reqFireACycles}",
      s"req_fire_b_cycles=${metrics.window.reqFireBCycles}",
      f"req_duty_a=${metrics.reqDutyA}%.6f",
      f"req_duty_b=${metrics.reqDutyB}%.6f",
      s"bucket_prefetch_setup_cycles=${metrics.window.bucketPrefetchSetupCycles}",
      s"bucket_bank_hazard_wait_cycles=${metrics.window.bucketBankHazardWaitCycles}",
      s"bucket_drain_enqueue_bookkeeping_cycles=${metrics.window.bucketDrainEnqueueBookkeepingCycles}",
      s"bucket_output_backpressure_cycles=${metrics.window.bucketOutputBackpressureCycles}",
      s"feed_run_count=${metrics.window.feedRunCount}",
      s"feed_run_longest_cycles=${metrics.window.feedRunLongestCycles}",
      f"feed_run_average_cycles=${metrics.window.feedRunAverageCycles}%.6f",
      s"feed_break_count=${metrics.window.feedBreakCount}",
      s"feed_break_prefetch_setup=${metrics.window.feedBreakPrefetchSetup}",
      s"feed_break_bank_hazard_wait=${metrics.window.feedBreakBankHazardWait}",
      s"feed_break_drain_enqueue_bookkeeping=${metrics.window.feedBreakDrainEnqueueBookkeeping}",
      s"feed_break_output_backpressure=${metrics.window.feedBreakOutputBackpressure}",
      s"feed_break_other=${metrics.window.feedBreakOther}",
      s"feed_break_classified_count=${metrics.window.feedBreaksClassified}",
      s"bucket_non_feed_cycles=${metrics.window.nonFeedCycles}",
      s"bucket_non_feed_sum_cycles=${metrics.window.bucketedNonFeedCycles}",
      s"stall_no_step_cycles=${metrics.steadyStalls.stallNoStepCycles}",
      s"stall_a_not_ready_cycles=${metrics.steadyStalls.stallANotReadyCycles}",
      s"stall_b_not_ready_cycles=${metrics.steadyStalls.stallBNotReadyCycles}",
      s"stall_bank_hazard_cycles=${metrics.steadyStalls.stallBankHazardCycles}",
      s"stall_drain_blocked_cycles=${metrics.steadyStalls.stallDrainBlockedCycles}",
      s"stall_output_backpressure_cycles=${metrics.steadyStalls.stallOutputBackpressureCycles}",
      s"stall_error_flush_cycles=${metrics.steadyStalls.stallErrorFlushCycles}",
      s"stall_prefetch_setup_cycles=${metrics.steadyStalls.stallPrefetchSetupCycles}",
      s"stall_bank_hazard_wait_cycles=${metrics.steadyStalls.stallBankHazardWaitCycles}",
      s"stall_drain_enqueue_bookkeeping_cycles=${metrics.steadyStalls.stallDrainEnqueueBookkeepingCycles}",
      s"output_beat_count=${metrics.outputBeatCount}",
      s"expected_output_beat_count=${metrics.expectedOutputBeatCount}",
      s"output_first_cycle=${metrics.outputFirstCycle}",
      s"output_last_cycle=${metrics.outputLastCycle}",
      s"max_outstanding_a=${metrics.maxOutstandingA}",
      s"max_outstanding_b=${metrics.maxOutstandingB}"
    )

    Files.write(outPath, lines.asJava, StandardCharsets.UTF_8)
    outPath
  }

  private def sampleCounters(dut: SystolicMatmul): CounterSnapshot =
    CounterSnapshot(
      injectWindowCycles = dut.utilInjectWindowCycles.toBigInt,
      injectFullCycles = dut.utilInjectFullCycles.toBigInt,
      stallNoStepCycles = dut.utilStallNoStepCycles.toBigInt,
      stallANotReadyCycles = dut.utilStallANotReadyCycles.toBigInt,
      stallBNotReadyCycles = dut.utilStallBNotReadyCycles.toBigInt,
      stallBankHazardCycles = dut.utilStallBankHazardCycles.toBigInt,
      stallDrainBlockedCycles = dut.utilStallDrainBlockedCycles.toBigInt,
      stallOutputBackpressureCycles = dut.utilStallOutputBackpressureCycles.toBigInt,
      stallErrorFlushCycles = dut.utilStallErrorFlushCycles.toBigInt,
      stallPrefetchSetupCycles = dut.utilStallPrefetchSetupCycles.toBigInt,
      stallBankHazardWaitCycles = dut.utilStallBankHazardWaitCycles.toBigInt,
      stallDrainEnqueueBookkeepingCycles = dut.utilStallDrainEnqueueBookkeepingCycles.toBigInt
    )

  private def expectedStepCount(cmd: CommandDesc, s: Int): Int = {
    val numPi = cmd.m / cmd.primM
    val numPj = cmd.n / cmd.primN
    val numPkCmd = cmd.k / cmd.primK
    val numMi = cmd.primM / s
    val numNj = cmd.primN / s
    val numPk = cmd.primK / s
    numPi * numPj * numPkCmd * numMi * numNj * numPk
  }

  private def expectedFullCycles(cmd: CommandDesc, s: Int): BigInt = BigInt(expectedStepCount(cmd, s)) * BigInt(s)

  private def expectedOutputBeats(cmd: CommandDesc, s: Int): Int = cmd.m * (cmd.ldd / s)

  private def mkThroughputCmd(cmdId: Int, baseAddr: BigInt, s: Int): CommandDesc =
    CommandDesc(
      cmdId = cmdId,
      aBase = baseAddr,
      bBase = baseAddr + BigInt(0x100000),
      dBase = baseAddr + BigInt(0x200000),
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

  private def mkScaledCmd(
      cmdId: Int,
      baseAddr: BigInt,
      s: Int,
      mMul: Int,
      nMul: Int,
      kMul: Int,
      primMMul: Int,
      primNMul: Int,
      primKMul: Int
  ): CommandDesc = {
    val m = mMul * s
    val n = nMul * s
    val k = kMul * s
    val primM = primMMul * s
    val primN = primNMul * s
    val primK = primKMul * s
    CommandDesc(
      cmdId = cmdId,
      aBase = baseAddr,
      bBase = baseAddr + BigInt(0x100000),
      dBase = baseAddr + BigInt(0x200000),
      m = m,
      n = n,
      k = k,
      lda = k,
      ldb = n,
      ldd = n,
      primM = primM,
      primN = primN,
      primK = primK
    )
  }

  private def mkMixedPrimitiveBatch(startCmdId: Int, baseAddr: BigInt, s: Int): Seq[CommandDesc] =
    Seq(
      mkScaledCmd(startCmdId + 0, baseAddr + BigInt(0x000000), s, mMul = 4, nMul = 4, kMul = 16, primMMul = 4, primNMul = 4, primKMul = 16),
      mkScaledCmd(startCmdId + 1, baseAddr + BigInt(0x080000), s, mMul = 8, nMul = 4, kMul = 16, primMMul = 4, primNMul = 2, primKMul = 16),
      mkScaledCmd(startCmdId + 2, baseAddr + BigInt(0x100000), s, mMul = 4, nMul = 8, kMul = 16, primMMul = 2, primNMul = 4, primKMul = 16),
      mkScaledCmd(startCmdId + 3, baseAddr + BigInt(0x180000), s, mMul = 8, nMul = 8, kMul = 16, primMMul = 4, primNMul = 4, primKMul = 16)
    )

  private def buildWindowMetrics(scenario: String, samples: Seq[CycleSample], startCycle: Long, endCycle: Long): WindowMetrics = {
    require(endCycle >= startCycle, s"$scenario invalid window start=$startCycle end=$endCycle")

    val inWindow = samples.filter(s => s.cycle >= startCycle && s.cycle <= endCycle)
    assert(inWindow.nonEmpty, s"$scenario measured window has no samples")

    val measuredWindowCycles = BigInt(inWindow.size)
    val measuredFeedCycles = BigInt(inWindow.count(_.feed))
    val measuredStallSampleCycles = BigInt(inWindow.count(_.stallSample))
    val reqFireACycles = BigInt(inWindow.count(_.aReqFire))
    val reqFireBCycles = BigInt(inWindow.count(_.bReqFire))

    val bucketPrefetchSetupCycles = BigInt(inWindow.count(s => s.stallSample && s.bucketPrefetchSetup))
    val bucketBankHazardWaitCycles = BigInt(inWindow.count(s => s.stallSample && s.bucketBankHazardWait))
    val bucketDrainEnqueueBookkeepingCycles = BigInt(inWindow.count(s => s.stallSample && s.bucketDrainEnqueueBookkeeping))
    val bucketOutputBackpressureCycles = BigInt(inWindow.count(s => s.stallSample && s.bucketOutputBackpressure))

    val feedRunLengths = mutable.ArrayBuffer.empty[Int]
    var feedRun = 0
    inWindow.foreach { s =>
      if (s.feed) {
        feedRun += 1
      } else if (feedRun > 0) {
        feedRunLengths += feedRun
        feedRun = 0
      }
    }
    if (feedRun > 0) feedRunLengths += feedRun

    val feedRunCount = BigInt(feedRunLengths.size)
    val feedRunLongestCycles = BigInt(if (feedRunLengths.nonEmpty) feedRunLengths.max else 0)
    val feedRunAverageCycles = if (feedRunLengths.nonEmpty) feedRunLengths.sum.toDouble / feedRunLengths.size.toDouble else 0.0

    val hasFutureFeed = Array.fill(inWindow.size)(false)
    var seenFeedAhead = false
    var idx = inWindow.size - 1
    while (idx >= 0) {
      hasFutureFeed(idx) = seenFeedAhead
      if (inWindow(idx).feed) seenFeedAhead = true
      idx -= 1
    }

    var feedBreakCount = BigInt(0)
    var feedBreakPrefetchSetup = BigInt(0)
    var feedBreakBankHazardWait = BigInt(0)
    var feedBreakDrainEnqueueBookkeeping = BigInt(0)
    var feedBreakOutputBackpressure = BigInt(0)
    var feedBreakOther = BigInt(0)
    var prevFeed = false
    inWindow.zipWithIndex.foreach { case (sample, i) =>
      val isBreakStart = !sample.feed && prevFeed && hasFutureFeed(i)
      if (isBreakStart) {
        feedBreakCount += 1
        if (sample.bucketOutputBackpressure) feedBreakOutputBackpressure += 1
        else if (sample.bucketDrainEnqueueBookkeeping) feedBreakDrainEnqueueBookkeeping += 1
        else if (sample.bucketBankHazardWait) feedBreakBankHazardWait += 1
        else if (sample.bucketPrefetchSetup) feedBreakPrefetchSetup += 1
        else feedBreakOther += 1
      }
      prevFeed = sample.feed
    }

    WindowMetrics(
      startCycle = startCycle,
      endCycle = endCycle,
      measuredWindowCycles = measuredWindowCycles,
      measuredFeedCycles = measuredFeedCycles,
      measuredStallSampleCycles = measuredStallSampleCycles,
      reqFireACycles = reqFireACycles,
      reqFireBCycles = reqFireBCycles,
      bucketPrefetchSetupCycles = bucketPrefetchSetupCycles,
      bucketBankHazardWaitCycles = bucketBankHazardWaitCycles,
      bucketDrainEnqueueBookkeepingCycles = bucketDrainEnqueueBookkeepingCycles,
      bucketOutputBackpressureCycles = bucketOutputBackpressureCycles,
      feedRunCount = feedRunCount,
      feedRunLongestCycles = feedRunLongestCycles,
      feedRunAverageCycles = feedRunAverageCycles,
      feedBreakCount = feedBreakCount,
      feedBreakPrefetchSetup = feedBreakPrefetchSetup,
      feedBreakBankHazardWait = feedBreakBankHazardWait,
      feedBreakDrainEnqueueBookkeeping = feedBreakDrainEnqueueBookkeeping,
      feedBreakOutputBackpressure = feedBreakOutputBackpressure,
      feedBreakOther = feedBreakOther
    )
  }

  private def runScenarioWithCommands(
      compiled: SimCompiled[SystolicMatmul],
      simName: String,
      scenario: String,
      cfg: HarnessConfig,
      cmds: Seq[CommandDesc],
      seedBase: Int
  ): UtilizationMetrics = {
    require(cmds.nonEmpty, "cmd list must be non-empty")

    var captured: Option[UtilizationMetrics] = None

    compiled.doSim(simName) { dut =>
      dut.clockDomain.forkStimulus(2)
      val tb = new SystolicMatmulTb(dut, cfg)
      tb.startAgents()
      dut.clockDomain.waitSampling(2)

      val s = cfg.dutCfg.s

      val queued = cmds.zipWithIndex.map { case (cmd, i) =>
        val a = mkRows(cmd.m, cmd.k, cmd.lda, seedBase + i * 2)
        val b = mkRows(cmd.k, cmd.n, cmd.ldb, seedBase + i * 2 + 1)
        tb.enqueueCommand(cmd, a, b)
      }

      val samples = mutable.ArrayBuffer.empty[CycleSample]
      var firstFeedStartCycle: Option[Long] = None
      var firstFeedCycle: Option[Long] = None
      var lastFeedCycle: Option[Long] = None
      @volatile var monitorStop = false
      var simCycle = 0L

      fork {
        while (!monitorStop) {
          dut.clockDomain.waitSampling()
          val cycle = simCycle
          simCycle += 1

          if (dut.traceFeedStart.toBoolean && firstFeedStartCycle.isEmpty) {
            firstFeedStartCycle = Some(cycle)
          }

          if (firstFeedStartCycle.isDefined) {
            val feed = dut.utilInjectFullCycle.toBoolean
            val stallSample = dut.utilStallSample.toBoolean
            val aReqFire = dut.io.a_rd_req_valid.toBoolean && dut.io.a_rd_req_ready.toBoolean
            val bReqFire = dut.io.b_rd_req_valid.toBoolean && dut.io.b_rd_req_ready.toBoolean

            if (feed) {
              if (firstFeedCycle.isEmpty) firstFeedCycle = Some(cycle)
              lastFeedCycle = Some(cycle)
            }

            val bucketPrefetchSetup = dut.utilStallCausePrefetchSetup.toBoolean
            val bucketBankHazardWait = dut.utilStallCauseBankHazardWait.toBoolean
            val bucketDrainEnqueueBookkeeping = dut.utilStallCauseDrainEnqueueBookkeeping.toBoolean
            val bucketOutputBackpressure = dut.utilStallCauseOutputBackpressure.toBoolean

            if (stallSample) {
              val bucketHot =
                (if (bucketPrefetchSetup) 1 else 0) +
                  (if (bucketBankHazardWait) 1 else 0) +
                  (if (bucketDrainEnqueueBookkeeping) 1 else 0) +
                  (if (bucketOutputBackpressure) 1 else 0)
              assert(
                bucketHot == 1,
                s"$scenario non-FEED bucket attribution violation at cycle=$cycle prefetch=$bucketPrefetchSetup bankHazard=$bucketBankHazardWait drainEnq=$bucketDrainEnqueueBookkeeping outBp=$bucketOutputBackpressure"
              )
            }

            samples += CycleSample(
              cycle = cycle,
              feed = feed,
              stallSample = stallSample,
              aReqFire = aReqFire,
              bReqFire = bReqFire,
              bucketPrefetchSetup = bucketPrefetchSetup,
              bucketBankHazardWait = bucketBankHazardWait,
              bucketDrainEnqueueBookkeeping = bucketDrainEnqueueBookkeeping,
              bucketOutputBackpressure = bucketOutputBackpressure
            )
          }
        }
      }

      val startCounters = sampleCounters(dut)

      queued.foreach { q =>
        assert(tb.awaitCommand(q).isInstanceOf[CommandSuccess], s"$scenario failed cmdId=${q.cmd.cmdId}")
      }

      monitorStop = true
      dut.clockDomain.waitSampling(2)

      val endCounters = sampleCounters(dut)
      val steady = endCounters - startCounters
      val expectedSteady = cmds.map(c => expectedFullCycles(c, s)).foldLeft(BigInt(0))(_ + _)

      val startCycle = firstFeedCycle.getOrElse(fail(s"$scenario missing first FEED cycle"))
      val endCycle = lastFeedCycle.getOrElse(fail(s"$scenario missing last FEED cycle"))
      val window = buildWindowMetrics(scenario, samples.toSeq, startCycle, endCycle)

      val fireCycles = tb.out.fireCycles.toSeq
      val expectedBeats = cmds.map(c => expectedOutputBeats(c, s)).sum
      assert(fireCycles.length == expectedBeats, s"$scenario output beat mismatch got=${fireCycles.length} exp=$expectedBeats")

      val metrics = UtilizationMetrics(
        scenario = scenario,
        tag = s"s$s",
        s = s,
        cmdCount = cmds.length,
        expectedSteadyFullCycles = expectedSteady,
        observedSteadyFullCycles = steady.injectFullCycles,
        steadyWindowCycles = steady.injectWindowCycles,
        steadyStalls = steady,
        totalStalls = endCounters,
        window = window,
        outputBeatCount = fireCycles.length,
        expectedOutputBeatCount = expectedBeats,
        outputFirstCycle = fireCycles.headOption.getOrElse(0L),
        outputLastCycle = fireCycles.lastOption.getOrElse(0L),
        maxOutstandingA = tb.aMem.maxOutstandingObserved,
        maxOutstandingB = tb.bMem.maxOutstandingObserved
      )

      captured = Some(metrics)
    }

    captured.getOrElse(fail(s"$scenario did not capture metrics"))
  }

  private def runScenario(
      compiled: SimCompiled[SystolicMatmul],
      simName: String,
      scenario: String,
      cfg: HarnessConfig,
      cmdCount: Int,
      seedBase: Int
  ): UtilizationMetrics = {
    require(cmdCount >= 1, "cmdCount must be >= 1")
    val s = cfg.dutCfg.s
    val cmds = (0 until cmdCount).map { i =>
      mkThroughputCmd(
        cmdId = 7000 + i,
        baseAddr = BigInt(0x1000000L) + BigInt(i) * 0x800000,
        s = s
      )
    }
    runScenarioWithCommands(compiled, simName, scenario, cfg, cmds, seedBase)
  }

  private def assertCommonStallBudgets(metrics: UtilizationMetrics, label: String): Unit = {
    assert(metrics.steadyStalls.stallNoStepCycles == 0, s"$label no_step must be 0")
    assert(metrics.steadyStalls.stallDrainBlockedCycles == 0, s"$label drain_blocked must be 0")
    assert(metrics.steadyStalls.stallErrorFlushCycles == 0, s"$label error_flush must be 0")
  }

  private def assertMeasuredAttribution(metrics: UtilizationMetrics, label: String): Unit = {
    assert(metrics.window.measuredWindowCycles > 0, s"$label measured window must be > 0")
    assert(metrics.window.endCycle >= metrics.window.startCycle, s"$label invalid measured window range")
    assert(metrics.window.bucketedNonFeedCycles == metrics.window.nonFeedCycles,
      s"$label non-FEED cycles not fully attributed: nonFeed=${metrics.window.nonFeedCycles} bucketed=${metrics.window.bucketedNonFeedCycles}")
    assert(metrics.window.feedDutyWindowCycles > 0, s"$label feed_duty window must be > 0")
    assert(metrics.window.feedBreaksClassified == metrics.window.feedBreakCount,
      s"$label feed break causes not fully attributed: breaks=${metrics.window.feedBreakCount} classified=${metrics.window.feedBreaksClassified}")
    assert(metrics.window.feedRunCount >= 0, s"$label feed run count must be >= 0")
    assert(metrics.window.feedRunLongestCycles >= 0, s"$label feed longest run must be >= 0")
    if (metrics.window.feedRunCount == 0) {
      assert(metrics.window.measuredFeedCycles == 0, s"$label zero run-count with non-zero feed cycles")
      assert(metrics.window.feedBreakCount == 0, s"$label zero run-count must imply zero feed breaks")
    } else {
      val reconstructedFeedAvg = metrics.window.measuredFeedCycles.toDouble / metrics.window.feedRunCount.toDouble
      val feedAvgDelta = math.abs(reconstructedFeedAvg - metrics.window.feedRunAverageCycles)
      assert(feedAvgDelta <= 1e-9, f"$label feed run average mismatch: expected=$reconstructedFeedAvg%.9f got=${metrics.window.feedRunAverageCycles}%.9f")
      assert(metrics.window.feedRunLongestCycles <= metrics.window.measuredFeedCycles, s"$label longest run exceeds measured feed cycles")
      assert(metrics.window.feedBreakCount <= (metrics.window.feedRunCount - 1), s"$label feed break count exceeds max for run-count")
    }
    assert(metrics.feedDuty >= 0.0 && metrics.feedDuty <= 1.0, s"$label feed_duty out of range")
    assert(metrics.reqDutyA >= 0.0 && metrics.reqDutyA <= 1.0, s"$label req_duty_a out of range")
    assert(metrics.reqDutyB >= 0.0 && metrics.reqDutyB <= 1.0, s"$label req_duty_b out of range")
  }

  private def assertFeedDutyStable(reference: UtilizationMetrics, repeat: UtilizationMetrics, label: String, tolerance: Double): Unit = {
    val delta = math.abs(reference.feedDuty - repeat.feedDuty)
    assert(
      delta <= tolerance,
      f"$label feed_duty unstable across seeds: ref=${reference.feedDuty}%.6f repeat=${repeat.feedDuty}%.6f delta=$delta%.6f tol=$tolerance%.6f"
    )
  }

  private def assertIdealHazardSplit(
      metrics: UtilizationMetrics,
      label: String,
      drainStepBudget: BigInt
  ): Unit = {
    val trueBankHazardBudget = BigInt(4)
    val bookkeepingBudget = drainStepBudget + 4

    assert(
      metrics.window.bucketBankHazardWaitCycles <= trueBankHazardBudget,
      s"$label true bank-hazard wait too high: got=${metrics.window.bucketBankHazardWaitCycles} budget<=$trueBankHazardBudget"
    )
    assert(
      metrics.steadyStalls.stallBankHazardCycles <= trueBankHazardBudget,
      s"$label stall_bank_hazard too high: got=${metrics.steadyStalls.stallBankHazardCycles} budget<=$trueBankHazardBudget"
    )
    assert(
      metrics.window.bucketDrainEnqueueBookkeepingCycles <= bookkeepingBudget,
      s"$label drain-enqueue bookkeeping too high: got=${metrics.window.bucketDrainEnqueueBookkeepingCycles} budget<=$bookkeepingBudget"
    )
    assert(
      metrics.steadyStalls.stallDrainEnqueueBookkeepingCycles <= bookkeepingBudget,
      s"$label stall_drain_enqueue_bookkeeping too high: got=${metrics.steadyStalls.stallDrainEnqueueBookkeepingCycles} budget<=$bookkeepingBudget"
    )
  }

  private def runGateSuite(tag: String, baseCfg: HarnessConfig): Unit = {
    val s = baseCfg.dutCfg.s
    val cmdCount = 1
    val compiled = SpinalSimConfig().withVerilator.compile(SystolicMatmul(baseCfg.dutCfg))
    val idealCmd = mkThroughputCmd(cmdId = 7000, baseAddr = BigInt(0x1000000L), s = s)
    val idealDrainStepBudget = BigInt(expectedStepCount(idealCmd, s))

    val idealCfg = baseCfg.copy(
      memoryCfg = MemoryAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        latencyModel = MemoryLatencyModel.Fixed(0),
        reorderWindow = 0,
        seed = 700 + s
      ),
      outputCfg = OutputAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        backpressureMode = BackpressureMode.AlwaysReady,
        seed = 710 + s
      )
    )

    val ideal = runScenario(
      compiled = compiled,
      simName = s"util_gate_${tag}_ideal",
      scenario = "ideal",
      cfg = idealCfg,
      cmdCount = cmdCount,
      seedBase = 1000 + s
    )

    val idealRepeat = runScenario(
      compiled = compiled,
      simName = s"util_gate_${tag}_ideal_repeat",
      scenario = "ideal_repeat",
      cfg = idealCfg.copy(
        memoryCfg = idealCfg.memoryCfg.copy(seed = idealCfg.memoryCfg.seed + 101),
        outputCfg = idealCfg.outputCfg.copy(seed = idealCfg.outputCfg.seed + 103)
      ),
      cmdCount = cmdCount,
      seedBase = 11000 + s
    )

    assert(ideal.observedSteadyFullCycles == ideal.expectedSteadyFullCycles, s"ideal $tag steady full-cycle mismatch")
    assert(ideal.steadyArrayUtilization == 1.0, s"ideal $tag array_util must be exactly 1.0")
    assert(ideal.streamUtilization >= 0.98, f"ideal $tag stream_util=${ideal.streamUtilization}%.6f < 0.98")
    assertCommonStallBudgets(ideal, s"ideal $tag")
    assertMeasuredAttribution(ideal, s"ideal $tag")
    assertFeedDutyStable(ideal, idealRepeat, s"ideal $tag", tolerance = 1e-6)
    assertIdealHazardSplit(ideal, s"ideal $tag", drainStepBudget = idealDrainStepBudget)
    assertIdealHazardSplit(idealRepeat, s"ideal_repeat $tag", drainStepBudget = idealDrainStepBudget)
    assert(ideal.steadyStalls.stallOutputBackpressureCycles == 0, s"ideal $tag output_backpressure must be 0")
    val idealOut = writeMetrics(ideal, s"ideal_${tag}")
    info(
      f"[util-gate $tag ideal] array=${ideal.steadyArrayUtilization}%.6f stream=${ideal.streamUtilization}%.6f " +
        f"feed_duty=${ideal.feedDuty}%.6f reqA=${ideal.reqDutyA}%.6f reqB=${ideal.reqDutyB}%.6f " +
        f"feedRunLongest=${ideal.window.feedRunLongestCycles} feedRunAvg=${ideal.window.feedRunAverageCycles}%.3f " +
        s"feedBreaks=${ideal.window.feedBreakCount} file=${idealOut.toAbsolutePath}"
    )

    val idealFeedCmd = mkScaledCmd(
      cmdId = 7900 + s,
      baseAddr = BigInt(0x2400000L),
      s = s,
      mMul = 4,
      nMul = 4,
      kMul = 16,
      primMMul = 4,
      primNMul = 4,
      primKMul = 16
    )
    val idealFeed = runScenarioWithCommands(
      compiled = compiled,
      simName = s"util_gate_${tag}_ideal_feed_dense",
      scenario = "ideal_feed_dense",
      cfg = idealCfg,
      cmds = Seq(idealFeedCmd),
      seedBase = 12500 + s
    )
    assert(idealFeed.observedSteadyFullCycles == idealFeed.expectedSteadyFullCycles, s"ideal_feed_dense $tag steady full-cycle mismatch")
    assert(idealFeed.steadyArrayUtilization == 1.0, s"ideal_feed_dense $tag array_util must be exactly 1.0")
    assert(idealFeed.streamUtilization >= 0.98, f"ideal_feed_dense $tag stream_util=${idealFeed.streamUtilization}%.6f < 0.98")
    assertCommonStallBudgets(idealFeed, s"ideal_feed_dense $tag")
    assertMeasuredAttribution(idealFeed, s"ideal_feed_dense $tag")
    assert(idealFeed.feedDuty >= 0.95, f"ideal_feed_dense $tag feed_duty=${idealFeed.feedDuty}%.6f < 0.95")
    assert(idealFeed.window.feedRunLongestCycles >= BigInt(5 * s),
      s"ideal_feed_dense $tag longest FEED run too short: got=${idealFeed.window.feedRunLongestCycles} floor=${5 * s}")
    assert(idealFeed.window.feedRunAverageCycles >= (1.2 * s),
      f"ideal_feed_dense $tag average FEED run too short: got=${idealFeed.window.feedRunAverageCycles}%.3f floor=${1.2 * s}%.3f")
    val idealFeedOut = writeMetrics(idealFeed, s"ideal_feed_dense_${tag}")
    info(
      f"[util-gate $tag ideal_feed_dense] array=${idealFeed.steadyArrayUtilization}%.6f stream=${idealFeed.streamUtilization}%.6f " +
        f"feed_duty=${idealFeed.feedDuty}%.6f feedRunLongest=${idealFeed.window.feedRunLongestCycles} " +
        f"feedRunAvg=${idealFeed.window.feedRunAverageCycles}%.3f feedBreaks=${idealFeed.window.feedBreakCount} " +
        s"file=${idealFeedOut.toAbsolutePath}"
    )

    val longBatchCmdCount = if (s <= 4) 6 else 3
    val longBatchCmds = (0 until longBatchCmdCount).map { i =>
      mkScaledCmd(
        cmdId = 8000 + i,
        baseAddr = BigInt(0x3000000L) + BigInt(i) * 0x800000,
        s = s,
        mMul = 4,
        nMul = 4,
        kMul = 16,
        primMMul = 4,
        primNMul = 4,
        primKMul = 16
      )
    }

    val longBatch = runScenarioWithCommands(
      compiled = compiled,
      simName = s"util_gate_${tag}_ideal_long_batch",
      scenario = "ideal_long_batch",
      cfg = idealCfg,
      cmds = longBatchCmds,
      seedBase = 13000 + s
    )

    assert(longBatch.observedSteadyFullCycles == longBatch.expectedSteadyFullCycles, s"ideal_long_batch $tag steady full-cycle mismatch")
    assert(longBatch.steadyArrayUtilization == 1.0, s"ideal_long_batch $tag array_util must be exactly 1.0")
    assertCommonStallBudgets(longBatch, s"ideal_long_batch $tag")
    assertMeasuredAttribution(longBatch, s"ideal_long_batch $tag")
    assert(longBatch.feedDuty >= 0.95, f"ideal_long_batch $tag feed_duty=${longBatch.feedDuty}%.6f < 0.95")
    assert(longBatch.window.feedRunLongestCycles >= BigInt(5 * s),
      s"ideal_long_batch $tag longest FEED run too short: got=${longBatch.window.feedRunLongestCycles} floor=${5 * s}")
    assert(longBatch.window.feedRunAverageCycles >= (1.2 * s),
      f"ideal_long_batch $tag average FEED run too short: got=${longBatch.window.feedRunAverageCycles}%.3f floor=${1.2 * s}%.3f")
    assert(longBatch.window.feedBreakOutputBackpressure == 0,
      s"ideal_long_batch $tag feed breaks due to output backpressure must be 0")
    val longBatchOut = writeMetrics(longBatch, s"ideal_long_batch_${tag}")
    info(
      f"[util-gate $tag ideal_long_batch] array=${longBatch.steadyArrayUtilization}%.6f stream=${longBatch.streamUtilization}%.6f " +
        f"feed_duty=${longBatch.feedDuty}%.6f feedRunLongest=${longBatch.window.feedRunLongestCycles} " +
        f"feedRunAvg=${longBatch.window.feedRunAverageCycles}%.3f feedBreaks=${longBatch.window.feedBreakCount} " +
        s"file=${longBatchOut.toAbsolutePath}"
    )

    val mixedCmds = mkMixedPrimitiveBatch(startCmdId = 9000, baseAddr = BigInt(0x3800000L), s = s)
    val mixed = runScenarioWithCommands(
      compiled = compiled,
      simName = s"util_gate_${tag}_ideal_mixed_prims",
      scenario = "ideal_mixed_prims",
      cfg = idealCfg,
      cmds = mixedCmds,
      seedBase = 14000 + s
    )

    assert(mixed.observedSteadyFullCycles == mixed.expectedSteadyFullCycles, s"ideal_mixed_prims $tag steady full-cycle mismatch")
    assert(mixed.steadyArrayUtilization == 1.0, s"ideal_mixed_prims $tag array_util must be exactly 1.0")
    assertCommonStallBudgets(mixed, s"ideal_mixed_prims $tag")
    assertMeasuredAttribution(mixed, s"ideal_mixed_prims $tag")
    assert(mixed.feedDuty >= 0.95, f"ideal_mixed_prims $tag feed_duty=${mixed.feedDuty}%.6f < 0.95")
    assert(mixed.window.feedRunLongestCycles >= BigInt(4 * s),
      s"ideal_mixed_prims $tag longest FEED run too short: got=${mixed.window.feedRunLongestCycles} floor=${4 * s}")
    assert(mixed.window.feedRunAverageCycles >= (1.2 * s),
      f"ideal_mixed_prims $tag average FEED run too short: got=${mixed.window.feedRunAverageCycles}%.3f floor=${1.2 * s}%.3f")
    assert(mixed.window.feedBreakOutputBackpressure == 0,
      s"ideal_mixed_prims $tag feed breaks due to output backpressure must be 0")
    val mixedOut = writeMetrics(mixed, s"ideal_mixed_prims_${tag}")
    info(
      f"[util-gate $tag ideal_mixed_prims] array=${mixed.steadyArrayUtilization}%.6f stream=${mixed.streamUtilization}%.6f " +
        f"feed_duty=${mixed.feedDuty}%.6f feedRunLongest=${mixed.window.feedRunLongestCycles} " +
        f"feedRunAvg=${mixed.window.feedRunAverageCycles}%.3f feedBreaks=${mixed.window.feedBreakCount} " +
        s"file=${mixedOut.toAbsolutePath}"
    )

    val randomCfg = baseCfg.copy(
      memoryCfg = MemoryAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        latencyModel = MemoryLatencyModel.RandomRange(1, baseCfg.dutCfg.maxOutstandingRd + 8),
        reorderWindow = (baseCfg.dutCfg.maxOutstandingRd / 2).max(1),
        seed = 720 + s
      ),
      outputCfg = OutputAgentConfig(
        clBytes = baseCfg.dutCfg.clBytes,
        backpressureMode = BackpressureMode.AlwaysReady,
        seed = 730 + s
      )
    )

    val randomLat = runScenario(
      compiled = compiled,
      simName = s"util_gate_${tag}_randlat",
      scenario = "random_latency",
      cfg = randomCfg,
      cmdCount = cmdCount,
      seedBase = 2000 + s
    )

    val randomLatRepeat = runScenario(
      compiled = compiled,
      simName = s"util_gate_${tag}_randlat_repeat",
      scenario = "random_latency_repeat",
      cfg = randomCfg.copy(
        memoryCfg = randomCfg.memoryCfg.copy(seed = randomCfg.memoryCfg.seed + 107),
        outputCfg = randomCfg.outputCfg.copy(seed = randomCfg.outputCfg.seed + 109)
      ),
      cmdCount = cmdCount,
      seedBase = 12000 + s
    )

    assert(randomLat.observedSteadyFullCycles == randomLat.expectedSteadyFullCycles, s"random_latency $tag steady full-cycle mismatch")
    assert(randomLat.steadyArrayUtilization == 1.0, s"random_latency $tag array_util must be exactly 1.0")
    assertCommonStallBudgets(randomLat, s"random_latency $tag")
    assertMeasuredAttribution(randomLat, s"random_latency $tag")
    assertFeedDutyStable(randomLat, randomLatRepeat, s"random_latency $tag", tolerance = 0.02)
    assert(randomLat.steadyStalls.stallOutputBackpressureCycles == 0, s"random_latency $tag output_backpressure must be 0")
    assert(randomLat.streamUtilization >= 0.92, f"random_latency $tag stream_util=${randomLat.streamUtilization}%.6f < 0.92")
    assert(
      randomLat.maxOutstandingA >= (baseCfg.dutCfg.maxOutstandingRd - 1),
      s"random_latency $tag A outstanding depth too low: got=${randomLat.maxOutstandingA} exp>=${baseCfg.dutCfg.maxOutstandingRd - 1}"
    )
    assert(
      randomLat.maxOutstandingB >= (baseCfg.dutCfg.maxOutstandingRd - 1),
      s"random_latency $tag B outstanding depth too low: got=${randomLat.maxOutstandingB} exp>=${baseCfg.dutCfg.maxOutstandingRd - 1}"
    )
    val randOut = writeMetrics(randomLat, s"random_latency_${tag}")
    info(
      f"[util-gate $tag randlat] array=${randomLat.steadyArrayUtilization}%.6f stream=${randomLat.streamUtilization}%.6f " +
        f"feed_duty=${randomLat.feedDuty}%.6f reqA=${randomLat.reqDutyA}%.6f reqB=${randomLat.reqDutyB}%.6f " +
        s"Amax=${randomLat.maxOutstandingA} Bmax=${randomLat.maxOutstandingB} file=${randOut.toAbsolutePath}"
    )

    val backpressureModes = Seq(
      ("bp0", BackpressureMode.AlwaysReady, 0.10),
      ("bp1", BackpressureMode.PeriodicStall(periodCycles = 8, stallCycles = 1), 0.10),
      ("bp2", BackpressureMode.PeriodicStall(periodCycles = 4, stallCycles = 1), 0.10),
      ("bp3", BackpressureMode.PeriodicStall(periodCycles = 4, stallCycles = 2), 0.10)
    )
    val sweepCmdCount = if (s <= 4) 4 else 2

    val sweep = backpressureModes.zipWithIndex.map { case ((modeTag, bpMode, streamFloor), idx) =>
      val bpCfg = baseCfg.copy(
        memoryCfg = MemoryAgentConfig(
          clBytes = baseCfg.dutCfg.clBytes,
          latencyModel = MemoryLatencyModel.Fixed(0),
          reorderWindow = 0,
          seed = 740 + s + idx
        ),
        outputCfg = OutputAgentConfig(
          clBytes = baseCfg.dutCfg.clBytes,
          backpressureMode = bpMode,
          seed = 760 + s + idx
        )
      )

      val metrics = runScenario(
        compiled = compiled,
        simName = s"util_gate_${tag}_${modeTag}",
        scenario = s"backpressure_${modeTag}",
        cfg = bpCfg,
        cmdCount = sweepCmdCount,
        seedBase = 3000 + s + idx * 20
      )

      assert(metrics.observedSteadyFullCycles == metrics.expectedSteadyFullCycles, s"$modeTag $tag steady full-cycle mismatch")
      assert(metrics.steadyArrayUtilization == 1.0, s"$modeTag $tag array_util must be exactly 1.0")
      assertCommonStallBudgets(metrics, s"$modeTag $tag")
      assertMeasuredAttribution(metrics, s"$modeTag $tag")

      assert(metrics.streamUtilization >= streamFloor, f"$modeTag $tag stream_util=${metrics.streamUtilization}%.6f < $streamFloor%.2f")
      val outPath = writeMetrics(metrics, s"${modeTag}_${tag}")
      info(
        f"[util-gate $tag $modeTag] array=${metrics.steadyArrayUtilization}%.6f stream=${metrics.streamUtilization}%.6f " +
          f"feed_duty=${metrics.feedDuty}%.6f reqA=${metrics.reqDutyA}%.6f reqB=${metrics.reqDutyB}%.6f " +
          s"out_bp=${metrics.steadyStalls.stallOutputBackpressureCycles} file=${outPath.toAbsolutePath}"
      )
      metrics
    }

    info(
      f"[util-gate $tag sweep] bp0=${sweep(0).streamUtilization}%.6f bp1=${sweep(1).streamUtilization}%.6f " +
        f"bp2=${sweep(2).streamUtilization}%.6f bp3=${sweep(3).streamUtilization}%.6f"
    )
  }

  test("utilization gate suite S=4") {
    runGateSuite(tag = "s4", baseCfg = defaultHarnessConfigS4())
  }

  test("utilization gate suite S=16") {
    runGateSuite(tag = "s16", baseCfg = defaultHarnessConfigS16)
  }
}
