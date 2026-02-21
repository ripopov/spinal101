package matmul

import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class SystolicMatmulConfig(
    addrBits: Int = 64,
    clBits: Int = 128,
    fpBits: Int = 32,
    maxOutstandingRd: Int = 16,
    cmdqDepth: Int = 4,
    outFifoDepthCl: Int = 32,
    lMul: Int = 4,
    lAdd: Int = 4
) {
  require(clBits % fpBits == 0, "CL_BITS must be a multiple of FP_BITS")
  require((maxOutstandingRd & (maxOutstandingRd - 1)) == 0, "MAX_OUTSTANDING_RD must be power of 2")

  val clBytes: Int = clBits / 8
  val clElems: Int = clBits / fpBits
  val s: Int = clElems
  val tagBits: Int = log2Up(maxOutstandingRd)
}

case class SystolicMatmul(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val io = new Bundle {
    val cmd_valid = in Bool()
    val cmd_ready = out Bool()
    val cmd_desc_cmd_id = in UInt(16 bits)
    val cmd_desc_a_base = in UInt(cfg.addrBits bits)
    val cmd_desc_b_base = in UInt(cfg.addrBits bits)
    val cmd_desc_d_base = in UInt(cfg.addrBits bits)
    val cmd_desc_m = in UInt(16 bits)
    val cmd_desc_n = in UInt(16 bits)
    val cmd_desc_k = in UInt(16 bits)
    val cmd_desc_lda = in UInt(16 bits)
    val cmd_desc_ldb = in UInt(16 bits)
    val cmd_desc_ldd = in UInt(16 bits)
    val cmd_desc_prim_m = in UInt(16 bits)
    val cmd_desc_prim_n = in UInt(16 bits)
    val cmd_desc_prim_k = in UInt(16 bits)
    val cmd_desc_flags = in Bits(8 bits)

    val a_rd_req_valid = out Bool()
    val a_rd_req_ready = in Bool()
    val a_rd_req_addr = out UInt(cfg.addrBits bits)
    val a_rd_req_tag = out UInt(cfg.tagBits bits)

    val a_rd_rsp_valid = in Bool()
    val a_rd_rsp_ready = out Bool()
    val a_rd_rsp_data = in Bits(cfg.clBits bits)
    val a_rd_rsp_tag = in UInt(cfg.tagBits bits)
    val a_rd_rsp_err = in Bool()

    val b_rd_req_valid = out Bool()
    val b_rd_req_ready = in Bool()
    val b_rd_req_addr = out UInt(cfg.addrBits bits)
    val b_rd_req_tag = out UInt(cfg.tagBits bits)

    val b_rd_rsp_valid = in Bool()
    val b_rd_rsp_ready = out Bool()
    val b_rd_rsp_data = in Bits(cfg.clBits bits)
    val b_rd_rsp_tag = in UInt(cfg.tagBits bits)
    val b_rd_rsp_err = in Bool()

    val d_wr_valid = out Bool()
    val d_wr_ready = in Bool()
    val d_wr_addr = out UInt(cfg.addrBits bits)
    val d_wr_data = out Bits(cfg.clBits bits)
    val d_wr_last = out Bool()
    val d_wr_cmd_id = out UInt(16 bits)

    val sts_valid = out Bool()
    val sts_ready = in Bool()
    val sts_cmd_id = out UInt(16 bits)
    val sts_ok = out Bool()
    val sts_err_code = out Bits(8 bits)
  }

  // ---------- Submodule Instantiation ----------

  val cmdFrontend = CmdFrontend(cfg)
  val tileScheduler = TileScheduler(cfg)
  val readEngineA = ReadEngine(cfg.addrBits, cfg.clBits, cfg.maxOutstandingRd)
  val readEngineB = ReadEngine(cfg.addrBits, cfg.clBits, cfg.maxOutstandingRd)
  val transposeBuffer = TransposeBuffer(cfg.s)
  val systolicCore = SystolicCore(cfg.s, cfg.lMul, cfg.lAdd)
  val drainPacker = DrainPacker(cfg)
  val statusGen = StatusGen(cfg)

  // B feed FIFO: holds S CLs between ReadEngine output and systolic feed
  val bFifo = StreamFifo(Bits(cfg.clBits bits), cfg.s + 2)

  // Output FIFO between DrainPacker and d_wr_* ports
  val outFifo = StreamFifo(DWriteEntry(cfg), cfg.outFifoDepthCl)

  // ---------- Command Frontend Wiring ----------

  cmdFrontend.io.cmdValid := io.cmd_valid
  cmdFrontend.io.cmdDesc.cmdId := io.cmd_desc_cmd_id
  cmdFrontend.io.cmdDesc.aBase := io.cmd_desc_a_base
  cmdFrontend.io.cmdDesc.bBase := io.cmd_desc_b_base
  cmdFrontend.io.cmdDesc.dBase := io.cmd_desc_d_base
  cmdFrontend.io.cmdDesc.m := io.cmd_desc_m
  cmdFrontend.io.cmdDesc.n := io.cmd_desc_n
  cmdFrontend.io.cmdDesc.k := io.cmd_desc_k
  cmdFrontend.io.cmdDesc.lda := io.cmd_desc_lda
  cmdFrontend.io.cmdDesc.ldb := io.cmd_desc_ldb
  cmdFrontend.io.cmdDesc.ldd := io.cmd_desc_ldd
  cmdFrontend.io.cmdDesc.primM := io.cmd_desc_prim_m
  cmdFrontend.io.cmdDesc.primN := io.cmd_desc_prim_n
  cmdFrontend.io.cmdDesc.primK := io.cmd_desc_prim_k
  cmdFrontend.io.cmdDesc.flags := io.cmd_desc_flags
  io.cmd_ready := cmdFrontend.io.cmdReady

  // ---------- Read Engine <-> External Memory ----------

  io.a_rd_req_valid := readEngineA.io.rdReqValid
  io.a_rd_req_addr := readEngineA.io.rdReqAddr
  io.a_rd_req_tag := readEngineA.io.rdReqTag
  readEngineA.io.rdReqReady := io.a_rd_req_ready
  readEngineA.io.rdRspValid := io.a_rd_rsp_valid
  readEngineA.io.rdRspData := io.a_rd_rsp_data
  readEngineA.io.rdRspTag := io.a_rd_rsp_tag
  readEngineA.io.rdRspErr := io.a_rd_rsp_err
  io.a_rd_rsp_ready := readEngineA.io.rdRspReady

  io.b_rd_req_valid := readEngineB.io.rdReqValid
  io.b_rd_req_addr := readEngineB.io.rdReqAddr
  io.b_rd_req_tag := readEngineB.io.rdReqTag
  readEngineB.io.rdReqReady := io.b_rd_req_ready
  readEngineB.io.rdRspValid := io.b_rd_rsp_valid
  readEngineB.io.rdRspData := io.b_rd_rsp_data
  readEngineB.io.rdRspTag := io.b_rd_rsp_tag
  readEngineB.io.rdRspErr := io.b_rd_rsp_err
  io.b_rd_rsp_ready := readEngineB.io.rdRspReady

  // ---------- Output FIFO -> d_wr_* ----------

  io.d_wr_valid := outFifo.io.pop.valid
  io.d_wr_addr := outFifo.io.pop.payload.addr
  io.d_wr_data := outFifo.io.pop.payload.data
  io.d_wr_last := outFifo.io.pop.payload.last
  io.d_wr_cmd_id := outFifo.io.pop.payload.cmdId
  outFifo.io.pop.ready := io.d_wr_ready

  val dWriteLastFire = io.d_wr_valid && io.d_wr_ready && io.d_wr_last

  // ---------- Status Gen -> sts_* ----------

  io.sts_valid := statusGen.io.stsValid
  io.sts_cmd_id := statusGen.io.stsCmdId
  io.sts_ok := statusGen.io.stsOk
  io.sts_err_code := statusGen.io.stsErrCode
  statusGen.io.stsReady := io.sts_ready

  // Rejection path from CmdFrontend -> StatusGen
  statusGen.io.rejValid := cmdFrontend.io.rejValid
  cmdFrontend.io.rejReady := statusGen.io.rejReady
  statusGen.io.rejCmdId := cmdFrontend.io.rejCmdId
  statusGen.io.rejErrCode := cmdFrontend.io.rejErrCode

  // ---------- DrainPacker -> Output FIFO ----------

  outFifo.io.push.valid := drainPacker.io.outValid
  outFifo.io.push.payload.addr := drainPacker.io.outAddr
  outFifo.io.push.payload.data := drainPacker.io.outData
  outFifo.io.push.payload.last := drainPacker.io.outLast
  outFifo.io.push.payload.cmdId := drainPacker.io.outCmdId
  drainPacker.io.outReady := outFifo.io.push.ready

  // ---------- DrainPacker -> SystolicCore drain port ----------

  systolicCore.io.drainBank := drainPacker.io.drainBank
  systolicCore.io.drainRow := drainPacker.io.drainRow
  drainPacker.io.drainData := systolicCore.io.drainData

  // ---------- d_wr completion -> StatusGen ----------

  statusGen.io.cmdDoneValid := dWriteLastFire
  statusGen.io.cmdDoneCmdId := io.d_wr_cmd_id

  // ---------- Controller decomposition ----------

  val ctrlFifoDepth = (cfg.cmdqDepth * 4).max(4)
  val cmdDispatchQ = StreamFifo(DispatchCmdCtx(cfg), cfg.cmdqDepth)
  val stepIssueQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)
  val injectQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)
  val drainQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)

  object PrefetchState extends SpinalEnum {
    val IDLE, START_READS, LOAD_AB, ENQ_INJECT, ERROR_DRAIN, ERROR_STATUS = newElement()
  }
  object InjectState extends SpinalEnum {
    val IDLE, SWAP_TB, FEED, TAIL, ENQ_DRAIN = newElement()
  }
  object DrainState extends SpinalEnum {
    val IDLE, DRAIN_START, DRAIN_WAIT, FLUSH_START, FLUSH_WAIT, STEP_DONE = newElement()
  }

  val prefetchState = RegInit(PrefetchState.IDLE)
  val injectState = RegInit(InjectState.IDLE)
  val drainState = RegInit(DrainState.IDLE)

  val schedCmdCtx = Reg(DispatchCmdCtx(cfg))
  val prefetchCtx = Reg(StepCtx(cfg))
  val injectCtx = Reg(StepCtx(cfg))
  val drainCtx = Reg(StepCtx(cfg))

  val schedCmdActive = Reg(Bool()) init (False)
  val schedStepWaitingDone = Reg(Bool()) init (False)
  val schedAbort = Reg(Bool()) init (False)
  val errorDetected = Reg(Bool()) init (False)
  val errorCmdId = Reg(UInt(16 bits)) init (0)

  val aLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val bLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val feedCnt = Reg(UInt(log2Up(cfg.s) bits)) init (0)
  val tailTotal = 2 * (cfg.s - 1) + cfg.lMul + cfg.lAdd
  val tailCnt = Reg(UInt(log2Up(tailTotal + 1) bits)) init (0)
  val tailTarget = U(tailTotal - 1, log2Up(tailTotal + 1) bits)

  // Explicit hazard scoreboard for compute/drain bank conflicts.
  val bank0ComputeBusy = Reg(Bool()) init (False)
  val bank1ComputeBusy = Reg(Bool()) init (False)
  val bank0DrainBusy = Reg(Bool()) init (False)
  val bank1DrainBusy = Reg(Bool()) init (False)
  val bank0RowBusy = Reg(Bits(cfg.s bits)) init (0)
  val bank1RowBusy = Reg(Bits(cfg.s bits)) init (0)
  val coreBankSel = Reg(Bool()) init (False)

  // ---------- Utilization/Trace Instrumentation (simulation-visible) ----------

  val utilTotalCycles = Reg(UInt(64 bits)) init (0)
  val utilInjectWindowCycles = Reg(UInt(64 bits)) init (0)
  val utilInjectFullCycles = Reg(UInt(64 bits)) init (0)

  val utilStallNoStepCycles = Reg(UInt(64 bits)) init (0)
  val utilStallANotReadyCycles = Reg(UInt(64 bits)) init (0)
  val utilStallBNotReadyCycles = Reg(UInt(64 bits)) init (0)
  val utilStallBankHazardCycles = Reg(UInt(64 bits)) init (0)
  val utilStallDrainBlockedCycles = Reg(UInt(64 bits)) init (0)
  val utilStallOutputBackpressureCycles = Reg(UInt(64 bits)) init (0)
  val utilStallErrorFlushCycles = Reg(UInt(64 bits)) init (0)

  val utilInjectWindowActive = Reg(Bool()) init (False)
  val utilInjectFullCycle = Bool()
  val utilStallSample = Bool()

  val utilStallCauseNoStep = Bool()
  val utilStallCauseANotReady = Bool()
  val utilStallCauseBNotReady = Bool()
  val utilStallCauseBankHazard = Bool()
  val utilStallCauseDrainBlocked = Bool()
  val utilStallCauseOutputBackpressure = Bool()
  val utilStallCauseErrorFlush = Bool()

  val traceCmdAccepted = Bool()
  val traceFeedStart = Bool()
  val traceDrainStart = Bool()
  val traceDrainDone = Bool()

  val traceCmdAcceptedCount = Reg(UInt(32 bits)) init (0)
  val traceFeedStartCount = Reg(UInt(32 bits)) init (0)
  val traceDrainStartCount = Reg(UInt(32 bits)) init (0)
  val traceDrainDoneCount = Reg(UInt(32 bits)) init (0)

  val traceLastCmdAcceptedCycle = Reg(UInt(64 bits)) init (0)
  val traceLastFeedStartCycle = Reg(UInt(64 bits)) init (0)
  val traceLastDrainStartCycle = Reg(UInt(64 bits)) init (0)
  val traceLastDrainDoneCycle = Reg(UInt(64 bits)) init (0)

  utilTotalCycles.simPublic()
  utilInjectWindowCycles.simPublic()
  utilInjectFullCycles.simPublic()

  utilStallNoStepCycles.simPublic()
  utilStallANotReadyCycles.simPublic()
  utilStallBNotReadyCycles.simPublic()
  utilStallBankHazardCycles.simPublic()
  utilStallDrainBlockedCycles.simPublic()
  utilStallOutputBackpressureCycles.simPublic()
  utilStallErrorFlushCycles.simPublic()

  utilInjectWindowActive.simPublic()
  utilInjectFullCycle.simPublic()
  utilStallSample.simPublic()

  utilStallCauseNoStep.simPublic()
  utilStallCauseANotReady.simPublic()
  utilStallCauseBNotReady.simPublic()
  utilStallCauseBankHazard.simPublic()
  utilStallCauseDrainBlocked.simPublic()
  utilStallCauseOutputBackpressure.simPublic()
  utilStallCauseErrorFlush.simPublic()

  traceCmdAccepted.simPublic()
  traceFeedStart.simPublic()
  traceDrainStart.simPublic()
  traceDrainDone.simPublic()

  traceCmdAcceptedCount.simPublic()
  traceFeedStartCount.simPublic()
  traceDrainStartCount.simPublic()
  traceDrainDoneCount.simPublic()

  traceLastCmdAcceptedCycle.simPublic()
  traceLastFeedStartCycle.simPublic()
  traceLastDrainStartCycle.simPublic()
  traceLastDrainDoneCycle.simPublic()

  // ---------- Default assignments ----------

  // Queue defaults
  cmdDispatchQ.io.push.payload.cmdDesc := cmdFrontend.io.outDesc
  cmdDispatchQ.io.push.payload.numPi := cmdFrontend.io.outDesc.m / cmdFrontend.io.outDesc.primM
  cmdDispatchQ.io.push.payload.numPj := cmdFrontend.io.outDesc.n / cmdFrontend.io.outDesc.primN
  cmdDispatchQ.io.push.payload.numPkCmd := cmdFrontend.io.outDesc.k / cmdFrontend.io.outDesc.primK
  cmdDispatchQ.io.push.payload.numMi := cmdFrontend.io.outDesc.primM / U(cfg.s, 16 bits)
  cmdDispatchQ.io.push.payload.numNj := cmdFrontend.io.outDesc.primN / U(cfg.s, 16 bits)
  cmdDispatchQ.io.push.payload.numPk := cmdFrontend.io.outDesc.primK / U(cfg.s, 16 bits)
  cmdDispatchQ.io.pop.ready := False

  stepIssueQ.io.push.valid := False
  stepIssueQ.io.pop.ready := False

  injectQ.io.push.valid := False
  injectQ.io.push.payload := prefetchCtx
  injectQ.io.pop.ready := False

  drainQ.io.push.valid := False
  drainQ.io.push.payload := injectCtx
  drainQ.io.pop.ready := False

  // TileScheduler
  tileScheduler.io.cmdValid := False
  tileScheduler.io.cmdDesc := schedCmdCtx.cmdDesc
  tileScheduler.io.stepReady := False

  // ReadEngine A config
  readEngineA.io.start := False
  readEngineA.io.cfgBase := 0
  readEngineA.io.cfgStride := 0
  readEngineA.io.cfgCount := 0

  // ReadEngine B config
  readEngineB.io.start := False
  readEngineB.io.cfgBase := 0
  readEngineB.io.cfgStride := 0
  readEngineB.io.cfgCount := 0

  // ReadEngine A -> TransposeBuffer
  readEngineA.io.outReady := False
  transposeBuffer.io.wrValid := False
  transposeBuffer.io.wrRowIdx := aLoadCnt.resized
  for (e <- 0 until cfg.s) {
    transposeBuffer.io.wrData(e) := readEngineA.io.outData((e + 1) * 32 - 1 downto e * 32)
  }
  transposeBuffer.io.swap := False
  transposeBuffer.io.rdColIdx := feedCnt

  // ReadEngine B -> B FIFO
  readEngineB.io.outReady := bFifo.io.push.ready
  bFifo.io.push.valid := readEngineB.io.outValid
  bFifo.io.push.payload := readEngineB.io.outData

  // B FIFO -> SystolicCore (default: not popping)
  bFifo.io.pop.ready := False

  // SystolicCore defaults
  for (i <- 0 until cfg.s) {
    systolicCore.io.aIn(i) := transposeBuffer.io.rdData(i)
    systolicCore.io.aValid(i) := False
    systolicCore.io.bIn(i) := bFifo.io.pop.payload((i + 1) * 32 - 1 downto i * 32)
    systolicCore.io.bValid(i) := False
  }
  systolicCore.io.bankSel := coreBankSel
  systolicCore.io.clearBank := False

  // DrainPacker defaults
  drainPacker.io.drainStart := False
  drainPacker.io.flushStart := False
  drainPacker.io.drainBankIn := False
  drainPacker.io.pi := 0
  drainPacker.io.pj := 0
  drainPacker.io.pkCmd := 0
  drainPacker.io.mi := 0
  drainPacker.io.nj := 0
  drainPacker.io.numPkCmd := 0
  drainPacker.io.numMi := 0
  drainPacker.io.numNj := 0
  drainPacker.io.numPi := 0
  drainPacker.io.numPj := 0
  drainPacker.io.cmdDesc.cmdId := 0
  drainPacker.io.cmdDesc.aBase := 0
  drainPacker.io.cmdDesc.bBase := 0
  drainPacker.io.cmdDesc.dBase := 0
  drainPacker.io.cmdDesc.m := 0
  drainPacker.io.cmdDesc.n := 0
  drainPacker.io.cmdDesc.k := 0
  drainPacker.io.cmdDesc.lda := 0
  drainPacker.io.cmdDesc.ldb := 0
  drainPacker.io.cmdDesc.ldd := 0
  drainPacker.io.cmdDesc.primM := 0
  drainPacker.io.cmdDesc.primN := 0
  drainPacker.io.cmdDesc.primK := 0
  drainPacker.io.cmdDesc.flags := 0

  // StatusGen error defaults
  statusGen.io.errValid := False
  statusGen.io.errCmdId := errorCmdId
  statusGen.io.errCode := B(0x02, 8 bits) // readErr

  traceCmdAccepted := False
  traceFeedStart := False
  traceDrainStart := False
  traceDrainDone := False

  // ---------- Address computation ----------

  val sWide = U(cfg.s, cfg.addrBits bits)

  def computeABase(step: StepCtx): UInt = {
    val globalM = (step.pi.resize(cfg.addrBits) * step.cmdDesc.primM.resize(cfg.addrBits) +
      step.mi.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val globalK = (step.pkCmd.resize(cfg.addrBits) * step.cmdDesc.primK.resize(cfg.addrBits) +
      step.pk.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val offset = (globalM * step.cmdDesc.lda.resize(cfg.addrBits) + globalK).resize(cfg.addrBits)
    (step.cmdDesc.aBase + offset * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
  }

  def computeBBase(step: StepCtx): UInt = {
    val globalK = (step.pkCmd.resize(cfg.addrBits) * step.cmdDesc.primK.resize(cfg.addrBits) +
      step.pk.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val globalN = (step.pj.resize(cfg.addrBits) * step.cmdDesc.primN.resize(cfg.addrBits) +
      step.nj.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val offset = (globalK * step.cmdDesc.ldb.resize(cfg.addrBits) + globalN).resize(cfg.addrBits)
    (step.cmdDesc.bBase + offset * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
  }

  // ---------- Command dispatch engine ----------

  cmdDispatchQ.io.push.valid := cmdFrontend.io.outValid && !schedAbort
  cmdFrontend.io.outReady := cmdDispatchQ.io.push.ready && !schedAbort
  when(cmdDispatchQ.io.push.fire) {
    traceCmdAccepted := True
  }

  val stepDoneInjectPulse = Bool()
  val stepDoneDrainPulse = Bool()
  val stepDoneErrorPulse = Bool()
  stepDoneInjectPulse := False
  stepDoneDrainPulse := False
  stepDoneErrorPulse := False
  val stepDonePulse = stepDoneInjectPulse || stepDoneDrainPulse || stepDoneErrorPulse

  // ---------- Step generation engine ----------

  val emittedStep = StepCtx(cfg)
  emittedStep.cmdDesc := schedCmdCtx.cmdDesc
  emittedStep.numPi := schedCmdCtx.numPi
  emittedStep.numPj := schedCmdCtx.numPj
  emittedStep.numPkCmd := schedCmdCtx.numPkCmd
  emittedStep.numMi := schedCmdCtx.numMi
  emittedStep.numNj := schedCmdCtx.numNj
  emittedStep.numPk := schedCmdCtx.numPk
  emittedStep.pi := tileScheduler.io.pi
  emittedStep.pj := tileScheduler.io.pj
  emittedStep.pkCmd := tileScheduler.io.pkCmd
  emittedStep.mi := tileScheduler.io.mi
  emittedStep.nj := tileScheduler.io.nj
  emittedStep.pk := tileScheduler.io.pk
  emittedStep.clearBank := tileScheduler.io.clearBank
  emittedStep.drainTrigger := tileScheduler.io.drainTrigger
  emittedStep.bankSel := tileScheduler.io.bankSel
  stepIssueQ.io.push.payload := emittedStep

  when(!schedCmdActive && !schedAbort) {
    tileScheduler.io.cmdValid := cmdDispatchQ.io.pop.valid
    tileScheduler.io.cmdDesc := cmdDispatchQ.io.pop.payload.cmdDesc
    cmdDispatchQ.io.pop.ready := tileScheduler.io.cmdReady
    when(cmdDispatchQ.io.pop.fire) {
      schedCmdCtx := cmdDispatchQ.io.pop.payload
      schedCmdActive := True
      schedStepWaitingDone := False
      errorDetected := False
    }
  } elsewhen (schedAbort) {
    stepIssueQ.io.pop.ready := stepIssueQ.io.pop.valid
    when(tileScheduler.io.busy && tileScheduler.io.stepValid) {
      tileScheduler.io.stepReady := True
    } elsewhen (!tileScheduler.io.busy) {
      schedAbort := False
      schedCmdActive := False
      schedStepWaitingDone := False
    }
  } otherwise {
    when(!schedStepWaitingDone && tileScheduler.io.stepValid && stepIssueQ.io.push.ready) {
      stepIssueQ.io.push.valid := True
      schedStepWaitingDone := True
    }

    when(schedStepWaitingDone && stepDonePulse) {
      tileScheduler.io.stepReady := True
      schedStepWaitingDone := False
    }

    when(!tileScheduler.io.busy && !schedStepWaitingDone) {
      schedCmdActive := False
    }
  }

  // ---------- Prefetch/issue engine ----------

  switch(prefetchState) {
    is(PrefetchState.IDLE) {
      when(schedAbort) {
        stepIssueQ.io.pop.ready := stepIssueQ.io.pop.valid
      } elsewhen(stepIssueQ.io.pop.valid) {
        stepIssueQ.io.pop.ready := True
        prefetchCtx := stepIssueQ.io.pop.payload
        prefetchState := PrefetchState.START_READS
      }
    }

    is(PrefetchState.START_READS) {
      readEngineA.io.cfgBase := computeABase(prefetchCtx)
      readEngineA.io.cfgStride := (prefetchCtx.cmdDesc.lda.resize(cfg.addrBits) * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
      readEngineA.io.cfgCount := U(cfg.s, 16 bits)

      readEngineB.io.cfgBase := computeBBase(prefetchCtx)
      readEngineB.io.cfgStride := (prefetchCtx.cmdDesc.ldb.resize(cfg.addrBits) * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
      readEngineB.io.cfgCount := U(cfg.s, 16 bits)

      when(!readEngineA.io.busy && !readEngineB.io.busy) {
        readEngineA.io.start := True
        readEngineB.io.start := True
        aLoadCnt := 0
        bLoadCnt := 0
        prefetchState := PrefetchState.LOAD_AB
      }
    }

    is(PrefetchState.LOAD_AB) {
      when(readEngineA.io.outValid && (aLoadCnt < U(cfg.s, aLoadCnt.getWidth bits))) {
        readEngineA.io.outReady := True
        transposeBuffer.io.wrValid := True
        transposeBuffer.io.wrRowIdx := aLoadCnt.resized
        aLoadCnt := aLoadCnt + 1
      }

      when(readEngineB.io.outValid && bFifo.io.push.ready) {
        bLoadCnt := bLoadCnt + 1
      }

      when(readEngineA.io.error || readEngineB.io.error) {
        errorDetected := True
        errorCmdId := prefetchCtx.cmdDesc.cmdId
        prefetchState := PrefetchState.ERROR_DRAIN
      }

      val aDone = aLoadCnt === U(cfg.s, aLoadCnt.getWidth bits)
      val bDone = bLoadCnt === U(cfg.s, bLoadCnt.getWidth bits)
      when(aDone && bDone && !errorDetected) {
        prefetchState := PrefetchState.ENQ_INJECT
      }
    }

    is(PrefetchState.ENQ_INJECT) {
      injectQ.io.push.valid := True
      injectQ.io.push.payload := prefetchCtx
      when(injectQ.io.push.ready) {
        prefetchState := PrefetchState.IDLE
      }
    }

    is(PrefetchState.ERROR_DRAIN) {
      readEngineA.io.outReady := True
      readEngineB.io.outReady := True
      bFifo.io.pop.ready := bFifo.io.pop.valid
      when(!readEngineA.io.busy && !readEngineB.io.busy) {
        prefetchState := PrefetchState.ERROR_STATUS
      }
    }

    is(PrefetchState.ERROR_STATUS) {
      statusGen.io.errValid := True
      statusGen.io.errCmdId := errorCmdId
      statusGen.io.errCode := B(0x02, 8 bits)
      stepDoneErrorPulse := True
      schedAbort := True
      bank0ComputeBusy := False
      bank1ComputeBusy := False
      bank0DrainBusy := False
      bank1DrainBusy := False
      bank0RowBusy := 0
      bank1RowBusy := 0
      injectState := InjectState.IDLE
      drainState := DrainState.IDLE
      prefetchState := PrefetchState.IDLE
    }
  }

  // ---------- Inject engine + hazard scoreboard ----------

  val bankHazardForInject = Bool()
  bankHazardForInject := False
  when(injectQ.io.pop.valid) {
    when(injectQ.io.pop.payload.bankSel) {
      bankHazardForInject := bank1ComputeBusy || bank1DrainBusy || (bank1RowBusy =/= 0)
    } otherwise {
      bankHazardForInject := bank0ComputeBusy || bank0DrainBusy || (bank0RowBusy =/= 0)
    }
  }

  switch(injectState) {
    is(InjectState.IDLE) {
      when(schedAbort) {
        injectQ.io.pop.ready := injectQ.io.pop.valid
      } elsewhen(injectQ.io.pop.valid && !bankHazardForInject) {
        injectQ.io.pop.ready := True
        injectCtx := injectQ.io.pop.payload
        coreBankSel := injectQ.io.pop.payload.bankSel
        when(injectQ.io.pop.payload.bankSel) {
          bank1ComputeBusy := True
        } otherwise {
          bank0ComputeBusy := True
        }
        feedCnt := 0
        injectState := InjectState.SWAP_TB
      }
    }

    is(InjectState.SWAP_TB) {
      transposeBuffer.io.swap := True
      traceFeedStart := True
      feedCnt := 0
      injectState := InjectState.FEED
    }

    is(InjectState.FEED) {
      transposeBuffer.io.rdColIdx := feedCnt
      bFifo.io.pop.ready := True
      for (i <- 0 until cfg.s) {
        systolicCore.io.aIn(i) := transposeBuffer.io.rdData(i)
        systolicCore.io.aValid(i) := True
        systolicCore.io.bIn(i) := bFifo.io.pop.payload((i + 1) * 32 - 1 downto i * 32)
        systolicCore.io.bValid(i) := True
      }
      when(feedCnt === 0 && injectCtx.clearBank) {
        systolicCore.io.clearBank := True
      }

      when(feedCnt === U(cfg.s - 1, feedCnt.getWidth bits)) {
        when(injectCtx.drainTrigger) {
          tailCnt := 0
          injectState := InjectState.TAIL
        } otherwise {
          stepDoneInjectPulse := True
          when(injectCtx.bankSel) {
            bank1ComputeBusy := False
          } otherwise {
            bank0ComputeBusy := False
          }
          injectState := InjectState.IDLE
        }
      } otherwise {
        feedCnt := feedCnt + 1
      }
    }

    is(InjectState.TAIL) {
      if (cfg.s > 1) {
        when(tailCnt === tailTarget) {
          injectState := InjectState.ENQ_DRAIN
        } otherwise {
          tailCnt := tailCnt + 1
        }
      } else {
        injectState := InjectState.ENQ_DRAIN
      }
    }

    is(InjectState.ENQ_DRAIN) {
      drainQ.io.push.valid := True
      drainQ.io.push.payload := injectCtx
      when(drainQ.io.push.ready) {
        when(injectCtx.bankSel) {
          bank1ComputeBusy := False
        } otherwise {
          bank0ComputeBusy := False
        }
        injectState := InjectState.IDLE
      }
    }
  }

  // ---------- Drain engine ----------

  when(drainState =/= DrainState.IDLE) {
    drainPacker.io.drainBankIn := drainCtx.bankSel
    drainPacker.io.pi := drainCtx.pi
    drainPacker.io.pj := drainCtx.pj
    drainPacker.io.pkCmd := drainCtx.pkCmd
    drainPacker.io.mi := drainCtx.mi
    drainPacker.io.nj := drainCtx.nj
    drainPacker.io.numPkCmd := drainCtx.numPkCmd
    drainPacker.io.numMi := drainCtx.numMi
    drainPacker.io.numNj := drainCtx.numNj
    drainPacker.io.numPi := drainCtx.numPi
    drainPacker.io.numPj := drainCtx.numPj
    drainPacker.io.cmdDesc := drainCtx.cmdDesc
  }

  switch(drainState) {
    is(DrainState.IDLE) {
      when(schedAbort) {
        drainQ.io.pop.ready := drainQ.io.pop.valid
      } elsewhen(drainQ.io.pop.valid) {
        drainQ.io.pop.ready := True
        drainCtx := drainQ.io.pop.payload
        when(drainQ.io.pop.payload.bankSel) {
          bank1DrainBusy := True
          bank1RowBusy := B((BigInt(1) << cfg.s) - 1, cfg.s bits)
        } otherwise {
          bank0DrainBusy := True
          bank0RowBusy := B((BigInt(1) << cfg.s) - 1, cfg.s bits)
        }
        drainState := DrainState.DRAIN_START
      }
    }

    is(DrainState.DRAIN_START) {
      traceDrainStart := True
      drainPacker.io.drainStart := True
      drainState := DrainState.DRAIN_WAIT
    }

    is(DrainState.DRAIN_WAIT) {
      when(drainPacker.io.drainDone) {
        traceDrainDone := True
        val lastMiNj = (drainCtx.mi === drainCtx.numMi - 1) && (drainCtx.nj === drainCtx.numNj - 1)
        val lastPkCmd = drainCtx.pkCmd === drainCtx.numPkCmd - 1
        when(lastMiNj && lastPkCmd) {
          drainState := DrainState.FLUSH_START
        } otherwise {
          drainState := DrainState.STEP_DONE
        }
      }
    }

    is(DrainState.FLUSH_START) {
      drainPacker.io.flushStart := True
      drainState := DrainState.FLUSH_WAIT
    }

    is(DrainState.FLUSH_WAIT) {
      when(drainPacker.io.flushDone) {
        drainState := DrainState.STEP_DONE
      }
    }

    is(DrainState.STEP_DONE) {
      stepDoneDrainPulse := True
      when(drainCtx.bankSel) {
        bank1DrainBusy := False
        bank1RowBusy := 0
      } otherwise {
        bank0DrainBusy := False
        bank0RowBusy := 0
      }
      drainState := DrainState.IDLE
    }
  }

  // ---------- Utilization/Trace Counter Updates ----------

  val aLoadDone = aLoadCnt === U(cfg.s, aLoadCnt.getWidth bits)
  val bLoadDone = bLoadCnt === U(cfg.s, bLoadCnt.getWidth bits)

  utilInjectFullCycle := (injectState === InjectState.FEED) && bFifo.io.pop.valid
  utilStallSample := utilInjectWindowActive && !utilInjectFullCycle

  utilStallCauseNoStep := False
  utilStallCauseANotReady := False
  utilStallCauseBNotReady := False
  utilStallCauseBankHazard := False
  utilStallCauseDrainBlocked := False
  utilStallCauseOutputBackpressure := False
  utilStallCauseErrorFlush := False

  val stallErrorFlushCond =
    (prefetchState === PrefetchState.ERROR_DRAIN) ||
      (prefetchState === PrefetchState.ERROR_STATUS) ||
      schedAbort
  val stallOutputBackpressureCond =
    (drainState === DrainState.DRAIN_WAIT) && drainPacker.io.outValid && !drainPacker.io.outReady
  val stallDrainBlockedCond =
    (injectState === InjectState.TAIL) ||
      (drainState === DrainState.DRAIN_START) ||
      ((drainState === DrainState.DRAIN_WAIT) && !drainPacker.io.drainDone && !stallOutputBackpressureCond)
  val stallBankHazardCond = bankHazardForInject
  val stallANotReadyCond = (prefetchState === PrefetchState.LOAD_AB) && !aLoadDone
  val stallBNotReadyCond = (prefetchState === PrefetchState.LOAD_AB) && aLoadDone && !bLoadDone

  when(utilStallSample) {
    when(stallErrorFlushCond) {
      utilStallCauseErrorFlush := True
    } elsewhen (stallOutputBackpressureCond) {
      utilStallCauseOutputBackpressure := True
    } elsewhen (stallDrainBlockedCond) {
      utilStallCauseDrainBlocked := True
    } elsewhen (stallBankHazardCond) {
      utilStallCauseBankHazard := True
    } elsewhen (stallANotReadyCond) {
      utilStallCauseANotReady := True
    } elsewhen (stallBNotReadyCond) {
      utilStallCauseBNotReady := True
    } otherwise {
      utilStallCauseNoStep := True
    }
  }

  utilTotalCycles := utilTotalCycles + 1

  when(traceCmdAccepted) {
    traceCmdAcceptedCount := traceCmdAcceptedCount + 1
    traceLastCmdAcceptedCycle := utilTotalCycles
  }
  when(traceFeedStart) {
    traceFeedStartCount := traceFeedStartCount + 1
    traceLastFeedStartCycle := utilTotalCycles
    utilInjectWindowActive := True
  }
  when(traceDrainStart) {
    traceDrainStartCount := traceDrainStartCount + 1
    traceLastDrainStartCycle := utilTotalCycles
  }
  when(traceDrainDone) {
    traceDrainDoneCount := traceDrainDoneCount + 1
    traceLastDrainDoneCycle := utilTotalCycles
  }

  val cmdPipelineQuiescent =
    !schedCmdActive &&
      !tileScheduler.io.busy &&
      !schedStepWaitingDone &&
      (prefetchState === PrefetchState.IDLE) &&
      (injectState === InjectState.IDLE) &&
      (drainState === DrainState.IDLE) &&
      !stepIssueQ.io.pop.valid &&
      !injectQ.io.pop.valid &&
      !drainQ.io.pop.valid

  when(cmdPipelineQuiescent || (prefetchState === PrefetchState.ERROR_STATUS)) {
    utilInjectWindowActive := False
  }

  when(utilInjectWindowActive) {
    utilInjectWindowCycles := utilInjectWindowCycles + 1
  }
  when(utilInjectFullCycle) {
    utilInjectFullCycles := utilInjectFullCycles + 1
  }

  when(utilStallCauseNoStep) {
    utilStallNoStepCycles := utilStallNoStepCycles + 1
  }
  when(utilStallCauseANotReady) {
    utilStallANotReadyCycles := utilStallANotReadyCycles + 1
  }
  when(utilStallCauseBNotReady) {
    utilStallBNotReadyCycles := utilStallBNotReadyCycles + 1
  }
  when(utilStallCauseBankHazard) {
    utilStallBankHazardCycles := utilStallBankHazardCycles + 1
  }
  when(utilStallCauseDrainBlocked) {
    utilStallDrainBlockedCycles := utilStallDrainBlockedCycles + 1
  }
  when(utilStallCauseOutputBackpressure) {
    utilStallOutputBackpressureCycles := utilStallOutputBackpressureCycles + 1
  }
  when(utilStallCauseErrorFlush) {
    utilStallErrorFlushCycles := utilStallErrorFlushCycles + 1
  }
}
