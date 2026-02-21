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
    stagedTileDepth: Int = 8,
    lMul: Int = 4,
    lAdd: Int = 4
) {
  require(clBits % fpBits == 0, "CL_BITS must be a multiple of FP_BITS")
  require((maxOutstandingRd & (maxOutstandingRd - 1)) == 0, "MAX_OUTSTANDING_RD must be power of 2")
  require(stagedTileDepth > 1, "stagedTileDepth must be > 1")

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
  val readEngineA = ReadEngine(cfg.addrBits, cfg.clBits, cfg.maxOutstandingRd, descriptorQueueDepth = cfg.stagedTileDepth)
  val readEngineB = ReadEngine(cfg.addrBits, cfg.clBits, cfg.maxOutstandingRd, descriptorQueueDepth = cfg.stagedTileDepth)
  val systolicCore = SystolicCore(cfg.s, cfg.lMul, cfg.lAdd)
  val drainPacker = DrainPacker(cfg)

  // Deep staged-tile buffers (depth-configurable, ordered producer/consumer).
  val stageDepth = cfg.stagedTileDepth
  val stagePtrW = log2Up(stageDepth)
  val stageCountW = log2Up(stageDepth + 1)

  val aStageTiles = Vec(
    Vec(
      Vec(Reg(Bits(32 bits)) init (B(0, 32 bits)), cfg.s),
      cfg.s
    ),
    stageDepth
  )
  val bStageTiles = Vec(
    Vec(Reg(Bits(cfg.clBits bits)) init (B(0, cfg.clBits bits)), cfg.s),
    stageDepth
  )
  val stagedAValid = Vec(Reg(Bool()) init (False), stageDepth)
  val stagedBValid = Vec(Reg(Bool()) init (False), stageDepth)

  // Output FIFO between DrainPacker and d_wr_* ports
  val outFifo = StreamFifo(DWriteEntry(cfg), cfg.outFifoDepthCl)

  // In-order commit queue for status emission.
  val commitDepth = 1 << log2Up((cfg.cmdqDepth * 4).max(8))
  val commitPtrW = log2Up(commitDepth)
  val commitCountW = log2Up(commitDepth + 1)
  val commitHead = Reg(UInt(commitPtrW bits)) init (0)
  val commitTail = Reg(UInt(commitPtrW bits)) init (0)
  val commitCount = Reg(UInt(commitCountW bits)) init (0)
  val commitCmdId = Vec(Reg(UInt(16 bits)) init (0), commitDepth)
  val commitWritesDone = Vec(Reg(Bool()) init (False), commitDepth)
  val commitDrainsDone = Vec(Reg(Bool()) init (False), commitDepth)
  val commitErrorsSeen = Vec(Reg(Bool()) init (False), commitDepth)
  val commitErrCode = Vec(Reg(Bits(8 bits)) init (0), commitDepth)
  val commitQueueFull = commitCount === U(commitDepth, commitCountW bits)
  val commitCanAccept = !commitQueueFull

  // ---------- Command Frontend Wiring ----------

  cmdFrontend.io.cmdValid := io.cmd_valid && commitCanAccept
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
  io.cmd_ready := cmdFrontend.io.cmdReady && commitCanAccept

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

  cmdFrontend.io.rejReady := True

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

  // ---------- Controller decomposition ----------

  val ctrlFifoDepth = (cfg.cmdqDepth * 4).max(4)
  val cmdDispatchQ = StreamFifo(DispatchCmdCtx(cfg), cfg.cmdqDepth)
  val stepIssueQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)
  val injectQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)
  val injectSlotQ = StreamFifo(UInt(stagePtrW bits), ctrlFifoDepth)
  val injectSeqQ = StreamFifo(UInt(32 bits), ctrlFifoDepth)
  val drainQ = StreamFifo(StepCtx(cfg), ctrlFifoDepth)
  val drainDelayW = log2Up(2 * (cfg.s - 1) + cfg.lMul + cfg.lAdd + 2)
  val drainDelayQ = StreamFifo(UInt(drainDelayW bits), ctrlFifoDepth)

  object PrefetchState extends SpinalEnum {
    val IDLE, START_READS, LOAD_AB, ENQ_INJECT, ERROR_DRAIN, ERROR_STATUS = newElement()
  }
  object InjectState extends SpinalEnum {
    val IDLE, FEED, ENQ_DRAIN = newElement()
  }
  object DrainState extends SpinalEnum {
    val IDLE, LOAD_PENDING, WAIT_DELAY, DRAIN_REQ, DRAIN_WAIT, FLUSH_REQ, FLUSH_WAIT = newElement()
  }

  val prefetchState = RegInit(PrefetchState.IDLE)
  val injectState = RegInit(InjectState.IDLE)
  val drainState = RegInit(DrainState.IDLE)

  val schedCmdCtx = Reg(DispatchCmdCtx(cfg))
  val prefetchCtx = Reg(StepCtx(cfg))
  val injectCtx = Reg(StepCtx(cfg))
  val drainCtx = Reg(StepCtx(cfg))
  val drainPendingValid = Reg(Bool()) init (False)
  val drainPendingCtx = Reg(StepCtx(cfg))
  val drainPendingDelay = Reg(UInt(drainDelayW bits)) init (0)
  val prefetchStageSlotA = Reg(UInt(stagePtrW bits)) init (0)
  val prefetchStageSlotB = Reg(UInt(stagePtrW bits)) init (0)
  val injectStageSlotA = Reg(UInt(stagePtrW bits)) init (0)
  val injectStageSlotB = Reg(UInt(stagePtrW bits)) init (0)

  val stagedAProdPtr = Reg(UInt(stagePtrW bits)) init (0)
  val stagedBProdPtr = Reg(UInt(stagePtrW bits)) init (0)
  val stagedAConsPtr = Reg(UInt(stagePtrW bits)) init (0)
  val stagedBConsPtr = Reg(UInt(stagePtrW bits)) init (0)

  val stagedACount = Reg(UInt(stageCountW bits)) init (0)
  val stagedBCount = Reg(UInt(stageCountW bits)) init (0)
  val stagedIssueSeq = Reg(UInt(32 bits)) init (0)
  val stagedConsumeSeq = Reg(UInt(32 bits)) init (0)

  val schedCmdActive = Reg(Bool()) init (False)
  val schedAbort = Reg(Bool()) init (False)
  val errorDetected = Reg(Bool()) init (False)
  val errorCmdId = Reg(UInt(16 bits)) init (0)
  val flushCmdLastPending = Reg(Bool()) init (False)

  val aLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val bLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val feedCnt = Reg(UInt(log2Up(cfg.s) bits)) init (0)
  val tailTotal = 2 * (cfg.s - 1) + cfg.lMul + cfg.lAdd
  val drainTailDelay = U(tailTotal + 1, drainDelayW bits)

  def stagePtrInc(ptr: UInt): UInt = {
    if (stageDepth == 1) U(0, stagePtrW bits)
    else Mux(ptr === U(stageDepth - 1, stagePtrW bits), U(0, stagePtrW bits), ptr + 1)
  }

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
  val utilStallPrefetchSetupCycles = Reg(UInt(64 bits)) init (0)
  val utilStallBankHazardWaitCycles = Reg(UInt(64 bits)) init (0)
  val utilStallDrainEnqueueBookkeepingCycles = Reg(UInt(64 bits)) init (0)

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
  val utilStallCausePrefetchSetup = Bool()
  val utilStallCauseBankHazardWait = Bool()
  val utilStallCauseDrainEnqueueBookkeeping = Bool()

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
  utilStallPrefetchSetupCycles.simPublic()
  utilStallBankHazardWaitCycles.simPublic()
  utilStallDrainEnqueueBookkeepingCycles.simPublic()

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
  utilStallCausePrefetchSetup.simPublic()
  utilStallCauseBankHazardWait.simPublic()
  utilStallCauseDrainEnqueueBookkeeping.simPublic()

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

  injectSlotQ.io.push.valid := False
  injectSlotQ.io.push.payload := prefetchStageSlotA
  injectSlotQ.io.pop.ready := False

  injectSeqQ.io.push.valid := False
  injectSeqQ.io.push.payload := stagedIssueSeq
  injectSeqQ.io.pop.ready := False

  drainQ.io.push.valid := False
  drainQ.io.push.payload := injectCtx
  drainQ.io.pop.ready := False

  drainDelayQ.io.push.valid := False
  drainDelayQ.io.push.payload := drainTailDelay
  drainDelayQ.io.pop.ready := False

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

  // ReadEngine A -> staged A tiles
  readEngineA.io.outReady := False

  // ReadEngine B -> staged B tiles
  readEngineB.io.outReady := False

  // SystolicCore defaults
  val aFeedCol = Vec(Bits(32 bits), cfg.s)
  for (i <- 0 until cfg.s) {
    aFeedCol(i) := aStageTiles(injectStageSlotA)(i)(feedCnt)
  }
  val bFeedLine = bStageTiles(injectStageSlotB)(feedCnt)
  for (i <- 0 until cfg.s) {
    systolicCore.io.aIn(i) := aFeedCol(i)
    systolicCore.io.aValid(i) := False
    systolicCore.io.bIn(i) := bFeedLine((i + 1) * 32 - 1 downto i * 32)
    systolicCore.io.bValid(i) := False
  }
  systolicCore.io.bankSel := coreBankSel
  systolicCore.io.clearBank := False

  // DrainPacker defaults
  drainPacker.io.drainReqValid := False
  drainPacker.io.flushReqValid := False
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

  traceCmdAccepted := False
  traceFeedStart := False
  traceDrainStart := False
  traceDrainDone := False
  val stagedProduceFire = Bool()
  val stagedConsumeFire = Bool()
  stagedProduceFire := False
  stagedConsumeFire := False

  val poisonReqValid = Bool()
  val poisonReqCmdId = UInt(16 bits)
  val poisonReqErrCode = Bits(8 bits)
  poisonReqValid := False
  poisonReqCmdId := 0
  poisonReqErrCode := B(0x02, 8 bits) // readErr

  val drainCmdDoneValid = Bool()
  val drainCmdDoneCmdId = UInt(16 bits)
  drainCmdDoneValid := False
  drainCmdDoneCmdId := 0

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
  cmdFrontend.io.outReady := cmdDispatchQ.io.push.ready || schedAbort
  when(cmdDispatchQ.io.push.fire) {
    traceCmdAccepted := True
  }

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

  when(schedAbort) {
    cmdDispatchQ.io.pop.ready := cmdDispatchQ.io.pop.valid
    stepIssueQ.io.pop.ready := stepIssueQ.io.pop.valid
    when(tileScheduler.io.busy && tileScheduler.io.stepValid) {
      tileScheduler.io.stepReady := True
    } elsewhen (!tileScheduler.io.busy) {
      schedAbort := False
      schedCmdActive := False
    }
  } elsewhen (!schedCmdActive) {
    tileScheduler.io.cmdValid := cmdDispatchQ.io.pop.valid
    tileScheduler.io.cmdDesc := cmdDispatchQ.io.pop.payload.cmdDesc
    cmdDispatchQ.io.pop.ready := tileScheduler.io.cmdReady
    when(cmdDispatchQ.io.pop.fire) {
      schedCmdCtx := cmdDispatchQ.io.pop.payload
      schedCmdActive := True
      errorDetected := False
    }
  } otherwise {
    when(tileScheduler.io.stepValid && stepIssueQ.io.push.ready) {
      stepIssueQ.io.push.valid := True
      tileScheduler.io.stepReady := True
    }
    when(!tileScheduler.io.busy) {
      schedCmdActive := False
    }
  }

  // ---------- Prefetch/issue engine ----------

  val stagedAFull = stagedACount === U(stageDepth, stageCountW bits)
  val stagedBFull = stagedBCount === U(stageDepth, stageCountW bits)
  val stagedCanAllocate = !stagedAFull && !stagedBFull

  switch(prefetchState) {
    is(PrefetchState.IDLE) {
      when(schedAbort) {
        stepIssueQ.io.pop.ready := stepIssueQ.io.pop.valid
      } elsewhen(stepIssueQ.io.pop.valid && stagedCanAllocate) {
        assert(stagedAProdPtr === stagedBProdPtr, "A/B staged producer pointer mismatch")
        stepIssueQ.io.pop.ready := True
        prefetchCtx := stepIssueQ.io.pop.payload
        prefetchStageSlotA := stagedAProdPtr
        prefetchStageSlotB := stagedBProdPtr
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
        for (e <- 0 until cfg.s) {
          aStageTiles(prefetchStageSlotA)(aLoadCnt.resized)(e) := readEngineA.io.outData((e + 1) * 32 - 1 downto e * 32)
        }
        aLoadCnt := aLoadCnt + 1
      }

      when(readEngineB.io.outValid && (bLoadCnt < U(cfg.s, bLoadCnt.getWidth bits))) {
        readEngineB.io.outReady := True
        bStageTiles(prefetchStageSlotB)(bLoadCnt.resized) := readEngineB.io.outData
        bLoadCnt := bLoadCnt + 1
      }

      when(readEngineA.io.error || readEngineB.io.error) {
        errorDetected := True
        errorCmdId := prefetchCtx.cmdDesc.cmdId
        prefetchState := PrefetchState.ERROR_DRAIN
      }

      val aDone = aLoadCnt === U(cfg.s, aLoadCnt.getWidth bits)
      val bDone = bLoadCnt === U(cfg.s, bLoadCnt.getWidth bits)
      val prefetchReadErr = errorDetected || readEngineA.io.error || readEngineB.io.error
      when(aDone && bDone && !prefetchReadErr) {
        prefetchState := PrefetchState.ENQ_INJECT
      }
    }

    is(PrefetchState.ENQ_INJECT) {
      val canEnqueueInject = injectQ.io.push.ready && injectSlotQ.io.push.ready && injectSeqQ.io.push.ready
      injectQ.io.push.valid := canEnqueueInject
      injectQ.io.push.payload := prefetchCtx
      injectSlotQ.io.push.valid := canEnqueueInject
      injectSlotQ.io.push.payload := prefetchStageSlotA
      injectSeqQ.io.push.valid := canEnqueueInject
      injectSeqQ.io.push.payload := stagedIssueSeq
      when(canEnqueueInject) {
        assert(stagedACount =/= U(stageDepth, stageCountW bits), "staged A FIFO overflow")
        assert(stagedBCount =/= U(stageDepth, stageCountW bits), "staged B FIFO overflow")
        assert(!stagedAValid(prefetchStageSlotA), "staged A slot overwrite")
        assert(!stagedBValid(prefetchStageSlotB), "staged B slot overwrite")
        assert(prefetchStageSlotA === prefetchStageSlotB, "A/B staged producer slot mismatch")
        stagedAValid(prefetchStageSlotA) := True
        stagedBValid(prefetchStageSlotB) := True
        stagedProduceFire := True
        prefetchState := PrefetchState.IDLE
      }
    }

    is(PrefetchState.ERROR_DRAIN) {
      readEngineA.io.outReady := True
      readEngineB.io.outReady := True
      when(!readEngineA.io.busy && !readEngineB.io.busy) {
        prefetchState := PrefetchState.ERROR_STATUS
      }
    }

    is(PrefetchState.ERROR_STATUS) {
      poisonReqValid := True
      poisonReqCmdId := errorCmdId
      poisonReqErrCode := B(0x02, 8 bits)
      schedAbort := True
      bank0ComputeBusy := False
      bank1ComputeBusy := False
      bank0DrainBusy := False
      bank1DrainBusy := False
      bank0RowBusy := 0
      bank1RowBusy := 0
      stagedAProdPtr := 0
      stagedBProdPtr := 0
      stagedAConsPtr := 0
      stagedBConsPtr := 0
      stagedACount := 0
      stagedBCount := 0
      stagedIssueSeq := 0
      stagedConsumeSeq := 0
      for (slot <- 0 until stageDepth) {
        stagedAValid(slot) := False
        stagedBValid(slot) := False
      }
      drainPendingValid := False
      flushCmdLastPending := False
      injectState := InjectState.IDLE
      drainState := DrainState.IDLE
      prefetchState := PrefetchState.IDLE
    }
  }

  // ---------- Inject engine + hazard scoreboard ----------

  val enableDrainOverlap = false
  val bankHazardForInject = Bool()
  bankHazardForInject := False
  if (enableDrainOverlap) {
    when(injectQ.io.pop.valid) {
      when(injectQ.io.pop.payload.bankSel) {
        bankHazardForInject := bank1ComputeBusy || bank1DrainBusy || (bank1RowBusy =/= 0)
      } otherwise {
        bankHazardForInject := bank0ComputeBusy || bank0DrainBusy || (bank0RowBusy =/= 0)
      }
    }
  } else {
    when(bank0DrainBusy || bank1DrainBusy) {
      bankHazardForInject := True
    } elsewhen (injectQ.io.pop.valid) {
      when(injectQ.io.pop.payload.bankSel) {
        bankHazardForInject := bank1ComputeBusy || bank1DrainBusy || (bank1RowBusy =/= 0)
      } otherwise {
        bankHazardForInject := bank0ComputeBusy || bank0DrainBusy || (bank0RowBusy =/= 0)
      }
    }
  }

  val injectStageReady = Bool()
  injectStageReady := False
  when(injectSlotQ.io.pop.valid) {
    injectStageReady := stagedAValid(injectSlotQ.io.pop.payload) && stagedBValid(injectSlotQ.io.pop.payload)
  }

  switch(injectState) {
    is(InjectState.IDLE) {
      when(schedAbort) {
        val canDropInject = injectQ.io.pop.valid && injectSlotQ.io.pop.valid && injectSeqQ.io.pop.valid
        injectQ.io.pop.ready := canDropInject
        injectSlotQ.io.pop.ready := canDropInject
        injectSeqQ.io.pop.ready := canDropInject
      } elsewhen(
        injectQ.io.pop.valid &&
          injectSlotQ.io.pop.valid &&
          injectSeqQ.io.pop.valid &&
          !bankHazardForInject &&
          injectStageReady
      ) {
        assert(injectSeqQ.io.pop.payload === stagedConsumeSeq, "staged step order violation at inject")
        assert(injectSlotQ.io.pop.payload === stagedAConsPtr, "staged A consume pointer order violation at inject")
        assert(injectSlotQ.io.pop.payload === stagedBConsPtr, "staged B consume pointer order violation at inject")

        injectQ.io.pop.ready := True
        injectSlotQ.io.pop.ready := True
        injectSeqQ.io.pop.ready := True
        injectCtx := injectQ.io.pop.payload
        injectStageSlotA := injectSlotQ.io.pop.payload
        injectStageSlotB := injectSlotQ.io.pop.payload
        stagedConsumeSeq := stagedConsumeSeq + 1
        coreBankSel := injectQ.io.pop.payload.bankSel
        when(injectQ.io.pop.payload.bankSel) {
          bank1ComputeBusy := True
        } otherwise {
          bank0ComputeBusy := True
        }
        feedCnt := 0
        traceFeedStart := True
        injectState := InjectState.FEED
      }
    }

    is(InjectState.FEED) {
      for (i <- 0 until cfg.s) {
        systolicCore.io.aIn(i) := aFeedCol(i)
        systolicCore.io.aValid(i) := True
        systolicCore.io.bIn(i) := bFeedLine((i + 1) * 32 - 1 downto i * 32)
        systolicCore.io.bValid(i) := True
      }
      when(feedCnt === 0 && injectCtx.clearBank) {
        systolicCore.io.clearBank := True
      }

      when(feedCnt === U(cfg.s - 1, feedCnt.getWidth bits)) {
        assert(stagedACount =/= 0, "staged A FIFO underflow")
        assert(stagedBCount =/= 0, "staged B FIFO underflow")
        assert(stagedAValid(injectStageSlotA), "staged A consume without valid tile")
        assert(stagedBValid(injectStageSlotB), "staged B consume without valid tile")
        assert(injectStageSlotA === injectStageSlotB, "A/B staged consume slot mismatch")

        stagedAValid(injectStageSlotA) := False
        stagedBValid(injectStageSlotB) := False
        stagedConsumeFire := True

        when(injectCtx.drainTrigger) {
          injectState := InjectState.ENQ_DRAIN
        } otherwise {
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

    is(InjectState.ENQ_DRAIN) {
      val canEnqueueDrain = drainQ.io.push.ready && drainDelayQ.io.push.ready
      drainQ.io.push.valid := canEnqueueDrain
      drainQ.io.push.payload := injectCtx
      drainDelayQ.io.push.valid := canEnqueueDrain
      drainDelayQ.io.push.payload := drainTailDelay
      when(canEnqueueDrain) {
        when(injectCtx.bankSel) {
          bank1ComputeBusy := False
          bank1DrainBusy := True
          bank1RowBusy := B((BigInt(1) << cfg.s) - 1, cfg.s bits)
        } otherwise {
          bank0ComputeBusy := False
          bank0DrainBusy := True
          bank0RowBusy := B((BigInt(1) << cfg.s) - 1, cfg.s bits)
        }
        injectState := InjectState.IDLE
      }
    }
  }

  when(stagedProduceFire) {
    stagedAProdPtr := stagePtrInc(stagedAProdPtr)
    stagedBProdPtr := stagePtrInc(stagedBProdPtr)
    stagedIssueSeq := stagedIssueSeq + 1
  }
  when(stagedConsumeFire) {
    stagedAConsPtr := stagePtrInc(stagedAConsPtr)
    stagedBConsPtr := stagePtrInc(stagedBConsPtr)
  }
  when(stagedProduceFire && !stagedConsumeFire) {
    stagedACount := stagedACount + 1
    stagedBCount := stagedBCount + 1
  } elsewhen(!stagedProduceFire && stagedConsumeFire) {
    stagedACount := stagedACount - 1
    stagedBCount := stagedBCount - 1
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
        val canDropDrain = drainQ.io.pop.valid && drainDelayQ.io.pop.valid
        drainQ.io.pop.ready := canDropDrain
        drainDelayQ.io.pop.ready := canDropDrain
        drainPendingValid := False
      } elsewhen(drainPendingValid || (drainQ.io.pop.valid && drainDelayQ.io.pop.valid)) {
        drainState := DrainState.LOAD_PENDING
      }
    }

    is(DrainState.LOAD_PENDING) {
      when(schedAbort) {
        val canDropDrain = drainQ.io.pop.valid && drainDelayQ.io.pop.valid
        drainQ.io.pop.ready := canDropDrain
        drainDelayQ.io.pop.ready := canDropDrain
        drainPendingValid := False
        drainState := DrainState.IDLE
      } elsewhen(!drainPendingValid) {
        val canPopDrain = drainQ.io.pop.valid && drainDelayQ.io.pop.valid
        drainQ.io.pop.ready := canPopDrain
        drainDelayQ.io.pop.ready := canPopDrain
        when(canPopDrain) {
          drainPendingCtx := drainQ.io.pop.payload
          drainPendingDelay := drainDelayQ.io.pop.payload
          drainPendingValid := True
        } otherwise {
          drainState := DrainState.IDLE
        }
      } otherwise {
        drainState := DrainState.WAIT_DELAY
      }
    }

    is(DrainState.WAIT_DELAY) {
      when(!drainPendingValid) {
        drainState := DrainState.IDLE
      } elsewhen(drainPendingDelay === 0) {
        drainCtx := drainPendingCtx
        drainPendingValid := False
        drainState := DrainState.DRAIN_REQ
      } otherwise {
        drainPendingDelay := drainPendingDelay - 1
      }
    }

    is(DrainState.DRAIN_REQ) {
      drainPacker.io.drainReqValid := True
      when(drainPacker.io.drainReqReady) {
        traceDrainStart := True
        drainState := DrainState.DRAIN_WAIT
      }
    }

    is(DrainState.DRAIN_WAIT) {
      when(drainPacker.io.drainDone) {
        traceDrainDone := True
        when(drainCtx.bankSel) {
          bank1DrainBusy := False
          bank1RowBusy := 0
        } otherwise {
          bank0DrainBusy := False
          bank0RowBusy := 0
        }
        val lastMiNj = (drainCtx.mi === drainCtx.numMi - 1) && (drainCtx.nj === drainCtx.numNj - 1)
        val lastPkCmd = drainCtx.pkCmd === drainCtx.numPkCmd - 1
        val lastPiPj = (drainCtx.pi === drainCtx.numPi - 1) && (drainCtx.pj === drainCtx.numPj - 1)
        when(lastMiNj && lastPkCmd) {
          flushCmdLastPending := lastPiPj
          drainState := DrainState.FLUSH_REQ
        } otherwise {
          flushCmdLastPending := False
          drainState := DrainState.IDLE
        }
      }
    }

    is(DrainState.FLUSH_REQ) {
      drainPacker.io.flushReqValid := True
      when(drainPacker.io.flushReqReady) {
        drainState := DrainState.FLUSH_WAIT
      }
    }

    is(DrainState.FLUSH_WAIT) {
      when(drainPacker.io.flushDone) {
        when(flushCmdLastPending) {
          drainCmdDoneValid := True
          drainCmdDoneCmdId := drainCtx.cmdDesc.cmdId
        }
        flushCmdLastPending := False
        drainState := DrainState.IDLE
      }
    }

  }

  // ---------- In-order commit queue / status commit ----------

  val commitHeadReady =
    (commitCount =/= 0) &&
      (commitErrorsSeen(commitHead) || (commitWritesDone(commitHead) && commitDrainsDone(commitHead)))
  val commitHeadCmdId = commitCmdId(commitHead)
  val commitHeadErrCode = commitErrCode(commitHead)
  val commitHeadErr = commitErrorsSeen(commitHead)

  io.sts_valid := commitHeadReady
  io.sts_cmd_id := Mux(commitCount =/= 0, commitHeadCmdId, U(0, 16 bits))
  io.sts_ok := (commitCount =/= 0) && !commitHeadErr
  io.sts_err_code := Mux((commitCount =/= 0) && commitHeadErr, commitHeadErrCode, B(0, 8 bits))

  val commitPopFire = io.sts_valid && io.sts_ready
  val commitPushFire = cmdFrontend.io.acceptFire && commitCanAccept

  val poisonMatchByDist = Bits(commitDepth bits)
  poisonMatchByDist := B(0, commitDepth bits)
  for (dist <- 0 until commitDepth) {
    val distU = U(dist, commitCountW bits)
    val idx = (commitHead + U(dist, commitPtrW bits)).resized
    val active = distU < commitCount
    poisonMatchByDist(dist) := poisonReqValid && active && (commitCmdId(idx) === poisonReqCmdId)
  }

  val poisonHit = poisonMatchByDist.orR
  val poisonDist = UInt(commitCountW bits)
  poisonDist := 0
  var priorHit: Bool = False
  for (dist <- 0 until commitDepth) {
    val hit = poisonMatchByDist(dist)
    when(!priorHit && hit) {
      poisonDist := U(dist, commitCountW bits)
    }
    priorHit = priorHit || hit
  }

  for (i <- 0 until commitDepth) {
    val idx = U(i, commitPtrW bits)
    val dist = (idx - commitHead).resize(commitCountW)
    val active = dist < commitCount

    when(dWriteLastFire && active && (commitCmdId(i) === io.d_wr_cmd_id)) {
      commitWritesDone(i) := True
    }

    when(drainCmdDoneValid && active && (commitCmdId(i) === drainCmdDoneCmdId)) {
      commitDrainsDone(i) := True
    }

    val poisonAllActive = poisonReqValid && !poisonHit
    val poisonThis = poisonReqValid && active && (poisonAllActive || (dist >= poisonDist))
    when(poisonThis) {
      commitErrorsSeen(i) := True
      commitErrCode(i) := poisonReqErrCode
      commitWritesDone(i) := True
      commitDrainsDone(i) := True
    }
  }

  when(commitPopFire) {
    commitCmdId(commitHead) := U(0, 16 bits)
    commitWritesDone(commitHead) := False
    commitDrainsDone(commitHead) := False
    commitErrorsSeen(commitHead) := False
    commitErrCode(commitHead) := B(0, 8 bits)
    commitHead := (commitHead + 1).resized
  }

  when(commitPushFire) {
    commitCmdId(commitTail) := cmdFrontend.io.acceptCmdId
    commitWritesDone(commitTail) := cmdFrontend.io.acceptRejected
    commitDrainsDone(commitTail) := cmdFrontend.io.acceptRejected
    commitErrorsSeen(commitTail) := cmdFrontend.io.acceptRejected
    commitErrCode(commitTail) := cmdFrontend.io.acceptErrCode
    commitTail := (commitTail + 1).resized
  }

  when(commitPopFire =/= commitPushFire) {
    when(commitPopFire) {
      commitCount := commitCount - 1
    } otherwise {
      commitCount := commitCount + 1
    }
  }

  // Keep staged A/B FIFOs tightly aligned; any mismatch indicates control corruption.
  assert(stagedACount === stagedBCount, "A/B staged count mismatch")
  assert(stagedAProdPtr === stagedBProdPtr, "A/B staged producer pointer mismatch")
  assert(stagedAConsPtr === stagedBConsPtr, "A/B staged consumer pointer mismatch")

  // ---------- Utilization/Trace Counter Updates ----------

  val aLoadDone = aLoadCnt === U(cfg.s, aLoadCnt.getWidth bits)
  val bLoadDone = bLoadCnt === U(cfg.s, bLoadCnt.getWidth bits)

  utilInjectFullCycle := injectState === InjectState.FEED
  utilStallSample := utilInjectWindowActive && !utilInjectFullCycle

  utilStallCauseNoStep := False
  utilStallCauseANotReady := False
  utilStallCauseBNotReady := False
  utilStallCauseBankHazard := False
  utilStallCauseDrainBlocked := False
  utilStallCauseOutputBackpressure := False
  utilStallCauseErrorFlush := False
  utilStallCausePrefetchSetup := False
  utilStallCauseBankHazardWait := False
  utilStallCauseDrainEnqueueBookkeeping := False

  val stallErrorFlushCond =
    (prefetchState === PrefetchState.ERROR_DRAIN) ||
      (prefetchState === PrefetchState.ERROR_STATUS) ||
      schedAbort
  val stallOutputBackpressureCond =
    (drainState === DrainState.FLUSH_WAIT) && drainPacker.io.outValid && !drainPacker.io.outReady
  val stallDrainBlockedCond =
    (injectState === InjectState.ENQ_DRAIN) && !(drainQ.io.push.ready && drainDelayQ.io.push.ready)
  val stallInjectDrainBookkeepingCond = injectState === InjectState.ENQ_DRAIN
  val stallBankHazardCond = bankHazardForInject || stallInjectDrainBookkeepingCond
  val stallPrefetchControlCond =
    (prefetchState === PrefetchState.START_READS) ||
      (prefetchState === PrefetchState.ENQ_INJECT)
  val stallInjectWaitingForPrefetchCond =
    (injectState === InjectState.IDLE) &&
      !injectQ.io.pop.valid &&
      (prefetchState =/= PrefetchState.IDLE || stepIssueQ.io.pop.valid || readEngineA.io.busy || readEngineB.io.busy)
  val stallANotReadyCond =
    ((prefetchState === PrefetchState.LOAD_AB) && !aLoadDone) ||
      stallPrefetchControlCond ||
      stallInjectWaitingForPrefetchCond
  val stallBNotReadyCond = (prefetchState === PrefetchState.LOAD_AB) && aLoadDone && !bLoadDone
  val bucketOutputBackpressureCond = stallOutputBackpressureCond
  val bucketDrainEnqueueBookkeepingCond = !bucketOutputBackpressureCond && stallInjectDrainBookkeepingCond
  val bucketBankHazardWaitCond = !bucketOutputBackpressureCond && !bucketDrainEnqueueBookkeepingCond && bankHazardForInject
  val bucketPrefetchSetupCond = !bucketOutputBackpressureCond && !bucketDrainEnqueueBookkeepingCond && !bucketBankHazardWaitCond

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
      utilStallCauseANotReady := True
    }

    when(bucketOutputBackpressureCond) {
      utilStallCauseOutputBackpressure := True
    } elsewhen (bucketDrainEnqueueBookkeepingCond) {
      utilStallCauseDrainEnqueueBookkeeping := True
    } elsewhen (bucketBankHazardWaitCond) {
      utilStallCauseBankHazardWait := True
    } elsewhen (bucketPrefetchSetupCond) {
      utilStallCausePrefetchSetup := True
    } otherwise {
      utilStallCausePrefetchSetup := True
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
      (prefetchState === PrefetchState.IDLE) &&
      (injectState === InjectState.IDLE) &&
      (drainState === DrainState.IDLE) &&
      !stepIssueQ.io.pop.valid &&
      !injectQ.io.pop.valid &&
      !injectSlotQ.io.pop.valid &&
      !injectSeqQ.io.pop.valid &&
      !drainQ.io.pop.valid &&
      !drainDelayQ.io.pop.valid &&
      !drainPendingValid &&
      (stagedACount === 0) &&
      (stagedBCount === 0) &&
      !readEngineA.io.busy &&
      !readEngineB.io.busy

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
  when(utilStallCausePrefetchSetup) {
    utilStallPrefetchSetupCycles := utilStallPrefetchSetupCycles + 1
  }
  when(utilStallCauseBankHazardWait) {
    utilStallBankHazardWaitCycles := utilStallBankHazardWaitCycles + 1
  }
  when(utilStallCauseDrainEnqueueBookkeeping) {
    utilStallDrainEnqueueBookkeepingCycles := utilStallDrainEnqueueBookkeepingCycles + 1
  }
}
