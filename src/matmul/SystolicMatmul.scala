package matmul

import spinal.core._
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

  // ---------- FeedController State Machine ----------

  object FeedState extends SpinalEnum {
    val IDLE, ACCEPT_CMD, START_READS, LOAD_AB, SWAP_TB, FEED,
        TAIL, DRAIN_START, DRAIN_WAIT, FLUSH_START, FLUSH_D, NEXT_STEP, CMD_DONE, ERROR_DRAIN, ERROR_STATUS = newElement()
  }
  val feedState = RegInit(FeedState.IDLE)

  // Latched command descriptor
  val curCmd = Reg(CmdDesc(cfg))

  // Latched step info from TileScheduler
  val stepPi = Reg(UInt(16 bits)) init (0)
  val stepPj = Reg(UInt(16 bits)) init (0)
  val stepPkCmd = Reg(UInt(16 bits)) init (0)
  val stepMi = Reg(UInt(16 bits)) init (0)
  val stepNj = Reg(UInt(16 bits)) init (0)
  val stepPk = Reg(UInt(16 bits)) init (0)
  val stepClearBank = Reg(Bool()) init (False)
  val stepDrainTrigger = Reg(Bool()) init (False)
  val stepBankSel = Reg(Bool()) init (False)

  // DrainPacker bank selection (must be after stepBankSel declaration)
  drainPacker.io.drainBankIn := stepBankSel

  // Derived from command
  val numPi = Reg(UInt(16 bits)) init (0)
  val numPj = Reg(UInt(16 bits)) init (0)
  val numPkCmd = Reg(UInt(16 bits)) init (0)
  val numMi = Reg(UInt(16 bits)) init (0)
  val numNj = Reg(UInt(16 bits)) init (0)
  val numPk = Reg(UInt(16 bits)) init (0)

  val aLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val bLoadCnt = Reg(UInt(log2Up(cfg.s + 1) bits)) init (0)
  val feedCnt = Reg(UInt(log2Up(cfg.s) bits)) init (0)
  val tailTotal = 2 * (cfg.s - 1) + cfg.lMul + cfg.lAdd
  val tailCnt = Reg(UInt(log2Up(tailTotal + 1) bits)) init (0)
  val tailTarget = U(tailTotal - 1, log2Up(tailTotal + 1) bits)

  val cmdActive = Reg(Bool()) init (False)
  val errorDetected = Reg(Bool()) init (False)
  val errorCmdId = Reg(UInt(16 bits)) init (0)

  // Whether we need a flush after the current drain
  val needsFlush = Reg(Bool()) init (False)

  // Track whether this is the last step of the scheduler
  val lastPkForMiNj = Reg(Bool()) init (False)

  // ---------- Default assignments ----------

  // TileScheduler
  tileScheduler.io.cmdValid := False
  tileScheduler.io.cmdDesc := curCmd
  tileScheduler.io.stepReady := False

  // CmdFrontend output consumption
  cmdFrontend.io.outReady := False

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
  systolicCore.io.bankSel := stepBankSel
  systolicCore.io.clearBank := False

  // DrainPacker defaults
  drainPacker.io.drainStart := False
  drainPacker.io.flushStart := False
  drainPacker.io.pi := stepPi
  drainPacker.io.pj := stepPj
  drainPacker.io.pkCmd := stepPkCmd
  drainPacker.io.mi := stepMi
  drainPacker.io.nj := stepNj
  drainPacker.io.numPkCmd := numPkCmd
  drainPacker.io.numMi := numMi
  drainPacker.io.numNj := numNj
  drainPacker.io.numPi := numPi
  drainPacker.io.numPj := numPj
  drainPacker.io.cmdDesc := curCmd

  // StatusGen error defaults
  statusGen.io.errValid := False
  statusGen.io.errCmdId := errorCmdId
  statusGen.io.errCode := B(0x02, 8 bits) // readErr

  // ---------- Address computation ----------

  val sWide = U(cfg.s, cfg.addrBits bits)

  def computeABase(): UInt = {
    val globalM = (stepPi.resize(cfg.addrBits) * curCmd.primM.resize(cfg.addrBits) +
      stepMi.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val globalK = (stepPkCmd.resize(cfg.addrBits) * curCmd.primK.resize(cfg.addrBits) +
      stepPk.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val offset = (globalM * curCmd.lda.resize(cfg.addrBits) + globalK).resize(cfg.addrBits)
    (curCmd.aBase + offset * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
  }

  def computeBBase(): UInt = {
    val globalK = (stepPkCmd.resize(cfg.addrBits) * curCmd.primK.resize(cfg.addrBits) +
      stepPk.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val globalN = (stepPj.resize(cfg.addrBits) * curCmd.primN.resize(cfg.addrBits) +
      stepNj.resize(cfg.addrBits) * sWide).resize(cfg.addrBits)
    val offset = (globalK * curCmd.ldb.resize(cfg.addrBits) + globalN).resize(cfg.addrBits)
    (curCmd.bBase + offset * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
  }

  // ---------- State Machine ----------

  switch(feedState) {
    is(FeedState.IDLE) {
      when(cmdFrontend.io.outValid && !cmdActive) {
        cmdFrontend.io.outReady := True
        curCmd := cmdFrontend.io.outDesc
        numPi := cmdFrontend.io.outDesc.m / cmdFrontend.io.outDesc.primM
        numPj := cmdFrontend.io.outDesc.n / cmdFrontend.io.outDesc.primN
        numPkCmd := cmdFrontend.io.outDesc.k / cmdFrontend.io.outDesc.primK
        numMi := cmdFrontend.io.outDesc.primM / U(cfg.s, 16 bits)
        numNj := cmdFrontend.io.outDesc.primN / U(cfg.s, 16 bits)
        numPk := cmdFrontend.io.outDesc.primK / U(cfg.s, 16 bits)
        cmdActive := True
        errorDetected := False
        feedState := FeedState.ACCEPT_CMD
      }
    }

    is(FeedState.ACCEPT_CMD) {
      tileScheduler.io.cmdValid := True
      tileScheduler.io.cmdDesc := curCmd
      when(tileScheduler.io.cmdReady) {
        feedState := FeedState.NEXT_STEP
      }
    }

    is(FeedState.NEXT_STEP) {
      when(tileScheduler.io.stepValid) {
        // Latch step info
        stepPi := tileScheduler.io.pi
        stepPj := tileScheduler.io.pj
        stepPkCmd := tileScheduler.io.pkCmd
        stepMi := tileScheduler.io.mi
        stepNj := tileScheduler.io.nj
        stepPk := tileScheduler.io.pk
        stepClearBank := tileScheduler.io.clearBank
        stepDrainTrigger := tileScheduler.io.drainTrigger
        stepBankSel := tileScheduler.io.bankSel
        feedState := FeedState.START_READS
      } elsewhen(!tileScheduler.io.busy) {
        // Scheduler finished all steps
        feedState := FeedState.CMD_DONE
      }
    }

    is(FeedState.START_READS) {
      // Program both read engines. Only pulse start when both are idle.
      readEngineA.io.cfgBase := computeABase()
      readEngineA.io.cfgStride := (curCmd.lda.resize(cfg.addrBits) * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
      readEngineA.io.cfgCount := U(cfg.s, 16 bits)

      readEngineB.io.cfgBase := computeBBase()
      readEngineB.io.cfgStride := (curCmd.ldb.resize(cfg.addrBits) * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
      readEngineB.io.cfgCount := U(cfg.s, 16 bits)

      when(!readEngineA.io.busy && !readEngineB.io.busy) {
        readEngineA.io.start := True
        readEngineB.io.start := True
        aLoadCnt := 0
        bLoadCnt := 0
        feedState := FeedState.LOAD_AB
      }
    }

    is(FeedState.LOAD_AB) {
      // A side: ReadEngine -> TransposeBuffer
      when(readEngineA.io.outValid && (aLoadCnt < U(cfg.s, aLoadCnt.getWidth bits))) {
        readEngineA.io.outReady := True
        transposeBuffer.io.wrValid := True
        transposeBuffer.io.wrRowIdx := aLoadCnt.resized
        aLoadCnt := aLoadCnt + 1
      }

      // B side: ReadEngine -> B FIFO (wired by default above)
      when(readEngineB.io.outValid && bFifo.io.push.ready) {
        bLoadCnt := bLoadCnt + 1
      }

      // Check for read errors
      when(readEngineA.io.error || readEngineB.io.error) {
        errorDetected := True
        errorCmdId := curCmd.cmdId
        feedState := FeedState.ERROR_DRAIN
      }

      // Both loaded S CLs
      val aDone = aLoadCnt === U(cfg.s, aLoadCnt.getWidth bits)
      val bDone = bLoadCnt === U(cfg.s, bLoadCnt.getWidth bits)
      when(aDone && bDone && !errorDetected) {
        feedState := FeedState.SWAP_TB
      }
    }

    is(FeedState.SWAP_TB) {
      transposeBuffer.io.swap := True
      feedCnt := 0
      feedState := FeedState.FEED
    }

    is(FeedState.FEED) {
      // Feed transposed A column and B row into systolic core
      transposeBuffer.io.rdColIdx := feedCnt

      bFifo.io.pop.ready := True

      for (i <- 0 until cfg.s) {
        systolicCore.io.aIn(i) := transposeBuffer.io.rdData(i)
        systolicCore.io.aValid(i) := True
        systolicCore.io.bIn(i) := bFifo.io.pop.payload((i + 1) * 32 - 1 downto i * 32)
        systolicCore.io.bValid(i) := True
      }

      // Clear bank on first cycle of first pk step for this micro-tile
      when(feedCnt === 0 && stepClearBank) {
        systolicCore.io.clearBank := True
      }

      when(feedCnt === U(cfg.s - 1, feedCnt.getWidth bits)) {
        when(stepDrainTrigger) {
          // Last pk: wait for tail then drain
          tailCnt := 0
          feedState := FeedState.TAIL
        } otherwise {
          // More pk steps: advance scheduler
          tileScheduler.io.stepReady := True
          feedState := FeedState.NEXT_STEP
        }
      } otherwise {
        feedCnt := feedCnt + 1
      }
    }

    is(FeedState.TAIL) {
      // Wait 2*(S-1) cycles for skew drain
      if (cfg.s > 1) {
        when(tailCnt === tailTarget) {
          feedState := FeedState.DRAIN_START
        } otherwise {
          tailCnt := tailCnt + 1
        }
      } else {
        feedState := FeedState.DRAIN_START
      }
    }

    is(FeedState.DRAIN_START) {
      // One-cycle pulse to start drain
      drainPacker.io.drainStart := True
      feedState := FeedState.DRAIN_WAIT
    }

    is(FeedState.DRAIN_WAIT) {
      when(drainPacker.io.drainDone) {
        val lastMiNj = (stepMi === numMi - 1) && (stepNj === numNj - 1)
        val lastPkCmd = stepPkCmd === numPkCmd - 1

        when(lastMiNj && lastPkCmd) {
          feedState := FeedState.FLUSH_START
        } otherwise {
          tileScheduler.io.stepReady := True
          feedState := FeedState.NEXT_STEP
        }
      }
    }

    is(FeedState.FLUSH_START) {
      // One-cycle pulse to start flush
      drainPacker.io.flushStart := True
      feedState := FeedState.FLUSH_D
    }

    is(FeedState.FLUSH_D) {
      when(drainPacker.io.flushDone) {
        tileScheduler.io.stepReady := True
        feedState := FeedState.NEXT_STEP
      }
    }

    is(FeedState.CMD_DONE) {
      // Command fully completed
      cmdActive := False
      feedState := FeedState.IDLE
    }

    is(FeedState.ERROR_DRAIN) {
      // Drain outstanding reads from both engines
      readEngineA.io.outReady := True
      readEngineB.io.outReady := True
      // Drain B FIFO
      bFifo.io.pop.ready := bFifo.io.pop.valid

      when(!readEngineA.io.busy && !readEngineB.io.busy) {
        feedState := FeedState.ERROR_STATUS
      }
    }

    is(FeedState.ERROR_STATUS) {
      statusGen.io.errValid := True
      statusGen.io.errCmdId := errorCmdId
      statusGen.io.errCode := B(0x02, 8 bits)
      cmdActive := False
      feedState := FeedState.IDLE
    }
  }
}
