package matmul

import spinal.core._
import spinal.lib._

case class DrainPacker(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val maxDBufferCls: Int = 8 * 8 * cfg.s
  val dBufIdxW: Int = log2Up(maxDBufferCls)

  val io = new Bundle {
    val drainStart = in Bool()
    val drainDone = out Bool()

    val pi = in UInt(16 bits)
    val pj = in UInt(16 bits)
    val pkCmd = in UInt(16 bits)
    val mi = in UInt(16 bits)
    val nj = in UInt(16 bits)
    val numPkCmd = in UInt(16 bits)
    val numMi = in UInt(16 bits)
    val numNj = in UInt(16 bits)
    val numPi = in UInt(16 bits)
    val numPj = in UInt(16 bits)

    val cmdDesc = in(CmdDesc(cfg))

    val drainBankIn = in Bool()
    val drainBank = out Bool()
    val drainRow = out UInt(log2Up(cfg.s) bits)
    val drainData = in Vec(Bits(32 bits), cfg.s)

    val flushStart = in Bool()
    val flushDone = out Bool()

    val outValid = out Bool()
    val outReady = in Bool()
    val outAddr = out UInt(cfg.addrBits bits)
    val outData = out Bits(cfg.clBits bits)
    val outLast = out Bool()
    val outCmdId = out UInt(16 bits)

    val cmdDone = out Bool()
  }

  val dBuffer = Mem(Bits(cfg.clBits bits), maxDBufferCls)

  object S extends SpinalEnum {
    val IDLE, DRAIN_READ, DRAIN_PACK, DRAIN_RMW_READ, DRAIN_RMW_ADD, FLUSH_READ, FLUSH_OUT = newElement()
  }
  val state = RegInit(S.IDLE)

  val rowCnt = Reg(UInt(log2Up(cfg.s) bits)) init (0)
  val packedCl = Reg(Bits(cfg.clBits bits)) init (0)

  val bufMi = Reg(UInt(16 bits)) init (0)
  val bufNj = Reg(UInt(16 bits)) init (0)
  val bufIdx = Reg(UInt(dBufIdxW bits)) init (0)

  val flushMi = Reg(UInt(16 bits)) init (0)
  val flushNj = Reg(UInt(16 bits)) init (0)
  val flushRow = Reg(UInt(log2Up(cfg.s) bits)) init (0)
  val flushIdx = Reg(UInt(dBufIdxW bits)) init (0)

  // Metadata aligned with readSync output during FLUSH_OUT.
  val flushOutAddr = Reg(UInt(cfg.addrBits bits)) init (0)
  val flushOutLast = Reg(Bool()) init (False)
  val flushOutCmdDone = Reg(Bool()) init (False)
  val flushOutIsLastFlush = Reg(Bool()) init (False)

  // Unified read port: address mux + registered read
  val rdAddr = UInt(dBufIdxW bits)
  rdAddr := flushIdx // default: flush path
  when(state === S.DRAIN_RMW_READ) {
    rdAddr := bufIdx
  }
  val rdEn = Bool()
  rdEn := (state === S.DRAIN_RMW_READ) || (state === S.FLUSH_READ)
  val rdData = dBuffer.readSync(rdAddr, rdEn)

  io.drainDone := False
  io.flushDone := False
  io.cmdDone := False
  io.drainBank := io.drainBankIn
  io.drainRow := rowCnt
  io.outValid := False
  io.outAddr := 0
  io.outData := 0
  io.outLast := False
  io.outCmdId := io.cmdDesc.cmdId

  // Write port
  val wrEn = Bool()
  val wrAddr = UInt(dBufIdxW bits)
  val wrData = Bits(cfg.clBits bits)
  wrEn := False
  wrAddr := bufIdx
  wrData := packedCl
  dBuffer.write(wrAddr, wrData, wrEn)

  def computeBufIdx(mi: UInt, nj: UInt, row: UInt): UInt = {
    ((mi * io.numNj + nj) * U(cfg.s, 16 bits) + row.resize(16)).resize(dBufIdxW)
  }

  def packDrainData(): Bits = {
    val cl = Bits(cfg.clBits bits)
    for (e <- 0 until cfg.s) {
      cl((e + 1) * 32 - 1 downto e * 32) := io.drainData(e)
    }
    cl
  }

  def addCls(existing: Bits, newData: Bits): Bits = {
    val result = Bits(cfg.clBits bits)
    for (e <- 0 until cfg.s) {
      result((e + 1) * 32 - 1 downto e * 32) :=
        Fp32Math.add(
          existing((e + 1) * 32 - 1 downto e * 32),
          newData((e + 1) * 32 - 1 downto e * 32)
        )
    }
    result
  }

  def computeFlushAddr(mi: UInt, nj: UInt, row: UInt): UInt = {
    val globalM = (io.pi.resize(cfg.addrBits) * io.cmdDesc.primM.resize(cfg.addrBits) +
      mi.resize(cfg.addrBits) * U(cfg.s, cfg.addrBits bits) +
      row.resize(cfg.addrBits)).resize(cfg.addrBits)
    val globalN = (io.pj.resize(cfg.addrBits) * io.cmdDesc.primN.resize(cfg.addrBits) +
      nj.resize(cfg.addrBits) * U(cfg.s, cfg.addrBits bits)).resize(cfg.addrBits)
    val offset = (globalM * io.cmdDesc.ldd.resize(cfg.addrBits) + globalN).resize(cfg.addrBits)
    (io.cmdDesc.dBase + offset * U(4, cfg.addrBits bits)).resize(cfg.addrBits)
  }

  def isLastFlush(): Bool = {
    val lastMi = flushMi === (io.numMi - 1)
    val lastNj = flushNj === (io.numNj - 1)
    val lastRow = flushRow === U(cfg.s - 1, flushRow.getWidth bits)
    lastMi && lastNj && lastRow
  }

  def isLastCmd(): Bool = {
    isLastFlush() && (io.pi === (io.numPi - 1)) && (io.pj === (io.numPj - 1))
  }

  switch(state) {
    is(S.IDLE) {
      when(io.flushStart) {
        flushMi := 0
        flushNj := 0
        flushRow := 0
        flushIdx := 0
        state := S.FLUSH_READ
      } elsewhen(io.drainStart) {
        rowCnt := 0
        bufMi := io.mi
        bufNj := io.nj
        state := S.DRAIN_READ
      }
    }

    is(S.DRAIN_READ) {
      packedCl := packDrainData()
      bufIdx := computeBufIdx(bufMi, bufNj, rowCnt.resize(16))
      when(io.pkCmd === 0) {
        state := S.DRAIN_PACK
      } otherwise {
        state := S.DRAIN_RMW_READ
      }
    }

    is(S.DRAIN_PACK) {
      wrEn := True
      wrAddr := bufIdx
      wrData := packedCl
      when(rowCnt === U(cfg.s - 1, rowCnt.getWidth bits)) {
        io.drainDone := True
        state := S.IDLE
      } otherwise {
        rowCnt := rowCnt + 1
        state := S.DRAIN_READ
      }
    }

    is(S.DRAIN_RMW_READ) {
      // rdAddr is driven to bufIdx above; rdData available next cycle
      state := S.DRAIN_RMW_ADD
    }

    is(S.DRAIN_RMW_ADD) {
      val result = addCls(rdData, packedCl)
      wrEn := True
      wrAddr := bufIdx
      wrData := result
      when(rowCnt === U(cfg.s - 1, rowCnt.getWidth bits)) {
        io.drainDone := True
        state := S.IDLE
      } otherwise {
        rowCnt := rowCnt + 1
        state := S.DRAIN_READ
      }
    }

    is(S.FLUSH_READ) {
      // rdAddr defaults to flushIdx; rdData is available next cycle.
      // Latch metadata now so it aligns with rdData in FLUSH_OUT.
      flushOutAddr := computeFlushAddr(flushMi, flushNj, flushRow.resize(16))
      flushOutIsLastFlush := isLastFlush()
      flushOutCmdDone := isLastCmd()
      flushOutLast := isLastCmd()
      state := S.FLUSH_OUT
    }

    is(S.FLUSH_OUT) {
      io.outValid := True
      io.outAddr := flushOutAddr
      io.outData := rdData
      io.outLast := flushOutLast
      io.outCmdId := io.cmdDesc.cmdId

      when(io.outReady) {
        when(flushOutIsLastFlush) {
          when(flushOutCmdDone) {
            io.cmdDone := True
          }
          io.flushDone := True
          state := S.IDLE
        } otherwise {
          when(flushRow =/= U(cfg.s - 1, flushRow.getWidth bits)) {
            flushRow := flushRow + 1
            flushIdx := flushIdx + 1
          } otherwise {
            flushRow := 0
            when(flushNj + 1 < io.numNj) {
              flushNj := flushNj + 1
              flushIdx := flushIdx + 1
            } otherwise {
              flushNj := 0
              flushMi := flushMi + 1
              flushIdx := flushIdx + 1
            }
          }
          state := S.FLUSH_READ
        }
      }
    }
  }
}
