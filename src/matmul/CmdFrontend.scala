package matmul

import spinal.core._
import spinal.lib._

object CmdErrorCode {
  val None = B(0x00, 8 bits)
  val Invalid = B(0x01, 8 bits)
}

case class CmdFrontend(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val io = new Bundle {
    val cmdValid = in Bool()
    val cmdReady = out Bool()
    val cmdDesc = in(CmdDesc(cfg))

    val outValid = out Bool()
    val outReady = in Bool()
    val outDesc = out(CmdDesc(cfg))

    val rejValid = out Bool()
    val rejReady = in Bool()
    val rejCmdId = out UInt(16 bits)
    val rejErrCode = out Bits(8 bits)
  }

  val fifo = StreamFifo(CmdDesc(cfg), cfg.cmdqDepth)

  val e = U(cfg.clElems, 16 bits)
  val eInt = cfg.clElems

  def inSet(value: UInt, elems: Seq[Int]): Bool = {
    elems.map(x => value === U(x, value.getWidth bits)).reduce(_ || _)
  }

  def isAligned(addr: UInt): Bool = {
    val mask = U(cfg.clBytes - 1, cfg.addrBits bits)
    (addr & mask) === 0
  }

  val validPrimM = inSet(io.cmdDesc.primM, Seq(eInt, 2 * eInt, 4 * eInt, 8 * eInt))
  val validPrimN = inSet(io.cmdDesc.primN, Seq(eInt, 2 * eInt, 4 * eInt, 8 * eInt))
  val validPrimK = inSet(io.cmdDesc.primK, Seq(eInt, 2 * eInt, 4 * eInt, 8 * eInt, 16 * eInt))

  val mPos = io.cmdDesc.m =/= 0
  val nPos = io.cmdDesc.n =/= 0
  val kPos = io.cmdDesc.k =/= 0

  val dimsElemAligned =
    (io.cmdDesc.m % e) === 0 &&
      (io.cmdDesc.n % e) === 0 &&
      (io.cmdDesc.k % e) === 0

  val dimsPrimDiv =
    (io.cmdDesc.m % io.cmdDesc.primM) === 0 &&
      (io.cmdDesc.n % io.cmdDesc.primN) === 0 &&
      (io.cmdDesc.k % io.cmdDesc.primK) === 0

  val strideOk =
    (io.cmdDesc.lda >= io.cmdDesc.k) &&
      (io.cmdDesc.ldb >= io.cmdDesc.n) &&
      (io.cmdDesc.ldd >= io.cmdDesc.n)

  val addrOk =
    isAligned(io.cmdDesc.aBase) &&
      isAligned(io.cmdDesc.bBase) &&
      isAligned(io.cmdDesc.dBase)

  val flagsOk = io.cmdDesc.flags === 0

  val isValidDesc =
    mPos && nPos && kPos &&
      validPrimM && validPrimN && validPrimK &&
      dimsElemAligned && dimsPrimDiv &&
      strideOk && addrOk && flagsOk

  val rejPending = Reg(Bool()) init (False)
  val rejCmdIdReg = Reg(UInt(16 bits)) init (0)
  val rejErrReg = Reg(Bits(8 bits)) init (CmdErrorCode.None)

  val canAcceptReject = !rejPending || io.rejReady

  io.cmdReady := Mux(isValidDesc, fifo.io.push.ready, canAcceptReject)

  fifo.io.push.valid := io.cmdValid && io.cmdReady && isValidDesc
  fifo.io.push.payload := io.cmdDesc

  io.outValid := fifo.io.pop.valid
  io.outDesc := fifo.io.pop.payload
  fifo.io.pop.ready := io.outReady

  when(io.cmdValid && io.cmdReady && !isValidDesc) {
    rejPending := True
    rejCmdIdReg := io.cmdDesc.cmdId
    rejErrReg := CmdErrorCode.Invalid
  } elsewhen(rejPending && io.rejReady) {
    rejPending := False
    rejErrReg := CmdErrorCode.None
  }

  io.rejValid := rejPending
  io.rejCmdId := rejCmdIdReg
  io.rejErrCode := rejErrReg
}
