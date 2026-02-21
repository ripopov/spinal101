package matmul

import spinal.core._

case class ReadEngine(
    addrBits: Int = 64,
    clBits: Int = 512,
    maxOutstandingRd: Int = 16
) extends Component {
  require((maxOutstandingRd & (maxOutstandingRd - 1)) == 0, "maxOutstandingRd must be power of 2")

  private val tagBits = log2Up(maxOutstandingRd)

  val io = new Bundle {
    val start = in Bool()
    val cfgBase = in UInt(addrBits bits)
    val cfgStride = in UInt(addrBits bits)
    val cfgCount = in UInt(16 bits)

    val busy = out Bool()
    val error = out Bool()

    val rdReqValid = out Bool()
    val rdReqReady = in Bool()
    val rdReqAddr = out UInt(addrBits bits)
    val rdReqTag = out UInt(tagBits bits)

    val rdRspValid = in Bool()
    val rdRspReady = out Bool()
    val rdRspData = in Bits(clBits bits)
    val rdRspTag = in UInt(tagBits bits)
    val rdRspErr = in Bool()

    val outValid = out Bool()
    val outReady = in Bool()
    val outData = out Bits(clBits bits)

    val outstanding = out UInt(log2Up(maxOutstandingRd + 1) bits)
  }

  val rb = ReorderBuffer(depth = maxOutstandingRd, dataWidth = clBits)

  val active = Reg(Bool()) init (False)
  val nextAddr = Reg(UInt(addrBits bits)) init (0)
  val stride = Reg(UInt(addrBits bits)) init (0)
  val remaining = Reg(UInt(16 bits)) init (0)
  val outstanding = Reg(UInt(log2Up(maxOutstandingRd + 1) bits)) init (0)
  val errorReg = Reg(Bool()) init (False)

  when(io.start && !active) {
    active := True
    nextAddr := io.cfgBase
    stride := io.cfgStride
    remaining := io.cfgCount
    outstanding := 0
    errorReg := False
  }

  val canIssue = active && (remaining =/= 0) && rb.io.allocReady && (outstanding =/= U(maxOutstandingRd, outstanding.getWidth bits))

  io.rdReqValid := canIssue
  io.rdReqAddr := nextAddr
  io.rdReqTag := rb.io.allocTag

  rb.io.allocValid := io.rdReqValid && io.rdReqReady

  val issuing = io.rdReqValid && io.rdReqReady
  val retiring = io.rdRspValid && io.rdRspReady

  when(issuing) {
    nextAddr := nextAddr + stride
    remaining := remaining - 1
  }

  when(issuing && !retiring) {
    outstanding := outstanding + 1
  } elsewhen(!issuing && retiring && (outstanding =/= 0)) {
    outstanding := outstanding - 1
  }

  io.rdRspReady := True
  rb.io.wrValid := retiring
  rb.io.wrTag := io.rdRspTag
  rb.io.wrData := io.rdRspData

  when(retiring) {
    when(io.rdRspErr) {
      errorReg := True
    }
  }

  io.outValid := rb.io.rdValid
  io.outData := rb.io.rdData
  rb.io.rdReady := io.outReady

  when(active && (remaining === 0) && (outstanding === 0) && (rb.io.entriesUsed === 0)) {
    active := False
  }

  io.busy := active
  io.error := errorReg
  io.outstanding := outstanding
}
