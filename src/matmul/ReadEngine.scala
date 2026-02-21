package matmul

import spinal.core._
import spinal.lib._

case class ReadEngine(
    addrBits: Int = 64,
    clBits: Int = 128,
    maxOutstandingRd: Int = 16,
    descriptorQueueDepth: Int = 8
) extends Component {
  require((maxOutstandingRd & (maxOutstandingRd - 1)) == 0, "maxOutstandingRd must be power of 2")
  require(descriptorQueueDepth > 0, "descriptorQueueDepth must be > 0")

  private val tagBits = log2Up(maxOutstandingRd)

  val io = new Bundle {
    val start = in Bool()
    val cfgBase = in UInt(addrBits bits)
    val cfgStride = in UInt(addrBits bits)
    val cfgCount = in UInt(16 bits)
    val cfgReady = out Bool()

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
  val descPtrW = log2Up(descriptorQueueDepth)
  val descCountW = log2Up(descriptorQueueDepth + 1)
  val descBaseQ = Vec(Reg(UInt(addrBits bits)) init (0), descriptorQueueDepth)
  val descStrideQ = Vec(Reg(UInt(addrBits bits)) init (0), descriptorQueueDepth)
  val descCountQ = Vec(Reg(UInt(16 bits)) init (0), descriptorQueueDepth)
  val descHead = Reg(UInt(descPtrW bits)) init (0)
  val descTail = Reg(UInt(descPtrW bits)) init (0)
  val descCount = Reg(UInt(descCountW bits)) init (0)

  val descEmpty = descCount === 0
  val descFull = descCount === U(descriptorQueueDepth, descCountW bits)
  val descPopBase = descBaseQ(descHead)
  val descPopStride = descStrideQ(descHead)
  val descPopCount = descCountQ(descHead)

  def ptrInc(ptr: UInt): UInt = {
    if (descriptorQueueDepth == 1) U(0, descPtrW bits)
    else Mux(ptr === U(descriptorQueueDepth - 1, descPtrW bits), U(0, descPtrW bits), ptr + 1)
  }

  val immediateStart = io.start && !active && descEmpty
  val queueStart = io.start && io.cfgReady && !immediateStart

  val loadFromQueue = Bool()
  loadFromQueue := !active && !descEmpty

  io.cfgReady := !descFull

  when(immediateStart) {
    active := True
    nextAddr := io.cfgBase
    stride := io.cfgStride
    remaining := io.cfgCount
    outstanding := 0
    errorReg := False
  }

  when(loadFromQueue) {
    active := True
    nextAddr := descPopBase
    stride := descPopStride
    remaining := descPopCount
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

  val chainFromQueue = Bool()
  chainFromQueue := active && (remaining === 0) && (outstanding === 0) && !descEmpty
  val doneNoChain = Bool()
  doneNoChain := active && (remaining === 0) && (outstanding === 0) && descEmpty
  val popDesc = Bool()
  popDesc := loadFromQueue || chainFromQueue

  when(chainFromQueue) {
    nextAddr := descPopBase
    stride := descPopStride
    remaining := descPopCount
    outstanding := 0
    errorReg := False
  } elsewhen(doneNoChain) {
    active := False
  }

  when(queueStart) {
    descBaseQ(descTail) := io.cfgBase
    descStrideQ(descTail) := io.cfgStride
    descCountQ(descTail) := io.cfgCount
  }

  when(queueStart && !popDesc) {
    descTail := ptrInc(descTail)
    descCount := descCount + 1
  } elsewhen(!queueStart && popDesc) {
    descHead := ptrInc(descHead)
    descCount := descCount - 1
  } elsewhen(queueStart && popDesc) {
    descHead := ptrInc(descHead)
    descTail := ptrInc(descTail)
  }

  io.busy := active || !descEmpty
  io.error := errorReg
  io.outstanding := outstanding
}

case class ReadEngineDescriptor(addrBits: Int) extends Bundle {
  val base = UInt(addrBits bits)
  val stride = UInt(addrBits bits)
  val count = UInt(16 bits)
}
