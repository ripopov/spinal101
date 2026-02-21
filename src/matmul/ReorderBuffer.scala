package matmul

import spinal.core._

case class ReorderBuffer(depth: Int, dataWidth: Int) extends Component {
  require(depth > 0 && (depth & (depth - 1)) == 0, "depth must be a power of 2")
  require(dataWidth > 0, "dataWidth must be > 0")

  private val tagBits = log2Up(depth)

  val io = new Bundle {
    val allocValid = in Bool()
    val allocReady = out Bool()
    val allocTag = out UInt(tagBits bits)

    val wrValid = in Bool()
    val wrTag = in UInt(tagBits bits)
    val wrData = in Bits(dataWidth bits)

    val rdValid = out Bool()
    val rdReady = in Bool()
    val rdData = out Bits(dataWidth bits)

    val entriesUsed = out UInt(log2Up(depth + 1) bits)
  }

  val dataMem = Vec(Reg(Bits(dataWidth bits)) init (B(0, dataWidth bits)), depth)
  val allocated = Vec(Reg(Bool()) init (False), depth)
  val complete = Vec(Reg(Bool()) init (False), depth)

  val head = Reg(UInt(tagBits bits)) init (0)
  val tail = Reg(UInt(tagBits bits)) init (0)
  val used = Reg(UInt(log2Up(depth + 1) bits)) init (0)

  io.allocReady := used =/= U(depth, used.getWidth bits)
  io.allocTag := tail

  io.rdValid := allocated(head) && complete(head)
  io.rdData := dataMem(head)

  when(io.allocValid && io.allocReady) {
    allocated(tail) := True
    complete(tail) := False
    tail := tail + 1
    used := used + 1
  }

  when(io.wrValid) {
    assert(allocated(io.wrTag), "write to unallocated tag")
    dataMem(io.wrTag) := io.wrData
    complete(io.wrTag) := True
  }

  when(io.rdValid && io.rdReady) {
    allocated(head) := False
    complete(head) := False
    head := head + 1
    used := used - 1
  }

  io.entriesUsed := used
}
