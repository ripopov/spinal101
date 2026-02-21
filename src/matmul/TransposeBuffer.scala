package matmul

import spinal.core._

case class TransposeBuffer(s: Int = 16) extends Component {
  require(s > 0, "s must be > 0")

  val io = new Bundle {
    val wrValid = in Bool()
    val wrRowIdx = in UInt(log2Up(s) bits)
    val wrData = in Vec(Bits(32 bits), s)

    val swap = in Bool()

    val rdColIdx = in UInt(log2Up(s) bits)
    val rdData = out Vec(Bits(32 bits), s)

    val wrBank = out Bool()
    val rdBank = out Bool()
  }

  val bank0 = Vec(Vec(Reg(Bits(32 bits)) init (B(0, 32 bits)), s), s)
  val bank1 = Vec(Vec(Reg(Bits(32 bits)) init (B(0, 32 bits)), s), s)

  val writeBank = Reg(Bool()) init (False)

  when(io.swap) {
    writeBank := !writeBank
  }

  when(io.wrValid) {
    for (c <- 0 until s) {
      when(writeBank) {
        bank1(io.wrRowIdx)(c) := io.wrData(c)
      } otherwise {
        bank0(io.wrRowIdx)(c) := io.wrData(c)
      }
    }
  }

  val readBank = !writeBank
  for (r <- 0 until s) {
    io.rdData(r) := Mux(readBank, bank1(r)(io.rdColIdx), bank0(r)(io.rdColIdx))
  }

  io.wrBank := writeBank
  io.rdBank := readBank
}
