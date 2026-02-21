package matmul

import spinal.core._

case class SystolicCore(s: Int = 16, lMul: Int = 4, lAdd: Int = 4) extends Component {
  require(s > 0, "s must be > 0")

  val io = new Bundle {
    val aIn = in Vec(Bits(32 bits), s)
    val aValid = in Vec(Bool(), s)

    val bIn = in Vec(Bits(32 bits), s)
    val bValid = in Vec(Bool(), s)

    val bankSel = in Bool()
    val clearBank = in Bool()

    val drainBank = in Bool()
    val drainRow = in UInt (log2Up(s) bits)
    val drainData = out Vec(Bits(32 bits), s)
  }

  val aSkew = SkewNetwork(s = s, direction = SkewDirection.WestEdgeA)
  aSkew.io.dataIn := io.aIn
  aSkew.io.validIn := io.aValid

  val bSkew = SkewNetwork(s = s, direction = SkewDirection.NorthEdgeB)
  bSkew.io.dataIn := io.bIn
  bSkew.io.validIn := io.bValid

  val pes = Array.tabulate(s, s) { (_, _) => PE(lMul = lMul, lAdd = lAdd) }

  val aValidGrid = Array.fill(s, s)(Bool())
  val bValidGrid = Array.fill(s, s)(Bool())

  for (r <- 0 until s) {
    for (c <- 0 until s) {
      val pe = pes(r)(c)

      if (c == 0) {
        pe.io.a_in := aSkew.io.dataOut(r)
        aValidGrid(r)(c) := aSkew.io.validOut(r)
      } else {
        pe.io.a_in := pes(r)(c - 1).io.a_out
        aValidGrid(r)(c) := RegNext(aValidGrid(r)(c - 1)) init (False)
      }

      if (r == 0) {
        pe.io.b_in := bSkew.io.dataOut(c)
        bValidGrid(r)(c) := bSkew.io.validOut(c)
      } else {
        pe.io.b_in := pes(r - 1)(c).io.b_out
        bValidGrid(r)(c) := RegNext(bValidGrid(r - 1)(c)) init (False)
      }

      pe.io.a_valid := aValidGrid(r)(c)
      pe.io.b_valid := bValidGrid(r)(c)
      pe.io.bank_sel := io.bankSel
      pe.io.clear := io.clearBank
      pe.io.drain_bank := io.drainBank
    }
  }

  for (c <- 0 until s) {
    val rowValues = Vec(Bits(32 bits), s)
    for (r <- 0 until s) {
      rowValues(r) := pes(r)(c).io.drain_data
    }
    io.drainData(c) := rowValues(io.drainRow)
  }
}
