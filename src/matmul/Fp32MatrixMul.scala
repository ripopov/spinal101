package matmul

import spinal.core._

case class Fp32MatrixMulV0IO(size: Int) extends Bundle {
  val start = in Bool()
  val busy = out Bool()
  val done = out Bool()
  val a = in Vec (Vec(Bits(32 bits), size), size)
  val b = in Vec (Vec(Bits(32 bits), size), size)
  val c = out Vec (Vec(Bits(32 bits), size), size)
}

case class Fp32MatrixMulV0(size: Int = 2) extends Component {
  require(size >= 2, "size must be >= 2")

  private val indexWidth = log2Up(size)
  private val flatIndexWidth = log2Up(size * size)

  val io = new Fp32MatrixMulV0IO(size)

  val aReg = Vec(Reg(Bits(32 bits)) init (0), size * size)
  val bReg = Vec(Reg(Bits(32 bits)) init (0), size * size)
  val cReg = Vec(Reg(Bits(32 bits)) init (0), size * size)

  val busyReg = Reg(Bool()) init (False)
  val doneReg = Reg(Bool()) init (False)

  val rowCounter = Reg(UInt(indexWidth bits)) init (0)
  val colCounter = Reg(UInt(indexWidth bits)) init (0)
  val kCounter = Reg(UInt(indexWidth bits)) init (0)

  val accReg = Reg(Bits(32 bits)) init (0)

  private def flatIndex(row: UInt, col: UInt): UInt = {
    val idx = UInt(flatIndexWidth bits)
    idx := (row * size + col).resized
    idx
  }

  val aIndex = flatIndex(rowCounter, kCounter)
  val bIndex = flatIndex(kCounter, colCounter)
  val cIndex = flatIndex(rowCounter, colCounter)

  io.busy := busyReg
  io.done := doneReg

  for (row <- 0 until size; col <- 0 until size) {
    io.c(row)(col) := cReg(row * size + col)
  }

  doneReg := False

  when(io.start && !busyReg) {
    for (row <- 0 until size; col <- 0 until size) {
      val idx = row * size + col
      aReg(idx) := io.a(row)(col)
      bReg(idx) := io.b(row)(col)
      cReg(idx) := B(0, 32 bits)
    }

    rowCounter := 0
    colCounter := 0
    kCounter := 0
    accReg := B(0, 32 bits)
    busyReg := True
  } elsewhen (busyReg) {
    val product = Fp32Math.mul(aReg(aIndex), bReg(bIndex))
    val accumulated = Bits(32 bits)
    accumulated := Mux(kCounter === 0, product, Fp32Math.add(accReg, product))

    accReg := accumulated

    when(kCounter === size - 1) {
      cReg(cIndex) := accumulated
      kCounter := 0

      when(colCounter === size - 1) {
        colCounter := 0
        when(rowCounter === size - 1) {
          busyReg := False
          doneReg := True
        } otherwise {
          rowCounter := rowCounter + 1
        }
      } otherwise {
        colCounter := colCounter + 1
      }
    } otherwise {
      kCounter := kCounter + 1
    }
  }
}
