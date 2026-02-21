package matmul

import spinal.core._

case class PE(lMul: Int = 4, lAdd: Int = 4) extends Component {
  require(lMul >= 0, "lMul must be >= 0")
  require(lAdd >= 0, "lAdd must be >= 0")

  val io = new Bundle {
    val a_in = in Bits (32 bits)
    val b_in = in Bits (32 bits)
    val a_valid = in Bool()
    val b_valid = in Bool()
    val bank_sel = in Bool()
    val clear = in Bool()

    val a_out = out Bits (32 bits)
    val b_out = out Bits (32 bits)

    val drain_bank = in Bool()
    val drain_data = out Bits (32 bits)
  }

  io.a_out := RegNext(io.a_in) init (B(0, 32 bits))
  io.b_out := RegNext(io.b_in) init (B(0, 32 bits))

  val inputValid = io.a_valid && io.b_valid

  val mulPipe = Fp32MulPipe(stages = lMul)
  mulPipe.io.a := io.a_in
  mulPipe.io.b := io.b_in
  mulPipe.io.validIn := inputValid

  val bankSelMul = if (lMul == 0) {
    io.bank_sel
  } else {
    var pipe = io.bank_sel
    for (_ <- 0 until lMul) {
      pipe = RegNext(pipe) init (False)
    }
    pipe
  }

  val bank0 = Reg(Bits(32 bits)) init (B(0, 32 bits))
  val bank1 = Reg(Bits(32 bits)) init (B(0, 32 bits))

  val bank0Shadow = Reg(Bits(32 bits)) init (B(0, 32 bits))
  val bank1Shadow = Reg(Bits(32 bits)) init (B(0, 32 bits))

  val accBase = Mux(bankSelMul, bank1Shadow, bank0Shadow)

  val addPipe = Fp32AddPipe(stages = lAdd)
  addPipe.io.a := accBase
  addPipe.io.b := mulPipe.io.result
  addPipe.io.validIn := mulPipe.io.validOut

  val bankSelAdd = if (lAdd == 0) {
    bankSelMul
  } else {
    var pipe = bankSelMul
    for (_ <- 0 until lAdd) {
      pipe = RegNext(pipe) init (False)
    }
    pipe
  }

  when(io.clear) {
    when(io.bank_sel) {
      bank1 := B(0, 32 bits)
      bank1Shadow := B(0, 32 bits)
    } otherwise {
      bank0 := B(0, 32 bits)
      bank0Shadow := B(0, 32 bits)
    }
  } otherwise {
    // Speculative running sum used for throughput-1 accumulation.
    when(mulPipe.io.validOut) {
      when(bankSelMul) {
        bank1Shadow := Fp32Math.add(bank1Shadow, mulPipe.io.result)
      } otherwise {
        bank0Shadow := Fp32Math.add(bank0Shadow, mulPipe.io.result)
      }
    }

    // Committed/drain-visible bank value follows pipelined adder output.
    when(addPipe.io.validOut) {
      when(bankSelAdd) {
        bank1 := addPipe.io.result
      } otherwise {
        bank0 := addPipe.io.result
      }
    }
  }

  io.drain_data := Mux(io.drain_bank, bank1, bank0)
}
