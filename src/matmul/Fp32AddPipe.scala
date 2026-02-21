package matmul

import spinal.core._

case class Fp32AddPipe(stages: Int = 4) extends Component {
  require(stages >= 0, "stages must be >= 0")

  val io = new Bundle {
    val a = in Bits (32 bits)
    val b = in Bits (32 bits)
    val validIn = in Bool()

    val result = out Bits (32 bits)
    val validOut = out Bool()
  }

  val raw = Fp32Math.add(io.a, io.b)

  if (stages == 0) {
    io.result := raw
    io.validOut := io.validIn
  } else {
    var dataPipe = raw
    var validPipe = io.validIn

    for (_ <- 0 until stages) {
      dataPipe = RegNext(dataPipe) init (B(0, 32 bits))
      validPipe = RegNext(validPipe) init (False)
    }

    io.result := dataPipe
    io.validOut := validPipe
  }
}
