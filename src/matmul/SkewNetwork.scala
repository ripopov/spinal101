package matmul

import spinal.core._

object SkewDirection extends SpinalEnum {
  val WestEdgeA, NorthEdgeB = newElement()
}

case class SkewNetwork(s: Int, direction: SkewDirection.E) extends Component {
  require(s > 0, "s must be > 0")

  val io = new Bundle {
    val dataIn = in Vec(Bits(32 bits), s)
    val validIn = in Vec(Bool(), s)
    val dataOut = out Vec(Bits(32 bits), s)
    val validOut = out Vec(Bool(), s)
  }

  // Delay lane i by exactly i cycles. Direction is explicit for readability and
  // future specialization, but both required skews use the same per-lane delay rule.
  val _useDirection = direction

  for (i <- 0 until s) {
    if (i == 0) {
      io.dataOut(i) := io.dataIn(i)
      io.validOut(i) := io.validIn(i)
    } else {
      var d = io.dataIn(i)
      var v = io.validIn(i)
      for (_ <- 0 until i) {
        d = RegNext(d) init (B(0, 32 bits))
        v = RegNext(v) init (False)
      }
      io.dataOut(i) := d
      io.validOut(i) := v
    }
  }
}
