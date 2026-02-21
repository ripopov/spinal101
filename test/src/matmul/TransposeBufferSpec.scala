package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class TransposeBufferSpec extends AnyFunSuite {
  test("TransposeBuffer writes rows and reads columns after swap") {
    val s = 4
    SpinalSimConfig().withVerilator.compile(TransposeBuffer(s = s)).doSim("transpose_buffer") { dut =>
      dut.clockDomain.forkStimulus(2)

      dut.io.wrValid #= false
      dut.io.wrRowIdx #= 0
      dut.io.swap #= false
      dut.io.rdColIdx #= 0
      for (i <- 0 until s) dut.io.wrData(i) #= 0

      // Fill write bank rows.
      for (r <- 0 until s) {
        dut.io.wrRowIdx #= r
        for (c <- 0 until s) {
          dut.io.wrData(c) #= (r * 10 + c)
        }
        dut.io.wrValid #= true
        dut.clockDomain.waitSampling()
      }
      dut.io.wrValid #= false

      // Swap so written bank becomes read bank.
      dut.io.swap #= true
      dut.clockDomain.waitSampling()
      dut.io.swap #= false

      // Read each column and verify transpose behavior.
      for (col <- 0 until s) {
        dut.io.rdColIdx #= col
        dut.clockDomain.waitSampling()
        for (row <- 0 until s) {
          val got = dut.io.rdData(row).toBigInt.intValue
          val exp = row * 10 + col
          assert(got == exp, s"transpose mismatch row=$row col=$col got=$got exp=$exp")
        }
      }
    }
  }
}
