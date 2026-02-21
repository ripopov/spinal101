package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class ReorderBufferSpec extends AnyFunSuite {
  test("ReorderBuffer retires in allocation order with scrambled writes") {
    SpinalSimConfig().withVerilator.compile(ReorderBuffer(depth = 8, dataWidth = 32)).doSim("reorder_buffer") { dut =>
      dut.clockDomain.forkStimulus(2)

      dut.io.allocValid #= false
      dut.io.wrValid #= false
      dut.io.wrTag #= 0
      dut.io.wrData #= 0
      dut.io.rdReady #= false

      def allocOne(): Int = {
        dut.io.allocValid #= true
        dut.clockDomain.waitSampling()
        val tag = dut.io.allocTag.toBigInt.intValue
        assert(dut.io.allocReady.toBoolean)
        dut.io.allocValid #= false
        tag
      }

      val t0 = allocOne()
      val t1 = allocOne()
      val t2 = allocOne()
      val t3 = allocOne()

      val payload = Map(t0 -> 0x11111111, t1 -> 0x22222222, t2 -> 0x33333333, t3 -> 0x44444444)

      def writeTag(tag: Int): Unit = {
        dut.io.wrTag #= tag
        dut.io.wrData #= payload(tag)
        dut.io.wrValid #= true
        dut.clockDomain.waitSampling()
        dut.io.wrValid #= false
      }

      // Scrambled completion order.
      writeTag(t2)
      writeTag(t0)
      writeTag(t3)
      writeTag(t1)

      dut.io.rdReady #= true
      val got = scala.collection.mutable.ArrayBuffer.empty[Int]
      var cycles = 0
      while (got.size < 4 && cycles < 20) {
        dut.clockDomain.waitSampling()
        if (dut.io.rdValid.toBoolean) {
          got += dut.io.rdData.toBigInt.intValue
        }
        cycles += 1
      }

      assert(got.toSeq == Seq(payload(t0), payload(t1), payload(t2), payload(t3)))
    }
  }
}
