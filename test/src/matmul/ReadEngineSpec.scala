package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class ReadEngineSpec extends AnyFunSuite {
  test("ReadEngine enforces credits, allocates tags, and outputs in-order stream") {
    SpinalSimConfig().withVerilator.compile(ReadEngine(addrBits = 64, clBits = 128, maxOutstandingRd = 4)).doSim("read_engine") {
      dut =>
        dut.clockDomain.forkStimulus(2)

        dut.io.start #= false
        dut.io.cfgBase #= BigInt(0x1000)
        dut.io.cfgStride #= 16
        dut.io.cfgCount #= 4

        dut.io.rdReqReady #= true
        dut.io.rdRspValid #= false
        dut.io.rdRspData #= 0
        dut.io.rdRspTag #= 0
        dut.io.rdRspErr #= false

        dut.io.outReady #= false

        dut.clockDomain.waitSampling()
        dut.io.start #= true
        dut.clockDomain.waitSampling()
        dut.io.start #= false

        val reqs = scala.collection.mutable.ArrayBuffer.empty[(BigInt, Int)]
        var cycles = 0
        while (reqs.size < 4 && cycles < 30) {
          dut.clockDomain.waitSampling()
          if (dut.io.rdReqValid.toBoolean && dut.io.rdReqReady.toBoolean) {
            reqs += ((dut.io.rdReqAddr.toBigInt, dut.io.rdReqTag.toBigInt.intValue))
          }
          cycles += 1
        }

        assert(reqs.size == 4)
        assert(reqs.map(_._1) == Seq(BigInt(0x1000), BigInt(0x1010), BigInt(0x1020), BigInt(0x1030)))

        // Response order scrambled by tag; output must still be request order.
        val dataByTag = reqs.zipWithIndex.map { case ((_, tag), idx) => tag -> BigInt(idx + 1) }.toMap
        val scrambled = Seq(reqs(2)._2, reqs(0)._2, reqs(3)._2, reqs(1)._2)

        for (tag <- scrambled) {
          dut.io.rdRspTag #= tag
          dut.io.rdRspData #= dataByTag(tag)
          dut.io.rdRspErr #= false
          dut.io.rdRspValid #= true
          dut.clockDomain.waitSampling()
          dut.io.rdRspValid #= false
        }

        dut.io.outReady #= true
        val out = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        cycles = 0
        while (out.size < 4 && cycles < 40) {
          dut.clockDomain.waitSampling()
          if (dut.io.outValid.toBoolean && dut.io.outReady.toBoolean) {
            out += dut.io.outData.toBigInt
          }
          cycles += 1
        }

        assert(out.toSeq == Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(4)))
      }
  }

  test("ReadEngine accepts a queued descriptor while current transfer is active") {
    SpinalSimConfig().withVerilator.compile(ReadEngine(addrBits = 64, clBits = 128, maxOutstandingRd = 4)).doSim(
      "read_engine_queued"
    ) { dut =>
      dut.clockDomain.forkStimulus(2)

      dut.io.start #= false
      dut.io.cfgBase #= 0
      dut.io.cfgStride #= 16
      dut.io.cfgCount #= 0

      dut.io.rdReqReady #= true
      dut.io.rdRspValid #= false
      dut.io.rdRspData #= 0
      dut.io.rdRspTag #= 0
      dut.io.rdRspErr #= false
      dut.io.outReady #= true

      val reqAddrs = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      val outData = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      val rspQ = scala.collection.mutable.Queue.empty[(Int, BigInt)]
      var nextData = 1

      fork {
        while (true) {
          dut.clockDomain.waitSampling()

          if (dut.io.rdReqValid.toBoolean && dut.io.rdReqReady.toBoolean) {
            reqAddrs += dut.io.rdReqAddr.toBigInt
            rspQ.enqueue((dut.io.rdReqTag.toBigInt.intValue, BigInt(nextData)))
            nextData += 1
          }

          if (rspQ.nonEmpty) {
            val (tag, data) = rspQ.dequeue()
            dut.io.rdRspTag #= tag
            dut.io.rdRspData #= data
            dut.io.rdRspErr #= false
            dut.io.rdRspValid #= true
          } else {
            dut.io.rdRspValid #= false
          }

          if (dut.io.outValid.toBoolean && dut.io.outReady.toBoolean) {
            outData += dut.io.outData.toBigInt
          }
        }
      }

      dut.io.cfgBase #= BigInt(0x1000)
      dut.io.cfgStride #= 16
      dut.io.cfgCount #= 2
      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      dut.clockDomain.waitSampling()
      assert(dut.io.cfgReady.toBoolean, "queued descriptor slot should be available")
      dut.io.cfgBase #= BigInt(0x2000)
      dut.io.cfgStride #= 16
      dut.io.cfgCount #= 2
      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      var cycles = 0
      while (outData.size < 4 && cycles < 80) {
        dut.clockDomain.waitSampling()
        cycles += 1
      }

      assert(outData.size == 4, s"expected 4 outputs, got ${outData.size}")
      assert(reqAddrs.toSeq == Seq(BigInt(0x1000), BigInt(0x1010), BigInt(0x2000), BigInt(0x2010)))
      assert(outData.toSeq == Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(4)))
    }
  }
}
