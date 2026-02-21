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

  test("ReadEngine accepts multiple queued descriptors and chains request issue with bounded gaps") {
    SpinalSimConfig().withVerilator.compile(
      ReadEngine(addrBits = 64, clBits = 128, maxOutstandingRd = 4, descriptorQueueDepth = 8)
    ).doSim("read_engine_queued") { dut =>
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
      val reqCycles = scala.collection.mutable.ArrayBuffer.empty[Int]
      val outData = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      val rspQ = scala.collection.mutable.Queue.empty[(Int, BigInt)]
      var nextData = 1
      var cycle = 0

      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          cycle += 1

          if (dut.io.rdReqValid.toBoolean && dut.io.rdReqReady.toBoolean) {
            reqAddrs += dut.io.rdReqAddr.toBigInt
            reqCycles += cycle
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

      val descs = Seq(
        (BigInt(0x1000), 2),
        (BigInt(0x2000), 2),
        (BigInt(0x3000), 2),
        (BigInt(0x4000), 2)
      )

      var di = 0
      while (di < descs.length) {
        dut.clockDomain.waitSampling()
        if (dut.io.cfgReady.toBoolean) {
          dut.io.cfgBase #= descs(di)._1
          dut.io.cfgStride #= 16
          dut.io.cfgCount #= descs(di)._2
          dut.io.start #= true
          dut.clockDomain.waitSampling()
          dut.io.start #= false
          di += 1
        }
      }

      var cycles = 0
      while (outData.size < 8 && cycles < 200) {
        dut.clockDomain.waitSampling()
        cycles += 1
      }

      val expectedAddrs = Seq(
        BigInt(0x1000), BigInt(0x1010),
        BigInt(0x2000), BigInt(0x2010),
        BigInt(0x3000), BigInt(0x3010),
        BigInt(0x4000), BigInt(0x4010)
      )
      assert(outData.size == 8, s"expected 8 outputs, got ${outData.size}")
      assert(reqAddrs.toSeq == expectedAddrs, s"unexpected request addresses: ${reqAddrs.mkString(",")}")
      assert(outData.toSeq == Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(4), BigInt(5), BigInt(6), BigInt(7), BigInt(8)))

      val reqGaps = reqCycles.sliding(2).collect { case Seq(c0, c1) => c1 - c0 }.toSeq
      val maxReqGap = if (reqGaps.nonEmpty) reqGaps.max else 0
      assert(maxReqGap <= 3, s"descriptor chaining introduced excessive request gap: maxReqGap=$maxReqGap")
    }
  }
}
