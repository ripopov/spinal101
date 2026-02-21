package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

class Fp32PipeSpec extends AnyFunSuite {
  private def specials: Seq[Int] = Seq(
    0x00000000,
    0x80000000,
    0x7f800000,
    0xff800000,
    0x7fc00000,
    0x7f800001,
    0x00000001,
    0x00400000,
    0x80000001,
    java.lang.Float.floatToRawIntBits(1.0f),
    java.lang.Float.floatToRawIntBits(-1.0f),
    java.lang.Float.floatToRawIntBits(3.1415926f),
    java.lang.Float.floatToRawIntBits(-2.7182817f),
    java.lang.Float.floatToRawIntBits(Float.MaxValue),
    java.lang.Float.floatToRawIntBits(Float.MinPositiveValue)
  )

  private def randomPairs(seed: Int, n: Int): Seq[(Int, Int)] = {
    val rng = new Random(seed)
    (0 until n).map(_ => (rng.nextInt(), rng.nextInt()))
  }

  private def testPairs: Seq[(Int, Int)] = {
    val sp = specials
    val cross = for (a <- sp; b <- sp) yield (a, b)
    cross ++ randomPairs(0x1337, 400)
  }

  private case class PipeIn(a: Int, b: Int, valid: Boolean)

  private def buildStimulus(stageCount: Int): Seq[PipeIn] = {
    val stimulus = {
      val rng = new Random(0x4242)
      testPairs.flatMap { case (a, b) =>
        if (rng.nextBoolean()) Seq(PipeIn(0, 0, valid = false), PipeIn(a, b, valid = true))
        else Seq(PipeIn(a, b, valid = true))
      } ++ Seq.fill(stageCount + 3)(PipeIn(0, 0, valid = false))
    }
    stimulus
  }

  private def runMulPipe(stageCount: Int): Unit = {
    case class Dut() extends Component {
      val io = new Bundle {
        val a = in Bits(32 bits)
        val b = in Bits(32 bits)
        val validIn = in Bool()
        val result = out Bits(32 bits)
        val validOut = out Bool()
      }
      val p = Fp32MulPipe(stageCount)
      p.io.a := io.a
      p.io.b := io.b
      p.io.validIn := io.validIn
      io.result := p.io.result
      io.validOut := p.io.validOut
    }

    val stimulus = buildStimulus(stageCount)
    val expected = mutable.Queue.empty[Int]

    SpinalSimConfig().withVerilator.compile(Dut()).doSim(s"mul_pipe_stage_$stageCount") { dut =>
      dut.clockDomain.forkStimulus(2)
      for (in <- stimulus) {
        dut.io.a #= BigInt(in.a.toLong & 0xffffffffL)
        dut.io.b #= BigInt(in.b.toLong & 0xffffffffL)
        dut.io.validIn #= in.valid
        if (in.valid) expected.enqueue(RefFp32.mul(in.a, in.b))
        dut.clockDomain.waitSampling()
        if (dut.io.validOut.toBoolean) {
          assert(expected.nonEmpty, "validOut asserted with empty expected queue")
          val exp = expected.dequeue()
          val outVal = dut.io.result.toBigInt.intValue
          assert(
            outVal == exp,
            f"pipe mismatch expected=0x$exp%08x actual=0x$outVal%08x"
          )
        }
      }

      assert(expected.isEmpty, s"expected queue not drained: ${expected.size} entries")
    }
  }

  private def runAddPipe(stageCount: Int): Unit = {
    case class Dut() extends Component {
      val io = new Bundle {
        val a = in Bits(32 bits)
        val b = in Bits(32 bits)
        val validIn = in Bool()
        val result = out Bits(32 bits)
        val validOut = out Bool()
      }
      val p = Fp32AddPipe(stageCount)
      p.io.a := io.a
      p.io.b := io.b
      p.io.validIn := io.validIn
      io.result := p.io.result
      io.validOut := p.io.validOut
    }

    val stimulus = buildStimulus(stageCount)
    val expected = mutable.Queue.empty[Int]

    SpinalSimConfig().withVerilator.compile(Dut()).doSim(s"add_pipe_stage_$stageCount") { dut =>
      dut.clockDomain.forkStimulus(2)
      for (in <- stimulus) {
        dut.io.a #= BigInt(in.a.toLong & 0xffffffffL)
        dut.io.b #= BigInt(in.b.toLong & 0xffffffffL)
        dut.io.validIn #= in.valid
        if (in.valid) expected.enqueue(RefFp32.add(in.a, in.b))
        dut.clockDomain.waitSampling()
        if (dut.io.validOut.toBoolean) {
          assert(expected.nonEmpty, "validOut asserted with empty expected queue")
          val exp = expected.dequeue()
          val outVal = dut.io.result.toBigInt.intValue
          assert(
            outVal == exp,
            f"pipe mismatch expected=0x$exp%08x actual=0x$outVal%08x"
          )
        }
      }

      assert(expected.isEmpty, s"expected queue not drained: ${expected.size} entries")
    }
  }

  test("Fp32MulPipe matches RefFp32.mul (bit-exact)") {
    for (stages <- Seq(0, 1, 4)) runMulPipe(stages)
  }

  test("Fp32AddPipe matches RefFp32.add (bit-exact)") {
    for (stages <- Seq(0, 1, 4)) runAddPipe(stages)
  }
}
