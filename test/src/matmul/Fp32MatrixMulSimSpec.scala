package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.util.Random

class Fp32MatrixMulSimSpec extends AnyFunSuite {
  private val size = 2
  private val random = new Random(0xF32)

  private def floatToBits(value: Float): BigInt = {
    BigInt(java.lang.Integer.toUnsignedLong(java.lang.Float.floatToRawIntBits(value)))
  }

  private def bitsToFloat(value: BigInt): Float = {
    java.lang.Float.intBitsToFloat(value.intValue)
  }

  private def referenceMatMul(a: Vector[Vector[Float]], b: Vector[Vector[Float]]): Vector[Vector[Float]] = {
    Vector.tabulate(size, size) { (row, col) =>
      var acc = 0.0f
      var k = 0
      while (k < size) {
        acc = acc + (a(row)(k) * b(k)(col))
        k += 1
      }
      acc
    }
  }

  private def near(actual: Float, expected: Float): Boolean = {
    val absErr = math.abs(actual.toDouble - expected.toDouble)
    val relErr = absErr / math.max(1.0, math.abs(expected.toDouble))
    absErr <= 5e-3 || relErr <= 5e-3
  }

  private def randomValue(): Float = {
    (random.nextInt(33) - 16) / 4.0f
  }

  private def randomMatrix(): Vector[Vector[Float]] = {
    Vector.tabulate(size, size) { (_, _) => randomValue() }
  }

  private def runCase(
      dut: Fp32MatrixMul,
      a: Vector[Vector[Float]],
      b: Vector[Vector[Float]],
      label: String
  ): Unit = {
    for (row <- 0 until size; col <- 0 until size) {
      dut.io.a(row)(col) #= floatToBits(a(row)(col))
      dut.io.b(row)(col) #= floatToBits(b(row)(col))
    }

    dut.io.start #= true
    dut.clockDomain.waitSampling()
    dut.io.start #= false

    var cycleCount = 0
    while (!dut.io.done.toBoolean && cycleCount < 200) {
      dut.clockDomain.waitSampling()
      cycleCount += 1
    }

    assert(cycleCount < 200, s"$label: timeout waiting for done")

    val expected = referenceMatMul(a, b)
    for (row <- 0 until size; col <- 0 until size) {
      val actual = bitsToFloat(dut.io.c(row)(col).toBigInt)
      val golden = expected(row)(col)
      assert(
        near(actual, golden),
        s"$label mismatch @ ($row,$col): actual=$actual expected=$golden"
      )
    }

    dut.clockDomain.waitSampling()
  }

  private def runBackend(name: String, config: SpinalSimConfig): Unit = {
    config.compile(new Fp32MatrixMul(size)).doSim(name) { dut: Fp32MatrixMul =>
      dut.clockDomain.forkStimulus(2)
      dut.io.start #= false

      for (row <- 0 until size; col <- 0 until size) {
        dut.io.a(row)(col) #= 0
        dut.io.b(row)(col) #= 0
      }

      dut.clockDomain.waitSampling(3)

      val directed = Seq(
        (
          Vector(Vector(1.0f, 2.0f), Vector(3.0f, 4.0f)),
          Vector(Vector(5.0f, 6.0f), Vector(7.0f, 8.0f))
        ),
        (
          Vector(Vector(-1.5f, 0.5f), Vector(2.0f, -3.0f)),
          Vector(Vector(4.0f, -2.0f), Vector(1.0f, 0.25f))
        )
      )

      for (((a, b), idx) <- directed.zipWithIndex) {
        runCase(dut, a, b, s"$name directed-$idx")
      }

      for (idx <- 0 until 40) {
        runCase(dut, randomMatrix(), randomMatrix(), s"$name random-$idx")
      }
    }
  }

  test("FP32 matrix multiply via Verilator") {
    runBackend("verilator", SpinalSimConfig().withVerilator)
  }
}
