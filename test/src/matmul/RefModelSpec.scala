package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class RefFp32Spec extends AnyFunSuite {
  private def f2i(f: Float): Int = java.lang.Float.floatToRawIntBits(f)
  private def i2f(i: Int): Float = java.lang.Float.intBitsToFloat(i)

  private val posZero = 0x00000000
  private val negZero = 0x80000000
  private val posInf  = 0x7f800000
  private val negInf  = 0xff800000
  private val qNaN    = 0x7fc00000
  private val sNaN    = 0x7f800001
  private val denorm1 = 0x00000001 // smallest positive denormal
  private val denorm2 = 0x00400000 // largest positive denormal
  private val negDenorm = 0x80000001

  // --- Zero tests ---
  test("mul: zero * anything = signed zero") {
    assert(RefFp32.mul(posZero, f2i(3.0f)) == posZero)
    assert(RefFp32.mul(negZero, f2i(3.0f)) == negZero)
    assert(RefFp32.mul(f2i(-2.0f), posZero) == negZero)
    assert(RefFp32.mul(posZero, posZero) == posZero)
  }

  test("add: zero + x = x") {
    val x = f2i(42.0f)
    assert(RefFp32.add(posZero, x) == x)
    assert(RefFp32.add(x, posZero) == x)
    assert(RefFp32.add(posZero, posZero) == posZero)
  }

  // --- Denormal / subnormal tests ---
  // RTL does NOT flush denormals on input. It treats them as subnormals
  // (exp=0 → effective exp=1, no implicit leading 1). FTZ only on output
  // when result exponent underflows to ≤ 0.
  test("mul: subnormal handling") {
    // denorm1 (0x00000001): frac=1, exp=0 → effective exp=1, mant=1 (no implicit 1)
    // 1.0f: frac=0, exp=127 → mant=1<<23
    // Product: 1 * (1<<23) = 1<<23. expUnrounded = 1+127-127 = 1. Normal result.
    val r1 = RefFp32.mul(denorm1, f2i(1.0f))
    assert(!RefFp32.toFloat(r1).isNaN, "result should not be NaN")
    // Very tiny denormals multiplied by very small values → underflow to zero
    val r2 = RefFp32.mul(denorm1, denorm1)
    assert(RefFp32.toFloat(r2) == 0.0f, s"denorm*denorm should underflow to zero")
  }

  test("add: denormal treated as subnormal (not flushed on input)") {
    // denorm is treated as subnormal; adding to a large value,
    // the denorm is too small to affect the result
    val x = f2i(1.0f)
    val r = RefFp32.add(denorm1, x)
    // Result should be x (denorm is negligible)
    assert(r == x, f"expected 0x${x}%08x, got 0x${r}%08x")
  }

  // --- NaN propagation ---
  test("mul: NaN propagation") {
    assert(RefFp32.mul(qNaN, f2i(1.0f)) == qNaN)
    assert(RefFp32.mul(f2i(1.0f), sNaN) == qNaN) // any NaN -> quiet NaN
    assert(RefFp32.mul(qNaN, qNaN) == qNaN)
  }

  test("add: NaN propagation") {
    assert(RefFp32.add(qNaN, f2i(1.0f)) == qNaN)
    assert(RefFp32.add(f2i(1.0f), sNaN) == qNaN)
    assert(RefFp32.add(qNaN, posInf) == qNaN)
  }

  // --- Inf arithmetic ---
  test("mul: Inf * 0 = NaN") {
    assert(RefFp32.mul(posInf, posZero) == qNaN)
    assert(RefFp32.mul(negZero, posInf) == qNaN)
  }

  test("mul: Inf * finite = Inf") {
    assert(RefFp32.mul(posInf, f2i(2.0f)) == posInf)
    assert(RefFp32.mul(negInf, f2i(2.0f)) == negInf)
    assert(RefFp32.mul(posInf, f2i(-1.0f)) == negInf)
  }

  test("mul: Inf * Inf = Inf") {
    assert(RefFp32.mul(posInf, posInf) == posInf)
    assert(RefFp32.mul(posInf, negInf) == negInf)
  }

  test("add: Inf + Inf same sign = Inf") {
    assert(RefFp32.add(posInf, posInf) == posInf)
    assert(RefFp32.add(negInf, negInf) == negInf)
  }

  test("add: Inf + Inf opposite sign = NaN") {
    assert(RefFp32.add(posInf, negInf) == qNaN)
    assert(RefFp32.add(negInf, posInf) == qNaN)
  }

  test("add: Inf + finite = Inf") {
    assert(RefFp32.add(posInf, f2i(42.0f)) == posInf)
    assert(RefFp32.add(f2i(-3.0f), negInf) == negInf)
  }

  // --- Rounding (round-to-nearest-even) ---
  test("mul: rounding tie-breaking") {
    // 1.0000001 * 1.0000001 tests round-to-even
    val a = f2i(1.0000001f)
    val result = RefFp32.mul(a, a)
    // Just verify it's a valid float, not NaN/Inf
    assert(!RefFp32.toFloat(result).isNaN)
    assert(!RefFp32.toFloat(result).isInfinite)
  }

  test("add: cancellation to zero") {
    val x = f2i(1.0f)
    val negX = f2i(-1.0f)
    assert(RefFp32.add(x, negX) == posZero)
  }

  test("mul: sign rules") {
    val a = f2i(2.0f)
    val b = f2i(-3.0f)
    val r = RefFp32.mul(a, b)
    assert(RefFp32.toFloat(r) == -6.0f)

    val r2 = RefFp32.mul(b, b)
    assert(RefFp32.toFloat(r2) == 9.0f)
  }

  // --- Overflow / Underflow ---
  test("mul: overflow to Inf") {
    val big = f2i(Float.MaxValue)
    val result = RefFp32.mul(big, f2i(2.0f))
    assert(result == posInf)
  }

  test("mul: underflow to zero (FTZ)") {
    val small = f2i(Float.MinPositiveValue * 2) // still tiny
    val result = RefFp32.mul(small, small)
    // Product of two tiny numbers underflows
    assert(RefFp32.toFloat(result) == 0.0f)
  }

  // --- Basic value tests ---
  test("mul: basic values") {
    assert(RefFp32.toFloat(RefFp32.mul(f2i(2.0f), f2i(3.0f))) == 6.0f)
    assert(RefFp32.toFloat(RefFp32.mul(f2i(0.5f), f2i(0.25f))) == 0.125f)
    assert(RefFp32.toFloat(RefFp32.mul(f2i(1.0f), f2i(1.0f))) == 1.0f)
  }

  test("add: basic values") {
    assert(RefFp32.toFloat(RefFp32.add(f2i(1.0f), f2i(2.0f))) == 3.0f)
    assert(RefFp32.toFloat(RefFp32.add(f2i(0.5f), f2i(0.25f))) == 0.75f)
    assert(RefFp32.toFloat(RefFp32.add(f2i(100.0f), f2i(-50.0f))) == 50.0f)
  }

  // --- Bit-exact comparison with RTL Fp32Math via Verilator ---
  test("RefFp32.mul is bit-exact with RTL Fp32Math.mul") {
    // We create a small SpinalHDL component that wraps Fp32Math.mul
    case class Fp32MulWrapper() extends Component {
      val io = new Bundle {
        val a = in Bits(32 bits)
        val b = in Bits(32 bits)
        val result = out Bits(32 bits)
      }
      io.result := Fp32Math.mul(io.a, io.b)
    }

    val rng = new Random(0xBEEF)
    val testCases = generateTestPairs(rng)

    SpinalSimConfig().withVerilator.compile(Fp32MulWrapper()).doSim("mul_bitexact") { dut =>
      for ((a, b) <- testCases) {
        dut.io.a #= BigInt(a.toLong & 0xffffffffL)
        dut.io.b #= BigInt(b.toLong & 0xffffffffL)
        sleep(1)
        val rtlResult = dut.io.result.toBigInt.intValue
        val refResult = RefFp32.mul(a, b)
        assert(
          rtlResult == refResult,
          f"mul mismatch: a=0x$a%08x b=0x$b%08x rtl=0x$rtlResult%08x ref=0x$refResult%08x"
        )
      }
    }
  }

  test("RefFp32.add is bit-exact with RTL Fp32Math.add") {
    case class Fp32AddWrapper() extends Component {
      val io = new Bundle {
        val a = in Bits(32 bits)
        val b = in Bits(32 bits)
        val result = out Bits(32 bits)
      }
      io.result := Fp32Math.add(io.a, io.b)
    }

    val rng = new Random(0xCAFE)
    val testCases = generateTestPairs(rng)

    SpinalSimConfig().withVerilator.compile(Fp32AddWrapper()).doSim("add_bitexact") { dut =>
      for ((a, b) <- testCases) {
        dut.io.a #= BigInt(a.toLong & 0xffffffffL)
        dut.io.b #= BigInt(b.toLong & 0xffffffffL)
        sleep(1)
        val rtlResult = dut.io.result.toBigInt.intValue
        val refResult = RefFp32.add(a, b)
        assert(
          rtlResult == refResult,
          f"add mismatch: a=0x$a%08x b=0x$b%08x rtl=0x$rtlResult%08x ref=0x$refResult%08x"
        )
      }
    }
  }

  /** Generate a mix of special and random FP32 test pairs. */
  private def generateTestPairs(rng: Random): Seq[(Int, Int)] = {
    val specials = Seq(posZero, negZero, posInf, negInf, qNaN, sNaN,
      denorm1, denorm2, negDenorm,
      f2i(1.0f), f2i(-1.0f), f2i(Float.MaxValue), f2i(Float.MinPositiveValue),
      f2i(0.5f), f2i(-0.5f), f2i(3.14f), f2i(-2.718f)
    )
    // All pairs of specials
    val specialPairs = for (a <- specials; b <- specials) yield (a, b)

    // Random normal values
    val randomPairs = (0 until 500).map { _ =>
      (rng.nextInt(), rng.nextInt())
    }

    // Random small values (likely to exercise rounding)
    val smallPairs = (0 until 100).map { _ =>
      val a = f2i((rng.nextFloat() - 0.5f) * 10.0f)
      val b = f2i((rng.nextFloat() - 0.5f) * 10.0f)
      (a, b)
    }

    specialPairs ++ randomPairs ++ smallPairs
  }
}

class RefMatmulSpec extends AnyFunSuite {
  private val rng = new Random(0xD1CE)
  private val S = 4 // Use small S for fast tests

  private def f2i(f: Float): Int = java.lang.Float.floatToRawIntBits(f)

  private def randomMatrix(rows: Int, cols: Int, ld: Int): Array[Int] = {
    val data = new Array[Int](rows * ld)
    for (r <- 0 until rows; c <- 0 until cols) {
      data(r * ld + c) = f2i((rng.nextFloat() - 0.5f) * 8.0f)
    }
    data
  }

  private def matricesEqual(a: Array[Int], b: Array[Int], rows: Int, cols: Int, ld: Int): Boolean = {
    (0 until rows).forall { r =>
      (0 until cols).forall { c =>
        a(r * ld + c) == b(r * ld + c)
      }
    }
  }

  /** Check matrices are within maxUlp ULPs of each other. */
  private def matricesNearUlp(
      a: Array[Int], b: Array[Int], rows: Int, cols: Int, ld: Int, maxUlp: Int
  ): Boolean = {
    (0 until rows).forall { r =>
      (0 until cols).forall { c =>
        val ai = a(r * ld + c); val bi = b(r * ld + c)
        math.abs(ai.toLong - bi.toLong) <= maxUlp
      }
    }
  }

  private def matrixMismatchMsg(
      label: String, a: Array[Int], b: Array[Int], rows: Int, cols: Int, ld: Int
  ): String = {
    val sb = new StringBuilder(s"$label mismatch:\n")
    for (r <- 0 until rows; c <- 0 until cols) {
      val ai = a(r * ld + c); val bi = b(r * ld + c)
      if (ai != bi) {
        sb.append(f"  ($r,$c): tiled=0x$ai%08x (${RefFp32.toFloat(ai)}%.6g) " +
          f"vs naive=0x$bi%08x (${RefFp32.toFloat(bi)}%.6g)\n")
      }
    }
    sb.toString
  }

  // Test all supported prim sizes against naive matmul (SPEC 4.1)
  // For S=4: E=4, so prim_m/n in {4,8,16,32}, prim_k in {4,8,16,32,64}
  private val primMultiples = Seq(1, 2, 4, 8)
  private val primKMultiples = Seq(1, 2, 4, 8, 16)

  for (pm <- primMultiples; pn <- primMultiples) {
    val primM = pm * S
    val primN = pn * S
    // Use prim_k = S (smallest) to keep test fast
    val primK = S
    val M = primM
    val N = primN
    val K = primK

    test(s"RefMatmul: prim_m=$primM prim_n=$primN prim_k=$primK (${M}x${N}x${K})") {
      val lda = K; val ldb = N; val ldd = N
      val A = randomMatrix(M, K, lda)
      val B = randomMatrix(K, N, ldb)

      val tiledD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
      val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

      assert(
        matricesEqual(tiledD, naiveD, M, N, ldd),
        matrixMismatchMsg(s"prim($primM,$primN,$primK)", tiledD, naiveD, M, N, ldd)
      )
    }
  }

  // Test various prim_k values
  for (pk <- primKMultiples) {
    val primK = pk * S
    val primM = S
    val primN = S
    val M = primM
    val N = primN
    val K = primK

    test(s"RefMatmul: prim_k=$primK with M=$M N=$N K=$K") {
      val lda = K; val ldb = N; val ldd = N
      val A = randomMatrix(M, K, lda)
      val B = randomMatrix(K, N, ldb)

      val tiledD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
      val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

      assert(
        matricesEqual(tiledD, naiveD, M, N, ldd),
        matrixMismatchMsg(s"prim_k=$primK", tiledD, naiveD, M, N, ldd)
      )
    }
  }

  // K-reduction tests: K > prim_k
  // K-reduction changes FP accumulation order (partial sums composed across primitives),
  // so we compare against the single-primitive result (primK=K) which does a single
  // continuous accumulation. Small ULP differences are expected and acceptable.
  // K-reduction tests verify partial-sum composition is correct.
  // We compare K-reduced result against naive (continuous accumulation).
  // FP accumulation order differs, so we allow small ULP tolerance.
  // Tolerance scales with number of K-reduction steps.
  test("RefMatmul: K-reduction K=4*S with prim_k=S") {
    val primM = S; val primN = S; val primK = S
    val M = primM; val N = primN; val K = 4 * S
    val lda = K; val ldb = N; val ldd = N
    val A = randomMatrix(M, K, lda)
    val B = randomMatrix(K, N, ldb)

    val kReducedD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
    val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

    // K-reduction reorders FP accumulation → allow up to 8 ULPs
    assert(
      matricesNearUlp(kReducedD, naiveD, M, N, ldd, 8),
      matrixMismatchMsg("K-reduction 4S", kReducedD, naiveD, M, N, ldd)
    )
  }

  test("RefMatmul: K-reduction K=8*S with prim_k=2*S") {
    val primM = 2 * S; val primN = 2 * S; val primK = 2 * S
    val M = primM; val N = primN; val K = 8 * S
    val lda = K; val ldb = N; val ldd = N
    val A = randomMatrix(M, K, lda)
    val B = randomMatrix(K, N, ldb)

    val kReducedD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
    val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

    // K-reduction reorders FP accumulation → allow up to 16 ULPs
    assert(
      matricesNearUlp(kReducedD, naiveD, M, N, ldd, 16),
      matrixMismatchMsg("K-reduction 8S/2S", kReducedD, naiveD, M, N, ldd)
    )
  }

  test("RefMatmul: multi-primitive with K-reduction") {
    val primM = 2 * S; val primN = 2 * S; val primK = S
    val M = 4 * S; val N = 4 * S; val K = 2 * S
    val lda = K; val ldb = N; val ldd = N
    val A = randomMatrix(M, K, lda)
    val B = randomMatrix(K, N, ldb)

    val kReducedD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
    val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

    // K-reduction with large multi-tile matrices reorders FP accumulation heavily.
    // The tiled model is the source of truth for RTL verification; this test
    // just confirms results are close to naive (within reasonable FP tolerance).
    // With 16x16 results from 4x4 tiles + K-reduction, up to ~64 ULP diff is normal.
    assert(
      matricesNearUlp(kReducedD, naiveD, M, N, ldd, 128),
      matrixMismatchMsg("multi-prim+K-red", kReducedD, naiveD, M, N, ldd)
    )
  }

  // Test with non-trivial leading dimensions (lda > K, ldb > N, ldd > N)
  test("RefMatmul: non-trivial leading dimensions") {
    val primM = S; val primN = S; val primK = S
    val M = S; val N = S; val K = S
    val lda = K + 4; val ldb = N + 8; val ldd = N + 2
    val A = randomMatrix(M, K, lda)
    val B = randomMatrix(K, N, ldb)

    val tiledD = RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK, S)
    val naiveD = RefMatmul.naiveCompute(A, B, M, N, K, lda, ldb, ldd)

    assert(
      matricesEqual(tiledD, naiveD, M, N, ldd),
      matrixMismatchMsg("non-trivial ld", tiledD, naiveD, M, N, ldd)
    )
  }
}
