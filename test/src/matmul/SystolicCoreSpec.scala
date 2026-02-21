package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class SystolicCoreSpec extends AnyFunSuite {
  private def f2i(v: Float): Int = java.lang.Float.floatToRawIntBits(v)

  private def mkMatrix(s: Int, base: Float): Array[Int] = {
    Array.tabulate(s * s) { idx =>
      val r = idx / s
      val c = idx % s
      f2i(base + r * 0.25f + c * 0.5f)
    }
  }

  private def feedMicroTile(dut: SystolicCore, s: Int, a: Array[Int], b: Array[Int], bankSel: Boolean): Unit = {
    dut.io.bankSel #= bankSel
    dut.io.clearBank #= true
    for (i <- 0 until s) {
      dut.io.aIn(i) #= 0
      dut.io.bIn(i) #= 0
      dut.io.aValid(i) #= false
      dut.io.bValid(i) #= false
    }
    dut.clockDomain.waitSampling()
    dut.io.clearBank #= false

    // Inject S cycles: A(:,k) from west and B(k,:) from north.
    for (k <- 0 until s) {
      for (r <- 0 until s) {
        dut.io.aIn(r) #= BigInt(a(r * s + k).toLong & 0xffffffffL)
        dut.io.aValid(r) #= true
      }
      for (c <- 0 until s) {
        dut.io.bIn(c) #= BigInt(b(k * s + c).toLong & 0xffffffffL)
        dut.io.bValid(c) #= true
      }
      dut.clockDomain.waitSampling()
    }

    for (i <- 0 until s) {
      dut.io.aIn(i) #= 0
      dut.io.bIn(i) #= 0
      dut.io.aValid(i) #= false
      dut.io.bValid(i) #= false
    }
  }

  private def drainMatrix(dut: SystolicCore, s: Int, bank: Boolean): Array[Int] = {
    val out = Array.fill(s * s)(0)
    dut.io.drainBank #= bank
    for (r <- 0 until s) {
      dut.io.drainRow #= r
      dut.clockDomain.waitSampling()
      for (c <- 0 until s) {
        out(r * s + c) = dut.io.drainData(c).toBigInt.intValue
      }
    }
    out
  }

  private def runSingleTileCase(s: Int): Unit = {
    val cfgName = s"systolic_core_single_tile_s${s}"
    SpinalSimConfig().withVerilator.compile(SystolicCore(s = s, lMul = 0, lAdd = 0)).doSim(cfgName) { dut: SystolicCore =>
      dut.clockDomain.forkStimulus(2)
      dut.io.bankSel #= false
      dut.io.clearBank #= false
      dut.io.drainBank #= false
      dut.io.drainRow #= 0
      for (i <- 0 until s) {
        dut.io.aIn(i) #= 0
        dut.io.bIn(i) #= 0
        dut.io.aValid(i) #= false
        dut.io.bValid(i) #= false
      }

      val a = mkMatrix(s, 1.0f)
      val b = mkMatrix(s, -0.5f)

      feedMicroTile(dut, s, a, b, bankSel = false)

      val tail = 2 * (s - 1) + 2
      dut.clockDomain.waitSampling(tail)

      val got = drainMatrix(dut, s, bank = false)
      val expected = RefMatmul.compute(
        A = a,
        B = b,
        M = s,
        N = s,
        K = s,
        lda = s,
        ldb = s,
        ldd = s,
        primM = s,
        primN = s,
        primK = s,
        S = s
      )

      for (idx <- got.indices) {
        assert(
          got(idx) == expected(idx),
          f"single-tile mismatch s=$s idx=$idx expected=0x${expected(idx)}%08x got=0x${got(idx)}%08x"
        )
      }
    }
  }

  private def runBankOverlapCase(s: Int): Unit = {
    val cfgName = s"systolic_core_bank_overlap_s${s}"
    SpinalSimConfig().withVerilator.compile(SystolicCore(s = s, lMul = 0, lAdd = 0)).doSim(cfgName) { dut: SystolicCore =>
      dut.clockDomain.forkStimulus(2)
      dut.io.bankSel #= false
      dut.io.clearBank #= false
      dut.io.drainBank #= false
      dut.io.drainRow #= 0
      for (i <- 0 until s) {
        dut.io.aIn(i) #= 0
        dut.io.bIn(i) #= 0
        dut.io.aValid(i) #= false
        dut.io.bValid(i) #= false
      }

      val a0 = mkMatrix(s, 0.75f)
      val b0 = mkMatrix(s, -1.25f)
      val a1 = mkMatrix(s, 2.5f)
      val b1 = mkMatrix(s, 0.125f)

      val exp0 = RefMatmul.compute(a0, b0, s, s, s, s, s, s, s, s, s, s)
      val exp1 = RefMatmul.compute(a1, b1, s, s, s, s, s, s, s, s, s, s)

      feedMicroTile(dut, s, a0, b0, bankSel = false)
      dut.clockDomain.waitSampling(2 * (s - 1) + 2)

      // Start bank1 while bank0 result remains intact.
      feedMicroTile(dut, s, a1, b1, bankSel = true)
      dut.clockDomain.waitSampling(2 * (s - 1) + 2)

      val got0 = drainMatrix(dut, s, bank = false)
      val got1 = drainMatrix(dut, s, bank = true)

      for (idx <- got0.indices) {
        assert(
          got0(idx) == exp0(idx),
          f"bank0 mismatch s=$s idx=$idx expected=0x${exp0(idx)}%08x got=0x${got0(idx)}%08x"
        )
      }

      for (idx <- got1.indices) {
        assert(
          got1(idx) == exp1(idx),
          f"bank1 mismatch s=$s idx=$idx expected=0x${exp1(idx)}%08x got=0x${got1(idx)}%08x"
        )
      }
    }
  }

  test("SystolicCore computes one SxS micro-tile for S=4") {
    runSingleTileCase(4)
  }

  test("SystolicCore computes one SxS micro-tile for S=16") {
    runSingleTileCase(16)
  }

  test("SystolicCore bank overlap works for S=4") {
    runBankOverlapCase(4)
  }

  test("SystolicCore bank overlap works for S=16") {
    runBankOverlapCase(16)
  }
}
