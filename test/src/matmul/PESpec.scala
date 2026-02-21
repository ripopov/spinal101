package matmul

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class PESpec extends AnyFunSuite {
  private def f2i(v: Float): Int = java.lang.Float.floatToRawIntBits(v)

  private def waitCycles(dut: PE, cycles: Int): Unit = {
    var i = 0
    while (i < cycles) {
      dut.clockDomain.waitSampling()
      i += 1
    }
  }

  private def driveMac(dut: PE, a: Int, b: Int, bankSel: Boolean): Unit = {
    dut.io.a_in #= BigInt(a.toLong & 0xffffffffL)
    dut.io.b_in #= BigInt(b.toLong & 0xffffffffL)
    dut.io.a_valid #= true
    dut.io.b_valid #= true
    dut.io.bank_sel #= bankSel
    dut.io.clear #= false
    dut.clockDomain.waitSampling()
    dut.io.a_valid #= false
    dut.io.b_valid #= false
    dut.io.a_in #= 0
    dut.io.b_in #= 0
  }

  test("PE accumulates sequence on one bank") {
    SpinalSimConfig().withVerilator.compile(PE(lMul = 2, lAdd = 2)).doSim("pe_accumulate") { dut: PE =>
      dut.clockDomain.forkStimulus(2)
      dut.io.a_in #= 0
      dut.io.b_in #= 0
      dut.io.a_valid #= false
      dut.io.b_valid #= false
      dut.io.bank_sel #= false
      dut.io.clear #= false
      dut.io.drain_bank #= false

      val pairs = Seq(
        (f2i(1.0f), f2i(2.0f)),
        (f2i(-3.0f), f2i(4.0f)),
        (f2i(0.5f), f2i(-8.0f))
      )

      var exp = 0
      for ((a, b) <- pairs) {
        exp = RefFp32.add(exp, RefFp32.mul(a, b))
        driveMac(dut, a, b, bankSel = false)
      }

      waitCycles(dut, 8)
      dut.io.drain_bank #= false
      waitCycles(dut, 1)

      val got = dut.io.drain_data.toBigInt.intValue
      assert(got == exp, f"bank0 mismatch expected=0x$exp%08x got=0x$got%08x")
    }
  }

  test("PE bank switching allows independent accumulations") {
    SpinalSimConfig().withVerilator.compile(PE(lMul = 2, lAdd = 2)).doSim("pe_bank_switch") { dut: PE =>
      dut.clockDomain.forkStimulus(2)
      dut.io.a_in #= 0
      dut.io.b_in #= 0
      dut.io.a_valid #= false
      dut.io.b_valid #= false
      dut.io.bank_sel #= false
      dut.io.clear #= false
      dut.io.drain_bank #= false

      val bank0Pairs = Seq((f2i(2.0f), f2i(3.0f)), (f2i(-1.0f), f2i(4.0f)))
      val bank1Pairs = Seq((f2i(10.0f), f2i(0.5f)), (f2i(8.0f), f2i(-0.25f)))

      var exp0 = 0
      for ((a, b) <- bank0Pairs) {
        exp0 = RefFp32.add(exp0, RefFp32.mul(a, b))
        driveMac(dut, a, b, bankSel = false)
      }

      waitCycles(dut, 6)
      dut.io.drain_bank #= false
      waitCycles(dut, 1)
      val got0 = dut.io.drain_data.toBigInt.intValue
      assert(got0 == exp0, f"bank0 mismatch expected=0x$exp0%08x got=0x$got0%08x")

      var exp1 = 0
      for ((a, b) <- bank1Pairs) {
        exp1 = RefFp32.add(exp1, RefFp32.mul(a, b))
        driveMac(dut, a, b, bankSel = true)
      }

      waitCycles(dut, 6)
      dut.io.drain_bank #= false
      waitCycles(dut, 1)
      val got0After = dut.io.drain_data.toBigInt.intValue
      assert(got0After == exp0, f"bank0 changed unexpectedly expected=0x$exp0%08x got=0x$got0After%08x")

      dut.io.drain_bank #= true
      waitCycles(dut, 1)
      val got1 = dut.io.drain_data.toBigInt.intValue
      assert(got1 == exp1, f"bank1 mismatch expected=0x$exp1%08x got=0x$got1%08x")
    }
  }

  test("PE clear zeros active bank") {
    SpinalSimConfig().withVerilator.compile(PE(lMul = 2, lAdd = 2)).doSim("pe_clear") { dut: PE =>
      dut.clockDomain.forkStimulus(2)
      dut.io.a_in #= 0
      dut.io.b_in #= 0
      dut.io.a_valid #= false
      dut.io.b_valid #= false
      dut.io.bank_sel #= false
      dut.io.clear #= false
      dut.io.drain_bank #= false

      driveMac(dut, f2i(2.0f), f2i(2.0f), bankSel = false)
      waitCycles(dut, 6)
      dut.io.drain_bank #= false
      waitCycles(dut, 1)
      val preClear = dut.io.drain_data.toBigInt.intValue
      assert(preClear != 0, "expected non-zero accumulator before clear")

      dut.io.bank_sel #= false
      dut.io.clear #= true
      dut.clockDomain.waitSampling()
      dut.io.clear #= false

      waitCycles(dut, 1)
      dut.io.drain_bank #= false
      waitCycles(dut, 1)
      val postClear = dut.io.drain_data.toBigInt.intValue
      assert(postClear == 0, f"expected cleared bank0=0 got=0x$postClear%08x")
    }
  }
}
