package matmul

import spinal.core._
import spinal.lib._

object Fp32Math {
  private def quietNaN = B(0x7fc00000L, 32 bits)

  private case class Parts(sign: Bool, exp: UInt, frac: UInt)

  private def unpack(value: Bits): Parts = {
    val exp = UInt(8 bits)
    val frac = UInt(23 bits)
    exp := value(30 downto 23).asUInt
    frac := value(22 downto 0).asUInt
    Parts(value(31), exp, frac)
  }

  private def pack(sign: Bool, exp: UInt, frac: UInt): Bits = {
    sign.asBits ## exp.asBits ## frac.asBits
  }

  private def shiftRightJam(value: UInt, shift: UInt): UInt = {
    val shifted = UInt(value.getWidth bits)
    shifted := value >> shift

    val sticky = Bool()
    sticky := False
    for (bit <- 0 until value.getWidth) {
      when((shift > U(bit, shift.getWidth bits)) && value(bit)) {
        sticky := True
      }
    }

    val result = UInt(value.getWidth bits)
    result := shifted
    when(sticky) {
      result(0) := True
    }
    result
  }

  private def leadingZeroCount(value: UInt): UInt = {
    val width = value.getWidth
    val highest = UInt(log2Up(width) bits)
    highest := 0
    for (bit <- 0 until width) {
      when(value(bit)) {
        highest := U(bit, highest.getWidth bits)
      }
    }

    val result = UInt(log2Up(width + 1) bits)
    result := U(width, result.getWidth bits)
    when(value =/= 0) {
      result := U(width - 1, result.getWidth bits) - highest.resized
    }
    result
  }

  def mul(aBits: Bits, bBits: Bits): Bits = {
    val a = unpack(aBits)
    val b = unpack(bBits)

    val sign = a.sign ^ b.sign
    val aIsZero = (a.exp === 0) && (a.frac === 0)
    val bIsZero = (b.exp === 0) && (b.frac === 0)
    val aIsInf = (a.exp === 255) && (a.frac === 0)
    val bIsInf = (b.exp === 255) && (b.frac === 0)
    val aIsNaN = (a.exp === 255) && (a.frac =/= 0)
    val bIsNaN = (b.exp === 255) && (b.frac =/= 0)

    val out = Bits(32 bits)
    out := B(0, 32 bits)

    when(aIsNaN || bIsNaN || ((aIsInf || bIsInf) && (aIsZero || bIsZero))) {
      out := quietNaN
    } elsewhen(aIsInf || bIsInf) {
      out := pack(sign, U(255, 8 bits), U(0, 23 bits))
    } elsewhen(aIsZero || bIsZero) {
      out := sign.asBits ## B(0, 31 bits)
    } otherwise {
      val aExp = UInt(9 bits)
      val bExp = UInt(9 bits)
      aExp := Mux(a.exp === 0, U(1, 8 bits), a.exp).resized
      bExp := Mux(b.exp === 0, U(1, 8 bits), b.exp).resized

      val aMant = UInt(24 bits)
      val bMant = UInt(24 bits)
      aMant := ((a.exp =/= 0).asBits ## a.frac.asBits).asUInt
      bMant := ((b.exp =/= 0).asBits ## b.frac.asBits).asUInt

      val product = UInt(48 bits)
      product := aMant * bMant

      val productHigh = product(47)

      val mantBase = UInt(24 bits)
      val guard = Bool()
      val round = Bool()
      val sticky = Bool()

      mantBase := product(46 downto 23)
      guard := product(22)
      round := product(21)
      sticky := product(20 downto 0).orR

      when(productHigh) {
        mantBase := product(47 downto 24)
        guard := product(23)
        round := product(22)
        sticky := product(21 downto 0).orR
      }

      val roundIncrement = guard && (round || sticky || mantBase(0))
      val mantRounded = UInt(25 bits)
      mantRounded := mantBase.resize(25) + roundIncrement.asUInt

      val mantFinal = UInt(24 bits)
      val expUnrounded = SInt(12 bits)
      val expRounded = SInt(12 bits)

      expUnrounded := (
        aExp.resize(12).asSInt +
          bExp.resize(12).asSInt -
          S(127, 12 bits) +
          productHigh.asUInt.resize(12).asSInt
      )

      mantFinal := mantRounded(23 downto 0)
      expRounded := expUnrounded

      when(mantRounded(24)) {
        mantFinal := mantRounded(24 downto 1)
        expRounded := expUnrounded + 1
      }

      when(expRounded >= S(255, 12 bits)) {
        out := pack(sign, U(255, 8 bits), U(0, 23 bits))
      } elsewhen(expRounded <= 0) {
        out := sign.asBits ## B(0, 31 bits)
      } otherwise {
        out := pack(sign, expRounded.asUInt(7 downto 0), mantFinal(22 downto 0))
      }
    }

    out
  }

  def add(aBits: Bits, bBits: Bits): Bits = {
    val a = unpack(aBits)
    val b = unpack(bBits)

    val aIsZero = (a.exp === 0) && (a.frac === 0)
    val bIsZero = (b.exp === 0) && (b.frac === 0)
    val aIsInf = (a.exp === 255) && (a.frac === 0)
    val bIsInf = (b.exp === 255) && (b.frac === 0)
    val aIsNaN = (a.exp === 255) && (a.frac =/= 0)
    val bIsNaN = (b.exp === 255) && (b.frac =/= 0)

    val out = Bits(32 bits)
    out := B(0, 32 bits)

    when(aIsNaN || bIsNaN || (aIsInf && bIsInf && (a.sign =/= b.sign))) {
      out := quietNaN
    } elsewhen(aIsInf) {
      out := pack(a.sign, U(255, 8 bits), U(0, 23 bits))
    } elsewhen(bIsInf) {
      out := pack(b.sign, U(255, 8 bits), U(0, 23 bits))
    } elsewhen(aIsZero && bIsZero) {
      out := B(0, 32 bits)
    } elsewhen(aIsZero) {
      out := bBits
    } elsewhen(bIsZero) {
      out := aBits
    } otherwise {
      val aExp = UInt(8 bits)
      val bExp = UInt(8 bits)
      aExp := Mux(a.exp === 0, U(1, 8 bits), a.exp)
      bExp := Mux(b.exp === 0, U(1, 8 bits), b.exp)

      val aSig24 = UInt(24 bits)
      val bSig24 = UInt(24 bits)
      aSig24 := ((a.exp =/= 0).asBits ## a.frac.asBits).asUInt
      bSig24 := ((b.exp =/= 0).asBits ## b.frac.asBits).asUInt

      val aSig = UInt(27 bits)
      val bSig = UInt(27 bits)
      aSig := (aSig24 ## B(0, 3 bits)).asUInt
      bSig := (bSig24 ## B(0, 3 bits)).asUInt

      val aGreaterMagnitude = Bool()
      aGreaterMagnitude := (aExp > bExp) || ((aExp === bExp) && (aSig24 >= bSig24))

      val expBig = UInt(8 bits)
      val expSmall = UInt(8 bits)
      val sigBig = UInt(27 bits)
      val sigSmall = UInt(27 bits)
      val signBig = Bool()
      val signSmall = Bool()

      expBig := Mux(aGreaterMagnitude, aExp, bExp)
      expSmall := Mux(aGreaterMagnitude, bExp, aExp)
      sigBig := Mux(aGreaterMagnitude, aSig, bSig)
      sigSmall := Mux(aGreaterMagnitude, bSig, aSig)
      signBig := Mux(aGreaterMagnitude, a.sign, b.sign)
      signSmall := Mux(aGreaterMagnitude, b.sign, a.sign)

      val shiftAmount = UInt(8 bits)
      shiftAmount := expBig - expSmall

      val sigSmallAligned = shiftRightJam(sigSmall, shiftAmount)
      val sameSign = signBig === signSmall

      val sigNorm = UInt(27 bits)
      val expNorm = UInt(9 bits)
      val signNorm = Bool()

      sigNorm := 0
      expNorm := expBig.resize(9)
      signNorm := signBig

      when(sameSign) {
        val sum = UInt(28 bits)
        sum := sigBig.resize(28) + sigSmallAligned.resize(28)

        when(sum(27)) {
          val shifted = shiftRightJam(sum, U(1, 8 bits))
          sigNorm := shifted(26 downto 0)
          expNorm := expBig.resize(9) + 1
        } otherwise {
          sigNorm := sum(26 downto 0)
          expNorm := expBig.resize(9)
        }
      } otherwise {
        val diff = UInt(27 bits)
        diff := sigBig - sigSmallAligned

        when(diff === 0) {
          sigNorm := 0
          expNorm := 0
          signNorm := False
        } otherwise {
          val shiftNeeded = leadingZeroCount(diff).resized
          val shiftMax = UInt(8 bits)
          val shiftApplied = UInt(8 bits)

          shiftMax := expBig - 1
          shiftApplied := Mux(shiftNeeded > shiftMax, shiftMax, shiftNeeded)

          sigNorm := (diff |<< shiftApplied).resized
          expNorm := expBig.resize(9) - shiftApplied.resize(9)
        }
      }

      val mantBase = sigNorm(26 downto 3)
      val guard = sigNorm(2)
      val round = sigNorm(1)
      val sticky = sigNorm(0)
      val roundIncrement = guard && (round || sticky || mantBase(0))

      val mantRounded = UInt(25 bits)
      mantRounded := mantBase.resize(25) + roundIncrement.asUInt

      val mantFinal = UInt(24 bits)
      val expRounded = UInt(10 bits)

      mantFinal := mantRounded(23 downto 0)
      expRounded := expNorm.resize(10)

      when(mantRounded(24)) {
        mantFinal := mantRounded(24 downto 1)
        expRounded := expNorm.resize(10) + 1
      }

      when(sigNorm === 0) {
        out := B(0, 32 bits)
      } elsewhen(expRounded >= U(255, 10 bits)) {
        out := pack(signNorm, U(255, 8 bits), U(0, 23 bits))
      } elsewhen(expRounded === 0) {
        out := B(0, 32 bits)
      } otherwise {
        out := pack(signNorm, expRounded(7 downto 0), mantFinal(22 downto 0))
      }
    }

    out
  }
}
