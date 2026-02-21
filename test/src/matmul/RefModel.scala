package matmul

/**
 * Pure-Scala reference model for FP32 arithmetic and tiled matrix multiply.
 * Bit-exact with the RTL Fp32Math implementation.
 */
object RefFp32 {
  private val QUIET_NAN: Int = 0x7fc00000

  private def sign(bits: Int): Int = (bits >>> 31) & 1
  private def exp(bits: Int): Int = (bits >>> 23) & 0xff
  private def frac(bits: Int): Int = bits & 0x7fffff

  private def pack(s: Int, e: Int, f: Int): Int =
    ((s & 1) << 31) | ((e & 0xff) << 23) | (f & 0x7fffff)

  private def isZero(bits: Int): Boolean = exp(bits) == 0 && frac(bits) == 0
  private def isDenorm(bits: Int): Boolean = exp(bits) == 0 && frac(bits) != 0
  private def isInf(bits: Int): Boolean = exp(bits) == 255 && frac(bits) == 0
  private def isNaN(bits: Int): Boolean = exp(bits) == 255 && frac(bits) != 0

  /**
   * Shift right with sticky bit (matches RTL shiftRightJam).
   * value is unsigned (use Long to avoid sign issues), width bits wide.
   * Returns shifted value with sticky OR'd into bit 0.
   */
  private def shiftRightJam(value: Long, shift: Int, width: Int): Long = {
    val mask = (1L << width) - 1
    val v = value & mask
    if (shift <= 0) return v
    if (shift >= width) return if (v != 0) 1L else 0L
    val shifted = v >>> shift
    val stickyMask = (1L << shift) - 1
    val sticky = if ((v & stickyMask) != 0) 1L else 0L
    shifted | sticky
  }

  /**
   * Count leading zeros of an unsigned value of given width.
   * Matches RTL leadingZeroCount behavior.
   */
  private def leadingZeroCount(value: Long, width: Int): Int = {
    if (value == 0) return width
    var highest = 0
    var bit = 0
    while (bit < width) {
      if (((value >> bit) & 1) != 0) highest = bit
      bit += 1
    }
    (width - 1) - highest
  }

  /** FP32 multiply, bit-exact with Fp32Math.mul. No input FTZ — matches RTL. */
  def mul(aRaw: Int, bRaw: Int): Int = {
    val aBits = aRaw
    val bBits = bRaw

    val s = sign(aBits) ^ sign(bBits)
    // RTL isZero checks exp==0 && frac==0 (denormals are NOT zero)
    val aZ = isZero(aBits); val bZ = isZero(bBits)
    val aI = isInf(aBits);  val bI = isInf(bBits)
    val aN = isNaN(aBits);  val bN = isNaN(bBits)

    if (aN || bN || ((aI || bI) && (aZ || bZ))) return QUIET_NAN
    if (aI || bI) return pack(s, 255, 0)
    if (aZ || bZ) return pack(s, 0, 0)

    val aExp = if (exp(aBits) == 0) 1 else exp(aBits)
    val bExp = if (exp(bBits) == 0) 1 else exp(bBits)
    val aMant: Long = if (exp(aBits) != 0) (1L << 23) | frac(aBits) else frac(aBits).toLong
    val bMant: Long = if (exp(bBits) != 0) (1L << 23) | frac(bBits) else frac(bBits).toLong

    val product: Long = aMant * bMant  // up to 48 bits

    val productHigh = ((product >> 47) & 1) != 0

    val (mantBase, guard, round, sticky) = if (productHigh) {
      (
        ((product >> 24) & 0xffffffL).toInt,
        ((product >> 23) & 1).toInt,
        ((product >> 22) & 1).toInt,
        if ((product & 0x3fffffL) != 0) 1 else 0
      )
    } else {
      (
        ((product >> 23) & 0xffffffL).toInt,
        ((product >> 22) & 1).toInt,
        ((product >> 21) & 1).toInt,
        if ((product & 0x1fffffL) != 0) 1 else 0
      )
    }

    val roundIncrement = if (guard != 0 && (round != 0 || sticky != 0 || (mantBase & 1) != 0)) 1 else 0
    val mantRounded = mantBase + roundIncrement

    var mantFinal = mantRounded & 0xffffff
    var expUnrounded = aExp + bExp - 127 + (if (productHigh) 1 else 0)
    var expRounded = expUnrounded

    if ((mantRounded & (1 << 24)) != 0) {
      mantFinal = (mantRounded >> 1) & 0xffffff
      expRounded = expUnrounded + 1
    }

    if (expRounded >= 255) {
      pack(s, 255, 0) // overflow -> Inf
    } else if (expRounded <= 0) {
      pack(s, 0, 0) // underflow -> zero (FTZ)
    } else {
      pack(s, expRounded & 0xff, mantFinal & 0x7fffff)
    }
  }

  /** FP32 add, bit-exact with Fp32Math.add. No input FTZ — matches RTL. */
  def add(aRaw: Int, bRaw: Int): Int = {
    val aBits = aRaw
    val bBits = bRaw

    val aZ = isZero(aBits); val bZ = isZero(bBits)
    val aI = isInf(aBits);  val bI = isInf(bBits)
    val aN = isNaN(aBits);  val bN = isNaN(bBits)

    if (aN || bN || (aI && bI && sign(aBits) != sign(bBits))) return QUIET_NAN
    if (aI) return pack(sign(aBits), 255, 0)
    if (bI) return pack(sign(bBits), 255, 0)
    if (aZ && bZ) return 0 // +0
    if (aZ) return bBits
    if (bZ) return aBits

    val aExp = if (exp(aBits) == 0) 1 else exp(aBits)
    val bExp = if (exp(bBits) == 0) 1 else exp(bBits)
    val aSig24: Long = if (exp(aBits) != 0) (1L << 23) | frac(aBits) else frac(aBits).toLong
    val bSig24: Long = if (exp(bBits) != 0) (1L << 23) | frac(bBits) else frac(bBits).toLong

    val aSig: Long = aSig24 << 3 // 27 bits
    val bSig: Long = bSig24 << 3

    val aGreater = (aExp > bExp) || (aExp == bExp && aSig24 >= bSig24)

    val (expBig, sigBig, signBig, expSmall, sigSmall, signSmall) = if (aGreater) {
      (aExp, aSig, sign(aBits), bExp, bSig, sign(bBits))
    } else {
      (bExp, bSig, sign(bBits), aExp, aSig, sign(aBits))
    }

    val shiftAmount = expBig - expSmall
    val sigSmallAligned = shiftRightJam(sigSmall, shiftAmount, 27)
    val sameSign = signBig == signSmall

    var sigNorm: Long = 0
    var expNorm: Int = expBig
    var signNorm: Int = signBig

    if (sameSign) {
      val sum = sigBig + sigSmallAligned
      if (((sum >> 27) & 1) != 0) {
        sigNorm = shiftRightJam(sum, 1, 28) & 0x7ffffffL
        expNorm = expBig + 1
      } else {
        sigNorm = sum & 0x7ffffffL
        expNorm = expBig
      }
    } else {
      val diff = sigBig - sigSmallAligned
      if (diff == 0) {
        sigNorm = 0
        expNorm = 0
        signNorm = 0
      } else {
        val shiftNeeded = leadingZeroCount(diff, 27)
        val shiftMax = expBig - 1
        val shiftApplied = math.min(shiftNeeded, shiftMax)
        sigNorm = (diff << shiftApplied) & 0x7ffffffL
        expNorm = expBig - shiftApplied
      }
    }

    if (sigNorm == 0) return 0

    val mantBase = ((sigNorm >> 3) & 0xffffffL).toInt
    val guardBit = ((sigNorm >> 2) & 1).toInt
    val roundBit = ((sigNorm >> 1) & 1).toInt
    val stickyBit = (sigNorm & 1).toInt
    val roundInc = if (guardBit != 0 && (roundBit != 0 || stickyBit != 0 || (mantBase & 1) != 0)) 1 else 0

    val mantRounded = mantBase + roundInc
    var mantFinal = mantRounded & 0xffffff
    var expRounded = expNorm

    if ((mantRounded & (1 << 24)) != 0) {
      mantFinal = (mantRounded >> 1) & 0xffffff
      expRounded = expNorm + 1
    }

    if (expRounded >= 255) {
      pack(signNorm, 255, 0)
    } else if (expRounded <= 0) {
      0 // FTZ
    } else {
      pack(signNorm, expRounded & 0xff, mantFinal & 0x7fffff)
    }
  }

  /** Convert Int bits to Float for display/comparison. */
  def toFloat(bits: Int): Float = java.lang.Float.intBitsToFloat(bits)

  /** Convert Float to Int bits. */
  def fromFloat(f: Float): Int = java.lang.Float.floatToRawIntBits(f)
}

/**
 * Reference tiled matrix multiply matching SPEC.md 8.2 two-level iteration.
 * Uses RefFp32 for all arithmetic (FTZ/DAZ, round-to-nearest-even).
 */
object RefMatmul {
  /**
   * Compute D = A x B using the exact two-level tiling loop from SPEC.md 8.2.
   *
   * @param A    row-major matrix as flat Int array (FP32 bits), M x lda elements
   * @param B    row-major matrix as flat Int array (FP32 bits), K x ldb elements
   * @param M    rows of A / D
   * @param N    cols of B / D
   * @param K    shared reduction dimension
   * @param lda  leading dimension of A (elements)
   * @param ldb  leading dimension of B (elements)
   * @param ldd  leading dimension of D (elements)
   * @param primM primitive M
   * @param primN primitive N
   * @param primK primitive K
   * @param S    physical systolic dimension (default 4)
   * @return D as flat Int array, M x ldd elements
   */
  def compute(
      A: Array[Int], B: Array[Int],
      M: Int, N: Int, K: Int,
      lda: Int, ldb: Int, ldd: Int,
      primM: Int, primN: Int, primK: Int,
      S: Int = 4
  ): Array[Int] = {
    val D = new Array[Int](M * ldd)

    // Command-level loop (SPEC.md 8.2)
    val numPi = M / primM
    val numPj = N / primN
    val numPkCmd = K / primK

    for (pi <- 0 until numPi) {
      for (pj <- 0 until numPj) {
        for (pkCmd <- 0 until numPkCmd) {
          // Within each primitive, micro-tile decomposition
          val numMi = primM / S
          val numNj = primN / S
          val numPk = primK / S

          for (mi <- 0 until numMi) {
            for (nj <- 0 until numNj) {
              // Accumulator for this micro-tile (S x S), zero-initialized
              val acc = Array.fill(S * S)(0) // FP32 zero = 0x00000000

              for (pk <- 0 until numPk) {
                // Feed systolic: compute S x S partial product
                for (i <- 0 until S) {
                  for (j <- 0 until S) {
                    for (kk <- 0 until S) {
                      val globalM = pi * primM + mi * S + i
                      val globalK = pkCmd * primK + pk * S + kk
                      val globalN = pj * primN + nj * S + j

                      val aVal = A(globalM * lda + globalK)
                      val bVal = B(globalK * ldb + globalN)
                      val prod = RefFp32.mul(aVal, bVal)
                      acc(i * S + j) = RefFp32.add(acc(i * S + j), prod)
                    }
                  }
                }
              }

              // Write micro-tile results to D
              // K-reduction: for pkCmd > 0, add partial sum to existing D
              for (i <- 0 until S) {
                for (j <- 0 until S) {
                  val globalM = pi * primM + mi * S + i
                  val globalN = pj * primN + nj * S + j
                  val dIdx = globalM * ldd + globalN

                  if (pkCmd > 0) {
                    D(dIdx) = RefFp32.add(D(dIdx), acc(i * S + j))
                  } else {
                    D(dIdx) = acc(i * S + j)
                  }
                }
              }
            }
          }
        }
      }
    }

    D
  }

  /** Naive (non-tiled) FP32 matmul for comparison. */
  def naiveCompute(
      A: Array[Int], B: Array[Int],
      M: Int, N: Int, K: Int,
      lda: Int, ldb: Int, ldd: Int
  ): Array[Int] = {
    val D = new Array[Int](M * ldd)
    for (m <- 0 until M) {
      for (n <- 0 until N) {
        var acc = 0 // FP32 zero
        for (k <- 0 until K) {
          val prod = RefFp32.mul(A(m * lda + k), B(k * ldb + n))
          acc = RefFp32.add(acc, prod)
        }
        D(m * ldd + n) = acc
      }
    }
    D
  }
}
