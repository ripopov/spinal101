package matmul

import spinal.core._

object GenerateFp32MatrixMul {
  def main(args: Array[String]): Unit = {
    val size = if (args.nonEmpty) args(0).toInt else 2

    SpinalConfig(targetDirectory = "generated")
      .generateVerilog(Fp32MatrixMulV0(size = size))
  }
}
