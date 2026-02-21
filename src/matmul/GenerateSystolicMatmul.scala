package matmul

import spinal.core._

object GenerateSystolicMatmul {
  def main(args: Array[String]): Unit = {
    val s = if (args.nonEmpty) args(0).toInt else 4
    val cfg = SystolicMatmulConfig(clBits = s * 32)

    SpinalConfig(targetDirectory = "generated")
      .generateVerilog(SystolicMatmul(cfg))
  }
}
