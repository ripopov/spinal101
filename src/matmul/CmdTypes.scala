package matmul

import spinal.core._

case class CmdDesc(cfg: SystolicMatmulConfig) extends Bundle {
  val cmdId = UInt(16 bits)
  val aBase = UInt(cfg.addrBits bits)
  val bBase = UInt(cfg.addrBits bits)
  val dBase = UInt(cfg.addrBits bits)
  val m = UInt(16 bits)
  val n = UInt(16 bits)
  val k = UInt(16 bits)
  val lda = UInt(16 bits)
  val ldb = UInt(16 bits)
  val ldd = UInt(16 bits)
  val primM = UInt(16 bits)
  val primN = UInt(16 bits)
  val primK = UInt(16 bits)
  val flags = Bits(8 bits)
}

case class DWriteEntry(cfg: SystolicMatmulConfig) extends Bundle {
  val addr = UInt(cfg.addrBits bits)
  val data = Bits(cfg.clBits bits)
  val last = Bool()
  val cmdId = UInt(16 bits)
}

case class StatusEntry() extends Bundle {
  val cmdId = UInt(16 bits)
  val ok = Bool()
  val errCode = Bits(8 bits)
}
