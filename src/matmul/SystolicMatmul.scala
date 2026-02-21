package matmul

import spinal.core._

case class SystolicMatmulConfig(
    addrBits: Int = 64,
    clBits: Int = 512,
    fpBits: Int = 32,
    maxOutstandingRd: Int = 16,
    cmdqDepth: Int = 4,
    outFifoDepthCl: Int = 32,
    lMul: Int = 4,
    lAdd: Int = 4
) {
  require(clBits % fpBits == 0, "CL_BITS must be a multiple of FP_BITS")
  require((maxOutstandingRd & (maxOutstandingRd - 1)) == 0, "MAX_OUTSTANDING_RD must be power of 2")

  val clBytes: Int = clBits / 8
  val clElems: Int = clBits / fpBits
  val s: Int = clElems
  val tagBits: Int = log2Up(maxOutstandingRd)
}

case class SystolicMatmul(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val io = new Bundle {
    // Command Descriptor Input (Decoupled)
    val cmd_valid = in Bool()
    val cmd_ready = out Bool()
    val cmd_desc_cmd_id = in UInt (16 bits)
    val cmd_desc_a_base = in UInt (cfg.addrBits bits)
    val cmd_desc_b_base = in UInt (cfg.addrBits bits)
    val cmd_desc_d_base = in UInt (cfg.addrBits bits)
    val cmd_desc_m = in UInt (16 bits)
    val cmd_desc_n = in UInt (16 bits)
    val cmd_desc_k = in UInt (16 bits)
    val cmd_desc_lda = in UInt (16 bits)
    val cmd_desc_ldb = in UInt (16 bits)
    val cmd_desc_ldd = in UInt (16 bits)
    val cmd_desc_prim_m = in UInt (16 bits)
    val cmd_desc_prim_n = in UInt (16 bits)
    val cmd_desc_prim_k = in UInt (16 bits)
    val cmd_desc_flags = in Bits (8 bits)

    // A Read Request Channel
    val a_rd_req_valid = out Bool()
    val a_rd_req_ready = in Bool()
    val a_rd_req_addr = out UInt (cfg.addrBits bits)
    val a_rd_req_tag = out UInt (cfg.tagBits bits)

    // A Read Response Channel
    val a_rd_rsp_valid = in Bool()
    val a_rd_rsp_ready = out Bool()
    val a_rd_rsp_data = in Bits (cfg.clBits bits)
    val a_rd_rsp_tag = in UInt (cfg.tagBits bits)
    val a_rd_rsp_err = in Bool()

    // B Read Request Channel
    val b_rd_req_valid = out Bool()
    val b_rd_req_ready = in Bool()
    val b_rd_req_addr = out UInt (cfg.addrBits bits)
    val b_rd_req_tag = out UInt (cfg.tagBits bits)

    // B Read Response Channel
    val b_rd_rsp_valid = in Bool()
    val b_rd_rsp_ready = out Bool()
    val b_rd_rsp_data = in Bits (cfg.clBits bits)
    val b_rd_rsp_tag = in UInt (cfg.tagBits bits)
    val b_rd_rsp_err = in Bool()

    // D Write Output Channel
    val d_wr_valid = out Bool()
    val d_wr_ready = in Bool()
    val d_wr_addr = out UInt (cfg.addrBits bits)
    val d_wr_data = out Bits (cfg.clBits bits)
    val d_wr_last = out Bool()
    val d_wr_cmd_id = out UInt (16 bits)

    // Completion / Status Channel
    val sts_valid = out Bool()
    val sts_ready = in Bool()
    val sts_cmd_id = out UInt (16 bits)
    val sts_ok = out Bool()
    val sts_err_code = out Bits (8 bits)
  }

  // Stub: command not accepted
  io.cmd_ready := False

  // Stub: no read requests issued
  io.a_rd_req_valid := False
  io.a_rd_req_addr := 0
  io.a_rd_req_tag := 0

  io.b_rd_req_valid := False
  io.b_rd_req_addr := 0
  io.b_rd_req_tag := 0

  // Stub: always accept responses (sink them)
  io.a_rd_rsp_ready := True
  io.b_rd_rsp_ready := True

  // Stub: no D writes
  io.d_wr_valid := False
  io.d_wr_addr := 0
  io.d_wr_data := 0
  io.d_wr_last := False
  io.d_wr_cmd_id := 0

  // Stub: no status
  io.sts_valid := False
  io.sts_cmd_id := 0
  io.sts_ok := False
  io.sts_err_code := 0
}
