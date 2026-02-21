package matmul

import spinal.core._
import spinal.lib._

case class StatusGen(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val io = new Bundle {
    val cmdDoneValid = in Bool()
    val cmdDoneCmdId = in UInt(16 bits)

    val errValid = in Bool()
    val errCmdId = in UInt(16 bits)
    val errCode = in Bits(8 bits)

    val rejValid = in Bool()
    val rejReady = out Bool()
    val rejCmdId = in UInt(16 bits)
    val rejErrCode = in Bits(8 bits)

    val stsValid = out Bool()
    val stsReady = in Bool()
    val stsCmdId = out UInt(16 bits)
    val stsOk = out Bool()
    val stsErrCode = out Bits(8 bits)
  }

  val fifo = StreamFifo(StatusEntry(), 4)

  val pushValid = Bool()
  val pushPayload = StatusEntry()

  pushValid := False
  pushPayload.cmdId := 0
  pushPayload.ok := False
  pushPayload.errCode := 0

  io.rejReady := False

  when(io.errValid) {
    pushValid := True
    pushPayload.cmdId := io.errCmdId
    pushPayload.ok := False
    pushPayload.errCode := io.errCode
  } elsewhen(io.rejValid && fifo.io.push.ready) {
    pushValid := True
    pushPayload.cmdId := io.rejCmdId
    pushPayload.ok := False
    pushPayload.errCode := io.rejErrCode
    io.rejReady := True
  } elsewhen(io.cmdDoneValid) {
    pushValid := True
    pushPayload.cmdId := io.cmdDoneCmdId
    pushPayload.ok := True
    pushPayload.errCode := 0
  }

  fifo.io.push.valid := pushValid
  fifo.io.push.payload := pushPayload

  io.stsValid := fifo.io.pop.valid
  io.stsCmdId := fifo.io.pop.payload.cmdId
  io.stsOk := fifo.io.pop.payload.ok
  io.stsErrCode := fifo.io.pop.payload.errCode
  fifo.io.pop.ready := io.stsReady
}
