package matmul

import spinal.core._

case class TileScheduler(cfg: SystolicMatmulConfig = SystolicMatmulConfig()) extends Component {
  val io = new Bundle {
    val cmdValid = in Bool()
    val cmdReady = out Bool()
    val cmdDesc = in(CmdDesc(cfg))

    val stepValid = out Bool()
    val stepReady = in Bool()

    val pi = out UInt(16 bits)
    val pj = out UInt(16 bits)
    val pkCmd = out UInt(16 bits)
    val mi = out UInt(16 bits)
    val nj = out UInt(16 bits)
    val pk = out UInt(16 bits)

    val clearBank = out Bool()
    val drainTrigger = out Bool()
    val bankSel = out Bool()
    val busy = out Bool()
  }

  val active = Reg(Bool()) init (False)
  val descReg = Reg(CmdDesc(cfg))

  val pi = Reg(UInt(16 bits)) init (0)
  val pj = Reg(UInt(16 bits)) init (0)
  val pkCmd = Reg(UInt(16 bits)) init (0)
  val mi = Reg(UInt(16 bits)) init (0)
  val nj = Reg(UInt(16 bits)) init (0)
  val pk = Reg(UInt(16 bits)) init (0)
  // Explicit bank rotation: one bank epoch per drained micro-tile.
  val bankEpoch = Reg(Bool()) init (False)

  val numPi = UInt(16 bits)
  val numPj = UInt(16 bits)
  val numPkCmd = UInt(16 bits)
  val numMi = UInt(16 bits)
  val numNj = UInt(16 bits)
  val numPk = UInt(16 bits)

  numPi := descReg.m / descReg.primM
  numPj := descReg.n / descReg.primN
  numPkCmd := descReg.k / descReg.primK
  numMi := descReg.primM / U(cfg.s, 16 bits)
  numNj := descReg.primN / U(cfg.s, 16 bits)
  numPk := descReg.primK / U(cfg.s, 16 bits)

  io.cmdReady := !active
  when(io.cmdValid && io.cmdReady) {
    active := True
    descReg := io.cmdDesc
    pi := 0
    pj := 0
    pkCmd := 0
    mi := 0
    nj := 0
    pk := 0
    bankEpoch := False
  }

  io.stepValid := active
  io.pi := pi
  io.pj := pj
  io.pkCmd := pkCmd
  io.mi := mi
  io.nj := nj
  io.pk := pk

  io.clearBank := pk === 0
  io.drainTrigger := pk === (numPk - 1)
  io.bankSel := bankEpoch

  when(active && io.stepReady) {
    when(pk === (numPk - 1)) {
      bankEpoch := !bankEpoch
    }
    when(pk + 1 < numPk) {
      pk := pk + 1
    } otherwise {
      pk := 0
      when(nj + 1 < numNj) {
        nj := nj + 1
      } otherwise {
        nj := 0
        when(mi + 1 < numMi) {
          mi := mi + 1
        } otherwise {
          mi := 0
          when(pkCmd + 1 < numPkCmd) {
            pkCmd := pkCmd + 1
          } otherwise {
            pkCmd := 0
            when(pj + 1 < numPj) {
              pj := pj + 1
            } otherwise {
              pj := 0
              when(pi + 1 < numPi) {
                pi := pi + 1
              } otherwise {
                active := False
              }
            }
          }
        }
      }
    }
  }

  io.busy := active
}
