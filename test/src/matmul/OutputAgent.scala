package matmul

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

sealed trait BackpressureMode {
  def readyForCycle(cycle: Long, rng: Random): Boolean
}

object BackpressureMode {
  case object AlwaysReady extends BackpressureMode {
    override def readyForCycle(cycle: Long, rng: Random): Boolean = true
  }

  final case class RandomReady(readyProbability: Double) extends BackpressureMode {
    require(readyProbability >= 0.0 && readyProbability <= 1.0, "readyProbability must be in [0,1]")
    override def readyForCycle(cycle: Long, rng: Random): Boolean = rng.nextDouble() < readyProbability
  }

  final case class PeriodicStall(periodCycles: Int, stallCycles: Int) extends BackpressureMode {
    require(periodCycles > 0, "periodCycles must be > 0")
    require(stallCycles >= 0 && stallCycles <= periodCycles, "stallCycles must be in [0, periodCycles]")
    override def readyForCycle(cycle: Long, rng: Random): Boolean = {
      val slot = (cycle % periodCycles).toInt
      slot >= stallCycles
    }
  }
}

final case class OutputAgentConfig(
    clBytes: Int,
    backpressureMode: BackpressureMode = BackpressureMode.AlwaysReady,
    seed: Int = 1
) {
  require(clBytes > 0, "clBytes must be > 0")
}

final case class DWriteBeat(addr: BigInt, data: BigInt, cmdId: Int, last: Boolean, cycle: Long)

/**
 * Captures D write channel beats and applies configurable d_wr_ready backpressure.
 */
final class OutputAgent(
    cfg: OutputAgentConfig,
    clockDomain: ClockDomain,
    dWrValid: Bool,
    dWrReady: Bool,
    dWrAddr: UInt,
    dWrData: Bits,
    dWrLast: Bool,
    dWrCmdId: UInt
) {
  private val rng = new Random(cfg.seed)
  private var cycle = 0L

  val writesByAddr: mutable.Map[BigInt, BigInt] = mutable.HashMap.empty
  val writeBeats: mutable.ArrayBuffer[DWriteBeat] = mutable.ArrayBuffer.empty
  val completedCmdIds: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
  val fireCycles: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty

  private var activeCmdId: Option[Int] = None

  def start(): Unit = {
    dWrReady #= true

    fork {
      while (true) {
        val ready = cfg.backpressureMode.readyForCycle(cycle, rng)
        dWrReady #= ready

        clockDomain.waitSampling()

        if (dWrValid.toBoolean && dWrReady.toBoolean) {
          val addr = dWrAddr.toBigInt
          val data = dWrData.toBigInt
          val cmdId = dWrCmdId.toBigInt.intValue
          val last = dWrLast.toBoolean

          assert((addr % cfg.clBytes) == 0, s"D write address not CL-aligned: 0x${addr.toString(16)}")

          activeCmdId match {
            case None => activeCmdId = Some(cmdId)
            case Some(active) =>
              assert(
                active == cmdId,
                s"d_wr_cmd_id changed before d_wr_last: active=$active new=$cmdId"
              )
          }

          writesByAddr(addr) = data
          writeBeats += DWriteBeat(addr, data, cmdId, last, cycle)
          fireCycles += cycle

          if (last) {
            completedCmdIds += cmdId
            activeCmdId = None
          }
        }

        cycle += 1
      }
    }
  }
}
