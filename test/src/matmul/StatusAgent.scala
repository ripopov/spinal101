package matmul

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable

final case class StatusBeat(cmdId: Int, ok: Boolean, errCode: Int)

/**
 * Captures status completions and checks in-order cmd_id completion.
 */
final class StatusAgent(
    clockDomain: ClockDomain,
    stsValid: Bool,
    stsReady: Bool,
    stsCmdId: UInt,
    stsOk: Bool,
    stsErrCode: Bits,
    expectedCmdOrder: mutable.Queue[Int]
) {
  val statuses: mutable.ArrayBuffer[StatusBeat] = mutable.ArrayBuffer.empty

  def start(alwaysReady: Boolean = true): Unit = {
    stsReady #= alwaysReady

    fork {
      while (true) {
        if (!alwaysReady) stsReady #= true

        clockDomain.waitSampling()

        if (stsValid.toBoolean && stsReady.toBoolean) {
          val cmdId = stsCmdId.toBigInt.intValue
          val ok = stsOk.toBoolean
          val err = stsErrCode.toBigInt.intValue

          if (expectedCmdOrder.nonEmpty) {
            val expected = expectedCmdOrder.dequeue()
            assert(
              expected == cmdId,
              s"status out-of-order: expected cmd_id=$expected observed=$cmdId"
            )
          }

          statuses += StatusBeat(cmdId, ok, err)
        }
      }
    }
  }

  def waitFor(cmdId: Int, timeoutCycles: Int, clockDomain: ClockDomain): Option[StatusBeat] = {
    var waited = 0
    while (waited < timeoutCycles) {
      statuses.find(_.cmdId == cmdId) match {
        case some @ Some(_) => return some
        case None =>
          clockDomain.waitSampling()
          waited += 1
      }
    }
    None
  }
}
