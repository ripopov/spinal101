package matmul

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

sealed trait MemoryLatencyModel {
  def sampleCycles(rng: Random): Int
}

object MemoryLatencyModel {
  final case class Fixed(cycles: Int) extends MemoryLatencyModel {
    require(cycles >= 0, "fixed latency must be >= 0")
    override def sampleCycles(rng: Random): Int = cycles
  }

  final case class RandomRange(minCycles: Int, maxCycles: Int) extends MemoryLatencyModel {
    require(minCycles >= 0, "random latency min must be >= 0")
    require(maxCycles >= minCycles, "random latency max must be >= min")
    override def sampleCycles(rng: Random): Int = minCycles + rng.nextInt(maxCycles - minCycles + 1)
  }

  final case class WorstCase(maxCycles: Int) extends MemoryLatencyModel {
    require(maxCycles >= 0, "worst-case latency must be >= 0")
    override def sampleCycles(rng: Random): Int = maxCycles
  }
}

final case class MemoryAgentConfig(
    clBytes: Int,
    latencyModel: MemoryLatencyModel = MemoryLatencyModel.Fixed(0),
    reorderWindow: Int = 0,
    injectErrEvery: Int = 0,
    seed: Int = 1
) {
  require(clBytes > 0, "clBytes must be > 0")
  require(reorderWindow >= 0, "reorderWindow must be >= 0")
  require(injectErrEvery >= 0, "injectErrEvery must be >= 0")
}

/**
 * Simulated read-channel memory agent for one interface (A or B).
 *
 * Features:
 * - Backing-store cacheline map
 * - Configurable response latency (fixed/random/worst-case)
 * - Out-of-order response delivery within a configurable window
 * - CL alignment checks on requests
 * - Outstanding tag tracking and tag-protocol assertions
 * - Optional error injection via injectErrEvery
 */
final class MemoryAgent(
    name: String,
    cfg: MemoryAgentConfig,
    clockDomain: ClockDomain,
    reqValid: Bool,
    reqReady: Bool,
    reqAddr: UInt,
    reqTag: UInt,
    rspValid: Bool,
    rspReady: Bool,
    rspData: Bits,
    rspTag: UInt,
    rspErr: Bool
) {
  private final case class PendingRsp(seq: Long, dueCycle: Long, addr: BigInt, tag: Int, data: BigInt, err: Boolean)

  private val memory = mutable.HashMap.empty[BigInt, BigInt]
  private val pending = mutable.ArrayBuffer.empty[PendingRsp]
  private val outstandingTags = mutable.HashSet.empty[Int]
  private val rng = new Random(cfg.seed)

  private var nextSeq = 0L
  private var cycle = 0L
  private var rspCounter = 0L
  private var heldRsp: Option[PendingRsp] = None

  @volatile var maxOutstandingObserved: Int = 0
  @volatile var lastOutstandingObserved: Int = 0

  def writeCacheLine(addr: BigInt, data: BigInt): Unit = {
    require(isAligned(addr), s"$name writeCacheLine unaligned address 0x${addr.toString(16)}")
    memory(addr) = data
  }

  def readCacheLine(addr: BigInt): BigInt = {
    require(isAligned(addr), s"$name readCacheLine unaligned address 0x${addr.toString(16)}")
    memory.getOrElse(addr, BigInt(0))
  }

  def start(): Unit = {
    reqReady #= true
    rspValid #= false
    rspData #= 0
    rspTag #= 0
    rspErr #= false

    fork {
      while (true) {
        val currentRsp = heldRsp.orElse(selectDueResponse())
        heldRsp = currentRsp

        reqReady #= true
        currentRsp match {
          case Some(r) =>
            rspValid #= true
            rspData #= r.data
            rspTag #= r.tag
            rspErr #= r.err
          case None =>
            rspValid #= false
            rspData #= 0
            rspTag #= 0
            rspErr #= false
        }

        clockDomain.waitSampling()

        if (reqValid.toBoolean && reqReady.toBoolean) {
          val addr = reqAddr.toBigInt
          val tag = reqTag.toBigInt.intValue
          assert(isAligned(addr), s"$name request address not CL-aligned: 0x${addr.toString(16)}")
          assert(!outstandingTags.contains(tag), s"$name duplicate outstanding tag: $tag")

          outstandingTags += tag
          lastOutstandingObserved = outstandingTags.size
          if (lastOutstandingObserved > maxOutstandingObserved) {
            maxOutstandingObserved = lastOutstandingObserved
          }
          val latency = cfg.latencyModel.sampleCycles(rng)
          val data = memory.getOrElse(addr, BigInt(0))
          rspCounter += 1
          val err = cfg.injectErrEvery > 0 && (rspCounter % cfg.injectErrEvery == 0)

          pending += PendingRsp(
            seq = nextSeq,
            dueCycle = cycle + latency,
            addr = addr,
            tag = tag,
            data = data,
            err = err
          )
          nextSeq += 1
        }

        if (rspValid.toBoolean && rspReady.toBoolean) {
          val deliveredTag = rspTag.toBigInt.intValue
          assert(outstandingTags.contains(deliveredTag), s"$name response tag violation: unknown tag $deliveredTag")
          outstandingTags -= deliveredTag
          lastOutstandingObserved = outstandingTags.size
          heldRsp = None
        }

        cycle += 1
      }
    }
  }

  private def selectDueResponse(): Option[PendingRsp] = {
    val due = pending.zipWithIndex.filter { case (p, _) => p.dueCycle <= cycle }
    if (due.isEmpty) return None

    val orderedDue = due.sortBy { case (p, _) => p.seq }
    val candidateCount = math.min(cfg.reorderWindow + 1, orderedDue.length)
    val pick = if (candidateCount <= 1) 0 else rng.nextInt(candidateCount)
    val (_, pickedIdx) = orderedDue(pick)
    val rsp = pending(pickedIdx)
    pending.remove(pickedIdx)
    Some(rsp)
  }

  private def isAligned(addr: BigInt): Boolean = (addr % cfg.clBytes) == 0
}
