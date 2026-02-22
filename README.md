# Systolic FP32 Matrix Multiplier in SpinalHDL

This repository is currently focused on the command-driven `SystolicMatmul` design:

- IEEE-754 single-precision (FP32) datapath
- Systolic PE array with dual-bank accumulation
- Tagged read engines (`A`/`B`), reordered responses, transpose/skew feed path
- Drain/output packer with K-reduction accumulation
- Completion/error status channel

## Toolchain

- Scala: `2.12.21`
- SpinalHDL: `1.14.1`
- Mill: `0.12.17`
- Simulation backend: Verilator

## Current Architecture

Top-level dataflow:

```text
cmd_* --> CmdFrontend --> cmdDispatchQ --> TileScheduler --> stepIssueQ
                                                           |
                                                           v
                        +-------------------------------------------------------+
                        | Prefetch Engine (in SystolicMatmul top-level)         |
                        | - config/start ReadEngine A/B                          |
                        | - load staged A/B tile banks (depth = stagedTileDepth) |
                        | - enqueue inject context/slot/sequence                 |
                        +----------------------------+--------------------------+
                                                     |
                      a_rd_req/rsp_* <-> ReadEngineA|ReadEngineB <-> b_rd_req/rsp_*
                                                     v
                                         injectQ + injectSlotQ + injectSeqQ
                                                     |
                                                     v
                        +-------------------------------------------------------+
                        | Inject Engine + bank-hazard scoreboard                |
                        | - consume staged tiles                                |
                        | - drive SystolicCore FEED                             |
                        | - dual-bank compute/drain ownership tracking          |
                        +----------------------------+--------------------------+
                                                     |
                                                     v
                                       SystolicCore (Skew A/B + SxS PEs)
                                                     |
                                                     v
                                         drainQ + drainDelayQ
                                                     |
                                                     v
                        +-------------------------------------------------------+
                        | Drain Engine + DrainPacker                            |
                        | - tail-delay aware drain start                         |
                        | - in-module D-buffer + pk_cmd RMW accumulation        |
                        | - flush to outFifo                                    |
                        +----------------------------+--------------------------+
                                                     |
                                                     v
                                                  outFifo --> d_wr_*

Status path:
  Cmd accept --> in-order commit queue (writes-done + drains-done + poison)
              --> sts_*
```

Detailed `SystolicCore` view (conceptual `S=4`, same structure scales to `SxS`):

```text
Core IO and timing
------------------
aIn[0..S-1]  : west-edge operands after A-skew network
bIn[0..S-1]  : north-edge operands after B-skew network
validIn      : data-valid wave entering the mesh
tileStart    : clears/arms PE accumulators for a new K tile
bankSel      : active accumulation bank selector (ping-pong)
drainBank    : bank exported on drain_data this cycle
drainRow     : row mux for external drainData[0..S-1]

Steady-state movement per cycle:
- A values move west -> east by one PE.
- B values move north -> south by one PE.
- Each PE performs FP32 MAC into the selected bank with latency (L_MUL + L_ADD).

S=4 PE mesh (coordinate view)
-----------------------------
west aIn ->   c0                  c1                  c2                  c3
           +---------+ A->     +---------+ A->     +---------+ A->     +---------+
north bIn  | PE(0,0) |         | PE(0,1) |         | PE(0,2) |         | PE(0,3) |
   v       +----+----+         +----+----+         +----+----+         +----+----+
           B v  |               B v  |               B v  |               B v  |
           +---------+ A->     +---------+ A->     +---------+ A->     +---------+
           | PE(1,0) |         | PE(1,1) |         | PE(1,2) |         | PE(1,3) |
           +----+----+         +----+----+         +----+----+         +----+----+
           B v  |               B v  |               B v  |               B v  |
           +---------+ A->     +---------+ A->     +---------+ A->     +---------+
           | PE(2,0) |         | PE(2,1) |         | PE(2,2) |         | PE(2,3) |
           +----+----+         +----+----+         +----+----+         +----+----+
           B v  |               B v  |               B v  |               B v  |
           +---------+ A->     +---------+ A->     +---------+ A->     +---------+
           | PE(3,0) |         | PE(3,1) |         | PE(3,2) |         | PE(3,3) |
           +---------+         +---------+         +---------+         +---------+
             south                south               south               south

PE micro-architecture (each PE(r,c))
------------------------------------
a_in --> [A pipe regs] ----+-------------------------> a_out
                           |
b_in --> [B pipe regs] ----+-------------------------> b_out
                           |
                           +--> [fpMul pipeline] --> [fpAdd pipeline] --> sum_new
                                                              |
                                   +--------------------------+--------------------------+
                                   |                                                     |
                        bank0_active when bankSel=0                           bank1_active when bankSel=1
                                   |                                                     |
                                 bank0 <----------------------------------------------> bank1
                                   ^                                                     ^
                                   |                                                     |
                              shadow0 update                                        shadow1 update

drain_data = (drainBank == 0) ? bank0_committed : bank1_committed

Drain path across rows
----------------------
drainRow selects one PE row for export:
drainData[0] = PE(drainRow, 0).drain_data
drainData[1] = PE(drainRow, 1).drain_data
drainData[2] = PE(drainRow, 2).drain_data
drainData[3] = PE(drainRow, 3).drain_data
... generalized to `S` columns.
```

## Key Files

- `src/matmul/SystolicMatmul.scala` - top-level integration
- `src/matmul/CmdFrontend.scala` - command validation/accept path
- `src/matmul/TileScheduler.scala` - primitive/micro-tile scheduling
- `src/matmul/ReadEngine.scala` - tagged request issue + response retirement
- `src/matmul/ReorderBuffer.scala` - in-order retirement for out-of-order responses
- `src/matmul/SkewNetwork.scala` - systolic edge skew
- `src/matmul/SystolicCore.scala` - PE array compute core
- `src/matmul/PE.scala` - per-PE dual-bank FP32 MAC
- `src/matmul/Fp32MulPipe.scala` - pipelined FP32 multiplier
- `src/matmul/Fp32AddPipe.scala` - pipelined FP32 adder
- `src/matmul/DrainPacker.scala` - drain, pack, and K-reduction
- `test/src/matmul/SystolicMatmulTb.scala` - integration harness
- `test/src/matmul/PerfSpec.scala` - performance contract tests
- `test/src/matmul/UtilizationSpec.scala` - Stage 20 utilization gates and attribution

Reference/decomposition modules (currently not the active top-level control path):
- `src/matmul/controllers/PrefetchController.scala`
- `src/matmul/controllers/InjectController.scala`
- `src/matmul/controllers/DrainController.scala`

## Common Commands

```bash
# compile and run all tests
mill spinal101.compile
mill spinal101.test

# generate systolic RTL (default S=4)
make rtl

# explicit alias for systolic RTL generation
make rtl-systolic

# generate legacy baseline RTL (Fp32MatrixMulV0, default S=2)
make rtl-v0

# explicitly choose systolic size for RTL generation
make rtl-systolic SYSTOLIC_S=4
make rtl-systolic SYSTOLIC_S=16
```

## Pre-PnR STA Target

Pre-PnR synthesis/STA is intentionally targeted at a smaller systolic size by default:

- Default pre-PnR target: `PREPNR_S=4` (4x4 systolic)
- Full-size override: `PREPNR_S=16`

```bash
# default pre-PnR run (4x4)
make prepnr

# explicit small run
make prepnr PREPNR_S=4

# full-size run
make prepnr PREPNR_S=16
```

`make flow` runs test + pre-PnR + report, and uses the same pre-PnR default (`PREPNR_S=4`).

## Utilization Gates (Stage 20)

`UtilizationSpec` exports per-scenario utilization metrics to:

- `build/utilization_gates/*.metrics.env`

CI now enforces explicit Stage 20 gates in the workflow in addition to test assertions:

- `feed_duty >= 0.95` in `ideal_feed_dense` for `S=4` and `S=16`
- `req_duty_a` / `req_duty_b` floors in `ideal_feed_dense`:
  - `S=4 >= 0.39`
  - `S=16 >= 0.70`
- bounded true bank-conflict stalls in ideal scenarios:
  - `stall_bank_hazard_cycles <= 4`
  - `bucket_bank_hazard_wait_cycles <= 4`

Current near-100% gate outcomes (local `mill spinal101.test.testOnly matmul.UtilizationSpec`):

| Metric (ideal_feed_dense) | S=4 | S=16 |
| --- | --- | --- |
| `feed_duty` | 0.985563 | 0.996351 |
| `req_duty_a` | 0.399374 | 0.725204 |
| `req_duty_b` | 0.399374 | 0.725204 |
| `stall_bank_hazard_cycles` | 0 | 0 |
| `bucket_bank_hazard_wait_cycles` | 0 | 0 |
| `feed_run_longest_cycles` | 20 | 208 |
| `feed_run_average_cycles` | 5.224490 | 53.894737 |

CI summaries now publish trend tables for:

- `feed_duty` (S4 vs S16 across scenarios)
- `req_duty_a` / `req_duty_b` (S4 vs S16 across scenarios)
- FEED run-length metrics (`feed_run_longest_cycles`, `feed_run_average_cycles`, `feed_break_count`)

### Utilization Counter Guide

These counters are exported per scenario (`*.metrics.env`) and are intended to be read together.

| Counter(s) | Interpretation |
| --- | --- |
| `measured_window_start_cycle`, `measured_window_end_cycle`, `measured_window_cycles` | Measurement span used for utilization accounting. |
| `measured_feed_cycles` | Cycles where injector is in FEED (`utilInjectFullCycle`). |
| `measured_stall_sample_cycles` | Non-FEED sampled cycles in the same window (`utilStallSample`). |
| `feed_duty_window_cycles` | Denominator for `feed_duty`: `feed + bank_hazard_wait + drain_enqueue_bookkeeping + output_backpressure`. |
| `feed_duty` | Effective FEED duty factor within the FEED-activity window. |
| `req_fire_a_cycles`, `req_fire_b_cycles` | Cycles where A/B read requests fire (`valid && ready`). |
| `req_duty_a`, `req_duty_b` | A/B request duty over `measured_window_cycles`. |
| `bucket_prefetch_setup_cycles` | Non-FEED cycles attributed to prefetch/setup residency. |
| `bucket_bank_hazard_wait_cycles` | Non-FEED cycles attributed to true bank conflict waiting. |
| `bucket_drain_enqueue_bookkeeping_cycles` | Non-FEED cycles attributed to drain-enqueue bookkeeping serialization. |
| `bucket_output_backpressure_cycles` | Non-FEED cycles attributed to output channel backpressure. |
| `bucket_non_feed_cycles`, `bucket_non_feed_sum_cycles` | Sanity check: classified non-FEED cycles should match sampled non-FEED cycles. |
| `feed_run_count` | Number of FEED streaks in the measured window. |
| `feed_run_longest_cycles` | Longest contiguous FEED streak length. |
| `feed_run_average_cycles` | Average FEED streak length. |
| `feed_break_count` | Number of FEED-to-non-FEED transitions that later return to FEED. |
| `feed_break_prefetch_setup`, `feed_break_bank_hazard_wait`, `feed_break_drain_enqueue_bookkeeping`, `feed_break_output_backpressure`, `feed_break_other` | First classified cause for each FEED break. |
| `feed_break_classified_count` | Sanity check: sum of FEED break causes. |
| `stall_no_step_cycles` | No-step stall cycles (scheduler/inject starvation). |
| `stall_a_not_ready_cycles`, `stall_b_not_ready_cycles` | A/B readiness-limited stall cycles. |
| `stall_bank_hazard_cycles` | Stall cycles due to true bank conflict hazard. |
| `stall_drain_blocked_cycles` | Stall cycles where drain enqueue cannot push. |
| `stall_output_backpressure_cycles` | Stall cycles due to D-channel backpressure. |
| `stall_error_flush_cycles` | Stall cycles while error flush/fence is active. |
| `stall_prefetch_setup_cycles` | Stall cycles in prefetch setup/control states. |
| `stall_bank_hazard_wait_cycles` | Stall cycles counted in true bank-hazard wait bucket. |
| `stall_drain_enqueue_bookkeeping_cycles` | Stall cycles counted in drain bookkeeping bucket. |

Important interpretation note:
- `utilInjectFullCycle` is a raw FEED-state indicator. The ratio `utilInjectFullCycles / utilInjectWindowCycles` includes prefetch/setup residency and is not the Stage 20 near-100% gate.
- Stage 20 `feed_duty` uses denominator `feed + bank_hazard_wait + drain_enqueue_bookkeeping + output_backpressure` (`feed_duty_window_cycles`), which is the metric expected to be close to 1.0 in dense ideal scenarios.

## Output Artifacts

Pre-PnR outputs are written under `build/openlane_prepnr/`:

- `summary.md` - summarized gate count / slack / frequency
- `metrics.env` - key-value metrics export
- `yosys.log` - synthesis log
- `openroad.log` - STA/floorplan log
- `opensta_checks.rpt` - timing path report
