# Staged Implementation Plan

**Spec:** SPEC.md v1.1-draft
**Target:** Systolic FP32 Matmul (S=16, 512-bit CL, output-stationary)

## Current Status

- [x] **Stage 0** — Scaffold and CI green baseline
- [x] **Stage 1** — Scala reference model
- [x] **Stage 2** — Testbench infrastructure (memory model + scoreboard)
- [x] **Stage 3** — RTL: PE and pipelined FP32 datapath
- [x] **Stage 4** — RTL: Systolic core (S x S array + skew networks)
- [x] **Stage 5** — RTL: Memory engines, transpose buffer, command frontend
- [x] **Stage 6** — RTL: Drain, output packer, status — full top-level integration
- [x] **Stage 7** — Performance tests (throughput and overlap contracts)
- [x] **Stage 8** — Pre-PnR STA and timing closure
- [ ] **Stage 9** — Controller utilization instrumentation and baseline
- [ ] **Stage 10** — Controller decomposition into pipelined engines
- [ ] **Stage 11** — Zero-bubble A/B supply pipeline
- [ ] **Stage 12** — Drain/compute overlap with bank rotation
- [ ] **Stage 13** — Multi-command pipelining and in-order commit
- [ ] **Stage 14** — Utilization closure and CI gating

---

## Stage 0 — Scaffold and CI green baseline

**Goal:** Restructure the project for the new top-level module without breaking `make flow`.

**Work items:**

1. Keep existing `Fp32MatrixMul` and its tests intact as `Fp32MatrixMulV0` (rename the class).
   Update `GenerateFp32MatrixMul` and test to use the new name.
2. Create a new empty top-level `SystolicMatmul` component with the full port list from
   SPEC.md Section 5 (all ports stubbed: `cmd_ready := False`, no requests issued, no writes,
   no status). This is a compile-only placeholder.
3. Add a `GenerateSystolicMatmul` entry point targeting `generated/SystolicMatmul.v`.
4. Update `Makefile` to support `make rtl-systolic` (generates `SystolicMatmul.v`) alongside
   existing `make rtl`. Update `make flow` to use the systolic target.
5. Verify: `mill spinal101.compile` succeeds, `mill spinal101.test` passes (existing V0 tests),
   `make rtl-systolic` produces Verilog.

**Exit criterion:** `make test` and `make rtl-systolic` pass. CI green.

---

## Stage 1 — Scala reference model

**Goal:** Pure-Scala cycle-free golden model implementing the full SPEC math and tiling logic.
This model is the single source of truth for all RTL verification.

**Work items:**

1. Create `test/src/matmul/RefModel.scala` containing:
   - `RefFp32`: FP32 multiply and add with round-to-nearest-even, FTZ/DAZ, IEEE NaN/Inf rules
     (matching SPEC.md 6.2). Must be bit-exact with the RTL `Fp32Math` — validate by comparing
     against the existing `Fp32Math` Verilator sim on a sweep of edge cases.
   - `RefMatmul.compute(A, B, M, N, K, lda, ldb, ldd, primM, primN, primK)`: returns the D
     matrix. Implements the exact two-level tiling loop from SPEC.md 8.2 (command-level
     primitives, then micro-tile decomposition) with K-reduction accumulation.
2. Create `test/src/matmul/RefModelSpec.scala`:
   - Test `RefFp32` against known IEEE 754 corner cases: zeroes, denormals (flush-to-zero),
     NaN propagation, Inf arithmetic, rounding tie-breaking.
   - Test `RefMatmul` for all supported `prim_m/n/k` sizes from SPEC.md 4.1 with small
     random matrices. Verify against a naive triple-loop FP32 matmul (no tiling).
   - Test `RefMatmul` K-reduction: `K > prim_k` cases, confirming partial sums combine correctly.

**Exit criterion:** `mill spinal101.test` passes including all new ref-model tests. No RTL
changes — this is pure Scala.

---

## Stage 2 — Testbench infrastructure

**Goal:** Build the simulation harness that will drive and verify the `SystolicMatmul` RTL through
its SPEC.md interfaces. Reusable across all subsequent stages.

**Work items:**

1. Create `test/src/matmul/MemoryAgent.scala` — simulated memory backing store:
   - Serves A and B read channels with configurable latency (fixed, random, or worst-case).
   - Supports out-of-order response delivery (shuffles responses within a configurable window).
   - Validates request address alignment (`CL_BYTES`-aligned).
   - Tracks outstanding tags and asserts no tag violations.
   - Optionally injects `*_rd_rsp_err` for error-path testing.
2. Create `test/src/matmul/OutputAgent.scala` — D write channel sink:
   - Captures all write beats into a result buffer indexed by address.
   - Configurable backpressure pattern (`d_wr_ready` toggling: always-on, random, periodic stall).
   - Validates address alignment.
   - Tracks `d_wr_last` and `d_wr_cmd_id` for command boundary checking.
3. Create `test/src/matmul/StatusAgent.scala` — completion channel sink:
   - Captures `(cmd_id, ok, err_code)` tuples.
   - Asserts in-order completion per SPEC.md 9.2.
4. Create `test/src/matmul/CmdDriver.scala` — command descriptor driver:
   - Queues command descriptors and drives `cmd_valid/cmd_desc_*`.
   - Respects `cmd_ready` backpressure.
5. Create `test/src/matmul/SystolicMatmulTb.scala` — top-level test harness:
   - Instantiates `SystolicMatmul` DUT + all agents.
   - Helper method `runCommand(...)`: populates memory with A/B matrices, issues command,
     waits for status, reads D output, compares against `RefMatmul`.
   - Initial smoke test: single `S x S` matmul command with zero-latency memory and no
     backpressure. Expected to fail (DUT is a stub) — test should detect the timeout/mismatch
     and report it clearly. This validates the harness itself.

**Exit criterion:** `mill spinal101.test` passes. The systolic smoke test reports a clear
expected failure (stub DUT). All other tests pass. `make flow` green.

---

## Stage 3 — RTL: PE and pipelined FP32 datapath

**Goal:** Implement the building-block PE with pipelined FP32 multiply and add.

**Work items:**

1. Create `src/matmul/Fp32MulPipe.scala`:
   - Pipelined FP32 multiplier, `L_MUL` stages (configurable, default 4).
   - Uses the same logic as `Fp32Math.mul` but with pipeline registers inserted.
   - Valid sideband propagated through pipeline.
2. Create `src/matmul/Fp32AddPipe.scala`:
   - Pipelined FP32 adder, `L_ADD` stages (configurable, default 4).
   - Same approach as above.
3. Create `src/matmul/PE.scala`:
   - Inputs: `a_in` (32-bit), `b_in` (32-bit), `a_valid`, `b_valid`, `bank_sel`, `clear`.
   - Outputs: `a_out` (registered forwarding), `b_out` (registered forwarding).
   - Internal: `Fp32MulPipe` -> `Fp32AddPipe` -> accumulator write-back.
   - Dual accumulator banks (`bank0`, `bank1`), selected by `bank_sel`.
   - Drain read port: outputs accumulator value for a given bank/row.
4. Create `test/src/matmul/PESpec.scala`:
   - Test single PE: feed a sequence of `(a, b)` pairs with valid sideband, verify accumulated
     result matches `RefFp32` after the pipeline drains.
   - Test bank switching: accumulate on bank0, read bank0 while accumulating on bank1.
   - Test `clear` signal zeroes the active bank.
5. Create `test/src/matmul/Fp32PipeSpec.scala`:
   - Bit-exact comparison of `Fp32MulPipe` and `Fp32AddPipe` outputs against `RefFp32` over
     random and edge-case inputs. This locks the RTL to the reference model.

**Exit criterion:** `mill spinal101.test` passes all PE and pipe tests. `make flow` green.

---

## Stage 4 — RTL: Systolic core (S x S array + skew networks)

**Goal:** Wire up the S x S PE grid with skew networks and drain logic.

**Work items:**

1. Create `src/matmul/SkewNetwork.scala`:
   - Parameterized by `S` and direction (west-edge A skew, north-edge B skew).
   - Row/column `i` gets `i` stages of delay registers.
   - Valid sideband propagated.
2. Create `src/matmul/SystolicCore.scala`:
   - Instantiates `S x S` PEs with A skew (west) and B skew (north) networks.
   - Inputs: `S` A-column values + valids, `S` B-row values + valids per cycle.
   - Control: `bank_sel`, `clear_bank`.
   - Drain interface: read accumulator values from a specified bank, one row (`S` values) per
     cycle.
3. Create `test/src/matmul/SystolicCoreSpec.scala`:
   - Feed a single `S x S` micro-tile (S columns of A, S rows of B, one per cycle).
   - After inject + `2*(S-1)` tail cycles, drain and compare against `RefMatmul` for an
     `S x S` multiply.
   - Test with `S = 4` (small, fast simulation) and `S = 16` (default).
   - Test bank overlap: inject micro-tile T1 on bank0, switch to bank1, inject T2, verify
     both results independently.

**Exit criterion:** `mill spinal101.test` passes all core tests. `make flow` green.

---

## Stage 5 — RTL: Memory engines, transpose buffer, command frontend

**Goal:** Implement the data-supply path and command interface.

**Work items:**

1. Create `src/matmul/ReorderBuffer.scala`:
   - `MAX_OUTSTANDING_RD`-entry buffer.
   - Write port: indexed by tag (random-access).
   - Read port: drains in allocation order (FIFO head).
   - Head/tail pointers for sequential tag allocation and in-order retirement.
2. Create `src/matmul/ReadEngine.scala`:
   - Issues tagged read requests on `*_rd_req_*`, respects credit limit.
   - Receives responses on `*_rd_rsp_*`, writes to reorder buffer.
   - Output: in-order CL stream to downstream consumer.
   - Address generation driven by scheduler-provided (base, stride, count) descriptors.
3. Create `src/matmul/TransposeBuffer.scala`:
   - Double-buffered `S x S` register file.
   - Write side: accepts one CL (row) per cycle.
   - Read side: emits one column per cycle (transposed).
   - Bank swap handshake.
4. Create `src/matmul/CmdFrontend.scala`:
   - `cmd_valid/cmd_ready` interface with `CMDQ_DEPTH` FIFO.
   - Validation logic per SPEC.md 4.2 (dimension, stride, alignment, prim size checks).
   - Outputs validated command descriptors to tile scheduler.
5. Create `src/matmul/TileScheduler.scala`:
   - Consumes validated commands from `CmdFrontend`.
   - Generates the two-level iteration loop (SPEC.md 8.2): command-level primitives, then
     micro-tiles.
   - Drives `ReadEngine` address generation for A and B.
   - Controls `SystolicCore` bank select, clear, and drain triggers.
6. Unit tests for each module:
   - `ReorderBufferSpec`: in-order retirement with scrambled writes.
   - `ReadEngineSpec`: credit tracking, tag allocation, address sequence.
   - `TransposeBufferSpec`: write rows, read columns, verify transpose.
   - `CmdFrontendSpec`: valid commands accepted, invalid commands rejected with error status.

**Exit criterion:** `mill spinal101.test` passes all unit tests. `make flow` green.

---

## Stage 6 — RTL: Drain, output packer, status — full top-level integration

**Goal:** Complete `SystolicMatmul` top-level. The existing testbench harness from Stage 2
should now produce correct results.

**Work items:**

1. Create `src/matmul/DrainPacker.scala`:
   - Reads one row (S values) per cycle from `SystolicCore` inactive bank.
   - Packs into `CL_BITS` output beat.
   - K-reduction logic: for `pk_cmd > 0`, adds partial sum to buffered previous result.
   - Feeds `OUT_FIFO_DEPTH_CL`-deep output FIFO.
   - Drives `d_wr_*` interface with backpressure support.
2. Create `src/matmul/StatusGen.scala`:
   - Tracks command completion (all D writes for a command finished).
   - Drives `sts_*` interface.
   - Error path: on read error or validation failure, flushes pipeline, emits error status
     for failed command and any queued commands behind it (SPEC.md 10.2).
3. Wire all submodules into `SystolicMatmul`:
   - `CmdFrontend` -> `TileScheduler` -> `ReadEngine` (A, B) -> `ReorderBuffer` ->
     A: `TransposeBuffer` -> `SkewNetwork` | B: FIFO -> `SkewNetwork` -> `SystolicCore` ->
     `DrainPacker` -> D output FIFO -> `d_wr_*`.
   - `StatusGen` connected to completion events and error sources.
4. Update `GenerateSystolicMatmul` to generate the real design.
5. Integration tests using `SystolicMatmulTb` from Stage 2:
   - Single `S x S` command, zero-latency memory, no backpressure.
   - All supported `prim_m/n/k` sizes (directed).
   - Multi-primitive commands: `M, N > prim_m, prim_n`.
   - K-reduction: `K > prim_k`.
   - Back-to-back commands (CMDQ depth > 1).
   - Random memory latency with out-of-order responses.
   - Error injection: `*_rd_rsp_err` triggers error status.
   - Invalid command rejection.

**Test dimensions (use S=4 for fast iteration, S=16 for full validation):**

| Test | S | Memory latency | Backpressure | Description |
|---|---:|---|---|---|
| smoke | 4 | 0 | none | Single SxS tile |
| all-prims | 4 | 0 | none | All prim_m/n/k combos |
| multi-tile | 4 | 0 | none | M=2S, N=2S, K=S |
| k-reduction | 4 | 0 | none | K=4S, prim_k=S |
| ooo-memory | 4 | random 1-20 | none | Out-of-order responses |
| backpressure | 4 | random 1-10 | random | d_wr_ready toggling |
| full-16 | 16 | random 1-20 | random | Full-size validation |
| error | 4 | 0 | none | Read error injection |
| bad-cmd | 4 | n/a | n/a | Invalid command rejection |

**Exit criterion:** All integration tests pass. `make flow` green.

---

## Stage 7 — Performance tests

**Goal:** Verify the throughput and overlap contracts from SPEC.md Section 9.

**Work items:**

1. Create `test/src/matmul/PerfSpec.scala` with cycle-counting instrumentation:
   - **Drain throughput test:** Issue a large command (e.g., M=N=128, K=16) with `d_wr_ready`
     always asserted and zero-latency memory. Count D write beats. Assert sustained
     `1 CL/cycle` during drain windows.
   - **Overlap test:** Issue two back-to-back primitives. Measure the gap (in cycles) between
     the last inject cycle of P0 and the first inject cycle of P1. Assert gap <= 1 cycle
     (no-bubble contract, SPEC.md 8.9).
   - **Backpressure absorption test:** Issue a command with periodic `d_wr_ready` deassertion.
     Verify no data loss, correct results, and that drain resumes at `1 CL/cycle` when ready
     reasserts.
   - **Outstanding depth test:** With high-latency memory (latency > `MAX_OUTSTANDING_RD`),
     verify that outstanding request count reaches `MAX_OUTSTANDING_RD` on both A and B
     interfaces.
2. Performance tests run with `S = 4` for speed and `S = 16` for contract validation.
3. All tests report measured cycle counts and pass/fail against SPEC thresholds.

**Exit criterion:** `mill spinal101.test` passes all performance tests at S=4 and S=16.
`make flow` green.

---

## Stage 8 — Pre-PnR STA and timing closure

**Goal:** Synthesize the full `SystolicMatmul` on SKY130 and measure achieved clock speed.

**Work items:**

1. Update `Makefile`:
   - `make rtl` generates `SystolicMatmul.v` (not the old V0).
   - `make prepnr` synthesizes and runs STA on the systolic design.
   - `make flow` runs the full pipeline: test -> rtl -> prepnr -> report.
2. Update `.github/scripts/openlane_prepnr.sh` and CI workflow for the new design name if needed.
3. Run `make flow` and record baseline metrics:
   - Gate count
   - Worst slack at target period (10 ns / 100 MHz)
   - Critical path delay
   - Max achievable frequency
4. If timing is negative (expected for a 16x16 array on SKY130):
   - Identify critical path (combinational depth in FP32 mul/add, skew fanout, etc.).
   - Document findings and achievable frequency in a `TIMING.md` or update this plan.
   - Consider reducing `S` (e.g., `S = 4` or `S = 8`) for timing closure if needed.
   - **Do not** change the architecture to chase timing — document the tradeoff.

**Exit criterion:** `make flow` completes successfully. Pre-PnR metrics are recorded and
reported. CI green.

---

## Controller Pipelining Redesign (100% Utilization Track)

**Objective:** Redesign the top-level controller so the systolic array is continuously busy in
steady-state operation (no controller-induced bubbles) and command streaming is near-continuous.

**Utilization definitions (must be implemented as counters in simulation):**

- `inject_full_cycle`: cycle where all `S` A lanes and all `S` B lanes are valid at systolic input.
- `inject_window_cycle`: cycle where the controller is in active compute window (excluding cold start
  and final command drain flush).
- `array_utilization = inject_full_cycle / inject_window_cycle`.
- `stall_cause` breakdown (one-hot per cycle): `no_step`, `a_not_ready`, `b_not_ready`,
  `bank_hazard`, `drain_blocked`, `output_backpressure`, `error_flush`.

Target in no-backpressure, no-memory-latency steady-state:
- `array_utilization == 1.0` over long windows (`>= 2000` cycles).
- max contiguous idle run inside steady-state window: `<= 1` cycle.

---

## Stage 9 — Controller utilization instrumentation and baseline

**Goal:** Make utilization and bubble sources observable before redesign.

**Work items:**

1. Add cycle counters and stall-cause counters to `SystolicMatmul` (simulation-visible only).
2. Add trace points for phase boundaries (command accepted, feed start, drain start, drain done).
3. Add `UtilizationSpec` baseline test for current controller:
   - Long command stream, small primitives, zero memory latency, always-ready output.
   - Reports utilization and per-cause bubble counts.
4. Emit a machine-readable summary artifact (e.g., JSON or `metrics.env`) from the test.

**Exit criterion:** Baseline utilization and stall breakdown are reproducibly reported for `S=4`
and `S=16`.

---

## Stage 10 — Controller decomposition into pipelined engines

**Goal:** Replace monolithic serialized FSM with decoupled pipeline stages.

**Work items:**

1. Split controller into independent handshake-driven engines:
   - command dispatch
   - step generation
   - prefetch/issue
   - inject
   - drain
   - commit/status
2. Introduce in-flight context records (`cmd_id`, `pi/pj/pk/mi/nj`, `bank`, `clear`, `drain`).
3. Add bounded FIFOs between engines with credit/ready flow control.
4. Add explicit hazard scoreboard to prevent illegal bank/row conflicts.

**Exit criterion:** Functional equivalence retained (`mill spinal101.test` green), and no global
single-state bottleneck remains in controller datapath.

---

## Stage 11 — Zero-bubble A/B supply pipeline

**Goal:** Ensure next micro-tile inputs are ready before current injection ends.

**Work items:**

1. Extend read engines to accept queued descriptors while previous descriptor is still active.
2. Convert A transpose and B staging into true ping-pong buffers with independent fill/read domains.
3. Add prefetch lookahead (`>= 1` micro-tile) in scheduler-to-read path.
4. Guarantee inject stage can advance directly from tile `n` to tile `n+1` without waiting for
   `LOAD_AB`.

**Exit criterion:** Under zero-latency memory and no backpressure, inject gap between consecutive
micro-tiles is `<= 1` cycle after warm-up.

---

## Stage 12 — Drain/compute overlap with bank rotation

**Goal:** Remove drain as a blocking phase for next compute step.

**Work items:**

1. Make bank rotation explicit and deterministic each drain epoch.
2. Allow compute on active bank while previous bank drains concurrently.
3. Refactor `DrainPacker` interface to non-blocking enqueue/consume semantics.
4. Keep command-final flush as a barrier only at true end-of-command commit.

**Exit criterion:** `drain_blocked` stall cause is zero in no-backpressure tests; array keeps
feeding while drain is active.

---

## Stage 13 — Multi-command pipelining and in-order commit

**Goal:** Pipeline command stream across controller stages while preserving ordering guarantees.

**Work items:**

1. Allow next command dispatch/step/prefetch while prior command is still in drain/commit.
2. Track per-command completion tokens (`writes_done`, `drains_done`, `errors_seen`).
3. Emit status strictly in command order using commit queue.
4. Implement poison/fence error handling that flushes safely and resumes cleanly.

**Exit criterion:** Back-to-back command stream (small primitives) has no `no_step` bubbles after
warm-up, and status ordering remains correct in all tests.

---

## Stage 14 — Utilization closure and CI gating

**Goal:** Prove and continuously enforce 100% steady-state utilization target.

**Work items:**

1. Add utilization-focused tests:
   - steady-state no-latency/no-backpressure stream (`S=4`, `S=16`)
   - random memory latency with sufficient outstanding depth
   - output backpressure sensitivity sweep
2. Gate pass/fail on quantitative thresholds:
   - steady-state `array_utilization == 1.0` in ideal case
   - end-to-end stream utilization (including boundaries) `>= 0.98`
   - stall counters within strict per-cause budgets
3. Export utilization summary in CI and include trend table in workflow summary.
4. Document achieved utilization and remaining non-ideal corner cases.

**Exit criterion:** All utilization gates pass in CI for both `S=4` and `S=16`; controller redesign
is considered complete.

---

## Conventions

**Applicable to all stages:**

- Scala 2.12 compatible. No global hardware `val` definitions.
- `mill spinal101.test` must pass after every stage — no broken windows.
- Each stage is a self-contained PR or commit series. No forward dependencies.
- Test names follow `<Module>Spec` naming. Use Verilator backend only.
- Parameterize `S` in all RTL modules (default 16, test with 4 for fast iteration).
- Keep `Fp32Math.scala` as the combinational reference; pipelined versions wrap it.
