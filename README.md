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
                           +-------------------+
cmd_* --------------------> |   CmdFrontend     | -- valid cmd --> +-------------------+
                            +-------------------+                   |   TileScheduler   |
                               | reject/invalid                      +-------------------+
                               v                                              |
                           +-------------------+                              |
                           |     StatusGen      | <---- done/error -----------+
                           +-------------------+               |
                                   |                           |
                                   v                           v
                                sts_*                   +-------------------------+
                                                        |         A path          |
a_rd_req/rsp_* <--------------------------------------> | ReadEngine -> Reorder   |
                                                        | Buffer -> TransposeBuf  |
                                                        +-------------------------+
                                                                     |
                                                                     v
                                                                +----------+
                                                                | Skew A   |
                                                                +----------+

                                                        +-------------------------+
                                                        |         B path          |
b_rd_req/rsp_* <--------------------------------------> | ReadEngine -> Reorder   |
                                                        | Buffer -> FIFO          |
                                                        +-------------------------+
                                                                     |
                                                                     v
                                                                +----------+
                                                                | Skew B   |
                                                                +----------+
                                                                     |
                                                                     v
                                                        +-------------------------+
                                                        |       SystolicCore      |
                                                        |      (S x S PE array)   |
                                                        +-------------------------+
                                                                     |
                                                                     v
                                                        +-------------------------+
                                                        |       DrainPacker       |
                                                        |  + K-reduction buffer   |
                                                        +-------------------------+
                                                                     |
                                                                     v
                                                                Out FIFO
                                                                     |
                                                                     v
                                                                   d_wr_*
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
- `src/matmul/TileScheduler.scala` - primitive/micro-tile scheduling
- `src/matmul/ReadEngine.scala` - tagged request issue + response retirement
- `src/matmul/TransposeBuffer.scala` - A-path row/column transform
- `src/matmul/SkewNetwork.scala` - systolic injection skew
- `src/matmul/SystolicCore.scala` - PE array compute core
- `src/matmul/DrainPacker.scala` - drain, pack, and K-reduction
- `src/matmul/StatusGen.scala` - completion and error reporting
- `test/src/matmul/SystolicMatmulTb.scala` - integration harness
- `test/src/matmul/PerfSpec.scala` - performance contract tests

## Common Commands

```bash
# compile and run all tests
mill spinal101.compile
mill spinal101.test

# generate legacy baseline RTL (Fp32MatrixMul)
make rtl

# generate systolic RTL (default S=16)
make rtl-systolic

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

## Output Artifacts

Pre-PnR outputs are written under `build/openlane_prepnr/`:

- `summary.md` - summarized gate count / slack / frequency
- `metrics.env` - key-value metrics export
- `yosys.log` - synthesis log
- `openroad.log` - STA/floorplan log
- `opensta_checks.rpt` - timing path report
