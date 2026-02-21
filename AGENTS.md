# AGENTS.md

Repository guidance for coding agents.

## Project

- Language: Scala 2.12
- HDL: SpinalHDL 1.14.1
- Build system: Mill 0.12.17
- Main module: `spinal101`

## Key Paths

- `build.mill` - build and dependency setup
- `src/matmul/Fp32Math.scala` - FP32 add/mul datapath logic
- `src/matmul/Fp32MatrixMul.scala` - parameterized matrix multiplier component
- `src/matmul/GenerateFp32MatrixMul.scala` - Verilog generation entry point
- `test/src/matmul/Fp32MatrixMulSimSpec.scala` - Scala/Spinal simulation testbench
- `README.md` - user-facing run and version notes

## Common Commands

- Run tests: `mill spinal101.test`
- Generate RTL (default 2x2): `mill spinal101.run`
- Generate RTL (example 3x3): `mill spinal101.run 3`

## Verification Expectations

- Run `mill spinal101.test` after functional RTL changes.
- Keep the testbench end-to-end: Scala testbench drives/verifies generated Verilog through Verilator.

## Constraints and Notes

- Keep code compatible with Scala 2.12.
- Avoid global hardware `val` definitions that can leak across elaborations; prefer methods for hardware literals/constants.
- Build artifacts are ignored (`out/`, `generated/`, `simWorkspace/`).
- Icarus backend is not enabled in tests in this environment; Verilator is the supported path.

## Current CI / OpenLane Status (2026-02-21 UTC)

- Repo: `https://github.com/ripopov/spinal101`
- Branch: `main`
- Latest CI-focused commit: `6756232` (`Report critical path endpoints in OpenLane CI summary`)

### Workflow state

- `.github/workflows/ci.yml` currently runs on each push/PR with:
  - `build-and-test` (Mill + Verilator tests)
  - `openlane-prepnr` (Yosys + DEF + pre-PnR STA in OpenLane container on SKY130)
  - `report` (aggregated summary)
- OpenLane metrics exported to workflow outputs:
  - `gate_count`
  - `worst_slack_ns`
  - `critical_path_ns`
  - `critical_path_delay_ns`
  - `critical_startpoint`
  - `critical_endpoint`
  - `max_frequency_mhz`

### Last known GitHub Actions results

- Latest run: `22253055097` on commit `6756232` - `success`
  - Jobs: `build-and-test`, `openlane-prepnr`, `report` all green.
  - OpenLane metrics:
    - Gate count: `6620`
    - Worst slack: `-34.73 ns`
    - Critical path estimate: `44.7300 ns`
    - Critical path delay (`report_checks`): `44.5466 ns`
    - Critical path startpoint: `_21215_ (rising edge-triggered flip-flop clocked by core_clk)`
    - Critical path endpoint: `_21138_ (rising edge-triggered flip-flop clocked by core_clk)`
    - Max frequency estimate: `22.356 MHz`
- Previous run: `22252963847` on commit `f2f8d09` - `success`
  - Metrics present for gate/slack/frequency; critical path endpoint fields were added in `6756232`.

### Debug handoff for next session

- Primary status: CI pipeline is stable and exporting complete OpenLane pre-PnR metrics, including critical path endpoints.
- Optimization starting point:
  - Startpoint net/register: `_21215_`
  - Endpoint net/register: `_21138_`
  - Inspect path in `build/openlane_prepnr/opensta_checks.rpt` and corresponding logic in `build/openlane_prepnr/Fp32MatrixMul_synth.v`.
