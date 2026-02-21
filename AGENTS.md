# AGENTS.md

Repository guidance for coding agents.

## Project

- Language: Scala 2.12
- HDL: SpinalHDL 1.14.1
- Build system: Mill 0.12.17
- Main module: `spinal101`

## Staged Implementation Plan

The project follows `STAGED_PLAN.md` — a staged build-up of a systolic FP32 matrix multiplier
defined by `SPEC.md`.

### "Implement next stage" protocol

When the user says **"implement next stage"** (or "next step", "next stage"), do the following:

1. **Read `STAGED_PLAN.md`** and find the first unchecked stage (`- [ ]`).
2. **Implement all work items** listed under that stage. Follow the exit criteria exactly.
3. **Run `mill spinal101.test`** — all tests must pass.
4. **Run `mill spinal101.compile`** — must succeed.
5. **If the stage requires RTL generation**, run the appropriate `make rtl` / `make rtl-systolic`
   target and verify it produces Verilog without errors.
6. **Update `STAGED_PLAN.md`**: change the completed stage's checkbox from `- [ ]` to `- [x]`.
7. **Commit all changes** (source, tests, and updated plan) with a descriptive message.
8. **Do not push** unless the user explicitly asks.

If a stage fails its exit criteria, fix the issue before marking it complete. Never check a box
for a stage that does not pass its tests.

## Key Paths

- `SPEC.md` - architecture specification (v1.1-draft)
- `STAGED_PLAN.md` - staged implementation plan with progress checkboxes
- `build.mill` - build and dependency setup
- `src/matmul/Fp32Math.scala` - FP32 add/mul datapath logic
- `src/matmul/Fp32MatrixMul.scala` - parameterized matrix multiplier component
- `src/matmul/GenerateFp32MatrixMul.scala` - Verilog generation entry point
- `test/src/matmul/Fp32MatrixMulSimSpec.scala` - Scala/Spinal simulation testbench
- `README.md` - user-facing run and version notes

## Common Commands

- Run tests: `mill spinal101.test`
- Compile only: `mill spinal101.compile`
- Generate RTL (default 2x2): `mill spinal101.run`
- Generate RTL (example 3x3): `mill spinal101.run 3`
- Full flow (test + rtl + synthesis + report): `make flow`

## Verification Expectations

- Run `mill spinal101.test` after functional RTL changes.
- Keep the testbench end-to-end: Scala testbench drives/verifies generated Verilog through Verilator.

## Constraints and Notes

- Keep code compatible with Scala 2.12.
- Avoid global hardware `val` definitions that can leak across elaborations; prefer methods for hardware literals/constants.
- Build artifacts are ignored (`out/`, `generated/`, `simWorkspace/`).
- Icarus backend is not enabled in tests in this environment; Verilator is the supported path.
- Parameterize `S` in all new RTL modules (default 16, test with 4 for fast simulation).

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
