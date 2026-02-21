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
- Latest CI-focused commit: `1c6b9d5` (`Use grep-based metric extraction for OpenLane reports`)

### Workflow state

- `.github/workflows/ci.yml` currently runs on each push/PR with:
  - `build-and-test` (Mill + Verilator tests)
  - `openlane-prepnr` (Yosys + DEF + pre-PnR STA in OpenLane container on SKY130)
  - `report` (aggregated summary)

### Last known GitHub Actions results

- Failed: run `22252338545` on commit `1c6b9d5`
  - Job: `openlane-prepnr`
  - Step: `Run OpenLane Pre-PnR Flow`
  - Error: `.github/scripts/openlane_prepnr.sh: line 99: awk: command not found`
  - Exit code: `127`
- Last fully green run: `22252293910` on commit `db7dde3`
  - Note: jobs passed, but summary metrics were still `n/a` (parsing issue).

### Debug handoff for next session

- Primary file to fix: `.github/scripts/openlane_prepnr.sh`
- Likely next change: replace `awk`-dependent metric parsing with tooling guaranteed in container (portable `sed` or `python3`).
- After fix: push to `main`, then verify run `CI` has all jobs green and exports:
  - test status
  - gate count
  - worst slack / max frequency
