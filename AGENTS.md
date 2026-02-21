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
