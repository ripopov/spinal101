# FP32 Matrix Multiplier in SpinalHDL

This project implements a parameterized IEEE-754 single-precision (FP32) matrix multiplier in SpinalHDL (default 2x2), built with Mill.

## Online version research

Versions were selected from Maven metadata (queried online):

- SpinalHDL core/lib/sim/plugin: `1.14.1`
- Scala (2.12 line, compatible with Spinal artifacts): `2.12.21`
- Mill: `0.12.17`

Reference URLs used:

- `https://repo1.maven.org/maven2/com/github/spinalhdl/spinalhdl-core_2.12/maven-metadata.xml`
- `https://repo1.maven.org/maven2/org/scala-lang/scala-library/maven-metadata.xml`
- `https://repo1.maven.org/maven2/com/lihaoyi/mill-scalalib_2.13/maven-metadata.xml`
- `https://raw.githubusercontent.com/SpinalHDL/SpinalDoc-RTD/master/source/SpinalHDL/Data%20types/Floating.rst`

Because Spinal floating support is still documented as partial/experimental, this project implements FP32 add/mul datapaths directly in RTL logic (`Fp32Math.scala`).

## Project layout

- `build.mill` - Mill build definition
- `src/matmul/Fp32Math.scala` - FP32 add/mul combinational datapath
- `src/matmul/Fp32MatrixMul.scala` - matrix multiply hardware
- `src/matmul/GenerateFp32MatrixMul.scala` - Verilog generator entry point
- `test/src/matmul/Fp32MatrixMulSimSpec.scala` - single Scala testbench running both Icarus and Verilator

## Run

Generate Verilog:

```bash
mill spinal101.run

# optional: generate 3x3 variant
mill spinal101.run 3
```

Run full end-to-end simulation tests (same Scala testbench, Verilator backend):

```bash
mill spinal101.test
```

The testbench executes:

1. Directed vector tests
2. Random matrix tests
3. Verilated backend (`SpinalSimConfig().withVerilator`)

Expected behavior: all tests pass with numeric agreement versus a Scala FP32 golden model.
