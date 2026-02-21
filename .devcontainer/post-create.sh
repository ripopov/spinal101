#!/usr/bin/env bash
set -euo pipefail

mkdir -p .volare-sky130

echo "Toolchain versions:"
java -version 2>&1 | head -n 1
echo "Mill: $(mill --version | head -n 1)"
echo "Verilator: $(verilator --version)"
echo "Python: $(python3 --version)"
echo "Volare: $(python3 -m volare --version)"
echo "Docker: $(docker --version)"

cat <<'EOF'

Common commands inside this devcontainer:
  mill --no-server spinal101.test
  mill --no-server spinal101.run
  python3 -m volare enable --pdk sky130 --pdk-root ./.volare-sky130 "$OPEN_PDKS_REV"
  pdk_path="$(python3 -m volare path --pdk sky130 --pdk-root ./.volare-sky130 "$OPEN_PDKS_REV")"
  docker run --rm -v "$PWD":"$PWD" -w "$PWD" \
    -e PDK_ROOT="$pdk_path" -e PDK="$PDK" -e SCL="$SCL" \
    -e DESIGN_NAME="Fp32MatrixMul" -e RTL_PATH="generated/Fp32MatrixMul.v" \
    "$OPENLANE_IMAGE" bash .github/scripts/openlane_prepnr.sh
EOF
