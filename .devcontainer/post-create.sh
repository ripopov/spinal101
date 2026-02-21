#!/usr/bin/env bash
set -euo pipefail

PDK_FAMILY="${PDK_FAMILY:-sky130}"
PDK_ROOT_CACHE="${PDK_ROOT_CACHE:-./.volare-sky130}"
OPENLANE_IMAGE="${OPENLANE_IMAGE:-ghcr.io/efabless/openlane2:2.0.4}"

mkdir -p "$PDK_ROOT_CACHE"

for tool in java mill verilator python3 docker; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "Missing required tool: $tool" >&2
    exit 1
  fi
done

if ! docker info >/dev/null 2>&1; then
  echo "Docker daemon is not reachable from the devcontainer." >&2
  exit 1
fi

java_bin="$(readlink -f "$(command -v java)")"
java_home="$(dirname "$(dirname "$java_bin")")"
export JAVA_HOME="$java_home"

echo "Toolchain versions:"
java -version 2>&1 | head -n 1
echo "Mill: $(mill --version | head -n 1)"
echo "Verilator: $(verilator --version)"
echo "Python: $(python3 --version)"
echo "Volare: $(python3 -m volare --version)"
echo "Docker: $(docker --version)"

echo
echo "Ensuring SKY130 PDK revision is enabled..."
if python3 -m volare path --pdk "$PDK_FAMILY" --pdk-root "$PDK_ROOT_CACHE" "$OPEN_PDKS_REV" >/dev/null 2>&1; then
  echo "SKY130 PDK already enabled: $OPEN_PDKS_REV"
else
  python3 -m volare enable --pdk "$PDK_FAMILY" --pdk-root "$PDK_ROOT_CACHE" "$OPEN_PDKS_REV"
fi

pdk_path="$(python3 -m volare path --pdk "$PDK_FAMILY" --pdk-root "$PDK_ROOT_CACHE" "$OPEN_PDKS_REV")"

echo
echo "Pre-pulling OpenLane image: $OPENLANE_IMAGE"
docker pull "$OPENLANE_IMAGE"

cat > .devcontainer/openlane.env <<EOF
export JAVA_HOME=$java_home
export PDK_FAMILY=$PDK_FAMILY
export PDK_ROOT_CACHE=$PDK_ROOT_CACHE
export OPENLANE_PDK_ROOT=$pdk_path
EOF

env_file="$(pwd)/.devcontainer/openlane.env"
if ! grep -Fq "source $env_file" ~/.bashrc; then
  echo "source $env_file" >> ~/.bashrc
fi

cat <<'EOF'

Common commands inside this devcontainer:
  source .devcontainer/openlane.env
  make flow
  mill --no-server spinal101.test
  mill --no-server spinal101.run
  pdk_path="$OPENLANE_PDK_ROOT"
  docker run --rm -v "$PWD":"$PWD" -w "$PWD" \
    -e PDK_ROOT="$pdk_path" -e PDK="$PDK" -e SCL="$SCL" \
    -e DESIGN_NAME="Fp32MatrixMul" -e RTL_PATH="generated/Fp32MatrixMul.v" \
    "$OPENLANE_IMAGE" bash .github/scripts/openlane_prepnr.sh
EOF
