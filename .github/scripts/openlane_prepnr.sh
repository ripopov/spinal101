#!/usr/bin/env bash
set -euo pipefail

DESIGN_NAME="${DESIGN_NAME:-Fp32MatrixMul}"
RTL_PATH="${RTL_PATH:-generated/Fp32MatrixMul.v}"
PDK_ROOT="${PDK_ROOT:?PDK_ROOT must point to a Volare PDK root}"
PDK="${PDK:-sky130A}"
SCL="${SCL:-sky130_fd_sc_hd}"
CLOCK_PORT="${CLOCK_PORT:-clk}"
TARGET_PERIOD_NS="${TARGET_PERIOD_NS:-10.0}"
WORK_DIR="${WORK_DIR:-build/openlane_prepnr}"

mkdir -p "$WORK_DIR"

if [[ ! -f "$RTL_PATH" ]]; then
  echo "RTL file not found: $RTL_PATH" >&2
  exit 1
fi

if [[ -d "$PDK_ROOT/$PDK" ]]; then
  pdk_dir="$PDK_ROOT/$PDK"
else
  pdk_dir="$(find "$PDK_ROOT" -maxdepth 8 -type d -name "$PDK" | head -n1 || true)"
fi

if [[ -z "${pdk_dir:-}" || ! -d "$pdk_dir" ]]; then
  echo "Could not locate PDK directory for $PDK under $PDK_ROOT" >&2
  exit 1
fi

liberty="$(find "$pdk_dir" -type f -name "${SCL}__tt_025C_1v80.lib" | head -n1 || true)"
tech_lef="$(find "$pdk_dir" -type f -name "${SCL}__nom.tlef" | head -n1 || true)"
cell_lef="$(find "$pdk_dir" -type f -name "${SCL}.lef" | head -n1 || true)"

for file in "$liberty" "$tech_lef" "$cell_lef"; do
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "Missing SKY130 collateral required for synthesis/STA." >&2
    echo "liberty=$liberty" >&2
    echo "tech_lef=$tech_lef" >&2
    echo "cell_lef=$cell_lef" >&2
    exit 1
  fi
done

synth_netlist="$WORK_DIR/${DESIGN_NAME}_synth.v"
def_file="$WORK_DIR/${DESIGN_NAME}_prepnr.def"
yosys_log="$WORK_DIR/yosys.log"
openroad_log="$WORK_DIR/openroad.log"
checks_rpt="$WORK_DIR/opensta_checks.rpt"
metrics_env="$WORK_DIR/metrics.env"
summary_md="$WORK_DIR/summary.md"

cat > "$WORK_DIR/synth.ys" <<EOF
read_verilog $RTL_PATH
hierarchy -check -top $DESIGN_NAME
synth -top $DESIGN_NAME
dfflibmap -liberty $liberty
abc -liberty $liberty
stat -liberty $liberty
write_verilog -noattr $synth_netlist
EOF

yosys -l "$yosys_log" -s "$WORK_DIR/synth.ys"

cat > "$WORK_DIR/prepnr_sta.tcl" <<EOF
read_lef $tech_lef
read_lef $cell_lef
read_liberty $liberty
read_verilog $synth_netlist
link_design $DESIGN_NAME

initialize_floorplan -site unithd -die_area {0 0 500 500} -core_area {10 10 490 490}
write_def $def_file

create_clock -name core_clk -period $TARGET_PERIOD_NS [get_ports $CLOCK_PORT]

report_checks -path_delay max -digits 4
report_worst_slack -max
report_design_area
exit
EOF

openroad -exit "$WORK_DIR/prepnr_sta.tcl" > "$openroad_log" 2>&1
cp "$openroad_log" "$checks_rpt"

gate_count="$(grep -E 'Number of cells:' "$yosys_log" | tail -n1 | grep -Eo '[0-9]+' | tail -n1 || true)"
if [[ -z "$gate_count" ]]; then
  gate_count="n/a"
fi

worst_slack_ns="$(grep -E 'worst slack' "$openroad_log" | tail -n1 | grep -Eo '[-]?[0-9]+([.][0-9]+)?' | tail -n1 || true)"
if [[ -z "$worst_slack_ns" ]]; then
  worst_slack_ns="n/a"
fi

critical_path_ns="n/a"
max_frequency_mhz="n/a"
if [[ "$worst_slack_ns" =~ ^-?[0-9]+([.][0-9]+)?$ ]]; then
  critical_path_ns="$(awk -v period="$TARGET_PERIOD_NS" -v slack="$worst_slack_ns" 'BEGIN {printf "%.4f", period - slack}')"
  max_frequency_mhz="$(awk -v crit="$critical_path_ns" 'BEGIN {if (crit > 0) printf "%.3f", 1000/crit; else print "n/a"}')"
fi

cat > "$metrics_env" <<EOF
gate_count=$gate_count
worst_slack_ns=$worst_slack_ns
critical_path_ns=$critical_path_ns
max_frequency_mhz=$max_frequency_mhz
target_period_ns=$TARGET_PERIOD_NS
synth_netlist=$synth_netlist
def_file=$def_file
EOF

cat > "$summary_md" <<EOF
## OpenLane Pre-PnR Metrics (SKY130)

| Metric | Value |
| --- | --- |
| Gate count (std cells) | $gate_count |
| Target clock period | ${TARGET_PERIOD_NS} ns |
| Worst slack | ${worst_slack_ns} ns |
| Critical path estimate | ${critical_path_ns} ns |
| Max frequency estimate | ${max_frequency_mhz} MHz |

Artifacts:
- Synthesized netlist: \`$synth_netlist\`
- DEF: \`$def_file\`
- Yosys log: \`$yosys_log\`
- OpenSTA checks report: \`$checks_rpt\`
- OpenROAD log: \`$openroad_log\`
EOF
