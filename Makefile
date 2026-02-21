SHELL := /usr/bin/env bash
.SHELLFLAGS := -eu -o pipefail -c

MILL ?= $(if $(wildcard ./mill),./mill,mill)
MILL_NO_SERVER ?= --no-server

OPENLANE_IMAGE ?= ghcr.io/efabless/openlane2:2.0.4
OPEN_PDKS_REV ?= 0fe599b2afb6708d281543108caf8310912f54af
PDK ?= sky130A
SCL ?= sky130_fd_sc_hd
PDK_FAMILY ?= sky130
PDK_ROOT_CACHE ?= ./.volare-sky130
OPENLANE_PDK_ROOT ?=

DESIGN_NAME ?= SystolicMatmul
RTL_PATH ?= generated/SystolicMatmul.v
WORK_DIR ?= build/openlane_prepnr

CLOCK_PORT ?= clk
TARGET_PERIOD_NS ?= 10.0
SYSTOLIC_S ?= 4
PREPNR_S ?= 4

.PHONY: help build test rtl rtl-systolic rtl-v0 prepnr report flow clean-prepnr

help:
	@echo "Targets:"
	@echo "  make test         - Run Scala/Verilator testbench"
	@echo "  make rtl          - Generate SystolicMatmul RTL (SYSTOLIC_S)"
	@echo "  make rtl-systolic - Alias for make rtl"
	@echo "  make rtl-v0       - Generate legacy Fp32MatrixMulV0 RTL (S=2 default)"
	@echo "  make prepnr       - Run OpenLane pre-PnR synthesis/STA flow"
	@echo "  make report       - Print timing/gate-count summary"
	@echo "  make flow         - End-to-end: test + rtl + prepnr + report"
	@echo ""
	@echo "Key variables (override as needed):"
	@echo "  MILL=$(MILL)"
	@echo "  OPENLANE_IMAGE=$(OPENLANE_IMAGE)"
	@echo "  OPEN_PDKS_REV=$(OPEN_PDKS_REV)"
	@echo "  TARGET_PERIOD_NS=$(TARGET_PERIOD_NS)"
	@echo "  SYSTOLIC_S=$(SYSTOLIC_S)"
	@echo "  PREPNR_S=$(PREPNR_S)"

build:
	$(MILL) $(MILL_NO_SERVER) spinal101.compile

test:
	$(MILL) $(MILL_NO_SERVER) spinal101.test

rtl:
	$(MILL) $(MILL_NO_SERVER) spinal101.runMain matmul.GenerateSystolicMatmul $(SYSTOLIC_S)

rtl-systolic:
	@$(MAKE) rtl SYSTOLIC_S="$(SYSTOLIC_S)"

rtl-v0:
	$(MILL) $(MILL_NO_SERVER) spinal101.runMain matmul.GenerateFp32MatrixMul

prepnr: SYSTOLIC_S := $(PREPNR_S)
prepnr: rtl

prepnr:
	@pdk_path="$(OPENLANE_PDK_ROOT)"; \
	if [[ -z "$$pdk_path" ]]; then \
		pdk_path="$$(python3 -m volare path --pdk $(PDK_FAMILY) --pdk-root $(PDK_ROOT_CACHE) $(OPEN_PDKS_REV))"; \
	fi; \
	echo "Using PDK path: $$pdk_path"; \
	chmod +x .github/scripts/openlane_prepnr.sh; \
	if docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" "$(OPENLANE_IMAGE)" true >/dev/null 2>&1; then \
		echo "Running OpenLane with bind mount"; \
		docker run --rm \
			-v "$$PWD":"$$PWD" \
			-w "$$PWD" \
			-e PDK_ROOT="$$pdk_path" \
			-e PDK="$(PDK)" \
			-e SCL="$(SCL)" \
			-e DESIGN_NAME="$(DESIGN_NAME)" \
			-e RTL_PATH="$(RTL_PATH)" \
			-e CLOCK_PORT="$(CLOCK_PORT)" \
			-e TARGET_PERIOD_NS="$(TARGET_PERIOD_NS)" \
			-e WORK_DIR="$(WORK_DIR)" \
			"$(OPENLANE_IMAGE)" \
			bash .github/scripts/openlane_prepnr.sh; \
	else \
		echo "Bind mount unavailable, running OpenLane using docker cp fallback"; \
		container_name="openlane-prepnr-$$(date +%s)-$$RANDOM"; \
		container_pdk_root="/workspace/$(PDK_ROOT_CACHE)/volare/$(PDK_FAMILY)/versions/$(OPEN_PDKS_REV)"; \
		docker create --name "$$container_name" \
			-w /workspace \
			-e PDK_ROOT="$$container_pdk_root" \
			-e PDK="$(PDK)" \
			-e SCL="$(SCL)" \
			-e DESIGN_NAME="$(DESIGN_NAME)" \
			-e RTL_PATH="$(RTL_PATH)" \
			-e CLOCK_PORT="$(CLOCK_PORT)" \
			-e TARGET_PERIOD_NS="$(TARGET_PERIOD_NS)" \
			-e WORK_DIR="$(WORK_DIR)" \
			"$(OPENLANE_IMAGE)" \
			bash .github/scripts/openlane_prepnr.sh >/dev/null; \
		docker cp .github "$$container_name":/workspace/.github; \
		docker cp generated "$$container_name":/workspace/generated; \
		docker cp "$(PDK_ROOT_CACHE)" "$$container_name":/workspace/$(PDK_ROOT_CACHE); \
		docker start -a "$$container_name"; \
		rm -rf "$(WORK_DIR)"; \
		mkdir -p "$$(dirname "$(WORK_DIR)")"; \
		docker cp "$$container_name":/workspace/"$(WORK_DIR)" "$(WORK_DIR)"; \
		docker rm "$$container_name" >/dev/null; \
	fi

report:
	@if [[ ! -f "$(WORK_DIR)/summary.md" ]]; then \
		echo "Summary not found: $(WORK_DIR)/summary.md"; \
		echo "Run 'make prepnr' first."; \
		exit 1; \
	fi
	@cat "$(WORK_DIR)/summary.md"

flow: test prepnr report

clean-prepnr:
	rm -rf "$(WORK_DIR)"
