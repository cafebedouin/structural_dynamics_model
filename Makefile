# ==============================================================================
# Structural Dynamics Model — Dependency-aware Pipeline
# ==============================================================================
#
# Replaces run_full_pipeline.sh with a Makefile that expresses actual file
# dependencies, enabling parallel execution via `make -j`.
#
# Usage:
#   make              # full pipeline + quality gates + dashboard
#   make -j4          # parallel (4 workers)
#   make quick        # pipeline + dashboard, skip quality gates
#   make check        # quality gates only (linter + duplicate checker)
#   make lint-fix     # auto-fix AI-generated Prolog artifacts
#   make reports      # category reports + omega + enriched omega + meta
#   make prolog-analysis  # all Prolog analysis steps
#   make clean        # remove all generated outputs
#
# ==============================================================================

SHELL := /bin/bash
.SHELLFLAGS := -euo pipefail -c

# Directories
ROOT_DIR   := $(CURDIR)
PYTHON_DIR := $(ROOT_DIR)/python
PROLOG_DIR := $(ROOT_DIR)/prolog
TESTSETS   := $(PROLOG_DIR)/testsets
OUTPUT_DIR := $(ROOT_DIR)/outputs
STAMP_DIR  := $(ROOT_DIR)/.stamps

# Silence makes default echoing; recipes use @
.SILENT:

# ==============================================================================
# Phony targets
# ==============================================================================
.PHONY: all dashboard quick check lint-fix reports prolog-analysis clean golden-check schema-check

all: dashboard

# Testset files — PREP rebuilds when testsets change
TESTSET_FILES := $(wildcard $(TESTSETS)/*.pl)

# ==============================================================================
# Output files — declared for readability
# ==============================================================================

# Prep stamp (no single output file)
PREP_STAMP := $(STAMP_DIR)/prep

# Prolog tests
OUTPUT_TXT := $(OUTPUT_DIR)/output.txt

# JSON report
PIPELINE_JSON := $(OUTPUT_DIR)/pipeline_output.json
ENRICHED_PIPELINE_JSON := $(OUTPUT_DIR)/enriched_pipeline.json

# Linter
LINT_ERRORS := $(OUTPUT_DIR)/lint_errors.txt

# Prolog analysis reports (direct output, no raw→clean needed)
FINGERPRINT_REPORT   := $(OUTPUT_DIR)/fingerprint_report.md
COVERING_REPORT      := $(OUTPUT_DIR)/covering_analysis.md
GC_REPORT            := $(OUTPUT_DIR)/giant_component_analysis.md
CP_REPORT            := $(OUTPUT_DIR)/coupling_protocol.md
MAXDIAG_REPORT       := $(OUTPUT_DIR)/maxent_diagnostic_report.md

# Prolog analysis reports (raw→clean preamble stripping)
ORBIT_STAMP          := $(STAMP_DIR)/orbit
FPN_REPORT           := $(OUTPUT_DIR)/fpn_report.md
MAXENT_REPORT        := $(OUTPUT_DIR)/maxent_report.md
ABDUCTIVE_REPORT     := $(OUTPUT_DIR)/abductive_report.md
TRAJECTORY_REPORT    := $(OUTPUT_DIR)/trajectory_report.md

# Orbit normalization stamp
ORBIT_NORM_STAMP     := $(STAMP_DIR)/orbit_norm

# Category reports (Python, depend on pipeline_output.json + orbit_norm)
SNARE_REPORT         := $(OUTPUT_DIR)/snare_report.md
ROPE_REPORT          := $(OUTPUT_DIR)/rope_report.md
SCAFFOLD_REPORT      := $(OUTPUT_DIR)/scaffold_report.md
PITON_REPORT         := $(OUTPUT_DIR)/piton_report.md
MOUNTAIN_REPORT      := $(OUTPUT_DIR)/true_mountain_report.md
TANGLED_ROPE_REPORT  := $(OUTPUT_DIR)/tangled_rope_report.md
FALSE_MOUNTAIN_REPORT := $(OUTPUT_DIR)/false_mountain_report.md

TYPE_REPORTS := $(SNARE_REPORT) $(ROPE_REPORT) $(SCAFFOLD_REPORT) \
                $(PITON_REPORT) $(MOUNTAIN_REPORT) \
                $(TANGLED_ROPE_REPORT) $(FALSE_MOUNTAIN_REPORT)

# Omega
OMEGA_DATA    := $(OUTPUT_DIR)/omega_data.json

# Corpus
CORPUS_DATA   := $(OUTPUT_DIR)/corpus_data.json

# Python analysis (depend on corpus_data.json)
VARIANCE_REPORT    := $(OUTPUT_DIR)/variance_analysis.md
PATTERN_REPORT     := $(OUTPUT_DIR)/pattern_mining.md
SUFFICIENCY_REPORT := $(OUTPUT_DIR)/index_sufficiency.md
SUFFICIENCY_JSON   := $(OUTPUT_DIR)/index_sufficiency.json

# Omega enrichment stamp
OMEGA_ENRICH_STAMP := $(STAMP_DIR)/omega_enrich

# Pipeline enrichment stamp
ENRICH_STAMP := $(STAMP_DIR)/enrich_pipeline

# Tangled rope decomposition (report only, data lives in enriched_pipeline.json)
TANGLED_DECOMP_REPORT := $(OUTPUT_DIR)/tangled_rope_decomposition_report.md

# Classification confidence analysis (report only, data lives in enriched_pipeline.json)
CONFIDENCE_REPORT := $(OUTPUT_DIR)/classification_confidence_report.md

# Boundary normality analysis
BOUNDARY_NORM_DATA   := $(OUTPUT_DIR)/boundary_normality_data.json
BOUNDARY_NORM_REPORT := $(OUTPUT_DIR)/boundary_normality_report.md

# Boolean feature independence analysis
BOOLEAN_INDEP_DATA   := $(OUTPUT_DIR)/boolean_independence_data.json
BOOLEAN_INDEP_REPORT := $(OUTPUT_DIR)/boolean_independence_report.md

# Institutional dissent analysis
INST_DISSENT_DATA   := $(OUTPUT_DIR)/institutional_dissent_data.json
INST_DISSENT_REPORT := $(OUTPUT_DIR)/institutional_dissent_report.md

# Meta report
META_REPORT := $(OUTPUT_DIR)/meta_report.txt

# Golden-file classification baseline
GOLDEN_FILE := $(OUTPUT_DIR)/golden_classifications.json

# ==============================================================================
# Aggregate targets
# ==============================================================================

PROLOG_ANALYSES := $(FINGERPRINT_REPORT) $(ORBIT_STAMP) $(FPN_REPORT) \
                   $(MAXENT_REPORT) $(ABDUCTIVE_REPORT) $(TRAJECTORY_REPORT) \
                   $(COVERING_REPORT) $(GC_REPORT) $(CP_REPORT) $(MAXDIAG_REPORT)

PIPELINE_OUTPUTS := $(TYPE_REPORTS) $(OMEGA_DATA) $(OMEGA_ENRICH_STAMP) \
                    $(CORPUS_DATA) $(VARIANCE_REPORT) $(PATTERN_REPORT) $(SUFFICIENCY_REPORT) \
                    $(ENRICH_STAMP) $(TANGLED_DECOMP_REPORT) $(CONFIDENCE_REPORT) $(BOUNDARY_NORM_DATA) \
                    $(BOOLEAN_INDEP_DATA) \
                    $(INST_DISSENT_DATA) \
                    $(PROLOG_ANALYSES) $(ORBIT_NORM_STAMP) \
                    $(OUTPUT_TXT) $(PIPELINE_JSON) $(META_REPORT)

dashboard: $(PIPELINE_OUTPUTS) $(LINT_ERRORS)
	echo ""; echo "Running dashboard..."; echo ""
	bash scripts/pipeline_dashboard.sh

# Quick build: pipeline + dashboard, no quality gates (linter, duplicate check)
quick: $(PIPELINE_OUTPUTS)
	echo ""; echo "Running dashboard..."; echo ""
	bash scripts/pipeline_dashboard.sh

# Quality gates only
check: $(LINT_ERRORS)
	echo "[CHECK] Running duplicate checker..."
	python3 $(PYTHON_DIR)/duplicate_checker.py --dir $(TESTSETS) 2>&1 || echo "[WARN] duplicate_checker failed (non-critical, continuing)"
	echo "[CHECK] Done."

# Golden-file classification regression check (requires pipeline_output.json)
golden-check: $(PIPELINE_JSON)
	echo "[GOLDEN] Checking classifications against baseline..."
	python3 $(PYTHON_DIR)/golden_file_check.py
	echo "[GOLDEN] Done."

# Schema validation for pipeline_output.json
schema-check: $(PIPELINE_JSON)
	echo "[SCHEMA] Validating pipeline_output.json schema..."
	python3 -c "import json, sys; sys.path.insert(0, '$(PYTHON_DIR)'); from shared.schemas import validate_pipeline_output; data = json.load(open('$(PIPELINE_JSON)')); errors = validate_pipeline_output(data); [print(e, file=sys.stderr) for e in errors]; sys.exit(1 if errors else 0)"
	echo "[SCHEMA] Done."

# Auto-fix AI-generated Prolog artifacts (writes to testset files)
lint-fix:
	echo "[FIX] Running Prolog cleaner..."
	python3 $(PYTHON_DIR)/prolog_cleaner.py $(TESTSETS)/
	echo "[FIX] Done."

reports: $(TYPE_REPORTS) $(OMEGA_DATA) $(OMEGA_ENRICH_STAMP) $(META_REPORT)

prolog-analysis: $(PROLOG_ANALYSES)

# ==============================================================================
# PREP — generate build artifacts from testsets (reruns when testsets change)
# ==============================================================================

$(PREP_STAMP): $(TESTSET_FILES) | $(STAMP_DIR) $(OUTPUT_DIR)
	echo "[PREP] Preparing test suite..."
	python3 $(PYTHON_DIR)/domain_priors.py --input $(TESTSETS)/ --output $(PROLOG_DIR)/domain_registry.pl > /dev/null 2>&1
	python3 $(PYTHON_DIR)/python_test_suite.py > /dev/null 2>&1
	echo "[PREP] Done."
	touch $@

$(STAMP_DIR) $(OUTPUT_DIR):
	mkdir -p $@

# ==============================================================================
# PROLOG TESTS → output.txt
# ==============================================================================

$(OUTPUT_TXT): $(PREP_STAMP)
	echo "[TEST] Running Prolog test suite..."
	echo "Initializing Validation Suite - $$(date)" > $@
	echo "------------------------------------------" >> $@
	(cd $(PROLOG_DIR) && swipl -g "[validation_suite], run_dynamic_suite, halt.") >> $@ 2>&1 || echo "[WARN] validation_suite failed (non-critical, continuing)"
	echo "------------------------------------------" >> $@
	echo "Test suite completed at: $$(date)" >> $@
	echo "[TEST] Done -> output.txt"

# ==============================================================================
# JSON REPORT → pipeline_output.json
# ==============================================================================

$(PIPELINE_JSON): $(PREP_STAMP) $(ABDUCTIVE_REPORT)
	echo "[JSON] Generating structured JSON report..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl -l dirac_classification.pl -l diagnostic_summary.pl -l post_synthesis.pl -l json_report.pl -g "run_json_report, halt.") 2>/dev/null
	echo "[JSON] Done -> pipeline_output.json"

# ==============================================================================
# LINTER → lint_errors.txt
# ==============================================================================

$(LINT_ERRORS): $(PREP_STAMP)
	echo "[LINT] Running structural linter..."
	> $@
	for pl_file in $(TESTSETS)/*.pl; do \
		[ -f "$$pl_file" ] || continue; \
		filename=$$(basename "$$pl_file"); \
		result=$$(python3 -c "import sys; sys.path.insert(0, '$(PYTHON_DIR)'); from linter import lint_file; errors = lint_file('$$pl_file'); [print(e) for e in (errors or [])]; sys.exit(1 if errors else 0)" 2>&1) || { \
			echo "$$filename:" >> $@; \
			echo "$$result" | while IFS= read -r line; do echo "  $$line" >> $@; done; \
			echo "" >> $@; \
		}; \
	done
	echo "[LINT] Done -> lint_errors.txt"

# ==============================================================================
# PROLOG ANALYSIS — independent, all depend only on prep
# ==============================================================================

# --- Fingerprint (direct output) ---
$(FINGERPRINT_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Fingerprint analysis..."
	(cd $(PROLOG_DIR) && swipl -g "[fingerprint_report], halt.") > $@ 2>/dev/null || echo "[WARN] fingerprint_report failed (non-critical, continuing)"
	echo "[PROLOG] Done -> fingerprint_report.md"

# --- Orbit (produces orbit_report.md + orbit_data.json, stamp target) ---
$(ORBIT_STAMP): $(PREP_STAMP) | $(STAMP_DIR)
	echo "[PROLOG] Orbit analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
		-l orbit_report.pl -g "run_orbit_report, halt.") > $(OUTPUT_DIR)/orbit_report_raw.md 2>/dev/null
	if [ -s $(OUTPUT_DIR)/orbit_report_raw.md ]; then \
		sed -n '/<!-- ORBIT_REPORT_START -->/,$$p' $(OUTPUT_DIR)/orbit_report_raw.md | tail -n +2 > $(OUTPUT_DIR)/orbit_report.md; \
	else \
		> $(OUTPUT_DIR)/orbit_report.md; \
	fi
	rm -f $(OUTPUT_DIR)/orbit_report_raw.md
	touch $@
	echo "[PROLOG] Done -> orbit_report.md + orbit_data.json"

# --- FPN (raw→clean) ---
$(FPN_REPORT): $(PREP_STAMP)
	echo "[PROLOG] FPN analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l fpn_report.pl \
		-g "run_fpn_report, halt.") > $(OUTPUT_DIR)/fpn_report_raw.md 2>/dev/null || echo "[WARN] fpn_report failed (non-critical, continuing)"
	if [ -s $(OUTPUT_DIR)/fpn_report_raw.md ]; then \
		sed -n '/<!-- FPN_REPORT_START -->/,$$p' $(OUTPUT_DIR)/fpn_report_raw.md | tail -n +2 > $@; \
	else \
		> $@; \
	fi
	rm -f $(OUTPUT_DIR)/fpn_report_raw.md
	echo "[PROLOG] Done -> fpn_report.md"

# --- MaxEnt (raw→clean) ---
$(MAXENT_REPORT): $(PREP_STAMP)
	echo "[PROLOG] MaxEnt analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
		-l maxent_classifier.pl -l maxent_report.pl \
		-g "run_maxent_report, halt.") > $(OUTPUT_DIR)/maxent_report_raw.md 2>/dev/null || echo "[WARN] maxent_report failed (non-critical, continuing)"
	if [ -s $(OUTPUT_DIR)/maxent_report_raw.md ]; then \
		sed -n '/<!-- MAXENT_REPORT_START -->/,$$p' $(OUTPUT_DIR)/maxent_report_raw.md | tail -n +2 > $@; \
	else \
		> $@; \
	fi
	rm -f $(OUTPUT_DIR)/maxent_report_raw.md
	echo "[PROLOG] Done -> maxent_report.md"

# --- Abductive (raw→clean) ---
$(ABDUCTIVE_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Abductive reasoning analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl \
		-l dirac_classification.pl -l maxent_classifier.pl \
		-l abductive_engine.pl -l abductive_report.pl \
		-g "run_abductive_report, halt.") > $(OUTPUT_DIR)/abductive_report_raw.md 2>/dev/null || echo "[WARN] abductive_report failed (non-critical, continuing)"
	if [ -s $(OUTPUT_DIR)/abductive_report_raw.md ]; then \
		sed -n '/<!-- ABDUCTIVE_REPORT_START -->/,$$p' $(OUTPUT_DIR)/abductive_report_raw.md | tail -n +2 > $@; \
	else \
		> $@; \
	fi
	rm -f $(OUTPUT_DIR)/abductive_report_raw.md
	echo "[PROLOG] Done -> abductive_report.md"

# --- Trajectory (conditional on config) ---
$(TRAJECTORY_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Trajectory mining..."
	TRAJ_ENABLED=$$(cd $(PROLOG_DIR) && swipl -g "use_module(config), (config:param(trajectory_enabled, 1) -> write(1) ; write(0)), halt." 2>/dev/null || echo "0"); \
	if [ "$$TRAJ_ENABLED" = "1" ]; then \
		(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
			-l maxent_classifier.pl -l trajectory_mining.pl \
			-l trajectory_report.pl -g "run_trajectory_report, halt.") > $(OUTPUT_DIR)/trajectory_report_raw.md 2>/dev/null || echo "[WARN] trajectory_report failed (non-critical, continuing)"; \
		if [ -s $(OUTPUT_DIR)/trajectory_report_raw.md ]; then \
			sed -n '/<!-- TRAJECTORY_REPORT_START -->/,$$p' $(OUTPUT_DIR)/trajectory_report_raw.md | tail -n +2 > $@; \
		else \
			> $@; \
		fi; \
		rm -f $(OUTPUT_DIR)/trajectory_report_raw.md; \
		echo "[PROLOG] Done -> trajectory_report.md"; \
	else \
		echo "[PROLOG] Trajectory mining disabled (trajectory_enabled=0)"; \
		touch $@; \
	fi

# --- Covering analysis (direct output) ---
$(COVERING_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Covering analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl \
		-g "run_covering_analysis, halt.") > $@ 2>/dev/null || echo "[WARN] covering_analysis failed (non-critical, continuing)"
	echo "[PROLOG] Done -> covering_analysis.md"

# --- Giant component (direct output) ---
$(GC_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Giant component analysis..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l giant_component_analysis.pl \
		-g "run_giant_component_analysis, halt.") > $@ 2>/dev/null || echo "[WARN] giant_component_analysis failed (non-critical, continuing)"
	echo "[PROLOG] Done -> giant_component_analysis.md"

# --- Coupling protocol (direct output) ---
$(CP_REPORT): $(PREP_STAMP)
	echo "[PROLOG] Coupling protocol..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l inferred_coupling_protocol.pl \
		-g "run_coupling_protocol, halt.") > $@ 2>/dev/null || echo "[WARN] coupling_protocol failed (non-critical, continuing)"
	echo "[PROLOG] Done -> coupling_protocol.md"

# --- MaxEnt diagnostic (direct output) ---
$(MAXDIAG_REPORT): $(PREP_STAMP)
	echo "[PROLOG] MaxEnt diagnostic..."
	(cd $(PROLOG_DIR) && swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
		-l dirac_classification.pl -l maxent_diagnostic.pl \
		-g "run_maxent_diagnostic, halt.") > $@ 2>/dev/null || echo "[WARN] maxent_diagnostic failed (non-critical, continuing)"
	echo "[PROLOG] Done -> maxent_diagnostic_report.md"

# ==============================================================================
# ORBIT NORMALIZATION — depends on orbit stamp (orbit_data.json exists)
# ==============================================================================

$(ORBIT_NORM_STAMP): $(ORBIT_STAMP) | $(STAMP_DIR)
	echo "[NORM] Normalizing orbit data IDs..."
	if [ -f $(OUTPUT_DIR)/orbit_data.json ]; then \
		python3 $(PYTHON_DIR)/normalize_orbit_ids.py 2>&1; \
	fi
	touch $@
	echo "[NORM] Done."

# ==============================================================================
# CATEGORY REPORTS — depend on pipeline_output.json + orbit_norm
# ==============================================================================

# Family A: type_reporter.py --type <category>
$(SNARE_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating snare report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type snare 2>&1 || echo "[WARN] snare_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> snare_report.md"

$(ROPE_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating rope report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type rope 2>&1 || echo "[WARN] rope_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> rope_report.md"

$(SCAFFOLD_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating scaffold report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type scaffold 2>&1 || echo "[WARN] scaffold_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> scaffold_report.md"

$(PITON_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating piton report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type piton 2>&1 || echo "[WARN] piton_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> piton_report.md"

$(MOUNTAIN_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating true_mountain report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type mountain 2>&1 || echo "[WARN] true_mountain_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> true_mountain_report.md"

$(TANGLED_ROPE_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating tangled_rope report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type tangled_rope 2>&1 || echo "[WARN] tangled_rope_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> tangled_rope_report.md"

$(FALSE_MOUNTAIN_REPORT): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[REPORT] Generating false_mountain report..."
	python3 $(PYTHON_DIR)/type_reporter.py --type false_mountain 2>&1 || echo "[WARN] false_mountain_report failed (non-critical, continuing)"
	echo "[REPORT] Done -> false_mountain_report.md"

# ==============================================================================
# OMEGA — depends on pipeline_output.json only
# ==============================================================================

# omega_reporter.py produces both omega_report.md and omega_data.json
$(OMEGA_DATA): $(PIPELINE_JSON)
	echo "[OMEGA] Generating omega report..."
	python3 $(PYTHON_DIR)/omega_reporter.py 2>&1 || echo "[WARN] omega_reporter failed (non-critical, continuing)"
	echo "[OMEGA] Done -> omega_report.md + omega_data.json"

# ==============================================================================
# CORPUS DATA — depends on pipeline_output.json + orbit_norm
# ==============================================================================

$(CORPUS_DATA): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP)
	echo "[CORPUS] Extracting corpus data..."
	python3 $(PYTHON_DIR)/extract_corpus_data.py \
		--output-txt $(OUTPUT_TXT) \
		--json-output $@ 2>&1 | tail -5
	echo "[CORPUS] Done -> corpus_data.json"

# ==============================================================================
# PYTHON ANALYSIS — depend on corpus_data.json
# ==============================================================================

$(VARIANCE_REPORT): $(CORPUS_DATA)
	echo "[ANALYSIS] Variance analysis..."
	python3 $(PYTHON_DIR)/variance_analyzer.py \
		--corpus-data $(CORPUS_DATA) --output $@ 2>&1 | tail -1
	echo "[ANALYSIS] Done -> variance_analysis.md"

$(PATTERN_REPORT): $(CORPUS_DATA)
	echo "[ANALYSIS] Pattern mining..."
	python3 $(PYTHON_DIR)/pattern_miner.py \
		--corpus-data $(CORPUS_DATA) --output $@ 2>&1 | tail -1
	echo "[ANALYSIS] Done -> pattern_mining.md"

$(SUFFICIENCY_REPORT): $(CORPUS_DATA) $(PIPELINE_JSON)
	echo "[ANALYSIS] Index sufficiency test..."
	python3 $(PYTHON_DIR)/sufficiency_tester.py \
		--corpus-data $(CORPUS_DATA) --pipeline-data $(PIPELINE_JSON) \
		--output $@ --json-output $(SUFFICIENCY_JSON) 2>&1 | tail -1
	echo "[ANALYSIS] Done -> index_sufficiency.md + index_sufficiency.json"

# ==============================================================================
# PIPELINE ENRICHMENT — produces enriched_pipeline.json from pipeline_output.json
# ==============================================================================

$(ENRICH_STAMP): $(PIPELINE_JSON) $(ORBIT_NORM_STAMP) $(ABDUCTIVE_REPORT) | $(STAMP_DIR)
	echo "[ENRICH] Producing enriched_pipeline.json..."
	python3 $(PYTHON_DIR)/enrich_pipeline_json.py 2>&1
	touch $@
	echo "[ENRICH] Done -> enriched_pipeline.json"

# ==============================================================================
# TANGLED ROPE DECOMPOSITION — depends on enriched pipeline + corpus_data
# ==============================================================================

$(TANGLED_DECOMP_REPORT): $(ENRICH_STAMP) $(CORPUS_DATA)
	echo "[TANGLED] Running tangled rope decomposition..."
	python3 $(PYTHON_DIR)/tangled_decomposition.py 2>&1 || echo "[WARN] tangled_decomposition failed (non-critical, continuing)"
	echo "[TANGLED] Done -> tangled_rope_decomposition_report.md"

# ==============================================================================
# CLASSIFICATION CONFIDENCE — depends on enriched pipeline + corpus_data
# ==============================================================================

$(CONFIDENCE_REPORT): $(ENRICH_STAMP) $(CORPUS_DATA)
	echo "[CONFIDENCE] Running classification confidence analysis..."
	python3 $(PYTHON_DIR)/classification_confidence.py 2>&1 || echo "[WARN] classification_confidence failed (non-critical, continuing)"
	echo "[CONFIDENCE] Done -> classification_confidence_report.md"

# ==============================================================================
# BOUNDARY NORMALITY — depends on enriched pipeline + corpus_data
# ==============================================================================

$(BOUNDARY_NORM_DATA): $(ENRICH_STAMP) $(CORPUS_DATA)
	echo "[BOUNDARY] Running boundary normality analysis..."
	python3 $(PYTHON_DIR)/boundary_normality.py 2>&1 || echo "[WARN] boundary_normality failed (non-critical, continuing)"
	echo "[BOUNDARY] Done -> boundary_normality_data.json + boundary_normality_report.md"

# ==============================================================================
# BOOLEAN INDEPENDENCE — depends on enriched pipeline + corpus_data
# ==============================================================================

$(BOOLEAN_INDEP_DATA): $(ENRICH_STAMP) $(CORPUS_DATA)
	echo "[BOOL] Running boolean feature independence analysis..."
	python3 $(PYTHON_DIR)/boolean_independence.py 2>&1 || echo "[WARN] boolean_independence failed (non-critical, continuing)"
	echo "[BOOL] Done -> boolean_independence_data.json + boolean_independence_report.md"

# ==============================================================================
# INSTITUTIONAL DISSENT — depends on enriched pipeline + corpus_data
# ==============================================================================

$(INST_DISSENT_DATA): $(ENRICH_STAMP) $(CORPUS_DATA)
	echo "[DISSENT] Running institutional dissent analysis..."
	python3 $(PYTHON_DIR)/institutional_dissent_analysis.py 2>&1 || echo "[WARN] institutional_dissent failed (non-critical, continuing)"
	echo "[DISSENT] Done -> institutional_dissent_data.json + institutional_dissent_report.md"

# ==============================================================================
# OMEGA ENRICHMENT — depends on omega_data + corpus_data + orbit_norm
# ==============================================================================

$(OMEGA_ENRICH_STAMP): $(OMEGA_DATA) $(CORPUS_DATA) $(ORBIT_NORM_STAMP) | $(STAMP_DIR)
	echo "[ENRICH] Enriching omega report..."
	if [ -f $(OUTPUT_DIR)/corpus_data.json ] && [ -f $(OUTPUT_DIR)/omega_data.json ]; then \
		python3 $(PYTHON_DIR)/omega_enricher.py 2>&1 || echo "[WARN] omega_enricher failed (non-critical, continuing)"; \
	else \
		echo "[ENRICH] Skipping (missing dependencies)"; \
	fi
	touch $@
	echo "[ENRICH] Done."

# ==============================================================================
# META REPORT — depends on pipeline_output.json + output.txt + orbit_norm
# ==============================================================================

$(META_REPORT): $(PIPELINE_JSON) $(OUTPUT_TXT) $(ORBIT_NORM_STAMP)
	echo "[META] Generating meta-report..."
	python3 $(PYTHON_DIR)/meta_reporter.py > $@ 2>&1 || echo "[WARN] meta_reporter failed (non-critical, continuing)"
	echo "[META] Done -> meta_report.txt"

# ==============================================================================
# CLEAN
# ==============================================================================

clean:
	echo "Cleaning outputs and stamps..."
	rm -f $(OUTPUT_DIR)/output.txt $(OUTPUT_DIR)/pipeline_output.json
	rm -f $(OUTPUT_DIR)/enriched_pipeline.json
	rm -f $(OUTPUT_DIR)/lint_errors.txt
	rm -f $(OUTPUT_DIR)/fingerprint_report.md $(OUTPUT_DIR)/orbit_report.md
	rm -f $(OUTPUT_DIR)/orbit_data.json $(OUTPUT_DIR)/fpn_report.md
	rm -f $(OUTPUT_DIR)/maxent_report.md $(OUTPUT_DIR)/abductive_report.md $(OUTPUT_DIR)/abductive_data.json
	rm -f $(OUTPUT_DIR)/trajectory_report.md $(OUTPUT_DIR)/covering_analysis.md
	rm -f $(OUTPUT_DIR)/giant_component_analysis.md $(OUTPUT_DIR)/coupling_protocol.md
	rm -f $(OUTPUT_DIR)/maxent_diagnostic_report.md
	rm -f $(OUTPUT_DIR)/snare_report.md $(OUTPUT_DIR)/rope_report.md
	rm -f $(OUTPUT_DIR)/scaffold_report.md $(OUTPUT_DIR)/piton_report.md
	rm -f $(OUTPUT_DIR)/true_mountain_report.md $(OUTPUT_DIR)/tangled_rope_report.md
	rm -f $(OUTPUT_DIR)/false_mountain_report.md
	rm -f $(OUTPUT_DIR)/omega_report.md $(OUTPUT_DIR)/omega_data.json
	rm -f $(OUTPUT_DIR)/enriched_omega_report.md $(OUTPUT_DIR)/enriched_omega_data.json
	rm -f $(OUTPUT_DIR)/corpus_data.json
	rm -f $(OUTPUT_DIR)/variance_analysis.md $(OUTPUT_DIR)/pattern_mining.md
	rm -f $(OUTPUT_DIR)/index_sufficiency.md $(OUTPUT_DIR)/index_sufficiency.json
	rm -f $(OUTPUT_DIR)/tangled_rope_decomposition_report.md
	rm -f $(OUTPUT_DIR)/classification_confidence_report.md
	rm -f $(OUTPUT_DIR)/boundary_normality_data.json $(OUTPUT_DIR)/boundary_normality_report.md
	rm -f $(OUTPUT_DIR)/boolean_independence_data.json $(OUTPUT_DIR)/boolean_independence_report.md
	rm -f $(OUTPUT_DIR)/institutional_dissent_data.json $(OUTPUT_DIR)/institutional_dissent_report.md
	rm -f $(OUTPUT_DIR)/meta_report.txt
	rm -rf $(STAMP_DIR)
	echo "Clean."
