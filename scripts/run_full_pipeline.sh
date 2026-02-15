#!/bin/bash
# ==============================================================================
# Structural Dynamics Model - Full Pipeline
# ==============================================================================
# Runs the complete test + analysis + reporting pipeline:
#   1. Prepare test suite (clean, generate registry, generate validation_suite)
#   2. Run Prolog tests -> output.txt
#   3. Run structural linter on all testsets
#   4. Generate category reports (Mountain, Rope, Tangled Rope, Snare, Scaffold, Piton)
#   5. Generate Omega report
#   6. Extract corpus data -> corpus_data.json
#   7. Run analysis scripts (variance, pattern mining, sufficiency)
#   8. Run fingerprint analysis
#   9. Run meta-reporter
#  10. Print dashboard summary
# ==============================================================================

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( dirname "$SCRIPT_DIR" )"
PYTHON_DIR="$ROOT_DIR/python"
PROLOG_DIR="$ROOT_DIR/prolog"
TESTSETS_DIR="$PROLOG_DIR/testsets"
OUTPUT_DIR="$ROOT_DIR/outputs"
OUTPUT_LOG="$OUTPUT_DIR/output.txt"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Counters for final dashboard
STEP=0
ERRORS=0
WARNINGS=0

step() {
    STEP=$((STEP + 1))
    echo ""
    echo -e "${BOLD}${BLUE}[$STEP] $1${NC}"
    echo "--------------------------------------------------------------------------------"
}

ok()   { echo -e "  ${GREEN}OK${NC}  $1"; }
warn() { echo -e "  ${YELLOW}WARN${NC}  $1"; WARNINGS=$((WARNINGS + 1)); }
fail() { echo -e "  ${RED}FAIL${NC}  $1"; ERRORS=$((ERRORS + 1)); }

# ==============================================================================
# SETUP
# ==============================================================================
mkdir -p "$OUTPUT_DIR"

echo "================================================================================"
echo "         STRUCTURAL DYNAMICS MODEL - FULL PIPELINE"
echo "         $(date)"
echo "================================================================================"

# ==============================================================================
# STEP 1: Prepare test suite
# ==============================================================================
step "Preparing test suite"

if python3 "$PYTHON_DIR/prolog_cleaner.py" "$TESTSETS_DIR/" > /dev/null 2>&1; then
    ok "Prolog files cleaned"
else
    warn "Prolog cleaner returned errors (non-fatal)"
fi

if python3 "$PYTHON_DIR/domain_priors.py" --input "$TESTSETS_DIR/" --output "$PROLOG_DIR/domain_registry.pl" > /dev/null 2>&1; then
    ok "Domain registry generated"
else
    fail "Domain registry generation failed"
fi

if python3 "$PYTHON_DIR/python_test_suite.py" > /dev/null 2>&1; then
    TEST_COUNT=$(grep -c "^test_case(" "$PROLOG_DIR/validation_suite.pl" 2>/dev/null || echo "0")
    ok "Validation suite generated ($TEST_COUNT test cases)"
else
    fail "Validation suite generation failed"
fi

# ==============================================================================
# STEP 1b: Check for duplicate constraint IDs
# ==============================================================================
if DUPE_OUTPUT=$(python3 "$PYTHON_DIR/duplicate_checker.py" --dir "$TESTSETS_DIR" 2>&1); then
    ok "No duplicate constraint IDs"
else
    warn "Duplicate constraint IDs detected:"
    echo "$DUPE_OUTPUT" | while IFS= read -r line; do
        echo "        $line"
    done
fi

# ==============================================================================
# STEP 2: Run Prolog tests
# ==============================================================================
step "Running Prolog test suite"

echo "Initializing Validation Suite - $(date)" > "$OUTPUT_LOG"
echo "------------------------------------------" >> "$OUTPUT_LOG"

if (cd "$PROLOG_DIR" && swipl -g "[validation_suite], run_dynamic_suite, halt.") >> "$OUTPUT_LOG" 2>&1; then
    ok "Prolog tests completed normally"
else
    warn "Prolog tests completed with errors (check output.txt)"
fi
echo "------------------------------------------" >> "$OUTPUT_LOG"
echo "Test suite completed at: $(date)" >> "$OUTPUT_LOG"

PASSED=$(grep -c "\[PASS\]" "$OUTPUT_LOG" 2>/dev/null) || PASSED=0
FAILED_EX=$(grep -c "\[FAIL\]" "$OUTPUT_LOG" 2>/dev/null) || FAILED_EX=0
FAILED_AUDIT=$(grep -c "\[AUDIT FAIL\]" "$OUTPUT_LOG" 2>/dev/null) || FAILED_AUDIT=0
TOTAL_FAILED=$((FAILED_EX + FAILED_AUDIT))

ok "Results: $PASSED passed, $TOTAL_FAILED failed"

# ==============================================================================
# STEP 3: Structural linter
# ==============================================================================
step "Running structural linter on testsets"

LINT_PASS=0
LINT_FAIL=0
LINT_ERRORS_FILE="$OUTPUT_DIR/lint_errors.txt"
> "$LINT_ERRORS_FILE"

for pl_file in "$TESTSETS_DIR"/*.pl; do
    [ -f "$pl_file" ] || continue
    filename=$(basename "$pl_file")

    # Run linter via inline Python
    result=$(python3 -c "
import sys
sys.path.insert(0, '$PYTHON_DIR')
from structural_linter import lint_file
errors = lint_file('$pl_file')
if errors:
    for e in errors:
        print(e)
    sys.exit(1)
" 2>&1) || {
        LINT_FAIL=$((LINT_FAIL + 1))
        echo "$filename:" >> "$LINT_ERRORS_FILE"
        echo "$result" | while IFS= read -r line; do
            echo "  $line" >> "$LINT_ERRORS_FILE"
        done
        echo "" >> "$LINT_ERRORS_FILE"
        continue
    }
    LINT_PASS=$((LINT_PASS + 1))
done

LINT_TOTAL=$((LINT_PASS + LINT_FAIL))
if [ "$LINT_FAIL" -gt 0 ]; then
    warn "$LINT_PASS/$LINT_TOTAL passed, $LINT_FAIL with errors (see outputs/lint_errors.txt)"
else
    ok "All $LINT_TOTAL testsets passed linting"
fi

# ==============================================================================
# STEP 4: Category reports
# ==============================================================================
step "Generating category reports"

declare -A REPORTERS=(
    ["snare"]="snare_reporter.py"
    ["rope"]="rope_reporter.py"
    ["tangled_rope"]="tangled_rope_reporter.py"
    ["true_mountain"]="true_mountain_reporter.py"
    ["false_mountain"]="false_mountain_reporter.py"
    ["scaffold"]="scaffold_reporter.py"
    ["piton"]="piton_reporter.py"
)

for category in snare rope tangled_rope true_mountain false_mountain scaffold piton; do
    reporter="${REPORTERS[$category]}"
    report_file="$OUTPUT_DIR/${category}_report.md"

    if [ -f "$PYTHON_DIR/$reporter" ]; then
        output=$(python3 "$PYTHON_DIR/$reporter" 2>&1) || true
        # Extract count from reporter output
        count=$(echo "$output" | grep -oP 'Found \K\d+' | head -1 || true)
        if [ -n "$count" ] && [ "$count" -gt 0 ]; then
            ok "$category: $count found -> ${category}_report.md"
        else
            echo -e "  ${CYAN}--${NC}  $category: 0 found"
        fi
    else
        warn "$category: reporter $reporter not found"
    fi
done

# ==============================================================================
# STEP 5: Omega report
# ==============================================================================
step "Generating Omega report"

if [ -f "$PYTHON_DIR/omega_reporter.py" ]; then
    output=$(python3 "$PYTHON_DIR/omega_reporter.py" 2>&1) || true
    count=$(echo "$output" | grep -oP 'Found \K\d+' | head -1 || true)
    if [ -n "$count" ] && [ "$count" -gt 0 ]; then
        ok "Omegas: $count found -> omega_report.md"
    else
        echo -e "  ${CYAN}--${NC}  Omegas: 0 found"
    fi
fi

# ==============================================================================
# STEP 6: Extract corpus data
# ==============================================================================
step "Extracting corpus data"

if python3 "$PYTHON_DIR/extract_corpus_data.py" \
    --output-txt "$OUTPUT_LOG" \
    --testsets "$TESTSETS_DIR/" \
    --json-output "$OUTPUT_DIR/corpus_data.json" 2>&1 | tail -5; then
    ok "Corpus data extracted -> corpus_data.json"
else
    fail "Corpus data extraction failed"
fi

# ==============================================================================
# STEP 7: Analysis reports (depend on corpus_data.json)
# ==============================================================================
step "Running analysis reports"

if [ -f "$OUTPUT_DIR/corpus_data.json" ]; then
    # Variance analysis
    if python3 "$PYTHON_DIR/variance_analyzer.py" \
        --corpus-data "$OUTPUT_DIR/corpus_data.json" \
        --output "$OUTPUT_DIR/variance_analysis.md" 2>&1 | tail -1; then
        ok "Variance analysis -> variance_analysis.md"
    else
        fail "Variance analysis failed"
    fi

    # Pattern mining
    if python3 "$PYTHON_DIR/pattern_miner.py" \
        --corpus-data "$OUTPUT_DIR/corpus_data.json" \
        --output "$OUTPUT_DIR/pattern_mining.md" 2>&1 | tail -1; then
        ok "Pattern mining -> pattern_mining.md"
    else
        fail "Pattern mining failed"
    fi

    # Sufficiency test
    if python3 "$PYTHON_DIR/sufficiency_tester.py" \
        --corpus-data "$OUTPUT_DIR/corpus_data.json" \
        --output "$OUTPUT_DIR/index_sufficiency.md" 2>&1 | tail -1; then
        ok "Index sufficiency test -> index_sufficiency.md"
    else
        fail "Index sufficiency test failed"
    fi
else
    fail "corpus_data.json not found - skipping analysis reports"
fi

# ==============================================================================
# STEP 8: Fingerprint analysis
# ==============================================================================
step "Generating fingerprint analysis"

FINGERPRINT_REPORT="$OUTPUT_DIR/fingerprint_report.md"
if (cd "$PROLOG_DIR" && swipl -g "[fingerprint_report], halt.") > "$FINGERPRINT_REPORT" 2>/dev/null; then
    # Extract summary stats from report
    PATTERNS=$(grep -oP 'Distinct shift patterns\*\*: \K\d+' "$FINGERPRINT_REPORT" || echo "?")
    FCONSTRAINTS=$(grep -oP 'Constraints analyzed\*\*: \K\d+' "$FINGERPRINT_REPORT" || echo "?")
    ok "Fingerprint analysis: $FCONSTRAINTS constraints, $PATTERNS shift patterns -> fingerprint_report.md"
else
    warn "Fingerprint analysis had issues"
fi

# ==============================================================================
# STEP 8b: Orbit analysis
# ==============================================================================
step "Generating orbit analysis"

ORBIT_REPORT="$OUTPUT_DIR/orbit_report.md"
ORBIT_RAW="$OUTPUT_DIR/orbit_report_raw.md"
if (cd "$PROLOG_DIR" && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl -l orbit_report.pl -g "run_orbit_report, halt.") > "$ORBIT_RAW" 2>/dev/null; then
    # Strip FNL preamble noise before the delimiter
    sed -n '/<!-- ORBIT_REPORT_START -->/,$p' "$ORBIT_RAW" | tail -n +2 > "$ORBIT_REPORT"
    rm -f "$ORBIT_RAW"
    FAMILIES=$(grep -oP 'Orbit families\*\*: \K\d+' "$ORBIT_REPORT" || echo "?")
    OCONSTRAINTS=$(grep -oP 'Constraints analyzed\*\*: \K\d+' "$ORBIT_REPORT" || echo "?")
    ok "Orbit analysis: $OCONSTRAINTS constraints, $FAMILIES families -> orbit_report.md"
else
    rm -f "$ORBIT_RAW"
    warn "Orbit analysis had issues"
fi

# ==============================================================================
# STEP 8c: FPN (Fixed-Point Network) analysis
# ==============================================================================
step "Generating FPN analysis"

FPN_REPORT="$OUTPUT_DIR/fpn_report.md"
FPN_RAW="$OUTPUT_DIR/fpn_report_raw.md"
if (cd "$PROLOG_DIR" && swipl -l stack.pl -l covering_analysis.pl -l fpn_report.pl -g "run_fpn_report, halt.") > "$FPN_RAW" 2>/dev/null; then
    # Strip FNL preamble noise before the delimiter
    sed -n '/<!-- FPN_REPORT_START -->/,$p' "$FPN_RAW" | tail -n +2 > "$FPN_REPORT"
    rm -f "$FPN_RAW"
    FPN_ITERS=$(grep -oP 'Iterations to convergence\*\* \| \K\d+' "$FPN_REPORT" || echo "?")
    FPN_MIGRATIONS=$(grep -oP 'Zone migrations\*\* \| \K\d+' "$FPN_REPORT" || echo "?")
    ok "FPN analysis: $FPN_ITERS iterations, $FPN_MIGRATIONS zone migrations -> fpn_report.md"
else
    rm -f "$FPN_RAW"
    warn "FPN analysis had issues"
fi

# ==============================================================================
# STEP 8d: MaxEnt shadow classifier analysis
# ==============================================================================
step "Generating MaxEnt analysis"

MAXENT_REPORT="$OUTPUT_DIR/maxent_report.md"
MAXENT_RAW="$OUTPUT_DIR/maxent_report_raw.md"
if (cd "$PROLOG_DIR" && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
      -l maxent_classifier.pl -l maxent_report.pl -g "run_maxent_report, halt.") > "$MAXENT_RAW" 2>/dev/null; then
    sed -n '/<!-- MAXENT_REPORT_START -->/,$p' "$MAXENT_RAW" | tail -n +2 > "$MAXENT_REPORT"
    rm -f "$MAXENT_RAW"
    MAXENT_FLAGGED=$(grep -oP 'High uncertainty constraints\*\* \| \K\d+' "$MAXENT_REPORT" || echo "?")
    MAXENT_DISAGREE=$(grep -oP 'Hard disagreements\*\* \| \K\d+' "$MAXENT_REPORT" || echo "?")
    ok "MaxEnt analysis: $MAXENT_FLAGGED flagged, $MAXENT_DISAGREE disagreements -> maxent_report.md"
else
    rm -f "$MAXENT_RAW"
    warn "MaxEnt analysis had issues"
fi

# ==============================================================================
# STEP 8e: Abductive reasoning analysis
# ==============================================================================
step "Generating abductive reasoning analysis"

ABD_REPORT="$OUTPUT_DIR/abductive_report.md"
ABD_RAW="$OUTPUT_DIR/abductive_report_raw.md"
if (cd "$PROLOG_DIR" && swipl -l stack.pl -l covering_analysis.pl \
      -l dirac_classification.pl -l maxent_classifier.pl \
      -l abductive_engine.pl -l abductive_report.pl \
      -g "run_abductive_report, halt.") > "$ABD_RAW" 2>/dev/null; then
    sed -n '/<!-- ABDUCTIVE_REPORT_START -->/,$p' "$ABD_RAW" | tail -n +2 > "$ABD_REPORT"
    rm -f "$ABD_RAW"
    ABD_TOTAL=$(grep -oP 'Total hypotheses\*\* \| \K\d+' "$ABD_REPORT" || echo "?")
    ABD_GENUINE=$(grep -oP 'Genuine findings\*\* \| \K\d+' "$ABD_REPORT" || echo "?")
    ok "Abductive analysis: $ABD_TOTAL hypotheses, $ABD_GENUINE genuine -> abductive_report.md"
else
    rm -f "$ABD_RAW"
    warn "Abductive analysis had issues"
fi

# ==============================================================================
# STEP 8f: Trajectory mining analysis
# ==============================================================================
step "Generating trajectory mining analysis"

TRAJ_ENABLED=$(cd "$PROLOG_DIR" && swipl -g "use_module(config), (config:param(trajectory_enabled, 1) -> write(1) ; write(0)), halt." 2>/dev/null || echo "0")
if [ "$TRAJ_ENABLED" = "1" ]; then
    TRAJ_REPORT="$OUTPUT_DIR/trajectory_report.md"
    TRAJ_RAW="$OUTPUT_DIR/trajectory_report_raw.md"
    if (cd "$PROLOG_DIR" && swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
          -l maxent_classifier.pl -l trajectory_mining.pl \
          -l trajectory_report.pl -g "run_trajectory_report, halt.") > "$TRAJ_RAW" 2>/dev/null; then
        sed -n '/<!-- TRAJECTORY_REPORT_START -->/,$p' "$TRAJ_RAW" | tail -n +2 > "$TRAJ_REPORT"
        rm -f "$TRAJ_RAW"
        TRAJ_FAMILIES=$(grep -oP 'Structural families\*\* \| \K\d+' "$TRAJ_REPORT" || echo "?")
        TRAJ_TWINS=$(grep -oP 'Cross-domain twins\*\* \| \K\d+' "$TRAJ_REPORT" || echo "?")
        ok "Trajectory mining: $TRAJ_FAMILIES families, $TRAJ_TWINS twins -> trajectory_report.md"
    else
        rm -f "$TRAJ_RAW"
        warn "Trajectory mining had issues"
    fi
else
    echo -e "  ${CYAN}--${NC}  Trajectory mining disabled (trajectory_enabled=0)"
fi

# ==============================================================================
# STEP 9: Meta-report
# ==============================================================================
step "Generating meta-report"

if python3 "$PYTHON_DIR/meta_reporter.py" > "$OUTPUT_DIR/meta_report.txt" 2>&1; then
    ok "Meta-report -> meta_report.txt"
else
    warn "Meta-report generation had issues"
fi

# ==============================================================================
# STEP 10: Dashboard summary
# ==============================================================================
echo ""
echo "================================================================================"
echo -e "${BOLD}                        PIPELINE DASHBOARD${NC}"
echo "================================================================================"
echo ""

# Test results
echo -e "${BOLD}  PROLOG TESTS${NC}"
echo "  Passed:       $PASSED"
echo "  Failed:       $TOTAL_FAILED"
if [ "$((PASSED + TOTAL_FAILED))" -gt 0 ]; then
    PASS_RATE=$(python3 -c "print(f'{$PASSED / ($PASSED + $TOTAL_FAILED) * 100:.1f}%')" 2>/dev/null || echo "N/A")
    echo "  Pass Rate:    $PASS_RATE"
fi
echo ""

# Linter results
echo -e "${BOLD}  STRUCTURAL LINTER${NC}"
echo "  Passed:       $LINT_PASS / $LINT_TOTAL"
echo "  With Errors:  $LINT_FAIL"
if [ "$LINT_FAIL" -gt 0 ]; then
    echo "  Error File:   outputs/lint_errors.txt"
fi
echo ""

# Corpus composition (from corpus_data.json — avoids dual-ID double-counting)
echo -e "${BOLD}  CORPUS COMPOSITION (from corpus data)${NC}"
if [ -f "$OUTPUT_DIR/corpus_data.json" ]; then
    python3 -c "
import json
with open('$OUTPUT_DIR/corpus_data.json') as f:
    data = json.load(f)
from collections import Counter
types = Counter()
for cid, c in data['constraints'].items():
    ct = c.get('claimed_type')
    if ct:
        types[ct] += 1
for t in ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']:
    print(f'  {t + \":\":16s} {types.get(t, 0)}')
"
else
    echo "  (corpus_data.json not available)"
fi
echo ""

# Fingerprint summary
echo -e "${BOLD}  LOGICAL FINGERPRINT${NC}"
if [ -f "$OUTPUT_DIR/fingerprint_report.md" ]; then
    FP_PATTERNS=$(grep -oP 'Distinct shift patterns\*\*: \K\d+' "$OUTPUT_DIR/fingerprint_report.md" 2>/dev/null || echo "?")
    FP_CONSTRAINTS=$(grep -oP 'Constraints analyzed\*\*: \K\d+' "$OUTPUT_DIR/fingerprint_report.md" 2>/dev/null || echo "?")
    FP_SCOPE=$(grep -oP 'Scope-sensitive constraints\*\*: \K\d+' "$OUTPUT_DIR/fingerprint_report.md" 2>/dev/null || echo "?")
    echo "  Constraints:  $FP_CONSTRAINTS"
    echo "  Shift Patterns: $FP_PATTERNS"
    echo "  Scope-Sensitive: $FP_SCOPE"
else
    echo "  (not generated)"
fi
echo ""

# Orbit analysis summary
echo -e "${BOLD}  GAUGE ORBIT ANALYSIS${NC}"
if [ -f "$OUTPUT_DIR/orbit_report.md" ]; then
    OR_FAMILIES=$(grep -oP 'Orbit families\*\*: \K\d+' "$OUTPUT_DIR/orbit_report.md" 2>/dev/null || echo "?")
    OR_CONSTRAINTS=$(grep -oP 'Constraints analyzed\*\*: \K\d+' "$OUTPUT_DIR/orbit_report.md" 2>/dev/null || echo "?")
    OR_INVARIANT=$(grep -oP 'Gauge-invariant.*?: \K\d+' "$OUTPUT_DIR/orbit_report.md" 2>/dev/null || echo "?")
    OR_VARIANT=$(grep -oP 'Gauge-variant.*?: \K\d+' "$OUTPUT_DIR/orbit_report.md" 2>/dev/null || echo "?")
    echo "  Constraints:  $OR_CONSTRAINTS"
    echo "  Orbit Families: $OR_FAMILIES"
    echo "  Gauge-Invariant: $OR_INVARIANT"
    echo "  Gauge-Variant: $OR_VARIANT"
else
    echo "  (not generated)"
fi
echo ""

# FPN analysis summary
echo -e "${BOLD}  FIXED-POINT NETWORK (FPN) ANALYSIS${NC}"
if [ -f "$OUTPUT_DIR/fpn_report.md" ]; then
    FPN_D_ITERS=$(grep -oP 'Iterations to convergence\*\* \| \K\d+' "$OUTPUT_DIR/fpn_report.md" 2>/dev/null || echo "?")
    FPN_D_MIGRATIONS=$(grep -oP 'Zone migrations\*\* \| \K\d+' "$OUTPUT_DIR/fpn_report.md" 2>/dev/null || echo "?")
    FPN_D_SIGNIFICANT=$(grep -oP 'significant shift.*?\| \K\d+' "$OUTPUT_DIR/fpn_report.md" 2>/dev/null || echo "?")
    echo "  Iterations:   $FPN_D_ITERS"
    echo "  Zone Migrations: $FPN_D_MIGRATIONS"
    echo "  Significant Movers: $FPN_D_SIGNIFICANT"
else
    echo "  (not generated)"
fi
echo ""

# MaxEnt analysis summary
echo -e "${BOLD}  MAXENT SHADOW CLASSIFIER${NC}"
if [ -f "$OUTPUT_DIR/maxent_report.md" ]; then
    ME_CONSTRAINTS=$(grep -oP 'Constraints analyzed\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_ENTROPY=$(grep -oP 'Mean normalized entropy\*\* \| \K[0-9.]+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_FLAGGED=$(grep -oP 'High uncertainty constraints\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_HARD=$(grep -oP 'Hard disagreements\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    echo "  Constraints:  $ME_CONSTRAINTS"
    echo "  Mean Entropy: $ME_ENTROPY"
    echo "  High Uncertainty: $ME_FLAGGED"
    echo "  Hard Disagreements: $ME_HARD"
else
    echo "  (not generated)"
fi
echo ""

# Abductive analysis summary
echo -e "${BOLD}  ABDUCTIVE REASONING${NC}"
if [ -f "$OUTPUT_DIR/abductive_report.md" ]; then
    ABD_D_TOTAL=$(grep -oP 'Total hypotheses\*\* \| \K\d+' "$OUTPUT_DIR/abductive_report.md" 2>/dev/null || echo "?")
    ABD_D_GENUINE=$(grep -oP 'Genuine findings\*\* \| \K\d+' "$OUTPUT_DIR/abductive_report.md" 2>/dev/null || echo "?")
    ABD_D_ARTIFACTS=$(grep -oP 'Override artifacts\*\* \| \K\d+' "$OUTPUT_DIR/abductive_report.md" 2>/dev/null || echo "?")
    echo "  Total Hypotheses: $ABD_D_TOTAL"
    echo "  Genuine Findings: $ABD_D_GENUINE"
    echo "  Override Artifacts: $ABD_D_ARTIFACTS"
else
    echo "  (not generated)"
fi
echo ""

# Trajectory mining summary
echo -e "${BOLD}  TRAJECTORY MINING${NC}"
if [ -f "$OUTPUT_DIR/trajectory_report.md" ]; then
    TM_TRAJECTORIES=$(grep -oP 'Total trajectories\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_FAMILIES=$(grep -oP 'Structural families\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_TWINS=$(grep -oP 'Cross-domain twins\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_SINGLETONS=$(grep -oP 'Singletons \(anomalies\)\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    echo "  Trajectories: $TM_TRAJECTORIES"
    echo "  Structural Families: $TM_FAMILIES"
    echo "  Cross-Domain Twins: $TM_TWINS"
    echo "  Singletons: $TM_SINGLETONS"
else
    echo "  (not generated)"
fi
echo ""

# Generated reports
echo -e "${BOLD}  GENERATED REPORTS${NC}"
for report in \
    "output.txt" \
    "lint_errors.txt" \
    "snare_report.md" \
    "rope_report.md" \
    "tangled_rope_report.md" \
    "true_mountain_report.md" \
    "false_mountain_report.md" \
    "scaffold_report.md" \
    "piton_report.md" \
    "omega_report.md" \
    "corpus_data.json" \
    "fingerprint_report.md" \
    "orbit_report.md" \
    "orbit_data.json" \
    "fpn_report.md" \
    "maxent_report.md" \
    "abductive_report.md" \
    "trajectory_report.md" \
    "variance_analysis.md" \
    "pattern_mining.md" \
    "index_sufficiency.md" \
    "meta_report.txt"; do
    filepath="$OUTPUT_DIR/$report"
    if [ -f "$filepath" ]; then
        size=$(du -h "$filepath" 2>/dev/null | cut -f1)
        printf "  ${GREEN}+${NC} %-30s %s\n" "$report" "$size"
    else
        printf "  ${RED}-${NC} %-30s %s\n" "$report" "(missing)"
    fi
done
echo ""

# Final status
echo "================================================================================"
if [ "$ERRORS" -gt 0 ]; then
    echo -e "  ${RED}COMPLETED WITH $ERRORS ERROR(S) AND $WARNINGS WARNING(S)${NC}"
elif [ "$WARNINGS" -gt 0 ]; then
    echo -e "  ${YELLOW}COMPLETED WITH $WARNINGS WARNING(S)${NC}"
else
    echo -e "  ${GREEN}COMPLETED SUCCESSFULLY${NC}"
fi
echo "  Log: outputs/output.txt"
echo "================================================================================"
echo ""
