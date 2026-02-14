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
