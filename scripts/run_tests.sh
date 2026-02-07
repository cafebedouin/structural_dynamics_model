#!/bin/bash
set -e # Exit immediately if a command exits with a non-zero status.

# Define absolute paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( dirname "$SCRIPT_DIR" )"

# Output Paths
PYTHON_DIR="$ROOT_DIR/python"
PROLOG_DIR="$ROOT_DIR/prolog"
OUTPUT_DIR="$ROOT_DIR/outputs"
OUTPUT_LOG="$OUTPUT_DIR/output.txt"
META_REPORT_FILE="$OUTPUT_DIR/meta_report.txt" # Renamed to avoid conflict with function
CORPUS_REPORT="$OUTPUT_DIR/corpus_analysis.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "================================================================================"
echo "                    STRUCTURAL DYNAMICS TEST SUITE                             "
echo "================================================================================"
echo ""

# Ensure output directory exists and clear previous log
mkdir -p "$OUTPUT_DIR"
echo "Initializing Validation Suite - $(date)" > "$OUTPUT_LOG"
echo "------------------------------------------" >> "$OUTPUT_LOG"

# Step 1: Run python scripts to prepare the test suite
echo -e "${BLUE}Step 1: Preparing test suite...${NC}"
python3 "$PYTHON_DIR/prolog_cleaner.py" "$PROLOG_DIR/testsets/" >> "$OUTPUT_LOG" 2>&1
python3 "$PYTHON_DIR/domain_priors.py" --input "$PROLOG_DIR/testsets/" --output "$PROLOG_DIR/domain_registry.pl" >> "$OUTPUT_LOG" 2>&1
python3 "$PYTHON_DIR/python_test_suite.py" >> "$OUTPUT_LOG" 2>&1
# Using a more precise grep to count test_case facts
TEST_COUNT=$(grep -c "test_case(" "$PROLOG_DIR/validation_suite.pl" || echo "0")
echo -e "${GREEN}✓ Suite prepared ($TEST_COUNT tests)${NC}"
echo ""

# Step 2: Run Prolog Tests
echo -e "${BLUE}Step 2: Running Prolog Test Suite...${NC}"
echo -e "${YELLOW}  (This may take a moment - testing $TEST_COUNT scenarios)${NC}"
swipl -g "['$PROLOG_DIR/validation_suite'], run_dynamic_suite, halt." >> "$OUTPUT_LOG" 2>&1 || true
echo "------------------------------------------" >> "$OUTPUT_LOG"
echo "Test suite completed at: $(date)" >> "$OUTPUT_LOG"

# Step 3: Summarize results
echo -e "${BLUE}Step 3: Summarizing results...${NC}"
PASSED=$(grep -c "\[PASS\]" "$OUTPUT_LOG" || true)
FAILED_EX=$(grep -c "\[FAIL\]" "$OUTPUT_LOG" || true)
FAILED_AUDIT=$(grep -c "\[AUDIT FAIL\]" "$OUTPUT_LOG" || true)

# Default to 0 if the grep command finds nothing
PASSED=${PASSED:-0}
FAILED_EX=${FAILED_EX:-0}
FAILED_AUDIT=${FAILED_AUDIT:-0}

FAILED=$((FAILED_EX + FAILED_AUDIT))

echo -e "${GREEN}✓ Test run complete.${NC}"
echo -e "  Passed: $PASSED | Failed: $FAILED"
echo ""

# Step 4: Generate and Display Reports
echo -e "${BLUE}Step 4: Generating reports...${NC}"
# Run all reporters, redirecting their output to the main log
python3 "$PYTHON_DIR/meta_reporter.py" >> "$OUTPUT_LOG" 2>&1
python3 "$PYTHON_DIR/corpus_analyzer.py" --testsets "$PROLOG_DIR/testsets/" >> "$OUTPUT_LOG" 2>&1
# Generate the actual report files silently
python3 "$PYTHON_DIR/meta_reporter.py" > "$META_REPORT_FILE" # Using new variable name
python3 "$PYTHON_DIR/corpus_analyzer.py" --testsets "$PROLOG_DIR/testsets/" > "$CORPUS_REPORT"

echo -e "${GREEN}✓ Reports generated. Displaying Meta-Report:${NC}"
echo ""
cat "$META_REPORT_FILE"

# Final Summary
echo ""
echo "================================================================================"
echo -e "${GREEN}                         TEST SUITE COMPLETE${NC}"
echo "================================================================================"
echo "📁 Main log file: \"$OUTPUT_LOG\""
echo "📊 For a full summary, see the Meta-Report above or in \"$META_REPORT_FILE\""
echo "================================================================================"
echo ""