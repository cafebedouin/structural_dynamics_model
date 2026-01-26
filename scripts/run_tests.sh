#!/bin/bash

# Define absolute paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( dirname "$SCRIPT_DIR" )"

# Output Paths
PYTHON_DIR="$ROOT_DIR/python"
PROLOG_DIR="$ROOT_DIR/prolog"
OUTPUT_DIR="$ROOT_DIR/outputs"
OUTPUT_LOG="$OUTPUT_DIR/output.txt"
META_REPORT="$OUTPUT_DIR/meta_report.txt"
CORPUS_REPORT="$OUTPUT_DIR/corpus_analysis.txt"
FM_REPORT="$OUTPUT_DIR/false_mountain_report.md"
SNARE_REPORT="$OUTPUT_DIR/snare_report.md"
TR_REPORT="$OUTPUT_DIR/tangled_rope_report.md"
TM_REPORT="$OUTPUT_DIR/true_mountain_report.md"
ROPE_REPORT="$OUTPUT_DIR/rope_report.md"
PITON_REPORT="$OUTPUT_DIR/piton_report.md"
OMEGA_REPORT="$OUTPUT_DIR/omega_report.md"

# Colors for output (optional, makes it prettier)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "================================================================================"
echo "                    STRUCTURAL DYNAMICS TEST SUITE                             "
echo "================================================================================"
echo ""

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Step 0: Clean Prolog files
echo -e "${BLUE}Step 0: Cleaning Prolog files (fixing AI artifacts)...${NC}"
cd "$PYTHON_DIR" || exit
python3 prolog_cleaner.py "$PROLOG_DIR/testsets/"

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Prolog files cleaned${NC}"
else
    echo -e "${YELLOW}⚠ Prolog cleaner encountered issues (continuing anyway)${NC}"
fi
cd "$SCRIPT_DIR" || exit

echo ""

# Step 1: Structural Linter (optional - skip if not present)
if [ -f "$PYTHON_DIR/structural_linter.py" ]; then
    echo -e "${BLUE}Step 1: Running Structural Linter...${NC}"
    python3 "$PYTHON_DIR/structural_linter.py"
    if [ $? -ne 0 ]; then
        echo -e "${RED}✗ Structural linter failed. Fix issues and re-run.${NC}"
        exit 1
    fi
    echo -e "${GREEN}✓ Structural linter passed${NC}"
else
    echo -e "${YELLOW}Step 1: Skipping structural linter (not found)${NC}"
fi

echo ""

# Step 2: Update Domain Registry
echo -e "${BLUE}Step 2: Updating Domain Registry...${NC}"

# Clear previous log
echo "Initializing Validation Suite - $(date)" > "$OUTPUT_LOG"
echo "------------------------------------------" >> "$OUTPUT_LOG"

python3 "$PYTHON_DIR/domain_priors.py" --input "$PROLOG_DIR/testsets/" --output "$PROLOG_DIR/domain_registry.pl" >> "$OUTPUT_LOG" 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Domain Registry updated${NC}"
else
    echo -e "${RED}✗ Failed to update Domain Registry. Check $OUTPUT_LOG${NC}"
    exit 1
fi

echo ""

# Step 3: Build Test Suite
echo -e "${BLUE}Step 3: Building Validation Suite...${NC}"
cd "$PYTHON_DIR" || exit
python3 "python_test_suite.py" >> "$OUTPUT_LOG" 2>&1
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    # Count how many tests were generated
    TEST_COUNT=$(grep -c "test_file" "$PROLOG_DIR/validation_suite.pl" || echo "unknown")
    echo -e "${GREEN}✓ Validation suite generated ($TEST_COUNT tests)${NC}"
else
    echo -e "${RED}✗ Failed to build test suite. Check $OUTPUT_LOG${NC}"
    exit 1
fi

echo ""

# Step 4: Run Prolog Tests
echo -e "${BLUE}Step 4: Running Prolog Test Suite...${NC}"
echo -e "${YELLOW}  (This may take a moment - testing $TEST_COUNT scenarios)${NC}"

swipl -g "['$PROLOG_DIR/v3_1_stack'], ['$PROLOG_DIR/validation_suite'], run_dynamic_suite, halt." >> "$OUTPUT_LOG" 2>&1

echo "------------------------------------------" >> "$OUTPUT_LOG"
echo "Test suite completed at: $(date)" >> "$OUTPUT_LOG"

# Quick summary
PASSED=$(grep -c "test_passed" "$OUTPUT_LOG")
FAILED=$(grep -c "\[FAIL\]" "$OUTPUT_LOG")

echo -e "${GREEN}✓ Test suite completed${NC}"
echo -e "  Passed: $PASSED | Failed: $FAILED"

echo ""

# Step 5: Parse Results (if parser exists)
if [ -f "$PYTHON_DIR/parse_results.py" ]; then
    echo -e "${BLUE}Step 5: Parsing results...${NC}"
    python3 "$PYTHON_DIR/parse_results.py" >> "$OUTPUT_LOG" 2>&1
    echo -e "${GREEN}✓ Results parsed${NC}"
fi

echo ""

# Step 6: Profile Calibration (if available)
if [ -f "$PYTHON_DIR/calibrate_profiles.py" ]; then
    echo -e "${BLUE}Step 6: Generating profile calibration...${NC}"
    python3 "$PYTHON_DIR/calibrate_profiles.py" >> "$OUTPUT_LOG" 2>&1
    echo -e "${GREEN}✓ Profile calibration complete${NC}"
fi

echo ""

# Step 7: Generate Meta-Report (NEW!)
echo -e "${BLUE}Step 7: Generating Meta-Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 meta_reporter.py --output "$OUTPUT_LOG" > "$META_REPORT"
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Meta-report generated${NC}"
    # Display the meta-report to terminal
    cat "$META_REPORT"
    # Also save to output.txt
    echo "" >> "$OUTPUT_LOG"
    echo "================================================================================" >> "$OUTPUT_LOG"
    cat "$META_REPORT" >> "$OUTPUT_LOG"
else
    echo -e "${YELLOW}⚠ Meta-report generation encountered issues${NC}"
fi

echo ""

# Step 8: Corpus Analysis (NEW!)
echo -e "${BLUE}Step 8: Analyzing Corpus Connections...${NC}"
cd "$PYTHON_DIR" || exit
python3 corpus_analyzer.py --testsets "$PROLOG_DIR/testsets/" > "$CORPUS_REPORT"
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Corpus analysis complete${NC}"
    # Display the corpus analysis
    cat "$CORPUS_REPORT"
    # Also save to output.txt
    echo "" >> "$OUTPUT_LOG"
    cat "$CORPUS_REPORT" >> "$OUTPUT_LOG"
else
    echo -e "${YELLOW}⚠ Corpus analysis encountered issues${NC}"
fi

echo ""

# Step 9: Generate False Mountain Report
echo -e "${BLUE}Step 9: Generating False Mountain Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 false_mountain_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ False Mountain Report generated${NC}"
else
    echo -e "${YELLOW}⚠ False Mountain Report generation encountered issues${NC}"
fi

echo ""

# Step 10: Generate Snare Report
echo -e "${BLUE}Step 10: Generating Snare Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 snare_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Snare Report generated${NC}"
else
    echo -e "${YELLOW}⚠ Snare Report generation encountered issues${NC}"
fi

echo ""

# Step 11: Generate Tangled Rope Report
echo -e "${BLUE}Step 11: Generating Tangled Rope Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 tangled_rope_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Tangled Rope Report generated${NC}"
else
    echo -e "${YELLOW}⚠ Tangled Rope Report generation encountered issues${NC}"
fi

echo ""

# Step 12: Generate True Mountain Report
echo -e "${BLUE}Step 12: Generating True Mountain Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 true_mountain_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ True Mountain Report generated${NC}"
else
    echo -e "${YELLOW}⚠ True Mountain Report generation encountered issues${NC}"
fi

echo ""

# Step 13: Generate Rope Report
echo -e "${BLUE}Step 13: Generating Rope Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 rope_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Rope Report generated${NC}"
else
    echo -e "${YELLOW}⚠ Rope Report generation encountered issues${NC}"
fi

echo ""

# Step 14: Generate Piton Report (NEW!)
echo -e "${BLUE}Step 14: Generating Piton Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 piton_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Piton Report generated${NC}"
else
    echo -e "${YELLOW}⚠ Piton Report generation encountered issues${NC}"
fi

echo ""

# Step 15: Generate Omega Report
echo -e "${BLUE}Step 15: Generating Omega Report...${NC}"
cd "$PYTHON_DIR" || exit
python3 omega_reporter.py
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Omega Report generated${NC}"
else
    echo -e "${YELLOW}⚠ Omega Report generation encountered issues${NC}"
fi

echo ""


# Final Summary
echo "================================================================================"
echo -e "${GREEN}                         TEST SUITE COMPLETE${NC}"
echo "================================================================================"
echo ""
echo "📁 Output files:"
echo "   Main log:     \"$OUTPUT_LOG\""
echo "   Meta-report:  \"$META_REPORT\""
echo "   Corpus:       \"$CORPUS_REPORT\""
echo "   FM Report:    \"$FM_REPORT\""
echo "   Snare Report: \"$SNARE_REPORT\""
echo "   TR Report:    \"$TR_REPORT\""
echo "   TM Report:    \"$TM_REPORT\""
echo "   Rope Report:  \"$ROPE_REPORT\""
echo "   Piton Report: \"$PITON_REPORT\""
echo "   Omega Report: \"$OMEGA_REPORT\""
echo ""
echo "📊 Quick Stats:"
echo "   Tests passed: $PASSED"
echo "   Tests failed: $FAILED"

# Count key issues
FALSE_MOUNTAINS=$(grep -c "type_1_false_mountain" "$OUTPUT_LOG")
OMEGAS=$(grep -c "Ω:" "$OUTPUT_LOG")
ERRORS=$(grep -c "\[ERROR\]" "$OUTPUT_LOG")

echo "   False mountains: $FALSE_MOUNTAINS"
echo "   Omegas: $OMEGAS"
echo "   Errors: $ERRORS"

echo ""

# Suggest next steps based on findings
if [ "$ERRORS" -gt "10" ]; then
    echo -e "${RED}⚠ HIGH PRIORITY: Fix data quality issues (many errors detected)${NC}"
fi

if [ "$FALSE_MOUNTAINS" -gt "20" ]; then
    echo -e "${YELLOW}⚠ Review false mountains - significant ontological misclassification${NC}"
fi

if [ "$FAILED" -gt "0" ]; then
    echo -e "${YELLOW}💡 Check failed tests in: $OUTPUT_LOG${NC}"
fi

echo ""
echo "💡 Next Steps:"
echo "   1. Review all generated reports for actionable insights."
echo "   2. Fix any critical errors or false mountains."
echo "   3. Add scenarios based on recommendations."
echo "   4. Re-run ./scripts/run_tests.sh to verify improvements."
echo ""
echo "================================================================================"
