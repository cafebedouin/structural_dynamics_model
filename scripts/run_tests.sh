#!/bin/bash

# Define absolute path for the root to prevent relative path breakage
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( dirname "$SCRIPT_DIR" )"

PYTHON_DIR="$ROOT_DIR/python"
PROLOG_DIR="$ROOT_DIR/prolog"
OUTPUT_DIR="$ROOT_DIR/outputs"
OUTPUT_LOG="$OUTPUT_DIR/output.txt"

echo "Step 1: Running Structural Linter (Indexical Relativity Gate)..."
python3 ../python/structural_linter.py
 if [ $? -ne 0 ]; then
    echo "Aborting: New files do not meet structural requirements."
    exit 1
 fi

echo "Step 2: Updating Domain Registry..."

# 1. Ensure the output directory exists
mkdir -p "$OUTPUT_DIR"

# 2. Clear previous log
echo "Initializing Validation Suite - $(date)" > "$OUTPUT_LOG"
echo "------------------------------------------" >> "$OUTPUT_LOG"

# 3. Run Domain Registry Generation
python3 "$PYTHON_DIR/domain_priors.py" --input "$PROLOG_DIR/testsets/" --output "$PROLOG_DIR/domain_registry.pl" >> "$OUTPUT_LOG" 2>&1

if [ $? -eq 0 ]; then
    echo "✓ Domain Registry updated."
else
    echo "✗ Failed to update Domain Registry. Check $OUTPUT_LOG for details."
    exit 1
fi

# 4. Run Test Suite Builder
echo "Step 3: Building Validation Suite..."
# Changing directory to python folder to ensure internal python paths resolve
cd "$PYTHON_DIR" || exit
python3 "python_test_suite.py" >> "$OUTPUT_LOG" 2>&1
cd "$SCRIPT_DIR" || exit

if [ $? -eq 0 ]; then
    echo "✓ validation_suite.pl generated."
else
    echo "✗ Failed to build test suite. Check $OUTPUT_LOG for details."
    exit 1
fi

# 5. Run Prolog Validation
echo "Step 4: Running Prolog Dynamic Suite (this may take a moment)..."
# Using absolute paths for the swipl files to ensure they load regardless of CWD
swipl -g "['$PROLOG_DIR/v3_1_stack'], ['$PROLOG_DIR/validation_suite'], run_dynamic_suite, halt." >> "$OUTPUT_LOG" 2>&1

echo "------------------------------------------" >> "$OUTPUT_LOG"
echo "DONE. Full results available in: $OUTPUT_LOG"

# Display the final summary from the log
tail -n 5 "$OUTPUT_LOG"

echo "Step 5: Parsing results for LLM evaluation..."
python3 "$PYTHON_DIR/parse_results.py" >> "$OUTPUT_LOG" 2>&1

# New: Summary of Omegas and Fraud
echo "--- SYSTEM INSIGHTS ---"
grep "omega_variable" "$OUTPUT_LOG" | wc -l | xargs echo "Omega Warnings Identified:"
grep "ERROR: Invalid" "$OUTPUT_LOG" | wc -l | xargs echo "Ontological Violations:"


echo "Step 6: Generating profile calibration suggestions..."
python3 "$PYTHON_DIR/calibrate_profiles.py" >> "$OUTPUT_LOG" 2>&1

echo "DONE. Check $OUTPUT_LOG for 'CALIBRATED CATEGORY PROFILES' section."

echo "------------------------------------------" >> "$OUTPUT_LOG"
echo "FAILED FILE SUMMARY:" >> "$OUTPUT_LOG"

# This finds the DOMAIN: lines immediately preceding a failure
grep -B 5 "\[FAIL\]" "$OUTPUT_LOG" | grep "DOMAIN:" >> "$OUTPUT_LOG"

echo "DONE. Full results available in: $OUTPUT_LOG"
tail -n 10 "$OUTPUT_LOG"
