#!/bin/bash
# ==============================================================================
# Pipeline Dashboard — standalone summary of pipeline output files
# ==============================================================================
# Reads completed output files and prints a summary dashboard.
# Called by the Makefile after all targets complete, or standalone:
#   bash scripts/pipeline_dashboard.sh
# ==============================================================================

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( dirname "$SCRIPT_DIR" )"
OUTPUT_DIR="$ROOT_DIR/outputs"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

WARNINGS=0
ERRORS=0

echo "================================================================================"
echo -e "${BOLD}                        PIPELINE DASHBOARD${NC}"
echo "================================================================================"
echo ""

# --------------------------------------------------------------------------
# Prolog tests
# --------------------------------------------------------------------------
echo -e "${BOLD}  PROLOG TESTS${NC}"
if [ -f "$OUTPUT_DIR/output.txt" ]; then
    PASSED=$(grep -c "\[PASS\]" "$OUTPUT_DIR/output.txt" 2>/dev/null) || PASSED=0
    FAILED_EX=$(grep -c "\[FAIL\]" "$OUTPUT_DIR/output.txt" 2>/dev/null) || FAILED_EX=0
    FAILED_AUDIT=$(grep -c "\[AUDIT FAIL\]" "$OUTPUT_DIR/output.txt" 2>/dev/null) || FAILED_AUDIT=0
    TOTAL_FAILED=$((FAILED_EX + FAILED_AUDIT))
    echo "  Passed:       $PASSED"
    echo "  Failed:       $TOTAL_FAILED"
    if [ "$((PASSED + TOTAL_FAILED))" -gt 0 ]; then
        PASS_RATE=$(python3 -c "print(f'{$PASSED / ($PASSED + $TOTAL_FAILED) * 100:.1f}%')" 2>/dev/null || echo "N/A")
        echo "  Pass Rate:    $PASS_RATE"
    fi
else
    echo "  (output.txt not found)"
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# --------------------------------------------------------------------------
# Structural linter
# --------------------------------------------------------------------------
echo -e "${BOLD}  STRUCTURAL LINTER${NC}"
if [ -f "$OUTPUT_DIR/lint_errors.txt" ]; then
    LINT_FAIL=$(grep -c ':$' "$OUTPUT_DIR/lint_errors.txt" 2>/dev/null) || LINT_FAIL=0
    LINT_TOTAL=$(find "$ROOT_DIR/prolog/testsets" -name '*.pl' 2>/dev/null | wc -l) || LINT_TOTAL=0
    LINT_PASS=$((LINT_TOTAL - LINT_FAIL))
    echo "  Passed:       $LINT_PASS / $LINT_TOTAL"
    echo "  With Errors:  $LINT_FAIL"
    if [ "$LINT_FAIL" -gt 0 ]; then
        echo "  Error File:   outputs/lint_errors.txt"
    fi
else
    echo "  (lint_errors.txt not found)"
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# --------------------------------------------------------------------------
# Corpus composition
# --------------------------------------------------------------------------
echo -e "${BOLD}  CORPUS COMPOSITION (from corpus data)${NC}"
if [ -f "$OUTPUT_DIR/corpus_data.json" ]; then
    python3 -c "
import json, os
with open('$OUTPUT_DIR/corpus_data.json') as f:
    data = json.load(f)
from collections import Counter
types = Counter()
for cid, c in data['constraints'].items():
    ct = c.get('claimed_type')
    if ct:
        types[ct] += 1
# Try to load confidence cross-tab from pipeline_output.json
type_bands = {}
pipe_path = '$OUTPUT_DIR/pipeline_output.json'
if os.path.exists(pipe_path):
    try:
        with open(pipe_path) as f2:
            pdata = json.load(f2)
        for pc in pdata.get('per_constraint', []):
            ct = pc.get('claimed_type')
            band = pc.get('confidence_band')
            if ct and band:
                if ct not in type_bands:
                    type_bands[ct] = Counter()
                type_bands[ct][band] += 1
    except Exception:
        pass
for t in ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']:
    count = types.get(t, 0)
    extra = ''
    if t in type_bands and count > 0:
        tb = type_bands[t]
        total = sum(tb.values())
        parts = []
        for band in ['deep', 'borderline']:
            n = tb.get(band, 0)
            if n > 0:
                pct = round(n / total * 100)
                parts.append(f'{pct}% {band}')
        if parts:
            extra = '  (' + ', '.join(parts) + ')'
    print(f'  {t + \":\":16s} {count}{extra}')
" 2>/dev/null || echo "  (error reading corpus_data.json)"
else
    echo "  (corpus_data.json not available)"
fi
echo ""

# --------------------------------------------------------------------------
# Fingerprint summary
# --------------------------------------------------------------------------
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

# --------------------------------------------------------------------------
# Orbit analysis
# --------------------------------------------------------------------------
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

# --------------------------------------------------------------------------
# FPN analysis
# --------------------------------------------------------------------------
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

# --------------------------------------------------------------------------
# MaxEnt analysis
# --------------------------------------------------------------------------
echo -e "${BOLD}  MAXENT SHADOW CLASSIFIER${NC}"
if [ -f "$OUTPUT_DIR/maxent_report.md" ]; then
    ME_CONSTRAINTS=$(grep -oP 'Constraints analyzed\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_ENTROPY=$(grep -oP 'Mean normalized entropy\*\* \| \K[0-9.]+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_FLAGGED=$(grep -oP 'High uncertainty constraints\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null || echo "?")
    ME_HARD=$(grep -oP 'Hard disagreements\*\* \| \K\d+' "$OUTPUT_DIR/maxent_report.md" 2>/dev/null | head -1 || echo "?")
    echo "  Constraints:  $ME_CONSTRAINTS"
    echo "  Mean Entropy: $ME_ENTROPY"
    echo "  High Uncertainty: $ME_FLAGGED"
    echo "  Hard Disagreements: $ME_HARD"
else
    echo "  (not generated)"
fi
echo ""

# --------------------------------------------------------------------------
# Classification confidence (from pipeline_output.json enrichment)
# --------------------------------------------------------------------------
echo -e "${BOLD}  CLASSIFICATION CONFIDENCE${NC}"
if [ -f "$OUTPUT_DIR/pipeline_output.json" ]; then
    python3 -c "
import json, sys
with open('$OUTPUT_DIR/pipeline_output.json') as f:
    data = json.load(f)
pcs = data.get('per_constraint', [])
bands = {}
total_conf = 0.0
conf_count = 0
for pc in pcs:
    b = pc.get('confidence_band')
    if b is None:
        continue
    bands[b] = bands.get(b, 0) + 1
    c = pc.get('confidence')
    if c is not None:
        total_conf += c
        conf_count += 1
if not bands:
    print('  (confidence fields not yet available)')
    sys.exit(0)
total = sum(bands.values())
for label in ['deep', 'moderate', 'borderline']:
    n = bands.get(label, 0)
    pct = n / total * 100 if total else 0
    print(f'  {label + \":\":16s} {n:>4d} ({pct:.1f}%)')
if conf_count:
    print(f'  {\"Mean Confidence:\":16s} {total_conf / conf_count:.4f}')
" 2>/dev/null || echo "  (error reading pipeline_output.json)"
else
    echo "  (pipeline_output.json not available)"
fi
echo ""

# --------------------------------------------------------------------------
# Abductive analysis
# --------------------------------------------------------------------------
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

# --------------------------------------------------------------------------
# Trajectory mining
# --------------------------------------------------------------------------
echo -e "${BOLD}  TRAJECTORY MINING${NC}"
if [ -f "$OUTPUT_DIR/trajectory_report.md" ] && [ -s "$OUTPUT_DIR/trajectory_report.md" ]; then
    TM_TRAJECTORIES=$(grep -oP 'Total trajectories\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_FAMILIES=$(grep -oP 'Structural families\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_TWINS=$(grep -oP 'Cross-domain twins\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    TM_SINGLETONS=$(grep -oP 'Singletons \(anomalies\)\*\* \| \K\d+' "$OUTPUT_DIR/trajectory_report.md" 2>/dev/null || echo "?")
    echo "  Trajectories: $TM_TRAJECTORIES"
    echo "  Structural Families: $TM_FAMILIES"
    echo "  Cross-Domain Twins: $TM_TWINS"
    echo "  Singletons: $TM_SINGLETONS"
else
    echo "  (not generated or disabled)"
fi
echo ""

# --------------------------------------------------------------------------
# Generated reports listing
# --------------------------------------------------------------------------
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
    "omega_data.json" \
    "enriched_omega_report.md" \
    "enriched_omega_data.json" \
    "corpus_data.json" \
    "pipeline_output.json" \
    "fingerprint_report.md" \
    "orbit_report.md" \
    "orbit_data.json" \
    "fpn_report.md" \
    "maxent_report.md" \
    "abductive_report.md" \
    "trajectory_report.md" \
    "covering_analysis.md" \
    "giant_component_analysis.md" \
    "coupling_protocol.md" \
    "maxent_diagnostic_report.md" \
    "variance_analysis.md" \
    "pattern_mining.md" \
    "index_sufficiency.md" \
    "meta_report.txt"; do
    filepath="$OUTPUT_DIR/$report"
    if [ -f "$filepath" ]; then
        size=$(du -h "$filepath" 2>/dev/null | cut -f1)
        printf "  ${GREEN}+${NC} %-35s %s\n" "$report" "$size"
    else
        printf "  ${RED}-${NC} %-35s %s\n" "$report" "(missing)"
        ERRORS=$((ERRORS + 1))
    fi
done
echo ""

# --------------------------------------------------------------------------
# Final status
# --------------------------------------------------------------------------
echo "================================================================================"
if [ "$ERRORS" -gt 0 ]; then
    echo -e "  ${RED}COMPLETED WITH $ERRORS MISSING FILE(S) AND $WARNINGS WARNING(S)${NC}"
elif [ "$WARNINGS" -gt 0 ]; then
    echo -e "  ${YELLOW}COMPLETED WITH $WARNINGS WARNING(S)${NC}"
else
    echo -e "  ${GREEN}COMPLETED SUCCESSFULLY${NC}"
fi
echo "  Pipeline outputs: outputs/"
echo "================================================================================"
echo ""
