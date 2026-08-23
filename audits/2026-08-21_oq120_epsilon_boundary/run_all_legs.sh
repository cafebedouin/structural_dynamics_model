#!/usr/bin/env bash
# OQ-120 Phase 0 Step B driver.
# CONCURRENCY CONTRACT: another instance is generating legs in this tree. Before
# sweeping each leg we snapshot its .pl count and re-count after; any leg whose
# count MOVED was in flight and its results are VOID — dropped and named.
# An empty glob throws corpus_empty (fail-closed, the loud failure); a PARTIALLY
# written leg is the silent one, and the count-pair is what catches it.
set -u
cd "$(dirname "$0")/../.."
D=audits/2026-08-21_oq120_epsilon_boundary
PY=.venv/bin/python
LOG=$D/raw/sweep_log.txt
: > "$LOG"

LEGS=$(for d in prolog/testsets*/; do n=$(ls "$d"*.pl 2>/dev/null | wc -l); \
       [ "$n" -gt 0 ] && basename "$d"; done)
LEGS="$LEGS archives/datasets/kernel_v1"

echo "legs to sweep:" | tee -a "$LOG"
for L in $LEGS; do echo "  $L $(ls prolog/$L/*.pl 2>/dev/null | wc -l)" | tee -a "$LOG"; done
echo "---" | tee -a "$LOG"

for L in $LEGS; do
  TAG=$(echo "$L" | tr '/' '_')
  BEFORE=$(ls prolog/$L/*.pl 2>/dev/null | wc -l)
  echo "[$(date +%H:%M:%S)] $L before=$BEFORE" | tee -a "$LOG"
  $PY $D/eps_transition_map.py --corpus "$L" --json-out "$D/raw/tm_${TAG}.json" \
      > "$D/raw/out_${TAG}.txt" 2> "$D/raw/err_${TAG}.txt"
  RC=$?
  AFTER=$(ls prolog/$L/*.pl 2>/dev/null | wc -l)
  if [ "$BEFORE" -ne "$AFTER" ]; then
    echo "  VOID $L IN FLIGHT: count moved $BEFORE -> $AFTER; results dropped" | tee -a "$LOG"
    mv "$D/raw/tm_${TAG}.json" "$D/raw/VOID_tm_${TAG}.json" 2>/dev/null
  elif [ $RC -ne 0 ]; then
    echo "  FAILED rc=$RC (see raw/err_${TAG}.txt)" | tee -a "$LOG"
  else
    echo "  ok after=$AFTER | $(grep -m1 '^corpus=' $D/raw/out_${TAG}.txt)" | tee -a "$LOG"
    grep '^  control:' "$D/raw/out_${TAG}.txt" | tee -a "$LOG"
  fi
done
echo "[$(date +%H:%M:%S)] ALL DONE" | tee -a "$LOG"
