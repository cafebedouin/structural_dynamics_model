#!/usr/bin/env bash
# OQ-302 Phase 2 driver — one swipl process per corpus leg.
# Pre-flight per PREREGISTRATION §6.7: leg md5 fingerprinted AROUND the run,
# loaded count reconciled against the on-disk file count. Any mismatch -> that
# leg's output is void.
set -u
REPO=/home/scott/bin/structural_dynamics_model
DIR="$REPO/audits/2026-08-19_oq302_bound_false_repair"
TAG="${1:-phase2}"
OUT="$DIR/tsv_$TAG"
mkdir -p "$OUT"
LOG="$DIR/probe_run_$TAG.log"
: > "$LOG"

LEGS="testsets testsets_haiku testsets_flash testsets_kimi testsets_sonnet archives/datasets/kernel_v1"

fingerprint () {  # $1 = leg dir relative to prolog/
  find "$REPO/prolog/$1" -maxdepth 1 -name '*.pl' -type f | sort | xargs md5sum | md5sum | cut -d' ' -f1
}
filecount () { find "$REPO/prolog/$1" -maxdepth 1 -name '*.pl' -type f | wc -l; }

{
  echo "# OQ-302 Phase-2 probe run  tag=$TAG"
  echo "# started $(date -u +%Y-%m-%dT%H:%M:%SZ)  code=$(cd "$REPO" && git rev-parse --short HEAD) dirty=$(cd "$REPO" && git status --porcelain prolog/ | wc -l)"
  echo "# prereg md5: $(md5sum "$DIR/PREREGISTRATION.md" | cut -d' ' -f1)"
  echo
} >> "$LOG"

for LEG in $LEGS; do
  NAME=$(echo "$LEG" | tr '/' '_')
  PRE=$(fingerprint "$LEG"); NFILE=$(filecount "$LEG")
  echo "== leg=$LEG  files_on_disk=$NFILE  md5_before=$PRE" >> "$LOG"
  ( cd "$REPO/prolog" && timeout 5400 swipl -q -l stack.pl \
      -g "consult('$DIR/invariance_probe.pl'), probe_leg('$LEG', '$OUT/$NAME.tsv')" \
      -t "halt" ) >> "$LOG" 2>&1
  RC=$?
  POST=$(fingerprint "$LEG")
  echo "== leg=$LEG  rc=$RC  md5_after=$POST" >> "$LOG"
  if [ "$PRE" != "$POST" ]; then echo "!! VOID: corpus md5 moved during the run for $LEG" >> "$LOG"; fi
  echo >> "$LOG"
done
echo "# finished $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$LOG"
tail -5 "$LOG"
