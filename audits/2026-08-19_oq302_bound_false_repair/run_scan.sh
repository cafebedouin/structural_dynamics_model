#!/usr/bin/env bash
# OQ-302 Phase 0 — run bound_false_scan.sh at HEAD and at the free control pair.
# Writes bound_false_scan.out. Re-runnable.
set -u
REPO=/home/scott/bin/structural_dynamics_model
DIR="$REPO/audits/2026-08-19_oq302_bound_false_repair"
SCAN="$DIR/bound_false_scan.sh"
TMP=$(mktemp -d)
cd "$REPO"
{
  echo "# OQ-302 Phase-0 enumeration — bound-\`false\` CALL-site scan"
  echo "# generated $(date -u +%Y-%m-%dT%H:%M:%SZ) by run_scan.sh -> bound_false_scan.sh"
  echo "# selection rule is stated in bound_false_scan.sh; /usr/bin/grep pinned."
  echo
  echo "## HEAD  ($(git log -1 --format='%h %ad %s' --date=short HEAD))"
  "$SCAN" prolog
  echo "count=$("$SCAN" prolog | wc -l)"
  echo
  for C in '0bfd3b31^' '0bfd3b31' 'a0e8d772^'; do
    d="$TMP/$(echo "$C" | tr '^' 'P')"
    mkdir -p "$d"
    git archive "$C" prolog | tar -x -C "$d"
    echo "## $C  ($(git log -1 --format='%h %ad %s' --date=short "$C"))"
    ( cd "$d" && "$SCAN" prolog )
    echo "count=$( cd "$d" && "$SCAN" prolog | wc -l )"
    echo
  done
} > "$DIR/bound_false_scan.out" 2>&1
rm -rf "$TMP"
cat "$DIR/bound_false_scan.out"
