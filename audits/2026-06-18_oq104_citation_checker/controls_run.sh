#!/usr/bin/env bash
# controls_run.sh — whole-run controls for audit_citation_status.py (OQ-104):
# idempotence + rot-sensitivity (a tracked cited file that becomes untracked MUST flip
# pass -> flag). A checker byte-identical because it "stopped looking" fails the second.
# Self-contained and reproducible: creates fixtures under a temp audit dir, exercises the
# real CLI, then removes them. Run from repo root.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

CHK="python3 python/audit_citation_status.py"
FIX="audits/2026-06-18_oq104_rot_fixture"
CITE="$FIX/rot_evidence.txt"

cleanup() {
  git rm -q --cached "$CITE" 2>/dev/null || true
  git rm -q --cached "$FIX/probe.md" 2>/dev/null || true
  rm -rf "$FIX"
}
trap cleanup EXIT

echo "=== idempotence ==="
$CHK >/tmp/r1.out 2>/tmp/r1.err; $CHK >/tmp/r2.out 2>/tmp/r2.err
if diff -q /tmp/r1.out /tmp/r2.out >/dev/null && diff -q /tmp/r1.err /tmp/r2.err >/dev/null; then
  echo "PASS: two runs byte-identical (stdout+stderr)"
else
  echo "FAIL: runs differ"; diff /tmp/r1.out /tmp/r2.out || true; exit 1
fi

echo "=== rot-sensitivity ==="
mkdir -p "$FIX"
printf 'rot fixture evidence\n' > "$CITE"
printf 'cite: `%s`\n' "$CITE" > "$FIX/probe.md"
git add "$CITE" "$FIX/probe.md"

echo "--- tracked: expect NO flag for $CITE"
if $CHK --file "$FIX" 2>/dev/null | grep -q "$CITE"; then
  echo "FAIL: tracked cited file was flagged"; exit 1
else
  echo "PASS: tracked cited file passes (not flagged)"
fi

git rm -q --cached "$CITE"   # now untracked, still on disk
echo "--- untracked (git rm --cached): expect untracked-pending flag for $CITE"
if $CHK --file "$FIX" 2>/dev/null | grep -E "^untracked-pending.*$CITE" ; then
  echo "PASS: pass -> flag flip witnessed (rot-sensitive)"
else
  echo "FAIL: rot NOT detected — checker stopped looking"; exit 1
fi

echo "--- ALL whole-run controls passed"
