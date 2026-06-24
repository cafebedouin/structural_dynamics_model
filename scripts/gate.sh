#!/usr/bin/env bash
# [GATE] — run all project gate checks and print a green/red summary.
# Exit 1 if any check is red. Committed (travels with the repo); the `[GATE]`
# activation in CLAUDE.md runs this.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2

fail=0
run() {  # <name> <cmd...>
  local name="$1"; shift
  local out
  if out="$("$@" 2>&1)"; then
    printf '  \342\234\223 %-16s %s\n' "$name" "$(printf '%s' "$out" | tail -1)"
  else
    printf '  \342\234\227 %-16s %s\n' "$name" "$(printf '%s' "$out" | tail -1)"
    fail=1
  fi
}

echo "# Gate checks"
run "issues_status"  python3 python/issues_status.py --check
run "omega check"    python3 python/omega_resolver.py check
run "omega selftest" python3 python/omega_resolver.py selftest
run "omega index"    python3 python/omega_resolver.py index --check
run "known_state"    python3 python/known_state_status.py --check
run "axis boundary"  python3 python/check_axis_boundary.py --selftest
echo
if [ "$fail" = 0 ]; then echo "GATE: GREEN"; else echo "GATE: RED"; fi
exit "$fail"
