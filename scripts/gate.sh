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
run "spec enums"     python3 python/spec_enum_check.py --check
# Canonicity of the build-discipline taxonomy (OQ-278). CLAUDE.md and
# docs/technical/build_discipline.md publish the same numbered list and have disagreed at
# indices 3 and 4 since 220739b8 (2026-05-30) — undetected for 151 commits, because the
# member COUNTS converged at the exact commit the contents diverged. Compares names per
# index; collisions 3 and 4 are allowlisted with their state, so a silent resolution goes
# red too. Selftest (6 controls) rides --check, so this is one row, not two.
run "doc patterns"   python3 python/doc_pattern_check.py --check
run "claim cites"    python3 python/claim_cite_check.py --check
run "claim cites st" python3 python/claim_cite_check.py --selftest
run "known_state"    python3 python/known_state_status.py --check
run "axis boundary"  python3 python/check_axis_boundary.py --selftest
run "audit cites"    python3 python/audit_citation_status.py --check
run "audit writeup"  python3 python/audit_writeup_gate.py --check
run "apparatus"      python3 python/apparatus_instrument.py --check
run "gap surfaces"   python3 python/check_gap_status_surfaces.py
run "cli selftest"   python3 python/cli.py selftest
run "tripwire hook"  python3 python/pretooluse_tripwires.py --selftest
# RETIRE WHEN OQ-277 CLOSES (added 2026-08-11, operator ruling; expiry is deliberate).
# Standing detection that OQ-277's FROZEN preregistration has not been altered — a run was
# made under md5 4118f64e, so if the document changes, the stamp stops naming what is on
# disk and every result loses its pre-registration. Gated rather than checked on request
# because "when someone remembers to look" is the failure mode this arc is about.
# It also fails if the check stops being red-capable (its own selftest rides along).
# This is the one audit-specific entry here: when OQ-277 closes, delete this line and the
# tool, or promote it to a general frozen-artifact check if a second audit needs one.
# Next consolidation pass owns the call — see CLAUDE.md "Memory Consolidation Review".
run "oq277 freeze"   python3 python/audits/oq277_build_prereg.py --check
echo
if [ "$fail" = 0 ]; then echo "GATE: GREEN"; else echo "GATE: RED"; fi
exit "$fail"
