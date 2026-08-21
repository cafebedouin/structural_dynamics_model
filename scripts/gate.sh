#!/usr/bin/env bash
# [GATE] — run all project gate checks and print a green/red summary.
# Exit 1 if any check is red. Committed (travels with the repo); the `[GATE]`
# activation in CLAUDE.md runs this.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2

# --- Interpreter resolution (2026-08-18) -------------------------------------------------
# ONE resolution point, not 22. This file used to hardcode bare `python3` at every row, so
# after the OS upgrade moved the system interpreter 3.10 -> 3.12 (stranding every pip
# package) the gate measured an EMPTY interpreter while the work ran in .venv. The same
# checker was red under `python3` and green under `.venv/bin/python` — interpreter
# selection, not content. Order: explicit override, then the repo venv, then system.
# The `python env` row below asserts whichever one wins can actually import what we import.
if [ -n "${SDM_PYTHON:-}" ]; then
  PY="$SDM_PYTHON"
elif [ -x ".venv/bin/python" ]; then
  PY=".venv/bin/python"
else
  PY="python3"
fi
if ! "$PY" -c 'import sys' >/dev/null 2>&1; then
  echo "FATAL: interpreter '$PY' is not runnable" >&2; exit 2
fi

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

echo "# Gate checks  [interpreter: $("$PY" -c 'import sys;print(sys.executable)')]"
# FIRST row on purpose: if this is red, later rows' reds may be downstream of a missing
# import rather than a real finding. Read it before believing anything below it.
run "python env"     "$PY" python/python_env_check.py --check
run "python env st"  "$PY" python/python_env_check.py --selftest
run "issues_status"  "$PY" python/issues_status.py --check
run "omega check"    "$PY" python/omega_resolver.py check
run "omega selftest" "$PY" python/omega_resolver.py selftest
run "omega index"    "$PY" python/omega_resolver.py index --check
run "spec enums"     "$PY" python/spec_enum_check.py --check
# Canonicity of the build-discipline taxonomy (OQ-278). CLAUDE.md and
# docs/technical/build_discipline.md publish the same numbered list and disagreed at indices
# 3 and 4 from 220739b8 (2026-05-30) until the 2026-08-17 ruling — undetected for 151 commits,
# because the member COUNTS converged at the exact commit the contents diverged. Compares
# names per index. The collision and spine-lag allowlists are now EMPTY, which is the strong
# state: nothing exempted, so any divergence is a new fork. Selftest (7 controls) rides
# --check, so this is one row, not two.
run "doc patterns"   "$PY" python/doc_pattern_check.py --check
# The bound-selector rule made mechanical (2026-08-17). A bound SELECTOR on a cut-ordered
# dispatch predicate skips earlier clauses' cuts and answers "satisfies that clause body",
# not "the engine assigns it" — over-permissive, so a bound ZERO is safe and a bound NONZERO
# is an artifact. This is a gate row rather than a documented rule because the rule WAS
# documented (build_discipline Pattern 7, written 2026-05-30 with this exact worked example)
# and inline-annotated at two sibling sites, and still left 5 bound-selector call sites —
# one of them feeding a reported FCR percentage.
# Discrimination record: RED (5 sites) at dcde9591, GREEN after the repairs in this change.
run "bound selector" "$PY" python/bound_selector_check.py --check
# DEFINITION-SITE sibling of the row above (2026-08-17, bound-dispatch audit). Flags any
# engine predicate whose heads carry the bound-probe shape (>= 2 same-position output
# atoms + cuts); keyed to where the invariant lives, so it catches contract-level bound
# selectors no call-site regex can see, and stops firing on a predicate once converted
# to fresh-variable heads + unify-after-cut. Red on an UNDECLARED new member and on a
# CONVERTED predicate firing again (revert detection). Membership registry + reasons in
# the checker. Discrimination record: fired on pre-fix classify_from_metrics/6 +
# constraint_signature/2, declined on dr_type/3 and on both post-fix
# (audits/2026-08-17_bound_dispatch_hardening/).
run "dispatch head"  "$PY" python/dispatch_head_check.py --check
# CALL-SITE arm of the same registry (2026-08-18, OQ-303(a) re-witness). The `latent-B` class
# label asserts "no live bound caller"; caller_sweep.py, the regex that produced it, missed one
# on the verdict_join headline path because the call ends its clause and its clause-head
# heuristic read the terminating `.` as a fact. This row is library(prolog_codewalk) over the
# loaded program — module-resolved, multi-line bodies, meta-called goals — and it exists to
# catch the NEXT such miss, not to memorialize that one. Red on a bound caller for any
# latent-B row not adjudicated in prolog/codewalk_caller_allowlist.txt, and red per-ATOM
# (over-permissiveness is atom-specific: signature_grade/2 is exact at `correction` on all five
# legs and diverges by 29-167 per leg at `commentary`). NOT a superset of the regex row above —
# disjoint blind spots, both directions witnessed (audits/2026-08-18_bound_caller_rewitness/).
# Discrimination record: RED naming signature_grade/2 with the allowlist row deleted, RED naming
# the uncovered atom with ATOMS narrowed to `commentary`, GREEN restored.
run "codewalk caller" "$PY" python/codewalk_caller_check.py --check
# Unswept consumers of a DISPLACED taxonomy member (OQ-278) — one manifest block per member,
# with the state that says WHY its citations are stale: `destructive-replace` was vacated
# (2026-08-11), `bound-probe` was renumbered 3 -> 7 (2026-08-17). build_discipline.md's
# consumer-sweep rules have fired three times on this one taxonomy, so this is an instrument
# rather than a fourth note (operator, 2026-08-14). Declaration-based: red on a NEW consumer,
# red on a silent repair that does not retire its manifest entry in the same change.
run "displaced cites" "$PY" python/pattern_citation_check.py --check
# OQ-68 made mechanical (2026-08-18). A cross-module `other:pred(...)` call reaches past
# other's export list; SWI permits it unconditionally, so an internal signature change fails
# SILENTLY at every bypass site and the blast radius is not enumerable without a sweep. This
# is a gate row rather than a documented rule because the corpus-schema half is opt-in in
# exactly the way reading_registry registration and the spec_enum sentinels are: a new
# predicate is unguarded until someone remembers it, and TWO members had already fallen out
# undetected (flat_control_of/2 declared nowhere engine-side; has_sunset_clause/1 :- dynamic
# but never :- multifile) — held correct only by every writing testset self-declaring.
# Arm D goes red when a schema predicate declared for load-correctness only ACQUIRES a
# consumer — declaring it turned undefined-throws into defined-but-empty-fails on legs with no
# writers (the OQ-66 shape), and "no consumer exists" is both the mitigation and the thing that
# stops being true silently. Arm C additionally BUYS BACK the typo detector that `:- multifile` silences: once a
# predicate is multifile, SWI stops warning on redefinition, and that warning was doing the
# job by accident. OQ-308 added arms E-H over prolog/schema_shape.txt: E closes the
# repo-wide resolved DECLARATION set (63) against that file in both directions and re-checks
# the allowlist derivation as an IFF; F conforms authored values against 54 enforced argument
# positions; G checks declared per-leg emptiness against the head census; H flags a
# narrative_ontology:P/N reference whose arity the namespace does not resolve. Arms F and G
# are DRIFT RATCHETS transcribed from the corpus, not specifications -- a green F/G means the
# schema has not changed unnoticed, NOT that it is right. --full is retired: --check now scans
# all five legs and is a strict superset of it. One run, all five legs: 16.7s (re-measured
# 2026-08-18 under .venv over a 279/960/960/1005/1001 corpus; the pre-OQ-308 numbers were
# 1.35s default-leg / 14.4s --full).
# Discrimination record: arm B fires at dc12bf5a^ and declines at dc12bf5a, differing by
# exactly {story_provenance/8, story_seed/3} — the pair that commit repaired — with three
# constant fires on both sides. Arms A and C verified red-capable by plant-and-restore on
# the live tree. Full record + the ruling: python/module_boundary_check.py docstring.
run "module bounds"  "$PY" python/module_boundary_check.py --check

# OQ-306. Guards the SHARE OVER TIME of non-story corpus members, which is the thing that
# went wrong: the *_contradictions.pl stratum grew 5 -> 22 -> 26 -> 27 inside
# manifest.n_constraints with nothing going red. A GROWING contaminant does not bias a rate
# by a constant, it rewrites a time series, so historical rates silently stop being
# comparable to current ones. Two arms: totality (any `unknown`/`dual_family` member is RED)
# and a per-leg stratum pin. Re-pinning is executor-licensed but REQUIRES a recorded cause
# and authorizer (R-A) — the stratum demonstrably moves with no commit to point at, so that
# cause field is the only record such a move will ever have.
run "corpus census" "$PY" python/corpus_census_check.py --check
run "claim cites"    "$PY" python/claim_cite_check.py --check
run "claim cites st" "$PY" python/claim_cite_check.py --selftest
run "known_state"    "$PY" python/known_state_status.py --check
run "axis boundary"  "$PY" python/check_axis_boundary.py --selftest
run "audit cites"    "$PY" python/audit_citation_status.py --check
run "paper carriage"  "$PY" python/amnesiac_carriage_check.py --check
run "audit writeup"  "$PY" python/audit_writeup_gate.py --check
run "apparatus"      "$PY" python/apparatus_instrument.py --check
run "gap surfaces"   "$PY" python/check_gap_status_surfaces.py
# Dated obligations turn RED on their day (OQ-317 ruling, 2026-08-19). Scans allowlist
# REVIEW-BY tokens AND ISSUES **Sunset:** lines on active entries — two surfaces on purpose,
# so neither obligation dies with the other's carrier (row removal in October must not
# silence the November socket disposition). Licensed responses to a red are in the checker
# docstring: review, or extend BY OPERATOR RULING recorded in the owning entry — never a
# silent date edit. Selftest rides --check: past fires, SAME-DAY fires (the boundary),
# future declines, malformed fires, closed-entry Sunset declines.
run "sunset"         "$PY" python/sunset_check.py
run "cli selftest"   "$PY" python/cli.py selftest
run "tripwire hook"  "$PY" python/pretooluse_tripwires.py --selftest
# RETIRE WHEN OQ-277 CLOSES (added 2026-08-11, operator ruling; expiry is deliberate).
# Standing detection that OQ-277's FROZEN preregistration has not been altered — a run was
# made under md5 4118f64e, so if the document changes, the stamp stops naming what is on
# disk and every result loses its pre-registration. Gated rather than checked on request
# because "when someone remembers to look" is the failure mode this arc is about.
# It also fails if the check stops being red-capable (its own selftest rides along).
# This is the one audit-specific entry here: when OQ-277 closes, delete this line and the
# tool, or promote it to a general frozen-artifact check if a second audit needs one.
# Next consolidation pass owns the call — see CLAUDE.md "Memory Consolidation Review".
run "oq277 freeze"   "$PY" python/audits/oq277_build_prereg.py --check
echo
if [ "$fail" = 0 ]; then echo "GATE: GREEN"; else echo "GATE: RED"; fi
exit "$fail"
