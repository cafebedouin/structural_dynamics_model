#!/usr/bin/env python3
"""dispatch_head_check.py — gate row for the bound-dispatch DEFINITION-SITE shape.

Wraps prolog/dispatch_head_check.pl (read_term walker, never loads modules): flags any
engine predicate with >= 2 clauses carrying a same-position atom in the LAST (output)
argument of the head plus cuts — the shape on which a bound call answers "satisfies
this clause body in isolation" instead of "the engine assigns" (build_discipline ->
Pattern 7, seen from the definition side). Complements bound_selector_check.py (call
sites, registry-keyed): THIS row is keyed to where the invariant lives, sees callers no
regex can (contract-level bound selectors), and stops firing on a predicate once its
heads are converted to the immune idiom (fresh-variable heads + unify-after-cut,
dr_type/3).

DISCRIMINATION RECORD (audits/2026-08-17_bound_dispatch_hardening/, anchored to
content): the walker FIRED on pre-fix classify_from_metrics/6 and constraint_signature/2
at HEAD 9a5d8526 (census_checker_run1_HEAD_9a5d8526.txt), DECLINED on dr_type/3 in the
same run, and DECLINES on both post-fix (the conversion commit). The naturally-arising
pair: pre-fix commit = positive, fix commit = negative.

REGISTRY SEMANTICS (declaration-based, both directions red-capable):
  latent-B    expected to fire; class-B latent member (shape present, no live bound
              caller found in the 2026-08-17 caller sweep). Firing = OK. Stops firing
              -> stale note (converted/removed; retire the entry), exit 0.
  input-key   expected to fire; adjudicated: the last argument is an INPUT supplied by
              the caller by contract (availability key / test name / value to
              serialize) — not an engine answer. Same stale-note rule.
  wrapper     expected to fire; is_X/3 alias family — two-clause atom/fail wrappers of
              the (converted) cascade. Same stale-note rule.
  finding     expected to fire; carries a live or latent bound-caller finding recorded
              in the audit's RECON.md §4 — repair proposed, not yet landed.
  MUST-NOT-FIRE  a CONVERTED predicate. If it fires again, a revert re-armed the
              bound-probe shape -> RED naming the predicate.
A hit with NO registry entry -> RED (new member of the class; adjudicate + declare in
the same change, or convert it).

Usage:
    python3 python/dispatch_head_check.py --check     # selftest, then live sweep
    python3 python/dispatch_head_check.py --selftest  # fixtures only
    python3 python/dispatch_head_check.py --list      # every hit, classified
"""
from __future__ import annotations

import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
PROLOG = REPO / "prolog"
WALKER = PROLOG / "dispatch_head_check.pl"

MUST_NOT_FIRE = "MUST-NOT-FIRE"

# (file, name/arity) -> class. Census + adjudication: 2026-08-17 bound-dispatch audit
# (RECON.md §§1-4). Retire an entry when its predicate is converted or removed.
DECLARED: dict[tuple[str, str], str] = {
    ("abductive_helpers.pl", "fpn_band/2"): "latent-B",
    ("abductive_helpers.pl", "seat_overrides/2"): "latent-B",
    ("abductive_helpers.pl", "subsystem_available/1"): "input-key",
    ("boltzmann_compliance.pl", "epistemic_access_check/2"): "finding",
    ("boltzmann_compliance.pl", "expected_power_divergence/4"): "latent-B",
    ("constraint_indexing.pl", "restricted_classify/7"): "latent-B",
    ("context_profile_mining.pl", "classify_isomorphism_level/2"): "latent-B",
    ("covering_analysis.pl", "cell_short/2"): "latent-B",
    ("covering_analysis.pl", "sigma_label/2"): "latent-B",
    ("cs_kernel_registry.pl", "stance_member_provenance/3"): "latent-B",
    ("cs_pattern_detection.pl", "cs_verdict/2"): "finding",
    ("data_repair.pl", "source_class/2"): "latent-B",
    ("diagnostic_summary.pl", "compute_verdict/4"): "latent-B",
    ("diagnostic_summary.pl", "ds_subsystem_available/1"): "input-key",
    ("diagnostic_summary.pl", "mismatch_source/2"): "latent-B",
    ("dirac_classification.pl", "gauge_fixed/3"): "latent-B",
    ("dirac_classification.pl", "score_to_separability/2"): "latent-B",
    ("dirac_classification.pl", "type_to_dirac_class/4"): "latent-B",
    ("domain_priors.pl", "category_of/2"): "latent-B",
    ("domain_priors.pl", "infer_category_from_priors/2"): "latent-B",
    ("drl_boltzmann_analysis.pl", "has_purity_drift/2"): "latent-B",
    ("drl_boltzmann_analysis.pl", "purity_to_cut_priority/2"): "latent-B",
    ("drl_boltzmann_analysis.pl", "qualify_action/5"): "latent-B",
    ("drl_boltzmann_analysis.pl", "raw_urgency/4"): "latent-B",
    ("drl_composition.pl", "composition_rule/3"): "latent-B",
    ("drl_core.pl", "classify_from_metrics/6"): MUST_NOT_FIRE,
    ("drl_core.pl", "dr_action/3"): "latent-B",
    ("drl_core.pl", "is_mountain/3"): "wrapper",
    ("drl_core.pl", "is_piton/3"): "wrapper",
    ("drl_core.pl", "is_rope/3"): "wrapper",
    ("drl_core.pl", "is_scaffold/3"): "wrapper",
    ("drl_core.pl", "is_snare/3"): "wrapper",
    ("drl_core.pl", "is_tangled_rope/3"): "wrapper",
    ("drl_counterfactual.pl", "estimate_impact_indexed/5"): "latent-B",
    ("fpn_report.pl", "ep_band/2"): "latent-B",
    ("gap_diagnostic.pl", "gate_description/2"): "latent-B",
    ("giant_component_analysis.pl", "action_band/2"): "latent-B",
    ("giant_component_analysis.pl", "would_cross_threshold/5"): "latent-B",
    ("grothendieck_cohomology.pl", "classify_deltas/2"): "finding",
    ("invertibility_analysis.pl", "chi_subband/2"): "latent-B",
    ("invertibility_analysis.pl", "predict_rope_snare/4"): "latent-B",
    ("invertibility_analysis.pl", "predict_rope_tangled/4"): "latent-B",
    ("invertibility_analysis.pl", "predict_snare_tangled/4"): "latent-B",
    ("invertibility_analysis.pl", "predict_three_type/4"): "latent-B",
    ("json_report.pl", "boltzmann_label/2"): "latent-B",
    ("json_report.pl", "live_index_label/3"): "latent-B",
    ("json_report.pl", "write_json_number/2"): "input-key",
    ("logical_fingerprint.pl", "categorize_coupling/5"): "latent-B",
    ("logical_fingerprint.pl", "extraction_zone/2"): "latent-B",
    ("logical_fingerprint.pl", "purity_zone/2"): "latent-B",
    ("logical_fingerprint.pl", "structural_property_holds/2"): "latent-B",
    ("logical_fingerprint.pl", "suppression_zone/2"): "latent-B",
    ("maxent_report.pl", "entropy_interpretation/2"): "latent-B",
    ("metric_drift_events.pl", "drift_severity/3"): "latent-B",
    ("network_dynamics.pl", "ep_base_severity/2"): "latent-B",
    ("orbit_report.pl", "characterize_family/2"): "latent-B",
    ("probe_oq197_controls.pl", "status_kind/2"): "latent-B",
    ("report_generator.pl", "completeness_to_confidence/2"): "latent-B",
    ("report_generator.pl", "generate_scenario_for_omega/5"): "latent-B",
    ("report_generator.pl", "omega_severity/2"): "latent-B",
    ("report_generator.pl", "resolve_omega_source/4"): "latent-B",
    ("routing_sink.pl", "detector_state/2"): "latent-B",
    ("signature_detection.pl", "appears_as_rope/2"): "latent-B",
    ("signature_detection.pl", "capture_disposition/2"): "latent-B",
    ("signature_detection.pl", "claimed_natural/2"): "latent-B",
    ("signature_detection.pl", "classify_by_signature/3"): MUST_NOT_FIRE,
    ("signature_detection.pl", "constraint_signature/2"): MUST_NOT_FIRE,
    ("signature_detection.pl", "determine_pure_subtype/2"): "latent-B",
    ("signature_detection.pl", "has_viable_alternatives/2"): "latent-B",
    ("signature_detection.pl", "resolve_with_perspectival_check/4"): "latent-B",
    ("signature_detection.pl", "signature_diagnostic_severity/3"): "latent-B",
    ("transition_paths.pl", "predicted_terminal_state/3"): "latent-B",
}

HIT_RE = re.compile(r"^DHC_HIT: (\S+) (\S+/\d+) ")


def run_walker(directory: str | None = None) -> tuple[list[tuple[str, str]], list[str], int, list[str]]:
    """Returns (hits, readerr_lines, n_scanned, raw_lines). Fails closed on a broken run."""
    goal = ("run_dispatch_head_check" if directory is None
            else f"run_dispatch_head_check('{directory}')")
    proc = subprocess.run(
        ["swipl", "-q", "-l", str(WALKER), "-g", f"{goal}, halt", "-t", "halt(1)"],
        cwd=PROLOG, capture_output=True, text=True, timeout=300)
    lines = proc.stdout.splitlines()
    hits = [(m.group(1), m.group(2)) for ln in lines if (m := HIT_RE.match(ln))]
    readerr = [ln for ln in lines if ln.startswith("DHC_READERR:")]
    scanned = 0
    for ln in lines:
        if ln.startswith("DHC_SCANNED:"):
            scanned = int(ln.split()[1])
    if proc.returncode != 0 or scanned == 0:
        raise SystemExit(
            f"dispatch_head_check: RED — walker did not complete (rc={proc.returncode}, "
            f"scanned={scanned}). stderr: {proc.stderr[-500:]}")
    return hits, readerr, scanned, lines


# ---------------------------------------------------------------------------
# Selftest fixtures: written to a temp dir and swept by the REAL walker (same path).
# ---------------------------------------------------------------------------
FIXTURES: list[tuple[str, str, bool]] = [
    # (label, prolog_text, expect_hit)
    ("pre-fix bound-probe shape fires",
     "shape_a(C, foo) :- cond1(C), !.\n"
     "shape_a(C, bar) :- cond2(C), !.\n"
     "shape_a(_, baz).\n", True),
    ("immune idiom (fresh-var head + unify-after-cut) declines",
     "shape_b(C, T) :- cond1(C), !, T = foo.\n"
     "shape_b(C, T) :- cond2(C), !, T = bar.\n"
     "shape_b(_, baz).\n", False),          # one atom head < 2
    ("post-fix shape (all fresh-var heads) declines",
     "shape_c(C, T) :- cond1(C), !, T = foo.\n"
     "shape_c(_, T) :- T = unknown.\n", False),
    ("fact table without cuts declines",
     "shape_d(a, foo).\nshape_d(b, bar).\nshape_d(c, baz).\n", False),
    ("two atom heads but zero cuts declines",
     "shape_e(C, foo) :- cond1(C).\nshape_e(C, bar) :- cond2(C).\n", False),
]
SYNTAX_ERR_FIXTURE = "broken(X :- foo.\n"   # must yield DHC_READERR, not silence


def selftest() -> list[str]:
    fails: list[str] = []
    with tempfile.TemporaryDirectory(prefix="dhc_selftest_") as td:
        tdp = Path(td)
        for i, (_, text, _) in enumerate(FIXTURES):
            (tdp / f"fixture_{i}.pl").write_text(text)
        (tdp / "fixture_readerr.pl").write_text(SYNTAX_ERR_FIXTURE)
        try:
            hits, readerr, scanned, _ = run_walker(td)
        except SystemExit as e:
            return [f"SELFTEST walker failed on fixture dir: {e}"]
        hitpreds = {p for _, p in hits}
        for i, (label, _, expect) in enumerate(FIXTURES):
            pred = f"shape_{chr(ord('a') + i)}"
            got = any(h.startswith(pred + "/") for h in hitpreds)
            if got != expect:
                fails.append(f"SELFTEST {label}: expected hit={expect}, got hit={got}")
        if not any("fixture_readerr.pl" in ln for ln in readerr):
            fails.append("SELFTEST syntax-error fixture produced no DHC_READERR — "
                         "read errors would be silent")
        if scanned != len(FIXTURES) + 1:
            fails.append(f"SELFTEST scanned {scanned} != {len(FIXTURES) + 1} fixtures")
    # Empty-scan fail-loud (Pattern 5): the walker must refuse a 0-file directory.
    with tempfile.TemporaryDirectory(prefix="dhc_empty_") as td:
        try:
            run_walker(td)
            fails.append("SELFTEST empty dir did not fail loud")
        except SystemExit:
            pass
    return fails


def main(argv: list[str]) -> int:
    st = selftest()
    if st:
        for f in st:
            print(f"  {f}")
        print("dispatch_head_check: RED (selftest)")
        return 1
    if "--selftest" in argv:
        print(f"dispatch_head_check: selftest OK ({len(FIXTURES)} shape fixtures + "
              f"read-error control + empty-scan control)")
        return 0

    hits, readerr, scanned, _ = run_walker()
    if "--list" in argv:
        for f, p in sorted(hits):
            cls = DECLARED.get((f, p), "UNDECLARED")
            print(f"{cls:14} {f} {p}")
        return 0

    problems, notes = [], []
    hitset = set(hits)
    for key in hitset:
        cls = DECLARED.get(key)
        if cls is None:
            problems.append(f"NEW bound-dispatch shape: {key[0]} {key[1]} — adjudicate "
                            f"and declare (or convert) in the same change")
        elif cls == MUST_NOT_FIRE:
            problems.append(f"REVERT? converted predicate fires again: {key[0]} {key[1]}")
    for key, cls in DECLARED.items():
        if cls != MUST_NOT_FIRE and key not in hitset:
            notes.append(f"stale registry entry (no longer fires): {key[0]} {key[1]}")
    for ln in readerr:
        notes.append(ln + " — unparsed terms are unchecked terms")

    for n in notes:
        print(f"  note: {n}")
    if problems:
        for p in sorted(problems):
            print(f"  {p}")
        print(f"dispatch_head_check: RED — {len(problems)} problem(s)")
        return 1
    n_declared = sum(1 for v in DECLARED.values() if v != MUST_NOT_FIRE)
    print(f"dispatch_head_check: GREEN — {scanned} engine files, {len(hitset)} shape "
          f"hit(s) all declared ({n_declared} declared + "
          f"{sum(1 for v in DECLARED.values() if v == MUST_NOT_FIRE)} must-not-fire), "
          f"{len(readerr)} file(s) with read errors, selftest OK")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
