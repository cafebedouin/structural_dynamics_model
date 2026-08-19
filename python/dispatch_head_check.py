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
  unreached   expected to fire; the shape is present AND the predicate is CALLED BY NOTHING
              on any corpus this project has. A DIFFERENT FACT FROM latent-B, with a
              different remedy, and it gets its own disposition so the next reader does not
              inherit these as convertible-pending-effort (operator ruling, 2026-08-19).
              `latent-B` means "no live BOUND caller found"; this means "no live caller found
              AT ALL". Evidence: a six-leg profiler pass over the same run_json_report goal
              classify_corpus runs — testsets, testsets_haiku, testsets_flash, testsets_kimi,
              testsets_sonnet and archives/datasets/kernel_v1, 5,311 constraints — with
              hot-path controls firing at ~1.1M calls per leg. Zero calls on every leg.
              (audits/2026-08-18_classb_conversion_rollout/reachability.json.)
              DO NOT CONVERT THESE ON THE CLASS-B LICENCE. Their footing is adjudication plus
              the by-construction census with NO corpus leg at all — and the corpus is the
              only instrument in this chain that has ever caught a misconversion: both
              failure classes this OQ found (input-keyed rows, wrong-category rows) were
              caught by the corpus and missed by everything internal to the change.
              Converting here spends the one licence whose backing instrument is
              structurally silent. Same stale-note rule as latent-B.
  input-key   expected to fire; adjudicated: the last argument is an INPUT supplied by
              the caller by contract (availability key / test name / value to
              serialize) — not an engine answer. Same stale-note rule.
              THE CLASS-B TEMPLATE IS INVALID HERE and the failure is silent-then-loud:
              rewriting `p(C, sig) :- !, guard(C).` to `p(C, T) :- !, guard(C), T = sig.`
              makes the clause match EVERY second argument and cut, so every later clause
              becomes unreachable. Witnessed 2026-08-19: seat_overrides/2 and
              expected_power_divergence/4 were misfiled `latent-B`, converted, and moved 129
              and 17 live constraints (six-leg pair + per-file bisect,
              audits/2026-08-18_classb_conversion_rollout/).
              MECHANICAL CANDIDATE-FINDER, not a verdict: a clause whose body's FIRST goal is
              `!` commits before testing anything, so it is SELECTING on its head arguments.
              `audits/2026-08-18_classb_conversion_rollout/inputkey_screen.py` flags 23 of the
              58 latent-B rows on that tell; 2 are these, the other 21 are UNADJUDICATED
              candidates (a cut-first clause can still have a genuine output last argument —
              characterize_family/2 selects on arg 1). ADJUDICATE BEFORE CONVERTING ANY OF
              THEM; the `latent-B` label does not license the template on its own.
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
    ("abductive_helpers.pl", "fpn_band/2"): "unreached",
    ("abductive_helpers.pl", "seat_overrides/2"): "input-key",
    ("abductive_helpers.pl", "subsystem_available/1"): "input-key",
    ("boltzmann_compliance.pl", "epistemic_access_check/2"): "finding",
    ("boltzmann_compliance.pl", "expected_power_divergence/4"): "input-key",
    ("context_profile_mining.pl", "classify_isomorphism_level/2"): "unreached",
    ("covering_analysis.pl", "cell_short/2"): "unreached",
    ("covering_analysis.pl", "sigma_label/2"): "unreached",
    ("cs_kernel_registry.pl", "stance_member_provenance/3"): "unreached",
    ("cs_pattern_detection.pl", "cs_verdict/2"): "finding",
    ("diagnostic_summary.pl", "ds_subsystem_available/1"): "input-key",
    ("dirac_classification.pl", "gauge_fixed/3"): "unreached",
    ("domain_priors.pl", "infer_category_from_priors/2"): "unreached",
    ("drl_boltzmann_analysis.pl", "has_purity_drift/2"): "unreached",
    ("drl_boltzmann_analysis.pl", "purity_to_cut_priority/2"): "unreached",
    ("drl_boltzmann_analysis.pl", "qualify_action/5"): "unreached",
    ("drl_boltzmann_analysis.pl", "raw_urgency/4"): "unreached",
    ("drl_composition.pl", "composition_rule/3"): "unreached",
    ("drl_core.pl", "classify_from_metrics/6"): MUST_NOT_FIRE,
    ("drl_core.pl", "dr_action/3"): "unreached",
    ("drl_core.pl", "is_mountain/3"): "wrapper",
    ("drl_core.pl", "is_piton/3"): "wrapper",
    ("drl_core.pl", "is_rope/3"): "wrapper",
    ("drl_core.pl", "is_scaffold/3"): "wrapper",
    ("drl_core.pl", "is_snare/3"): "wrapper",
    ("drl_core.pl", "is_tangled_rope/3"): "wrapper",
    ("drl_counterfactual.pl", "estimate_impact_indexed/5"): "unreached",
    ("fpn_report.pl", "ep_band/2"): "unreached",
    ("gap_diagnostic.pl", "gate_description/2"): "unreached",
    ("giant_component_analysis.pl", "action_band/2"): "unreached",
    ("giant_component_analysis.pl", "would_cross_threshold/5"): "unreached",
    ("grothendieck_cohomology.pl", "classify_deltas/2"): "finding",
    ("invertibility_analysis.pl", "chi_subband/2"): "unreached",
    ("invertibility_analysis.pl", "predict_rope_snare/4"): "unreached",
    ("invertibility_analysis.pl", "predict_rope_tangled/4"): "unreached",
    ("invertibility_analysis.pl", "predict_snare_tangled/4"): "unreached",
    ("invertibility_analysis.pl", "predict_three_type/4"): "unreached",
    ("json_report.pl", "write_json_number/2"): "input-key",
    ("logical_fingerprint.pl", "extraction_zone/2"): "unreached",
    ("logical_fingerprint.pl", "structural_property_holds/2"): "unreached",
    ("logical_fingerprint.pl", "suppression_zone/2"): "unreached",
    ("maxent_report.pl", "entropy_interpretation/2"): "unreached",
    ("orbit_report.pl", "characterize_family/2"): "unreached",
    ("probe_oq197_controls.pl", "status_kind/2"): "unreached",
    ("report_generator.pl", "completeness_to_confidence/2"): "unreached",
    ("report_generator.pl", "generate_scenario_for_omega/5"): "input-key",
    ("report_generator.pl", "resolve_omega_source/4"): "unreached",
    ("routing_sink.pl", "detector_state/2"): "unreached",
    ("signature_detection.pl", "classify_by_signature/3"): MUST_NOT_FIRE,
    ("signature_detection.pl", "constraint_signature/2"): MUST_NOT_FIRE,
    ("signature_detection.pl", "determine_pure_subtype/2"): "unreached",
    ("transition_paths.pl", "predicted_terminal_state/3"): "unreached",
}

# ---------------------------------------------------------------------------
# LAST-ARGUMENT MODE, PER ROW, WITH ITS EVIDENCE.
#
# WHY THIS TABLE EXISTS (operator ruling, 2026-08-19). This file's header states the
# assumption the whole worklist rests on — "OUTPUT ARGUMENT is taken to be the LAST argument,
# by engine convention. This is a declared assumption, not a fact about every predicate" — and
# a header is exactly what let TWO violations sit inside the worklist unnoticed until a
# conversion moved 129-1106 constraints per leg. A stated assumption is not a checked one.
#
# THE FACT WAS ALREADY IN THE SOURCE. Both misfiled rows carried an authored mode line three
# lines above their clauses:
#     %% seat_overrides(+C, +Signature)
#     %% expected_power_divergence(+P1, +P2, +T1, +T2)
# Nothing read it. So the durable fix is not a better heuristic — it is putting the per-row
# fact where the checker can see it, with the evidence that settled it.
#
#   "output"    — the last argument carries an answer OUT; the class-B conversion template
#                 (fresh-variable heads + unify-after-cut) applies.
#   "input"     — the last argument is supplied BY THE CALLER; the template is INVALID and
#                 silently destroys the predicate (the clause matches every value, cuts, and
#                 makes every later clause unreachable). Such a row belongs in `input-key`.
#   "generator" — the predicate is NOT CUT-ORDERED DISPATCH AT ALL: it is enumerated for all
#                 solutions (its caller is a findall/setof/forall), so the last argument is an
#                 output but there is no bound-probe hazard for the template to retire. A
#                 FOURTH kind of misfiling, and deliberately not folded into "output"
#                 (operator, 2026-08-19): "this dispatches on an output" and "this was never
#                 dispatch" fail differently, and a future census must be able to tell them
#                 apart. Converting one is harmless and pointless; the interesting property of
#                 such a predicate is whether a cut in a late clause truncates the enumeration.
#
# Evidence is either the authored `%%` mode line or a dated read naming what settled it. A row
# RETIRED by conversion has its fact removed here in the converting change — the adjudication
# survives in audits/2026-08-18_classb_conversion_rollout/mode_adjudication.json and in the
# commit — so this table stays exactly co-extensive with the live worklist and the gate does
# not accumulate permanent notes that train readers to skip them.
# ADJUDICATION PASS: 2026-08-19, all 58 registry rows of the 2026-08-17 census
# (audits/2026-08-18_classb_conversion_rollout/mode_adjudication.py + hand reads for the 18
# rows carrying no mode line).
#
# THE FINDING THIS TABLE RECORDS IS NOT THE TABLE. Three rows were misfiled; two carried
# hand-authored `+` mode lines three lines above their clauses, and NO instrument in the chain
# read them — not the regex caller sweep, not the codewalk arm, not the clause-order census,
# not the cut-first screen. Two call-site instruments and a structural checker were built to
# answer a question the author had already answered in a comment. Where this codebase's
# DECLARED facts live and where its verification LOOKS are different places; that is the
# general lesson, and it is worth more than the rows.
#
# CROSS-CHECK BOUND, SCOPED (operator, 2026-08-19). Against the independent cut-first screen
# (inputkey_screen.py): 10 rows where the screen over-flags an authored output, and 0 rows
# where an author wrote `+` and the screen missed it. **That zero is scoped to the 37 of 55
# rows that carry an authored mode line** — it is a false-negative bound measured against the
# only instrument that can check the screen. The 18 hand-read rows have NO independent check
# on the screen at all; their evidence is the read, and the screen's behaviour on them is
# unbounded. Neither instrument clears a row alone.
# ---------------------------------------------------------------------------
LAST_ARG: dict[tuple[str, str], tuple[str, str]] = {
    ("abductive_helpers.pl", "fpn_band/2"):
        ("output", "authored: %% fpn_band(+EP, -Band)"),
    ("abductive_helpers.pl", "seat_overrides/2"):
        ("input", "authored: %% seat_overrides(+C, +Signature)"),
    ("boltzmann_compliance.pl", "expected_power_divergence/4"):
        ("input", "authored: %% expected_power_divergence(+P1, +P2, +T1, +T2)"),
    ("context_profile_mining.pl", "classify_isomorphism_level/2"):
        ("output", "read 2026-08-19: arg1 Evidence in, arg2 the level computed from it"),
    ("covering_analysis.pl", "cell_short/2"):
        ("output", "read 2026-08-19: arg1 cell(Power,Scope) selects, arg2 is the short label for it"),
    ("covering_analysis.pl", "sigma_label/2"):
        ("output", "authored: %% sigma_label(+Sigma, -Label)"),
    ("cs_kernel_registry.pl", "stance_member_provenance/3"):
        ("output", "authored: %% stance_member_provenance(+C, +Stance, -Prov)  — morphology_suggested | hand_declared."),
    ("dirac_classification.pl", "gauge_fixed/3"):
        ("output", "authored: %% gauge_fixed(+Constraint, +Context, -Fixed)"),
    ("domain_priors.pl", "infer_category_from_priors/2"):
        ("output", "read 2026-08-19: arg1 id in, arg2 the inferred category"),
    ("drl_boltzmann_analysis.pl", "has_purity_drift/2"):
        ("output", "authored: %% has_purity_drift(+C, -Detected)"),
    ("drl_boltzmann_analysis.pl", "purity_to_cut_priority/2"):
        ("output", "authored: %% purity_to_cut_priority(+Purity, -Priority)"),
    ("drl_boltzmann_analysis.pl", "qualify_action/5"):
        ("output", "authored: %% qualify_action(+BaseAction, +Purity, +C, -QAction, -Rationale)"),
    ("drl_boltzmann_analysis.pl", "raw_urgency/4"):
        ("output", "authored: %% raw_urgency(+Gap, +Pressure, +Reformability, -Urgency)"),
    ("drl_composition.pl", "composition_rule/3"):
        ("output", "authored: %% composition_rule(+Type1, +Type2, -CompositeType)"),
    ("drl_core.pl", "dr_action/3"):
        ("output", "read 2026-08-19: arg3 is the recommended action; callers bind it as output (drl_purity_network.pl:472, drl_boltzmann_analysis.pl:432)"),
    ("drl_counterfactual.pl", "estimate_impact_indexed/5"):
        ("output", "authored: %% estimate_impact_indexed(+Source, +Target, +Context, -Impact, -Reason)"),
    ("fpn_report.pl", "ep_band/2"):
        ("output", "authored: %% ep_band(+EP, -Band)"),
    ("gap_diagnostic.pl", "gate_description/2"):
        ("output", "read 2026-08-19: arg1 gate atom selects, arg2 is its prose description"),
    ("giant_component_analysis.pl", "action_band/2"):
        ("output", "authored: %% action_band(+P, -Band)"),
    ("giant_component_analysis.pl", "would_cross_threshold/5"):
        ("output", "authored: %% would_cross_threshold(+Target, +Source, +Dist, +Ctx, -Result)"),
    ("invertibility_analysis.pl", "chi_subband/2"):
        ("output", "read 2026-08-19: arg1 chi in, arg2 the band it falls in"),
    ("invertibility_analysis.pl", "predict_rope_snare/4"):
        ("output", "read 2026-08-19: arg4 is the predicted target type; call site invertibility_analysis.pl:277 passes reconstruct_from_type_only/5's own output var"),
    ("invertibility_analysis.pl", "predict_rope_tangled/4"):
        ("output", "read 2026-08-19: as predict_rope_snare/4; call site :283"),
    ("invertibility_analysis.pl", "predict_snare_tangled/4"):
        ("output", "read 2026-08-19: as predict_rope_snare/4; same lookup shape"),
    ("invertibility_analysis.pl", "predict_three_type/4"):
        ("output", "read 2026-08-19: as predict_rope_snare/4; call site :289"),
    ("logical_fingerprint.pl", "extraction_zone/2"):
        ("output", "authored: %% extraction_zone(+Epsilon, -Zone)"),
    ("logical_fingerprint.pl", "structural_property_holds/2"):
        ("generator", "read 2026-08-19: NOT dispatch. Sole caller is "
                      "findall(Prop, structural_property_holds(C, Prop), _) at "
                      "logical_fingerprint.pl:161, so every clause is enumerated and arg2 is "
                      "an output produced on backtracking. No bound-probe hazard exists here "
                      "for the template to retire — converting it is harmless and pointless. "
                      "The live question for this predicate is different: the late clauses "
                      "(:181 onward) carry cuts, which truncate the enumeration."),
    ("logical_fingerprint.pl", "suppression_zone/2"):
        ("output", "authored: %% suppression_zone(+Supp, -Zone)"),
    ("maxent_report.pl", "entropy_interpretation/2"):
        ("output", "read 2026-08-19: arg1 entropy in, arg2 the interpretation string"),
    ("orbit_report.pl", "characterize_family/2"):
        ("output", "authored: %% characterize_family(+Signature, -Description)"),
    ("probe_oq197_controls.pl", "status_kind/2"):
        ("output", "read 2026-08-19: arg1 status term selects, arg2 is its kind label"),
    ("report_generator.pl", "completeness_to_confidence/2"):
        ("output", "read 2026-08-19: arg1 score in, arg2 the confidence band"),
    ("report_generator.pl", "generate_scenario_for_omega/5"):
        ("input", "authored: %% generate_scenario_for_omega(+OmegaID, +Type, +Description, +Constraint, +GapPattern)"),
    ("report_generator.pl", "resolve_omega_source/4"):
        ("output", "authored: %% resolve_omega_source(+OmegaID, +Subject, -Constraint, -GapPattern)"),
    ("routing_sink.pl", "detector_state/2"):
        ("output", "read 2026-08-19: arg1 constraint in, arg2 the detector state"),
    ("signature_detection.pl", "determine_pure_subtype/2"):
        ("output", "authored: %% determine_pure_subtype(+C, -Subtype)"),
    ("transition_paths.pl", "predicted_terminal_state/3"):
        ("output", "authored: %% predicted_terminal_state(+ConstraintID, -State, -Confidence)"),
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

    # REGISTRY HYGIENE (2026-08-19): the last-argument fact is a precondition of the
    # `latent-B` class, not a nice-to-have. A row with no fact has not been adjudicated, and
    # `latent-B` would license the template on it.
    for key, cls in sorted(DECLARED.items()):
        if cls not in ("latent-B", "unreached"):
            continue
        fact = LAST_ARG.get(key)
        if fact is None:
            problems.append(
                f"{cls} {key[0]} {key[1]} has NO LAST_ARG fact — the class licenses the "
                f"conversion template, which is valid only if the last argument is an output. "
                f"Adjudicate and record it (with evidence) in the same change.")
        elif fact[0] == "generator":
            notes.append(
                f"{cls} {key[0]} {key[1]} is LAST_ARG=generator — not cut-ordered dispatch "
                f"(its caller enumerates it), so the conversion template retires no hazard "
                f"here. Not an error; do not fold it into the `output` population when "
                f"counting what the rollout covers.")
        elif fact[0] == "input":
            problems.append(
                f"{cls} {key[0]} {key[1]} is recorded LAST_ARG=input ({fact[1]}) — the "
                f"template is invalid for it; its class must be `input-key`, not `latent-B`.")
    for key, fact in sorted(LAST_ARG.items()):
        if key not in DECLARED:
            notes.append(f"stale LAST_ARG row (no registry entry): {key[0]} {key[1]}")

    for n in notes:
        print(f"  note: {n}")
    if problems:
        for p in sorted(problems):
            print(f"  {p}")
        print(f"dispatch_head_check: RED — {len(problems)} problem(s)")
        return 1
    n_declared = sum(1 for v in DECLARED.values() if v != MUST_NOT_FIRE)
    n_out = sum(1 for v in LAST_ARG.values() if v[0] == "output")
    n_in = sum(1 for v in LAST_ARG.values() if v[0] == "input")
    n_gen = sum(1 for v in LAST_ARG.values() if v[0] == "generator")
    print(f"  last-arg facts: {len(LAST_ARG)} row(s) adjudicated ({n_out} output, {n_in} "
          f"input, {n_gen} generator); every latent-B/unreached row carries one")
    print(f"dispatch_head_check: GREEN — {scanned} engine files, {len(hitset)} shape "
          f"hit(s) all declared ({n_declared} declared + "
          f"{sum(1 for v in DECLARED.values() if v == MUST_NOT_FIRE)} must-not-fire), "
          f"{len(readerr)} file(s) with read errors, selftest OK")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
