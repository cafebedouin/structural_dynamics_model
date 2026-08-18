"""
audit3_synthesis.py — Synthesis for Audit 3 (profile accumulation corpus impact).

Inputs:
  outputs/audit3_maxent_raw.json  — Prolog comparison output (both sessions)
  outputs/audit3_sample.json      — stratified sample IDs
  outputs/abductive_data.json     — clean-session trigger results (ground truth)
  outputs/pipeline_output.json    — per-constraint non-MaxEnt data

Produces:
  outputs/audit3_report.md        — three required synthesis outputs

Tasks:
  1. Population-level top-type and trigger-firing divergence counts
  2. Trigger-class impact table (six conditions)
  3. Wasserstein dependency (code-reading from source files)
"""

import json
import os
import re
import sys
from collections import defaultdict, Counter

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

MAXENT_RAW_PATH  = os.path.join(REPO_ROOT, "outputs", "audit3_maxent_raw.json")
SAMPLE_PATH      = os.path.join(REPO_ROOT, "outputs", "audit3_sample.json")
ABD_PATH         = os.path.join(REPO_ROOT, "outputs", "abductive_data.json")
PIPELINE_PATH    = os.path.join(REPO_ROOT, "outputs", "pipeline_output.json")
CONFIG_PATH      = os.path.join(REPO_ROOT, "prolog", "config.pl")
MEAS_LAYER_PATH  = os.path.join(REPO_ROOT, "prolog", "measurement_layer.pl")
MAXENT_CLS_PATH  = os.path.join(REPO_ROOT, "prolog", "maxent_classifier.pl")
OUT_PATH         = os.path.join(REPO_ROOT, "outputs", "audit3_report.md")


# Override target table — from abductive_helpers.pl:70-80
# OQ-296 (confirmed 2026-08-18): the "natural_law" and "coordination_scaffold"
# keys below are dead — never produced by the engine. Mirrors
# abductive_helpers.pl override_target/2. Any count keyed on them is
# structurally 0, not a measured absence.
OVERRIDE_TARGET = {
    "false_natural_law":          "tangled_rope",
    "false_ci_rope":              "tangled_rope",
    "false_summit_mountain":      "tangled_rope",
    "coupling_invariant_rope":    "rope",
    "natural_law":                "mountain",
    "coordination_scaffold":      "rope",
    "constructed_low_extraction": "rope",
    "constructed_high_extraction":"tangled_rope",
    "constructed_constraint":     "tangled_rope",
}
KNOWN_OVERRIDE_SIGS = set(OVERRIDE_TARGET.keys())

# Residual types that produce residual_override (not hard) in maxent_disagreement
RESIDUAL_TYPES = {"unknown", "naturalized"}


def load_json(path):
    print(f"[synthesis] Loading {os.path.basename(path)}...", file=sys.stderr)
    with open(path) as f:
        return json.load(f)


def read_threshold_from_config(param_name, config_text):
    """Extract numeric value of a param/2 fact from config.pl text."""
    pattern = rf"param\({re.escape(param_name)},\s*([\d.]+)\)"
    m = re.search(pattern, config_text)
    if m:
        return float(m.group(1))
    raise ValueError(f"Threshold '{param_name}' not found in config.pl")


def read_string_from_config(param_name, config_text):
    """Extract atom value of a param/2 fact from config.pl text."""
    pattern = rf"param\({re.escape(param_name)},\s*(\w+)\)"
    m = re.search(pattern, config_text)
    if m:
        return m.group(1)
    raise ValueError(f"Param '{param_name}' not found in config.pl")


def get_det_type_analytical(c_data):
    """Extract deterministic type at analytical context from pipeline_output classifications."""
    for cl in c_data.get("classifications", []):
        ctx = cl.get("context", {})
        if ctx.get("agent_power") == "analytical":
            return cl.get("type", "unknown")
    return "unknown"


def compute_non_maxent_signals(c_data, purity_thresh, coupling_thresh, drift_mode):
    """Compute stress signals that don't depend on MaxEnt session."""
    signals = []
    sig = c_data.get("signature", "")
    if sig in ("false_ci_rope", "false_natural_law"):
        signals.append("has_false_signature")
    purity = c_data.get("purity_score", 1.0)
    if isinstance(purity, (int, float)) and purity >= 0 and purity < purity_thresh:
        signals.append("low_purity")
    drift = c_data.get("drift_events", [])
    if drift_mode == "any" and isinstance(drift, list) and len(drift) > 0:
        signals.append("has_drift")
    elif drift_mode == "critical" and any(e.get("severity") == "critical" for e in (drift or [])):
        signals.append("has_drift")
    elif drift_mode == "count_2plus" and isinstance(drift, list) and len(drift) >= 2:
        signals.append("has_drift")
    coupling_obj = c_data.get("coupling", {})
    if isinstance(coupling_obj, dict):
        coupling_score = coupling_obj.get("score")
        if isinstance(coupling_score, (int, float)) and coupling_score > coupling_thresh:
            signals.append("high_coupling")
    return signals


def eval_triggers_under_session(cid, maxent_row, pipeline_lookup, abd_clean_triggers,
                                 entropy_thresh, uncertainty_thresh, shadow_thresh,
                                 stress_min, purity_thresh, coupling_thresh, drift_mode,
                                 session):
    """
    Evaluate which of the 6 trigger conditions fire under `session` ('clean' or 'accum').

    Returns a dict: {condition_name: bool}
    Conditions: t1, t4, t9, elevated_entropy, maxent_hard_disagree, t10
    """
    if session == "clean":
        top_type  = maxent_row.get("clean_top_type", "missing")
        H         = maxent_row.get("clean_H", -1.0)
        P_top     = maxent_row.get("clean_P_top", -1.0)
        hard_d    = maxent_row.get("clean_hard_disagree", False)
    else:
        top_type  = maxent_row.get("accum_top_type", "missing")
        H         = maxent_row.get("accum_H", -1.0)
        P_top     = maxent_row.get("accum_P_top", -1.0)
        hard_d    = maxent_row.get("accum_hard_disagree", False)

    c_data = pipeline_lookup.get(cid, {})
    sig    = c_data.get("signature", "")
    has_override_sig = sig in KNOWN_OVERRIDE_SIGS
    override_target  = OVERRIDE_TARGET.get(sig)

    h1 = c_data.get("h1_band", 0)
    h1_val = h1 if isinstance(h1, (int, float)) else 0

    # ── Condition booleans ──

    # elevated_entropy: H > entropy_thresh
    elevated_entropy = H >= 0 and H > entropy_thresh

    # maxent_hard_disagree: hard disagreement fires
    maxent_hard_disagree = bool(hard_d)

    # T1: hard_disagree AND has_override_sig AND det_type == override_target(sig)
    # For T1, det_type comes from drl_core:dr_type (includes signature overrides).
    # The hard_disagree flag from Prolog already encodes MaxEntTop \= DetType.
    # We approximate det_type == override_target by: has_override_sig AND hard_d fires.
    # (When a signature is active and override applies, det_type = override_target.)
    t1 = has_override_sig and bool(hard_d)

    # T4: H > uncertainty_thresh AND orbit AND drift AND NOT T1
    # orbit and drift are session-independent; use abductive_data for clean-session T4,
    # and H threshold check for accumulated.
    if session == "clean":
        t4 = cid in abd_clean_triggers.get("confirmed_liminal", set())
    else:
        # T4 can only flip off (H drops, or T1 fires) or flip on (H rises and orbit+drift).
        # For constraints already known to have orbit+drift (those in T4 clean set),
        # check H > threshold and NOT T1.
        drift_ok  = len(c_data.get("drift_events", []) or []) > 0
        # We don't have orbit data in pipeline_output; conservatively require that the
        # constraint was T4-eligible in clean (orbit verified) or flag as uncertain.
        was_t4_clean = cid in abd_clean_triggers.get("confirmed_liminal", set())
        if was_t4_clean:
            t4 = H >= 0 and H > uncertainty_thresh and not t1
        else:
            # Potential new T4 in accum: flag if H > threshold but mark orbit unverified
            t4 = False  # conservative — orbit not verified from pipeline_output.json

    # T9: top_type \= override_target AND P_top > shadow_thresh AND NOT T1
    if has_override_sig and override_target is not None:
        t9 = (top_type != override_target and
              P_top >= 0 and P_top > shadow_thresh and
              not t1)
    else:
        t9 = False

    # T10: ≥ stress_min stress signals AND rare gate AND NOT T1
    non_maxent = compute_non_maxent_signals(c_data, purity_thresh, coupling_thresh, drift_mode)
    n_signals = len(non_maxent) + (1 if elevated_entropy else 0)
    rare_gate = maxent_hard_disagree or (h1_val >= 4)
    t10 = n_signals >= stress_min and rare_gate and not t1

    return {
        "t1":                  t1,
        "t4":                  t4,
        "t9":                  t9,
        "elevated_entropy":    elevated_entropy,
        "maxent_hard_disagree":maxent_hard_disagree,
        "t10":                 t10,
    }


def session_delta(clean_fires, accum_fires):
    if clean_fires and not accum_fires:
        return "clean_only"
    if not clean_fires and accum_fires:
        return "accum_only"
    if clean_fires and accum_fires:
        return "both"
    return "neither"


def extract_prolog_clause(text, head_pattern):
    """
    Extract the first Prolog clause whose head matches head_pattern (regex).
    Returns the full clause from the head to the terminating period, verbatim.
    """
    # Find head, then read until the clause-terminating period (period at end of line
    # or standalone period not inside a compound term).
    m = re.search(head_pattern + r"\s*:-", text)
    if not m:
        return None
    start = m.start()
    # Find the terminating '.' — it must be a standalone period (not inside parens or a number).
    # Heuristic: count unmatched parens; period when depth=0 and at end of meaningful text.
    depth = 0
    i = start
    while i < len(text):
        ch = text[i]
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        elif ch == '.' and depth == 0:
            # Check it's not a module-qualified name (preceded by identifier char)
            # or a float literal. A clause-ending period is followed by whitespace or EOF.
            if i + 1 >= len(text) or text[i + 1] in (' ', '\n', '\r', '\t', '%', ''):
                return text[start:i + 1].strip()
        i += 1
    return text[start:].strip()


def task3_wasserstein_dependency():
    """
    Task 3: Read source files; enumerate MaxEnt predicate calls in Wasserstein
    computation path; determine indexing structure; return one-sentence verdict.
    """
    with open(MEAS_LAYER_PATH) as f:
        meas_text = f.read()
    with open(MAXENT_CLS_PATH) as f:
        cls_text = f.read()

    # Extract wasserstein_edge_transport/4 verbatim (predicate definition only)
    edge_transport_code = extract_prolog_clause(
        meas_text, r"wasserstein_edge_transport\(C,\s*Ctx1,\s*Ctx2,\s*W1\)")
    if edge_transport_code is None:
        edge_transport_code = "(not found — check measurement_layer.pl)"

    # Extract wasserstein_transport_profile/2 verbatim
    transport_profile_code = extract_prolog_clause(
        meas_text, r"wasserstein_transport_profile\(C,\s*Profile\)")
    if transport_profile_code is None:
        transport_profile_code = "(not found — check measurement_layer.pl)"

    # Find all maxent-related predicate calls in the Wasserstein path
    wasserstein_block = edge_transport_code + "\n" + transport_profile_code
    maxent_calls_in_wasserstein = re.findall(
        r"maxent_classifier:\w+\(", wasserstein_block)
    maxent_calls_in_wasserstein = sorted(set(maxent_calls_in_wasserstein))

    # All maxent refs in measurement_layer (module-level awareness)
    all_maxent_refs = re.findall(r"maxent_\w+", meas_text)
    unique_maxent_refs = sorted(set(all_maxent_refs))

    # Count how many edge transport calls are in wasserstein_transport_profile
    n_edge_calls = len(re.findall(r"wasserstein_edge_transport\(", transport_profile_code))

    # Extract maxent_distribution/3 definition from maxent_classifier.pl
    dist_def_code = extract_prolog_clause(
        cls_text, r"maxent_distribution\(C,\s*Context,\s*Dist\)")
    if dist_def_code is None:
        dist_def_code = "(not found — check maxent_classifier.pl)"

    # Extract maxent_dist/3 assertz call (shows how it is stored and keyed)
    maxent_dist_assert = re.search(
        r"assertz\(maxent_dist\([^\)]+\)\)", cls_text)
    maxent_dist_args = maxent_dist_assert.group(0) if maxent_dist_assert else "(assertz not found)"

    # Extract maxent_profile/3 assertz call
    maxent_profile_assert = re.search(
        r"assertz\(maxent_profile\([^\)]+\)\)", cls_text)
    maxent_profile_args = maxent_profile_assert.group(0) if maxent_profile_assert else "(assertz not found)"

    # Dynamic fact declarations
    dynamic_decls = re.findall(r":- dynamic\s+maxent_\w+/\d+\.", cls_text)

    findings = {
        "edge_transport_code":              edge_transport_code,
        "transport_profile_code":           transport_profile_code,
        "maxent_distribution_definition":   dist_def_code,
        "maxent_dist_assertz":              maxent_dist_args,
        "maxent_profile_assertz":           maxent_profile_args,
        "maxent_calls_in_wasserstein":      maxent_calls_in_wasserstein,
        "unique_maxent_refs_in_meas_layer": unique_maxent_refs,
        "n_edge_calls_in_transport_profile":n_edge_calls,
        "dynamic_declarations":             dynamic_decls,
    }

    # Derive verdict from code evidence
    # maxent_dist/3: keyed by (Constraint, Context) — determined from assertz call
    dist_is_context_indexed = "Context" in maxent_dist_args
    # maxent_profile/3: keyed by (Type, MetricName) — no Context in key
    profile_has_context = "Context" in maxent_profile_args

    # wasserstein_transport_profile needs n_edge_calls edge transports, each needing
    # distributions at 2 distinct contexts. With 3 edges across 4 contexts, all 4
    # contexts must be populated simultaneously.
    contexts_needed = n_edge_calls + 1 if n_edge_calls >= 1 else "unknown"

    if dist_is_context_indexed and n_edge_calls >= 3:
        verdict = (
            f"Wasserstein reads from `maxent_dist/3` (via thin wrapper `maxent_distribution/3` "
            f"at measurement_layer.pl:214–215, confirmed: `{dist_def_code}`); "
            f"`wasserstein_transport_profile/2` makes {n_edge_calls} calls to "
            f"`wasserstein_edge_transport`, requiring distributions at all 4 canonical "
            f"contexts simultaneously in `maxent_dist/3`, so a cleanup-between-contexts "
            f"fix is NOT safe for Wasserstein without redesigning distribution storage."
        )
    elif dist_is_context_indexed and n_edge_calls >= 1:
        verdict = (
            f"Wasserstein reads from `maxent_dist/3` via `maxent_distribution/3` "
            f"(context-indexed); transport profile needs {n_edge_calls} edge transports "
            f"across {contexts_needed} contexts; cleanup-between-contexts safety: "
            f"NOT safe if any edge spans a context that would be cleared."
        )
    else:
        verdict = (
            f"Wasserstein reads from `maxent_distribution/3` (measurement_layer.pl). "
            f"maxent_dist assertz: {maxent_dist_args}. "
            f"context_indexed={dist_is_context_indexed}. "
            f"n_edge_calls={n_edge_calls}. Manual review required."
        )

    return findings, verdict


def main():
    # ── Load data ──
    with open(CONFIG_PATH) as f:
        config_text = f.read()

    entropy_thresh    = read_threshold_from_config("abductive_stress_entropy_threshold", config_text)
    uncertainty_thresh= read_threshold_from_config("maxent_uncertainty_threshold",       config_text)
    shadow_thresh     = read_threshold_from_config("abductive_shadow_divergence_threshold", config_text)
    purity_thresh     = read_threshold_from_config("abductive_stress_purity_threshold",  config_text)
    coupling_thresh   = read_threshold_from_config("abductive_stress_coupling_threshold", config_text)
    stress_min        = int(read_threshold_from_config("abductive_stress_convergence_min", config_text))
    drift_mode        = read_string_from_config("abductive_stress_drift_mode",          config_text)

    print(f"[synthesis] Thresholds from config.pl:", file=sys.stderr)
    print(f"  entropy_thresh={entropy_thresh}  uncertainty_thresh={uncertainty_thresh}", file=sys.stderr)
    print(f"  shadow_thresh={shadow_thresh}  purity_thresh={purity_thresh}", file=sys.stderr)
    print(f"  coupling_thresh={coupling_thresh}  stress_min={stress_min}  drift_mode={drift_mode}", file=sys.stderr)

    maxent_raw = load_json(MAXENT_RAW_PATH)
    sample_data = load_json(SAMPLE_PATH)
    abd_data = load_json(ABD_PATH)
    pipeline_data = load_json(PIPELINE_PATH)

    # Index by constraint ID
    maxent_lookup = {row["id"]: row for row in maxent_raw["constraints"]}
    pipeline_lookup = {c["id"]: c for c in pipeline_data["per_constraint"]}

    # Clean-session trigger sets from abductive_data.json
    abd_clean_triggers = defaultdict(set)
    for cid, hyps in abd_data["per_constraint"].items():
        for h in hyps:
            abd_clean_triggers[h["trigger_class"]].add(cid)

    sample_ids = set(sample_data["sample_ids"])
    sample_by_cat = sample_data["by_category"]
    N_sample = len(sample_ids)
    N_corpus = len(maxent_lookup)

    print(f"[synthesis] Sample: {N_sample} constraints; Corpus: {N_corpus}", file=sys.stderr)

    # ── Task 1: Population-level top-type divergence ──
    top_type_diverge = 0       # clean_top_type ≠ accum_top_type
    top_type_diverge_sample = 0
    missing_clean = 0
    missing_accum = 0

    for cid, row in maxent_lookup.items():
        c = row.get("clean_top_type", "missing")
        a = row.get("accum_top_type", "missing")
        if c == "missing":
            missing_clean += 1
        if a == "missing":
            missing_accum += 1
        if c != "missing" and a != "missing" and c != a:
            top_type_diverge += 1
            if cid in sample_ids:
                top_type_diverge_sample += 1

    # ── Task 2: Trigger-firing comparison ──
    CONDITIONS = ["t1", "t4", "t9", "elevated_entropy", "maxent_hard_disagree", "t10"]
    condition_delta_counts = {cond: Counter() for cond in CONDITIONS}
    trigger_any_diverge = 0  # constraints where at least one trigger changes

    for cid in sample_ids:
        row = maxent_lookup.get(cid)
        if row is None:
            print(f"[synthesis] WARNING: {cid} not in Prolog output", file=sys.stderr)
            continue

        clean_fires = eval_triggers_under_session(
            cid, row, pipeline_lookup, abd_clean_triggers,
            entropy_thresh, uncertainty_thresh, shadow_thresh,
            stress_min, purity_thresh, coupling_thresh, drift_mode,
            session="clean"
        )
        accum_fires = eval_triggers_under_session(
            cid, row, pipeline_lookup, abd_clean_triggers,
            entropy_thresh, uncertainty_thresh, shadow_thresh,
            stress_min, purity_thresh, coupling_thresh, drift_mode,
            session="accum"
        )

        any_changed = False
        for cond in CONDITIONS:
            delta = session_delta(clean_fires[cond], accum_fires[cond])
            condition_delta_counts[cond][delta] += 1
            if delta in ("clean_only", "accum_only"):
                any_changed = True

        if any_changed:
            trigger_any_diverge += 1

    # ── Task 3: Wasserstein dependency ──
    wasserstein_findings, wasserstein_verdict = task3_wasserstein_dependency()

    # ── Write report ──
    lines = []
    lines.append("# Audit 3 Report — Profile Accumulation Impact on Corpus\n")
    lines.append(f"Manifest: pipeline_output.json commit `{pipeline_data['manifest']['code_commit_short']}`"
                 f", run `{pipeline_data['manifest']['pipeline_run_at']}`"
                 f", n_constraints={pipeline_data['manifest']['n_constraints']}")
    lines.append(f"Audit sample: {N_sample} constraints from corpus of {N_corpus} "
                 f"(full corpus run, sample filters output)")
    lines.append(f"Config.pl thresholds read at audit time: entropy_thresh={entropy_thresh}, "
                 f"uncertainty_thresh={uncertainty_thresh}, shadow_thresh={shadow_thresh}, "
                 f"stress_min={stress_min}, purity_thresh={purity_thresh}, "
                 f"coupling_thresh={coupling_thresh}, drift_mode={drift_mode}")
    lines.append("")

    # ── Output 1: Population-level impact ──
    lines.append("## Output 1: Population-Level Impact Statement\n")
    top_type_pct = 100.0 * top_type_diverge / N_corpus if N_corpus > 0 else 0
    trigger_pct  = 100.0 * trigger_any_diverge / N_sample if N_sample > 0 else 0

    lines.append(
        f"{top_type_diverge} of {N_corpus} constraints in the full corpus show top-type "
        f"divergence between clean and accumulated sessions ({top_type_pct:.1f}%). "
        f"{trigger_any_diverge} of {N_sample} constraints in the sample show trigger-firing "
        f"divergence on at least one of the six trigger conditions ({trigger_pct:.1f}% of sample). "
    )
    missing_note = (
        f"Missing data: {missing_clean} constraints had no clean distribution, "
        f"{missing_accum} had no accumulated distribution."
    )
    lines.append(missing_note)

    # Characterize: systematic vs atypical
    if top_type_diverge / max(N_corpus, 1) > 0.10:
        character = "systematic (>10% of corpus)"
    elif top_type_diverge / max(N_corpus, 1) > 0.01:
        character = "moderate (1–10% of corpus)"
    else:
        character = "constraint-specific (<1% of corpus)"
    lines.append(f"The discrepancy is **{character}**.")
    lines.append("")

    # ── Output 2: Trigger-class impact table ──
    lines.append("## Output 2: Trigger-Class Impact Table\n")
    lines.append(f"Sample N = {N_sample}. Counts: how many sample constraints fall in each cell.\n")
    header = "| Condition | clean_only | accum_only | both | neither |"
    sep    = "|-----------|-----------|-----------|------|---------|"
    lines.append(header)
    lines.append(sep)
    for cond in CONDITIONS:
        cnt = condition_delta_counts[cond]
        total = sum(cnt.values())
        def fmt(k): return f"{cnt[k]} ({100*cnt[k]//max(total,1)}%)"
        lines.append(
            f"| {cond} | {fmt('clean_only')} | {fmt('accum_only')} | "
            f"{fmt('both')} | {fmt('neither')} |"
        )
    lines.append("")

    # Notes on T4 methodology
    lines.append("**Notes on T4:** Orbit and drift (session-independent conditions) were not "
                 "available from pipeline_output.json for constraints not previously confirmed "
                 "as T4. Conservative: only the known confirmed_liminal constraint was checked "
                 "for T4 accumulated firing; new T4 candidates in accumulated session (H_accum "
                 "> 0.40 but not previously T4) are not counted (orbit unverified).")
    lines.append("")
    lines.append("**Notes on T1 approximation:** T1 accumulated fires if the constraint has a "
                 "known override signature AND accum_hard_disagree. This approximates "
                 "`det_type == override_target(sig)` as always true when the signature is active. "
                 "Constraints where the signature is present but det_type ≠ override_target "
                 "would be over-counted.")
    lines.append("")

    # ── Output 3: Wasserstein dependency ──
    lines.append("## Output 3: Wasserstein Dependency Statement\n")
    lines.append("**Code-reading findings:**\n")
    lines.append("```prolog")
    lines.append("% measurement_layer.pl — wasserstein_edge_transport/4")
    lines.append(wasserstein_findings["edge_transport_code"])
    lines.append("")
    lines.append("% maxent_classifier.pl — maxent_distribution/3 definition")
    lines.append(wasserstein_findings["maxent_distribution_definition"])
    lines.append("")
    lines.append("% maxent_classifier.pl — maxent_dist/3 assertz call")
    lines.append(wasserstein_findings["maxent_dist_assertz"])
    lines.append("```\n")
    lines.append(f"maxent refs in measurement_layer.pl: {wasserstein_findings['unique_maxent_refs_in_meas_layer']}")
    lines.append(f"edge transport calls in wasserstein_transport_profile: "
                 f"{wasserstein_findings['n_edge_calls_in_transport_profile']} (needs all 4 contexts)")
    lines.append("")
    lines.append("**Verdict:**")
    lines.append(wasserstein_verdict)
    lines.append("")

    # ── Verification sanity check ──
    lines.append("## Verification\n")
    test_id = "collective_action_as_leverage_conversion"
    if test_id in maxent_lookup:
        row = maxent_lookup[test_id]
        lines.append(f"Sanity check — `{test_id}`:")
        lines.append(f"  clean_top_type={row.get('clean_top_type')}, clean_H={row.get('clean_H'):.4f}")
        lines.append(f"  accum_top_type={row.get('accum_top_type')}, accum_H={row.get('accum_H'):.6f}")
        lines.append(f"  Expected (Audit 2): clean≈tangled_rope/H≈0.45, accum≈scaffold/H≈0.000229")
        clean_ok = row.get("clean_top_type") == "tangled_rope" and (row.get("clean_H", 0) > 0.40)
        accum_ok = row.get("accum_top_type") == "scaffold" and (row.get("accum_H", 1) < 0.01)
        lines.append(f"  Sanity: clean={'PASS' if clean_ok else 'FAIL'}, accum={'PASS' if accum_ok else 'FAIL'}")
    else:
        lines.append(f"Sanity check: `{test_id}` not found in Prolog output.")

    report = "\n".join(lines)
    with open(OUT_PATH, "w") as f:
        f.write(report)
    print(f"[synthesis] Written: {OUT_PATH}", file=sys.stderr)
    print(report)


if __name__ == "__main__":
    main()
