#!/usr/bin/env python3
"""δ Sensitivity Sweep for Nash Vulnerability Concentration

Tests whether the 94% institutional vulnerability concentration survives
under uniform cognitive displacement (δ) perturbation.

Unlike the π sweep (which varies f(d) at institutional only), this applies
δ uniformly to ALL 4 observers via d_eff = clamp(d + δ, 0, 1), recomputing
f(d_eff) through the sigmoid and reclassifying.

Reads:  outputs/enriched_pipeline.json
        outputs/game_theory_nash.json (baseline validation)
        python/cognitive_displacement_results.json (834-flip cross-ref)
        prolog/config.pl (classification thresholds + sigmoid params)

Writes: outputs/game_theory_delta_sensitivity.json
        outputs/game_theory_delta_sensitivity_report.md

Usage:  python3 python/game_theory_delta_sensitivity.py
"""

import json
import math
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import (
    load_json,
    read_config,
    ENRICHED_PIPELINE_JSON,
    OUTPUT_DIR,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CONTEXTS = ["powerless", "moderate", "institutional", "analytical"]
CHAIN_TYPES_LIST = ["mountain", "rope", "tangled_rope", "snare"]
SIGNATURE_TYPES = {"naturalized", "scaffold", "piton", "mountain", "unknown"}

DELTA_SWEEP_POINTS = [-0.10, -0.05, 0.00, +0.05, +0.10]

NASH_JSON = OUTPUT_DIR / "game_theory_nash.json"
CDR_JSON = Path(__file__).resolve().parent / "cognitive_displacement_results.json"
OUTPUT_JSON = OUTPUT_DIR / "game_theory_delta_sensitivity.json"
OUTPUT_REPORT = OUTPUT_DIR / "game_theory_delta_sensitivity_report.md"

RELIABILITY_THRESHOLD = 30


# ---------------------------------------------------------------------------
# Sigmoid
# ---------------------------------------------------------------------------

def sigmoid_f(d, L, U, D0, K):
    """f(d) = L + (U - L) / (1 + exp(-K*(d - D0)))"""
    return L + (U - L) / (1 + math.exp(-K * (d - D0)))


# ---------------------------------------------------------------------------
# Nash distance (from game_theory_pi_sensitivity.py, inlined)
# ---------------------------------------------------------------------------

def is_constant(vec):
    return len(set(vec)) == 1


def compute_nash_single_observer(vec):
    vulnerable = []
    for pos in range(4):
        for new_type in CHAIN_TYPES_LIST:
            trial = vec.copy()
            trial[pos] = new_type
            if is_constant(trial):
                vulnerable.append(CONTEXTS[pos])
                break
    return vulnerable


def compute_nash_two_observer(vec):
    for p1 in range(4):
        for p2 in range(p1 + 1, 4):
            for t1 in CHAIN_TYPES_LIST:
                for t2 in CHAIN_TYPES_LIST:
                    trial = vec.copy()
                    trial[p1] = t1
                    trial[p2] = t2
                    if is_constant(trial):
                        return True
    return False


def compute_nash_distance(vec):
    if is_constant(vec):
        return 0, []
    vulnerable = compute_nash_single_observer(vec)
    if vulnerable:
        return 1, vulnerable
    if compute_nash_two_observer(vec):
        return 2, []
    unique = len(set(vec))
    return min(unique, 3), []


# ---------------------------------------------------------------------------
# Classification under perturbed χ (from game_theory_pi_sensitivity.py)
# ---------------------------------------------------------------------------

def classify_from_chi(chi, epsilon, cfg, consensus_type=None):
    """Classify a constraint given new χ and ε using dual thresholds."""
    rope_chi = cfg["rope_chi_ceiling"]
    rope_eps = cfg["rope_epsilon_ceiling"]
    snare_chi = cfg["snare_chi_floor"]
    snare_eps = cfg["snare_epsilon_floor"]
    tr_chi_lo = cfg["tangled_rope_chi_floor"]
    tr_chi_hi = cfg["tangled_rope_chi_ceil"]
    tr_eps = cfg["tangled_rope_epsilon_floor"]

    has_coordination = consensus_type in ("rope", "tangled_rope", "mountain")
    lacks_coordination = consensus_type in ("snare",)

    if (has_coordination
            and tr_chi_lo <= chi <= tr_chi_hi
            and epsilon >= tr_eps):
        return "tangled_rope"

    if has_coordination and chi <= rope_chi and epsilon <= rope_eps:
        return "rope"

    if lacks_coordination and chi >= snare_chi and epsilon >= snare_eps:
        return "snare"

    if chi <= rope_chi:
        return "rope"

    if chi >= tr_chi_lo and epsilon >= tr_eps:
        return "tangled_rope"

    return "rope"


# ---------------------------------------------------------------------------
# Recompute orbit at δ
# ---------------------------------------------------------------------------

def metric_type_at_delta(obs_data, delta, cfg, sigmoid_params, consensus):
    """Compute the metric-based type for one observer at a given δ."""
    L, U, D0, K = sigmoid_params
    d_base = obs_data.get("d", 0.0)
    epsilon = obs_data.get("epsilon", 0.0)
    scope_mod = obs_data.get("scope_mod", 1.0)

    d_eff = max(0.0, min(1.0, d_base + delta))
    f_d_new = sigmoid_f(d_eff, L, U, D0, K)
    chi_new = epsilon * f_d_new * scope_mod

    return classify_from_chi(chi_new, epsilon, cfg, consensus)


def recompute_orbit_at_delta(entry, delta, cfg, sigmoid_params, consensus):
    """Recompute 4-observer type vector for a constraint at given δ.

    Uses delta-from-baseline approach: compute metric-based types at both
    δ=0 and the target δ. If they agree, keep the original Prolog type
    (which includes signature overrides). If they disagree, use the
    δ-recomputed type. This preserves signature integration at positions
    where δ doesn't change the metric classification.
    """
    perspectives = entry["perspectives"]
    pchi = entry.get("perspective_chi", {})

    new_vec = []
    for ctx in CONTEXTS:
        original_type = perspectives.get(ctx, "")
        if original_type in SIGNATURE_TYPES:
            new_vec.append(original_type)
            continue

        obs = pchi.get(ctx, {})
        if not obs:
            new_vec.append(original_type)
            continue

        # Metric type at δ=0 and at target δ
        type_at_zero = metric_type_at_delta(obs, 0.0, cfg, sigmoid_params, consensus)
        type_at_delta = metric_type_at_delta(obs, delta, cfg, sigmoid_params, consensus)

        if type_at_zero == type_at_delta:
            # δ didn't change the metric classification → keep original Prolog type
            new_vec.append(original_type)
        else:
            # δ changed the metric classification → use the new type
            new_vec.append(type_at_delta)

    return new_vec


# ---------------------------------------------------------------------------
# Load 834 institutional flip set
# ---------------------------------------------------------------------------

def load_institutional_flip_set():
    """Load constraint IDs that flip at institutional from positional δ-sweep."""
    cdr = load_json(CDR_JSON, "cognitive_displacement_results")
    if not cdr:
        print("Warning: Could not load cognitive_displacement_results.json", file=sys.stderr)
        return set()
    inst_data = cdr.get("positional", {}).get("institutional", {})
    first_flips = inst_data.get("first_flip_delta", [])
    return {f["constraint"] for f in first_flips if f.get("power") == "institutional"}


# ---------------------------------------------------------------------------
# Main sweep
# ---------------------------------------------------------------------------

def main():
    enriched = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    if not enriched:
        print("ERROR: Could not load enriched_pipeline.json", file=sys.stderr)
        sys.exit(1)

    nash_baseline = load_json(NASH_JSON, "game_theory_nash")
    cfg = read_config()

    sigmoid_params = (
        cfg.get("sigmoid_lower", -0.20),
        cfg.get("sigmoid_upper", 1.50),
        cfg.get("sigmoid_midpoint", 0.50),
        cfg.get("sigmoid_steepness", 6.00),
    )

    per_constraint = enriched.get("per_constraint", [])
    institutional_flip_set = load_institutional_flip_set()
    print(f"Loaded {len(per_constraint)} constraints, "
          f"{len(institutional_flip_set)} institutional-flip IDs")

    # Identify non-constant orbits at baseline
    orbit_constraints = []
    for entry in per_constraint:
        perspectives = entry.get("perspectives", {})
        if not all(c in perspectives for c in CONTEXTS):
            continue
        vec = [perspectives[c] for c in CONTEXTS]
        if is_constant(vec):
            continue
        orbit_constraints.append(entry)

    print(f"Non-constant orbits: {len(orbit_constraints)}")

    # Precompute original consensus for each constraint
    original_consensus = {}
    for entry in per_constraint:
        cid = entry["id"]
        perspectives = entry.get("perspectives", {})
        if perspectives:
            counts = Counter(perspectives.values())
            original_consensus[cid] = counts.most_common(1)[0][0]

    # Baseline validation (δ=0)
    baseline_vuln = Counter()
    baseline_resolvable = 0
    baseline_resolvable_ids = set()
    for entry in orbit_constraints:
        vec = [entry["perspectives"][c] for c in CONTEXTS]
        dist, vuln = compute_nash_distance(vec)
        if dist == 1:
            baseline_resolvable += 1
            baseline_resolvable_ids.add(entry["id"])
            for v in vuln:
                baseline_vuln[v] += 1

    baseline_u3_frac = baseline_vuln.get("institutional", 0) / max(baseline_resolvable, 1)
    print(f"Baseline: {baseline_resolvable} resolvable, "
          f"institutional={baseline_vuln.get('institutional', 0)} ({baseline_u3_frac:.1%})")

    # Run sweep
    sweep_results = []

    for delta in DELTA_SWEEP_POINTS:
        vuln_counter = Counter()
        resolvable = 0
        nash_stable = 0
        total_non_constant = 0
        resolvable_ids = []

        for entry in orbit_constraints:
            cid = entry["id"]
            consensus = original_consensus.get(cid)
            new_vec = recompute_orbit_at_delta(entry, delta, cfg, sigmoid_params, consensus)

            if is_constant(new_vec):
                continue

            total_non_constant += 1
            dist, vuln = compute_nash_distance(new_vec)

            if dist == 1:
                resolvable += 1
                resolvable_ids.append(cid)
                for v in vuln:
                    vuln_counter[v] += 1
            if dist >= 2:
                nash_stable += 1

        resolvable_set = set(resolvable_ids)
        in_834_and_resolvable = resolvable_set & institutional_flip_set
        in_834_not_resolvable = institutional_flip_set - resolvable_set

        entered = resolvable_set - baseline_resolvable_ids
        exited = baseline_resolvable_ids - resolvable_set

        result = {
            "delta": delta,
            "non_constant_orbits": total_non_constant,
            "resolvable_count": resolvable,
            "nash_stable_count": nash_stable,
            "u1_vulnerable_count": vuln_counter.get("powerless", 0),
            "u2_vulnerable_count": vuln_counter.get("moderate", 0),
            "u3_vulnerable_count": vuln_counter.get("institutional", 0),
            "u4_vulnerable_count": vuln_counter.get("analytical", 0),
            "u1_vulnerable_fraction": round(vuln_counter.get("powerless", 0) / max(resolvable, 1), 4),
            "u2_vulnerable_fraction": round(vuln_counter.get("moderate", 0) / max(resolvable, 1), 4),
            "u3_vulnerable_fraction": round(vuln_counter.get("institutional", 0) / max(resolvable, 1), 4),
            "u4_vulnerable_fraction": round(vuln_counter.get("analytical", 0) / max(resolvable, 1), 4),
            "reliable": resolvable >= RELIABILITY_THRESHOLD,
            "top_movers_in": sorted(entered)[:3],
            "top_movers_out": sorted(exited)[:3],
            "entered_count": len(entered),
            "exited_count": len(exited),
            "overlap_834": {
                "in_resolvable": len(in_834_and_resolvable),
                "not_in_resolvable": len(in_834_not_resolvable),
                "total_834": len(institutional_flip_set),
            },
        }
        sweep_results.append(result)

        flag = "" if result["reliable"] else " [UNRELIABLE: low n]"
        print(f"  δ={delta:+.2f}: resolvable={resolvable:3d}, "
              f"u3={vuln_counter.get('institutional',0):3d} "
              f"({result['u3_vulnerable_fraction']:.1%}), "
              f"834-overlap={len(in_834_and_resolvable)}{flag}")

    # Verdict
    reliable = [r for r in sweep_results if r["reliable"]]
    if not reliable:
        verdict = "INCONCLUSIVE"
        verdict_detail = "No reliable sweep points."
    else:
        u3_fracs = [r["u3_vulnerable_fraction"] for r in reliable]
        min_u3 = min(u3_fracs)
        max_u3 = max(u3_fracs)

        if all(f >= 0.80 for f in u3_fracs):
            verdict = "STRUCTURAL"
            verdict_detail = (
                f"Institutional vulnerability concentration stays above 80% across all "
                f"δ points ({min_u3:.0%} to {max_u3:.0%}). The 94% concentration "
                f"survives uniform cognitive displacement perturbation at δ ∈ [-0.10, +0.10]."
            )
        elif any(f < 0.50 for f in u3_fracs):
            verdict = "FRAGILE"
            verdict_detail = (
                f"Institutional concentration drops below 50% at some δ values "
                f"(range: {min_u3:.0%} to {max_u3:.0%}). The concentration is partially "
                f"dependent on cognitive displacement calibration."
            )
        else:
            verdict = "MIXED"
            verdict_detail = (
                f"Institutional concentration varies from {min_u3:.0%} to {max_u3:.0%}. "
                f"Robust but shows some sensitivity to uniform displacement."
            )

    # Build output
    output = {
        "generated": datetime.now().isoformat(),
        "methodology": (
            "Applied uniform cognitive displacement δ to ALL 4 observer positions. "
            "For each observer: d_eff = clamp(d + δ, 0, 1), then χ_new = ε × f(d_eff) × σ(S) "
            "where f is the configured sigmoid. Uses delta-from-baseline approach: computes "
            "metric-based type at both δ=0 and target δ using the same Python classifier. "
            "If they agree, the original Prolog type (including signature overrides from "
            "integrate_signature_with_modal) is preserved. If they disagree, the δ-recomputed "
            "type is used. This ensures δ=0 exactly reproduces the Prolog baseline while "
            "capturing genuine δ-induced classification changes. Signature-based types "
            "(naturalized/scaffold/piton/mountain/unknown) always held invariant."
        ),
        "delta_sweep_points": DELTA_SWEEP_POINTS,
        "baseline": {
            "resolvable_count": baseline_resolvable,
            "u3_vulnerable_count": baseline_vuln.get("institutional", 0),
            "u3_vulnerable_fraction": round(baseline_u3_frac, 4),
        },
        "per_sweep_point": sweep_results,
        "institutional_flip_set_size": len(institutional_flip_set),
        "verdict": verdict,
        "verdict_detail": verdict_detail,
    }

    OUTPUT_JSON.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_JSON, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)
    print(f"\nJSON: {OUTPUT_JSON}")

    write_report(output, sweep_results)
    print(f"Report: {OUTPUT_REPORT}")
    print(f"\nVerdict: {verdict}")
    print(f"  {verdict_detail}")


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------

def write_report(output, sweep_results):
    lines = []
    lines.append("# δ Sensitivity Sweep: Nash Vulnerability Concentration")
    lines.append("")
    lines.append(f"Generated: {output['generated']}")
    lines.append("")

    lines.append("## Methodology")
    lines.append("")
    lines.append(output["methodology"])
    lines.append("")

    lines.append("## Baseline")
    lines.append("")
    b = output["baseline"]
    lines.append(f"- Resolvable cases (nash_distance=1): {b['resolvable_count']}")
    lines.append(f"- U₃ vulnerable: {b['u3_vulnerable_count']} ({b['u3_vulnerable_fraction']:.1%})")
    lines.append(f"- Institutional flip set (from positional δ-sweep): {output['institutional_flip_set_size']}")
    lines.append("")

    # Sweep table
    lines.append("## Sweep Results")
    lines.append("")
    lines.append("| δ | Resolvable | U₃ count | U₃ frac | U₁ frac | U₂ frac | U₄ frac | Entered | Exited | Reliable |")
    lines.append("|-----:|-----------:|---------:|--------:|--------:|--------:|--------:|--------:|-------:|:--------:|")
    for r in sweep_results:
        rel = "Y" if r["reliable"] else "N"
        lines.append(
            f"| {r['delta']:+.2f} "
            f"| {r['resolvable_count']:3d} "
            f"| {r['u3_vulnerable_count']:3d} "
            f"| {r['u3_vulnerable_fraction']:.1%} "
            f"| {r['u1_vulnerable_fraction']:.1%} "
            f"| {r['u2_vulnerable_fraction']:.1%} "
            f"| {r['u4_vulnerable_fraction']:.1%} "
            f"| {r['entered_count']:3d} "
            f"| {r['exited_count']:3d} "
            f"| {rel} |"
        )
    lines.append("")

    # ASCII chart
    lines.append("## Visualization")
    lines.append("")
    lines.append("```")
    lines.append("δ      Resolvable  U₃ vulnerable fraction")
    lines.append("       count       0%       25%       50%       75%      100%")
    lines.append("       ────────    |─────────|─────────|─────────|─────────|")
    for r in sweep_results:
        bar_len = int(r["u3_vulnerable_fraction"] * 50)
        bar = "█" * bar_len + "░" * (50 - bar_len)
        rel = " " if r["reliable"] else "*"
        lines.append(
            f"{r['delta']:+.2f}  {r['resolvable_count']:4d}       "
            f"|{bar}| {r['u3_vulnerable_fraction']:.0%}{rel}"
        )
    lines.append("       ────────    |─────────|─────────|─────────|─────────|")
    lines.append("                   0%       25%       50%       75%      100%")
    lines.append("```")
    lines.append("")

    # 834-flip overlap
    lines.append("## 834-Flip Overlap Analysis")
    lines.append("")
    lines.append("How many of the 834 institutional-position flips (from positional δ-sweep)")
    lines.append("are in the Nash resolvable set at each δ?")
    lines.append("")
    lines.append("| δ | In resolvable | Not in resolvable | Total 834 |")
    lines.append("|-----:|--------------:|------------------:|----------:|")
    for r in sweep_results:
        o = r["overlap_834"]
        lines.append(
            f"| {r['delta']:+.2f} "
            f"| {o['in_resolvable']:3d} "
            f"| {o['not_in_resolvable']:3d} "
            f"| {o['total_834']:3d} |"
        )
    lines.append("")

    # Top movers
    lines.append("## Top Movers (vs baseline)")
    lines.append("")
    for r in sweep_results:
        if abs(r["delta"]) < 1e-9:
            continue
        lines.append(f"### δ = {r['delta']:+.2f}")
        lines.append(f"- Entered resolvable set ({r['entered_count']}): {', '.join(r['top_movers_in']) or 'none'}")
        lines.append(f"- Exited resolvable set ({r['exited_count']}): {', '.join(r['top_movers_out']) or 'none'}")
        lines.append("")

    # Denominator analysis
    lines.append("## Denominator Analysis")
    lines.append("")
    res_counts = [r["resolvable_count"] for r in sweep_results]
    nc_counts = [r["non_constant_orbits"] for r in sweep_results]
    lines.append(f"- Resolvable count range: {min(res_counts)} to {max(res_counts)}")
    lines.append(f"- Non-constant orbit range: {min(nc_counts)} to {max(nc_counts)}")
    if min(res_counts) != max(res_counts):
        lines.append("- δ is creating/destroying Nash-resolvable configurations.")
    else:
        lines.append("- Resolvable set is stable across δ range.")
    lines.append("")

    # Verdict
    lines.append("## Verdict")
    lines.append("")
    lines.append(f"**{output['verdict']}**")
    lines.append("")
    lines.append(output["verdict_detail"])
    lines.append("")

    with open(OUTPUT_REPORT, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))


if __name__ == "__main__":
    main()
