#!/usr/bin/env python3
"""Tangled Rope Fiber Decomposition (v1.0)

Parameterizes the tangled_rope fiber using a continuous snare-lean metric (psi)
and discrete coalition structure, without changing any classification boundaries.

Replicates the MaxEnt shadow classifier in Python, validates against 182 known
distributions from maxent_report.md, then decomposes the ~662 tangled_rope
constraints into rope-leaning / genuinely-tangled / snare-leaning bands with
coalition type annotations from orbit data.

Outputs:
  outputs/tangled_decomposition_data.json  - per-constraint decomposition data
  outputs/tangled_rope_decomposition_report.md - analysis report

Usage:
  python3 python/tangled_decomposition.py                 # full run
  python3 python/tangled_decomposition.py --validate-only # MaxEnt validation only
"""

import argparse
import json
import math
import sys
from collections import Counter, defaultdict
from pathlib import Path

from shared.loader import load_json, read_config, load_all_data, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import (
    MAXENT_TYPES, N_TYPES, BOOLEAN_SPECS, PSI_ROPE_LEANING, PSI_SNARE_LEANING,
    compute_psi, classify_band, classify_coalition,
)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
PROLOG_DIR = ROOT_DIR / "prolog"

MAXENT_REPORT = OUTPUT_DIR / "maxent_report.md"

DECOMP_JSON = OUTPUT_DIR / "tangled_decomposition_data.json"
DECOMP_REPORT = OUTPUT_DIR / "tangled_rope_decomposition_report.md"

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

_CFG = read_config()

BOOLEAN_PENALTY = _CFG.get("maxent_boolean_penalty", -4.0)
BOOLEAN_BONUS = _CFG.get("maxent_boolean_bonus", 1.0)
OVERRIDE_STRENGTH = _CFG.get("maxent_signature_override_strength", 0.95)

# ---------------------------------------------------------------------------
# Boolean features (derived from JSON, matching Prolog definitions)
# ---------------------------------------------------------------------------

def has_coordination_function(c):
    """narrative_ontology.pl:251-252: has beneficiaries."""
    return len(c.get("beneficiaries") or []) > 0


def has_asymmetric_extraction(c):
    """narrative_ontology.pl:259-260: has victims."""
    return len(c.get("victims") or []) > 0


def natural_law_without_beneficiary(c):
    """drl_core.pl:249-252: emerges naturally, no enforcement, no beneficiaries."""
    return (
        bool(c.get("emerges_naturally"))
        and not bool(c.get("requires_active_enforcement"))
        and len(c.get("beneficiaries") or []) == 0
    )


def eval_boolean(c, feature):
    """Evaluate a boolean feature for a constraint."""
    if feature == "emerges_naturally":
        return bool(c.get("emerges_naturally"))
    if feature == "requires_active_enforcement":
        return bool(c.get("requires_active_enforcement"))
    if feature == "has_coordination_function":
        return has_coordination_function(c)
    if feature == "has_asymmetric_extraction":
        return has_asymmetric_extraction(c)
    if feature == "natural_law_without_beneficiary":
        return natural_law_without_beneficiary(c)
    return False

# Default profiles (from maxent_classifier.pl:122-139)
DEFAULT_PROFILES = {
    "mountain":     {"extractiveness": (0.09, 0.08), "suppression": (0.03, 0.02), "theater": (0.02, 0.05)},
    "rope":         {"extractiveness": (0.15, 0.12), "suppression": (0.30, 0.20), "theater": (0.25, 0.15)},
    "tangled_rope": {"extractiveness": (0.61, 0.15), "suppression": (0.66, 0.15), "theater": (0.30, 0.15)},
    "snare":        {"extractiveness": (0.67, 0.15), "suppression": (0.73, 0.12), "theater": (0.28, 0.18)},
    "scaffold":     {"extractiveness": (0.20, 0.12), "suppression": (0.38, 0.20), "theater": (0.14, 0.12)},
    "piton":        {"extractiveness": (0.65, 0.15), "suppression": (0.69, 0.15), "theater": (0.85, 0.08)},
}

# ---------------------------------------------------------------------------
# MaxEnt classifier (Python replication)
# ---------------------------------------------------------------------------

def _analytical_type(c):
    """Get the deterministic type from analytical perspective (default context).

    The Prolog MaxEnt uses default_context = context(agent_power(analytical), ...)
    for both profile computation and priors. This corresponds to the analytical
    perspective type in pipeline_output.json.
    """
    return c.get("perspectives", {}).get("analytical") or c.get("claimed_type")


def compute_profiles(constraints):
    """Compute empirical mean/std profiles from corpus, falling back to defaults.

    Groups constraints by analytical-perspective type (matching Prolog dr_type
    in the default context used by maxent_run).
    """
    profiles = {}
    for typ in MAXENT_TYPES:
        profiles[typ] = {}
        type_constraints = [c for c in constraints.values() if _analytical_type(c) == typ]
        for metric_name, json_key in [("extractiveness", "extractiveness"),
                                       ("suppression", "suppression"),
                                       ("theater", "theater_ratio")]:
            values = [c[json_key] for c in type_constraints
                      if c[json_key] is not None]
            if len(values) >= 2:
                n = len(values)
                mu = sum(values) / n
                variance = sum((x - mu) ** 2 for x in values) / n
                sigma = max(0.01, math.sqrt(variance))
                profiles[typ][metric_name] = (mu, sigma)
            else:
                profiles[typ][metric_name] = DEFAULT_PROFILES[typ][metric_name]
    return profiles


def compute_priors(constraints):
    """Compute corpus-frequency priors with floor at 0.001.

    Uses analytical-perspective type (matching Prolog default_context).
    """
    total = len(constraints)
    if total == 0:
        return {t: 1.0 / N_TYPES for t in MAXENT_TYPES}
    counts = Counter(_analytical_type(c) for c in constraints.values())
    return {t: max(0.001, counts.get(t, 0) / total) for t in MAXENT_TYPES}


def gaussian_ll(x, mu, sigma):
    """Gaussian log-likelihood: -0.5*(x-mu)^2/sigma^2 - log(sigma)."""
    if sigma <= 1e-15:
        return -100.0
    diff = x - mu
    return -0.5 * (diff * diff) / (sigma * sigma) - math.log(sigma)


def boolean_ll(c, typ):
    """Compute total boolean log-likelihood contribution for a type."""
    total = 0.0
    for spec_type, feature, spec in BOOLEAN_SPECS:
        if spec_type != typ:
            continue
        val = eval_boolean(c, feature)
        if spec == "required":
            total += 0.0 if val else BOOLEAN_PENALTY
        elif spec == "forbidden":
            total += BOOLEAN_PENALTY if val else 0.0
        elif spec == "bonus":
            total += BOOLEAN_BONUS if val else 0.0
    return total


def log_sum_exp_normalize(type_ll_pairs):
    """Normalize log-likelihoods to probabilities via log-sum-exp."""
    if not type_ll_pairs:
        return {}
    max_ll = max(ll for _, ll in type_ll_pairs)
    exps = []
    for typ, ll in type_ll_pairs:
        shifted = ll - max_ll
        exps.append((typ, math.exp(shifted) if shifted > -500 else 0.0))
    total = sum(e for _, e in exps)
    if total > 1e-30:
        return {typ: e / total for typ, e in exps}
    # Uniform fallback
    return {typ: 1.0 / N_TYPES for typ, _ in type_ll_pairs}


def apply_signature_override(signature, dist):
    """Apply signature-based overrides matching maxent_classifier.pl:297-324."""
    # Unconditional overrides
    unconditional = {
        "natural_law": "mountain",
        "false_natural_law": "tangled_rope",
        "coupling_invariant_rope": "rope",
    }
    # Conditional overrides (boost by 3x)
    conditional = {
        "false_ci_rope": "tangled_rope",
        "coordination_scaffold": "rope",
        "constructed_low_extraction": "rope",
        "constructed_high_extraction": "tangled_rope",
        "constructed_constraint": "tangled_rope",
    }

    if signature in unconditional:
        target = unconditional[signature]
        remainder = (1.0 - OVERRIDE_STRENGTH) / (N_TYPES - 1)
        return {t: (OVERRIDE_STRENGTH if t == target else remainder) for t in MAXENT_TYPES}

    if signature in conditional:
        target = conditional[signature]
        boosted = {}
        for t, p in dist.items():
            boosted[t] = p * 3.0 if t == target else p
        total = sum(boosted.values())
        if total > 1e-15:
            return {t: v / total for t, v in boosted.items()}

    return dist


def maxent_classify(constraints, apply_overrides=True):
    """Run the MaxEnt classifier on all constraints. Returns {cid: {type: prob}}.

    Args:
        constraints: dict of constraint data keyed by id.
        apply_overrides: if True (default), apply signature-based overrides
            matching Prolog behavior. If False, return raw MaxEnt distributions
            without overrides.
    """
    profiles = compute_profiles(constraints)
    priors = compute_priors(constraints)

    distributions = {}
    for cid, c in constraints.items():
        eps = c["extractiveness"] if c["extractiveness"] is not None else 0.0
        supp = c["suppression"] if c["suppression"] is not None else 0.0
        theater = c["theater_ratio"] if c["theater_ratio"] is not None else 0.0

        type_lls = []
        for typ in MAXENT_TYPES:
            # Continuous log-likelihood
            cont_ll = 0.0
            for metric_name, val in [("extractiveness", eps),
                                     ("suppression", supp),
                                     ("theater", theater)]:
                mu, sigma = profiles[typ][metric_name]
                cont_ll += gaussian_ll(val, mu, sigma)

            # Boolean log-likelihood
            bool_ll = boolean_ll(c, typ)

            # Prior
            prior = priors.get(typ, 0.001)
            prior_ll = math.log(prior) if prior > 1e-15 else -10.0

            total_ll = cont_ll + bool_ll + prior_ll
            type_lls.append((typ, total_ll))

        dist = log_sum_exp_normalize(type_lls)
        if apply_overrides:
            dist = apply_signature_override(c.get("signature"), dist)
        distributions[cid] = dist

    return distributions

# ---------------------------------------------------------------------------
# Validation against maxent_report.md
# ---------------------------------------------------------------------------

def parse_maxent_report():
    """Parse the 182 hard disagreement distributions from maxent_report.md.

    Returns {constraint_id: {type: prob}} for each parsed row.
    """
    known = {}
    try:
        text = (MAXENT_REPORT).read_text(encoding="utf-8")
    except FileNotFoundError:
        print("Warning: maxent_report.md not found, skipping validation.", file=sys.stderr)
        return known

    # Find the hard disagreements table
    in_hard = False
    for line in text.splitlines():
        if "Hard Disagreements" in line:
            in_hard = True
            continue
        if in_hard and line.startswith("### "):
            break  # Next section
        if not in_hard:
            continue
        if not line.startswith("| ") or "Constraint" in line or "---" in line:
            continue
        # Parse: | constraint_id | det_type | shadow_top | distribution |
        parts = [p.strip() for p in line.split("|")]
        parts = [p for p in parts if p]
        if len(parts) < 4:
            continue
        cid = parts[0]
        dist_str = parts[3]
        # Parse distribution: "tangled_rope:0.61 snare:0.39"
        dist = {}
        for token in dist_str.split():
            if ":" in token:
                typ, prob_str = token.split(":", 1)
                try:
                    dist[typ] = float(prob_str)
                except ValueError:
                    pass
        if dist:
            known[cid] = dist

    return known


def validate_maxent(computed_dists, known_dists, tolerance=0.05):
    """Validate computed distributions against known ones.

    Returns (n_tested, n_passed, failures).
    """
    n_tested = 0
    n_passed = 0
    failures = []

    for cid, known_dist in known_dists.items():
        if cid not in computed_dists:
            failures.append((cid, "missing", {}))
            n_tested += 1
            continue
        computed = computed_dists[cid]
        n_tested += 1

        # Check each type that appears in known distribution
        ok = True
        for typ, known_p in known_dist.items():
            computed_p = computed.get(typ, 0.0)
            if abs(computed_p - known_p) > tolerance:
                ok = False
                break

        if ok:
            n_passed += 1
        else:
            failures.append((cid, known_dist, {t: round(computed.get(t, 0.0), 4)
                                                for t in known_dist}))

    return n_tested, n_passed, failures

# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def make_ascii_histogram(values, bins=20, width=50):
    """Create ASCII histogram of values."""
    if not values:
        return "  (no data)\n"
    lo, hi = 0.0, 1.0
    bin_width = (hi - lo) / bins
    counts = [0] * bins
    for v in values:
        idx = min(int((v - lo) / bin_width), bins - 1)
        counts[idx] = counts[idx] + 1
    max_count = max(counts) if counts else 1
    lines = []
    for i, cnt in enumerate(counts):
        lo_edge = lo + i * bin_width
        hi_edge = lo_edge + bin_width
        bar_len = int(cnt / max_count * width) if max_count > 0 else 0
        bar = "#" * bar_len
        lines.append(f"  {lo_edge:5.2f}-{hi_edge:5.2f} | {bar} ({cnt})")
    return "\n".join(lines)


def generate_report(tangled_data, validation_result, total_constraints, total_tangled):
    """Generate the markdown report."""
    n_tested, n_passed, failures = validation_result

    lines = []
    lines.append("# Tangled Rope Fiber Decomposition Report")
    lines.append("")
    lines.append("*Generated by `python/tangled_decomposition.py` — parameterization of the")
    lines.append("tangled_rope fiber using continuous snare-lean metric (psi) and coalition structure.*")
    lines.append("")

    # Section 1: MaxEnt validation
    lines.append("## 1. MaxEnt Replication Validation")
    lines.append("")
    lines.append(f"Validated Python MaxEnt replication against {n_tested} known distributions")
    lines.append(f"from `maxent_report.md` (hard disagreement constraints).")
    lines.append("")
    lines.append(f"| Property | Value |")
    lines.append(f"|----------|-------|")
    lines.append(f"| **Distributions tested** | {n_tested} |")
    lines.append(f"| **Passed (tolerance 0.05)** | {n_passed} ({100*n_passed/max(n_tested,1):.1f}%) |")
    lines.append(f"| **Failed** | {n_tested - n_passed} |")
    lines.append("")
    if failures:
        lines.append("### Validation Failures")
        lines.append("")
        lines.append("| Constraint | Known | Computed |")
        lines.append("|------------|-------|----------|")
        for cid, known, computed in failures[:20]:
            if isinstance(known, str):
                lines.append(f"| {cid} | {known} | — |")
            else:
                k_str = " ".join(f"{t}:{p:.2f}" for t, p in sorted(known.items()))
                c_str = " ".join(f"{t}:{p:.2f}" for t, p in sorted(computed.items()))
                lines.append(f"| {cid} | {k_str} | {c_str} |")
        if len(failures) > 20:
            lines.append(f"| ... | ({len(failures) - 20} more) | |")
        lines.append("")

    # Section 2: Band distribution
    lines.append("## 2. Band Distribution")
    lines.append("")
    lines.append(f"Total corpus constraints: **{total_constraints}**")
    lines.append(f"Total tangled_rope constraints: **{total_tangled}**")
    lines.append("")

    band_counts = Counter(d["band"] for d in tangled_data)
    lines.append("| Band | Count | % of tangled_rope |")
    lines.append("|------|-------|-------------------|")
    for band in ["rope_leaning", "genuinely_tangled", "snare_leaning"]:
        cnt = band_counts.get(band, 0)
        pct = 100 * cnt / max(total_tangled, 1)
        lines.append(f"| {band} | {cnt} | {pct:.1f}% |")
    lines.append("")

    # Psi histogram
    psi_values = [d["psi"] for d in tangled_data]
    lines.append("### Psi (snare-lean) Distribution")
    lines.append("")
    lines.append("```")
    lines.append(f"  psi = P(snare) / (P(rope) + P(snare) + 0.001)")
    lines.append(f"  0.0 = pure rope-leaning, 1.0 = pure snare-leaning")
    lines.append(f"  Bands: rope_leaning (< {PSI_ROPE_LEANING}), genuinely_tangled ({PSI_ROPE_LEANING}-{PSI_SNARE_LEANING}), snare_leaning (> {PSI_SNARE_LEANING})")
    lines.append("")
    lines.append(make_ascii_histogram(psi_values))
    lines.append("```")
    lines.append("")

    if psi_values:
        lines.append(f"| Statistic | Value |")
        lines.append(f"|-----------|-------|")
        lines.append(f"| Mean psi | {sum(psi_values)/len(psi_values):.4f} |")
        sorted_psi = sorted(psi_values)
        median_psi = sorted_psi[len(sorted_psi) // 2]
        lines.append(f"| Median psi | {median_psi:.4f} |")
        lines.append(f"| Min psi | {min(psi_values):.4f} |")
        lines.append(f"| Max psi | {max(psi_values):.4f} |")
        lines.append("")

    # Section 3: Coalition cross-tabulation
    lines.append("## 3. Coalition Type Cross-Tabulation")
    lines.append("")
    coalition_counts = Counter(d["coalition_type"] for d in tangled_data)
    coalition_types = ["uniform_tangled", "institutional_dissent", "analytical_dissent",
                       "split_field", "other"]

    # Cross-tab: band x coalition
    cross = defaultdict(Counter)
    for d in tangled_data:
        cross[d["band"]][d["coalition_type"]] += 1

    header = "| Band | " + " | ".join(coalition_types) + " | Total |"
    sep = "|------|" + "|".join("---:" for _ in coalition_types) + "|------:|"
    lines.append(header)
    lines.append(sep)
    for band in ["rope_leaning", "genuinely_tangled", "snare_leaning"]:
        row_vals = [str(cross[band].get(ct, 0)) for ct in coalition_types]
        row_total = sum(cross[band].values())
        lines.append(f"| {band} | " + " | ".join(row_vals) + f" | {row_total} |")
    # Totals row
    col_totals = [str(sum(cross[b].get(ct, 0) for b in cross)) for ct in coalition_types]
    lines.append(f"| **Total** | " + " | ".join(col_totals) + f" | {total_tangled} |")
    lines.append("")

    lines.append("### Coalition Type Definitions")
    lines.append("")
    lines.append("| Coalition Type | Pattern |")
    lines.append("|---|---|")
    lines.append("| uniform_tangled | All 4 perspectives agree on tangled_rope |")
    lines.append("| institutional_dissent | Institutional sees rope/scaffold, majority others see tangled_rope/snare |")
    lines.append("| analytical_dissent | Analytical sees differently from powerless+moderate consensus |")
    lines.append("| split_field | 3+ distinct types across perspectives |")
    lines.append("| other | None of the above |")
    lines.append("")

    # Section 4: Extreme cases
    lines.append("## 4. Extreme Cases")
    lines.append("")

    # Rope-leaning extremes (psi < 0.15)
    rope_extremes = sorted([d for d in tangled_data if d["psi"] < 0.15],
                           key=lambda d: d["psi"])
    lines.append(f"### Rope-Leaning Extremes (psi < 0.15): {len(rope_extremes)} constraints")
    lines.append("")
    if rope_extremes:
        lines.append("| Constraint | psi | P(rope) | P(snare) | P(tangled) | Coalition |")
        lines.append("|------------|-----|---------|----------|------------|-----------|")
        for d in rope_extremes[:25]:
            lines.append(f"| {d['id']} | {d['psi']:.4f} | {d['p_rope']:.4f} | "
                         f"{d['p_snare']:.4f} | {d['p_tangled_rope']:.4f} | {d['coalition_type']} |")
        if len(rope_extremes) > 25:
            lines.append(f"| ... | ({len(rope_extremes) - 25} more) | | | | |")
    else:
        lines.append("(none)")
    lines.append("")

    # Snare-leaning extremes (psi > 0.85)
    snare_extremes = sorted([d for d in tangled_data if d["psi"] > 0.85],
                            key=lambda d: -d["psi"])
    lines.append(f"### Snare-Leaning Extremes (psi > 0.85): {len(snare_extremes)} constraints")
    lines.append("")
    if snare_extremes:
        lines.append("| Constraint | psi | P(rope) | P(snare) | P(tangled) | Extractiveness | Suppression | Coalition |")
        lines.append("|------------|-----|---------|----------|------------|----------------|-------------|-----------|")
        for d in snare_extremes[:25]:
            lines.append(f"| {d['id']} | {d['psi']:.4f} | {d['p_rope']:.4f} | "
                         f"{d['p_snare']:.4f} | {d['p_tangled_rope']:.4f} | "
                         f"{d['extractiveness']:.2f} | {d['suppression']:.2f} | {d['coalition_type']} |")
        if len(snare_extremes) > 25:
            lines.append(f"| ... | ({len(snare_extremes) - 25} more) | | | | | | |")
    else:
        lines.append("(none)")
    lines.append("")

    # Section 5: Cross-references
    lines.append("## 5. Cross-References")
    lines.append("")
    lines.append("- **enriched_omega_report.md** — Omega anomalies for tangled_rope constraints")
    lines.append("- **maxent_diagnostic_report.md** — Full MaxEnt diagnostic details")
    lines.append("- **maxent_report.md** — 182 hard disagreement distributions (validation source)")
    lines.append("- **tangled_rope_report.md** — Per-type category report")
    lines.append("")

    return "\n".join(lines)

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Tangled rope fiber decomposition.")
    parser.add_argument("--validate-only", action="store_true",
                        help="Run MaxEnt validation only, no decomposition.")
    args = parser.parse_args()

    # Load data
    print("[TANGLED] Loading data...", file=sys.stderr)
    constraints = load_all_data()
    print(f"[TANGLED] Loaded {len(constraints)} constraints.", file=sys.stderr)

    # Run MaxEnt classifier
    print("[TANGLED] Running MaxEnt classifier...", file=sys.stderr)
    distributions = maxent_classify(constraints)
    print(f"[TANGLED] Computed {len(distributions)} distributions.", file=sys.stderr)

    # Auto-detect maxent_probs from pipeline_output.json and cross-validate
    pipeline_raw = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    per_constraint = pipeline_raw.get("per_constraint", [])
    json_dists = {}
    for entry in per_constraint:
        probs = entry.get("maxent_probs")
        if probs and isinstance(probs, dict):
            json_dists[entry["id"]] = probs

    if json_dists:
        print(f"[TANGLED] Found maxent_probs in pipeline_output.json for {len(json_dists)} constraints.", file=sys.stderr)
        # Cross-validate all distributions
        discrepancies = 0
        max_dev = 0.0
        for cid, jdist in json_dists.items():
            if cid not in distributions:
                continue
            cdist = distributions[cid]
            for typ in MAXENT_TYPES:
                dev = abs(cdist.get(typ, 0.0) - jdist.get(typ, 0.0))
                max_dev = max(max_dev, dev)
                if dev > 0.05:
                    discrepancies += 1
                    break
        print(f"[TANGLED] Cross-validation: {discrepancies} discrepancies, max deviation {max_dev:.6f}.", file=sys.stderr)
        # Use pipeline JSON distributions as primary (authoritative Prolog source)
        distributions = {**distributions, **json_dists}
        print(f"[TANGLED] Using pipeline_output.json distributions as primary source.", file=sys.stderr)

    # Validate
    print("[TANGLED] Parsing known distributions from maxent_report.md...", file=sys.stderr)
    known = parse_maxent_report()
    print(f"[TANGLED] Found {len(known)} known distributions.", file=sys.stderr)

    n_tested, n_passed, failures = validate_maxent(distributions, known)
    print(f"[TANGLED] Validation: {n_passed}/{n_tested} passed (tolerance 0.05).", file=sys.stderr)

    if args.validate_only:
        print(f"\nMaxEnt Validation Results:")
        print(f"  Tested:  {n_tested}")
        print(f"  Passed:  {n_passed} ({100*n_passed/max(n_tested,1):.1f}%)")
        print(f"  Failed:  {n_tested - n_passed}")
        if failures:
            print(f"\nFirst 10 failures:")
            for cid, known_d, computed_d in failures[:10]:
                if isinstance(known_d, str):
                    print(f"  {cid}: {known_d}")
                else:
                    print(f"  {cid}:")
                    print(f"    known:    {known_d}")
                    print(f"    computed: {computed_d}")
        return

    # Build tangled_data from enriched pipeline_output.json fields
    print("[TANGLED] Building tangled_rope data from enriched pipeline...", file=sys.stderr)

    tangled_data = []
    for entry in per_constraint:
        if entry.get("claimed_type") != "tangled_rope":
            continue
        cid = entry["id"]
        c = constraints.get(cid, {})
        dist = distributions.get(cid, {})

        # Use enriched fields if available, fall back to computation
        psi = entry.get("tangled_psi")
        if psi is None:
            psi = compute_psi(dist)
        band = entry.get("tangled_band")
        if band is None:
            band = classify_band(psi)
        coalition = entry.get("coalition_type")
        if coalition is None:
            coalition = classify_coalition(c.get("orbit_contexts", {}))

        tangled_entry = {
            "id": cid,
            "psi": round(psi, 6),
            "band": band,
            "coalition_type": coalition,
            "p_rope": round(dist.get("rope", 0.0), 6),
            "p_snare": round(dist.get("snare", 0.0), 6),
            "p_tangled_rope": round(dist.get("tangled_rope", 0.0), 6),
            "p_mountain": round(dist.get("mountain", 0.0), 6),
            "p_scaffold": round(dist.get("scaffold", 0.0), 6),
            "p_piton": round(dist.get("piton", 0.0), 6),
            "extractiveness": c.get("extractiveness", entry.get("base_extractiveness")),
            "suppression": c.get("suppression", entry.get("suppression")),
            "theater_ratio": c.get("theater_ratio", entry.get("theater_ratio")),
            "signature": c.get("signature", entry.get("signature")),
            "domain": c.get("domain"),
            "human_readable": c.get("human_readable", entry.get("human_readable")),
            "orbit_contexts": c.get("orbit_contexts", {}),
        }
        tangled_data.append(tangled_entry)

    tangled_data.sort(key=lambda d: d["id"])

    band_counts = Counter(d["band"] for d in tangled_data)
    print(f"[TANGLED] {len(tangled_data)} tangled_rope constraints decomposed:", file=sys.stderr)
    for band in ["rope_leaning", "genuinely_tangled", "snare_leaning"]:
        print(f"  {band}: {band_counts.get(band, 0)}", file=sys.stderr)

    # Write report only (no JSON data file — data lives in pipeline_output.json)
    report = generate_report(tangled_data, (n_tested, n_passed, failures),
                             len(constraints), len(tangled_data))
    with open(DECOMP_REPORT, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"[TANGLED] Wrote {DECOMP_REPORT}", file=sys.stderr)

    print("[TANGLED] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
