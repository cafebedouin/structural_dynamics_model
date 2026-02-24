#!/usr/bin/env python3
"""Tangled Gradient Analysis

Computes per-constraint, per-perspective gradient vectors that position
tangled_rope constraints on the rope–snare continuum. Classifies subtypes
based on perspectival Chi gradient patterns.

Prerequisites:
  - Part 1: type_count_reconciliation.py (canonical population identified)
  - Part 2: perspective_chi field in enriched_pipeline.json

Reads:  outputs/enriched_pipeline.json, prolog/config.pl
Writes: docs/tangled_gradient_analysis.md
        outputs/tangled_gradient_data.json

Usage:  python3 python/tangled_gradient.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, read_config, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import MAXENT_TYPES, compute_psi, classify_band
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
REPORT_PATH = DOCS_DIR / "tangled_gradient_analysis.md"
DATA_PATH = OUTPUT_DIR / "tangled_gradient_data.json"

PERSPECTIVE_KEYS = ["powerless", "moderate", "institutional", "analytical"]

# ---------------------------------------------------------------------------
# Config thresholds
# ---------------------------------------------------------------------------

_CFG = read_config()

# Gradient boundaries
ROPE_CHI_CEILING = _CFG.get("rope_chi_ceiling", 0.35)
SNARE_CHI_FLOOR = _CFG.get("snare_chi_floor", 0.66)
ROPE_EPSILON_CEILING = _CFG.get("rope_epsilon_ceiling", 0.45)
SNARE_EPSILON_FLOOR = _CFG.get("snare_epsilon_floor", 0.46)
ROPE_SUPPRESSION_CEILING = _CFG.get("rope_suppression_ceiling", 0.16)
SNARE_SUPPRESSION_FLOOR = _CFG.get("snare_suppression_floor", 0.60)

# Composite weights
WEIGHTS_CHI_DOMINANT = (0.6, 0.2, 0.2)
WEIGHTS_EQUAL = (1/3, 1/3, 1/3)

# Clamp range for individual gradient components
CLAMP_MIN = -1.0
CLAMP_MAX = 2.0


# ---------------------------------------------------------------------------
# Gradient computation
# ---------------------------------------------------------------------------

def gradient_component(value, rope_ceiling, snare_floor):
    """Normalized position between rope ceiling and snare floor.

    Returns 0.0 at rope_ceiling, 1.0 at snare_floor.
    NOT clipped — off-scale values are diagnostic.
    """
    if snare_floor == rope_ceiling:
        return 0.5
    return (value - rope_ceiling) / (snare_floor - rope_ceiling)


def clamp(value, lo, hi):
    """Clamp value to [lo, hi]."""
    if value < lo:
        return lo
    if value > hi:
        return hi
    return value


def compute_gradient_vector(constraint):
    """Compute per-perspective gradient vectors for a constraint.

    Returns dict with per-perspective and composite gradient data.
    """
    pchi = constraint.get("perspective_chi", {})
    epsilon = constraint.get("base_extractiveness")
    suppression = constraint.get("suppression")

    # Per-perspective Chi gradients
    chi_gradients = {}
    for pk in PERSPECTIVE_KEYS:
        chi_val = pchi.get(pk, {}).get("chi")
        if chi_val is not None:
            chi_gradients[pk] = gradient_component(
                chi_val, ROPE_CHI_CEILING, SNARE_CHI_FLOOR)
        else:
            chi_gradients[pk] = None

    # Observer-invariant gradients
    g_epsilon = None
    if epsilon is not None:
        g_epsilon = gradient_component(
            epsilon, ROPE_EPSILON_CEILING, SNARE_EPSILON_FLOOR)

    g_suppression = None
    if suppression is not None:
        g_suppression = gradient_component(
            suppression, ROPE_SUPPRESSION_CEILING, SNARE_SUPPRESSION_FLOOR)

    # Composite gradients per perspective
    composites = {}
    for pk in PERSPECTIVE_KEYS:
        g_chi = chi_gradients.get(pk)
        if g_chi is None or g_epsilon is None or g_suppression is None:
            composites[pk] = {
                "chi_dominant": None,
                "chi_only": g_chi,
                "equal_weight": None,
            }
            continue

        # Clamped components for composites
        gc_chi = clamp(g_chi, CLAMP_MIN, CLAMP_MAX)
        gc_eps = clamp(g_epsilon, CLAMP_MIN, CLAMP_MAX)
        gc_sup = clamp(g_suppression, CLAMP_MIN, CLAMP_MAX)

        w_chi_d = WEIGHTS_CHI_DOMINANT
        chi_dominant = (w_chi_d[0] * gc_chi +
                        w_chi_d[1] * gc_eps +
                        w_chi_d[2] * gc_sup)

        w_eq = WEIGHTS_EQUAL
        equal_weight = (w_eq[0] * gc_chi +
                        w_eq[1] * gc_eps +
                        w_eq[2] * gc_sup)

        composites[pk] = {
            "chi_dominant": round(chi_dominant, 6),
            "chi_only": round(g_chi, 6),
            "equal_weight": round(equal_weight, 6),
        }

    return {
        "chi_gradients": {k: round(v, 6) if v is not None else None
                          for k, v in chi_gradients.items()},
        "g_epsilon": round(g_epsilon, 6) if g_epsilon is not None else None,
        "g_suppression": round(g_suppression, 6) if g_suppression is not None else None,
        "composites": composites,
    }


# ---------------------------------------------------------------------------
# Subtype classification
# ---------------------------------------------------------------------------

def classify_subtype(gradient_data):
    """Classify tangled_rope subtype based on clamped Chi-only gradient.

    Based on min/max of g_chi across 4 perspectives.
    """
    chi_grads = gradient_data["chi_gradients"]
    valid_grads = [v for v in chi_grads.values() if v is not None]
    if not valid_grads:
        return "unclassifiable"

    # Clamp for classification
    clamped = [clamp(g, CLAMP_MIN, CLAMP_MAX) for g in valid_grads]
    min_g = min(clamped)
    max_g = max(clamped)

    if max_g < 0.30:
        return "rope_dominant"
    if min_g > 0.70:
        return "snare_dominant"
    if min_g < 0.30 and max_g > 0.70:
        return "genuinely_perspectival"
    return "structurally_ambiguous"


# ---------------------------------------------------------------------------
# Perspectival test
# ---------------------------------------------------------------------------

def perspectival_test(gradient_data, constraint):
    """Compute perspectival variance and divergence metrics."""
    chi_grads = gradient_data["chi_gradients"]
    valid = {k: v for k, v in chi_grads.items() if v is not None}

    if len(valid) < 2:
        return {
            "chi_variance": None,
            "gradient_flip": False,
            "max_divergence_pair": None,
            "max_divergence_value": None,
            "epsilon_invariant": None,
        }

    values = list(valid.values())
    mean = sum(values) / len(values)
    variance = sum((v - mean) ** 2 for v in values) / len(values)

    # Gradient flip: does g_chi cross 0.5?
    has_below_half = any(v < 0.5 for v in values)
    has_above_half = any(v > 0.5 for v in values)
    gradient_flip = has_below_half and has_above_half

    # Max divergence pair
    keys = list(valid.keys())
    max_div = 0.0
    max_pair = None
    for i in range(len(keys)):
        for j in range(i + 1, len(keys)):
            div = abs(valid[keys[i]] - valid[keys[j]])
            if div > max_div:
                max_div = div
                max_pair = (keys[i], keys[j])

    # Epsilon invariance check
    pchi = constraint.get("perspective_chi", {})
    epsilons = set()
    for pk in PERSPECTIVE_KEYS:
        ep = pchi.get(pk, {}).get("epsilon")
        if ep is not None:
            epsilons.add(round(ep, 6))
    epsilon_invariant = len(epsilons) <= 1

    return {
        "chi_variance": round(variance, 6),
        "gradient_flip": gradient_flip,
        "max_divergence_pair": list(max_pair) if max_pair else None,
        "max_divergence_value": round(max_div, 6),
        "epsilon_invariant": epsilon_invariant,
    }


# ---------------------------------------------------------------------------
# Cross-references
# ---------------------------------------------------------------------------

def cross_reference(constraint, subtype):
    """Gather cross-reference data for a constraint."""
    return {
        "tangled_psi": constraint.get("tangled_psi"),
        "tangled_band": constraint.get("tangled_band"),
        "h1_band": constraint.get("h1_band"),
        "signature": constraint.get("signature"),
        "coalition_type": constraint.get("coalition_type"),
    }


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def write_report(population, results, subtype_counts, cross_tabs, divergence_stats):
    """Write the markdown analysis report."""
    now = datetime.now().strftime("%Y-%m-%d %H:%M")

    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write("# Tangled Gradient Analysis\n\n")
        f.write(f"**Generated:** {now}\n\n")
        f.write("---\n\n")

        # --- Executive Summary ---
        f.write("## 1. Executive Summary\n\n")
        f.write(f"Analyzed **{len(population)}** tangled_rope constraints "
                f"(defined by `claimed_type == 'tangled_rope'`).\n\n")
        f.write("Each constraint's position on the rope–snare continuum is "
                "measured via a gradient vector computed per-perspective. "
                "The primary axis (Chi) varies by observer; epsilon and "
                "suppression are observer-invariant.\n\n")

        f.write("### Subtype distribution\n\n")
        f.write("| Subtype | Count | % | Interpretation |\n")
        f.write("| :--- | ---: | ---: | :--- |\n")
        interpretations = {
            "rope_dominant": "Complicated rope — coordination primary",
            "snare_dominant": "Disguised snare — extraction primary",
            "genuinely_perspectival": "True tangled_rope — framework validation",
            "structurally_ambiguous": "Coordination and extraction inseparable",
            "unclassifiable": "Missing Chi data",
        }
        total = len(population)
        for st in ["rope_dominant", "snare_dominant", "genuinely_perspectival",
                    "structurally_ambiguous", "unclassifiable"]:
            count = subtype_counts.get(st, 0)
            pct = round(100 * count / total, 1) if total else 0
            interp = interpretations.get(st, "")
            f.write(f"| {st} | {count} | {pct}% | {interp} |\n")
        f.write(f"| **Total** | **{total}** | **100%** | |\n\n")

        # --- Data Sources ---
        f.write("## 2. Data Sources\n\n")
        f.write("- **Population**: `claimed_type == 'tangled_rope'` from "
                "`enriched_pipeline.json`\n")
        f.write("- **Chi values**: `perspective_chi` field (Prolog export, "
                "Part 2 of this audit arc)\n")
        f.write("- **Thresholds**: `prolog/config.pl` via `shared.loader.read_config()`\n\n")

        f.write("### Gradient boundaries\n\n")
        f.write("| Dimension | Rope ceiling | Snare floor | Gap |\n")
        f.write("| :--- | ---: | ---: | ---: |\n")
        chi_gap = SNARE_CHI_FLOOR - ROPE_CHI_CEILING
        eps_gap = SNARE_EPSILON_FLOOR - ROPE_EPSILON_CEILING
        sup_gap = SNARE_SUPPRESSION_FLOOR - ROPE_SUPPRESSION_CEILING
        f.write(f"| Chi | {ROPE_CHI_CEILING} | {SNARE_CHI_FLOOR} | "
                f"{chi_gap:.2f} |\n")
        f.write(f"| Epsilon | {ROPE_EPSILON_CEILING} | {SNARE_EPSILON_FLOOR} | "
                f"{eps_gap:.2f} |\n")
        f.write(f"| Suppression | {ROPE_SUPPRESSION_CEILING} | "
                f"{SNARE_SUPPRESSION_FLOOR} | {sup_gap:.2f} |\n\n")

        # --- Gradient Decomposition ---
        f.write("## 3. Gradient Decomposition\n\n")
        f.write("For each constraint C and perspective U_i:\n\n")
        f.write("```\n")
        f.write("g_chi(C, U_i) = (Chi(C, U_i) - rope_chi_ceiling) / "
                "(snare_chi_floor - rope_chi_ceiling)\n")
        f.write("g_epsilon(C)  = (epsilon(C) - rope_eps_ceiling) / "
                "(snare_eps_floor - rope_eps_ceiling)\n")
        f.write("g_suppression(C) = (supp(C) - rope_supp_ceiling) / "
                "(snare_supp_floor - rope_supp_ceiling)\n")
        f.write("```\n\n")
        f.write("Values are NOT clipped: g < 0.0 means below rope boundary; "
                "g > 1.0 means above snare boundary.\n\n")
        f.write("**Composite variants:**\n\n")
        f.write("1. **Chi-dominant** (0.6 / 0.2 / 0.2) — primary reporting scalar\n")
        f.write("2. **Chi-only** (g_chi alone) — most interpretable; only "
                "perspectival dimension with meaningful tangled zone\n")
        f.write("3. **Equal-weight** (1/3 each) — for comparison\n\n")
        f.write("Components are clamped to [-1, 2] before compositing to "
                "prevent g_epsilon from dominating (its denominator is only "
                f"{eps_gap:.2f}).\n\n")

        # --- Subtype Classification ---
        f.write("## 4. Subtype Classification\n\n")
        f.write("Based on clamped Chi-only gradient across 4 perspectives:\n\n")
        f.write("| Subtype | Criterion |\n")
        f.write("| :--- | :--- |\n")
        f.write("| rope_dominant | max(G_chi) < 0.30 |\n")
        f.write("| snare_dominant | min(G_chi) > 0.70 |\n")
        f.write("| genuinely_perspectival | min(G_chi) < 0.30 AND "
                "max(G_chi) > 0.70 |\n")
        f.write("| structurally_ambiguous | All else |\n\n")

        # Exemplars for each subtype
        for st in ["rope_dominant", "snare_dominant", "genuinely_perspectival",
                    "structurally_ambiguous"]:
            exemplars = [r for r in results if r["subtype"] == st][:5]
            if not exemplars:
                continue
            f.write(f"### {st} exemplars ({subtype_counts.get(st, 0)} total)\n\n")
            for ex in exemplars:
                cid = ex["constraint_id"]
                chi_g = ex["gradient"]["chi_gradients"]
                f.write(f"- **`{cid}`**: Chi gradients = ")
                parts = []
                for pk in PERSPECTIVE_KEYS:
                    v = chi_g.get(pk)
                    if v is not None:
                        parts.append(f"{pk[:3]}={v:.2f}")
                f.write(", ".join(parts))
                f.write(f" → {st}\n")
            f.write("\n")

        # --- Perspectival Test ---
        f.write("## 5. Perspectival Test\n\n")

        # Variance distribution by subtype
        f.write("### Chi variance by subtype\n\n")
        f.write("| Subtype | Count | Mean variance | Gradient flips |\n")
        f.write("| :--- | ---: | ---: | ---: |\n")
        for st in ["rope_dominant", "snare_dominant", "genuinely_perspectival",
                    "structurally_ambiguous"]:
            st_results = [r for r in results if r["subtype"] == st]
            variances = [r["perspectival"]["chi_variance"]
                         for r in st_results
                         if r["perspectival"]["chi_variance"] is not None]
            flips = sum(1 for r in st_results
                        if r["perspectival"]["gradient_flip"])
            mean_var = sum(variances) / len(variances) if variances else 0
            f.write(f"| {st} | {len(st_results)} | {mean_var:.4f} | {flips} |\n")
        f.write("\n")

        # Divergence pair frequency
        f.write("### Most common maximum-divergence pairs\n\n")
        f.write("| Pair | Frequency | Mean divergence |\n")
        f.write("| :--- | ---: | ---: |\n")
        for pair, stats in sorted(divergence_stats.items(),
                                   key=lambda x: -x[1]["count"]):
            mean_d = stats["total_div"] / stats["count"] if stats["count"] else 0
            f.write(f"| {pair} | {stats['count']} | {mean_d:.4f} |\n")
        f.write("\n")

        # Epsilon invariance
        eps_inv = sum(1 for r in results if r["perspectival"]["epsilon_invariant"])
        f.write(f"**Epsilon invariance:** {eps_inv}/{len(results)} constraints "
                f"have identical epsilon across all perspectives "
                f"({'confirmed' if eps_inv == len(results) else 'UNEXPECTED VIOLATIONS'}).\n\n")

        # --- Cross-References ---
        f.write("## 6. Cross-References\n\n")

        # PSI correlation
        f.write("### PSI band × subtype\n\n")
        psi_tab = cross_tabs.get("tangled_band", {})
        if psi_tab:
            bands = sorted(set().union(*(v.keys() for v in psi_tab.values())))
            f.write("| Subtype |")
            for b in bands:
                f.write(f" {b} |")
            f.write("\n| :--- |")
            for _ in bands:
                f.write(" ---: |")
            f.write("\n")
            for st in ["rope_dominant", "snare_dominant",
                        "genuinely_perspectival", "structurally_ambiguous"]:
                if st not in psi_tab:
                    continue
                f.write(f"| {st} |")
                for b in bands:
                    f.write(f" {psi_tab[st].get(b, 0)} |")
                f.write("\n")
            f.write("\n")

        # H1 band correlation
        f.write("### H1 band × subtype\n\n")
        h1_tab = cross_tabs.get("h1_band", {})
        if h1_tab:
            h1_vals = sorted(set().union(*(v.keys() for v in h1_tab.values())))
            f.write("| Subtype |")
            for h in h1_vals:
                f.write(f" H1={h} |")
            f.write("\n| :--- |")
            for _ in h1_vals:
                f.write(" ---: |")
            f.write("\n")
            for st in ["rope_dominant", "snare_dominant",
                        "genuinely_perspectival", "structurally_ambiguous"]:
                if st not in h1_tab:
                    continue
                f.write(f"| {st} |")
                for h in h1_vals:
                    f.write(f" {h1_tab[st].get(h, 0)} |")
                f.write("\n")
            f.write("\n")

        # Signature distribution
        f.write("### Signature × subtype\n\n")
        sig_tab = cross_tabs.get("signature", {})
        if sig_tab:
            all_sigs = Counter()
            for st_data in sig_tab.values():
                for sig, cnt in st_data.items():
                    all_sigs[sig] += cnt
            top_sigs = [s for s, _ in all_sigs.most_common(10)]
            f.write("| Subtype |")
            for s in top_sigs:
                f.write(f" {s} |")
            f.write("\n| :--- |")
            for _ in top_sigs:
                f.write(" ---: |")
            f.write("\n")
            for st in ["rope_dominant", "snare_dominant",
                        "genuinely_perspectival", "structurally_ambiguous"]:
                if st not in sig_tab:
                    continue
                f.write(f"| {st} |")
                for s in top_sigs:
                    f.write(f" {sig_tab[st].get(s, 0)} |")
                f.write("\n")
            f.write("\n")

        # Coalition type distribution
        f.write("### Coalition type × subtype\n\n")
        coal_tab = cross_tabs.get("coalition_type", {})
        if coal_tab:
            coal_types = sorted(set().union(*(v.keys() for v in coal_tab.values())))
            f.write("| Subtype |")
            for ct in coal_types:
                f.write(f" {ct} |")
            f.write("\n| :--- |")
            for _ in coal_types:
                f.write(" ---: |")
            f.write("\n")
            for st in ["rope_dominant", "snare_dominant",
                        "genuinely_perspectival", "structurally_ambiguous"]:
                if st not in coal_tab:
                    continue
                f.write(f"| {st} |")
                for ct in coal_types:
                    f.write(f" {coal_tab[st].get(ct, 0)} |")
                f.write("\n")
            f.write("\n")

        # --- Implications ---
        f.write("## 7. Implications\n\n")

        gp_count = subtype_counts.get("genuinely_perspectival", 0)
        rd_count = subtype_counts.get("rope_dominant", 0)
        sd_count = subtype_counts.get("snare_dominant", 0)
        sa_count = subtype_counts.get("structurally_ambiguous", 0)

        f.write(f"1. **Framework validation:** {gp_count} genuinely_perspectival "
                f"constraints ({round(100*gp_count/total, 1) if total else 0}%) "
                f"demonstrate that observer perspective genuinely changes the "
                f"rope↔snare classification — the core tangled_rope thesis.\n\n")
        f.write(f"2. **Population refinement:** {rd_count} rope_dominant + "
                f"{sd_count} snare_dominant constraints "
                f"({round(100*(rd_count+sd_count)/total, 1) if total else 0}%) "
                f"could potentially be reclassified to their dominant type, "
                f"reducing the tangled_rope population.\n\n")
        f.write(f"3. **Structural ambiguity:** {sa_count} structurally_ambiguous "
                f"constraints ({round(100*sa_count/total, 1) if total else 0}%) "
                f"resist clean decomposition — extraction and coordination are "
                f"genuinely intertwined.\n\n")


def write_data(results, subtype_counts, cross_tabs, divergence_stats, population):
    """Write JSON data output."""
    data = {
        "generated": datetime.now().isoformat(),
        "population_size": len(population),
        "population_definition": "claimed_type == 'tangled_rope'",
        "thresholds": {
            "rope_chi_ceiling": ROPE_CHI_CEILING,
            "snare_chi_floor": SNARE_CHI_FLOOR,
            "rope_epsilon_ceiling": ROPE_EPSILON_CEILING,
            "snare_epsilon_floor": SNARE_EPSILON_FLOOR,
            "rope_suppression_ceiling": ROPE_SUPPRESSION_CEILING,
            "snare_suppression_floor": SNARE_SUPPRESSION_FLOOR,
        },
        "subtype_counts": dict(subtype_counts),
        "per_constraint": {
            r["constraint_id"]: {
                "subtype": r["subtype"],
                "gradient": r["gradient"],
                "perspectival": r["perspectival"],
                "cross_references": r["cross_references"],
            }
            for r in results
        },
        "cross_tabulations": {
            dim: {st: dict(vals) for st, vals in tab.items()}
            for dim, tab in cross_tabs.items()
        },
        "divergence_pair_stats": {
            pair: {"count": s["count"],
                   "mean_divergence": round(s["total_div"] / s["count"], 6)
                   if s["count"] else 0}
            for pair, s in divergence_stats.items()
        },
    }

    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[GRADIENT] Loading enriched_pipeline.json...")
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                     schema=validate_enriched_pipeline)
    constraints = data["per_constraint"]
    print(f"[GRADIENT] Loaded {len(constraints)} constraints.")

    # Extract tangled_rope population (claimed_type)
    population = [c for c in constraints
                  if c.get("claimed_type") == "tangled_rope"]
    print(f"[GRADIENT] Tangled_rope population: {len(population)}")

    # Check for perspective_chi
    has_pchi = sum(1 for c in population if "perspective_chi" in c)
    print(f"[GRADIENT] With perspective_chi: {has_pchi}/{len(population)}")

    # Compute gradients, subtypes, perspectival tests
    results = []
    subtype_counts = Counter()
    divergence_stats = defaultdict(lambda: {"count": 0, "total_div": 0.0})

    for c in population:
        grad = compute_gradient_vector(c)
        subtype = classify_subtype(grad)
        persp = perspectival_test(grad, c)
        xref = cross_reference(c, subtype)

        subtype_counts[subtype] += 1

        # Track divergence pair stats
        pair = persp.get("max_divergence_pair")
        div_val = persp.get("max_divergence_value")
        if pair and div_val is not None:
            pair_key = f"{pair[0]}↔{pair[1]}"
            divergence_stats[pair_key]["count"] += 1
            divergence_stats[pair_key]["total_div"] += div_val

        results.append({
            "constraint_id": c["id"],
            "subtype": subtype,
            "gradient": grad,
            "perspectival": persp,
            "cross_references": xref,
        })

    print(f"[GRADIENT] Subtype distribution:")
    for st, cnt in sorted(subtype_counts.items()):
        print(f"  {st}: {cnt}")

    # Cross-tabulations
    cross_tabs = defaultdict(lambda: defaultdict(Counter))
    for r in results:
        st = r["subtype"]
        xref = r["cross_references"]
        for dim in ["tangled_band", "h1_band", "signature", "coalition_type"]:
            val = xref.get(dim)
            if val is not None:
                cross_tabs[dim][st][val] += 1

    # Write outputs
    DOCS_DIR.mkdir(parents=True, exist_ok=True)
    print("[GRADIENT] Writing report...")
    write_report(population, results, subtype_counts, cross_tabs,
                 divergence_stats)
    print("[GRADIENT] Writing data...")
    write_data(results, subtype_counts, cross_tabs, divergence_stats,
               population)

    print(f"[GRADIENT] Report: {REPORT_PATH}")
    print(f"[GRADIENT] Data:   {DATA_PATH}")

    # Verification checks
    total_subtypes = sum(subtype_counts.values())
    print(f"\n[GRADIENT] Verification:")
    print(f"  Subtypes sum to population: {total_subtypes} == {len(population)} "
          f"{'OK' if total_subtypes == len(population) else 'FAIL'}")

    gp = [r for r in results if r["subtype"] == "genuinely_perspectival"]
    if gp:
        gp_vars = [r["perspectival"]["chi_variance"]
                    for r in gp if r["perspectival"]["chi_variance"] is not None]
        other_vars = [r["perspectival"]["chi_variance"]
                      for r in results
                      if r["subtype"] != "genuinely_perspectival"
                      and r["perspectival"]["chi_variance"] is not None]
        if gp_vars and other_vars:
            mean_gp = sum(gp_vars) / len(gp_vars)
            mean_other = sum(other_vars) / len(other_vars)
            print(f"  genuinely_perspectival mean variance: {mean_gp:.4f} "
                  f"vs others: {mean_other:.4f} "
                  f"{'OK (higher)' if mean_gp > mean_other else 'UNEXPECTED'}")

    # Dominant divergence pair
    if divergence_stats:
        top_pair = max(divergence_stats.items(), key=lambda x: x[1]["count"])
        print(f"  Dominant divergence pair: {top_pair[0]} "
              f"(n={top_pair[1]['count']})")

    print("\n[GRADIENT] Done.")


if __name__ == "__main__":
    main()
