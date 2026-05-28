#!/usr/bin/env python3
"""false_ci_rope Internal Structure Audit

Performs a 5-step structural audit of the false_ci_rope signature population:
  1. Population extraction with full field census
  2. Binary gate profile analysis (4-gate; has_sunset_clause unavailable)
  3. Override-flipped vs non-flipped partition analysis
  4. Clustering within tangled_rope->snare boundary subpopulation
  5. Candidate subcategory identification

Reads:  outputs/enriched_pipeline.json
Writes: docs/false_ci_rope_audit.md
        outputs/false_ci_rope_audit_data.json

Usage:  python3 python/false_ci_rope_audit.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import MAXENT_TYPES
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
REPORT_PATH = DOCS_DIR / "false_ci_rope_audit.md"
DATA_PATH = OUTPUT_DIR / "false_ci_rope_audit_data.json"

# The 4 available Tier 1 binary gates.
# has_sunset_clause is Prolog-only (narrative_ontology.pl:326) and not
# exported to enriched_pipeline.json.
GATE_NAMES = [
    "emerges_naturally",
    "requires_active_enforcement",
    "has_coordination_function",
    "has_asymmetric_extraction",
]
GATE_SHORT = ["EN", "RAE", "HCF", "HAE"]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def compute_gates(r):
    """Return 4-tuple of boolean gates for a record."""
    en = bool(r.get("emerges_naturally"))
    rae = bool(r.get("requires_active_enforcement"))
    hcf = len(r.get("beneficiaries") or []) > 0
    hae = len(r.get("victims") or []) > 0
    return (en, rae, hcf, hae)


def gate_label(tup):
    """Human-readable label for a gate tuple, e.g. 'EN=F, RAE=T, HCF=T, HAE=T'."""
    return ", ".join(
        f"{s}={'T' if v else 'F'}" for s, v in zip(GATE_SHORT, tup)
    )


def raw_top_type(r):
    """Argmax of raw_maxent_probs (pre-override). None if missing."""
    raw = r.get("raw_maxent_probs") or {}
    if not raw:
        return None
    return max(raw, key=raw.get)


def raw_confidence_for_claimed(r):
    """P(claimed_type) from raw (pre-override) distribution."""
    raw = r.get("raw_maxent_probs") or {}
    ct = r.get("claimed_type")
    if not raw or not ct:
        return None
    return raw.get(ct, 0.0)


def desc_stats(values):
    """Pure-stdlib descriptive statistics. Returns dict or None if empty."""
    vals = [v for v in values if v is not None]
    n = len(vals)
    if n == 0:
        return None
    s = sorted(vals)
    mean = sum(s) / n
    median = s[n // 2] if n % 2 == 1 else (s[n // 2 - 1] + s[n // 2]) / 2.0
    variance = sum((x - mean) ** 2 for x in s) / n
    std = math.sqrt(variance)
    return {
        "n": n,
        "mean": round(mean, 6),
        "median": round(median, 6),
        "std": round(std, 6),
        "min": round(s[0], 6),
        "max": round(s[-1], 6),
        "q25": round(s[int(n * 0.25)], 6),
        "q75": round(s[int(n * 0.75)], 6),
    }


def pct(num, denom):
    """Safe percentage."""
    if denom == 0:
        return 0.0
    return round(100.0 * num / denom, 1)


def fmt(val, decimals=4):
    """Format a numeric value or return '---' for None."""
    if val is None:
        return "---"
    if isinstance(val, float):
        return f"{val:.{decimals}f}"
    return str(val)


def md_table(headers, rows, alignments=None):
    """Build a markdown table string."""
    if alignments is None:
        alignments = ["l"] * len(headers)
    sep_map = {"l": ":---", "r": "---:", "c": ":---:"}
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(sep_map.get(a, "---") for a in alignments) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(c) for c in row) + " |")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Step 1: Population Census
# ---------------------------------------------------------------------------


def step1_population_census(fcr, total_count):
    """Extract population-level distributions and metric summaries."""
    result = {
        "total_corpus": total_count,
        "fcr_count": len(fcr),
        "fcr_pct": pct(len(fcr), total_count),
    }

    result["claimed_type_dist"] = dict(
        Counter(r.get("claimed_type") or "None" for r in fcr).most_common()
    )
    result["boundary_dist"] = dict(
        Counter(r.get("boundary") or "None" for r in fcr).most_common()
    )
    result["confidence_band_dist"] = dict(
        Counter(r.get("confidence_band") or "None" for r in fcr).most_common()
    )
    result["tangled_band_dist"] = dict(
        Counter(r.get("tangled_band") or "None" for r in fcr).most_common()
    )
    result["coalition_dist"] = dict(
        Counter(r.get("coalition_type") or "other" for r in fcr).most_common()
    )

    for metric in [
        "base_extractiveness", "suppression", "theater_ratio",
        "purity_score", "confidence", "confidence_margin",
        "confidence_entropy", "tangled_psi",
    ]:
        vals = [r.get(metric) for r in fcr if r.get(metric) is not None]
        result[f"{metric}_stats"] = desc_stats(vals)

    return result


# ---------------------------------------------------------------------------
# Step 2: Binary Gate Profile Analysis
# ---------------------------------------------------------------------------


def _group_metrics(group):
    """Compute metric summaries for a group of constraint records."""
    out = {}
    for m in ["base_extractiveness", "suppression", "theater_ratio",
              "purity_score", "confidence", "tangled_psi"]:
        vals = [r.get(m) for r in group if r.get(m) is not None]
        out[m] = desc_stats(vals)
    return out


def step2_gate_profiles(fcr):
    """Compute 4-gate profile distribution with per-profile statistics."""
    by_profile = defaultdict(list)
    for r in fcr:
        by_profile[compute_gates(r)].append(r)

    profiles = []
    for tup, group in sorted(by_profile.items(), key=lambda kv: -len(kv[1])):
        ct_dist = dict(Counter(r.get("claimed_type") or "None" for r in group).most_common())
        band_dist = dict(Counter(r.get("confidence_band") or "None" for r in group))
        rival_dist = Counter(r.get("rival_type") or "None" for r in group)
        metrics = _group_metrics(group)

        profiles.append({
            "gate_tuple": list(tup),
            "label": gate_label(tup),
            "count": len(group),
            "pct": pct(len(group), len(fcr)),
            "claimed_type_dist": ct_dist,
            "confidence_band_dist": band_dist,
            "metrics": metrics,
            "top_rival": rival_dist.most_common(1)[0][0] if rival_dist else None,
        })

    # Marginal rates
    all_gates = [compute_gates(r) for r in fcr]
    marginals = {}
    for i, name in enumerate(GATE_NAMES):
        tc = sum(1 for t in all_gates if t[i])
        marginals[name] = {
            "true": tc,
            "false": len(all_gates) - tc,
            "p_true": round(tc / max(len(all_gates), 1), 4),
        }

    # Derived composites
    nlwb = sum(
        1 for r in fcr
        if bool(r.get("emerges_naturally"))
        and not bool(r.get("requires_active_enforcement"))
        and len(r.get("beneficiaries") or []) == 0
    )
    is_constructed = sum(1 for r in fcr if not bool(r.get("emerges_naturally")))

    return {
        "profiles": profiles,
        "marginals": marginals,
        "natural_law_without_beneficiary_count": nlwb,
        "is_constructed_count": is_constructed,
        "note": (
            "has_sunset_clause is evaluated only within the Prolog inference "
            "engine (narrative_ontology.pl:326) and is not exported to "
            "enriched_pipeline.json. This audit uses 4 of the 5 Tier 1 gates."
        ),
    }


# ---------------------------------------------------------------------------
# Step 3: Override-Flipped vs Non-Flipped
# ---------------------------------------------------------------------------


def _partition_profile(group):
    """Compute summary profile for a flipped/non-flipped partition."""
    metrics = {}
    for m in ["base_extractiveness", "suppression", "theater_ratio",
              "purity_score", "confidence", "confidence_margin", "tangled_psi"]:
        vals = [r.get(m) for r in group if r.get(m) is not None]
        metrics[m] = desc_stats(vals)

    gate_dist = Counter(compute_gates(r) for r in group)
    ct_dist = dict(Counter(r.get("claimed_type") or "None" for r in group).most_common())
    band_dist = dict(Counter(r.get("confidence_band") or "None" for r in group))
    rival_dist = dict(Counter(r.get("rival_type") or "None" for r in group).most_common())
    boundary_dist = dict(
        Counter(r.get("boundary") or "None" for r in group).most_common(5)
    )

    return {
        "n": len(group),
        "metrics": metrics,
        "gate_profile_dist": {
            gate_label(k): v for k, v in gate_dist.most_common()
        },
        "claimed_type_dist": ct_dist,
        "confidence_band_dist": band_dist,
        "rival_type_dist": rival_dist,
        "boundary_dist": boundary_dist,
    }


def step3_override_analysis(fcr):
    """Partition into flipped vs non-flipped by override effect."""
    flipped = []
    not_flipped = []
    no_data = []

    for r in fcr:
        rt = raw_top_type(r)
        ot = r.get("maxent_top_type")
        if rt is None or ot is None:
            no_data.append(r)
            continue
        if rt != ot:
            flipped.append(r)
        else:
            not_flipped.append(r)

    # Track specific flip transitions
    transitions = Counter()
    for r in flipped:
        rt = raw_top_type(r)
        ot = r.get("maxent_top_type")
        transitions[(rt, ot)] += 1

    return {
        "flipped": _partition_profile(flipped),
        "not_flipped": _partition_profile(not_flipped),
        "no_data_count": len(no_data),
        "flip_transitions": {
            f"{k[0]}->{k[1]}": v for k, v in transitions.most_common()
        },
    }


# ---------------------------------------------------------------------------
# Step 4: Clustering Within tangled_rope->snare Boundary
# ---------------------------------------------------------------------------


def step4_boundary_clustering(fcr):
    """Analyze the tangled_rope->snare boundary subpopulation."""
    boundary_pop = [r for r in fcr if r.get("boundary") == "tangled_rope->snare"]

    if not boundary_pop:
        return {"n": 0, "error": "No tangled_rope->snare boundary constraints"}

    # --- Group by binary gate profile ---
    by_gate = defaultdict(list)
    for r in boundary_pop:
        by_gate[compute_gates(r)].append(r)

    gate_clusters = {}
    for tup, group in sorted(by_gate.items(), key=lambda kv: -len(kv[1])):
        label = gate_label(tup)
        conf_vals = [r.get("confidence") for r in group
                     if r.get("confidence") is not None]
        psi_vals = [r.get("tangled_psi") for r in group
                    if r.get("tangled_psi") is not None]
        band_dist = dict(Counter(r.get("confidence_band") or "None" for r in group))
        flip_ct = sum(
            1 for r in group
            if raw_top_type(r) is not None
            and raw_top_type(r) != r.get("maxent_top_type")
        )
        gate_clusters[label] = {
            "gate_tuple": list(tup),
            "n": len(group),
            "pct": pct(len(group), len(boundary_pop)),
            "confidence_stats": desc_stats(conf_vals),
            "psi_stats": desc_stats(psi_vals),
            "confidence_band_dist": band_dist,
            "flipped_count": flip_ct,
            "flipped_pct": pct(flip_ct, len(group)),
        }

    # --- Group by tangled_band ---
    by_tband = defaultdict(list)
    for r in boundary_pop:
        by_tband[r.get("tangled_band") or "None"].append(r)

    tband_clusters = {}
    for band, group in sorted(by_tband.items(), key=lambda kv: -len(kv[1])):
        metrics = _group_metrics(group)
        coalition_dist = dict(
            Counter(r.get("coalition_type") or "other" for r in group).most_common()
        )
        flip_ct = sum(
            1 for r in group
            if raw_top_type(r) is not None
            and raw_top_type(r) != r.get("maxent_top_type")
        )
        tband_clusters[band] = {
            "n": len(group),
            "pct": pct(len(group), len(boundary_pop)),
            "metrics": metrics,
            "coalition_dist": coalition_dist,
            "flipped_count": flip_ct,
            "flipped_pct": pct(flip_ct, len(group)),
        }

    # --- PSI histogram ---
    psi_vals = [r.get("tangled_psi") for r in boundary_pop
                if r.get("tangled_psi") is not None]
    bins = [0.0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 1.001]
    psi_hist = []
    for i in range(len(bins) - 1):
        lo, hi = bins[i], bins[i + 1]
        count = sum(1 for v in psi_vals if lo <= v < hi)
        psi_hist.append({
            "lo": round(lo, 3),
            "hi": round(min(hi, 1.0), 3),
            "count": count,
        })

    return {
        "n": len(boundary_pop),
        "by_gate_profile": gate_clusters,
        "by_tangled_band": tband_clusters,
        "psi_histogram": psi_hist,
        "psi_stats": desc_stats(psi_vals),
        "psi_available": len(psi_vals),
        "psi_missing": len(boundary_pop) - len(psi_vals),
    }


# ---------------------------------------------------------------------------
# Step 5: Candidate Subcategories
# ---------------------------------------------------------------------------


def _exemplars(group, n=5):
    """Extract n exemplar records with key audit fields."""
    sample = sorted(group, key=lambda r: r.get("id", ""))[:n]
    return [
        {
            "id": r.get("id"),
            "claimed_type": r.get("claimed_type"),
            "maxent_top_type": r.get("maxent_top_type"),
            "raw_top_type": raw_top_type(r),
            "confidence": r.get("confidence"),
            "confidence_band": r.get("confidence_band"),
            "tangled_psi": r.get("tangled_psi"),
            "tangled_band": r.get("tangled_band"),
            "boundary": r.get("boundary"),
            "base_extractiveness": r.get("base_extractiveness"),
            "suppression": r.get("suppression"),
            "theater_ratio": r.get("theater_ratio"),
            "gate_profile": gate_label(compute_gates(r)),
        }
        for r in sample
    ]


def _confidence_std(group):
    """Standard deviation of confidence within a group."""
    vals = [r.get("confidence") for r in group if r.get("confidence") is not None]
    if len(vals) < 2:
        return None
    mean = sum(vals) / len(vals)
    return math.sqrt(sum((v - mean) ** 2 for v in vals) / len(vals))


def step5_subcategories(fcr, population_conf_std):
    """Identify candidate subcategories and assess internal homogeneity."""
    dominant_gate = (False, True, True, True)
    subcats = []

    # A. Near-snare borderline: snare_leaning + borderline confidence
    near_snare = [
        r for r in fcr
        if r.get("tangled_band") == "snare_leaning"
        and r.get("confidence_band") == "borderline"
    ]
    subcats.append(_build_subcat(
        "near_snare_borderline", near_snare, fcr, population_conf_std,
        "Snare-leaning (PSI > 0.7) with borderline confidence (P(claimed) < 0.5). "
        "The 3x override holds them as tangled_rope but the evidence strongly "
        "favors snare. These are the constraints most likely to be misclassified."
    ))

    # B. Override-flipped: raw top type != override top type
    override_flipped = [
        r for r in fcr
        if raw_top_type(r) is not None
        and raw_top_type(r) != r.get("maxent_top_type")
    ]
    subcats.append(_build_subcat(
        "override_flipped", override_flipped, fcr, population_conf_std,
        "Constraints where the 3x tangled_rope boost changed the argmax. "
        "Without the override, a different type would be the classifier's "
        "top pick. The override is doing real classificatory work here."
    ))

    # C. Genuinely tangled: PSI in [0.3, 0.7]
    genuinely_tangled = [
        r for r in fcr
        if r.get("tangled_psi") is not None
        and 0.3 <= r.get("tangled_psi") <= 0.7
    ]
    subcats.append(_build_subcat(
        "genuinely_tangled", genuinely_tangled, fcr, population_conf_std,
        "PSI in [0.3, 0.7] — the classifier genuinely cannot resolve whether "
        "this is more rope or more snare. These occupy the true middle ground "
        "of the tangled_rope concept."
    ))

    # D. Atypical gate profile: not the dominant (F,T,T,T)
    atypical_gate = [
        r for r in fcr if compute_gates(r) != dominant_gate
    ]
    atypical_subcat = _build_subcat(
        "atypical_gate_profile", atypical_gate, fcr, population_conf_std,
        "Constraints not matching the dominant gate profile "
        "(EN=F, RAE=T, HCF=T, HAE=T). These have different structural "
        "properties and may represent edge cases or distinct phenomena."
    )
    atypical_subcat["gate_dist"] = dict(
        Counter(gate_label(compute_gates(r)) for r in atypical_gate).most_common()
    )
    subcats.append(atypical_subcat)

    # E. Rope-leaning outliers: PSI < 0.3
    rope_leaning = [
        r for r in fcr
        if r.get("tangled_psi") is not None
        and r.get("tangled_psi") < 0.3
    ]
    subcats.append(_build_subcat(
        "rope_leaning_outliers", rope_leaning, fcr, population_conf_std,
        "PSI < 0.3 (rope-leaning) despite carrying the false_ci_rope signature. "
        "The classifier sees more rope than snare in these constraints, making "
        "the tangled_rope classification unusual."
    ))

    # F. Non-tangled_rope claimed types
    non_tr = [r for r in fcr if r.get("claimed_type") != "tangled_rope"]
    non_tr_subcat = _build_subcat(
        "non_tangled_rope_claimed", non_tr, fcr, population_conf_std,
        "Constraints with false_ci_rope signature but claimed_type is not "
        "tangled_rope. The signature override targets tangled_rope probability "
        "specifically, so the interaction with other claimed types is indirect."
    )
    non_tr_subcat["claimed_type_dist"] = dict(
        Counter(r.get("claimed_type") or "None" for r in non_tr).most_common()
    )
    subcats.append(non_tr_subcat)

    return subcats


def _build_subcat(name, group, full_pop, pop_conf_std, description):
    """Build a subcategory dict with homogeneity assessment."""
    grp_std = _confidence_std(group)
    more_homogeneous = None
    if grp_std is not None and pop_conf_std is not None:
        more_homogeneous = grp_std < pop_conf_std

    metrics = _group_metrics(group)
    band_dist = dict(Counter(r.get("confidence_band") or "None" for r in group))
    flip_ct = sum(
        1 for r in group
        if raw_top_type(r) is not None
        and raw_top_type(r) != r.get("maxent_top_type")
    )

    return {
        "name": name,
        "description": description,
        "count": len(group),
        "pct_of_fcr": pct(len(group), len(full_pop)),
        "metrics": metrics,
        "confidence_band_dist": band_dist,
        "override_flipped_count": flip_ct,
        "override_flipped_pct": pct(flip_ct, max(len(group), 1)),
        "confidence_std": round(grp_std, 6) if grp_std is not None else None,
        "more_homogeneous_than_population": more_homogeneous,
        "exemplars": _exemplars(group),
    }


# ---------------------------------------------------------------------------
# Report Generation
# ---------------------------------------------------------------------------


def generate_report(census, gates, override, boundary, subcats, timestamp):
    """Build the complete markdown audit report."""
    L = []  # lines accumulator

    def section(title, level=2):
        L.append("")
        L.append(f"{'#' * level} {title}")
        L.append("")

    def para(text):
        L.append(text)
        L.append("")

    # ---- Title ----
    L.append("# false_ci_rope Internal Structure Audit")
    L.append("")
    L.append(f"*Generated {timestamp} by `python/false_ci_rope_audit.py`*")

    # ---- Executive Summary ----
    section("Executive Summary")
    para(
        f"The `false_ci_rope` population contains **{census['fcr_count']}** "
        f"constraints ({census['fcr_pct']}% of {census['total_corpus']} total). "
        f"This signature applies a conditional 3x boost to `tangled_rope` "
        f"probability in the MaxEnt classifier, then renormalizes."
    )

    dom = gates["profiles"][0] if gates["profiles"] else None
    flip_n = override["flipped"]["n"]
    flip_pct = pct(flip_n, census["fcr_count"])
    bnd_n = boundary.get("n", 0)
    bnd_pct = pct(bnd_n, census["fcr_count"])

    L.append("Key findings:")
    L.append("")
    if dom:
        L.append(
            f"- **Dominant gate profile**: {dom['label']} — "
            f"{dom['count']}/{census['fcr_count']} ({dom['pct']}%)"
        )
    L.append(
        f"- **Override-flipped**: {flip_n} constraints ({flip_pct}%) "
        f"had their argmax changed by the override"
    )
    L.append(
        f"- **tangled_rope->snare boundary**: {bnd_n} constraints ({bnd_pct}%)"
    )
    L.append(f"- **{len(subcats)} candidate subcategories** identified")
    L.append("")

    # ---- Section 1: Population Census ----
    section("1. Population Census")

    # Claimed type distribution
    section("1.1 Claimed Type Distribution", 3)
    rows = [
        (ct, n, f"{pct(n, census['fcr_count'])}%")
        for ct, n in census["claimed_type_dist"].items()
    ]
    L.append(md_table(["Claimed Type", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    # Boundary distribution
    section("1.2 Boundary Distribution", 3)
    rows = [
        (b, n, f"{pct(n, census['fcr_count'])}%")
        for b, n in census["boundary_dist"].items()
    ]
    L.append(md_table(["Boundary", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    # Confidence band distribution
    section("1.3 Confidence Band Distribution", 3)
    rows = [
        (b, n, f"{pct(n, census['fcr_count'])}%")
        for b, n in census["confidence_band_dist"].items()
    ]
    L.append(md_table(["Band", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    # Tangled band distribution
    section("1.4 Tangled Band Distribution", 3)
    rows = [
        (b, n, f"{pct(n, census['fcr_count'])}%")
        for b, n in census["tangled_band_dist"].items()
    ]
    L.append(md_table(["Tangled Band", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    # Continuous metrics summary
    section("1.5 Continuous Metrics Summary", 3)
    metric_labels = [
        ("base_extractiveness", "Epsilon (base_extractiveness)"),
        ("suppression", "Sigma (suppression)"),
        ("theater_ratio", "Tau (theater_ratio)"),
        ("purity_score", "Purity Score"),
        ("confidence", "Confidence (P(claimed))"),
        ("confidence_margin", "Confidence Margin"),
        ("confidence_entropy", "Confidence Entropy"),
        ("tangled_psi", "Tangled PSI"),
    ]
    rows = []
    for key, label in metric_labels:
        st = census.get(f"{key}_stats")
        if st:
            rows.append((
                label, st["n"], fmt(st["mean"]), fmt(st["median"]),
                fmt(st["std"]), fmt(st["min"]), fmt(st["max"]),
            ))
        else:
            rows.append((label, 0, "---", "---", "---", "---", "---"))
    L.append(md_table(
        ["Metric", "N", "Mean", "Median", "Std", "Min", "Max"],
        rows, ["l", "r", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    # ---- Section 2: Binary Gate Profiles ----
    section("2. Binary Gate Profile Analysis")

    para(gates["note"])

    # Marginals
    section("2.1 Gate Marginal Rates", 3)
    rows = []
    for name in GATE_NAMES:
        m = gates["marginals"][name]
        rows.append((name, m["true"], m["false"], fmt(m["p_true"])))
    L.append(md_table(
        ["Gate", "True", "False", "P(True)"], rows, ["l", "r", "r", "r"],
    ))
    para(
        f"Derived: `is_constructed` (NOT emerges_naturally) = "
        f"{gates['is_constructed_count']}/{census['fcr_count']}. "
        f"`natural_law_without_beneficiary` = "
        f"{gates['natural_law_without_beneficiary_count']}/{census['fcr_count']}."
    )

    # Profile distribution
    section("2.2 Profile Distribution", 3)
    rows = []
    for p in gates["profiles"]:
        rows.append((
            p["label"], p["count"], f"{p['pct']}%",
            p["top_rival"] or "---",
        ))
    L.append(md_table(
        ["Profile", "Count", "%", "Top Rival"],
        rows, ["l", "r", "r", "l"],
    ))
    L.append("")

    # Per-profile detailed table
    section("2.3 Per-Profile Metric Summary", 3)
    rows = []
    for p in gates["profiles"]:
        m = p["metrics"]
        conf = m.get("confidence")
        eps = m.get("base_extractiveness")
        sig = m.get("suppression")
        tau = m.get("theater_ratio")
        bd = p["confidence_band_dist"]
        borderline_n = bd.get("borderline", 0)
        deep_n = bd.get("deep", 0)
        ct_top = max(p["claimed_type_dist"], key=p["claimed_type_dist"].get) \
            if p["claimed_type_dist"] else "---"
        rows.append((
            p["label"], p["count"],
            ct_top,
            fmt(conf["mean"] if conf else None),
            f"{pct(borderline_n, p['count'])}%",
            f"{pct(deep_n, p['count'])}%",
            fmt(eps["mean"] if eps else None),
            fmt(sig["mean"] if sig else None),
            fmt(tau["mean"] if tau else None),
        ))
    L.append(md_table(
        ["Profile", "N", "Top Claimed", "Mean Conf",
         "% Border", "% Deep", "Mean Eps", "Mean Sig", "Mean Tau"],
        rows, ["l", "r", "l", "r", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    # Per-profile claimed type breakdown for top profiles
    section("2.4 Claimed Type Breakdown by Profile", 3)
    for p in gates["profiles"][:5]:  # top 5
        L.append(f"**{p['label']}** (N={p['count']}):")
        L.append("")
        rows = [(ct, n) for ct, n in p["claimed_type_dist"].items()]
        L.append(md_table(["Claimed Type", "Count"], rows, ["l", "r"]))
        L.append("")

    # ---- Section 3: Override Analysis ----
    section("3. Override-Flipped vs Non-Flipped")

    fl = override["flipped"]
    nf = override["not_flipped"]
    para(
        f"**Flipped**: {fl['n']} constraints where the 3x tangled_rope boost "
        f"changed the argmax type.\n"
        f"**Non-flipped**: {nf['n']} constraints where the override did not "
        f"change the top type.\n"
        f"**No data**: {override['no_data_count']} constraints lacking "
        f"raw_maxent_probs."
    )

    # Flip transitions
    if override["flip_transitions"]:
        section("3.1 Flip Transitions", 3)
        rows = [(t, n) for t, n in override["flip_transitions"].items()]
        L.append(md_table(["Transition", "Count"], rows, ["l", "r"]))
        L.append("")

    # Side-by-side metric comparison
    section("3.2 Metric Comparison", 3)
    metric_keys = [
        "base_extractiveness", "suppression", "theater_ratio",
        "purity_score", "confidence", "confidence_margin", "tangled_psi",
    ]
    rows = []
    for m in metric_keys:
        fl_st = fl["metrics"].get(m)
        nf_st = nf["metrics"].get(m)
        rows.append((
            m,
            fmt(fl_st["mean"] if fl_st else None),
            fmt(fl_st["median"] if fl_st else None),
            fmt(fl_st["std"] if fl_st else None),
            fmt(nf_st["mean"] if nf_st else None),
            fmt(nf_st["median"] if nf_st else None),
            fmt(nf_st["std"] if nf_st else None),
        ))
    L.append(md_table(
        ["Metric", "Flip Mean", "Flip Med", "Flip Std",
         "NoFlip Mean", "NoFlip Med", "NoFlip Std"],
        rows, ["l", "r", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    # Confidence band comparison
    section("3.3 Confidence Band Comparison", 3)
    all_bands = sorted(set(
        list(fl.get("confidence_band_dist", {}))
        + list(nf.get("confidence_band_dist", {}))
    ))
    rows = []
    for b in all_bands:
        fl_n = fl["confidence_band_dist"].get(b, 0)
        nf_n = nf["confidence_band_dist"].get(b, 0)
        rows.append((
            b,
            fl_n, f"{pct(fl_n, fl['n'])}%",
            nf_n, f"{pct(nf_n, nf['n'])}%",
        ))
    L.append(md_table(
        ["Band", "Flip N", "Flip %", "NoFlip N", "NoFlip %"],
        rows, ["l", "r", "r", "r", "r"],
    ))
    L.append("")

    # Gate profile comparison
    section("3.4 Gate Profile Distribution", 3)
    fl_gates = fl.get("gate_profile_dist", {})
    nf_gates = nf.get("gate_profile_dist", {})
    all_gate_labels = sorted(set(list(fl_gates) + list(nf_gates)),
                             key=lambda k: -(fl_gates.get(k, 0) + nf_gates.get(k, 0)))
    rows = []
    for gl in all_gate_labels:
        rows.append((
            gl,
            fl_gates.get(gl, 0), f"{pct(fl_gates.get(gl, 0), fl['n'])}%",
            nf_gates.get(gl, 0), f"{pct(nf_gates.get(gl, 0), nf['n'])}%",
        ))
    L.append(md_table(
        ["Profile", "Flip N", "Flip %", "NoFlip N", "NoFlip %"],
        rows, ["l", "r", "r", "r", "r"],
    ))
    L.append("")

    # Rival type comparison
    section("3.5 Rival Type Distribution", 3)
    fl_rivals = fl.get("rival_type_dist", {})
    nf_rivals = nf.get("rival_type_dist", {})
    all_rivals = sorted(set(list(fl_rivals) + list(nf_rivals)),
                        key=lambda k: -(fl_rivals.get(k, 0) + nf_rivals.get(k, 0)))
    rows = []
    for rv in all_rivals:
        rows.append((
            rv,
            fl_rivals.get(rv, 0), f"{pct(fl_rivals.get(rv, 0), fl['n'])}%",
            nf_rivals.get(rv, 0), f"{pct(nf_rivals.get(rv, 0), nf['n'])}%",
        ))
    L.append(md_table(
        ["Rival Type", "Flip N", "Flip %", "NoFlip N", "NoFlip %"],
        rows, ["l", "r", "r", "r", "r"],
    ))
    L.append("")

    # Interpretation
    section("3.6 Interpretation", 3)
    para(
        "Flipped constraints had a raw MaxEnt top type that was *not* "
        "tangled_rope, but the 3x conditional boost on tangled_rope was "
        "sufficient to change the argmax. Non-flipped constraints already "
        "had tangled_rope as raw top type, so the override only increased "
        "its margin."
    )
    para(
        "The key diagnostic question: do the flipped constraints share a "
        "distinctive binary gate profile or continuous metric signature? "
        "If yes, that profile is a candidate for a new signature subcategory. "
        "If the gate profile distributions are similar between flipped and "
        "non-flipped, then the override is compensating for something the "
        "gates cannot see — likely fine-grained continuous metric positioning "
        "near the tangled_rope/snare decision boundary."
    )

    # ---- Section 4: Boundary Clustering ----
    section("4. Clustering Within tangled_rope->snare Boundary")

    para(
        f"This section focuses on the {boundary.get('n', 0)} false_ci_rope "
        f"constraints on the tangled_rope->snare boundary specifically."
    )

    # By gate profile
    section("4.1 By Binary Gate Profile", 3)
    gc = boundary.get("by_gate_profile", {})
    rows = []
    for label, data in sorted(gc.items(), key=lambda kv: -kv[1]["n"]):
        cs = data.get("confidence_stats")
        ps = data.get("psi_stats")
        bd = data.get("confidence_band_dist", {})
        rows.append((
            label, data["n"], f"{data['pct']}%",
            fmt(cs["mean"] if cs else None),
            fmt(cs["std"] if cs else None),
            f"{pct(bd.get('borderline', 0), data['n'])}%",
            fmt(ps["mean"] if ps else None),
            fmt(ps["std"] if ps else None),
            data["flipped_count"],
        ))
    L.append(md_table(
        ["Profile", "N", "%", "Conf Mean", "Conf Std",
         "% Border", "PSI Mean", "PSI Std", "Flipped"],
        rows, ["l", "r", "r", "r", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    # By tangled band
    section("4.2 By Tangled Band", 3)
    tc = boundary.get("by_tangled_band", {})
    rows = []
    for band, data in sorted(tc.items(), key=lambda kv: -kv[1]["n"]):
        m = data.get("metrics", {})
        conf_st = m.get("confidence")
        psi_st = m.get("tangled_psi")
        eps_st = m.get("base_extractiveness")
        rows.append((
            band, data["n"], f"{data['pct']}%",
            fmt(conf_st["mean"] if conf_st else None),
            fmt(eps_st["mean"] if eps_st else None),
            fmt(psi_st["mean"] if psi_st else None),
            data["flipped_count"], f"{data['flipped_pct']}%",
        ))
    L.append(md_table(
        ["Tangled Band", "N", "%", "Mean Conf", "Mean Eps",
         "Mean PSI", "Flipped", "Flip %"],
        rows, ["l", "r", "r", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    # PSI histogram
    section("4.3 PSI Distribution (Histogram)", 3)
    psi_hist = boundary.get("psi_histogram", [])
    psi_st = boundary.get("psi_stats")
    if psi_st:
        para(
            f"PSI available for {boundary['psi_available']} of "
            f"{boundary['n']} boundary constraints "
            f"({boundary['psi_missing']} missing). "
            f"Mean={fmt(psi_st['mean'])}, Median={fmt(psi_st['median'])}, "
            f"Std={fmt(psi_st['std'])}."
        )
    rows = []
    for b in psi_hist:
        bar = "#" * max(1, int(b["count"] / max(1, boundary["n"]) * 100))
        rows.append((
            f"[{fmt(b['lo'], 2)}, {fmt(b['hi'], 2)})",
            b["count"], bar,
        ))
    L.append(md_table(
        ["PSI Range", "Count", "Distribution"],
        rows, ["l", "r", "l"],
    ))
    L.append("")

    para(
        "PSI near 0.5 means the classifier genuinely cannot resolve "
        "rope vs snare. PSI near 1.0 means the classifier thinks it's "
        "really a snare. PSI near 0.0 means it looks like a rope."
    )

    # ---- Section 5: Candidate Subcategories ----
    section("5. Candidate Subcategories")

    para(
        "Based on the above analysis, the following candidate groupings "
        "were identified. For each, we assess whether the group is more "
        "internally homogeneous (lower confidence std) than the full "
        "false_ci_rope population."
    )

    for sc in subcats:
        section(f"5.{subcats.index(sc)+1} {sc['name']}", 3)
        para(sc["description"])
        L.append(f"- **Count**: {sc['count']} ({sc['pct_of_fcr']}% of FCR)")
        homo = sc.get("more_homogeneous_than_population")
        if homo is not None:
            homo_label = "Yes" if homo else "No"
            L.append(
                f"- **More homogeneous than population**: {homo_label} "
                f"(std={fmt(sc.get('confidence_std'))})"
            )
        flip_ct = sc.get("override_flipped_count", 0)
        L.append(
            f"- **Override flipped**: {flip_ct} "
            f"({sc.get('override_flipped_pct', 0)}%)"
        )
        bd = sc.get("confidence_band_dist", {})
        if bd:
            L.append(
                f"- **Confidence bands**: "
                + ", ".join(f"{b}={n}" for b, n in bd.items())
            )
        # Extra fields for specific subcats
        if "gate_dist" in sc:
            L.append(f"- **Gate profiles**: "
                     + ", ".join(f"{k}: {v}" for k, v in sc["gate_dist"].items()))
        if "claimed_type_dist" in sc:
            L.append(f"- **Claimed types**: "
                     + ", ".join(f"{k}: {v}" for k, v in sc["claimed_type_dist"].items()))
        L.append("")

        # Exemplars table
        exs = sc.get("exemplars", [])
        if exs:
            L.append("**Exemplars:**")
            L.append("")
            rows = []
            for e in exs:
                rows.append((
                    e["id"],
                    e["claimed_type"],
                    e["maxent_top_type"],
                    fmt(e.get("confidence")),
                    e.get("confidence_band") or "---",
                    fmt(e.get("tangled_psi")),
                    e.get("gate_profile"),
                ))
            L.append(md_table(
                ["ID", "Claimed", "MaxEnt Top", "Conf",
                 "Band", "PSI", "Gates"],
                rows, ["l", "l", "l", "r", "l", "r", "l"],
            ))
            L.append("")

    # ---- Section 6: Conclusions ----
    section("6. Conclusions")

    para(
        "This section summarizes what the data shows about whether "
        "`false_ci_rope` is one thing or several things wearing the same label."
    )

    # Auto-generate some observations based on data
    if dom and dom["pct"] > 85:
        para(
            f"**Gate profile homogeneity**: The dominant profile "
            f"({dom['label']}) accounts for {dom['pct']}% of the population. "
            f"The binary gates do not subdivide false_ci_rope into many "
            f"structurally distinct groups — the vast majority share the same "
            f"gate fingerprint."
        )

    if flip_pct > 10:
        para(
            f"**Override impact**: {flip_pct}% of constraints had their "
            f"top type flipped by the override. This is a substantial "
            f"minority where the override is doing real classificatory work "
            f"rather than merely reinforcing an existing lean."
        )

    bnd_pop = boundary.get("n", 0)
    if bnd_pop > 0:
        snare_leaning_data = boundary.get("by_tangled_band", {}).get("snare_leaning")
        if snare_leaning_data:
            sl_pct = snare_leaning_data["pct"]
            para(
                f"**Boundary population structure**: Of the {bnd_pop} "
                f"constraints on the tangled_rope->snare boundary, "
                f"{sl_pct}% are snare-leaning by PSI. The tangled_rope->snare "
                f"boundary is overwhelmingly populated by constraints that "
                f"the classifier considers more snare than rope."
            )

    para(
        "**Assessment**: The binary gate profiles provide limited subdivision "
        "of the false_ci_rope population because the dominant profile is so "
        "prevalent. The more meaningful axis of variation is the *continuous* "
        "one: where a constraint falls on the PSI spectrum (rope-leaning vs "
        "genuinely tangled vs snare-leaning) and whether the override flips "
        "the argmax. These continuous dimensions, not the binary gates, are "
        "what differentiate the population's internal structure."
    )

    return "\n".join(L)


# ---------------------------------------------------------------------------
# JSON Output
# ---------------------------------------------------------------------------


def generate_json(census, gates, override, boundary, subcats, timestamp):
    """Build the JSON output structure."""
    return {
        "metadata": {
            "generated": timestamp,
            "script": "python/false_ci_rope_audit.py",
            "source": "outputs/enriched_pipeline.json",
            "total_corpus": census["total_corpus"],
            "fcr_count": census["fcr_count"],
            "note": gates["note"],
        },
        "step1_population_census": census,
        "step2_gate_profiles": gates,
        "step3_override_analysis": override,
        "step4_boundary_clustering": boundary,
        "step5_subcategories": subcats,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    print("[FCR-AUDIT] Loading enriched_pipeline.json...", file=sys.stderr)
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                     schema=validate_enriched_pipeline)
    per_constraint = data.get("per_constraint", [])
    fcr = [r for r in per_constraint if r.get("signature") == "false_ci_rope"]
    total = len(per_constraint)
    print(f"[FCR-AUDIT] Found {len(fcr)} false_ci_rope constraints "
          f"(of {total} total)", file=sys.stderr)

    if not fcr:
        print("[FCR-AUDIT] ERROR: No false_ci_rope constraints found",
              file=sys.stderr)
        sys.exit(1)

    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Population-level confidence std for homogeneity comparison
    pop_conf_vals = [r.get("confidence") for r in fcr
                     if r.get("confidence") is not None]
    pop_conf_std = None
    if len(pop_conf_vals) >= 2:
        mean_c = sum(pop_conf_vals) / len(pop_conf_vals)
        pop_conf_std = math.sqrt(
            sum((v - mean_c) ** 2 for v in pop_conf_vals) / len(pop_conf_vals)
        )

    print("[FCR-AUDIT] Step 1: Population census...", file=sys.stderr)
    census = step1_population_census(fcr, total)

    print("[FCR-AUDIT] Step 2: Gate profiles...", file=sys.stderr)
    gates = step2_gate_profiles(fcr)

    print("[FCR-AUDIT] Step 3: Override analysis...", file=sys.stderr)
    override = step3_override_analysis(fcr)

    print("[FCR-AUDIT] Step 4: Boundary clustering...", file=sys.stderr)
    boundary = step4_boundary_clustering(fcr)

    print("[FCR-AUDIT] Step 5: Subcategories...", file=sys.stderr)
    subcats = step5_subcategories(fcr, pop_conf_std)

    # Generate outputs
    print("[FCR-AUDIT] Generating report...", file=sys.stderr)
    report_md = generate_report(census, gates, override, boundary,
                                subcats, timestamp)

    DOCS_DIR.mkdir(parents=True, exist_ok=True)
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report_md)
    print(f"[FCR-AUDIT] Wrote {REPORT_PATH}", file=sys.stderr)

    json_data = generate_json(census, gates, override, boundary,
                              subcats, timestamp)
    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(json_data, f, indent=2)
    print(f"[FCR-AUDIT] Wrote {DATA_PATH}", file=sys.stderr)

    print("[FCR-AUDIT] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
