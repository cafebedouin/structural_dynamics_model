#!/usr/bin/env python3
"""Scaffold & Piton Gate Trace Audit

Traces each scaffold and piton constraint through the complete Prolog gate
path (drl_core.pl classify_from_metrics/6) to diagnose why MaxEnt confidence
is universally borderline for these two types.

Seven analysis steps:
  1. Population extraction — field census of all scaffold/piton constraints
  2. Gate profile census — 4-gate distribution, cross-reference with false_ci_rope
  3. Gate path trace — simulate classify_from_metrics priority chain
  4. Rival type analysis — structural differences between rival groups
  5. Sunset clause inference — infer has_sunset_clause from RAE and pipeline status
  6. Hypothesis assessment — test H1 (piton→RAE=F), H2 (scaffold→sunset), H3 (RAE=F subgroup)
  7. Recommendations — data-driven prescriptions

Reads:  outputs/enriched_pipeline.json
Writes: outputs/scaffold_piton_gate_audit.md (archived copy: audits/2026-02-23_scaffold_piton_gate/)
        outputs/scaffold_piton_gate_audit_data.json

Usage:  python3 python/scaffold_piton_gate_audit.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from shared.loader import load_json, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import MAXENT_TYPES
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
REPORT_PATH = OUTPUT_DIR / "scaffold_piton_gate_audit.md"  # workspace; archived copy: audits/2026-02-23_scaffold_piton_gate/
DATA_PATH = OUTPUT_DIR / "scaffold_piton_gate_audit_data.json"

# ---------------------------------------------------------------------------
# Prolog gate thresholds — from prolog/config.pl
# Priority order: Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton
# ---------------------------------------------------------------------------

SCAFFOLD_EXTRACTION_CEIL = 0.45  # Must match config.pl param: scaffold_extraction_ceil -- updated 2026-02-24
PITON_EXTRACTION_CEILING = 0.45  # Must match config.pl param: piton_extraction_ceiling -- updated 2026-02-24
PITON_EPSILON_FLOOR = 0.10              # config.pl: piton_epsilon_floor
PITON_THEATER_FLOOR = 0.70             # config.pl: piton_theater_floor
SNARE_CHI_FLOOR = 0.66                 # config.pl: snare_chi_floor
SNARE_EPSILON_FLOOR = 0.46             # config.pl: snare_epsilon_floor
SNARE_SUPPRESSION_FLOOR = 0.60         # config.pl:167 snare_suppression_floor
TANGLED_ROPE_CHI_FLOOR = 0.40          # config.pl: tangled_rope_chi_floor
TANGLED_ROPE_CHI_CEIL = 0.90           # config.pl: tangled_rope_chi_ceil
TANGLED_ROPE_EPSILON_FLOOR = 0.30      # config.pl: tangled_rope_epsilon_floor
TANGLED_ROPE_SUPPRESSION_FLOOR = 0.40  # config.pl: tangled_rope_suppression_floor
ROPE_CHI_CEILING = 0.35               # config.pl: rope_chi_ceiling
ROPE_EPSILON_CEILING = 0.45           # config.pl: rope_epsilon_ceiling
MOUNTAIN_SUPPRESSION_CEILING = 0.05    # config.pl: mountain_suppression_ceiling
MOUNTAIN_EXTRACTIVENESS_MAX = 0.25     # config.pl: mountain_extractiveness_max

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

TARGET_TYPES = {"scaffold", "piton"}

# ---------------------------------------------------------------------------
# Helpers (same pattern as false_ci_rope_audit.py)
# ---------------------------------------------------------------------------


def compute_gates(r):
    """Return 4-tuple of boolean gates for a record."""
    en = bool(r.get("emerges_naturally"))
    rae = bool(r.get("requires_active_enforcement"))
    hcf = len(r.get("beneficiaries") or []) > 0
    hae = len(r.get("victims") or []) > 0
    return (en, rae, hcf, hae)


def gate_label(tup):
    """Human-readable label for a gate tuple."""
    return ", ".join(
        f"{s}={'T' if v else 'F'}" for s, v in zip(GATE_SHORT, tup)
    )


def raw_top_type(r):
    """Argmax of raw_maxent_probs (pre-override). None if missing."""
    raw = r.get("raw_maxent_probs") or {}
    if not raw:
        return None
    return max(raw, key=raw.get)


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


def _group_metrics(group):
    """Compute metric summaries for a group of constraint records."""
    out = {}
    for m in ["base_extractiveness", "suppression", "theater_ratio",
              "purity_score", "confidence"]:
        vals = [r.get(m) for r in group if r.get(m) is not None]
        out[m] = desc_stats(vals)
    return out


# ---------------------------------------------------------------------------
# Step 1: Population Extraction
# ---------------------------------------------------------------------------


def step1_population(pop, total_count):
    """Extract population-level distributions and metric summaries."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    def _census(group, label):
        result = {"label": label, "count": len(group)}
        result["signature_dist"] = dict(
            Counter(r.get("signature") or "None" for r in group).most_common()
        )
        result["maxent_top_type_dist"] = dict(
            Counter(r.get("maxent_top_type") or "None" for r in group).most_common()
        )
        result["raw_maxent_top_type_dist"] = dict(
            Counter(raw_top_type(r) or "None" for r in group).most_common()
        )
        result["confidence_band_dist"] = dict(
            Counter(r.get("confidence_band") or "None" for r in group).most_common()
        )
        result["boundary_dist"] = dict(
            Counter(r.get("boundary") or "None" for r in group).most_common()
        )
        result["rival_type_dist"] = dict(
            Counter(r.get("rival_type") or "None" for r in group).most_common()
        )
        result["coalition_dist"] = dict(
            Counter(r.get("coalition_type") or "other" for r in group).most_common()
        )
        for metric in ["base_extractiveness", "suppression", "theater_ratio",
                        "purity_score", "confidence", "confidence_margin",
                        "confidence_entropy"]:
            vals = [r.get(metric) for r in group if r.get(metric) is not None]
            result[f"{metric}_stats"] = desc_stats(vals)
        return result

    return {
        "total_corpus": total_count,
        "target_count": len(pop),
        "scaffold": _census(scaffolds, "scaffold"),
        "piton": _census(pitons, "piton"),
    }


# ---------------------------------------------------------------------------
# Step 2: Gate Profile Census
# ---------------------------------------------------------------------------


def step2_gate_profiles(pop, fcr_all):
    """Compute 4-gate profile distribution with cross-reference to false_ci_rope."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    # false_ci_rope RAE=F base rate
    fcr_total = len(fcr_all)
    fcr_rae_f = sum(1 for r in fcr_all if not bool(r.get("requires_active_enforcement")))
    fcr_rae_f_rate = fcr_rae_f / max(fcr_total, 1)

    def _profile_census(group, label):
        by_profile = defaultdict(list)
        for r in group:
            by_profile[compute_gates(r)].append(r)

        profiles = []
        for tup, grp in sorted(by_profile.items(), key=lambda kv: -len(kv[1])):
            rival_dist = dict(Counter(r.get("rival_type") or "None" for r in grp).most_common())
            band_dist = dict(Counter(r.get("confidence_band") or "None" for r in grp))
            metrics = _group_metrics(grp)
            profiles.append({
                "gate_tuple": list(tup),
                "label": gate_label(tup),
                "count": len(grp),
                "pct": pct(len(grp), len(group)),
                "rival_type_dist": rival_dist,
                "confidence_band_dist": band_dist,
                "metrics": metrics,
            })

        # Marginal rates
        all_gates = [compute_gates(r) for r in group]
        marginals = {}
        for i, name in enumerate(GATE_NAMES):
            tc = sum(1 for t in all_gates if t[i])
            marginals[name] = {
                "true": tc,
                "false": len(all_gates) - tc,
                "p_true": round(tc / max(len(all_gates), 1), 4),
            }

        rae_f_count = marginals["requires_active_enforcement"]["false"]
        rae_f_rate = rae_f_count / max(len(group), 1)

        return {
            "label": label,
            "count": len(group),
            "profiles": profiles,
            "marginals": marginals,
            "rae_f_count": rae_f_count,
            "rae_f_rate": round(rae_f_rate, 4),
            "rae_f_enrichment_vs_fcr": round(rae_f_rate / max(fcr_rae_f_rate, 0.001), 2),
        }

    # Cross-reference: dominant vs atypical profile counts
    dominant = (False, True, True, True)
    atypical = (False, False, True, True)

    def _profile_xref(group):
        gates_list = [compute_gates(r) for r in group]
        return {
            "dominant_FTTT": sum(1 for g in gates_list if g == dominant),
            "atypical_FFTT": sum(1 for g in gates_list if g == atypical),
            "other": sum(1 for g in gates_list if g != dominant and g != atypical),
        }

    return {
        "scaffold": _profile_census(scaffolds, "scaffold"),
        "piton": _profile_census(pitons, "piton"),
        "scaffold_xref": _profile_xref(scaffolds),
        "piton_xref": _profile_xref(pitons),
        "fcr_base_rates": {
            "total": fcr_total,
            "rae_f_count": fcr_rae_f,
            "rae_f_rate": round(fcr_rae_f_rate, 4),
        },
    }


# ---------------------------------------------------------------------------
# Step 3: Gate Path Trace
# ---------------------------------------------------------------------------


def _trace_gate_path(r):
    """Trace classify_from_metrics/6 priority chain for a single constraint.

    Uses base_extractiveness as proxy for Chi (analytical context: d≈0, f(d)≈1).
    Returns a dict with the trace result.

    Gate priority (drl_core.pl:170):
      Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > unknown
    """
    eps = r.get("base_extractiveness") or 0
    sigma = r.get("suppression") or 0
    tau = r.get("theater_ratio") or 0
    en = bool(r.get("emerges_naturally"))
    rae = bool(r.get("requires_active_enforcement"))
    hcf = len(r.get("beneficiaries") or []) > 0
    hae = len(r.get("victims") or []) > 0

    # We use eps as Chi proxy (analytical context)
    chi = eps

    trace = {
        "id": r.get("id"),
        "claimed_type": r.get("claimed_type"),
        "eps": eps,
        "chi_proxy": chi,
        "sigma": sigma,
        "tau": tau,
        "gates": {"EN": en, "RAE": rae, "HCF": hcf, "HAE": hae},
        "gate_checks": [],
        "prolog_result": None,
        "first_ambiguity": None,
        "notes": [],
    }

    # Natural law without beneficiary check (used by snare/tangled_rope blocks)
    nlwb = en and not rae and not hcf

    # --- Mountain gate (drl_core.pl:288-294) ---
    mountain_checks = {
        "sigma_le_ceil": sigma <= MOUNTAIN_SUPPRESSION_CEILING,
        "eps_le_max": eps <= MOUNTAIN_EXTRACTIVENESS_MAX,
        "emerges_naturally": en,
        # immutability check omitted (requires context indexing, not in JSON)
    }
    mountain_pass = all([
        mountain_checks["sigma_le_ceil"],
        mountain_checks["eps_le_max"],
        mountain_checks["emerges_naturally"],
    ])
    trace["gate_checks"].append({
        "gate": "mountain",
        "line": "drl_core.pl:288-294",
        "checks": mountain_checks,
        "passes": mountain_pass,
        "note": "immutability check omitted (not in JSON)",
    })
    if mountain_pass:
        trace["prolog_result"] = "mountain"
        trace["notes"].append("Classified as mountain (before scaffold/piton gates)")
        return trace

    # --- Snare gate (drl_core.pl:296-304) ---
    snare_checks = {
        "not_nlwb": not nlwb,
        "chi_ge_floor": chi >= SNARE_CHI_FLOOR,
        "eps_ge_floor": eps >= SNARE_EPSILON_FLOOR,
        "sigma_ge_floor": sigma >= SNARE_SUPPRESSION_FLOOR,
        # snare_immutability_check omitted (requires cross-context)
    }
    snare_pass = all([
        snare_checks["not_nlwb"],
        snare_checks["chi_ge_floor"],
        snare_checks["eps_ge_floor"],
        snare_checks["sigma_ge_floor"],
    ])
    trace["gate_checks"].append({
        "gate": "snare",
        "line": "drl_core.pl:296-304",
        "checks": snare_checks,
        "passes": snare_pass,
        "note": "immutability check omitted",
    })
    if snare_pass:
        trace["prolog_result"] = "snare"
        trace["notes"].append(
            "Snare gate fires BEFORE scaffold/piton — higher priority. "
            "This constraint is intercepted by the snare gate."
        )
        return trace

    # --- Scaffold gate (drl_core.pl:306-312) ---
    scaffold_temporality = (not rae)  # has_sunset_clause is default-fail in JSON
    scaffold_temporality_note = "RAE=F → auto-pass" if not rae else "RAE=T → depends on has_sunset_clause (unobservable)"
    scaffold_checks = {
        "chi_le_ceil": chi <= SCAFFOLD_EXTRACTION_CEIL,
        "has_coordination_function": hcf,
        "scaffold_temporality_check": scaffold_temporality,
        "tr_not_gt_070": not (tau > PITON_THEATER_FLOOR),
    }
    # For RAE=T constraints, temporality check depends on has_sunset_clause
    # which defaults to fail. But if the constraint was classified as scaffold
    # by the pipeline, the Prolog must have had sunset clause = T.
    if rae:
        scaffold_checks["scaffold_temporality_check"] = "unknown_sunset"
    scaffold_pass_known = all([
        scaffold_checks["chi_le_ceil"],
        scaffold_checks["has_coordination_function"],
        scaffold_checks["scaffold_temporality_check"] is True,
        scaffold_checks["tr_not_gt_070"],
    ])
    scaffold_pass_possible = all([
        scaffold_checks["chi_le_ceil"],
        scaffold_checks["has_coordination_function"],
        scaffold_checks["tr_not_gt_070"],
    ]) and (scaffold_checks["scaffold_temporality_check"] is True
            or scaffold_checks["scaffold_temporality_check"] == "unknown_sunset")
    trace["gate_checks"].append({
        "gate": "scaffold",
        "line": "drl_core.pl:306-312",
        "checks": scaffold_checks,
        "passes": scaffold_pass_known,
        "passes_if_sunset": scaffold_pass_possible and not scaffold_pass_known,
        "temporality_note": scaffold_temporality_note,
    })
    if scaffold_pass_known:
        trace["prolog_result"] = "scaffold"
        return trace
    if scaffold_pass_possible and not scaffold_pass_known:
        trace["notes"].append(
            "Scaffold gate passes IF has_sunset_clause=T (unobservable from JSON)"
        )

    # --- Rope gate (drl_core.pl:314-323) ---
    # Chi ≤ 0 net-beneficiary flip: skip eps check
    if chi <= 0:
        rope_eps_ok = True
    else:
        rope_eps_ok = eps <= ROPE_EPSILON_CEILING
    rope_checks = {
        "chi_le_ceil": chi <= ROPE_CHI_CEILING,
        "eps_le_ceil_or_chi_neg": rope_eps_ok,
        "emerges_naturally_or_rope_immutability": en,
        # effective_immutability check also needed but not in JSON
    }
    rope_pass = all([
        rope_checks["chi_le_ceil"],
        rope_checks["eps_le_ceil_or_chi_neg"],
        # We can only check emerges_naturally; immutability requires context
    ]) and en
    trace["gate_checks"].append({
        "gate": "rope",
        "line": "drl_core.pl:314-323",
        "checks": rope_checks,
        "passes": rope_pass,
        "note": "immutability check partial (emerges_naturally only)",
    })
    if rope_pass:
        trace["prolog_result"] = "rope"
        return trace

    # --- Tangled rope gate (drl_core.pl:325-337) ---
    tangled_checks = {
        "not_nlwb": not nlwb,
        "chi_ge_floor": chi >= TANGLED_ROPE_CHI_FLOOR,
        "chi_le_ceil": chi <= TANGLED_ROPE_CHI_CEIL,
        "eps_ge_floor": eps >= TANGLED_ROPE_EPSILON_FLOOR,
        "sigma_ge_floor": sigma >= TANGLED_ROPE_SUPPRESSION_FLOOR,
        "requires_active_enforcement": rae,
        "has_coordination_function": hcf,
        "has_asymmetric_extraction": hae,
    }
    tangled_pass = all(tangled_checks.values())
    trace["gate_checks"].append({
        "gate": "tangled_rope",
        "line": "drl_core.pl:325-337",
        "checks": tangled_checks,
        "passes": tangled_pass,
    })
    if tangled_pass:
        trace["prolog_result"] = "tangled_rope"
        return trace

    # --- Piton gate (drl_core.pl:339-347) ---
    piton_checks = {
        "chi_le_ceil": chi <= PITON_EXTRACTION_CEILING,
        "eps_gt_floor": eps > PITON_EPSILON_FLOOR,
        "tr_ge_floor": tau >= PITON_THEATER_FLOOR,
    }
    piton_pass = all(piton_checks.values())
    trace["gate_checks"].append({
        "gate": "piton",
        "line": "drl_core.pl:339-347",
        "checks": piton_checks,
        "passes": piton_pass,
    })
    if piton_pass:
        trace["prolog_result"] = "piton"
        return trace

    # --- Naturalized (drl_core.pl:352-356) ---
    nat_pass = eps > ROPE_EPSILON_CEILING and chi < TANGLED_ROPE_CHI_FLOOR
    trace["gate_checks"].append({
        "gate": "naturalized",
        "line": "drl_core.pl:352-356",
        "passes": nat_pass,
    })
    if nat_pass:
        trace["prolog_result"] = "naturalized"
        return trace

    trace["prolog_result"] = "unknown"
    return trace


def _compute_ambiguity(trace):
    """Compute the first point of ambiguity — which threshold is closest to boundary."""
    eps = trace["eps"]
    chi = trace["chi_proxy"]
    sigma = trace["sigma"]
    tau = trace["tau"]
    claimed = trace["claimed_type"]

    # Distance to each relevant threshold (smaller = more ambiguous)
    distances = []
    if claimed == "scaffold":
        distances.append(("chi_vs_scaffold_ceil", abs(chi - SCAFFOLD_EXTRACTION_CEIL), SCAFFOLD_EXTRACTION_CEIL))
        distances.append(("tau_vs_070", abs(tau - PITON_THEATER_FLOOR), PITON_THEATER_FLOOR))
        # Snare interception thresholds
        distances.append(("chi_vs_snare_floor", abs(chi - SNARE_CHI_FLOOR), SNARE_CHI_FLOOR))
        distances.append(("eps_vs_snare_floor", abs(eps - SNARE_EPSILON_FLOOR), SNARE_EPSILON_FLOOR))
        distances.append(("sigma_vs_snare_floor", abs(sigma - SNARE_SUPPRESSION_FLOOR), SNARE_SUPPRESSION_FLOOR))
    elif claimed == "piton":
        distances.append(("chi_vs_piton_ceil", abs(chi - PITON_EXTRACTION_CEILING), PITON_EXTRACTION_CEILING))
        distances.append(("eps_vs_piton_floor", abs(eps - PITON_EPSILON_FLOOR), PITON_EPSILON_FLOOR))
        distances.append(("tau_vs_piton_floor", abs(tau - PITON_THEATER_FLOOR), PITON_THEATER_FLOOR))
        # Snare interception
        distances.append(("chi_vs_snare_floor", abs(chi - SNARE_CHI_FLOOR), SNARE_CHI_FLOOR))
        distances.append(("eps_vs_snare_floor", abs(eps - SNARE_EPSILON_FLOOR), SNARE_EPSILON_FLOOR))

    if distances:
        distances.sort(key=lambda x: x[1])
        trace["first_ambiguity"] = {
            "threshold": distances[0][0],
            "distance": round(distances[0][1], 4),
            "threshold_value": distances[0][2],
        }
        trace["ambiguity_ranking"] = [
            {"threshold": d[0], "distance": round(d[1], 4)}
            for d in distances[:3]
        ]


def step3_gate_trace(pop):
    """Trace the gate path for every scaffold and piton constraint."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    def _trace_group(group, label):
        traces = []
        for r in group:
            t = _trace_gate_path(r)
            _compute_ambiguity(t)
            traces.append(t)

        # Summary: what does the Prolog gate trace say?
        prolog_results = Counter(t["prolog_result"] for t in traces)
        claimed_vs_prolog = Counter()
        for t in traces:
            match = "match" if t["prolog_result"] == t["claimed_type"] else "mismatch"
            claimed_vs_prolog[match] += 1

        # For scaffolds: temporality path analysis
        temporality_summary = None
        if label == "scaffold":
            rae_f = [t for t in traces if not t["gates"]["RAE"]]
            rae_t = [t for t in traces if t["gates"]["RAE"]]
            temporality_summary = {
                "rae_f_count": len(rae_f),
                "rae_f_note": "Temporality auto-passes (sunset clause irrelevant)",
                "rae_t_count": len(rae_t),
                "rae_t_note": "Temporality depends on has_sunset_clause (unobservable)",
                "rae_t_ids": [t["id"] for t in rae_t],
            }

        # For pitons: theater ratio analysis
        theater_summary = None
        if label == "piton":
            tr_pass = [t for t in traces if (t.get("tau") or 0) >= PITON_THEATER_FLOOR]
            tr_fail = [t for t in traces if (t.get("tau") or 0) < PITON_THEATER_FLOOR]
            theater_summary = {
                "tr_ge_070_count": len(tr_pass),
                "tr_lt_070_count": len(tr_fail),
                "tr_ge_070_pct": pct(len(tr_pass), len(traces)),
                "tr_lt_070_pct": pct(len(tr_fail), len(traces)),
                "tr_lt_070_note": (
                    "These pitons fail the Prolog piton gate (TR < 0.70). "
                    "They are claimed as piton but classify_from_metrics "
                    "would not assign them as piton."
                ),
            }
            # What does the Prolog assign instead for TR < 0.70 pitons?
            tr_fail_prolog = Counter(t["prolog_result"] for t in tr_fail)
            theater_summary["tr_fail_prolog_results"] = dict(tr_fail_prolog.most_common())

        # Interception analysis: how many are intercepted by a higher-priority gate?
        intercepted = [t for t in traces if t["prolog_result"] != t["claimed_type"]]
        interception_by_gate = Counter(t["prolog_result"] for t in intercepted)

        return {
            "label": label,
            "count": len(traces),
            "prolog_result_dist": dict(prolog_results.most_common()),
            "claimed_vs_prolog": dict(claimed_vs_prolog),
            "interception_count": len(intercepted),
            "interception_pct": pct(len(intercepted), len(traces)),
            "interception_by_gate": dict(interception_by_gate.most_common()),
            "temporality_summary": temporality_summary,
            "theater_summary": theater_summary,
            "traces": traces,
        }

    return {
        "scaffold": _trace_group(scaffolds, "scaffold"),
        "piton": _trace_group(pitons, "piton"),
        "chi_proxy_note": (
            "Chi (power-scaled extractiveness) is not stored in enriched_pipeline.json. "
            "This trace uses base_extractiveness (epsilon) as a proxy. For the "
            "analytical context (default), d≈0 so f(d)≈1.0, making Chi ≈ epsilon. "
            "Minor deviations are possible from scope scaling σ(S)."
        ),
    }


# ---------------------------------------------------------------------------
# Step 4: Rival Type Analysis
# ---------------------------------------------------------------------------


def step4_rival_analysis(pop):
    """Analyze rival types for scaffolds and pitons."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    def _rival_group_analysis(group, label):
        by_rival = defaultdict(list)
        for r in group:
            rival = r.get("rival_type") or "None"
            by_rival[rival].append(r)

        rival_groups = {}
        for rival, grp in sorted(by_rival.items(), key=lambda kv: -len(kv[1])):
            gate_dist = Counter(gate_label(compute_gates(r)) for r in grp)
            metrics = _group_metrics(grp)
            band_dist = dict(Counter(r.get("confidence_band") or "None" for r in grp))

            rival_groups[rival] = {
                "count": len(grp),
                "pct": pct(len(grp), len(group)),
                "gate_profile_dist": dict(gate_dist.most_common()),
                "confidence_band_dist": band_dist,
                "metrics": metrics,
            }

        return {
            "label": label,
            "count": len(group),
            "rival_groups": rival_groups,
        }

    return {
        "scaffold": _rival_group_analysis(scaffolds, "scaffold"),
        "piton": _rival_group_analysis(pitons, "piton"),
    }


# ---------------------------------------------------------------------------
# Step 5: Sunset Clause Inference
# ---------------------------------------------------------------------------


def step5_sunset_inference(pop):
    """Infer has_sunset_clause status from RAE and pipeline classification."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    # Scaffolds
    scaff_rae_t = [r for r in scaffolds if bool(r.get("requires_active_enforcement"))]
    scaff_rae_f = [r for r in scaffolds if not bool(r.get("requires_active_enforcement"))]

    # Pitons
    piton_rae_t = [r for r in pitons if bool(r.get("requires_active_enforcement"))]
    piton_rae_f = [r for r in pitons if not bool(r.get("requires_active_enforcement"))]

    return {
        "scaffold": {
            "total": len(scaffolds),
            "rae_t": {
                "count": len(scaff_rae_t),
                "inference": (
                    "scaffold_temporality_check requires has_sunset_clause=T when "
                    "RAE=T (drl_core.pl:273-276). If the Prolog classified these as "
                    "scaffold, has_sunset_clause must be T."
                ),
                "ids": [r.get("id") for r in scaff_rae_t],
                "metrics": _group_metrics(scaff_rae_t),
            },
            "rae_f": {
                "count": len(scaff_rae_f),
                "inference": (
                    "scaffold_temporality_check auto-passes when RAE=F "
                    "(drl_core.pl:276). Sunset clause status is unknown."
                ),
                "ids": [r.get("id") for r in scaff_rae_f],
                "metrics": _group_metrics(scaff_rae_f),
            },
        },
        "piton": {
            "total": len(pitons),
            "rae_t": {
                "count": len(piton_rae_t),
                "note": (
                    "Pitons with RAE=T: requires active enforcement but claimed as "
                    "dead/theatrical code. These are enforced pitons — the constraint "
                    "persists through enforcement even though its function is theatrical."
                ),
            },
            "rae_f": {
                "count": len(piton_rae_f),
                "note": (
                    "Pitons with RAE=F: no enforcement required. This is the "
                    "'institutional inertia' pattern — the constraint persists through "
                    "neglect, not through active maintenance."
                ),
                "ids": [r.get("id") for r in piton_rae_f],
                "metrics": _group_metrics(piton_rae_f),
            },
        },
    }


# ---------------------------------------------------------------------------
# Step 6: Hypothesis Assessment
# ---------------------------------------------------------------------------


def step6_hypothesis(pop, fcr_all):
    """Evaluate the three hypothesis branches."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]

    fcr_total = len(fcr_all)
    fcr_rae_f = [r for r in fcr_all if not bool(r.get("requires_active_enforcement"))]
    fcr_rae_f_rate = len(fcr_rae_f) / max(fcr_total, 1)

    # H1: Piton borderline traces to RAE=F ambiguity
    piton_rae_f = [r for r in pitons if not bool(r.get("requires_active_enforcement"))]
    piton_rae_f_rate = len(piton_rae_f) / max(len(pitons), 1)

    piton_rae_f_rivals = Counter(r.get("rival_type") or "None" for r in piton_rae_f)
    piton_rae_f_borderline = sum(
        1 for r in piton_rae_f if r.get("confidence_band") == "borderline"
    )

    h1 = {
        "piton_total": len(pitons),
        "piton_rae_f_count": len(piton_rae_f),
        "piton_rae_f_rate": round(piton_rae_f_rate, 4),
        "fcr_rae_f_rate": round(fcr_rae_f_rate, 4),
        "enrichment_ratio": round(piton_rae_f_rate / max(fcr_rae_f_rate, 0.001), 2),
        "piton_rae_f_rival_dist": dict(piton_rae_f_rivals.most_common()),
        "piton_rae_f_borderline_count": piton_rae_f_borderline,
        "piton_rae_f_borderline_pct": pct(piton_rae_f_borderline, len(piton_rae_f)),
        "verdict": None,
    }
    if piton_rae_f_rate > fcr_rae_f_rate * 2:
        h1["verdict"] = "SUPPORTED"
        h1["explanation"] = (
            f"Piton RAE=F rate ({h1['piton_rae_f_rate']:.1%}) is "
            f"{h1['enrichment_ratio']}x the false_ci_rope base rate "
            f"({h1['fcr_rae_f_rate']:.1%}). RAE=F is significantly "
            f"overrepresented in pitons."
        )
    else:
        h1["verdict"] = "NOT SUPPORTED"
        h1["explanation"] = (
            f"Piton RAE=F rate ({h1['piton_rae_f_rate']:.1%}) is not "
            f"significantly elevated above the false_ci_rope base rate "
            f"({h1['fcr_rae_f_rate']:.1%})."
        )

    # H2: Scaffold borderline traces to sunset clause unobservability
    scaff_rae_t = [r for r in scaffolds if bool(r.get("requires_active_enforcement"))]
    scaff_rae_f = [r for r in scaffolds if not bool(r.get("requires_active_enforcement"))]
    scaff_rae_f_borderline = sum(
        1 for r in scaff_rae_f if r.get("confidence_band") == "borderline"
    )
    scaff_total_borderline = sum(
        1 for r in scaffolds if r.get("confidence_band") == "borderline"
    )

    h2 = {
        "scaffold_total": len(scaffolds),
        "scaffold_rae_t_count": len(scaff_rae_t),
        "scaffold_rae_t_note": (
            "These scaffolds depend on has_sunset_clause for temporality check. "
            "Sunset clause status is unobservable from JSON."
        ),
        "scaffold_rae_f_count": len(scaff_rae_f),
        "scaffold_rae_f_borderline": scaff_rae_f_borderline,
        "scaffold_rae_f_borderline_pct": pct(scaff_rae_f_borderline, max(len(scaff_rae_f), 1)),
        "scaffold_total_borderline": scaff_total_borderline,
        "scaffold_total_borderline_pct": pct(scaff_total_borderline, len(scaffolds)),
        "verdict": None,
    }
    if scaff_rae_f_borderline == len(scaff_rae_f) and len(scaff_rae_f) > 0:
        h2["verdict"] = "PARTIALLY SUPPORTED"
        h2["explanation"] = (
            f"RAE=F scaffolds are also 100% borderline, meaning sunset clause "
            f"unobservability is not the SOLE source of ambiguity. Continuous "
            f"metric proximity to the tangled_rope/rope boundary also contributes."
        )
    elif len(scaff_rae_t) > 0 and len(scaff_rae_f) > 0:
        h2["verdict"] = "PARTIALLY SUPPORTED"
        h2["explanation"] = (
            f"{len(scaff_rae_t)} scaffolds depend on sunset clause (RAE=T), "
            f"but {scaff_rae_f_borderline}/{len(scaff_rae_f)} RAE=F scaffolds "
            f"are also borderline. Both gate ambiguity and metric proximity "
            f"contribute to the universal borderline status."
        )
    elif len(scaff_rae_t) == 0:
        h2["verdict"] = "NOT SUPPORTED"
        h2["explanation"] = (
            "No scaffolds have RAE=T, so sunset clause is irrelevant for "
            "all scaffolds. Borderline status must trace to other factors."
        )
    else:
        h2["verdict"] = "INCONCLUSIVE"
        h2["explanation"] = "Insufficient data to assess."

    # H3: RAE=F subgroup is the piton-scaffold population
    fcr_rae_f_piton_scaffold = sum(
        1 for r in fcr_rae_f
        if r.get("claimed_type") in TARGET_TYPES
    )
    fcr_piton_scaffold_total = sum(
        1 for r in fcr_all
        if r.get("claimed_type") in TARGET_TYPES
    )
    fcr_piton_scaffold_rate = fcr_piton_scaffold_total / max(fcr_total, 1)

    h3 = {
        "fcr_rae_f_total": len(fcr_rae_f),
        "fcr_rae_f_piton_scaffold": fcr_rae_f_piton_scaffold,
        "fcr_rae_f_piton_scaffold_pct": pct(fcr_rae_f_piton_scaffold, len(fcr_rae_f)),
        "fcr_piton_scaffold_total": fcr_piton_scaffold_total,
        "fcr_piton_scaffold_rate": round(fcr_piton_scaffold_rate, 4),
        "fcr_piton_scaffold_rate_pct": pct(fcr_piton_scaffold_total, fcr_total),
        "fcr_rae_f_claimed_dist": dict(
            Counter(r.get("claimed_type") or "None" for r in fcr_rae_f).most_common()
        ),
        "verdict": None,
    }
    rae_f_ps_rate = fcr_rae_f_piton_scaffold / max(len(fcr_rae_f), 1)
    if rae_f_ps_rate > fcr_piton_scaffold_rate * 2:
        h3["verdict"] = "SUPPORTED"
        h3["explanation"] = (
            f"{fcr_rae_f_piton_scaffold}/{len(fcr_rae_f)} "
            f"({pct(fcr_rae_f_piton_scaffold, len(fcr_rae_f))}%) of RAE=F "
            f"false_ci_rope constraints are piton/scaffold, vs "
            f"{pct(fcr_piton_scaffold_total, fcr_total)}% in the full "
            f"false_ci_rope population. Pitons and scaffolds are significantly "
            f"concentrated in the RAE=F subgroup."
        )
    else:
        h3["verdict"] = "NOT SUPPORTED"
        h3["explanation"] = (
            f"RAE=F subgroup piton/scaffold rate "
            f"({pct(fcr_rae_f_piton_scaffold, len(fcr_rae_f))}%) is not "
            f"substantially higher than the population rate "
            f"({pct(fcr_piton_scaffold_total, fcr_total)}%)."
        )

    return {"H1": h1, "H2": h2, "H3": h3}


# ---------------------------------------------------------------------------
# Step 7: Recommendations
# ---------------------------------------------------------------------------


def step7_recommendations(gate_trace, hypothesis, sunset):
    """Generate data-driven recommendations."""
    recs = []

    # R1: Expose has_sunset_clause
    scaff_rae_t = sunset["scaffold"]["rae_t"]["count"]
    if scaff_rae_t > 0:
        recs.append({
            "id": "R1",
            "title": "Expose has_sunset_clause in enriched_pipeline.json",
            "rationale": (
                f"{scaff_rae_t} scaffolds have RAE=T, meaning their scaffold "
                f"classification depends entirely on has_sunset_clause — a gate "
                f"that is invisible to the MaxEnt classifier. Exposing this gate "
                f"would give the classifier a direct feature for distinguishing "
                f"scaffold from tangled_rope in the RAE=T subpopulation."
            ),
            "impact": "high" if scaff_rae_t > 5 else "moderate",
        })
    else:
        recs.append({
            "id": "R1",
            "title": "Expose has_sunset_clause in enriched_pipeline.json",
            "rationale": (
                "All scaffolds have RAE=F (temporality auto-passes), so sunset "
                "clause exposure would not directly help scaffold classification. "
                "However, it may help distinguish pitons (which should lack sunset "
                "clauses) from scaffolds that happen to share similar metrics."
            ),
            "impact": "low",
        })

    # R2: Theater ratio threshold feature + Chi ceiling diagnosis
    piton_trace = gate_trace.get("piton", {})
    theater = piton_trace.get("theater_summary", {})
    tr_pass = theater.get("tr_ge_070_count", 0)
    tr_total = piton_trace.get("count", 0)
    piton_intercept = piton_trace.get("interception_count", 0)
    piton_intercept_by = piton_trace.get("interception_by_gate", {})
    recs.append({
        "id": "R2",
        "title": "Address piton gate interception by higher-priority gates",
        "rationale": (
            f"All {tr_pass}/{tr_total} pitons pass the TR ≥ 0.70 gate, so "
            f"theater ratio is not the bottleneck. The real issue is gate "
            f"priority: {piton_intercept}/{tr_total} pitons "
            f"({pct(piton_intercept, tr_total)}%) are intercepted by "
            f"higher-priority gates before the piton gate is reached. "
            f"Interceptions: {dict(piton_intercept_by)}. Most pitons have "
            f"epsilon > 0.25 (the piton Chi ceiling), so the snare and "
            f"tangled_rope gates fire first. The MaxEnt classifier should "
            f"encode the piton gate's Chi ≤ 0.25 ceiling and TR ≥ 0.70 "
            f"floor as joint boolean features. Without these, the classifier "
            f"has no way to distinguish piton from snare/tangled_rope."
        ),
        "impact": "high",
    })

    # R3: Split false_ci_rope by RAE gate
    h3_verdict = hypothesis["H3"]["verdict"]
    recs.append({
        "id": "R3",
        "title": "Consider splitting false_ci_rope subcategories by RAE gate",
        "rationale": (
            f"H3 verdict: {h3_verdict}. "
            + hypothesis["H3"]["explanation"]
        ),
        "impact": "moderate" if h3_verdict == "SUPPORTED" else "low",
    })

    # R4: Reclassification candidates
    piton_interceptions = gate_trace.get("piton", {}).get("interception_by_gate", {})
    scaff_interceptions = gate_trace.get("scaffold", {}).get("interception_by_gate", {})
    total_intercept = (
        gate_trace.get("piton", {}).get("interception_count", 0) +
        gate_trace.get("scaffold", {}).get("interception_count", 0)
    )
    recs.append({
        "id": "R4",
        "title": "Review gate-trace mismatches as reclassification candidates",
        "rationale": (
            f"{total_intercept} constraints are claimed as scaffold/piton but "
            f"would be classified differently by the Prolog gate priority chain. "
            f"Piton interceptions: {dict(piton_interceptions)}. "
            f"Scaffold interceptions: {dict(scaff_interceptions)}. "
            f"These are candidates for reclassification or for documenting "
            f"why the LLM's claimed type diverges from the gate logic."
        ),
        "impact": "high" if total_intercept > 10 else "moderate",
    })

    return recs


# ---------------------------------------------------------------------------
# Report Generation
# ---------------------------------------------------------------------------


def generate_report(s1, s2, s3, s4, s5, s6, s7, timestamp):
    """Build the complete markdown audit report."""
    L = []

    def section(title, level=2):
        L.append("")
        L.append(f"{'#' * level} {title}")
        L.append("")

    def para(text):
        L.append(text)
        L.append("")

    # ---- Title ----
    L.append("# Scaffold & Piton Gate Trace Audit")
    L.append("")
    L.append(f"*Generated {timestamp} by `python/scaffold_piton_gate_audit.py`*")

    # ---- Executive Summary ----
    section("Executive Summary")
    scaff = s1["scaffold"]
    pit = s1["piton"]
    para(
        f"This audit traces **{scaff['count']} scaffolds** and "
        f"**{pit['count']} pitons** through the complete Prolog gate "
        f"priority chain (`classify_from_metrics/6` in `drl_core.pl`) to "
        f"diagnose why MaxEnt confidence is universally borderline for "
        f"these two types."
    )

    # Key findings bullets
    L.append("Key findings:")
    L.append("")

    # Piton TR analysis
    theater = s3.get("piton", {}).get("theater_summary", {})
    if theater:
        L.append(
            f"- **Piton theater ratio**: {theater.get('tr_ge_070_count', 0)}/"
            f"{pit['count']} ({theater.get('tr_ge_070_pct', 0)}%) pass the "
            f"Prolog TR ≥ 0.70 gate; {theater.get('tr_lt_070_count', 0)} fail"
        )

    # Scaffold temporality
    temp = s3.get("scaffold", {}).get("temporality_summary", {})
    if temp:
        L.append(
            f"- **Scaffold temporality**: {temp.get('rae_f_count', 0)} with "
            f"RAE=F (auto-pass), {temp.get('rae_t_count', 0)} with RAE=T "
            f"(depends on has_sunset_clause)"
        )

    # RAE=F enrichment
    h1 = s6.get("H1", {})
    L.append(
        f"- **RAE=F enrichment in pitons**: {h1.get('piton_rae_f_rate', 0):.1%} "
        f"vs false_ci_rope base rate {h1.get('fcr_rae_f_rate', 0):.1%} "
        f"({h1.get('enrichment_ratio', 0)}x)"
    )

    # Hypothesis verdicts
    for hk in ["H1", "H2", "H3"]:
        hv = s6.get(hk, {})
        L.append(f"- **{hk}**: {hv.get('verdict', 'N/A')}")
    L.append("")

    # ---- Section 1: Population Extraction ----
    section("1. Population Extraction")

    for key, label in [("scaffold", "Scaffold"), ("piton", "Piton")]:
        data = s1[key]
        section(f"1.{1 if key == 'scaffold' else 2} {label} Population (N={data['count']})", 3)

        # Signature distribution
        L.append(f"**Signature distribution:**")
        L.append("")
        rows = [(sig, n) for sig, n in data["signature_dist"].items()]
        L.append(md_table(["Signature", "Count"], rows, ["l", "r"]))
        L.append("")

        # MaxEnt top type
        L.append(f"**MaxEnt top type (post-override):**")
        L.append("")
        rows = [
            (t, n, f"{pct(n, data['count'])}%")
            for t, n in data["maxent_top_type_dist"].items()
        ]
        L.append(md_table(["Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Raw MaxEnt top type
        L.append(f"**Raw MaxEnt top type (pre-override):**")
        L.append("")
        rows = [
            (t, n, f"{pct(n, data['count'])}%")
            for t, n in data["raw_maxent_top_type_dist"].items()
        ]
        L.append(md_table(["Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Confidence band
        L.append(f"**Confidence band:**")
        L.append("")
        rows = [
            (b, n, f"{pct(n, data['count'])}%")
            for b, n in data["confidence_band_dist"].items()
        ]
        L.append(md_table(["Band", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Rival types
        L.append(f"**Rival type distribution:**")
        L.append("")
        rows = [
            (t, n, f"{pct(n, data['count'])}%")
            for t, n in data["rival_type_dist"].items()
        ]
        L.append(md_table(["Rival Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Boundary
        L.append(f"**Boundary classification:**")
        L.append("")
        rows = [
            (b, n, f"{pct(n, data['count'])}%")
            for b, n in data["boundary_dist"].items()
        ]
        L.append(md_table(["Boundary", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Continuous metrics
        L.append(f"**Continuous metrics:**")
        L.append("")
        metric_labels = [
            ("base_extractiveness", "Epsilon (base_extractiveness)"),
            ("suppression", "Sigma (suppression)"),
            ("theater_ratio", "Tau (theater_ratio)"),
            ("purity_score", "Purity Score"),
            ("confidence", "Confidence (P(claimed))"),
            ("confidence_margin", "Confidence Margin"),
        ]
        rows = []
        for mkey, mlabel in metric_labels:
            st = data.get(f"{mkey}_stats")
            if st:
                rows.append((
                    mlabel, st["n"], fmt(st["mean"]), fmt(st["median"]),
                    fmt(st["std"]), fmt(st["min"]), fmt(st["max"]),
                ))
            else:
                rows.append((mlabel, 0, "---", "---", "---", "---", "---"))
        L.append(md_table(
            ["Metric", "N", "Mean", "Median", "Std", "Min", "Max"],
            rows, ["l", "r", "r", "r", "r", "r", "r"],
        ))
        L.append("")

    # ---- Section 2: Gate Profile Census ----
    section("2. Gate Profile Census")

    para(s3.get("chi_proxy_note", ""))

    for key, label in [("scaffold", "Scaffold"), ("piton", "Piton")]:
        data = s2[key]
        xref = s2[f"{key}_xref"]
        section(f"2.{1 if key == 'scaffold' else 2} {label} Gate Profiles", 3)

        # Marginal rates
        L.append(f"**Gate marginal rates:**")
        L.append("")
        rows = []
        for name in GATE_NAMES:
            m = data["marginals"][name]
            rows.append((name, m["true"], m["false"], fmt(m["p_true"])))
        L.append(md_table(
            ["Gate", "True", "False", "P(True)"], rows, ["l", "r", "r", "r"],
        ))
        L.append("")

        # Profile distribution
        L.append(f"**Profile distribution:**")
        L.append("")
        rows = []
        for p in data["profiles"]:
            m = p.get("metrics", {})
            eps = m.get("base_extractiveness")
            sig = m.get("suppression")
            tau = m.get("theater_ratio")
            conf = m.get("confidence")
            bd = p.get("confidence_band_dist", {})
            borderline_n = bd.get("borderline", 0)
            rows.append((
                p["label"], p["count"], f"{p['pct']}%",
                fmt(conf["mean"] if conf else None),
                f"{pct(borderline_n, p['count'])}%",
                fmt(eps["mean"] if eps else None),
                fmt(sig["mean"] if sig else None),
                fmt(tau["mean"] if tau else None),
            ))
        L.append(md_table(
            ["Profile", "N", "%", "Mean Conf", "% Border",
             "Mean Eps", "Mean Sig", "Mean Tau"],
            rows, ["l", "r", "r", "r", "r", "r", "r", "r"],
        ))
        L.append("")

        # Cross-reference with false_ci_rope
        L.append(f"**Cross-reference with false_ci_rope profiles:**")
        L.append("")
        L.append(f"- Dominant (F,T,T,T): {xref['dominant_FTTT']}")
        L.append(f"- Atypical (F,F,T,T): {xref['atypical_FFTT']}")
        L.append(f"- Other profiles: {xref['other']}")
        L.append("")

        # RAE=F enrichment
        L.append(
            f"**RAE=F rate**: {data['rae_f_count']}/{data['count']} "
            f"({data['rae_f_rate']:.1%}) vs false_ci_rope base rate "
            f"{s2['fcr_base_rates']['rae_f_rate']:.1%} "
            f"(**{data['rae_f_enrichment_vs_fcr']}x** enrichment)"
        )
        L.append("")

    # ---- Section 3: Gate Path Trace ----
    section("3. Gate Path Trace")

    para(
        "Each constraint is traced through the `classify_from_metrics/6` "
        "priority chain (drl_core.pl:288-358). The trace uses `base_extractiveness` "
        "as a proxy for Chi (power-scaled extractiveness) — see note above."
    )

    for key, label in [("scaffold", "Scaffold"), ("piton", "Piton")]:
        data = s3[key]
        section(f"3.{1 if key == 'scaffold' else 2} {label} Gate Trace (N={data['count']})", 3)

        # Prolog result distribution
        L.append(f"**Prolog gate trace result:**")
        L.append("")
        rows = [
            (t, n, f"{pct(n, data['count'])}%")
            for t, n in data["prolog_result_dist"].items()
        ]
        L.append(md_table(["Gate Result", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

        # Claimed vs Prolog
        match_n = data["claimed_vs_prolog"].get("match", 0)
        mismatch_n = data["claimed_vs_prolog"].get("mismatch", 0)
        para(
            f"**Claimed vs Prolog**: {match_n} match, {mismatch_n} mismatch "
            f"({data['interception_pct']}% intercepted by higher-priority gate)"
        )

        if data["interception_by_gate"]:
            L.append(f"**Interception by gate:**")
            L.append("")
            rows = [
                (g, n, f"{pct(n, data['count'])}%")
                for g, n in data["interception_by_gate"].items()
            ]
            L.append(md_table(["Intercepting Gate", "Count", "%"], rows, ["l", "r", "r"]))
            L.append("")

        # Type-specific analysis
        if key == "scaffold" and data.get("temporality_summary"):
            temp = data["temporality_summary"]
            section("3.1.1 Scaffold Temporality Path", 4)
            para(
                f"The scaffold gate requires `scaffold_temporality_check` "
                f"(drl_core.pl:273-276), which passes if `has_sunset_clause=T` "
                f"OR `requires_active_enforcement=F`."
            )
            L.append(f"- **RAE=F** (temporality auto-passes): {temp['rae_f_count']}")
            L.append(f"- **RAE=T** (depends on has_sunset_clause): {temp['rae_t_count']}")
            L.append("")
            if temp["rae_t_ids"]:
                L.append(f"RAE=T scaffold IDs (sunset-clause-dependent):")
                L.append("")
                for cid in temp["rae_t_ids"]:
                    L.append(f"- `{cid}`")
                L.append("")

        if key == "piton" and data.get("theater_summary"):
            theater = data["theater_summary"]
            section("3.2.1 Piton Theater Ratio Gate", 4)
            para(
                f"The piton gate requires TR ≥ 0.70 (drl_core.pl:346). "
                f"This is the threshold that distinguishes theatrical/dead code "
                f"(piton) from legitimate low-extraction coordination (rope/scaffold)."
            )
            L.append(
                f"- **TR ≥ 0.70** (passes piton gate): "
                f"{theater['tr_ge_070_count']} ({theater['tr_ge_070_pct']}%)"
            )
            L.append(
                f"- **TR < 0.70** (fails piton gate): "
                f"{theater['tr_lt_070_count']} ({theater['tr_lt_070_pct']}%)"
            )
            L.append("")
            if theater.get("tr_fail_prolog_results"):
                para(
                    f"For the {theater['tr_lt_070_count']} pitons that fail "
                    f"the TR gate, the Prolog priority chain assigns:"
                )
                rows = [
                    (t, n) for t, n in theater["tr_fail_prolog_results"].items()
                ]
                L.append(md_table(["Prolog Result", "Count"], rows, ["l", "r"]))
                L.append("")
                para(theater["tr_lt_070_note"])

    # ---- Section 4: Rival Type Analysis ----
    section("4. Rival Type Analysis")

    for key, label in [("scaffold", "Scaffold"), ("piton", "Piton")]:
        data = s4[key]
        section(f"4.{1 if key == 'scaffold' else 2} {label} Rival Analysis", 3)

        for rival, rdata in data["rival_groups"].items():
            L.append(f"**{label}→{rival}** (N={rdata['count']}, {rdata['pct']}%):")
            L.append("")

            # Gate profile for this rival group
            if rdata["gate_profile_dist"]:
                L.append(f"Gate profiles: " + ", ".join(
                    f"{k}: {v}" for k, v in rdata["gate_profile_dist"].items()
                ))
                L.append("")

            # Metrics comparison
            m = rdata.get("metrics", {})
            eps = m.get("base_extractiveness")
            sig = m.get("suppression")
            tau = m.get("theater_ratio")
            conf = m.get("confidence")
            L.append(
                f"Metrics — "
                f"eps: {fmt(eps['mean'] if eps else None)}, "
                f"sigma: {fmt(sig['mean'] if sig else None)}, "
                f"tau: {fmt(tau['mean'] if tau else None)}, "
                f"conf: {fmt(conf['mean'] if conf else None)}"
            )
            L.append("")

    # ---- Section 5: Sunset Clause Inference ----
    section("5. Sunset Clause Inference")

    para(
        "`has_sunset_clause` is evaluated only within the Prolog inference engine "
        "(narrative_ontology.pl:326, default fail). It is not exported to "
        "enriched_pipeline.json. However, we can make structural inferences:"
    )

    # Scaffold inference
    section("5.1 Scaffold Sunset Clause Inference", 3)
    scaff_sunset = s5["scaffold"]
    rae_t = scaff_sunset["rae_t"]
    rae_f = scaff_sunset["rae_f"]

    para(
        f"**RAE=T scaffolds** (N={rae_t['count']}): {rae_t['inference']}"
    )
    if rae_t["ids"]:
        L.append("IDs:")
        L.append("")
        for cid in rae_t["ids"]:
            L.append(f"- `{cid}`")
        L.append("")

    para(
        f"**RAE=F scaffolds** (N={rae_f['count']}): {rae_f['inference']}"
    )

    # Piton inference
    section("5.2 Piton RAE Analysis", 3)
    piton_sunset = s5["piton"]
    para(
        f"**RAE=T pitons** (N={piton_sunset['rae_t']['count']}): "
        f"{piton_sunset['rae_t']['note']}"
    )
    para(
        f"**RAE=F pitons** (N={piton_sunset['rae_f']['count']}): "
        f"{piton_sunset['rae_f']['note']}"
    )

    # ---- Section 6: Hypothesis Assessment ----
    section("6. Hypothesis Assessment")

    for hk, title in [
        ("H1", "Piton borderline confidence traces to RAE=F ambiguity"),
        ("H2", "Scaffold borderline confidence traces to sunset clause unobservability"),
        ("H3", "The RAE=F subgroup is the piton-scaffold population"),
    ]:
        hv = s6[hk]
        section(f"6.{hk[1]} {hk}: {title}", 3)

        L.append(f"**Verdict: {hv['verdict']}**")
        L.append("")
        para(hv["explanation"])

        # H1 details
        if hk == "H1":
            L.append(f"- Piton RAE=F count: {hv['piton_rae_f_count']}/{hv['piton_total']}")
            L.append(f"- Piton RAE=F rate: {hv['piton_rae_f_rate']:.1%}")
            L.append(f"- FCR base rate: {hv['fcr_rae_f_rate']:.1%}")
            L.append(f"- Enrichment ratio: {hv['enrichment_ratio']}x")
            if hv.get("piton_rae_f_rival_dist"):
                L.append(f"- RAE=F piton rival types: " + ", ".join(
                    f"{k}: {v}" for k, v in hv["piton_rae_f_rival_dist"].items()
                ))
            L.append("")

        # H2 details
        if hk == "H2":
            L.append(f"- Scaffolds with RAE=T: {hv['scaffold_rae_t_count']}")
            L.append(f"- Scaffolds with RAE=F: {hv['scaffold_rae_f_count']}")
            L.append(
                f"- RAE=F scaffolds borderline: {hv['scaffold_rae_f_borderline']}/"
                f"{hv['scaffold_rae_f_count']} ({hv['scaffold_rae_f_borderline_pct']}%)"
            )
            L.append(
                f"- Total scaffolds borderline: {hv['scaffold_total_borderline']}/"
                f"{hv['scaffold_total']} ({hv['scaffold_total_borderline_pct']}%)"
            )
            L.append("")

        # H3 details
        if hk == "H3":
            L.append(f"- FCR RAE=F total: {hv['fcr_rae_f_total']}")
            L.append(
                f"- FCR RAE=F piton/scaffold: {hv['fcr_rae_f_piton_scaffold']} "
                f"({hv['fcr_rae_f_piton_scaffold_pct']}%)"
            )
            L.append(
                f"- FCR piton/scaffold rate (full population): "
                f"{hv['fcr_piton_scaffold_rate_pct']}%"
            )
            if hv.get("fcr_rae_f_claimed_dist"):
                L.append("")
                L.append("FCR RAE=F claimed type distribution:")
                L.append("")
                rows = [(t, n) for t, n in hv["fcr_rae_f_claimed_dist"].items()]
                L.append(md_table(["Claimed Type", "Count"], rows, ["l", "r"]))
            L.append("")

    # ---- Section 7: Recommendations ----
    section("7. Recommendations")

    for rec in s7:
        section(f"7.{rec['id'][1:]}. {rec['title']}", 3)
        para(f"**Impact**: {rec['impact']}")
        para(rec["rationale"])

    return "\n".join(L)


# ---------------------------------------------------------------------------
# JSON Output
# ---------------------------------------------------------------------------


def generate_json(s1, s2, s3, s4, s5, s6, s7, timestamp):
    """Build the JSON output structure."""
    # Strip full trace details for JSON (keep summaries, sample traces)
    s3_json = {}
    for key in ["scaffold", "piton"]:
        data = s3.get(key, {})
        traces = data.get("traces", [])
        # Keep only summary-level fields and sample traces (first 10)
        s3_json[key] = {
            k: v for k, v in data.items() if k != "traces"
        }
        s3_json[key]["sample_traces"] = traces[:10]
        s3_json[key]["total_traces"] = len(traces)
    s3_json["chi_proxy_note"] = s3.get("chi_proxy_note", "")

    return {
        "metadata": {
            "generated": timestamp,
            "script": "python/scaffold_piton_gate_audit.py",
            "source": "outputs/enriched_pipeline.json",
            "total_corpus": s1["total_corpus"],
            "scaffold_count": s1["scaffold"]["count"],
            "piton_count": s1["piton"]["count"],
        },
        "step1_population": s1,
        "step2_gate_profiles": s2,
        "step3_gate_trace": s3_json,
        "step4_rival_analysis": s4,
        "step5_sunset_inference": s5,
        "step6_hypothesis": s6,
        "step7_recommendations": s7,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    print("[SP-AUDIT] Loading enriched_pipeline.json...", file=sys.stderr)
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                     schema=validate_enriched_pipeline)
    per_constraint = data.get("per_constraint", [])
    total = len(per_constraint)

    # Target population: scaffold and piton by claimed_type
    pop = [r for r in per_constraint if r.get("claimed_type") in TARGET_TYPES]
    print(f"[SP-AUDIT] Found {len(pop)} scaffold/piton constraints "
          f"(of {total} total)", file=sys.stderr)

    if not pop:
        print("[SP-AUDIT] ERROR: No scaffold/piton constraints found",
              file=sys.stderr)
        sys.exit(1)

    # false_ci_rope population for cross-reference
    fcr_all = [r for r in per_constraint if r.get("signature") == "false_ci_rope"]
    print(f"[SP-AUDIT] false_ci_rope reference population: {len(fcr_all)}",
          file=sys.stderr)

    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    print("[SP-AUDIT] Step 1: Population extraction...", file=sys.stderr)
    s1 = step1_population(pop, total)

    print("[SP-AUDIT] Step 2: Gate profile census...", file=sys.stderr)
    s2 = step2_gate_profiles(pop, fcr_all)

    print("[SP-AUDIT] Step 3: Gate path trace...", file=sys.stderr)
    s3 = step3_gate_trace(pop)

    print("[SP-AUDIT] Step 4: Rival type analysis...", file=sys.stderr)
    s4 = step4_rival_analysis(pop)

    print("[SP-AUDIT] Step 5: Sunset clause inference...", file=sys.stderr)
    s5 = step5_sunset_inference(pop)

    print("[SP-AUDIT] Step 6: Hypothesis assessment...", file=sys.stderr)
    s6 = step6_hypothesis(pop, fcr_all)

    print("[SP-AUDIT] Step 7: Recommendations...", file=sys.stderr)
    s7 = step7_recommendations(s3, s6, s5)

    # Generate outputs
    print("[SP-AUDIT] Generating report...", file=sys.stderr)
    report_md = generate_report(s1, s2, s3, s4, s5, s6, s7, timestamp)

    DOCS_DIR.mkdir(parents=True, exist_ok=True)
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report_md)
    print(f"[SP-AUDIT] Wrote {REPORT_PATH}", file=sys.stderr)

    json_data = generate_json(s1, s2, s3, s4, s5, s6, s7, timestamp)
    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(json_data, f, indent=2)
    print(f"[SP-AUDIT] Wrote {DATA_PATH}", file=sys.stderr)

    print("[SP-AUDIT] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
