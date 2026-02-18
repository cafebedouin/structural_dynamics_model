#!/usr/bin/env python3
"""Boolean Feature Independence Analysis (v1.0)

Tests whether boolean features (emerges_naturally, requires_active_enforcement,
has_coordination_function, has_asymmetric_extraction, natural_law_without_beneficiary)
vary independently of the current type assignments, or are already fully captured
by the type space.

If a boolean is nearly constant within each type, it's redundant — already encoded
in the type. If it varies significantly within a type and correlates with different
orbit structures or confidence levels, it represents an independent dimension the
current type space doesn't capture.

Reads from enriched pipeline_output.json + corpus_data.json. No recomputation.

Outputs:
  outputs/boolean_independence_data.json   - aggregate statistical data
  outputs/boolean_independence_report.md   - analysis report

Usage:
  python3 python/boolean_independence.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from pathlib import Path

# ---------------------------------------------------------------------------
# Optional dependencies
# ---------------------------------------------------------------------------

HAS_SCIPY = False
try:
    import scipy.stats
    HAS_SCIPY = True
except ImportError:
    print("Warning: scipy not available. Skipping chi-squared and MI tests.",
          file=sys.stderr)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
OUTPUT_DIR = ROOT_DIR / "outputs"

PIPELINE_JSON = OUTPUT_DIR / "pipeline_output.json"
CORPUS_JSON = OUTPUT_DIR / "corpus_data.json"

REPORT_PATH = OUTPUT_DIR / "boolean_independence_report.md"
DATA_PATH = OUTPUT_DIR / "boolean_independence_data.json"

MAXENT_TYPES = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]


class _SafeEncoder(json.JSONEncoder):
    """Handle numpy types that aren't JSON-serializable."""
    def default(self, obj):
        if hasattr(obj, 'item'):
            return obj.item()
        return super().default(obj)


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_json(path, label):
    """Load a JSON file, returning {} on failure."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Warning: Could not load {label} ({path}): {e}", file=sys.stderr)
        return {}


def load_constraints():
    """Load and merge pipeline + corpus data into unified per-constraint dicts."""
    pipeline_raw = load_json(PIPELINE_JSON, "pipeline_output")
    corpus_raw = load_json(CORPUS_JSON, "corpus_data")

    per_constraint = pipeline_raw.get("per_constraint", [])
    corpus_constraints = corpus_raw.get("constraints", {})

    constraints = {}
    for entry in per_constraint:
        cid = entry.get("id")
        if not cid:
            continue
        claimed = entry.get("claimed_type")
        if not claimed or claimed not in MAXENT_TYPES:
            continue

        corpus_entry = corpus_constraints.get(cid, {})
        analysis = corpus_entry.get("analysis", {})

        constraints[cid] = {
            "id": cid,
            "claimed_type": claimed,
            # Continuous metrics
            "extractiveness": entry.get("base_extractiveness"),
            "suppression": entry.get("suppression"),
            "theater_ratio": entry.get("theater_ratio"),
            # Direct boolean features
            "emerges_naturally": entry.get("emerges_naturally", False),
            "requires_active_enforcement": entry.get("requires_active_enforcement", False),
            # List fields for derived booleans
            "beneficiaries": entry.get("beneficiaries", []),
            "victims": entry.get("victims", []),
            # Enriched fields
            "confidence": entry.get("confidence"),
            "confidence_band": entry.get("confidence_band"),
            "confidence_margin": entry.get("confidence_margin"),
            "confidence_entropy": entry.get("confidence_entropy"),
            "coalition_type": entry.get("coalition_type"),
            "rival_type": entry.get("rival_type"),
            "boundary": entry.get("boundary"),
            "signature": entry.get("signature"),
            "maxent_probs": entry.get("maxent_probs", {}),
            # Corpus analysis fields
            "orbit_signature": analysis.get("orbit_signature", []),
            "orbit_contexts": analysis.get("orbit_contexts", {}),
            "variance_ratio": analysis.get("variance_ratio"),
            "types_produced": analysis.get("types_produced"),
            "is_constructed": analysis.get("is_constructed"),
            # Perspectives (H1 proxy)
            "perspectives": entry.get("perspectives", {}),
        }

    return constraints


# ---------------------------------------------------------------------------
# Phase 1: Boolean feature extraction
# ---------------------------------------------------------------------------

# Each feature: (name, extractor_function, definition, prolog_source, boolean_spec_usage)
BOOLEAN_FEATURES = [
    {
        "name": "emerges_naturally",
        "definition": "Constraint arises from natural/structural dynamics without deliberate design",
        "prolog_source": "domain_priors (per testset), eval'd in maxent_classifier.pl",
        "boolean_specs": "mountain: required, rope: bonus",
        "extract": lambda c: bool(c.get("emerges_naturally")),
    },
    {
        "name": "requires_active_enforcement",
        "definition": "Constraint requires ongoing enforcement mechanisms to persist",
        "prolog_source": "domain_priors (per testset), eval'd in maxent_classifier.pl",
        "boolean_specs": "mountain: forbidden, tangled_rope: required",
        "extract": lambda c: bool(c.get("requires_active_enforcement")),
    },
    {
        "name": "has_coordination_function",
        "definition": "Constraint has beneficiaries (len(beneficiaries) > 0)",
        "prolog_source": "narrative_ontology.pl via constraint_beneficiary/2",
        "boolean_specs": "scaffold: required, tangled_rope: required",
        "extract": lambda c: len(c.get("beneficiaries", [])) > 0,
    },
    {
        "name": "has_asymmetric_extraction",
        "definition": "Constraint has victims (len(victims) > 0)",
        "prolog_source": "narrative_ontology.pl via constraint_victim/2",
        "boolean_specs": "tangled_rope: required",
        "extract": lambda c: len(c.get("victims", [])) > 0,
    },
    {
        "name": "natural_law_without_beneficiary",
        "definition": "emerges_naturally AND has no beneficiaries — pure natural constraint",
        "prolog_source": "drl_core.pl composite gate",
        "boolean_specs": "snare: forbidden, tangled_rope: forbidden",
        "extract": lambda c: (bool(c.get("emerges_naturally"))
                              and len(c.get("beneficiaries", [])) == 0),
    },
    {
        "name": "is_constructed",
        "definition": "Constraint is constructed (not natural) per corpus analysis",
        "prolog_source": "corpus_data.json analysis.is_constructed",
        "boolean_specs": "(not used in MaxEnt boolean_spec, structural metadata only)",
        "extract": lambda c: bool(c.get("is_constructed")),
    },
]


def extract_boolean_values(constraints):
    """Extract boolean feature values for all constraints.

    Returns: {feature_name: {constraint_id: bool}}
    """
    result = {}
    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        extractor = feat["extract"]
        result[name] = {}
        for cid, c in constraints.items():
            result[name][cid] = extractor(c)
    return result


# ---------------------------------------------------------------------------
# Phase 2: Independence metrics
# ---------------------------------------------------------------------------

def within_type_proportions(boolean_values, constraints):
    """Compute P(B=true | type=T) for each boolean B and type T.

    Returns: {feature_name: {type: {"true": n, "false": n, "total": n, "p_true": float}}}
    """
    result = {}
    for feat_name, vals in boolean_values.items():
        by_type = {}
        for t in MAXENT_TYPES:
            by_type[t] = {"true": 0, "false": 0, "total": 0}

        for cid, bval in vals.items():
            ctype = constraints[cid]["claimed_type"]
            if ctype not in by_type:
                continue
            by_type[ctype]["total"] += 1
            if bval:
                by_type[ctype]["true"] += 1
            else:
                by_type[ctype]["false"] += 1

        for t in MAXENT_TYPES:
            n = by_type[t]["total"]
            by_type[t]["p_true"] = by_type[t]["true"] / n if n > 0 else 0.0

        result[feat_name] = by_type
    return result


def independence_score(proportions):
    """Compute independence score: mean across types of min(P(B|T), 1-P(B|T)).

    0 = B is constant within every type (redundant).
    0.5 = B is 50/50 within every type (maximally independent).
    """
    type_mins = []
    for t in MAXENT_TYPES:
        p = proportions[t]["p_true"]
        n = proportions[t]["total"]
        if n > 0:
            type_mins.append(min(p, 1.0 - p))
    if not type_mins:
        return 0.0
    return sum(type_mins) / len(type_mins)


def chi_squared_test(boolean_values, constraints, feat_name):
    """Chi-squared test of independence between boolean B and type T.

    Returns dict with chi2, p_value, dof, cramers_v, or None if scipy unavailable.
    """
    if not HAS_SCIPY:
        return None

    # Build contingency table: rows = [B=false, B=true], cols = types
    table = [[0] * len(MAXENT_TYPES), [0] * len(MAXENT_TYPES)]
    type_idx = {t: i for i, t in enumerate(MAXENT_TYPES)}

    for cid, bval in boolean_values[feat_name].items():
        ctype = constraints[cid]["claimed_type"]
        if ctype not in type_idx:
            continue
        row = 1 if bval else 0
        table[row][type_idx[ctype]] += 1

    # Remove columns with all zeros (types with no constraints)
    non_empty = [j for j in range(len(MAXENT_TYPES))
                 if table[0][j] + table[1][j] > 0]
    if len(non_empty) < 2:
        return {"error": "fewer than 2 non-empty types"}

    filtered = [[table[i][j] for j in non_empty] for i in range(2)]

    # Check if either row is all zeros
    if sum(filtered[0]) == 0 or sum(filtered[1]) == 0:
        return {"error": "boolean is constant across all types"}

    try:
        chi2, p, dof, expected = scipy.stats.chi2_contingency(filtered)
        n_total = sum(sum(row) for row in filtered)
        k = min(len(filtered), len(filtered[0]))
        cramers_v = math.sqrt(chi2 / (n_total * (k - 1))) if n_total > 0 and k > 1 else 0.0

        return {
            "chi2": round(float(chi2), 4),
            "p_value": round(float(p), 8),
            "dof": int(dof),
            "cramers_v": round(float(cramers_v), 4),
            "contingency_table": {
                "rows": ["false", "true"],
                "cols": [MAXENT_TYPES[j] for j in non_empty],
                "data": filtered,
            },
        }
    except Exception as e:
        return {"error": str(e)}


def mutual_information(boolean_values, constraints, feat_name):
    """Compute normalized mutual information: MI(B, type) / H(B).

    NMI ~ 1: boolean is redundant (type determines boolean).
    NMI ~ 0: boolean is independent (type tells nothing about boolean).
    """
    # Joint counts
    joint = defaultdict(int)  # (bval, type) -> count
    b_counts = Counter()      # bval -> count
    t_counts = Counter()      # type -> count
    total = 0

    for cid, bval in boolean_values[feat_name].items():
        ctype = constraints[cid]["claimed_type"]
        if ctype not in MAXENT_TYPES:
            continue
        joint[(bval, ctype)] += 1
        b_counts[bval] += 1
        t_counts[ctype] += 1
        total += 1

    if total == 0:
        return None

    # H(B)
    h_b = 0.0
    for bval, count in b_counts.items():
        p = count / total
        if p > 0:
            h_b -= p * math.log2(p)

    if h_b < 1e-10:
        return {"mi": 0.0, "h_b": 0.0, "nmi": 0.0, "note": "H(B)~0, boolean is constant"}

    # H(T)
    h_t = 0.0
    for t, count in t_counts.items():
        p = count / total
        if p > 0:
            h_t -= p * math.log2(p)

    # MI(B, T) = sum over (b,t) of P(b,t) * log2(P(b,t) / (P(b)*P(t)))
    mi = 0.0
    for (bval, ctype), count in joint.items():
        p_joint = count / total
        p_b = b_counts[bval] / total
        p_t = t_counts[ctype] / total
        if p_joint > 0 and p_b > 0 and p_t > 0:
            mi += p_joint * math.log2(p_joint / (p_b * p_t))

    nmi = mi / h_b if h_b > 0 else 0.0

    return {
        "mi": round(mi, 6),
        "h_b": round(h_b, 6),
        "h_t": round(h_t, 6),
        "nmi": round(nmi, 6),
    }


# ---------------------------------------------------------------------------
# Phase 3: Diagnostic cross-reference
# ---------------------------------------------------------------------------

def orbit_family(orbit_sig):
    """Classify orbit signature into a family name."""
    if not orbit_sig:
        return "none"
    sig = tuple(sorted(orbit_sig))
    n = len(sig)
    if n == 1:
        return f"uniform_{sig[0]}"
    if n == 2:
        return f"binary_{'-'.join(sig)}"
    if n >= 4:
        return "full_dispersion"
    return f"trio_{'-'.join(sig)}"


def compute_h1(perspectives):
    """Compute H1 (observer disagreement) from perspectives dict.

    H1 = number of distinct types across perspectives / total perspectives.
    Returns 0 for uniform, 1 for full disagreement.
    """
    if not perspectives:
        return None
    types = list(perspectives.values())
    if not types:
        return None
    n_distinct = len(set(types))
    return (n_distinct - 1) / (len(types) - 1) if len(types) > 1 else 0.0


def diagnostic_comparison(constraints, boolean_values, feat_name, type_name):
    """Compare B=true vs B=false within a single type on diagnostic dimensions.

    Returns dict with orbit, confidence, and metric comparisons, or None if
    too few constraints in one group.
    """
    true_ids = []
    false_ids = []
    for cid, bval in boolean_values[feat_name].items():
        if constraints[cid]["claimed_type"] == type_name:
            if bval:
                true_ids.append(cid)
            else:
                false_ids.append(cid)

    if len(true_ids) < 3 or len(false_ids) < 3:
        return None

    result = {
        "n_true": len(true_ids),
        "n_false": len(false_ids),
    }

    # --- Orbit structure ---
    true_orbits = Counter(orbit_family(constraints[cid]["orbit_signature"])
                          for cid in true_ids)
    false_orbits = Counter(orbit_family(constraints[cid]["orbit_signature"])
                           for cid in false_ids)
    result["orbit_families"] = {
        "true": dict(true_orbits.most_common()),
        "false": dict(false_orbits.most_common()),
    }

    # H1 comparison
    true_h1 = [compute_h1(constraints[cid]["perspectives"]) for cid in true_ids]
    false_h1 = [compute_h1(constraints[cid]["perspectives"]) for cid in false_ids]
    true_h1 = [h for h in true_h1 if h is not None]
    false_h1 = [h for h in false_h1 if h is not None]

    if true_h1 and false_h1:
        result["h1"] = {
            "true_mean": round(sum(true_h1) / len(true_h1), 4),
            "false_mean": round(sum(false_h1) / len(false_h1), 4),
        }
        if HAS_SCIPY and len(true_h1) >= 3 and len(false_h1) >= 3:
            u_stat, u_p = scipy.stats.mannwhitneyu(true_h1, false_h1,
                                                   alternative='two-sided')
            result["h1"]["mann_whitney_p"] = round(float(u_p), 6)

    # Coalition type comparison
    true_coalitions = Counter(constraints[cid]["coalition_type"] for cid in true_ids)
    false_coalitions = Counter(constraints[cid]["coalition_type"] for cid in false_ids)
    result["coalition_types"] = {
        "true": dict(true_coalitions.most_common()),
        "false": dict(false_coalitions.most_common()),
    }

    # --- Confidence ---
    true_conf = [constraints[cid]["confidence"] for cid in true_ids
                 if constraints[cid]["confidence"] is not None]
    false_conf = [constraints[cid]["confidence"] for cid in false_ids
                  if constraints[cid]["confidence"] is not None]

    if true_conf and false_conf:
        result["confidence"] = {
            "true_mean": round(sum(true_conf) / len(true_conf), 4),
            "false_mean": round(sum(false_conf) / len(false_conf), 4),
        }
        if HAS_SCIPY and len(true_conf) >= 3 and len(false_conf) >= 3:
            u_stat, u_p = scipy.stats.mannwhitneyu(true_conf, false_conf,
                                                   alternative='two-sided')
            result["confidence"]["mann_whitney_p"] = round(float(u_p), 6)

    # Borderline proportion
    true_borderline = sum(1 for cid in true_ids
                          if constraints[cid].get("confidence_band") == "borderline")
    false_borderline = sum(1 for cid in false_ids
                           if constraints[cid].get("confidence_band") == "borderline")
    result["borderline"] = {
        "true_pct": round(100 * true_borderline / len(true_ids), 1),
        "false_pct": round(100 * false_borderline / len(false_ids), 1),
    }

    # --- Metric profiles ---
    for metric in ["extractiveness", "suppression", "theater_ratio"]:
        true_vals = [constraints[cid][metric] for cid in true_ids
                     if constraints[cid][metric] is not None]
        false_vals = [constraints[cid][metric] for cid in false_ids
                      if constraints[cid][metric] is not None]
        if true_vals and false_vals:
            entry = {
                "true_mean": round(sum(true_vals) / len(true_vals), 4),
                "false_mean": round(sum(false_vals) / len(false_vals), 4),
            }
            if HAS_SCIPY and len(true_vals) >= 3 and len(false_vals) >= 3:
                u_stat, u_p = scipy.stats.mannwhitneyu(true_vals, false_vals,
                                                       alternative='two-sided')
                entry["mann_whitney_p"] = round(float(u_p), 6)
            result[metric] = entry

    return result


# ---------------------------------------------------------------------------
# Phase 5: Pairwise boolean interaction
# ---------------------------------------------------------------------------

def pairwise_correlation(boolean_values, constraints, feat_a, feat_b):
    """Compute phi coefficient (correlation) between two binary features."""
    # 2x2 contingency table
    n00 = n01 = n10 = n11 = 0
    for cid in boolean_values[feat_a]:
        if cid not in boolean_values[feat_b]:
            continue
        a = boolean_values[feat_a][cid]
        b = boolean_values[feat_b][cid]
        if not a and not b:
            n00 += 1
        elif not a and b:
            n01 += 1
        elif a and not b:
            n10 += 1
        else:
            n11 += 1

    total = n00 + n01 + n10 + n11
    if total == 0:
        return None

    # Phi coefficient
    denom = math.sqrt((n11 + n10) * (n01 + n00) * (n11 + n01) * (n10 + n00))
    phi = (n11 * n00 - n10 * n01) / denom if denom > 0 else 0.0

    return {
        "contingency": {"FF": n00, "FT": n01, "TF": n10, "TT": n11},
        "phi": round(phi, 4),
        "total": total,
    }


def within_type_quadrant_analysis(constraints, boolean_values, feat_a, feat_b, type_name):
    """For two booleans within a single type, check if the 2x2 grid creates
    structurally distinct quadrants."""
    quadrants = {"FF": [], "FT": [], "TF": [], "TT": []}

    for cid in boolean_values[feat_a]:
        if cid not in boolean_values[feat_b]:
            continue
        if constraints[cid]["claimed_type"] != type_name:
            continue
        a = boolean_values[feat_a][cid]
        b = boolean_values[feat_b][cid]
        key = ("T" if a else "F") + ("T" if b else "F")
        quadrants[key].append(cid)

    # Only analyze if at least 2 quadrants have >= 3 members
    populated = {k: v for k, v in quadrants.items() if len(v) >= 3}
    if len(populated) < 2:
        return None

    result = {"counts": {k: len(v) for k, v in quadrants.items()}}

    # Compare mean confidence across populated quadrants
    quad_conf = {}
    for key, ids in populated.items():
        confs = [constraints[cid]["confidence"] for cid in ids
                 if constraints[cid]["confidence"] is not None]
        if confs:
            quad_conf[key] = round(sum(confs) / len(confs), 4)
    result["mean_confidence"] = quad_conf

    # Compare mean extractiveness
    quad_extract = {}
    for key, ids in populated.items():
        vals = [constraints[cid]["extractiveness"] for cid in ids
                if constraints[cid]["extractiveness"] is not None]
        if vals:
            quad_extract[key] = round(sum(vals) / len(vals), 4)
    result["mean_extractiveness"] = quad_extract

    return result


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def generate_report(constraints, boolean_values, proportions, indep_scores,
                    chi_results, mi_results, candidates, diagnostics,
                    pairwise_results, pairwise_quadrants):
    """Generate the full boolean independence report."""
    lines = []
    lines.append("# Boolean Feature Independence Analysis Report")
    lines.append("")
    lines.append("*Generated by `python/boolean_independence.py` — tests whether boolean")
    lines.append("features vary independently of type assignments, identifying potential")
    lines.append("missing dimensions in the current type space.*")
    lines.append("")

    total = len(constraints)
    type_counts = Counter(c["claimed_type"] for c in constraints.values())
    lines.append(f"**Corpus:** {total} constraints across {len(type_counts)} types")
    lines.append("")

    # Type distribution
    lines.append("| Type | Count | % |")
    lines.append("|------|------:|----:|")
    for t in MAXENT_TYPES:
        n = type_counts.get(t, 0)
        lines.append(f"| {t} | {n} | {100*n/total:.1f}% |")
    lines.append("")

    if not HAS_SCIPY:
        lines.append("**Note:** scipy not available. Chi-squared, mutual information,")
        lines.append("and Mann-Whitney U tests are skipped. Independence scores and")
        lines.append("within-type proportions are still computed.")
        lines.append("")

    # =================================================================
    # Section 1: Feature Inventory
    # =================================================================
    lines.append("## 1. Feature Inventory")
    lines.append("")

    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        vals = boolean_values[name]
        n_true = sum(1 for v in vals.values() if v)
        n_false = len(vals) - n_true
        lines.append(f"### {name}")
        lines.append("")
        lines.append(f"- **Definition:** {feat['definition']}")
        lines.append(f"- **Source:** {feat['prolog_source']}")
        lines.append(f"- **MaxEnt boolean_spec:** {feat['boolean_specs']}")
        lines.append(f"- **Corpus:** {n_true} true ({100*n_true/total:.1f}%), "
                     f"{n_false} false ({100*n_false/total:.1f}%)")
        lines.append("")

    # =================================================================
    # Section 2: Independence Summary
    # =================================================================
    lines.append("## 2. Independence Summary")
    lines.append("")

    # Main summary table
    header = "| Feature | Global P(true) | Independence Score |"
    sep = "|---------|---------------:|-------------------:|"
    if HAS_SCIPY:
        header += " Chi² p-value | Cramér's V | NMI |"
        sep += "--------------:|-----------:|----:|"
    header += " Verdict |"
    sep += "---------|"

    lines.append(header)
    lines.append(sep)

    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        vals = boolean_values[name]
        p_global = sum(1 for v in vals.values() if v) / len(vals)
        iscore = indep_scores[name]

        row = f"| {name} | {p_global:.3f} | {iscore:.3f} |"

        if HAS_SCIPY:
            chi = chi_results.get(name, {})
            mi = mi_results.get(name, {})
            chi_p = chi.get("p_value", "—") if chi and "error" not in chi else "—"
            cramers = chi.get("cramers_v", "—") if chi and "error" not in chi else "—"
            nmi = mi.get("nmi", "—") if mi else "—"
            row += f" {chi_p} | {cramers} | {nmi} |"

        # Verdict
        if iscore < 0.05:
            verdict = "**redundant**"
        elif iscore >= 0.15:
            nmi_val = mi_results.get(name, {}).get("nmi", 1.0) if mi_results.get(name) else 1.0
            if nmi_val < 0.3:
                verdict = "**INDEPENDENT**"
            elif nmi_val < 0.5:
                verdict = "partially independent"
            else:
                verdict = "weakly captured"
        else:
            verdict = "mostly captured"
        row += f" {verdict} |"

        lines.append(row)

    lines.append("")

    # =================================================================
    # Section 3: Within-Type Proportions
    # =================================================================
    lines.append("## 3. Within-Type Proportions P(B=true | type)")
    lines.append("")

    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        props = proportions[name]
        iscore = indep_scores[name]

        lines.append(f"### {name} (independence = {iscore:.3f})")
        lines.append("")
        lines.append("| Type | N | P(true) | Count(true) | Count(false) | Variation |")
        lines.append("|------|--:|--------:|------------:|-------------:|-----------|")

        for t in MAXENT_TYPES:
            p = props[t]
            n = p["total"]
            pt = p["p_true"]
            variation = min(pt, 1.0 - pt)
            bar = ""
            if n > 0:
                if variation < 0.05:
                    bar = "constant"
                elif variation < 0.15:
                    bar = "low variation"
                elif variation < 0.30:
                    bar = "moderate"
                else:
                    bar = "**high variation**"
            lines.append(f"| {t} | {n} | {pt:.3f} | {p['true']} | {p['false']} | {bar} |")

        lines.append("")

    # =================================================================
    # Section 4: Candidate Dimensions
    # =================================================================
    lines.append("## 4. Candidate Independent Dimensions")
    lines.append("")

    if not candidates:
        lines.append("No boolean features met the independence criteria")
        lines.append("(independence > 0.15 and NMI < 0.3).")
        lines.append("")
    else:
        lines.append(f"**{len(candidates)} candidate(s)** met the independence criteria:")
        lines.append("")

        for cand_name in candidates:
            feat_def = next(f for f in BOOLEAN_FEATURES if f["name"] == cand_name)
            iscore = indep_scores[cand_name]
            nmi_val = mi_results.get(cand_name, {}).get("nmi", "?")
            props = proportions[cand_name]

            lines.append(f"### {cand_name}")
            lines.append("")
            lines.append(f"- **Independence score:** {iscore:.3f}")
            lines.append(f"- **NMI:** {nmi_val}")
            lines.append(f"- **Definition:** {feat_def['definition']}")
            lines.append("")

            # Which types it varies within
            varying_types = []
            for t in MAXENT_TYPES:
                p = props[t]["p_true"]
                n = props[t]["total"]
                variation = min(p, 1.0 - p) if n > 0 else 0
                if variation >= 0.15 and n >= 5:
                    varying_types.append((t, p, n))

            if varying_types:
                lines.append("**Varies significantly within:**")
                lines.append("")
                for t, p, n in varying_types:
                    lines.append(f"- **{t}** (N={n}): P(true)={p:.3f}")
                lines.append("")

            # Diagnostic details
            diags = diagnostics.get(cand_name, {})
            if diags:
                for type_name, diag in diags.items():
                    if diag is None:
                        continue
                    lines.append(f"#### Within {type_name} "
                                 f"(true={diag['n_true']}, false={diag['n_false']})")
                    lines.append("")

                    # Confidence comparison
                    conf = diag.get("confidence")
                    if conf:
                        p_str = ""
                        if "mann_whitney_p" in conf:
                            p_str = f" (U-test p={conf['mann_whitney_p']})"
                        lines.append(f"- **Confidence:** true={conf['true_mean']:.3f}, "
                                     f"false={conf['false_mean']:.3f}{p_str}")

                    # Borderline
                    bl = diag.get("borderline", {})
                    if bl:
                        lines.append(f"- **Borderline:** true={bl['true_pct']:.1f}%, "
                                     f"false={bl['false_pct']:.1f}%")

                    # H1
                    h1 = diag.get("h1")
                    if h1:
                        p_str = ""
                        if "mann_whitney_p" in h1:
                            p_str = f" (U-test p={h1['mann_whitney_p']})"
                        lines.append(f"- **H1 (observer disagreement):** "
                                     f"true={h1['true_mean']:.3f}, "
                                     f"false={h1['false_mean']:.3f}{p_str}")

                    # Metrics
                    for metric in ["extractiveness", "suppression", "theater_ratio"]:
                        m = diag.get(metric)
                        if m:
                            p_str = ""
                            if "mann_whitney_p" in m:
                                p_str = f" (p={m['mann_whitney_p']})"
                            lines.append(f"- **{metric}:** true={m['true_mean']:.3f}, "
                                         f"false={m['false_mean']:.3f}{p_str}")

                    # Orbit families
                    orb = diag.get("orbit_families", {})
                    if orb.get("true") or orb.get("false"):
                        lines.append("")
                        lines.append("  Orbit families:")
                        lines.append(f"  - true: {orb.get('true', {})}")
                        lines.append(f"  - false: {orb.get('false', {})}")

                    # Coalition types
                    coal = diag.get("coalition_types", {})
                    if coal.get("true") or coal.get("false"):
                        lines.append("")
                        lines.append("  Coalition types:")
                        lines.append(f"  - true: {coal.get('true', {})}")
                        lines.append(f"  - false: {coal.get('false', {})}")

                    lines.append("")

    # =================================================================
    # Section 5: Pairwise Interactions
    # =================================================================
    lines.append("## 5. Pairwise Boolean Interactions")
    lines.append("")

    if not candidates or len(candidates) < 2:
        lines.append("Fewer than 2 candidates — pairwise analysis not applicable.")
        lines.append("")
    else:
        # Correlation matrix
        lines.append("### Phi Correlation Matrix (candidates)")
        lines.append("")

        cand_list = sorted(candidates)
        header = "| |"
        for c in cand_list:
            header += f" {c} |"
        lines.append(header)
        sep = "|---|"
        for _ in cand_list:
            sep += "---:|"
        lines.append(sep)

        for a in cand_list:
            row = f"| {a} |"
            for b in cand_list:
                if a == b:
                    row += " 1.0 |"
                else:
                    key = tuple(sorted([a, b]))
                    pr = pairwise_results.get(key)
                    if pr:
                        row += f" {pr['phi']:.3f} |"
                    else:
                        row += " — |"
            lines.append(row)
        lines.append("")

        # Quadrant analysis
        if pairwise_quadrants:
            lines.append("### Within-Type Quadrant Analysis")
            lines.append("")

            for (fa, fb, tname), quad in sorted(pairwise_quadrants.items()):
                if quad is None:
                    continue
                lines.append(f"#### {fa} x {fb} within {tname}")
                lines.append("")
                counts = quad["counts"]
                lines.append(f"| | {fb}=F | {fb}=T |")
                lines.append("|---|---:|---:|")
                lines.append(f"| {fa}=F | {counts.get('FF', 0)} | {counts.get('FT', 0)} |")
                lines.append(f"| {fa}=T | {counts.get('TF', 0)} | {counts.get('TT', 0)} |")
                lines.append("")

                mc = quad.get("mean_confidence", {})
                if mc:
                    lines.append(f"Mean confidence: {mc}")
                me = quad.get("mean_extractiveness", {})
                if me:
                    lines.append(f"Mean extractiveness: {me}")
                lines.append("")

    # =================================================================
    # Section 6: Recommendations
    # =================================================================
    lines.append("## 6. Recommendations")
    lines.append("")

    if not candidates:
        lines.append("All boolean features are well-captured by the current type space.")
        lines.append("No additional type dimensions are indicated by this analysis.")
    else:
        lines.append("The following boolean features show independence from the type space")
        lines.append("and may warrant further investigation:")
        lines.append("")
        for cand_name in candidates:
            iscore = indep_scores[cand_name]
            nmi_val = mi_results.get(cand_name, {}).get("nmi", "?")
            lines.append(f"- **{cand_name}** (independence={iscore:.3f}, NMI={nmi_val})")

            # Specific recommendation based on diagnostics
            diags = diagnostics.get(cand_name, {})
            has_orbit_diff = False
            has_conf_diff = False
            for type_name, diag in diags.items():
                if diag is None:
                    continue
                h1 = diag.get("h1", {})
                if h1 and abs(h1.get("true_mean", 0) - h1.get("false_mean", 0)) > 0.1:
                    has_orbit_diff = True
                conf = diag.get("confidence", {})
                if conf and abs(conf.get("true_mean", 0) - conf.get("false_mean", 0)) > 0.1:
                    has_conf_diff = True

            if has_orbit_diff and has_conf_diff:
                lines.append(f"  Shows different orbit structures AND confidence levels "
                             f"within types — strong candidate for a new dimension")
            elif has_orbit_diff:
                lines.append(f"  Shows different orbit structures within types — "
                             f"may represent a perspectival sub-dimension")
            elif has_conf_diff:
                lines.append(f"  Shows different confidence levels within types — "
                             f"the classifier sees the distinction but can't express it")
            else:
                lines.append(f"  Varies within types but without clear diagnostic signature — "
                             f"better handled as a sub-type modifier")

        lines.append("")
        lines.append("*This is exploratory analysis, not a commitment to add types.*")
        lines.append("*Further investigation should examine whether these dimensions*")
        lines.append("*create genuinely different constraint behaviors or merely*")
        lines.append("*reflect narrative variation within existing categories.*")

    lines.append("")

    # Cross-references
    lines.append("## 7. Cross-References")
    lines.append("")
    lines.append("- **boundary_normality_report.md** — Boundary population distributions")
    lines.append("- **classification_confidence_report.md** — Full-corpus confidence analysis")
    lines.append("- **tangled_rope_decomposition_report.md** — Fiber decomposition with psi metric")
    lines.append("- **maxent_report.md** — MaxEnt shadow classifier analysis")
    lines.append("- **maxent_diagnostic_report.md** — MaxEnt diagnostic details")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[BOOL] Loading data...", file=sys.stderr)
    constraints = load_constraints()
    print(f"[BOOL] Loaded {len(constraints)} constraints.", file=sys.stderr)

    if not constraints:
        print("[BOOL] ERROR: No constraints loaded.", file=sys.stderr)
        sys.exit(1)

    # Phase 1: Extract boolean values
    print("[BOOL] Phase 1: Extracting boolean feature values...", file=sys.stderr)
    boolean_values = extract_boolean_values(constraints)

    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        n_true = sum(1 for v in boolean_values[name].values() if v)
        print(f"[BOOL]   {name}: {n_true} true, "
              f"{len(boolean_values[name]) - n_true} false", file=sys.stderr)

    # Phase 2: Independence metrics
    print("[BOOL] Phase 2: Computing independence metrics...", file=sys.stderr)
    proportions = within_type_proportions(boolean_values, constraints)

    indep_scores = {}
    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        indep_scores[name] = independence_score(proportions[name])
        print(f"[BOOL]   {name}: independence = {indep_scores[name]:.3f}", file=sys.stderr)

    chi_results = {}
    mi_results = {}
    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        chi_results[name] = chi_squared_test(boolean_values, constraints, name)
        mi_results[name] = mutual_information(boolean_values, constraints, name)

    # Phase 3 & 4: Identify candidates and compute diagnostics
    print("[BOOL] Phase 3-4: Identifying candidates and computing diagnostics...",
          file=sys.stderr)

    candidates = []
    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        iscore = indep_scores[name]
        nmi_val = mi_results.get(name, {}).get("nmi", 1.0) if mi_results.get(name) else 1.0

        if iscore > 0.15 and nmi_val < 0.3:
            candidates.append(name)
            print(f"[BOOL]   CANDIDATE: {name} "
                  f"(independence={iscore:.3f}, NMI={nmi_val:.3f})", file=sys.stderr)

    # Even if NMI criteria not met, include features with high independence
    # that might still be interesting (but not already in candidates)
    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        if name not in candidates and indep_scores[name] > 0.15:
            nmi_val = mi_results.get(name, {}).get("nmi", 1.0) if mi_results.get(name) else 1.0
            print(f"[BOOL]   Near-candidate: {name} "
                  f"(independence={indep_scores[name]:.3f}, NMI={nmi_val:.3f})", file=sys.stderr)

    # Diagnostics for all features with independence > 0.15
    diagnostics = {}
    analysis_features = set(candidates)
    for feat in BOOLEAN_FEATURES:
        if indep_scores[feat["name"]] > 0.10:
            analysis_features.add(feat["name"])

    for feat_name in analysis_features:
        diagnostics[feat_name] = {}
        props = proportions[feat_name]
        for t in MAXENT_TYPES:
            p = props[t]["p_true"]
            n = props[t]["total"]
            variation = min(p, 1.0 - p) if n > 0 else 0
            if variation >= 0.10 and n >= 6:
                diag = diagnostic_comparison(constraints, boolean_values, feat_name, t)
                if diag:
                    diagnostics[feat_name][t] = diag

    # Phase 5: Pairwise interactions
    print("[BOOL] Phase 5: Pairwise boolean interactions...", file=sys.stderr)

    pairwise_results = {}
    pairwise_quadrants = {}

    if len(candidates) >= 2:
        for i, fa in enumerate(sorted(candidates)):
            for fb in sorted(candidates)[i+1:]:
                key = (fa, fb)
                pairwise_results[key] = pairwise_correlation(boolean_values, constraints,
                                                             fa, fb)
                # Within-type quadrant analysis for types where both features vary
                for t in MAXENT_TYPES:
                    pa = proportions[fa][t]["p_true"]
                    pb = proportions[fb][t]["p_true"]
                    na = proportions[fa][t]["total"]
                    if (min(pa, 1-pa) >= 0.10 and min(pb, 1-pb) >= 0.10 and na >= 10):
                        quad = within_type_quadrant_analysis(
                            constraints, boolean_values, fa, fb, t)
                        if quad:
                            pairwise_quadrants[(fa, fb, t)] = quad

    # Generate report
    print("[BOOL] Generating report...", file=sys.stderr)
    report = generate_report(constraints, boolean_values, proportions, indep_scores,
                             chi_results, mi_results, candidates, diagnostics,
                             pairwise_results, pairwise_quadrants)

    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"[BOOL] Wrote {REPORT_PATH}", file=sys.stderr)

    # Generate JSON data
    print("[BOOL] Writing JSON data...", file=sys.stderr)

    output_data = {
        "metadata": {
            "total_constraints": len(constraints),
            "has_scipy": HAS_SCIPY,
            "features_analyzed": [f["name"] for f in BOOLEAN_FEATURES],
            "candidates": candidates,
        },
        "per_feature": {},
        "pairwise": {},
    }

    for feat in BOOLEAN_FEATURES:
        name = feat["name"]
        vals = boolean_values[name]
        feat_data = {
            "definition": feat["definition"],
            "boolean_specs": feat["boolean_specs"],
            "global_counts": {
                "true": sum(1 for v in vals.values() if v),
                "false": sum(1 for v in vals.values() if not v),
            },
            "independence_score": round(indep_scores[name], 6),
            "within_type": {t: {
                "n": proportions[name][t]["total"],
                "true": proportions[name][t]["true"],
                "false": proportions[name][t]["false"],
                "p_true": round(proportions[name][t]["p_true"], 6),
            } for t in MAXENT_TYPES},
        }

        if chi_results.get(name):
            feat_data["chi_squared"] = chi_results[name]
        if mi_results.get(name):
            feat_data["mutual_information"] = mi_results[name]
        if diagnostics.get(name):
            feat_data["diagnostics"] = diagnostics[name]

        output_data["per_feature"][name] = feat_data

    # Pairwise data
    for (fa, fb), pr in pairwise_results.items():
        if pr:
            output_data["pairwise"][f"{fa}_x_{fb}"] = pr

    for (fa, fb, t), quad in pairwise_quadrants.items():
        if quad:
            key = f"{fa}_x_{fb}_within_{t}"
            output_data["pairwise"][key] = quad

    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(output_data, f, indent=2, cls=_SafeEncoder)
    print(f"[BOOL] Wrote {DATA_PATH}", file=sys.stderr)

    print("[BOOL] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
