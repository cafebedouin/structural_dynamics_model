"""Institutional dissent analysis — query and JSON functions.

Tests whether the low-snare subpopulation within institutional_dissent has
genuine informational advantage (A) or reflects metric bias (B) by comparing
structural, orbital, and domain properties between the two groups.
"""

import json
import math
import sys
from collections import Counter, defaultdict

from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Optional dependencies
# ---------------------------------------------------------------------------

HAS_SCIPY = False
try:
    import scipy.stats
    HAS_SCIPY = True
except ImportError:
    pass

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

SNARE_THRESHOLD = 0.5

CONTINUOUS_METRICS = [
    "base_extractiveness", "suppression", "theater_ratio",
    "purity_score", "confidence", "confidence_margin", "tangled_psi",
]


# ---------------------------------------------------------------------------
# Statistics helpers
# ---------------------------------------------------------------------------

def descriptive_stats(values):
    """Pure-Python descriptive statistics."""
    n = len(values)
    if n == 0:
        return {}
    s = sorted(values)
    mean = sum(s) / n
    median = s[n // 2] if n % 2 == 1 else (s[n // 2 - 1] + s[n // 2]) / 2.0
    variance = sum((x - mean) ** 2 for x in s) / n
    std = math.sqrt(variance)

    q25 = s[int(n * 0.25)]
    q75 = s[int(n * 0.75)]
    iqr = q75 - q25

    return {
        "n": n,
        "mean": round(mean, 6),
        "median": round(median, 6),
        "std": round(std, 6),
        "min": round(s[0], 6),
        "max": round(s[-1], 6),
        "q25": round(q25, 6),
        "q75": round(q75, 6),
        "iqr": round(iqr, 6),
    }


def rank_biserial(u_stat, n1, n2):
    """Compute rank-biserial correlation from Mann-Whitney U."""
    denom = n1 * n2
    if denom == 0:
        return 0.0
    return 1.0 - 2.0 * u_stat / denom


def compute_h1(perspectives):
    """Compute H1 (observer disagreement) from perspectives dict."""
    if not perspectives:
        return None
    types = list(perspectives.values())
    if not types:
        return None
    return len(set(types))


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_institutional_dissent(enriched_data, corpus_raw, orbit_raw):
    """Load and split institutional_dissent constraints by raw P(snare).

    Accepts pre-loaded data instead of calling load_json() directly.
    Returns (low_snare_list, high_snare_list, all_list).
    """
    per_constraint = enriched_data.get("per_constraint", [])
    corpus_constraints = corpus_raw.get("constraints", {})

    all_dissent = []

    for entry in per_constraint:
        cid = entry.get("id")
        if not cid:
            continue
        if entry.get("claimed_type") != "tangled_rope":
            continue
        if entry.get("coalition_type") != "institutional_dissent":
            continue

        raw_probs = entry.get("raw_maxent_probs", {})
        raw_p_snare = raw_probs.get("snare", 0.0)

        orbit_entry = orbit_raw.get(cid, {})
        corpus_entry = corpus_constraints.get(cid, {})
        analysis = corpus_entry.get("analysis", {})

        rec = {
            "id": cid,
            "human_readable": entry.get("human_readable", ""),
            "raw_p_snare": raw_p_snare,
            "raw_maxent_probs": raw_probs,
            "base_extractiveness": entry.get("base_extractiveness"),
            "suppression": entry.get("suppression"),
            "theater_ratio": entry.get("theater_ratio"),
            "purity_score": entry.get("purity_score"),
            "purity_band": entry.get("purity_band"),
            "confidence": entry.get("confidence"),
            "confidence_margin": entry.get("confidence_margin"),
            "confidence_band": entry.get("confidence_band"),
            "tangled_psi": entry.get("tangled_psi"),
            "tangled_band": entry.get("tangled_band"),
            "signature": entry.get("signature"),
            "boundary": entry.get("boundary"),
            "rival_type": entry.get("rival_type"),
            "perspectives": entry.get("perspectives", {}),
            "beneficiaries": entry.get("beneficiaries", []),
            "victims": entry.get("victims", []),
            "topic_domain": entry.get("topic_domain") or entry.get("domain", ""),
            "domain": entry.get("domain", ""),
            "emerges_naturally": entry.get("emerges_naturally", False),
            "requires_active_enforcement": entry.get("requires_active_enforcement", False),
            "orbit_signature": orbit_entry.get("orbit_signature",
                                               analysis.get("orbit_signature", [])),
            "orbit_contexts": orbit_entry.get("contexts",
                                              analysis.get("orbit_contexts", {})),
        }
        all_dissent.append(rec)

    low_snare = [r for r in all_dissent if r["raw_p_snare"] < SNARE_THRESHOLD]
    high_snare = [r for r in all_dissent if r["raw_p_snare"] >= SNARE_THRESHOLD]

    return low_snare, high_snare, all_dissent


# ---------------------------------------------------------------------------
# Section 1: Population overview
# ---------------------------------------------------------------------------

def population_overview(low, high):
    """Basic population counts and P(snare) distribution stats."""
    low_snare_vals = [r["raw_p_snare"] for r in low]
    high_snare_vals = [r["raw_p_snare"] for r in high]

    return {
        "low_snare": {
            "n": len(low),
            "p_snare_stats": descriptive_stats(low_snare_vals),
        },
        "high_snare": {
            "n": len(high),
            "p_snare_stats": descriptive_stats(high_snare_vals),
        },
        "total": len(low) + len(high),
        "threshold": SNARE_THRESHOLD,
    }


# ---------------------------------------------------------------------------
# Section 2: Metric profile comparison
# ---------------------------------------------------------------------------

def metric_profile_comparison(low, high):
    """Compare continuous metrics between low-snare and high-snare groups."""
    results = {}

    for metric in CONTINUOUS_METRICS:
        low_vals = [r[metric] for r in low if r.get(metric) is not None]
        high_vals = [r[metric] for r in high if r.get(metric) is not None]

        entry = {
            "low_snare": descriptive_stats(low_vals),
            "high_snare": descriptive_stats(high_vals),
        }

        if HAS_SCIPY and len(low_vals) >= 3 and len(high_vals) >= 3:
            try:
                u_stat, u_p = scipy.stats.mannwhitneyu(low_vals, high_vals,
                                                       alternative='two-sided')
                r = rank_biserial(float(u_stat), len(low_vals), len(high_vals))
                entry["mann_whitney"] = {
                    "U": round(float(u_stat), 4),
                    "p_value": round(float(u_p), 8),
                    "rank_biserial_r": round(r, 4),
                }
            except Exception as e:
                entry["mann_whitney"] = {"error": str(e)}

        results[metric] = entry

    return results


# ---------------------------------------------------------------------------
# Section 3: Signature comparison
# ---------------------------------------------------------------------------

def signature_comparison(low, high):
    """Cross-tabulate signature x {low_snare, high_snare}."""
    low_sigs = Counter(r.get("signature") or "none" for r in low)
    high_sigs = Counter(r.get("signature") or "none" for r in high)

    all_sigs = sorted(set(low_sigs.keys()) | set(high_sigs.keys()))

    cross_tab = {}
    for sig in all_sigs:
        cross_tab[sig] = {
            "low_snare": low_sigs.get(sig, 0),
            "high_snare": high_sigs.get(sig, 0),
        }

    chi_result = None
    cramers_v = None

    if HAS_SCIPY and len(all_sigs) >= 2:
        table = []
        for sig in all_sigs:
            row = [low_sigs.get(sig, 0), high_sigs.get(sig, 0)]
            if row[0] + row[1] > 0:
                table.append(row)

        if len(table) >= 2:
            try:
                chi2, p, dof, expected = scipy.stats.chi2_contingency(table)
                n_total = sum(sum(row) for row in table)
                k = min(len(table), len(table[0]))
                cramers_v = math.sqrt(chi2 / (n_total * (k - 1))) if n_total > 0 and k > 1 else 0.0
                chi_result = {
                    "chi2": round(float(chi2), 4),
                    "p_value": round(float(p), 8),
                    "dof": int(dof),
                    "cramers_v": round(cramers_v, 4),
                }
            except Exception as e:
                chi_result = {"error": str(e)}

    return {
        "cross_tab": cross_tab,
        "chi_squared": chi_result,
        "cramers_v": round(cramers_v, 4) if cramers_v is not None else None,
    }


# ---------------------------------------------------------------------------
# Section 4: Orbit structure comparison
# ---------------------------------------------------------------------------

def orbit_structure_comparison(low, high):
    """Compare orbit signatures and perspective patterns between groups."""
    results = {}

    def orbit_key(r):
        sig = r.get("orbit_signature", [])
        return tuple(sorted(sig)) if sig else ("none",)

    low_orbits = Counter(orbit_key(r) for r in low)
    high_orbits = Counter(orbit_key(r) for r in high)
    all_orbit_keys = sorted(set(low_orbits.keys()) | set(high_orbits.keys()))

    orbit_cross_tab = {}
    for ok in all_orbit_keys:
        label = "+".join(ok)
        orbit_cross_tab[label] = {
            "low_snare": low_orbits.get(ok, 0),
            "high_snare": high_orbits.get(ok, 0),
        }
    results["orbit_signature_distribution"] = orbit_cross_tab

    if HAS_SCIPY and len(all_orbit_keys) >= 2:
        table = []
        for ok in all_orbit_keys:
            row = [low_orbits.get(ok, 0), high_orbits.get(ok, 0)]
            if row[0] + row[1] > 0:
                table.append(row)
        if len(table) >= 2:
            try:
                chi2, p, dof, expected = scipy.stats.chi2_contingency(table)
                n_total = sum(sum(row) for row in table)
                k = min(len(table), len(table[0]))
                cv = math.sqrt(chi2 / (n_total * (k - 1))) if n_total > 0 and k > 1 else 0.0
                results["orbit_chi_squared"] = {
                    "chi2": round(float(chi2), 4),
                    "p_value": round(float(p), 8),
                    "dof": int(dof),
                    "cramers_v": round(cv, 4),
                }
            except Exception as e:
                results["orbit_chi_squared"] = {"error": str(e)}

    low_h1 = [compute_h1(r["perspectives"]) for r in low]
    high_h1 = [compute_h1(r["perspectives"]) for r in high]
    low_h1 = [h for h in low_h1 if h is not None]
    high_h1 = [h for h in high_h1 if h is not None]

    results["h1"] = {
        "low_snare": descriptive_stats(low_h1),
        "high_snare": descriptive_stats(high_h1),
    }

    if HAS_SCIPY and len(low_h1) >= 3 and len(high_h1) >= 3:
        try:
            u_stat, u_p = scipy.stats.mannwhitneyu(low_h1, high_h1,
                                                   alternative='two-sided')
            results["h1"]["mann_whitney_p"] = round(float(u_p), 6)
        except Exception:
            pass

    observer_views = {}
    for perspective in ["powerless", "moderate", "analytical"]:
        low_types = Counter(r["perspectives"].get(perspective, "unknown") for r in low
                           if r["perspectives"].get(perspective))
        high_types = Counter(r["perspectives"].get(perspective, "unknown") for r in high
                            if r["perspectives"].get(perspective))
        observer_views[perspective] = {
            "low_snare": dict(low_types.most_common()),
            "high_snare": dict(high_types.most_common()),
        }
    results["non_institutional_views"] = observer_views

    def perspective_pattern(r):
        p = r.get("perspectives", {})
        parts = []
        for key in ["powerless", "moderate", "institutional", "analytical"]:
            parts.append(p.get(key, "?"))
        return "/".join(parts)

    low_patterns = Counter(perspective_pattern(r) for r in low)
    high_patterns = Counter(perspective_pattern(r) for r in high)
    results["perspective_patterns"] = {
        "low_snare": dict(low_patterns.most_common(10)),
        "high_snare": dict(high_patterns.most_common(10)),
    }

    return results


# ---------------------------------------------------------------------------
# Section 5: Domain analysis
# ---------------------------------------------------------------------------

def domain_analysis(low, high):
    """Compare topic_domain distributions between groups."""
    low_domains = Counter(r.get("topic_domain") or "unknown" for r in low)
    high_domains = Counter(r.get("topic_domain") or "unknown" for r in high)

    all_domains = sorted(set(low_domains.keys()) | set(high_domains.keys()))

    cross_tab = {}
    for d in all_domains:
        cross_tab[d] = {
            "low_snare": low_domains.get(d, 0),
            "high_snare": high_domains.get(d, 0),
        }

    chi_result = None
    cramers_v = None

    if HAS_SCIPY and len(all_domains) >= 2:
        table = []
        for d in all_domains:
            row = [low_domains.get(d, 0), high_domains.get(d, 0)]
            if row[0] + row[1] > 0:
                table.append(row)
        if len(table) >= 2:
            try:
                chi2, p, dof, expected = scipy.stats.chi2_contingency(table)
                n_total = sum(sum(row) for row in table)
                k = min(len(table), len(table[0]))
                cramers_v = math.sqrt(chi2 / (n_total * (k - 1))) if n_total > 0 and k > 1 else 0.0
                chi_result = {
                    "chi2": round(float(chi2), 4),
                    "p_value": round(float(p), 8),
                    "dof": int(dof),
                    "cramers_v": round(cramers_v, 4),
                }
            except Exception as e:
                chi_result = {"error": str(e)}

    return {
        "cross_tab": cross_tab,
        "chi_squared": chi_result,
        "cramers_v": round(cramers_v, 4) if cramers_v is not None else None,
    }


# ---------------------------------------------------------------------------
# Section 6: Beneficiary/victim analysis
# ---------------------------------------------------------------------------

def beneficiary_victim_analysis(low, high):
    """Compare beneficiary and victim patterns between groups."""
    results = {}

    for role in ["beneficiaries", "victims"]:
        low_counts = [len(r.get(role, [])) for r in low]
        high_counts = [len(r.get(role, [])) for r in high]

        entry = {
            "low_snare": descriptive_stats(low_counts),
            "high_snare": descriptive_stats(high_counts),
        }

        if HAS_SCIPY and len(low_counts) >= 3 and len(high_counts) >= 3:
            try:
                u_stat, u_p = scipy.stats.mannwhitneyu(low_counts, high_counts,
                                                       alternative='two-sided')
                entry["mann_whitney_p"] = round(float(u_p), 6)
            except Exception:
                pass

        low_labels = set()
        high_labels = set()
        for r in low:
            low_labels.update(r.get(role, []))
        for r in high:
            high_labels.update(r.get(role, []))

        entry["unique_labels"] = {
            "low_snare": len(low_labels),
            "high_snare": len(high_labels),
            "overlap": len(low_labels & high_labels),
        }

        results[role] = entry

    low_overlap = 0
    high_overlap = 0
    for r in low:
        b = set(r.get("beneficiaries", []))
        v = set(r.get("victims", []))
        if b & v:
            low_overlap += 1
    for r in high:
        b = set(r.get("beneficiaries", []))
        v = set(r.get("victims", []))
        if b & v:
            high_overlap += 1

    results["beneficiary_victim_overlap"] = {
        "low_snare": low_overlap,
        "high_snare": high_overlap,
    }

    return results


# ---------------------------------------------------------------------------
# Section 7: Rank discriminants
# ---------------------------------------------------------------------------

def rank_discriminants(metric_results, sig_results, orbit_results, domain_results):
    """Rank features by discriminative power."""
    ranked = []

    for metric, data in metric_results.items():
        mw = data.get("mann_whitney", {})
        r = mw.get("rank_biserial_r")
        if r is not None:
            ranked.append({
                "feature": metric,
                "type": "continuous",
                "effect_size": abs(r),
                "metric": "|rank_biserial_r|",
                "raw_value": r,
                "p_value": mw.get("p_value"),
            })

    for label, data, feature_type in [
        ("signature", sig_results, "categorical"),
        ("orbit_signature", orbit_results.get("orbit_chi_squared", {}), "categorical"),
        ("topic_domain", domain_results, "categorical"),
    ]:
        cv = None
        pv = None
        if isinstance(data, dict):
            cv = data.get("cramers_v")
            chi = data.get("chi_squared", data)
            if isinstance(chi, dict):
                pv = chi.get("p_value")
                if cv is None:
                    cv = chi.get("cramers_v")
        if cv is not None:
            ranked.append({
                "feature": label,
                "type": feature_type,
                "effect_size": abs(cv),
                "metric": "cramers_v",
                "raw_value": cv,
                "p_value": pv,
            })

    ranked.sort(key=lambda x: x["effect_size"], reverse=True)
    return ranked[:10]


# ---------------------------------------------------------------------------
# Section 8: Spot-check extremes
# ---------------------------------------------------------------------------

def spot_check_extremes(all_dissent):
    """Identify the 10 lowest and 10 highest P(snare) constraints."""
    sorted_all = sorted(all_dissent, key=lambda r: r["raw_p_snare"])

    def extract_spot(r):
        raw = r.get("raw_maxent_probs", {})
        return {
            "id": r["id"],
            "human_readable": r.get("human_readable", ""),
            "topic_domain": r.get("topic_domain", ""),
            "raw_p_snare": round(r["raw_p_snare"], 6),
            "raw_p_rope": round(raw.get("rope", 0.0), 6),
            "raw_p_tangled_rope": round(raw.get("tangled_rope", 0.0), 6),
            "base_extractiveness": r.get("base_extractiveness"),
            "suppression": r.get("suppression"),
            "theater_ratio": r.get("theater_ratio"),
            "perspectives": r.get("perspectives", {}),
        }

    lowest_10 = [extract_spot(r) for r in sorted_all[:10]]
    highest_10 = [extract_spot(r) for r in sorted_all[-10:]]

    return {
        "lowest_snare": lowest_10,
        "highest_snare": highest_10,
    }


# ---------------------------------------------------------------------------
# Sanitize numpy types for plain json.dump()
# ---------------------------------------------------------------------------

def _sanitize(obj):
    """Recursively convert numpy types to Python natives."""
    if isinstance(obj, dict):
        return {k: _sanitize(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_sanitize(v) for v in obj]
    if hasattr(obj, 'item'):
        return obj.item()
    return obj


# ---------------------------------------------------------------------------
# Query and JSON entry points
# ---------------------------------------------------------------------------

def _run_analysis(data):
    """Shared computation for query and json_fn."""
    enriched_data = data["enriched"]
    corpus_raw = data["corpus"]
    orbit_raw = data["orbit"]

    low, high, all_dissent = load_institutional_dissent(enriched_data, corpus_raw, orbit_raw)

    population = population_overview(low, high)
    metric_results = metric_profile_comparison(low, high)
    sig_results = signature_comparison(low, high)
    orbit_results = orbit_structure_comparison(low, high)
    domain_results = domain_analysis(low, high)
    bv_results = beneficiary_victim_analysis(low, high)
    discriminants = rank_discriminants(metric_results, sig_results,
                                       orbit_results, domain_results)
    spot_checks = spot_check_extremes(all_dissent)

    return {
        "population": population,
        "metric_results": metric_results,
        "sig_results": sig_results,
        "orbit_results": orbit_results,
        "domain_results": domain_results,
        "bv_results": bv_results,
        "discriminants": discriminants,
        "spot_checks": spot_checks,
        "has_scipy": HAS_SCIPY,
        "snare_threshold": SNARE_THRESHOLD,
        "continuous_metrics": CONTINUOUS_METRICS,
    }


def query(data: dict) -> dict:
    """Enriched/corpus/orbit data -> template context for institutional dissent report."""
    return _run_analysis(data)


def json_fn(data: dict):
    """Enriched/corpus/orbit data -> JSON-serializable aggregate structure."""
    ctx = _run_analysis(data)

    output = {
        "metadata": {
            "description": "Institutional dissent analysis: low-snare vs high-snare comparison",
            "snare_threshold": SNARE_THRESHOLD,
            "has_scipy": HAS_SCIPY,
            "total": ctx["population"]["total"],
            "low_snare_n": ctx["population"]["low_snare"]["n"],
            "high_snare_n": ctx["population"]["high_snare"]["n"],
        },
        "population": ctx["population"],
        "metric_comparison": ctx["metric_results"],
        "signature_comparison": ctx["sig_results"],
        "orbit_comparison": ctx["orbit_results"],
        "domain_comparison": ctx["domain_results"],
        "beneficiary_victim": ctx["bv_results"],
        "discriminant_ranking": ctx["discriminants"],
        "spot_check_extremes": ctx["spot_checks"],
    }

    return _sanitize(output)
