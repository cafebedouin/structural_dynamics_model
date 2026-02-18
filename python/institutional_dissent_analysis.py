#!/usr/bin/env python3
"""Institutional Dissent Analysis (v1.0)

The tangled_rope institutional_dissent coalition splits into two populations
by raw P(snare): high-snare (P(snare) >= 0.5, cover story -- institution masks
extraction) and low-snare (P(snare) < 0.5, puzzle -- MaxEnt partially agrees
with institutional view).

This analysis tests whether the low-snare group has genuine informational
advantage (A) or reflects metric bias (B) by comparing structural, orbital,
and domain properties between the two groups.

Reads from enriched pipeline_output.json, corpus_data.json, orbit_data.json.
No recomputation.

Outputs:
  outputs/institutional_dissent_data.json   - aggregate statistical data
  outputs/institutional_dissent_report.md   - analysis report

Usage:
  python3 python/institutional_dissent_analysis.py
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
    print("Warning: scipy not available. Skipping Mann-Whitney and chi-squared tests.",
          file=sys.stderr)

# ---------------------------------------------------------------------------
# Paths and constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
OUTPUT_DIR = ROOT_DIR / "outputs"

PIPELINE_JSON = OUTPUT_DIR / "pipeline_output.json"
CORPUS_JSON = OUTPUT_DIR / "corpus_data.json"
ORBIT_JSON = OUTPUT_DIR / "orbit_data.json"

REPORT_PATH = OUTPUT_DIR / "institutional_dissent_report.md"
DATA_PATH = OUTPUT_DIR / "institutional_dissent_data.json"

SNARE_THRESHOLD = 0.5

CONTINUOUS_METRICS = [
    "base_extractiveness", "suppression", "theater_ratio",
    "purity_score", "confidence", "confidence_margin", "tangled_psi",
]


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
    """Compute rank-biserial correlation from Mann-Whitney U.

    r = 1 - 2*U/(n1*n2). Range [-1, 1].
    """
    denom = n1 * n2
    if denom == 0:
        return 0.0
    return 1.0 - 2.0 * u_stat / denom


def compute_h1(perspectives):
    """Compute H1 (observer disagreement) from perspectives dict.

    H1 = number of distinct types across perspectives.
    """
    if not perspectives:
        return None
    types = list(perspectives.values())
    if not types:
        return None
    return len(set(types))


def load_institutional_dissent():
    """Load and split institutional_dissent constraints by raw P(snare).

    Returns (low_snare_list, high_snare_list, all_list) where each item is a dict
    with merged pipeline + orbit data.
    """
    pipeline_raw = load_json(PIPELINE_JSON, "pipeline_output")
    corpus_raw = load_json(CORPUS_JSON, "corpus_data")
    orbit_raw = load_json(ORBIT_JSON, "orbit_data")

    per_constraint = pipeline_raw.get("per_constraint", [])
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

        # Merge orbit data
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
            # Orbit data
            "orbit_signature": orbit_entry.get("orbit_signature",
                                               analysis.get("orbit_signature", [])),
            "orbit_contexts": orbit_entry.get("contexts",
                                              analysis.get("orbit_contexts", {})),
        }
        all_dissent.append(rec)

    # Split by threshold
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
        # Build contingency table: rows = signatures, cols = [low, high]
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

    # Orbit signature distribution (sorted tuple of types)
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

    # Chi-squared on orbit signatures
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

    # H1: number of distinct types across perspectives
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

    # Non-institutional observer views
    # What do powerless, moderate, analytical see in each group?
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

    # Full perspective pattern distribution
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

        # Unique label diversity
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

    # Overlap detection: constraints where same agents appear as both
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

    # Continuous features by |rank_biserial_r|
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

    # Categorical features by Cramer's V
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
# Report generation
# ---------------------------------------------------------------------------

def generate_report(population, metric_results, sig_results, orbit_results,
                    domain_results, bv_results, discriminants, spot_checks):
    """Generate the full institutional dissent analysis report."""
    lines = []
    lines.append("# Institutional Dissent Analysis Report")
    lines.append("")
    lines.append("*Generated by `python/institutional_dissent_analysis.py` -- tests whether")
    lines.append("the low-snare subpopulation within institutional_dissent has genuine")
    lines.append("informational advantage (A) or reflects metric bias (B).*")
    lines.append("")

    n_low = population["low_snare"]["n"]
    n_high = population["high_snare"]["n"]
    n_total = population["total"]

    lines.append(f"**Population:** {n_total} institutional_dissent constraints")
    lines.append(f"(tangled_rope with coalition_type = institutional_dissent)")
    lines.append(f"- **Low-snare** (P(snare) < {SNARE_THRESHOLD}): {n_low} constraints")
    lines.append(f"- **High-snare** (P(snare) >= {SNARE_THRESHOLD}): {n_high} constraints")
    lines.append("")

    if not HAS_SCIPY:
        lines.append("**Note:** scipy not available. Mann-Whitney U and chi-squared tests")
        lines.append("are skipped. Descriptive statistics are still computed.")
        lines.append("")

    # ==================================================================
    # Section 1: P(snare) distribution
    # ==================================================================
    lines.append("## 1. P(snare) Distribution by Group")
    lines.append("")

    for group, label in [("low_snare", "Low-Snare"), ("high_snare", "High-Snare")]:
        stats = population[group]["p_snare_stats"]
        if stats:
            lines.append(f"### {label} (N={stats['n']})")
            lines.append("")
            lines.append("| Statistic | Value |")
            lines.append("|-----------|------:|")
            for key in ["mean", "median", "std", "min", "max", "q25", "q75", "iqr"]:
                if key in stats:
                    lines.append(f"| {key} | {stats[key]} |")
            lines.append("")

    # ==================================================================
    # Section 2: Metric profile comparison
    # ==================================================================
    lines.append("## 2. Metric Profile Comparison")
    lines.append("")
    lines.append("*Note: purity subscores (factorizability, scope_invariance,")
    lines.append("coupling_cleanliness, excess_extraction) are not exported to JSON --")
    lines.append("only aggregate purity_score is available.*")
    lines.append("")

    # Summary table
    header = "| Metric | Low Mean | High Mean | Low Median | High Median |"
    sep = "|--------|--------:|---------:|----------:|-----------:|"
    if HAS_SCIPY:
        header += " U-test p | Rank-Biserial r |"
        sep += "---------:|----------------:|"
    lines.append(header)
    lines.append(sep)

    for metric in CONTINUOUS_METRICS:
        data = metric_results.get(metric, {})
        low_stats = data.get("low_snare", {})
        high_stats = data.get("high_snare", {})
        row = f"| {metric} | {low_stats.get('mean', '—')} | {high_stats.get('mean', '—')} |"
        row += f" {low_stats.get('median', '—')} | {high_stats.get('median', '—')} |"
        if HAS_SCIPY:
            mw = data.get("mann_whitney", {})
            p = mw.get("p_value", "—")
            r = mw.get("rank_biserial_r", "—")
            row += f" {p} | {r} |"
        lines.append(row)
    lines.append("")

    # ==================================================================
    # Section 3: Signature comparison
    # ==================================================================
    lines.append("## 3. Signature Comparison")
    lines.append("")

    cross_tab = sig_results.get("cross_tab", {})
    if cross_tab:
        lines.append("| Signature | Low-Snare | High-Snare | Total | % Low |")
        lines.append("|-----------|----------:|-----------:|------:|------:|")
        for sig in sorted(cross_tab.keys()):
            lo = cross_tab[sig]["low_snare"]
            hi = cross_tab[sig]["high_snare"]
            total = lo + hi
            pct_lo = (100.0 * lo / total) if total > 0 else 0.0
            lines.append(f"| {sig} | {lo} | {hi} | {total} | {pct_lo:.1f}% |")
        lines.append("")

    chi = sig_results.get("chi_squared")
    if chi and "error" not in chi:
        lines.append(f"Chi-squared: chi2={chi['chi2']}, df={chi['dof']}, "
                     f"p={chi['p_value']}, Cramer's V={chi['cramers_v']}")
        lines.append("")

    # ==================================================================
    # Section 4: Orbit structure comparison
    # ==================================================================
    lines.append("## 4. Orbit Structure Comparison")
    lines.append("")

    # Orbit signature distribution
    orbit_dist = orbit_results.get("orbit_signature_distribution", {})
    if orbit_dist:
        lines.append("### Orbit Signature Distribution")
        lines.append("")
        lines.append("| Orbit Signature | Low-Snare | High-Snare | Total |")
        lines.append("|-----------------|----------:|-----------:|------:|")
        for label in sorted(orbit_dist.keys()):
            lo = orbit_dist[label]["low_snare"]
            hi = orbit_dist[label]["high_snare"]
            lines.append(f"| {label} | {lo} | {hi} | {lo + hi} |")
        lines.append("")

    orbit_chi = orbit_results.get("orbit_chi_squared")
    if orbit_chi and "error" not in orbit_chi:
        lines.append(f"Chi-squared: chi2={orbit_chi['chi2']}, df={orbit_chi['dof']}, "
                     f"p={orbit_chi['p_value']}, Cramer's V={orbit_chi['cramers_v']}")
        lines.append("")

    # H1
    h1_data = orbit_results.get("h1", {})
    low_h1 = h1_data.get("low_snare", {})
    high_h1 = h1_data.get("high_snare", {})
    if low_h1 and high_h1:
        lines.append("### H1 (Observer Disagreement)")
        lines.append("")
        lines.append(f"- Low-snare: mean={low_h1.get('mean', '—')}, "
                     f"median={low_h1.get('median', '—')}")
        lines.append(f"- High-snare: mean={high_h1.get('mean', '—')}, "
                     f"median={high_h1.get('median', '—')}")
        mw_p = h1_data.get("mann_whitney_p")
        if mw_p is not None:
            lines.append(f"- Mann-Whitney p={mw_p}")
        lines.append("")

    # Non-institutional views
    niv = orbit_results.get("non_institutional_views", {})
    if niv:
        lines.append("### Non-Institutional Observer Views")
        lines.append("")
        lines.append("What powerless, moderate, and analytical observers see in each group:")
        lines.append("")
        for perspective in ["powerless", "moderate", "analytical"]:
            pdata = niv.get(perspective, {})
            low_types = pdata.get("low_snare", {})
            high_types = pdata.get("high_snare", {})
            lines.append(f"**{perspective}:**")
            lines.append("")
            lines.append(f"| Type | Low-Snare | High-Snare |")
            lines.append(f"|------|----------:|-----------:|")
            all_types = sorted(set(low_types.keys()) | set(high_types.keys()))
            for t in all_types:
                lines.append(f"| {t} | {low_types.get(t, 0)} | {high_types.get(t, 0)} |")
            lines.append("")

    # Perspective patterns
    pp = orbit_results.get("perspective_patterns", {})
    if pp:
        lines.append("### Perspective Patterns (powerless/moderate/institutional/analytical)")
        lines.append("")
        for group, label in [("low_snare", "Low-Snare"), ("high_snare", "High-Snare")]:
            patterns = pp.get(group, {})
            if patterns:
                lines.append(f"**{label} top patterns:**")
                lines.append("")
                lines.append("| Pattern | Count |")
                lines.append("|---------|------:|")
                for pattern, count in sorted(patterns.items(), key=lambda x: -x[1]):
                    lines.append(f"| {pattern} | {count} |")
                lines.append("")

    # ==================================================================
    # Section 5: Domain analysis
    # ==================================================================
    lines.append("## 5. Domain Analysis (topic_domain)")
    lines.append("")

    domain_tab = domain_results.get("cross_tab", {})
    if domain_tab:
        lines.append("| Domain | Low-Snare | High-Snare | Total |")
        lines.append("|--------|----------:|-----------:|------:|")
        for d in sorted(domain_tab.keys()):
            lo = domain_tab[d]["low_snare"]
            hi = domain_tab[d]["high_snare"]
            lines.append(f"| {d} | {lo} | {hi} | {lo + hi} |")
        lines.append("")

    domain_chi = domain_results.get("chi_squared")
    if domain_chi and "error" not in domain_chi:
        lines.append(f"Chi-squared: chi2={domain_chi['chi2']}, df={domain_chi['dof']}, "
                     f"p={domain_chi['p_value']}, Cramer's V={domain_chi['cramers_v']}")
        lines.append("")

    # ==================================================================
    # Section 6: Beneficiary/victim analysis
    # ==================================================================
    lines.append("## 6. Beneficiary/Victim Analysis")
    lines.append("")

    for role in ["beneficiaries", "victims"]:
        role_data = bv_results.get(role, {})
        low_stats = role_data.get("low_snare", {})
        high_stats = role_data.get("high_snare", {})
        unique = role_data.get("unique_labels", {})
        mw_p = role_data.get("mann_whitney_p")

        lines.append(f"### {role.capitalize()}")
        lines.append("")
        lines.append(f"- Low-snare: mean count={low_stats.get('mean', '—')}, "
                     f"unique labels={unique.get('low_snare', '—')}")
        lines.append(f"- High-snare: mean count={high_stats.get('mean', '—')}, "
                     f"unique labels={unique.get('high_snare', '—')}")
        lines.append(f"- Label overlap: {unique.get('overlap', '—')}")
        if mw_p is not None:
            lines.append(f"- Mann-Whitney p={mw_p} (count comparison)")
        lines.append("")

    overlap = bv_results.get("beneficiary_victim_overlap", {})
    if overlap:
        lines.append("### Beneficiary-Victim Overlap")
        lines.append("")
        lines.append(f"Constraints where same agent appears as both beneficiary and victim:")
        lines.append(f"- Low-snare: {overlap.get('low_snare', 0)}")
        lines.append(f"- High-snare: {overlap.get('high_snare', 0)}")
        lines.append("")

    # ==================================================================
    # Section 7: Discriminant ranking
    # ==================================================================
    lines.append("## 7. Discriminant Ranking")
    lines.append("")

    if discriminants:
        lines.append("Features ranked by discriminative power between low-snare and high-snare:")
        lines.append("")
        lines.append("| Rank | Feature | Type | Effect Size | Metric | p-value |")
        lines.append("|-----:|---------|------|------------:|--------|--------:|")
        for i, d in enumerate(discriminants, 1):
            lines.append(f"| {i} | {d['feature']} | {d['type']} | "
                         f"{d['effect_size']:.4f} | {d['metric']} | "
                         f"{d.get('p_value', '—')} |")
        lines.append("")
    else:
        lines.append("No discriminants computed (scipy may be unavailable).")
        lines.append("")

    # ==================================================================
    # Section 8: Spot-check extremes
    # ==================================================================
    lines.append("## 8. Spot-Check Extremes")
    lines.append("")

    for section, label in [("lowest_snare", "10 Lowest P(snare) (Most Rope-Leaning)"),
                           ("highest_snare", "10 Highest P(snare) (Most Snare-Leaning)")]:
        items = spot_checks.get(section, [])
        if items:
            lines.append(f"### {label}")
            lines.append("")
            lines.append("| ID | P(snare) | P(rope) | P(tangled) | Extract | Suppress | Theater | Perspectives |")
            lines.append("|----|--------:|-------:|-----------:|--------:|---------:|--------:|-------------|")
            for item in items:
                persp = item.get("perspectives", {})
                persp_str = "/".join(persp.get(k, "?")
                                     for k in ["powerless", "moderate", "institutional", "analytical"])
                extr = item.get("base_extractiveness", "—")
                supp = item.get("suppression", "—")
                thtr = item.get("theater_ratio", "—")
                lines.append(f"| {item['id'][:40]} | {item['raw_p_snare']:.4f} | "
                             f"{item['raw_p_rope']:.4f} | {item['raw_p_tangled_rope']:.4f} | "
                             f"{extr} | {supp} | {thtr} | {persp_str} |")
            lines.append("")

    # ==================================================================
    # Interpretation
    # ==================================================================
    lines.append("## 9. Interpretation")
    lines.append("")
    lines.append("**Explanation A (Informational Advantage):** The low-snare group reflects")
    lines.append("constraints where non-institutional observers partially agree with the")
    lines.append("institutional rope classification, suggesting the institution may have")
    lines.append("genuine insight. Evidence for A: different orbit signatures, non-institutional")
    lines.append("observers seeing tangled_rope rather than snare.")
    lines.append("")
    lines.append("**Explanation B (Metric Bias):** The low-snare group simply has lower")
    lines.append("extractiveness/suppression/theater metrics, which mechanically reduces")
    lines.append("P(snare). Evidence for B: strong rank-biserial correlations on extraction")
    lines.append("metrics, similar orbit structures between groups.")
    lines.append("")

    # ==================================================================
    # Cross-references
    # ==================================================================
    lines.append("## 10. Cross-References")
    lines.append("")
    lines.append("- **tangled_rope_decomposition_report.md** -- Coalition type definitions and fiber decomposition")
    lines.append("- **boundary_normality_report.md** -- Boundary population distributions (tangled_rope->snare)")
    lines.append("- **boolean_independence_report.md** -- Boolean feature independence from type")
    lines.append("- **classification_confidence_report.md** -- Full-corpus confidence analysis")
    lines.append("- **maxent_report.md** -- MaxEnt shadow classifier analysis")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# JSON output
# ---------------------------------------------------------------------------

def write_json_output(population, metric_results, sig_results, orbit_results,
                      domain_results, bv_results, discriminants, spot_checks):
    """Write aggregate JSON output (no per-constraint arrays)."""
    output = {
        "metadata": {
            "description": "Institutional dissent analysis: low-snare vs high-snare comparison",
            "snare_threshold": SNARE_THRESHOLD,
            "has_scipy": HAS_SCIPY,
            "total": population["total"],
            "low_snare_n": population["low_snare"]["n"],
            "high_snare_n": population["high_snare"]["n"],
        },
        "population": population,
        "metric_comparison": metric_results,
        "signature_comparison": sig_results,
        "orbit_comparison": orbit_results,
        "domain_comparison": domain_results,
        "beneficiary_victim": bv_results,
        "discriminant_ranking": discriminants,
        "spot_check_extremes": spot_checks,
    }

    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2, cls=_SafeEncoder)
    print(f"[DISSENT] Wrote {DATA_PATH}", file=sys.stderr)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[DISSENT] Loading institutional dissent data...", file=sys.stderr)
    low, high, all_dissent = load_institutional_dissent()
    print(f"[DISSENT] Loaded {len(all_dissent)} institutional_dissent constraints.",
          file=sys.stderr)
    print(f"[DISSENT]   Low-snare (P(snare) < {SNARE_THRESHOLD}): {len(low)}",
          file=sys.stderr)
    print(f"[DISSENT]   High-snare (P(snare) >= {SNARE_THRESHOLD}): {len(high)}",
          file=sys.stderr)

    if not all_dissent:
        print("[DISSENT] ERROR: No institutional_dissent constraints found.",
              file=sys.stderr)
        sys.exit(1)

    # Section 1: Population overview
    print("[DISSENT] Section 1: Population overview...", file=sys.stderr)
    population = population_overview(low, high)

    # Section 2: Metric profile comparison
    print("[DISSENT] Section 2: Metric profile comparison...", file=sys.stderr)
    metric_results = metric_profile_comparison(low, high)

    # Section 3: Signature comparison
    print("[DISSENT] Section 3: Signature comparison...", file=sys.stderr)
    sig_results = signature_comparison(low, high)

    # Section 4: Orbit structure comparison
    print("[DISSENT] Section 4: Orbit structure comparison...", file=sys.stderr)
    orbit_results = orbit_structure_comparison(low, high)

    # Section 5: Domain analysis
    print("[DISSENT] Section 5: Domain analysis...", file=sys.stderr)
    domain_results = domain_analysis(low, high)

    # Section 6: Beneficiary/victim analysis
    print("[DISSENT] Section 6: Beneficiary/victim analysis...", file=sys.stderr)
    bv_results = beneficiary_victim_analysis(low, high)

    # Section 7: Discriminant ranking
    print("[DISSENT] Section 7: Discriminant ranking...", file=sys.stderr)
    discriminants = rank_discriminants(metric_results, sig_results,
                                       orbit_results, domain_results)

    # Section 8: Spot-check extremes
    print("[DISSENT] Section 8: Spot-check extremes...", file=sys.stderr)
    spot_checks = spot_check_extremes(all_dissent)

    # Generate report
    print("[DISSENT] Generating report...", file=sys.stderr)
    report = generate_report(population, metric_results, sig_results,
                             orbit_results, domain_results, bv_results,
                             discriminants, spot_checks)

    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"[DISSENT] Wrote {REPORT_PATH}", file=sys.stderr)

    # Write JSON
    print("[DISSENT] Writing JSON output...", file=sys.stderr)
    write_json_output(population, metric_results, sig_results, orbit_results,
                      domain_results, bv_results, discriminants, spot_checks)

    print("[DISSENT] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
