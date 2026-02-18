#!/usr/bin/env python3
"""Boundary Population Distribution Analysis (v1.0)

Tests whether the raw (pre-override) P(rival) distribution within a single
boundary zone is normally distributed. This confirms whether the type boundary
cuts through a continuous population, justifying the gradient/fiber decomposition
approach.

Imports MaxEnt functions from tangled_decomposition.py rather than duplicating.

Outputs:
  outputs/boundary_normality_data.json  - per-boundary test results
  outputs/boundary_normality_report.md  - analysis report

Usage:
  python3 python/boundary_normality.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from pathlib import Path

from tangled_decomposition import (
    load_all_data, maxent_classify, MAXENT_TYPES,
    load_json, PIPELINE_JSON,
)

# ---------------------------------------------------------------------------
# Optional dependencies
# ---------------------------------------------------------------------------

HAS_SCIPY = False
try:
    import scipy.stats
    HAS_SCIPY = True
except ImportError:
    print("Warning: scipy not available. Skipping formal statistical tests.", file=sys.stderr)

HAS_SKLEARN = False
try:
    from sklearn.mixture import GaussianMixture
    HAS_SKLEARN = True
except ImportError:
    pass  # noted in report if needed

HAS_STATSMODELS = False
try:
    from statsmodels.stats.diagnostic import lilliefors
    HAS_STATSMODELS = True
except ImportError:
    pass

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
OUTPUT_DIR = ROOT_DIR / "outputs"

BOUNDARY_JSON = OUTPUT_DIR / "boundary_normality_data.json"
BOUNDARY_REPORT = OUTPUT_DIR / "boundary_normality_report.md"


class _SafeEncoder(json.JSONEncoder):
    """Handle numpy types that aren't JSON-serializable."""
    def default(self, obj):
        # numpy bool_, int64, float64 etc.
        if hasattr(obj, 'item'):
            return obj.item()
        return super().default(obj)

# Signatures that receive a conditional 3x boost on tangled_rope
OVERRIDE_AFFECTED_SIGS = {
    "false_ci_rope", "constructed_high_extraction", "constructed_constraint",
}

COALITION_TYPES = ["uniform_tangled", "institutional_dissent", "analytical_dissent", "split_field", "other"]
SNARE_CLUSTER_THRESHOLD = 0.5

# ---------------------------------------------------------------------------
# Step 1: Identify boundary populations from raw distributions
# ---------------------------------------------------------------------------

def identify_boundary_populations(constraints, dists_raw):
    """Group constraints by boundary (claimed_type -> rival_type) using raw distributions.

    Returns dict: boundary_string -> list of {id, claimed_type, rival_type, raw_rival_prob, signature}.
    """
    boundaries = defaultdict(list)

    for cid, c in constraints.items():
        claimed = c.get("claimed_type")
        dist = dists_raw.get(cid, {})
        if not claimed or not dist:
            continue

        # Find rival: argmax excluding claimed_type
        rival_type = None
        rival_prob = -1.0
        for t, p in dist.items():
            if t != claimed and p > rival_prob:
                rival_type = t
                rival_prob = p

        if rival_type is None:
            continue

        boundary = f"{claimed}->{rival_type}"
        boundaries[boundary].append({
            "id": cid,
            "claimed_type": claimed,
            "rival_type": rival_type,
            "raw_rival_prob": rival_prob,
            "signature": c.get("signature"),
        })

    return boundaries


# ---------------------------------------------------------------------------
# Step 2: Statistical normality tests
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


def normality_tests(values):
    """Run normality tests on a list of values. Returns dict of test results."""
    results = {}
    n = len(values)

    if not HAS_SCIPY or n < 8:
        return results

    # Shapiro-Wilk (best for n < 5000)
    if n >= 8:
        try:
            stat, pval = scipy.stats.shapiro(values)
            results["shapiro_wilk"] = {
                "statistic": round(stat, 6),
                "p_value": round(pval, 6),
                "reject_at_005": pval < 0.05,
            }
        except Exception:
            pass

    # D'Agostino-Pearson (requires n >= 20)
    if n >= 20:
        try:
            stat, pval = scipy.stats.normaltest(values)
            results["dagostino_pearson"] = {
                "statistic": round(stat, 6),
                "p_value": round(pval, 6),
                "reject_at_005": pval < 0.05,
            }
        except Exception:
            pass

    # Anderson-Darling
    try:
        ad_result = scipy.stats.anderson(values, dist='norm')
        # Compare statistic against critical values at 5% level
        crit_5pct = ad_result.critical_values[2] if len(ad_result.critical_values) > 2 else None
        results["anderson_darling"] = {
            "statistic": round(ad_result.statistic, 6),
            "critical_values": {
                f"{sig}%": round(cv, 6)
                for sig, cv in zip(ad_result.significance_level, ad_result.critical_values)
            },
            "reject_at_005": ad_result.statistic > crit_5pct if crit_5pct else None,
        }
    except Exception:
        pass

    # Skewness and kurtosis
    try:
        skew = scipy.stats.skew(values)
        kurt = scipy.stats.kurtosis(values)
        results["skewness"] = round(float(skew), 6)
        results["kurtosis"] = round(float(kurt), 6)
    except Exception:
        pass

    # Lilliefors (if statsmodels available)
    if HAS_STATSMODELS and n >= 8:
        try:
            stat, pval = lilliefors(values, dist='norm')
            results["lilliefors"] = {
                "statistic": round(stat, 6),
                "p_value": round(pval, 6),
                "reject_at_005": pval < 0.05,
            }
        except Exception:
            pass

    return results


# ---------------------------------------------------------------------------
# Step 3: Alternative distribution fitting
# ---------------------------------------------------------------------------

def fit_alternative_distributions(values):
    """Fit alternative distributions and compare via AIC."""
    results = {}
    n = len(values)

    if not HAS_SCIPY or n < 10:
        return results

    # Single normal
    try:
        mu, sigma = scipy.stats.norm.fit(values)
        ll = sum(scipy.stats.norm.logpdf(v, mu, sigma) for v in values)
        aic = 2 * 2 - 2 * ll  # k=2 (mu, sigma)
        results["normal"] = {
            "params": {"mu": round(mu, 6), "sigma": round(sigma, 6)},
            "log_likelihood": round(ll, 4),
            "aic": round(aic, 4),
            "k": 2,
        }
    except Exception:
        pass

    # Beta
    try:
        # Clip to (0, 1) open interval for beta fitting
        clipped = [max(1e-6, min(1 - 1e-6, v)) for v in values]
        a, b, loc, scale = scipy.stats.beta.fit(clipped, floc=0, fscale=1)
        ll = sum(scipy.stats.beta.logpdf(v, a, b, loc=0, scale=1) for v in clipped)
        aic = 2 * 2 - 2 * ll  # k=2 (a, b)
        results["beta"] = {
            "params": {"a": round(a, 6), "b": round(b, 6)},
            "log_likelihood": round(ll, 4),
            "aic": round(aic, 4),
            "k": 2,
        }
    except Exception:
        pass

    # Uniform on [min, max]
    try:
        lo, hi = min(values), max(values)
        span = hi - lo
        if span > 1e-10:
            ll = -n * math.log(span)
            aic = 2 * 2 - 2 * ll  # k=2 (lo, hi)
            results["uniform"] = {
                "params": {"lo": round(lo, 6), "hi": round(hi, 6)},
                "log_likelihood": round(ll, 4),
                "aic": round(aic, 4),
                "k": 2,
            }
    except Exception:
        pass

    # Gaussian mixture (2-component)
    if HAS_SKLEARN and n >= 20:
        try:
            import numpy as np
            X = np.array(values).reshape(-1, 1)
            gm = GaussianMixture(n_components=2, random_state=42)
            gm.fit(X)
            ll = gm.score(X) * n  # score returns per-sample avg log-likelihood
            k = 5  # 2 means + 2 variances + 1 mixing weight
            aic = 2 * k - 2 * ll
            bic = k * math.log(n) - 2 * ll
            results["gaussian_mixture_2"] = {
                "params": {
                    "means": [round(float(m), 6) for m in gm.means_.flatten()],
                    "variances": [round(float(v), 6) for v in gm.covariances_.flatten()],
                    "weights": [round(float(w), 6) for w in gm.weights_],
                },
                "log_likelihood": round(ll, 4),
                "aic": round(aic, 4),
                "bic": round(bic, 4),
                "k": k,
            }
        except Exception:
            pass

    return results


# ---------------------------------------------------------------------------
# Step 4: QQ table
# ---------------------------------------------------------------------------

def compute_qq_table(values):
    """Compute QQ table at deciles: expected normal quantile vs observed."""
    n = len(values)
    if n < 10:
        return []

    s = sorted(values)
    mean = sum(s) / n
    variance = sum((x - mean) ** 2 for x in s) / n
    std = math.sqrt(variance) if variance > 0 else 1e-6

    table = []
    for pct in range(10, 100, 10):
        # Observed quantile
        idx = int(n * pct / 100)
        idx = min(idx, n - 1)
        observed = s[idx]

        # Expected normal quantile
        if HAS_SCIPY:
            expected_z = float(scipy.stats.norm.ppf(pct / 100.0))
        else:
            # Rough approximation for common quantiles
            z_table = {10: -1.2816, 20: -0.8416, 30: -0.5244,
                       40: -0.2533, 50: 0.0, 60: 0.2533,
                       70: 0.5244, 80: 0.8416, 90: 1.2816}
            expected_z = z_table.get(pct, 0.0)

        expected = mean + expected_z * std

        table.append({
            "percentile": pct,
            "expected": round(expected, 6),
            "observed": round(observed, 6),
            "deviation": round(observed - expected, 6),
        })

    return table


# ---------------------------------------------------------------------------
# Step 5: Override sub-population analysis (tangled_rope→snare only)
# ---------------------------------------------------------------------------

def override_subpopulation_analysis(members):
    """Split tangled_rope→snare into override-affected and non-override-affected."""
    affected = []
    unaffected = []

    for m in members:
        sig = m.get("signature") or ""
        if sig in OVERRIDE_AFFECTED_SIGS:
            affected.append(m)
        else:
            unaffected.append(m)

    result = {
        "affected_count": len(affected),
        "unaffected_count": len(unaffected),
        "affected": {},
        "unaffected": {},
    }

    for label, group in [("affected", affected), ("unaffected", unaffected)]:
        if not group:
            continue
        values = [m["raw_rival_prob"] for m in group]
        result[label]["descriptive"] = descriptive_stats(values)
        result[label]["normality_tests"] = normality_tests(values)
        result[label]["signatures"] = dict(Counter(m.get("signature") or "none" for m in group))

    return result


# ---------------------------------------------------------------------------
# Step 6: Coalition Type × Snare Cluster Cross-Tabulation
# ---------------------------------------------------------------------------

def load_coalition_map():
    """Load coalition_type from pipeline_output.json per_constraint entries."""
    data = load_json(PIPELINE_JSON, "pipeline_output")
    if not data:
        return {}
    cmap = {}
    for entry in data.get("per_constraint", []):
        cid = entry.get("id")
        ctype = entry.get("coalition_type")
        if cid and ctype:
            cmap[cid] = ctype
    return cmap


def coalition_snare_crosstab(members, coalition_map):
    """Cross-tabulate coalition type × snare cluster for tangled_rope→snare members.

    members: list of dicts from identify_boundary_populations (each has 'id', 'raw_rival_prob')
    coalition_map: {constraint_id: coalition_type} from load_coalition_map()

    Returns dict with matched/unmatched counts, cross_tab, per_coalition stats,
    chi_squared result, and cluster_totals.
    """
    if not coalition_map:
        return {"error": "decomposition data not available"}

    matched = []
    unmatched_ids = []

    for m in members:
        cid = m["id"]
        ctype = coalition_map.get(cid)
        if ctype is None:
            unmatched_ids.append(cid)
        else:
            matched.append({
                "id": cid,
                "coalition_type": ctype,
                "raw_rival_prob": m["raw_rival_prob"],
                "cluster": "high_snare" if m["raw_rival_prob"] >= SNARE_CLUSTER_THRESHOLD else "low_snare",
            })

    # Build contingency table: coalition_type -> {low_snare, high_snare}
    cross_tab = {}
    for ct in COALITION_TYPES:
        cross_tab[ct] = {"low_snare": 0, "high_snare": 0}
    for m in matched:
        ct = m["coalition_type"]
        if ct not in cross_tab:
            cross_tab[ct] = {"low_snare": 0, "high_snare": 0}
        cross_tab[ct][m["cluster"]] += 1

    # Remove empty rows
    cross_tab = {ct: counts for ct, counts in cross_tab.items()
                 if counts["low_snare"] + counts["high_snare"] > 0}

    # Cluster totals
    total_low = sum(c["low_snare"] for c in cross_tab.values())
    total_high = sum(c["high_snare"] for c in cross_tab.values())

    # Per-coalition descriptive stats
    per_coalition = {}
    for ct in cross_tab:
        vals = [m["raw_rival_prob"] for m in matched if m["coalition_type"] == ct]
        if vals:
            per_coalition[ct] = descriptive_stats(vals)

    # Chi-squared test of independence
    chi_squared = None
    if HAS_SCIPY and len(cross_tab) >= 2:
        # Build matrix: rows = coalition types, cols = [low_snare, high_snare]
        ordered_types = [ct for ct in COALITION_TYPES if ct in cross_tab]
        observed = []
        for ct in ordered_types:
            observed.append([cross_tab[ct]["low_snare"], cross_tab[ct]["high_snare"]])
        try:
            chi2, p, dof, expected = scipy.stats.chi2_contingency(observed)
            chi_squared = {
                "chi2": round(float(chi2), 4),
                "p_value": round(float(p), 6),
                "dof": int(dof),
                "reject_at_005": float(p) < 0.05,
            }
        except Exception as e:
            chi_squared = {"error": str(e)}
    elif not HAS_SCIPY:
        chi_squared = {"error": "scipy not available"}

    return {
        "matched_count": len(matched),
        "unmatched_count": len(unmatched_ids),
        "unmatched_ids": unmatched_ids[:20],  # cap for JSON size
        "cluster_totals": {"low_snare": total_low, "high_snare": total_high},
        "cross_tab": cross_tab,
        "per_coalition": per_coalition,
        "chi_squared": chi_squared,
    }


# ---------------------------------------------------------------------------
# Step 7: Report generation
# ---------------------------------------------------------------------------

def make_ascii_histogram(values, bins=20, width=50, lo=None, hi=None):
    """Create ASCII histogram."""
    if not values:
        return "  (no data)\n"
    if lo is None:
        lo = min(values)
    if hi is None:
        hi = max(values)
    # Ensure range isn't zero
    if hi - lo < 1e-10:
        lo = lo - 0.05
        hi = hi + 0.05
    bin_width = (hi - lo) / bins
    counts = [0] * bins
    for v in values:
        idx = min(int((v - lo) / bin_width), bins - 1)
        idx = max(0, idx)
        counts[idx] += 1
    max_count = max(counts) if counts else 1
    lines = []
    for i, cnt in enumerate(counts):
        lo_edge = lo + i * bin_width
        hi_edge = lo_edge + bin_width
        bar_len = int(cnt / max_count * width) if max_count > 0 else 0
        bar = "#" * bar_len
        lines.append(f"  {lo_edge:5.3f}-{hi_edge:5.3f} | {bar} ({cnt})")
    return "\n".join(lines)


def format_test_result(test_name, result):
    """Format a single test result as a report line."""
    if isinstance(result, dict):
        stat = result.get("statistic", "?")
        pval = result.get("p_value", "?")
        reject = result.get("reject_at_005", None)
        verdict = "REJECT" if reject else ("fail to reject" if reject is not None else "?")
        return f"  {test_name}: statistic={stat}, p={pval} -> {verdict} H0(normal)"
    return f"  {test_name}: {result}"


def generate_report(boundaries, boundary_results, override_sub, coalition_crosstab=None):
    """Generate the full boundary normality report."""
    lines = []
    lines.append("# Boundary Population Distribution Analysis Report")
    lines.append("")
    lines.append("*Generated by `python/boundary_normality.py` — tests whether raw")
    lines.append("(pre-override) P(rival) distributions within boundary zones are normally")
    lines.append("distributed, confirming whether type boundaries cut through continuous")
    lines.append("populations.*")
    lines.append("")

    # --- Section 1: Summary ---
    lines.append("## 1. Summary")
    lines.append("")

    if not HAS_SCIPY:
        lines.append("**Note:** scipy not available. Only descriptive statistics and histograms")
        lines.append("are provided. Install scipy for formal normality tests.")
        lines.append("")

    # Table of all boundaries
    sorted_bnds = sorted(boundaries.items(), key=lambda x: -len(x[1]))
    lines.append("| Boundary | N | Mean P(rival) | Std | Min | Max |")
    lines.append("|----------|--:|-------------:|----:|----:|----:|")
    for bnd, members in sorted_bnds:
        if len(members) < 5:
            continue
        vals = [m["raw_rival_prob"] for m in members]
        stats = descriptive_stats(vals)
        lines.append(f"| {bnd} | {stats['n']} | {stats['mean']:.4f} | "
                     f"{stats['std']:.4f} | {stats['min']:.4f} | {stats['max']:.4f} |")
    lines.append("")

    # Normality verdict summary
    if HAS_SCIPY:
        lines.append("### Normality Test Summary")
        lines.append("")
        lines.append("| Boundary | N | Shapiro-Wilk p | D'Agostino p | Anderson stat | Verdict |")
        lines.append("|----------|--:|---------------:|-------------:|--------------:|---------|")
        for bnd, res in sorted(boundary_results.items(), key=lambda x: -x[1].get("descriptive", {}).get("n", 0)):
            n = res.get("descriptive", {}).get("n", 0)
            if n < 20:
                continue
            tests = res.get("normality_tests", {})
            sw_p = tests.get("shapiro_wilk", {}).get("p_value", "—")
            da_p = tests.get("dagostino_pearson", {}).get("p_value", "—")
            ad_stat = tests.get("anderson_darling", {}).get("statistic", "—")

            # Overall verdict: reject if any test rejects
            any_reject = False
            for test_name in ["shapiro_wilk", "dagostino_pearson", "anderson_darling"]:
                t = tests.get(test_name, {})
                if t.get("reject_at_005"):
                    any_reject = True
            verdict = "**NOT NORMAL**" if any_reject else "consistent w/ normal"
            lines.append(f"| {bnd} | {n} | {sw_p} | {da_p} | {ad_stat} | {verdict} |")
        lines.append("")

    # --- Section 2: Per-boundary detail ---
    lines.append("## 2. Per-Boundary Detail")
    lines.append("")

    for bnd, members in sorted_bnds:
        n = len(members)
        if n < 5:
            continue

        res = boundary_results.get(bnd, {})
        vals = [m["raw_rival_prob"] for m in members]
        stats = res.get("descriptive", descriptive_stats(vals))

        lines.append(f"### {bnd} (N={n})")
        lines.append("")

        # Descriptive stats table
        lines.append("| Statistic | Value |")
        lines.append("|-----------|------:|")
        for key in ["n", "mean", "median", "std", "min", "max", "q25", "q75", "iqr"]:
            if key in stats:
                lines.append(f"| {key} | {stats[key]} |")

        skew = res.get("normality_tests", {}).get("skewness")
        kurt = res.get("normality_tests", {}).get("kurtosis")
        if skew is not None:
            lines.append(f"| skewness | {skew} |")
        if kurt is not None:
            lines.append(f"| kurtosis (excess) | {kurt} |")
        lines.append("")

        # Signature breakdown
        sig_counts = Counter(m.get("signature") or "none" for m in members)
        lines.append("**Signature breakdown:**")
        lines.append("")
        lines.append("| Signature | Count | % |")
        lines.append("|-----------|------:|----:|")
        for sig, cnt in sig_counts.most_common():
            pct = 100 * cnt / n
            lines.append(f"| {sig} | {cnt} | {pct:.1f}% |")
        lines.append("")

        # Histogram
        lines.append("**Raw P(rival) distribution:**")
        lines.append("")
        lines.append("```")
        lines.append(make_ascii_histogram(vals, bins=min(20, max(10, n // 20)), width=50,
                                          lo=0.0, hi=max(0.5, max(vals) + 0.02)))
        lines.append("```")
        lines.append("")

        # Normality test details
        tests = res.get("normality_tests", {})
        if tests:
            lines.append("**Normality tests:**")
            lines.append("")
            for test_name in ["shapiro_wilk", "dagostino_pearson", "anderson_darling", "lilliefors"]:
                t = tests.get(test_name)
                if t:
                    lines.append(format_test_result(test_name, t))
            lines.append("")

        # Alternative distribution fits
        fits = res.get("distribution_fits", {})
        if fits:
            lines.append("**Distribution fitting (AIC comparison):**")
            lines.append("")
            lines.append("| Distribution | AIC | Log-Likelihood | Params |")
            lines.append("|-------------|----:|---------------:|--------|")
            for dist_name in sorted(fits.keys(), key=lambda d: fits[d].get("aic", 1e10)):
                f = fits[dist_name]
                params_str = ", ".join(f"{k}={v}" for k, v in f.get("params", {}).items())
                lines.append(f"| {dist_name} | {f.get('aic', '?')} | {f.get('log_likelihood', '?')} | {params_str} |")
            lines.append("")

            # Best fit verdict
            best = min(fits.items(), key=lambda x: x[1].get("aic", 1e10))
            lines.append(f"  Best fit by AIC: **{best[0]}** (AIC={best[1].get('aic', '?')})")
            lines.append("")

    # --- Section 3: QQ tables ---
    lines.append("## 3. QQ Tables (Expected Normal vs Observed)")
    lines.append("")

    for bnd, res in sorted(boundary_results.items(), key=lambda x: -x[1].get("descriptive", {}).get("n", 0)):
        qq = res.get("qq_table", [])
        if not qq:
            continue
        n = res.get("descriptive", {}).get("n", 0)
        lines.append(f"### {bnd} (N={n})")
        lines.append("")
        lines.append("| Percentile | Expected | Observed | Deviation |")
        lines.append("|-----------:|---------:|---------:|----------:|")
        for row in qq:
            lines.append(f"| {row['percentile']}th | {row['expected']:.4f} | "
                         f"{row['observed']:.4f} | {row['deviation']:+.4f} |")
        lines.append("")

    # --- Section 4: Override sub-analysis ---
    lines.append("## 4. Override Sub-Population Analysis (tangled_rope->snare)")
    lines.append("")

    if override_sub:
        lines.append(f"Override-affected signatures: `{', '.join(sorted(OVERRIDE_AFFECTED_SIGS))}`")
        lines.append(f"(These receive a conditional 3x boost on tangled_rope when overrides are on)")
        lines.append("")
        lines.append(f"- Override-affected: **{override_sub['affected_count']}** constraints")
        lines.append(f"- Non-override-affected: **{override_sub['unaffected_count']}** constraints")
        lines.append("")

        for label, title in [("affected", "Override-Affected Sub-Population"),
                             ("unaffected", "Non-Override-Affected Sub-Population")]:
            sub = override_sub.get(label, {})
            if not sub:
                continue
            desc = sub.get("descriptive", {})
            tests = sub.get("normality_tests", {})
            sigs = sub.get("signatures", {})

            lines.append(f"### {title} (N={desc.get('n', 0)})")
            lines.append("")

            # Signatures in this sub-group
            lines.append("| Signature | Count |")
            lines.append("|-----------|------:|")
            for sig, cnt in sorted(sigs.items(), key=lambda x: -x[1]):
                lines.append(f"| {sig} | {cnt} |")
            lines.append("")

            # Descriptive
            lines.append("| Statistic | Value |")
            lines.append("|-----------|------:|")
            for key in ["n", "mean", "median", "std", "min", "max", "iqr"]:
                if key in desc:
                    lines.append(f"| {key} | {desc[key]} |")
            lines.append("")

            # Normality tests
            if tests:
                lines.append("**Normality tests:**")
                lines.append("")
                for test_name in ["shapiro_wilk", "dagostino_pearson", "anderson_darling", "lilliefors"]:
                    t = tests.get(test_name)
                    if t:
                        lines.append(format_test_result(test_name, t))
                skew = tests.get("skewness")
                kurt = tests.get("kurtosis")
                if skew is not None:
                    lines.append(f"  skewness: {skew}")
                if kurt is not None:
                    lines.append(f"  kurtosis (excess): {kurt}")
                lines.append("")
    else:
        lines.append("(tangled_rope->snare boundary not found in raw distributions)")
        lines.append("")

    # --- Section 5: Cross-boundary comparison ---
    lines.append("## 5. Cross-Boundary Comparison")
    lines.append("")

    # Compare spread (std) across boundaries
    comparison_data = []
    for bnd, res in boundary_results.items():
        desc = res.get("descriptive", {})
        n = desc.get("n", 0)
        if n < 20:
            continue
        tests = res.get("normality_tests", {})
        any_reject = False
        for test_name in ["shapiro_wilk", "dagostino_pearson", "anderson_darling"]:
            if tests.get(test_name, {}).get("reject_at_005"):
                any_reject = True
        best_fit = "—"
        fits = res.get("distribution_fits", {})
        if fits:
            best_fit = min(fits.items(), key=lambda x: x[1].get("aic", 1e10))[0]
        comparison_data.append((bnd, n, desc.get("mean", 0), desc.get("std", 0),
                                tests.get("skewness", "—"), any_reject, best_fit))

    if comparison_data:
        lines.append("| Boundary | N | Mean | Std | Skewness | Normal? | Best Fit |")
        lines.append("|----------|--:|-----:|----:|---------:|---------|----------|")
        for bnd, n, mean, std, skew, reject, best in sorted(comparison_data, key=lambda x: -x[1]):
            normal = "NO" if reject else "yes"
            lines.append(f"| {bnd} | {n} | {mean:.4f} | {std:.4f} | {skew} | {normal} | {best} |")
        lines.append("")
    else:
        lines.append("(no boundaries with N >= 20)")
        lines.append("")

    # --- Section 6: Coalition Type × Snare Cluster Cross-Tabulation ---
    lines.append("## 6. Coalition Type × Snare Cluster Cross-Tabulation (tangled_rope→snare)")
    lines.append("")

    if coalition_crosstab and "error" not in coalition_crosstab:
        ct = coalition_crosstab
        lines.append(f"Matched {ct['matched_count']} of {ct['matched_count'] + ct['unmatched_count']} "
                     f"tangled_rope→snare members to coalition types "
                     f"({ct['unmatched_count']} unmatched — different rival type in decomposition).")
        lines.append(f"Cluster threshold: P(snare) {'≥' if True else '>'} {SNARE_CLUSTER_THRESHOLD} = high-snare, "
                     f"P(snare) < {SNARE_CLUSTER_THRESHOLD} = low-snare.")
        lines.append("")

        # Contingency table
        cross = ct["cross_tab"]
        totals = ct["cluster_totals"]
        lines.append("### Contingency Table")
        lines.append("")
        lines.append("| Coalition Type | Low-Snare | High-Snare | Total | % High-Snare |")
        lines.append("|----------------|----------:|-----------:|------:|-------------:|")
        for ctype in COALITION_TYPES:
            if ctype not in cross:
                continue
            lo = cross[ctype]["low_snare"]
            hi = cross[ctype]["high_snare"]
            total = lo + hi
            pct_hi = (100.0 * hi / total) if total > 0 else 0.0
            lines.append(f"| {ctype} | {lo} | {hi} | {total} | {pct_hi:.1f}% |")
        total_all = totals["low_snare"] + totals["high_snare"]
        pct_all = (100.0 * totals["high_snare"] / total_all) if total_all > 0 else 0.0
        lines.append(f"| **Total** | **{totals['low_snare']}** | **{totals['high_snare']}** "
                     f"| **{total_all}** | **{pct_all:.1f}%** |")
        lines.append("")

        # Per-coalition descriptive stats
        per_c = ct.get("per_coalition", {})
        if per_c:
            lines.append("### Per-Coalition Descriptive Statistics")
            lines.append("")
            lines.append("| Coalition Type | N | Mean | Median | Std | Min | Max |")
            lines.append("|----------------|--:|-----:|-------:|----:|----:|----:|")
            for ctype in COALITION_TYPES:
                s = per_c.get(ctype)
                if not s:
                    continue
                lines.append(f"| {ctype} | {s['n']} | {s['mean']:.4f} | {s['median']:.4f} | "
                             f"{s['std']:.4f} | {s['min']:.4f} | {s['max']:.4f} |")
            lines.append("")

        # Chi-squared result
        chi = ct.get("chi_squared")
        if chi and "error" not in chi:
            lines.append("### Chi-Squared Test of Independence")
            lines.append("")
            lines.append(f"- H0: Coalition type and snare cluster are independent")
            lines.append(f"- Chi² = {chi['chi2']}, df = {chi['dof']}, p = {chi['p_value']}")
            reject = chi.get("reject_at_005", False)
            if reject:
                lines.append(f"- **Result: REJECT H0 at α=0.05** — coalition type and snare cluster are NOT independent")
            else:
                lines.append(f"- Result: Fail to reject H0 at α=0.05 — no significant association detected")
            lines.append("")

            # Interpretation
            lines.append("**Interpretation:**")
            if reject:
                lines.append("The bimodal snare structure (low-snare vs high-snare clusters) is significantly "
                             "associated with coalition type. Different coalition types have different propensities "
                             "to fall into the low-snare vs high-snare cluster, confirming that the U-shaped "
                             "Beta(0.64, 0.47) distribution reflects underlying coalition structure rather than "
                             "random variation.")
            else:
                lines.append("No significant association was found between coalition type and snare cluster "
                             "membership. The bimodal structure may arise from a mechanism other than coalition "
                             "type (e.g., signature overrides).")
            lines.append("")
        elif chi and "error" in chi:
            lines.append(f"*Chi-squared test not available: {chi['error']}*")
            lines.append("")

        # Unmatched note
        if ct["unmatched_count"] > 0:
            lines.append(f"*Note: {ct['unmatched_count']} tangled_rope→snare constraints could not be matched "
                         f"to the decomposition (they appear in the decomposition with a different rival type). "
                         f"Analysis covers {ct['matched_count']} matched constraints.*")
            lines.append("")
    elif coalition_crosstab and "error" in coalition_crosstab:
        lines.append(f"*Coalition cross-tabulation not available: {coalition_crosstab['error']}*")
        lines.append("")
    else:
        lines.append("*(Coalition cross-tabulation not computed)*")
        lines.append("")

    # --- Section 7: Cross-references ---
    lines.append("## 7. Cross-References")
    lines.append("")
    lines.append("- **classification_confidence_report.md** — Full-corpus confidence analysis")
    lines.append("  (bimodal U-shaped distribution driven by signature overrides)")
    lines.append("- **tangled_rope_decomposition_report.md** — Fiber decomposition with psi metric")
    lines.append("- **tangled_decomposition_data.json** — Coalition type assignments per tangled_rope constraint")
    lines.append("- **maxent_report.md** — MaxEnt shadow classifier hard disagreements")
    lines.append("- **maxent_diagnostic_report.md** — MaxEnt diagnostic details")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[BOUNDARY] Loading data...", file=sys.stderr)
    constraints = load_all_data()
    print(f"[BOUNDARY] Loaded {len(constraints)} constraints.", file=sys.stderr)

    # --- Read raw distributions from pipeline_output.json ---
    print("[BOUNDARY] Reading raw distributions from pipeline_output.json...", file=sys.stderr)
    pipeline_raw = load_json(PIPELINE_JSON, "pipeline_output")
    per_constraint = pipeline_raw.get("per_constraint", [])

    dists_raw = {}
    for entry in per_constraint:
        cid = entry.get("id")
        raw_probs = entry.get("raw_maxent_probs")
        if raw_probs and isinstance(raw_probs, dict):
            dists_raw[cid] = raw_probs

    # Fall back to Python replication only if pipeline lacks raw distributions
    if not dists_raw:
        print("[BOUNDARY] No raw_maxent_probs in pipeline_output.json, computing via Python replication.", file=sys.stderr)
        dists_raw = maxent_classify(constraints, apply_overrides=False)

    print(f"[BOUNDARY] Raw distributions: {len(dists_raw)}.", file=sys.stderr)

    # --- Step 1: Identify boundary populations ---
    print("[BOUNDARY] Identifying boundary populations...", file=sys.stderr)
    boundaries = identify_boundary_populations(constraints, dists_raw)

    # Report boundary sizes
    for bnd, members in sorted(boundaries.items(), key=lambda x: -len(x[1])):
        if len(members) >= 10:
            print(f"[BOUNDARY]   {bnd}: {len(members)} constraints", file=sys.stderr)

    # --- Steps 2-4: Analyze each boundary ---
    print("[BOUNDARY] Running statistical tests...", file=sys.stderr)
    boundary_results = {}

    for bnd, members in boundaries.items():
        vals = [m["raw_rival_prob"] for m in members]
        result = {
            "descriptive": descriptive_stats(vals),
        }

        if len(vals) >= 20:
            result["normality_tests"] = normality_tests(vals)
            result["distribution_fits"] = fit_alternative_distributions(vals)
        elif len(vals) >= 8:
            result["normality_tests"] = normality_tests(vals)

        result["qq_table"] = compute_qq_table(vals)

        boundary_results[bnd] = result

    # --- Step 5: Override sub-population analysis ---
    override_sub = None
    tr_snare_key = "tangled_rope->snare"
    if tr_snare_key in boundaries:
        print(f"[BOUNDARY] Running override sub-analysis for {tr_snare_key}...", file=sys.stderr)
        override_sub = override_subpopulation_analysis(boundaries[tr_snare_key])
        print(f"[BOUNDARY]   Override-affected: {override_sub['affected_count']}, "
              f"Non-affected: {override_sub['unaffected_count']}", file=sys.stderr)

    # --- Step 6: Coalition × snare cluster cross-tabulation ---
    coalition_crosstab = None
    if tr_snare_key in boundaries:
        print(f"[BOUNDARY] Loading coalition map for cross-tabulation...", file=sys.stderr)
        coalition_map = load_coalition_map()
        if coalition_map:
            print(f"[BOUNDARY]   Coalition map: {len(coalition_map)} entries", file=sys.stderr)
            coalition_crosstab = coalition_snare_crosstab(boundaries[tr_snare_key], coalition_map)
            print(f"[BOUNDARY]   Matched: {coalition_crosstab.get('matched_count', 0)}, "
                  f"Unmatched: {coalition_crosstab.get('unmatched_count', 0)}", file=sys.stderr)
        else:
            print("[BOUNDARY]   Coalition map empty — skipping cross-tabulation", file=sys.stderr)

    # --- Write JSON ---
    print("[BOUNDARY] Writing JSON output...", file=sys.stderr)
    output_data = {
        "metadata": {
            "total_constraints": len(constraints),
            "total_with_boundary": sum(len(m) for m in boundaries.values()),
            "has_scipy": HAS_SCIPY,
            "has_sklearn": HAS_SKLEARN,
            "has_statsmodels": HAS_STATSMODELS,
        },
        "boundaries": {},
    }

    for bnd, members in boundaries.items():
        res = boundary_results.get(bnd, {})
        bnd_entry = {
            "count": len(members),
            "descriptive": res.get("descriptive", {}),
            "normality_tests": res.get("normality_tests", {}),
            "distribution_fits": res.get("distribution_fits", {}),
            "qq_table": res.get("qq_table", []),
            "member_ids": [m["id"] for m in members],
        }
        output_data["boundaries"][bnd] = bnd_entry

    if override_sub:
        output_data["override_subpopulation"] = override_sub

    if coalition_crosstab:
        output_data["coalition_snare_crosstab"] = coalition_crosstab

    with open(BOUNDARY_JSON, "w", encoding="utf-8") as f:
        json.dump(output_data, f, indent=2, cls=_SafeEncoder)
    print(f"[BOUNDARY] Wrote {BOUNDARY_JSON}", file=sys.stderr)

    # --- Generate report ---
    print("[BOUNDARY] Generating report...", file=sys.stderr)
    report = generate_report(boundaries, boundary_results, override_sub, coalition_crosstab)
    with open(BOUNDARY_REPORT, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"[BOUNDARY] Wrote {BOUNDARY_REPORT}", file=sys.stderr)

    print("[BOUNDARY] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
