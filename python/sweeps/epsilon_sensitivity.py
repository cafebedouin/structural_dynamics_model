"""Epsilon Sensitivity Diagnostic — Fisher Information of MaxEnt w.r.t. ε.

Computes per-constraint Fisher information I(ε) and total variation sensitivity
for the MaxEnt probability distribution, measuring how rapidly the generative
model's type assignments change under small perturbations to base extractiveness.

Designed for the Arakelov "uncertainty route" constraints (H¹=0, high Arakelov
height, Nash distance = 0) where the consensus manifold masks genuine extraction-
boundary uncertainty.

Fisher information is computed on RAW (pre-override) distributions because
unconditional signature overrides (e.g., false_natural_law → 0.95 tangled_rope)
produce constant post-override distributions with zero ε sensitivity.

Usage:
    python3 python/epsilon_sensitivity.py [--h 0.01] [--all]
"""

import json
import math
import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

try:
    from scipy import stats as _scipy_stats
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

from shared.loader import load_json, PIPELINE_JSON, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.maxent import (
    compute_profiles, compute_priors, gaussian_ll, boolean_ll,
    log_sum_exp_normalize, apply_signature_override,
)
from shared.constants import MAXENT_TYPES

# ---------------------------------------------------------------------------
# Core: single-constraint MaxEnt at a specific extractiveness value
# ---------------------------------------------------------------------------

def maxent_single(c, eps_override, profiles, priors, apply_overrides=False):
    """Compute MaxEnt distribution for one constraint with overridden extractiveness.

    Returns dict {type: prob}. By default returns raw (pre-override) distribution.
    """
    supp = c.get("suppression") or 0.0
    theater = c.get("theater_ratio") or 0.0

    type_lls = []
    for typ in MAXENT_TYPES:
        cont_ll = 0.0
        for metric_name, val in [("extractiveness", eps_override),
                                  ("suppression", supp),
                                  ("theater", theater)]:
            mu, sigma = profiles[typ][metric_name]
            cont_ll += gaussian_ll(val, mu, sigma)
        bool_ll_val = boolean_ll(c, typ)
        prior = priors.get(typ, 0.001)
        prior_ll = math.log(prior) if prior > 1e-15 else -10.0
        type_lls.append((typ, cont_ll + bool_ll_val + prior_ll))

    dist = log_sum_exp_normalize(type_lls)
    if apply_overrides:
        dist = apply_signature_override(c.get("signature"), dist)
    return dist


def fisher_info(dist_minus, dist_plus, h):
    """Compute Fisher information I(ε) = Σ_types (1/P)(dP/dε)² from finite differences.

    Uses central difference: dP/dε ≈ (P(ε+h) - P(ε-h)) / (2h).
    P is the average of the two distributions (midpoint estimate).
    """
    two_h = 2.0 * h
    fisher = 0.0
    for typ in MAXENT_TYPES:
        p_minus = dist_minus.get(typ, 0.0)
        p_plus = dist_plus.get(typ, 0.0)
        p_mid = 0.5 * (p_minus + p_plus)
        if p_mid < 1e-10:
            continue
        dp = (p_plus - p_minus) / two_h
        fisher += (dp * dp) / p_mid
    return fisher


def total_variation_sensitivity(dist_minus, dist_plus, h):
    """Total variation sensitivity: Σ|P(ε+h) - P(ε-h)| / (2h)."""
    two_h = 2.0 * h
    return sum(abs(dist_plus.get(t, 0) - dist_minus.get(t, 0)) for t in MAXENT_TYPES) / two_h


# ---------------------------------------------------------------------------
# Multi-observer: compute at all 4 canonical positions using perspective_chi
# ---------------------------------------------------------------------------

def maxent_single_chi(c, eps_override, pchi_entry, profiles, priors):
    """Compute raw MaxEnt distribution using χ = eps_override × f(d) × scope_mod.

    pchi_entry is the per-observer dict from perspective_chi (has f_d, scope_mod).
    Feeds χ as the "extractiveness" feature into ε-calibrated profiles (Option A).
    """
    f_d = pchi_entry["f_d"]
    scope_mod = pchi_entry["scope_mod"]
    chi = eps_override * f_d * scope_mod
    return maxent_single(c, chi, profiles, priors, apply_overrides=False)


# ---------------------------------------------------------------------------
# Selection and main analysis
# ---------------------------------------------------------------------------

def select_arakelov_constraints(pipeline_entries, enriched_by_id):
    """Select Arakelov uncertainty route constraints: H¹=0, height > p75, nash=0."""
    heights = sorted(
        e["arakelov_height"] for e in pipeline_entries
        if e.get("arakelov_height") is not None and e["arakelov_height"] > 0.01
    )
    if not heights:
        return [], 0.0
    p75 = heights[int(len(heights) * 0.75)]

    selected = []
    for e in pipeline_entries:
        ah = e.get("arakelov_height")
        h1 = e.get("h1_band", -1)
        ee = enriched_by_id.get(e["id"], {})
        nash = ee.get("nash_distance_structural", -1)
        if ah is not None and ah > p75 and h1 == 0 and nash == 0:
            selected.append(e)
    return selected, p75


def run_analysis(h=0.01, run_all=False):
    """Run epsilon sensitivity analysis."""
    # Load data
    pipeline_data = load_json(PIPELINE_JSON, "pipeline_output")
    enriched_data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    pipeline_entries = pipeline_data.get("per_constraint", [])
    enriched_list = enriched_data.get("per_constraint", enriched_data if isinstance(enriched_data, list) else [])
    enriched_by_id = {e["id"]: e for e in enriched_list}

    if not pipeline_entries:
        print("ERROR: No pipeline data found.", file=sys.stderr)
        return

    # Build constraint dict for profile computation (reuse loader pattern)
    constraints_for_profiles = {}
    for e in pipeline_entries:
        constraints_for_profiles[e["id"]] = {
            "id": e["id"],
            "extractiveness": e.get("base_extractiveness", 0.0),
            "suppression": e.get("suppression", 0.0),
            "theater_ratio": e.get("theater_ratio", 0.0),
            "signature": e.get("signature"),
            "emerges_naturally": e.get("emerges_naturally", False),
            "requires_active_enforcement": e.get("requires_active_enforcement", False),
            "beneficiaries": e.get("beneficiaries", []),
            "victims": e.get("victims", []),
            "perspectives": e.get("perspectives", {}),
        }

    profiles = compute_profiles(constraints_for_profiles)
    priors = compute_priors(constraints_for_profiles)

    # Select targets
    if run_all:
        targets = pipeline_entries
        p75 = None
        print(f"Running on ALL {len(targets)} constraints (h={h})")
    else:
        targets, p75 = select_arakelov_constraints(pipeline_entries, enriched_by_id)
        print(f"Selected {len(targets)} Arakelov constraints (p75={p75:.4f}, h={h})")

    if not targets:
        print("No constraints selected.", file=sys.stderr)
        return

    # Compute Fisher information for each target
    OBSERVERS = ["powerless", "moderate", "institutional", "analytical"]
    results = []

    for e in targets:
        cid = e["id"]
        eps = e.get("base_extractiveness")
        if eps is None:
            continue

        c = constraints_for_profiles[cid]
        pchi = e.get("perspective_chi", {})
        ee = enriched_by_id.get(cid, {})

        # Analytical-position Fisher (raw ε, no χ scaling)
        eps_lo = max(0.0, eps - h)
        eps_hi = min(1.0, eps + h)
        actual_h = (eps_hi - eps_lo) / 2.0
        if actual_h < 1e-12:
            continue

        dist_lo = maxent_single(c, eps_lo, profiles, priors, apply_overrides=False)
        dist_hi = maxent_single(c, eps_hi, profiles, priors, apply_overrides=False)

        fi_analytical = fisher_info(dist_lo, dist_hi, actual_h)
        tv_analytical = total_variation_sensitivity(dist_lo, dist_hi, actual_h)

        # Multi-observer Fisher (using χ = ε × f(d) × σ)
        fi_by_observer = {"analytical_raw": fi_analytical}
        tv_by_observer = {"analytical_raw": tv_analytical}

        f2d_abs_by_observer = {}
        for obs in OBSERVERS:
            if obs not in pchi:
                continue
            pchi_entry = pchi[obs]
            dist_lo_obs = maxent_single_chi(c, eps_lo, pchi_entry, profiles, priors)
            dist_hi_obs = maxent_single_chi(c, eps_hi, pchi_entry, profiles, priors)
            fi_by_observer[obs] = fisher_info(dist_lo_obs, dist_hi_obs, actual_h)
            tv_by_observer[obs] = total_variation_sensitivity(dist_lo_obs, dist_hi_obs, actual_h)
            f2d = pchi_entry.get('f2_d')
            if f2d is not None:
                f2d_abs_by_observer[obs] = abs(f2d)

        result = {
            "id": cid,
            "epsilon": eps,
            "claimed_type": e.get("claimed_type"),
            "signature": e.get("signature"),
            "arakelov_height": e.get("arakelov_height"),
            "arakelov_context": e.get("arakelov_height_context"),
            "confidence_margin": ee.get("confidence_margin"),
            "fisher_analytical_raw": fi_analytical,
            "tv_analytical_raw": tv_analytical,
            "fisher_by_observer": fi_by_observer,
            "tv_by_observer": tv_by_observer,
            "f2d_abs_by_observer": f2d_abs_by_observer,
        }
        results.append(result)

    # Analysis
    print(f"\n{'='*70}")
    print(f"EPSILON SENSITIVITY RESULTS ({len(results)} constraints)")
    print(f"{'='*70}")

    if not results:
        print("No results computed.")
        return

    # Fisher stats (analytical raw)
    fi_vals = [r["fisher_analytical_raw"] for r in results]
    fi_vals_sorted = sorted(fi_vals)
    n = len(fi_vals)
    print(f"\nFisher Information (analytical, raw ε):")
    print(f"  Min:    {fi_vals_sorted[0]:.4f}")
    print(f"  p25:    {fi_vals_sorted[n//4]:.4f}")
    print(f"  Median: {fi_vals_sorted[n//2]:.4f}")
    print(f"  p75:    {fi_vals_sorted[3*n//4]:.4f}")
    print(f"  Max:    {fi_vals_sorted[-1]:.4f}")
    print(f"  Mean:   {sum(fi_vals)/n:.4f}")

    # Correlation with confidence_margin
    cm_vals = [r["confidence_margin"] for r in results if r["confidence_margin"] is not None]
    fi_for_corr = [r["fisher_analytical_raw"] for r in results if r["confidence_margin"] is not None]
    if len(cm_vals) >= 2:
        r_corr = pearson_r(fi_for_corr, cm_vals)
        print(f"\nCorrelation with confidence_margin: r = {r_corr:.4f}")
        if abs(r_corr) > 0.95:
            print("  >>> KILL CONDITION MET: Fisher correlates >0.95 with confidence_margin.")
            print("  >>> Diagnostic is likely redundant.")
        elif abs(r_corr) < 0.85:
            print("  >>> Non-redundant: Fisher captures information beyond confidence_margin.")
        else:
            print("  >>> Moderate correlation: Fisher partially overlaps with confidence_margin.")

    # U3 vs U4 comparison (institutional vs analytical)
    u3_vals = [r["fisher_by_observer"].get("institutional", 0) for r in results]
    u4_vals = [r["fisher_by_observer"].get("analytical", 0) for r in results]
    if u3_vals and u4_vals:
        u3_mean = sum(u3_vals) / len(u3_vals)
        u4_mean = sum(u4_vals) / len(u4_vals)
        print(f"\nU3 (institutional) vs U4 (analytical) Fisher curvature:")
        print(f"  U3 mean: {u3_mean:.4f}")
        print(f"  U4 mean: {u4_mean:.4f}")
        print(f"  Ratio U3/U4: {u3_mean/u4_mean:.4f}" if u4_mean > 1e-10 else "  U4 ≈ 0")
        higher_u3 = sum(1 for u3, u4 in zip(u3_vals, u4_vals) if u3 > u4)
        print(f"  Constraints with Fisher(U3) > Fisher(U4): {higher_u3}/{len(u3_vals)}")

    # Breakdown by arakelov context
    print(f"\nFisher by Arakelov max-context:")
    by_ctx = {}
    for r in results:
        ctx = r.get("arakelov_context", "unknown")
        by_ctx.setdefault(ctx, []).append(r["fisher_analytical_raw"])
    for ctx in sorted(by_ctx):
        vals = by_ctx[ctx]
        print(f"  {ctx}: n={len(vals)}, mean={sum(vals)/len(vals):.4f}, "
              f"median={sorted(vals)[len(vals)//2]:.4f}")

    # Three-regime partition attempt
    fi_median = fi_vals_sorted[n // 2]
    fragile = [r for r in results if r["fisher_analytical_raw"] > fi_median]
    robust = [r for r in results if r["fisher_analytical_raw"] <= fi_median]
    print(f"\nThree-regime partition (median split at Fisher={fi_median:.4f}):")
    print(f"  Fragile consensus (high Fisher): {len(fragile)}")
    print(f"    Types: {dict(Counter(r['claimed_type'] for r in fragile))}")
    print(f"  Robust consensus (low Fisher):   {len(robust)}")
    print(f"    Types: {dict(Counter(r['claimed_type'] for r in robust))}")
    fragile_f2ds = [max(r["f2d_abs_by_observer"].values())
                    for r in fragile if r.get("f2d_abs_by_observer")]
    robust_f2ds = [max(r["f2d_abs_by_observer"].values())
                   for r in robust if r.get("f2d_abs_by_observer")]
    if fragile_f2ds:
        print(f"  Fragile consensus mean |f''(d)|: {sum(fragile_f2ds)/len(fragile_f2ds):.4f}"
              f" (n={len(fragile_f2ds)})")
    if robust_f2ds:
        print(f"  Robust consensus  mean |f''(d)|: {sum(robust_f2ds)/len(robust_f2ds):.4f}"
              f" (n={len(robust_f2ds)})")

    # f''(d) / Fisher correlation analysis
    OBSERVERS_LOCAL = ["powerless", "moderate", "institutional", "analytical"]
    f2d_fi_per_obs = []
    for r in results:
        for obs in OBSERVERS_LOCAL:
            f2d_abs = r.get("f2d_abs_by_observer", {}).get(obs)
            fi = r.get("fisher_by_observer", {}).get(obs)
            if f2d_abs is not None and fi is not None and isinstance(fi, (int, float)):
                f2d_fi_per_obs.append((f2d_abs, fi))

    f2d_fi_per_cstr = []
    for r in results:
        f2ds = list(r.get("f2d_abs_by_observer", {}).values())
        fis = [v for v in r.get("fisher_by_observer", {}).values()
               if v is not None and isinstance(v, (int, float))]
        if f2ds and fis:
            f2d_fi_per_cstr.append((max(f2ds), max(fis)))

    f2d_cm_pairs = []
    for r in results:
        f2ds = list(r.get("f2d_abs_by_observer", {}).values())
        cm = r.get("confidence_margin")
        if f2ds and cm is not None:
            f2d_cm_pairs.append((max(f2ds), cm))

    def _fmt_corr(pairs):
        if len(pairs) < 2:
            return f"{'n/a':>5}", f"{'n/a':>6}", f"{'n/a':>5}", f"{'n/a':>6}", len(pairs)
        xs = [p[0] for p in pairs]
        ys = [p[1] for p in pairs]
        rp, rs, pp, ps = corr_with_p(xs, ys)
        pp_s = f"{pp:.4f}" if pp is not None else "n/a"
        ps_s = f"{ps:.4f}" if ps is not None else "n/a"
        return f"{rp:+.3f}", pp_s, f"{rs:+.3f}", ps_s, len(pairs)

    rp1, pp1, rs1, ps1, n1 = _fmt_corr(f2d_fi_per_obs)
    rp2, pp2, rs2, ps2, n2 = _fmt_corr(f2d_fi_per_cstr)
    rp3, pp3, rs3, ps3, n3 = _fmt_corr(f2d_cm_pairs)

    print(f"\nf''(d) Curvature Correlation Analysis")
    print(f"{'─'*68}")
    print(f"{'Metric Pair':<33} │ {'N':>5} │ {'r_P':>6} │ {'p_P':>6} │ {'r_S':>6} │ {'p_S':>6}")
    print(f"{'─'*33}─┼─{'─'*5}─┼─{'─'*6}─┼─{'─'*6}─┼─{'─'*6}─┼─{'─'*6}")
    print(f"{'|f′′(d)| vs Fisher (per obs)':<33} │ {n1:>5} │ {rp1:>6} │ {pp1:>6} │ {rs1:>6} │ {ps1:>6}")
    print(f"{'|f′′(d)| vs Fisher (per cstr)':<33} │ {n2:>5} │ {rp2:>6} │ {pp2:>6} │ {rs2:>6} │ {ps2:>6}")
    print(f"{'|f′′(d)| vs conf_margin':<33} │ {n3:>5} │ {rp3:>6} │ {pp3:>6} │ {rs3:>6} │ {ps3:>6}")

    # Interpretation based on per-observer Pearson r
    try:
        r_abs = abs(float(rp1))
        if r_abs < 0.3:
            interp = "Low correlation — f''(d) and Fisher capture independent fragility dimensions. COMPLEMENTARY."
        elif r_abs < 0.7:
            interp = "Moderate correlation — partial overlap. f''(d) adds information beyond Fisher."
        else:
            interp = ("High correlation — f''(d) may be largely redundant with Fisher. "
                      "Curvature Alert value is computational efficiency, not new information.")
        print(f"\nInterpretation: {interp}")
    except (ValueError, TypeError):
        pass

    # Collect summary values for JSON
    f2d_fisher_pr = pearson_r([p[0] for p in f2d_fi_per_obs], [p[1] for p in f2d_fi_per_obs]) if len(f2d_fi_per_obs) >= 2 else None
    f2d_fisher_sr = spearman_r([p[0] for p in f2d_fi_per_obs], [p[1] for p in f2d_fi_per_obs]) if len(f2d_fi_per_obs) >= 2 else None
    f2d_cm_pr = pearson_r([p[0] for p in f2d_cm_pairs], [p[1] for p in f2d_cm_pairs]) if len(f2d_cm_pairs) >= 2 else None

    # Save results
    output = {
        "h": h,
        "p75_threshold": p75,
        "n_selected": len(targets),
        "n_computed": len(results),
        "summary": {
            "fisher_analytical_raw": {
                "min": fi_vals_sorted[0],
                "p25": fi_vals_sorted[n // 4],
                "median": fi_vals_sorted[n // 2],
                "p75": fi_vals_sorted[3 * n // 4],
                "max": fi_vals_sorted[-1],
                "mean": sum(fi_vals) / n,
            },
            "correlation_with_confidence_margin": pearson_r(fi_for_corr, cm_vals) if len(cm_vals) >= 2 else None,
            "u3_mean_fisher": u3_mean if u3_vals else None,
            "u4_mean_fisher": u4_mean if u4_vals else None,
            "f2d_fisher_pearson_r": f2d_fisher_pr,
            "f2d_fisher_spearman_r": f2d_fisher_sr,
            "f2d_confidence_margin_pearson_r": f2d_cm_pr,
            "fragile_mean_f2d_abs": sum(fragile_f2ds) / len(fragile_f2ds) if fragile_f2ds else None,
            "robust_mean_f2d_abs": sum(robust_f2ds) / len(robust_f2ds) if robust_f2ds else None,
        },
        "per_constraint": results,
    }

    out_path = OUTPUT_DIR / "epsilon_sensitivity_results.json"
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)
    print(f"\nResults saved to {out_path}")


def pearson_r(xs, ys):
    """Pearson correlation coefficient."""
    n = len(xs)
    if n < 2:
        return 0.0
    mx = sum(xs) / n
    my = sum(ys) / n
    cov = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    sx = math.sqrt(sum((x - mx) ** 2 for x in xs))
    sy = math.sqrt(sum((y - my) ** 2 for y in ys))
    if sx < 1e-15 or sy < 1e-15:
        return 0.0
    return cov / (sx * sy)


def spearman_r(xs, ys):
    """Spearman rank correlation (Pearson r of ranks with average-rank tie-breaking)."""
    n = len(xs)
    if n < 2:
        return 0.0

    def _ranks(vals):
        sorted_with_idx = sorted(enumerate(vals), key=lambda iv: iv[1])
        ranks = [0.0] * n
        i = 0
        while i < n:
            j = i
            while j < n - 1 and sorted_with_idx[j + 1][1] == sorted_with_idx[j][1]:
                j += 1
            avg_rank = (i + j) / 2.0 + 1
            for k in range(i, j + 1):
                ranks[sorted_with_idx[k][0]] = avg_rank
            i = j + 1
        return ranks

    return pearson_r(_ranks(xs), _ranks(ys))


def _approx_pvalue(r, n):
    """Approximate two-tailed p-value for Pearson/Spearman r using t approximation."""
    if n < 3:
        return None
    r_clamped = max(-1 + 1e-12, min(1 - 1e-12, r))
    t = r_clamped * math.sqrt(n - 2) / math.sqrt(1 - r_clamped ** 2)
    # Normal approximation for large n; erfc for t-to-p
    # Use math.erfc(|t|/sqrt(2)) as an approximation to 2*(1-Phi(|t|))
    p = math.erfc(abs(t) / math.sqrt(2))
    return p


def corr_with_p(xs, ys):
    """Return (pearson_r, spearman_r, p_pearson, p_spearman).

    Uses scipy if available for exact p-values; otherwise uses normal approximation.
    """
    n = len(xs)
    rp = pearson_r(xs, ys)
    rs = spearman_r(xs, ys)
    if HAS_SCIPY and n >= 3:
        try:
            _, pp = _scipy_stats.pearsonr(xs, ys)
            _, ps = _scipy_stats.spearmanr(xs, ys)
        except Exception:
            pp = _approx_pvalue(rp, n)
            ps = _approx_pvalue(rs, n)
    else:
        pp = _approx_pvalue(rp, n)
        ps = _approx_pvalue(rs, n)
    return rp, rs, pp, ps


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Epsilon sensitivity (Fisher information) diagnostic")
    parser.add_argument("--h", type=float, default=0.01, help="Perturbation step size (default: 0.01)")
    parser.add_argument("--all", action="store_true", help="Run on all constraints, not just Arakelov set")
    args = parser.parse_args()
    run_analysis(h=args.h, run_all=args.all)
