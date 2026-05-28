"""f''(d) vs Structural Config Sensitivity Crossing Analysis.

Tests whether curvature |f''(d)| predicts which constraints cross
classification boundaries when sigmoid parameters are perturbed.

Methodology: Path C (analytical reconstruction).
  For each constraint, for each observer, analytically recompute chi
  under perturbed sigmoid params. Define crossing as: the agreement
  pattern across observers changes (all-same ↔ mixed types). Correlate
  |f''(d; baseline)| with crossing status using Mann-Whitney U and ROC AUC.

Path C caveat: captures only the sigmoid-shift component of crossing.
  Prolog-side effects (immutability gates, signature overrides, scope
  adjustments that depend on Prolog facts) are NOT modelled. Type bins are
  approximated by chi thresholds; the full dual-threshold (chi AND epsilon)
  classification is not replicated.

Config (from config.pl):
  rope_chi_ceiling:  0.35
  snare_chi_floor:   0.66
  Baseline sigmoid:  L=-0.20, U=1.50, D0=0.50, K=6.00

Usage:
    python3 python/f2d_sensitivity_crossing.py [--top N]
"""

import json
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import ENRICHED_PIPELINE_JSON

try:
    from scipy import stats as _scipy_stats
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

# ---------------------------------------------------------------------------
# Config constants (must match prolog/config.pl)
# ---------------------------------------------------------------------------

ROPE_CHI_CEILING = 0.35
SNARE_CHI_FLOOR  = 0.66

SIGMOID_L  = -0.20
SIGMOID_U  =  1.50
SIGMOID_D0 =  0.50
SIGMOID_K  =  6.00

OBSERVERS = ["powerless", "moderate", "institutional", "analytical"]

# Group A sigmoid settings from structural_config_sensitivity.py
SIGMOID_SETTINGS = [
    # (param_name, value, L, U, D0, K)
    ("sigmoid_midpoint", 0.20, SIGMOID_L, SIGMOID_U, 0.20, SIGMOID_K),
    ("sigmoid_midpoint", 0.35, SIGMOID_L, SIGMOID_U, 0.35, SIGMOID_K),
    ("sigmoid_midpoint", 0.42, SIGMOID_L, SIGMOID_U, 0.42, SIGMOID_K),
    ("sigmoid_midpoint", 0.58, SIGMOID_L, SIGMOID_U, 0.58, SIGMOID_K),
    ("sigmoid_midpoint", 0.65, SIGMOID_L, SIGMOID_U, 0.65, SIGMOID_K),
    ("sigmoid_midpoint", 0.80, SIGMOID_L, SIGMOID_U, 0.80, SIGMOID_K),
    ("sigmoid_steepness", 1.5,  SIGMOID_L, SIGMOID_U, SIGMOID_D0, 1.5),
    ("sigmoid_steepness", 3.0,  SIGMOID_L, SIGMOID_U, SIGMOID_D0, 3.0),
    ("sigmoid_steepness", 4.5,  SIGMOID_L, SIGMOID_U, SIGMOID_D0, 4.5),
    ("sigmoid_steepness", 7.5,  SIGMOID_L, SIGMOID_U, SIGMOID_D0, 7.5),
    ("sigmoid_steepness", 9.0,  SIGMOID_L, SIGMOID_U, SIGMOID_D0, 9.0),
    ("sigmoid_steepness", 12.0, SIGMOID_L, SIGMOID_U, SIGMOID_D0, 12.0),
    ("sigmoid_upper",  0.80, SIGMOID_L, 0.80, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  1.00, SIGMOID_L, 1.00, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  1.20, SIGMOID_L, 1.20, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  1.35, SIGMOID_L, 1.35, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  1.65, SIGMOID_L, 1.65, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  1.80, SIGMOID_L, 1.80, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_upper",  2.00, SIGMOID_L, 2.00, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_lower", -0.40, -0.40, SIGMOID_U, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_lower", -0.30, -0.30, SIGMOID_U, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_lower", -0.10, -0.10, SIGMOID_U, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_lower",  0.00,  0.00, SIGMOID_U, SIGMOID_D0, SIGMOID_K),
    ("sigmoid_lower",  0.10,  0.10, SIGMOID_U, SIGMOID_D0, SIGMOID_K),
]

# ---------------------------------------------------------------------------
# Sigmoid and classification helpers
# ---------------------------------------------------------------------------

def sigmoid_f(d, L, U, D0, K):
    """Logistic sigmoid: f(d) = L + (U-L)/(1 + exp(-K*(d-D0)))."""
    return L + (U - L) / (1.0 + math.exp(-K * (d - D0)))


def chi_bin(chi):
    """Three-way classification bin from chi value.

    rope_like   : chi <= rope_chi_ceiling (0.35)
    snare_like  : chi >= snare_chi_floor  (0.66)
    tangled_like: in between
    """
    if chi <= ROPE_CHI_CEILING:
        return "rope_like"
    if chi >= SNARE_CHI_FLOOR:
        return "snare_like"
    return "tangled_like"


def is_presheaf(bins):
    """True if multiple distinct bins across observers (H¹>0 approximation)."""
    return len(set(bins)) > 1


# ---------------------------------------------------------------------------
# Per-constraint data extraction
# ---------------------------------------------------------------------------

def extract_constraint_data(pc_entries):
    """Extract per-constraint (d, epsilon, scope_mod, f2_d, baseline_chi) per observer.

    Returns dict: {cid: {obs: {d, epsilon, scope_mod, f2_d, chi_base}}}
    Only includes observers with complete data.
    """
    data = {}
    for item in pc_entries:
        cid = item.get("id")
        pchi = item.get("perspective_chi", {})
        obs_data = {}
        for obs in OBSERVERS:
            entry = pchi.get(obs)
            if not isinstance(entry, dict):
                continue
            d = entry.get("d")
            eps = entry.get("epsilon")
            scope = entry.get("scope_mod")
            f2d = entry.get("f2_d")
            chi_base = entry.get("chi")
            if d is None or eps is None or scope is None:
                continue
            obs_data[obs] = {
                "d": d,
                "epsilon": eps,
                "scope_mod": scope,
                "f2d": f2d,
                "chi_base": chi_base,
            }
        if obs_data:
            data[cid] = obs_data
    return data


# ---------------------------------------------------------------------------
# Crossing detection for one sigmoid setting
# ---------------------------------------------------------------------------

def detect_crossings(constraint_data, L, U, D0, K):
    """Compute per-constraint crossing status for a sigmoid perturbation.

    A constraint 'crosses' if its presheaf status (H¹>0 approximation)
    changes between baseline and perturbed sigmoid.

    Returns:
        crossings: set of constraint IDs that crossed
        n_s_to_p: sheaf→presheaf count
        n_p_to_s: presheaf→sheaf count
    """
    crossings = set()
    n_s_to_p = 0
    n_p_to_s = 0

    for cid, obs_dict in constraint_data.items():
        # Baseline bins from stored chi values
        base_bins = []
        pert_bins = []

        for obs, ov in obs_dict.items():
            chi_base = ov.get("chi_base")
            if chi_base is not None:
                base_bins.append(chi_bin(chi_base))

            d = ov["d"]
            eps = ov["epsilon"]
            scope = ov["scope_mod"]
            new_fd = sigmoid_f(d, L, U, D0, K)
            new_chi = eps * new_fd * scope
            pert_bins.append(chi_bin(new_chi))

        if not base_bins or not pert_bins:
            continue

        base_presheaf = is_presheaf(base_bins)
        pert_presheaf = is_presheaf(pert_bins)

        if base_presheaf != pert_presheaf:
            crossings.add(cid)
            if not base_presheaf and pert_presheaf:
                n_s_to_p += 1
            else:
                n_p_to_s += 1

    return crossings, n_s_to_p, n_p_to_s


# ---------------------------------------------------------------------------
# Analysis helpers
# ---------------------------------------------------------------------------

def extract_f2d_max(constraint_data):
    """Return {cid: max |f2_d| across observers} for constraints with f2_d data."""
    result = {}
    for cid, obs_dict in constraint_data.items():
        vals = [abs(ov["f2d"]) for ov in obs_dict.values() if ov.get("f2d") is not None]
        if vals:
            result[cid] = max(vals)
    return result


def extract_f2d_by_observer(constraint_data):
    """Return {cid: {obs: |f2_d|}} for constraints with f2_d data."""
    result = {}
    for cid, obs_dict in constraint_data.items():
        obs_vals = {obs: abs(ov["f2d"]) for obs, ov in obs_dict.items()
                    if ov.get("f2d") is not None}
        if obs_vals:
            result[cid] = obs_vals
    return result


def mann_whitney_p(xs, ys):
    """Two-sided Mann-Whitney U p-value.  Falls back to None if scipy unavailable."""
    if not HAS_SCIPY or len(xs) < 2 or len(ys) < 2:
        return None
    try:
        _, p = _scipy_stats.mannwhitneyu(xs, ys, alternative="two-sided")
        return p
    except Exception:
        return None


def roc_auc(positive_scores, negative_scores):
    """ROC AUC: probability that a random positive scores higher than a random negative.

    Computed as (Mann-Whitney U for positives) / (n_pos * n_neg).
    Works without scipy.
    """
    n_pos = len(positive_scores)
    n_neg = len(negative_scores)
    if n_pos == 0 or n_neg == 0:
        return None
    # Count concordant pairs
    concordant = sum(1 for p in positive_scores for n in negative_scores if p > n)
    tied = sum(1 for p in positive_scores for n in negative_scores if p == n)
    return (concordant + 0.5 * tied) / (n_pos * n_neg)


def mean_safe(vals):
    return sum(vals) / len(vals) if vals else float("nan")


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def run_analysis(top_n=None):
    import argparse
    # Load data
    print(f"Loading {ENRICHED_PIPELINE_JSON}...")
    with open(ENRICHED_PIPELINE_JSON, encoding="utf-8") as f:
        enriched = json.load(f)

    pc_entries = enriched.get("per_constraint", [])
    print(f"Loaded {len(pc_entries)} constraints.")

    constraint_data = extract_constraint_data(pc_entries)
    print(f"Constraints with complete observer data: {len(constraint_data)}")

    f2d_max = extract_f2d_max(constraint_data)
    f2d_by_obs = extract_f2d_by_observer(constraint_data)
    n_with_f2d = len(f2d_max)
    print(f"Constraints with f2_d populated: {n_with_f2d}")

    if n_with_f2d == 0:
        print("ERROR: No f2_d data found. Re-run pipeline to populate.", file=sys.stderr)
        return

    # Verify Check 2: mean |f''(d)| in range [2, 8]
    all_f2d = list(f2d_max.values())
    mean_f2d = mean_safe(all_f2d)
    print(f"Full corpus mean |f''(d)|: {mean_f2d:.4f}  (expected 4–6 range)")
    if not (2.0 <= mean_f2d <= 8.0):
        print(f"  WARNING: mean |f''(d)| = {mean_f2d:.4f} outside expected [2, 8] range")

    print()

    # Primary analysis: per setting
    settings_to_run = SIGMOID_SETTINGS
    if top_n is not None:
        settings_to_run = SIGMOID_SETTINGS[:top_n]

    setting_results = []

    for param, val, L, U, D0, K in settings_to_run:
        label = f"{param}={val}"
        crossings, n_s_to_p, n_p_to_s = detect_crossings(constraint_data, L, U, D0, K)

        # f2_d scores for crossed vs non-crossed (only constraints with f2_d data)
        cross_f2d  = [f2d_max[cid] for cid in crossings      if cid in f2d_max]
        no_cross_f2d = [f2d_max[cid] for cid in constraint_data if cid not in crossings and cid in f2d_max]

        mean_cross   = mean_safe(cross_f2d)
        mean_no      = mean_safe(no_cross_f2d)
        p_val        = mann_whitney_p(cross_f2d, no_cross_f2d)
        auc          = roc_auc(cross_f2d, no_cross_f2d)

        setting_results.append({
            "label": label,
            "n_cross": len(crossings),
            "n_s_to_p": n_s_to_p,
            "n_p_to_s": n_p_to_s,
            "n_no_cross": len(no_cross_f2d),
            "mean_f2d_cross": mean_cross,
            "mean_f2d_no": mean_no,
            "p_mw": p_val,
            "auc": auc,
            "crossings": crossings,
        })

    # Sort by crossing count descending for the table
    setting_results.sort(key=lambda r: r["n_cross"], reverse=True)

    # Primary table
    print("f''(d) vs Structural Sensitivity Crossings (Path C: Analytical)")
    print("=" * 90)
    print(f"{'Setting':<28} │ {'N_cross':>7} │ {'s→p':>5} │ {'p→s':>5} │ "
          f"{'mean|f″| cross':>15} │ {'mean|f″| no':>12} │ {'U-test p':>9} │ {'AUC':>6}")
    print(f"{'─'*28}─┼─{'─'*7}─┼─{'─'*5}─┼─{'─'*5}─┼─"
          f"{'─'*15}─┼─{'─'*12}─┼─{'─'*9}─┼─{'─'*6}")
    for r in setting_results:
        p_str  = f"{r['p_mw']:.4f}" if r['p_mw']  is not None else "  n/a"
        auc_str = f"{r['auc']:.4f}" if r['auc']    is not None else "  n/a"
        print(f"{r['label']:<28} │ {r['n_cross']:>7} │ {r['n_s_to_p']:>5} │ {r['n_p_to_s']:>5} │ "
              f"{r['mean_f2d_cross']:>15.4f} │ {r['mean_f2d_no']:>12.4f} │ {p_str:>9} │ {auc_str:>6}")

    print()

    # Per-observer breakdown for the focal setting: sigmoid_midpoint=0.80
    focal = next((r for r in setting_results if r["label"] == "sigmoid_midpoint=0.8"), None)
    if focal is None:
        focal = setting_results[0]  # use highest-crossing setting

    focal_crossings = focal["crossings"]
    print(f"Per-Observer Breakdown: {focal['label']}")
    print("─" * 68)
    print(f"{'Observer':<14} │ {'N_cross':>7} │ {'mean|f″| cross':>15} │ "
          f"{'mean|f″| no':>12} │ {'U-test p':>9}")
    print(f"{'─'*14}─┼─{'─'*7}─┼─{'─'*15}─┼─{'─'*12}─┼─{'─'*9}")

    for obs in OBSERVERS:
        # Per-observer: only include constraints where this observer has f2_d
        cross_obs  = [f2d_by_obs[cid][obs] for cid in focal_crossings
                      if cid in f2d_by_obs and obs in f2d_by_obs[cid]]
        no_obs     = [f2d_by_obs[cid][obs] for cid in constraint_data
                      if cid not in focal_crossings
                      and cid in f2d_by_obs and obs in f2d_by_obs[cid]]
        mean_c = mean_safe(cross_obs)
        mean_n = mean_safe(no_obs)
        p_o    = mann_whitney_p(cross_obs, no_obs)
        p_str  = f"{p_o:.4f}" if p_o is not None else "  n/a"
        print(f"{obs:<14} │ {len(cross_obs):>7} │ {mean_c:>15.4f} │ {mean_n:>12.4f} │ {p_str:>9}")

    print()

    # Verification checks
    print("Verification")
    print("─" * 50)

    # Check 1: warn that crossing counts differ from SOTU results
    print("Check 1 (corpus mismatch warning):")
    print("  Stored results cover 162 SOTU constraints; this analysis covers the full")
    print(f"  {len(constraint_data)}-constraint corpus. Counts are not expected to match.")
    print(f"  sigmoid_midpoint=0.80 crossings (full corpus): {focal['n_cross']}")
    print(f"  sigmoid_midpoint=0.80 crossings (SOTU stored):  81")

    # Check 2: mean f2_d in range
    print(f"Check 2: corpus mean |f''(d)| = {mean_f2d:.4f}  "
          f"({'OK' if 2.0 <= mean_f2d <= 8.0 else 'FAIL — outside [2,8]'})")

    # Check 3: both populations exist for focal setting
    print(f"Check 3: crossed={focal['n_cross']}, not-crossed={focal['n_no_cross']}  "
          f"({'OK' if focal['n_cross'] > 0 and focal['n_no_cross'] > 0 else 'FAIL'})")

    # Check 4: join stats
    n_total = len(constraint_data)
    n_with  = n_with_f2d
    n_miss  = n_total - n_with
    miss_pct = 100.0 * n_miss / n_total if n_total else 0
    print(f"Check 4: {n_with}/{n_total} constraints have f2_d data "
          f"({miss_pct:.1f}% missing; {'OK' if miss_pct <= 5.0 else 'FLAG — >5% missing'})")

    print()

    # Interpretation
    auc_focal = focal["auc"]
    if auc_focal is not None:
        print(f"Interpretation (AUC = {auc_focal:.4f} at {focal['label']}):")
        if auc_focal > 0.65:
            print("  f''(d) has discriminative power for parametric sensitivity.")
            print("  Curvature predicts which constraints cross under sigmoid perturbation.")
        elif auc_focal > 0.55:
            print("  f''(d) has weak discriminative power.")
            print("  Curvature is one factor among several.")
        else:
            print("  f''(d) does NOT predict structural sensitivity crossings.")
            print("  The 188 curvature-only alerts are NOT pre-fragile —")
            print("  they are a geometrically distinct but diagnostically inert population.")

    print()
    print(f"[Path C caveat] This analysis uses analytical chi reconstruction.")
    print(f"  Prolog-side effects (signature overrides, immutability gates) are excluded.")
    print(f"  Type bins: rope_like (χ≤{ROPE_CHI_CEILING}), tangled_like, snare_like (χ≥{SNARE_CHI_FLOOR})")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="f''(d) vs structural sensitivity crossing analysis")
    parser.add_argument("--top", type=int, default=None,
                        help="Limit to first N sigmoid settings (default: all 24)")
    args = parser.parse_args()
    run_analysis(top_n=args.top)
