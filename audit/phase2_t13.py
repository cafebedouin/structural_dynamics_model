"""Phase 2: T13 Fisher-Rao / Hellinger Audit.

Computes classical vs indexed MaxEnt distributions at each context,
measures KL divergence, Fisher-Rao geodesic distance, Hellinger distance,
and audits T13 trigger firing, asymmetry, and oracle gap.
"""

import csv
import math
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT / "python") not in sys.path:
    sys.path.insert(0, str(ROOT / "python"))

from shared.constants import MAXENT_TYPES, BOOLEAN_SPECS, N_TYPES
from shared.maxent import (
    gaussian_ll, boolean_ll, log_sum_exp_normalize,
    compute_profiles, compute_priors, apply_signature_override,
    DEFAULT_PROFILES, eval_boolean,
)

from .phase0_data import CONTEXTS

PROB_FLOOR = 1e-10
T13_THRESHOLD = 0.05


def _build_constraint_dict_for_maxent(c):
    """Convert our constraint dict to the format shared/maxent.py expects."""
    return {
        "extractiveness": c["epsilon"],
        "suppression": c["suppression"],
        "theater_ratio": c["theater_ratio"],
        "signature": c["signature"],
        "emerges_naturally": c["emerges_naturally"],
        "requires_active_enforcement": c["requires_active_enforcement"],
        "beneficiaries": c["beneficiaries"],
        "victims": c["victims"],
        "perspectives": dict(zip(CONTEXTS, c["types"])),
        "claimed_type": c["claimed_type"],
    }


def compute_maxent_distribution(eps_or_chi, supp, theater, c_dict, profiles, priors):
    """Compute MaxEnt probability distribution using given extractiveness value.

    This is the core computation adapted from shared/maxent.py, allowing us to
    substitute chi for epsilon.
    """
    type_lls = []
    for typ in MAXENT_TYPES:
        # Continuous log-likelihood
        cont_ll = 0.0
        for metric_name, val in [("extractiveness", eps_or_chi),
                                 ("suppression", supp),
                                 ("theater", theater)]:
            mu, sigma = profiles[typ][metric_name]
            cont_ll += gaussian_ll(val, mu, sigma)

        # Boolean log-likelihood
        bool_ll_val = boolean_ll(c_dict, typ)

        # Prior
        prior = priors.get(typ, 0.001)
        prior_ll = math.log(prior) if prior > 1e-15 else -10.0

        total_ll = cont_ll + bool_ll_val + prior_ll
        type_lls.append((typ, total_ll))

    return log_sum_exp_normalize(type_lls)


def floor_dist(dist):
    """Floor all probabilities and renormalize."""
    floored = {t: max(PROB_FLOOR, p) for t, p in dist.items()}
    total = sum(floored.values())
    return {t: p / total for t, p in floored.items()}


def kl_divergence(p, q):
    """KL(p || q) = sum_t p(t) * log(p(t) / q(t))."""
    p_f, q_f = floor_dist(p), floor_dist(q)
    return sum(p_f[t] * math.log(p_f[t] / q_f[t]) for t in MAXENT_TYPES)


def bhattacharyya_coefficient(p, q):
    """BC(p, q) = sum_t sqrt(p(t) * q(t))."""
    p_f, q_f = floor_dist(p), floor_dist(q)
    return sum(math.sqrt(p_f[t] * q_f[t]) for t in MAXENT_TYPES)


def fisher_rao_distance(p, q):
    """Fisher-Rao geodesic distance = 2 * arccos(BC(p, q))."""
    bc = bhattacharyya_coefficient(p, q)
    bc = min(1.0, max(-1.0, bc))  # clamp for numerical safety
    return 2.0 * math.acos(bc)


def hellinger_sq(p, q):
    """Squared Hellinger distance = sum_t (sqrt(p_t) - sqrt(q_t))^2."""
    p_f, q_f = floor_dist(p), floor_dist(q)
    return sum((math.sqrt(p_f[t]) - math.sqrt(q_f[t])) ** 2 for t in MAXENT_TYPES)


def hellinger_decomposition(p, q):
    """Per-type Hellinger contribution: {type: (sqrt(p_t) - sqrt(q_t))^2}."""
    p_f, q_f = floor_dist(p), floor_dist(q)
    return {t: (math.sqrt(p_f[t]) - math.sqrt(q_f[t])) ** 2 for t in MAXENT_TYPES}


def max_tvd(p, q):
    """Maximum absolute type-wise probability difference (T13 criterion)."""
    p_f, q_f = floor_dist(p), floor_dist(q)
    return max(abs(p_f[t] - q_f[t]) for t in MAXENT_TYPES)


def run_phase2(data, output_dir):
    """Run the complete Phase 2 T13 audit.

    Returns dict with all results for the report.
    """
    output_dir = Path(output_dir)
    constraints = data["constraints"]
    N = len(constraints)

    # Build constraint dicts for MaxEnt
    c_dicts = [_build_constraint_dict_for_maxent(c) for c in constraints]
    constraints_by_id = {c["id"]: c_dicts[i] for i, c in enumerate(constraints)}

    # Compute profiles and priors from corpus (using epsilon, matching Prolog)
    profiles = compute_profiles(constraints_by_id)
    priors = compute_priors(constraints_by_id)

    # --- For each constraint: compute classical and indexed MaxEnt ---
    results_rows = []  # Per constraint-context rows
    t13_results = []   # Per constraint T13 summary
    hellinger_decomp_rows = []  # For T13-firing constraints

    for i, c in enumerate(constraints):
        c_dict = c_dicts[i]
        eps = c["epsilon"]
        supp = c["suppression"]
        theater = c["theater_ratio"]
        h1 = c["h1"]

        # Classical MaxEnt (same for all contexts)
        p_cl = compute_maxent_distribution(eps, supp, theater, c_dict, profiles, priors)

        # Per-context indexed MaxEnt
        max_tvd_across_contexts = 0.0
        worst_context = ""
        context_divergences = []

        for j, ctx in enumerate(CONTEXTS):
            chi_j = c["chi"][j]

            # Indexed MaxEnt at this context
            p_idx = compute_maxent_distribution(chi_j, supp, theater, c_dict, profiles, priors)

            # Divergence measures
            kl_fwd = kl_divergence(p_cl, p_idx)
            kl_rev = kl_divergence(p_idx, p_cl)
            d_fr = fisher_rao_distance(p_cl, p_idx)
            h_sq = hellinger_sq(p_cl, p_idx)
            tvd = max_tvd(p_cl, p_idx)

            # Argmax types
            argmax_cl = max(p_cl, key=p_cl.get)
            argmax_idx = max(p_idx, key=p_idx.get)

            results_rows.append({
                "constraint_id": c["id"],
                "context": ctx,
                "chi": chi_j,
                "KL_fwd": kl_fwd,
                "KL_rev": kl_rev,
                "d_FR": d_fr,
                "H_sq": h_sq,
                "max_tvd": tvd,
                "argmax_cl": argmax_cl,
                "argmax_idx": argmax_idx,
                "type_match": argmax_cl == argmax_idx,
                "p_cl": p_cl,
                "p_idx": p_idx,
            })

            context_divergences.append({
                "ctx": ctx,
                "tvd": tvd,
                "d_FR": d_fr,
                "kl_fwd": kl_fwd,
                "kl_rev": kl_rev,
            })

            if tvd > max_tvd_across_contexts:
                max_tvd_across_contexts = tvd
                worst_context = ctx

        # T13 firing criterion
        t13_fires = max_tvd_across_contexts > T13_THRESHOLD and h1 > 0

        # Asymmetry at worst context
        if not worst_context:
            worst_context = CONTEXTS[0]  # fallback for zero-divergence
        worst_candidates = [r for r in results_rows[-4:] if r["context"] == worst_context]
        worst_row = worst_candidates[0] if worst_candidates else results_rows[-1]
        kl_fwd_w = worst_row["KL_fwd"]
        kl_rev_w = worst_row["KL_rev"]
        asymmetry = (abs(kl_fwd_w - kl_rev_w) / max(kl_fwd_w, kl_rev_w)
                     if max(kl_fwd_w, kl_rev_w) > 1e-10 else 0.0)

        t13_results.append({
            "constraint_id": c["id"],
            "h1": h1,
            "max_tvd": max_tvd_across_contexts,
            "t13_fires": t13_fires,
            "worst_context": worst_context,
            "d_FR_worst": worst_row["d_FR"],
            "KL_fwd_worst": kl_fwd_w,
            "KL_rev_worst": kl_rev_w,
            "asymmetry_ratio": asymmetry,
            "argmax_cl": worst_row["argmax_cl"],
            "argmax_idx": worst_row["argmax_idx"],
        })

        # Hellinger decomposition for T13-firing constraints
        if t13_fires:
            for r in results_rows[-4:]:
                h_decomp = hellinger_decomposition(r["p_cl"], r["p_idx"])
                h_total = sum(h_decomp.values())
                if h_total > 1e-15:
                    fractions = {t: v / h_total for t, v in h_decomp.items()}
                else:
                    fractions = {t: 0.0 for t in MAXENT_TYPES}
                hellinger_decomp_rows.append({
                    "constraint_id": c["id"],
                    "context": r["context"],
                    "H_sq_total": h_total,
                    **{f"frac_{t}": fractions[t] for t in MAXENT_TYPES},
                })

    # --- Aggregate statistics ---
    t13_firing = [r for r in t13_results if r["t13_fires"]]
    t13_not_firing_h1_pos = [r for r in t13_results if not r["t13_fires"] and r["h1"] > 0]
    t13_h1_zero = [r for r in t13_results if r["h1"] == 0]

    n_t13 = len(t13_firing)
    n_h1_pos = sum(1 for c in constraints if c["h1"] > 0)
    n_h1_zero = sum(1 for c in constraints if c["h1"] == 0)

    # High asymmetry constraints
    high_asymmetry = [r for r in t13_firing if r["asymmetry_ratio"] > 0.3]

    # Would Fisher-Rao change T13 firing? (check if d_FR > some threshold vs TVD)
    # Use comparable threshold: arccos(1 - 0.05^2/2) ≈ 0.05 (approx for small distances)
    fr_threshold = fisher_rao_distance(
        {t: 1.0 / N_TYPES for t in MAXENT_TYPES},
        {t: (1.0 / N_TYPES + (T13_THRESHOLD if t == "mountain" else -T13_THRESHOLD / 5))
         for t in MAXENT_TYPES}
    )

    # Oracle gap analysis
    oracle_gap_tvds = [r["max_tvd"] for r in t13_not_firing_h1_pos]
    oracle_gap_dfr = [r["d_FR_worst"] for r in t13_not_firing_h1_pos]
    t13_dfr = [r["d_FR_worst"] for r in t13_firing]

    # --- Save CSVs ---
    with open(output_dir / "divergence_measures.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "context", "chi", "KL_fwd", "KL_rev",
                     "d_FR", "H_sq", "max_tvd", "argmax_cl", "argmax_idx", "type_match"])
        for r in results_rows:
            w.writerow([r["constraint_id"], r["context"], r["chi"], r["KL_fwd"],
                        r["KL_rev"], r["d_FR"], r["H_sq"], r["max_tvd"],
                        r["argmax_cl"], r["argmax_idx"], r["type_match"]])

    with open(output_dir / "t13_analysis.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "h1", "max_tvd", "t13_fires", "worst_context",
                     "d_FR_worst", "KL_fwd_worst", "KL_rev_worst", "asymmetry_ratio",
                     "argmax_cl", "argmax_idx"])
        for r in t13_results:
            w.writerow([r["constraint_id"], r["h1"], r["max_tvd"], r["t13_fires"],
                        r["worst_context"], r["d_FR_worst"], r["KL_fwd_worst"],
                        r["KL_rev_worst"], r["asymmetry_ratio"],
                        r["argmax_cl"], r["argmax_idx"]])

    with open(output_dir / "hellinger_decomposition.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "context", "H_sq_total"] +
                   [f"frac_{t}" for t in MAXENT_TYPES])
        for r in hellinger_decomp_rows:
            w.writerow([r["constraint_id"], r["context"], r["H_sq_total"]] +
                       [r[f"frac_{t}"] for t in MAXENT_TYPES])

    with open(output_dir / "oracle_gap.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "h1", "max_tvd", "d_FR_worst", "near_threshold"])
        for r in t13_not_firing_h1_pos:
            near = abs(r["max_tvd"] - T13_THRESHOLD) < 0.01
            w.writerow([r["constraint_id"], r["h1"], r["max_tvd"], r["d_FR_worst"], near])

    # --- Generate plots ---
    _generate_plots(output_dir, t13_results, t13_firing, t13_not_firing_h1_pos,
                    oracle_gap_tvds, oracle_gap_dfr, t13_dfr)

    return {
        "n_total": N,
        "n_h1_positive": n_h1_pos,
        "n_h1_zero": n_h1_zero,
        "n_t13_firing": n_t13,
        "t13_rate_of_h1_pos": n_t13 / n_h1_pos if n_h1_pos > 0 else 0.0,
        "t13_firing_ids": [r["constraint_id"] for r in t13_firing],
        "n_high_asymmetry": len(high_asymmetry),
        "high_asymmetry_ids": [r["constraint_id"] for r in high_asymmetry],
        "oracle_gap": {
            "n_h1_pos_not_t13": len(t13_not_firing_h1_pos),
            "mean_tvd": float(np.mean(oracle_gap_tvds)) if oracle_gap_tvds else 0.0,
            "median_tvd": float(np.median(oracle_gap_tvds)) if oracle_gap_tvds else 0.0,
            "max_tvd": float(np.max(oracle_gap_tvds)) if oracle_gap_tvds else 0.0,
            "mean_dfr_non_t13": float(np.mean(oracle_gap_dfr)) if oracle_gap_dfr else 0.0,
            "mean_dfr_t13": float(np.mean(t13_dfr)) if t13_dfr else 0.0,
            "n_near_threshold": sum(1 for t in oracle_gap_tvds if abs(t - T13_THRESHOLD) < 0.01),
        },
        "hellinger_decomp_summary": _summarize_hellinger(hellinger_decomp_rows),
        "fr_threshold_equivalent": fr_threshold,
        "t13_details": t13_firing[:20],  # First 20 for report
    }


def _summarize_hellinger(rows):
    """Summarize which types carry the Hellinger divergence for T13 fires."""
    if not rows:
        return {}
    type_fracs = defaultdict(list)
    for r in rows:
        for t in MAXENT_TYPES:
            type_fracs[t].append(r[f"frac_{t}"])
    return {t: {"mean": float(np.mean(v)), "max": float(np.max(v))}
            for t, v in type_fracs.items()}


def _generate_plots(output_dir, t13_results, t13_firing, t13_not_firing_h1_pos,
                    oracle_gap_tvds, oracle_gap_dfr, t13_dfr):
    """Generate all Phase 2 plots."""

    # Plot 1: Oracle gap — histogram of max TVD for H1>0 non-T13
    fig, ax = plt.subplots(figsize=(10, 6))
    if oracle_gap_tvds:
        ax.hist(oracle_gap_tvds, bins=40, alpha=0.7, color="steelblue",
                label=f"H¹>0, no T13 (n={len(oracle_gap_tvds)})", edgecolor="black")
    t13_tvds = [r["max_tvd"] for r in t13_firing]
    if t13_tvds:
        ax.hist(t13_tvds, bins=20, alpha=0.7, color="coral",
                label=f"T13 fires (n={len(t13_tvds)})", edgecolor="black")
    ax.axvline(T13_THRESHOLD, color="red", linestyle="--", label=f"T13 threshold={T13_THRESHOLD}")
    ax.set_xlabel("Max TVD (classical vs indexed MaxEnt)")
    ax.set_ylabel("Count")
    ax.set_title("Oracle Gap: Distance to T13 Threshold")
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_dir / "oracle_gap_histogram.png", dpi=150)
    plt.close(fig)

    # Plot 2: Fisher-Rao distance comparison
    fig, ax = plt.subplots(figsize=(10, 6))
    if oracle_gap_dfr:
        ax.hist(oracle_gap_dfr, bins=40, alpha=0.6, color="steelblue",
                label=f"Non-T13, H¹>0 (n={len(oracle_gap_dfr)})", edgecolor="black")
    if t13_dfr:
        ax.hist(t13_dfr, bins=20, alpha=0.6, color="coral",
                label=f"T13 fires (n={len(t13_dfr)})", edgecolor="black")
    ax.set_xlabel("Fisher-Rao Geodesic Distance d_FR")
    ax.set_ylabel("Count")
    ax.set_title("Fisher-Rao Distance: T13 vs Non-T13 Populations")
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_dir / "fisher_rao_comparison.png", dpi=150)
    plt.close(fig)

    # Plot 3: Asymmetry ratio distribution for T13 fires
    if t13_firing:
        fig, ax = plt.subplots(figsize=(8, 5))
        asym = [r["asymmetry_ratio"] for r in t13_firing]
        ax.hist(asym, bins=20, edgecolor="black", alpha=0.7, color="steelblue")
        ax.axvline(0.3, color="red", linestyle="--", label="High asymmetry threshold (0.3)")
        ax.set_xlabel("Asymmetry Ratio |KL_fwd - KL_rev| / max(KL_fwd, KL_rev)")
        ax.set_ylabel("Count")
        ax.set_title("KL Asymmetry for T13-Firing Constraints")
        ax.legend()
        fig.tight_layout()
        fig.savefig(output_dir / "t13_asymmetry.png", dpi=150)
        plt.close(fig)

    # Plot 4: TVD by H1 band
    fig, ax = plt.subplots(figsize=(10, 6))
    h1_vals = sorted(set(r["h1"] for r in t13_results))
    for h1 in h1_vals:
        tvds = [r["max_tvd"] for r in t13_results if r["h1"] == h1]
        if tvds:
            jitter = np.random.normal(0, 0.15, len(tvds))
            ax.scatter([h1] * len(tvds) + jitter, tvds, alpha=0.3, s=15)
    ax.axhline(T13_THRESHOLD, color="red", linestyle="--", alpha=0.7, label="T13 threshold")
    ax.set_xlabel("H¹ Band")
    ax.set_ylabel("Max TVD")
    ax.set_title("Max TVD by H¹ Band (T13 fires above dashed line + H¹>0)")
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_dir / "tvd_by_h1.png", dpi=150)
    plt.close(fig)
