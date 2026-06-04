"""Phase 1: Scalar Sheaf Laplacian — spectral analysis.

Builds the sheaf Laplacian L0 from canonical restriction map ratios,
computes eigenvalues/eigenvectors, per-constraint obstruction energy E(C),
and analyzes correlation with H1 cohomological obstruction bands.
"""

import csv
from collections import defaultdict
from pathlib import Path

import numpy as np
from scipy import stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from .phase0_data import CONTEXTS


def build_laplacian(r12, r23, r34):
    """Build the 4x4 sheaf Laplacian L0 = delta0^T @ delta0 on path graph P4.

    delta0 is the 3x4 coboundary operator:
        row e12: [1, -r12, 0, 0]
        row e23: [0, 1, -r23, 0]
        row e34: [0, 0, 1, -r34]

    Returns L0 (4x4 numpy array).
    """
    delta0 = np.array([
        [1.0, -r12, 0.0, 0.0],
        [0.0, 1.0, -r23, 0.0],
        [0.0, 0.0, 1.0, -r34],
    ])
    L0 = delta0.T @ delta0
    return L0, delta0


def compute_eigendecomposition(L0):
    """Compute eigenvalues and eigenvectors of L0.

    Returns (eigenvalues sorted ascending, eigenvectors as columns).
    """
    eigenvalues, eigenvectors = np.linalg.eigh(L0)
    idx = np.argsort(eigenvalues)
    return eigenvalues[idx], eigenvectors[:, idx]


def compute_constraint_energy(chi_matrix, L0, delta0):
    """Compute obstruction energy E(C) for all constraints.

    E(C) = x^T L0 x = ||delta0 x||^2

    Also decomposes into per-edge energies.

    Returns dict with:
        E_total: (N,) array
        E_edges: (N, 3) array — per-edge disagreement squared
    """
    N = chi_matrix.shape[0]
    E_total = np.array([chi_matrix[i] @ L0 @ chi_matrix[i] for i in range(N)])

    # Per-edge decomposition: delta0 @ x gives 3 edge disagreements
    edge_disagreements = (delta0 @ chi_matrix.T).T  # (N, 3)
    E_edges = edge_disagreements ** 2  # (N, 3)

    return {"E_total": E_total, "E_edges": E_edges, "edge_disagreements": edge_disagreements}


def eigenvector_projections(chi_matrix, eigenvalues, eigenvectors):
    """Project each constraint's chi vector onto eigenbasis.

    Returns:
        projections: (N, 4) array of projection coefficients <x, v_k>
        energy_fractions: (N, 4) array of lambda_k * <x, v_k>^2 / E(C)
    """
    # projections[i, k] = <x_i, v_k>
    projections = chi_matrix @ eigenvectors  # (N, 4)

    # energy_fractions[i, k] = lambda_k * projections[i,k]^2 / E_total[i]
    E_total = np.sum(projections ** 2 * eigenvalues[np.newaxis, :], axis=1)
    E_total_safe = np.where(E_total > 1e-15, E_total, 1.0)

    energy_fractions = (projections ** 2 * eigenvalues[np.newaxis, :]) / E_total_safe[:, np.newaxis]

    return projections, energy_fractions


def run_phase1(data, output_dir):
    """Run the complete Phase 1 spectral analysis.

    Args:
        data: dict from phase0_data.load_audit_data()
        output_dir: Path to output directory

    Returns dict with all results for the report.
    """
    output_dir = Path(output_dir)
    constraints = data["constraints"]
    ratios = data["canonical_ratios"]
    r12, r23, r34 = ratios["r12"], ratios["r23"], ratios["r34"]

    from .phase0_data import extract_chi_matrix, extract_h1_vector, extract_epsilon_vector

    chi_matrix = extract_chi_matrix(constraints)
    h1_vec = extract_h1_vector(constraints)
    eps_vec = extract_epsilon_vector(constraints)
    N = len(constraints)

    # 1a: Build Laplacian and eigendecomposition
    L0, delta0 = build_laplacian(r12, r23, r34)
    eigenvalues, eigenvectors = compute_eigendecomposition(L0)

    # Standard path Laplacian eigenvalues for comparison
    std_eigenvalues = np.array([0.0, 2.0 - np.sqrt(2), 2.0, 2.0 + np.sqrt(2)])

    # Eigenvalue ratios
    lambda_ratios = {}
    if eigenvalues[1] > 1e-10:
        lambda_ratios["lambda3_over_lambda2"] = eigenvalues[2] / eigenvalues[1]
        lambda_ratios["lambda4_over_lambda2"] = eigenvalues[3] / eigenvalues[1]
    else:
        lambda_ratios["lambda3_over_lambda2"] = float("inf")
        lambda_ratios["lambda4_over_lambda2"] = float("inf")

    # 1b: Per-constraint energy
    energy_data = compute_constraint_energy(chi_matrix, L0, delta0)
    E_total = energy_data["E_total"]
    E_edges = energy_data["E_edges"]

    # Per-edge fraction
    E_total_safe = np.where(E_total > 1e-15, E_total, 1.0)
    edge_fractions = E_edges / E_total_safe[:, np.newaxis]

    # 1c: Eigenvector projections
    projections, energy_fractions = eigenvector_projections(chi_matrix, eigenvalues, eigenvectors)

    # --- Analysis: E(C) vs H1 ---
    h1_bands = sorted(set(h1_vec))
    energy_by_h1 = {}
    for h1 in h1_bands:
        mask = h1_vec == h1
        E_band = E_total[mask]
        energy_by_h1[int(h1)] = {
            "count": int(np.sum(mask)),
            "mean": float(np.mean(E_band)) if len(E_band) > 0 else 0.0,
            "std": float(np.std(E_band)) if len(E_band) > 0 else 0.0,
            "min": float(np.min(E_band)) if len(E_band) > 0 else 0.0,
            "max": float(np.max(E_band)) if len(E_band) > 0 else 0.0,
            "median": float(np.median(E_band)) if len(E_band) > 0 else 0.0,
        }

    # Correlations
    corr_pearson, p_pearson = stats.pearsonr(E_total, h1_vec) if N > 2 else (0, 1)
    corr_spearman, p_spearman = stats.spearmanr(E_total, h1_vec) if N > 2 else (0, 1)

    # Within d-pattern correlations
    d_pattern_correlations = {}
    for dp, indices in data["d_patterns"].items():
        if len(indices) > 5:
            E_sub = E_total[indices]
            h1_sub = h1_vec[indices]
            if np.std(h1_sub) > 0 and np.std(E_sub) > 0:
                r_s, p_s = stats.spearmanr(E_sub, h1_sub)
                d_pattern_correlations[str(dp)] = {
                    "n": len(indices),
                    "spearman_r": float(r_s),
                    "spearman_p": float(p_s),
                }

    # Cross-tabulation: E(C) by H1 x d-pattern
    cross_tab = defaultdict(dict)
    for dp, indices in data["d_patterns"].items():
        for h1 in h1_bands:
            mask = np.array([(i in indices and h1_vec[i] == h1) for i in range(N)])
            E_sub = E_total[mask]
            if len(E_sub) > 0:
                cross_tab[str(dp)][int(h1)] = {
                    "count": len(E_sub),
                    "mean_E": float(np.mean(E_sub)),
                }

    # Mean edge fractions by H1
    edge_fractions_by_h1 = {}
    for h1 in h1_bands:
        mask = h1_vec == h1
        ef = edge_fractions[mask]
        if len(ef) > 0:
            edge_fractions_by_h1[int(h1)] = {
                "edge12_frac": float(np.mean(ef[:, 0])),
                "edge23_frac": float(np.mean(ef[:, 1])),
                "edge34_frac": float(np.mean(ef[:, 2])),
            }

    # Mean eigenvector energy fractions by H1
    eigfrac_by_h1 = {}
    for h1 in h1_bands:
        mask = h1_vec == h1
        ef = energy_fractions[mask]
        if len(ef) > 0:
            eigfrac_by_h1[int(h1)] = [float(np.mean(ef[:, k])) for k in range(4)]

    # --- Save CSVs ---
    # laplacian_eigenvalues.csv
    with open(output_dir / "laplacian_eigenvalues.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["index", "eigenvalue", "std_path_eigenvalue"])
        for i in range(4):
            w.writerow([i + 1, eigenvalues[i], std_eigenvalues[i]])

    # constraint_energy.csv
    with open(output_dir / "constraint_energy.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "epsilon", "E_total", "E_edge12", "E_edge23",
                     "E_edge34", "h1_band", "d_pattern"])
        for i, c in enumerate(constraints):
            w.writerow([c["id"], c["epsilon"], E_total[i], E_edges[i, 0],
                        E_edges[i, 1], E_edges[i, 2], c["h1"], str(c["d_pattern"])])

    # energy_by_h1.csv
    with open(output_dir / "energy_by_h1.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["h1_band", "count", "mean_E", "std_E", "min_E", "max_E", "median_E"])
        for h1 in sorted(energy_by_h1):
            e = energy_by_h1[h1]
            w.writerow([h1, e["count"], e["mean"], e["std"], e["min"], e["max"], e["median"]])

    # eigenvector_projections.csv
    with open(output_dir / "eigenvector_projections.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id", "h1_band", "proj_v1", "proj_v2", "proj_v3", "proj_v4",
                     "efrac_v1", "efrac_v2", "efrac_v3", "efrac_v4"])
        for i, c in enumerate(constraints):
            w.writerow([c["id"], c["h1"],
                        projections[i, 0], projections[i, 1], projections[i, 2], projections[i, 3],
                        energy_fractions[i, 0], energy_fractions[i, 1],
                        energy_fractions[i, 2], energy_fractions[i, 3]])

    # --- Generate plots ---
    _generate_plots(output_dir, E_total, h1_vec, E_edges, eigenvalues, eigenvectors,
                    std_eigenvalues, constraints, data["d_patterns"])

    return {
        "L0": L0.tolist(),
        "delta0": delta0.tolist(),
        "eigenvalues": eigenvalues.tolist(),
        "eigenvectors": eigenvectors.tolist(),
        "std_eigenvalues": std_eigenvalues.tolist(),
        "spectral_gap": float(eigenvalues[1]) if eigenvalues[1] > 1e-10 else 0.0,
        "lambda_ratios": lambda_ratios,
        "restriction_ratios": {"r12": r12, "r23": r23, "r34": r34},
        "r_squared": {"r12_sq": r12**2, "r23_sq": r23**2, "r34_sq": r34**2},
        "energy_by_h1": energy_by_h1,
        "correlation": {
            "pearson_r": float(corr_pearson), "pearson_p": float(p_pearson),
            "spearman_r": float(corr_spearman), "spearman_p": float(p_spearman),
        },
        "d_pattern_correlations": d_pattern_correlations,
        "cross_tab": dict(cross_tab),
        "edge_fractions_by_h1": edge_fractions_by_h1,
        "eigfrac_by_h1": eigfrac_by_h1,
        "trace_L0": float(np.trace(L0)),
        "sum_eigenvalues": float(np.sum(eigenvalues)),
    }


def _generate_plots(output_dir, E_total, h1_vec, E_edges, eigenvalues, eigenvectors,
                    std_eigenvalues, constraints, d_patterns):
    """Generate all Phase 1 plots."""

    # Plot 1: Histogram of E(C)
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.hist(E_total, bins=50, edgecolor="black", alpha=0.7)
    ax.set_xlabel("Obstruction Energy E(C)")
    ax.set_ylabel("Count")
    ax.set_title("Distribution of Sheaf Laplacian Obstruction Energy")
    ax.axvline(np.median(E_total), color="red", linestyle="--", label=f"median={np.median(E_total):.4f}")
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_dir / "energy_histogram.png", dpi=150)
    plt.close(fig)

    # Plot 2: Box plot E(C) by H1 band
    h1_bands = sorted(set(h1_vec))
    box_data = [E_total[h1_vec == h1] for h1 in h1_bands]
    box_data = [d for d in box_data if len(d) > 0]
    box_labels = [str(h1) for h1 in h1_bands if np.sum(h1_vec == h1) > 0]

    fig, ax = plt.subplots(figsize=(10, 6))
    bp = ax.boxplot(box_data, labels=box_labels, patch_artist=True)
    for patch in bp["boxes"]:
        patch.set_facecolor("lightblue")
    ax.set_xlabel("H¹ Band")
    ax.set_ylabel("Obstruction Energy E(C)")
    ax.set_title("E(C) by Cohomological Obstruction Band")
    fig.tight_layout()
    fig.savefig(output_dir / "energy_by_h1_boxplot.png", dpi=150)
    plt.close(fig)

    # Plot 3: Scatter E(C) vs H1 with jitter
    fig, ax = plt.subplots(figsize=(10, 6))
    jitter = np.random.normal(0, 0.15, size=len(h1_vec))
    ax.scatter(h1_vec + jitter, E_total, alpha=0.4, s=20, c="steelblue")
    ax.set_xlabel("H¹ Band")
    ax.set_ylabel("Obstruction Energy E(C)")
    ax.set_title("E(C) vs H¹ (jittered)")
    fig.tight_layout()
    fig.savefig(output_dir / "energy_vs_h1_scatter.png", dpi=150)
    plt.close(fig)

    # Plot 4: Eigenvalue comparison
    fig, ax = plt.subplots(figsize=(8, 5))
    x = np.arange(1, 5)
    width = 0.35
    ax.bar(x - width / 2, eigenvalues, width, label="Sheaf Laplacian", color="steelblue")
    ax.bar(x + width / 2, std_eigenvalues, width, label="Standard Path P₄", color="coral")
    ax.set_xlabel("Eigenvalue Index")
    ax.set_ylabel("Eigenvalue")
    ax.set_title("Eigenvalue Comparison: Sheaf vs Standard Path Laplacian")
    ax.set_xticks(x)
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_dir / "eigenvalue_comparison.png", dpi=150)
    plt.close(fig)

    # Plot 5: Per-edge energy fraction by H1
    edge_labels = ["e₁₂ (pwl→mod)", "e₂₃ (mod→inst)", "e₃₄ (inst→ana)"]
    fig, ax = plt.subplots(figsize=(10, 6))
    for h1 in h1_bands:
        mask = h1_vec == h1
        if np.sum(mask) > 0:
            E_sub = E_edges[mask]
            E_tot_sub = E_total[mask]
            E_tot_safe = np.where(E_tot_sub > 1e-15, E_tot_sub, 1.0)
            fracs = np.mean(E_sub / E_tot_safe[:, np.newaxis], axis=0)
            ax.bar(np.arange(3) + h1 * 0.12, fracs, width=0.1, label=f"H¹={h1}")
    ax.set_xticks(np.arange(3) + 0.3)
    ax.set_xticklabels(edge_labels)
    ax.set_ylabel("Mean Energy Fraction")
    ax.set_title("Per-Edge Energy Fraction by H¹ Band")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(output_dir / "edge_fractions_by_h1.png", dpi=150)
    plt.close(fig)
