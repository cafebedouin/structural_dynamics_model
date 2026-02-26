#!/usr/bin/env python3
"""DR Framework Spectral & Geometric Audit — Main Orchestrator.

Runs all three audit phases and generates the summary report.

Usage:
    cd /home/scott/bin/structural_dynamics_model
    python3 audit/run_audit.py
"""

import json
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
AUDIT_DIR = ROOT / "audit"
OUTPUT_DIR = AUDIT_DIR / "outputs"

# Ensure imports work
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
if str(ROOT / "python") not in sys.path:
    sys.path.insert(0, str(ROOT / "python"))


def main():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print("=" * 60)
    print("DR Framework Spectral & Geometric Audit")
    print("=" * 60)

    # Phase 0: Load data
    print("\n[Phase 0] Loading data...")
    t0 = time.time()
    from audit.phase0_data import load_audit_data, validate_chi
    data = load_audit_data()
    n_valid, n_violations, violations = validate_chi(data)
    print(f"  Loaded {data['n_constraints']} constraints ({len(data['skipped'])} skipped)")
    print(f"  Chi validation: {n_valid} valid, {n_violations} violations")
    if violations:
        for v in violations[:5]:
            print(f"    VIOLATION: {v[0]} @ {v[1]}: expected={v[2]:.6f}, actual={v[3]:.6f}")
    print(f"  D-pattern groups: {len(data['d_patterns'])}")
    for dp, indices in sorted(data["d_patterns"].items(), key=lambda x: -len(x[1])):
        print(f"    {dp}: {len(indices)} constraints")
    print(f"  Phase 0 completed in {time.time() - t0:.1f}s")

    # Phase 1: Spectral Analysis
    print("\n[Phase 1] Scalar Sheaf Laplacian...")
    t1 = time.time()
    from audit.phase1_laplacian import run_phase1
    phase1 = run_phase1(data, OUTPUT_DIR)
    print(f"  Eigenvalues: {[f'{v:.4f}' for v in phase1['eigenvalues']]}")
    print(f"  Spectral gap λ₂ = {phase1['spectral_gap']:.4f}")
    print(f"  λ₃/λ₂ = {phase1['lambda_ratios'].get('lambda3_over_lambda2', 'N/A'):.4f}")
    print(f"  λ₄/λ₂ = {phase1['lambda_ratios'].get('lambda4_over_lambda2', 'N/A'):.4f}")
    print(f"  Restriction ratios: r₁₂={phase1['restriction_ratios']['r12']:.4f}, "
          f"r₂₃={phase1['restriction_ratios']['r23']:.4f}, "
          f"r₃₄={phase1['restriction_ratios']['r34']:.4f}")
    print(f"  E(C) vs H¹ correlation: Spearman r={phase1['correlation']['spearman_r']:.4f} "
          f"(p={phase1['correlation']['spearman_p']:.4e})")
    print(f"  Phase 1 completed in {time.time() - t1:.1f}s")

    # Phase 2: T13 Audit
    print("\n[Phase 2] T13 Fisher-Rao / Hellinger Audit...")
    t2 = time.time()
    from audit.phase2_t13 import run_phase2
    phase2 = run_phase2(data, OUTPUT_DIR)
    print(f"  Total: {phase2['n_total']}, H¹>0: {phase2['n_h1_positive']}")
    print(f"  T13 fires: {phase2['n_t13_firing']} "
          f"({phase2['t13_rate_of_h1_pos']*100:.1f}% of H¹>0)")
    print(f"  High asymmetry (>0.3): {phase2['n_high_asymmetry']}")
    og = phase2["oracle_gap"]
    print(f"  Oracle gap: {og['n_h1_pos_not_t13']} H¹>0 non-T13, "
          f"mean TVD={og['mean_tvd']:.4f}, "
          f"near threshold: {og['n_near_threshold']}")
    print(f"  Phase 2 completed in {time.time() - t2:.1f}s")

    # Phase 3: FCA
    print("\n[Phase 3] FCA Gate Compression...")
    t3 = time.time()
    from audit.phase3_fca import run_phase3
    phase3 = run_phase3(data, OUTPUT_DIR)
    print(f"  Total gates: {phase3['n_total_gates']}, non-constant: {phase3['n_non_constant']}")
    print(f"  GF(2) rank: {phase3['gf2']['rank']} / {phase3['gf2']['n_cols']}")
    print(f"  Null space dimension: {phase3['gf2']['null_dim']}")
    fca = phase3["fca"]
    if fca.get("error"):
        print(f"  FCA error: {fca['error']}")
    else:
        print(f"  Concept count: {fca['concept_count']}")
    print(f"  Phase 3 completed in {time.time() - t3:.1f}s")

    # Generate report
    print("\n[Report] Generating summary...")
    report = generate_report(data, phase1, phase2, phase3)
    report_path = OUTPUT_DIR / "spectral_audit_report.md"
    report_path.write_text(report, encoding="utf-8")
    print(f"  Report saved to {report_path}")

    total_time = time.time() - t0
    print(f"\n{'=' * 60}")
    print(f"Audit complete in {total_time:.1f}s")
    print(f"Outputs in {OUTPUT_DIR}")
    print(f"{'=' * 60}")


def generate_report(data, phase1, phase2, phase3):
    """Generate the full markdown audit report."""
    lines = []

    def w(s=""):
        lines.append(s)

    w("# DR Framework Spectral & Geometric Audit Report")
    w()

    # --- 1. Data Summary ---
    w("## 1. Data Summary")
    w()
    w(f"- **Corpus size:** {data['n_constraints']} constraints "
      f"({len(data['skipped'])} skipped for null chi)")
    w()

    # H1 distribution
    h1_counts = Counter(c["h1"] for c in data["constraints"])
    w("### H¹ Distribution")
    w()
    w("| H¹ | Count | Pct |")
    w("|-----|-------|-----|")
    for h1 in sorted(h1_counts):
        pct = h1_counts[h1] / data["n_constraints"] * 100
        w(f"| {h1} | {h1_counts[h1]} | {pct:.1f}% |")
    w()

    # Type distribution (analytical perspective)
    type_counts = Counter(c["types"][3] for c in data["constraints"])
    w("### Type Distribution (Analytical Perspective)")
    w()
    w("| Type | Count | Pct |")
    w("|------|-------|-----|")
    for t in sorted(type_counts, key=type_counts.get, reverse=True):
        pct = type_counts[t] / data["n_constraints"] * 100
        w(f"| {t} | {type_counts[t]} | {pct:.1f}% |")
    w()

    # D-pattern groups
    w("### Directionality Pattern Groups")
    w()
    for dp, indices in sorted(data["d_patterns"].items(), key=lambda x: -len(x[1])):
        w(f"- `{dp}`: {len(indices)} constraints")
    w()

    # --- 2. Phase 1: Spectral Analysis ---
    w("## 2. Scalar Sheaf Laplacian (Phase 1)")
    w()

    w("### Restriction Map Ratios")
    w()
    r = phase1["restriction_ratios"]
    w(f"- r₁₂ = σ(π(U₁))/σ(π(U₂)) = **{r['r12']:.6f}**")
    w(f"- r₂₃ = σ(π(U₂))/σ(π(U₃)) = **{r['r23']:.6f}**")
    w(f"- r₃₄ = σ(π(U₃))/σ(π(U₄)) = **{r['r34']:.6f}**")
    w()
    rsq = phase1["r_squared"]
    w(f"Squared ratios: r₁₂²={rsq['r12_sq']:.2f}, r₂₃²={rsq['r23_sq']:.2f}, "
      f"r₃₄²={rsq['r34_sq']:.2f}")
    w(f"  → Edge e₂₃ (moderate→institutional) carries "
      f"**{rsq['r23_sq']/(rsq['r12_sq']+rsq['r23_sq']+rsq['r34_sq'])*100:.0f}%** "
      f"of the Laplacian's spectral weight.")
    w()

    w("### Laplacian Matrix L₀")
    w()
    w("```")
    L = phase1["L0"]
    for row in L:
        w("  " + "  ".join(f"{v:10.4f}" for v in row))
    w("```")
    w()

    w("### Eigenvalue Spectrum")
    w()
    ev = phase1["eigenvalues"]
    sev = phase1["std_eigenvalues"]
    w("| Index | λ (Sheaf) | λ (Std Path P₄) | Ratio |")
    w("|-------|-----------|------------------|-------|")
    for i in range(4):
        ratio = ev[i] / sev[i] if sev[i] > 1e-10 else "∞" if ev[i] > 1e-10 else "0/0"
        if isinstance(ratio, float):
            w(f"| λ_{i+1} | {ev[i]:.6f} | {sev[i]:.6f} | {ratio:.2f} |")
        else:
            w(f"| λ_{i+1} | {ev[i]:.6f} | {sev[i]:.6f} | {ratio} |")
    w()

    w(f"**Spectral gap:** λ₂ = {phase1['spectral_gap']:.6f}")
    lr = phase1["lambda_ratios"]
    w(f"**Eigenvalue ratios:** λ₃/λ₂ = {lr.get('lambda3_over_lambda2', 'N/A'):.4f}, "
      f"λ₄/λ₂ = {lr.get('lambda4_over_lambda2', 'N/A'):.4f}")
    w()

    if lr.get("lambda3_over_lambda2", 0) and abs(lr["lambda3_over_lambda2"] - 1.0) < 0.1:
        w("*Non-dominant modes are near-degenerate — limited independent structure beyond "
          "the dominant moderate→institutional edge.*")
    elif lr.get("lambda3_over_lambda2", 0):
        w(f"*Eigenvalue ratios show separation (λ₃/λ₂ = {lr['lambda3_over_lambda2']:.2f}), "
          f"suggesting structure beyond the dominant edge.*")
    w()

    w(f"**Trace check:** tr(L₀) = {phase1['trace_L0']:.6f}, "
      f"Σλ = {phase1['sum_eigenvalues']:.6f} "
      f"(diff = {abs(phase1['trace_L0'] - phase1['sum_eigenvalues']):.2e})")
    w()

    w("### Eigenvectors")
    w()
    w("| Mode | Powerless | Moderate | Institutional | Analytical |")
    w("|------|-----------|----------|---------------|------------|")
    evec = phase1["eigenvectors"]
    for k in range(4):
        w(f"| v_{k+1} (λ={ev[k]:.4f}) | "
          + " | ".join(f"{evec[j][k]:.4f}" for j in range(4)) + " |")
    w()

    w("### E(C) vs H¹ Correlation")
    w()
    corr = phase1["correlation"]
    w(f"- **Pearson r:** {corr['pearson_r']:.4f} (p = {corr['pearson_p']:.2e})")
    w(f"- **Spearman ρ:** {corr['spearman_r']:.4f} (p = {corr['spearman_p']:.2e})")
    w()

    w("### E(C) by H¹ Band")
    w()
    w("| H¹ | Count | Mean E | Std E | Min E | Max E | Median E |")
    w("|-----|-------|--------|-------|-------|-------|----------|")
    for h1 in sorted(phase1["energy_by_h1"]):
        e = phase1["energy_by_h1"][h1]
        w(f"| {h1} | {e['count']} | {e['mean']:.4f} | {e['std']:.4f} | "
          f"{e['min']:.4f} | {e['max']:.4f} | {e['median']:.4f} |")
    w()

    # D-pattern correlations
    if phase1["d_pattern_correlations"]:
        w("### Within D-Pattern Correlations")
        w()
        w("| D-Pattern | n | Spearman ρ | p-value |")
        w("|-----------|---|-----------|---------|")
        for dp, c in phase1["d_pattern_correlations"].items():
            w(f"| {dp} | {c['n']} | {c['spearman_r']:.4f} | {c['spearman_p']:.2e} |")
        w()

    # Edge fractions
    if phase1["edge_fractions_by_h1"]:
        w("### Per-Edge Energy Fractions by H¹")
        w()
        w("| H¹ | e₁₂ (pwl→mod) | e₂₃ (mod→inst) | e₃₄ (inst→ana) |")
        w("|-----|---------------|----------------|----------------|")
        for h1 in sorted(phase1["edge_fractions_by_h1"]):
            ef = phase1["edge_fractions_by_h1"][h1]
            w(f"| {h1} | {ef['edge12_frac']:.4f} | {ef['edge23_frac']:.4f} | "
              f"{ef['edge34_frac']:.4f} |")
        w()

    # Eigenvector projections by H1
    if phase1["eigfrac_by_h1"]:
        w("### Mean Eigenvector Energy Fractions by H¹")
        w()
        w("| H¹ | Mode 1 | Mode 2 | Mode 3 | Mode 4 |")
        w("|-----|--------|--------|--------|--------|")
        for h1 in sorted(phase1["eigfrac_by_h1"]):
            ef = phase1["eigfrac_by_h1"][h1]
            w(f"| {h1} | " + " | ".join(f"{v:.4f}" for v in ef) + " |")
        w()

    w("![Energy Histogram](energy_histogram.png)")
    w("![Energy by H¹](energy_by_h1_boxplot.png)")
    w("![E(C) vs H¹](energy_vs_h1_scatter.png)")
    w("![Eigenvalue Comparison](eigenvalue_comparison.png)")
    w("![Edge Fractions](edge_fractions_by_h1.png)")
    w()

    # --- 3. Phase 2: T13 Audit ---
    w("## 3. T13 Fisher-Rao / Hellinger Audit (Phase 2)")
    w()
    w(f"- **Total constraints:** {phase2['n_total']}")
    w(f"- **H¹ > 0:** {phase2['n_h1_positive']}")
    w(f"- **H¹ = 0:** {phase2['n_h1_zero']}")
    w(f"- **T13 fires:** {phase2['n_t13_firing']} "
      f"({phase2['t13_rate_of_h1_pos']*100:.1f}% of H¹>0)")
    w()

    w("### T13-Firing Constraints")
    w()
    if phase2["t13_details"]:
        w("| Constraint | H¹ | Max TVD | Worst Ctx | d_FR | Asym | cl→ | idx→ |")
        w("|------------|-----|---------|-----------|------|------|-----|------|")
        for r in phase2["t13_details"]:
            w(f"| {r['constraint_id'][:35]} | {r['h1']} | {r['max_tvd']:.4f} | "
              f"{r['worst_context']} | {r['d_FR_worst']:.4f} | {r['asymmetry_ratio']:.2f} | "
              f"{r['argmax_cl']} | {r['argmax_idx']} |")
        if phase2["n_t13_firing"] > 20:
            w(f"*... and {phase2['n_t13_firing'] - 20} more*")
        w()

    w("### Asymmetry Audit")
    w()
    w(f"Constraints with KL asymmetry > 0.3: **{phase2['n_high_asymmetry']}**")
    if phase2["high_asymmetry_ids"]:
        w(f"IDs: {', '.join(phase2['high_asymmetry_ids'][:10])}")
    w()

    w("### Hellinger Decomposition Summary (T13 fires)")
    w()
    hd = phase2.get("hellinger_decomp_summary", {})
    if hd:
        w("| Type | Mean Fraction | Max Fraction |")
        w("|------|---------------|--------------|")
        for t in sorted(hd, key=lambda x: -hd[x]["mean"]):
            w(f"| {t} | {hd[t]['mean']:.4f} | {hd[t]['max']:.4f} |")
        w()
        w("*Shows which types carry the Hellinger divergence when T13 fires.*")
        w()

    w("### 100x Oracle Gap")
    w()
    og = phase2["oracle_gap"]
    w(f"- H¹>0 constraints NOT firing T13: **{og['n_h1_pos_not_t13']}**")
    w(f"- Mean max TVD (non-T13): {og['mean_tvd']:.4f}")
    w(f"- Median max TVD (non-T13): {og['median_tvd']:.4f}")
    w(f"- Near threshold (within 0.01 of 0.05): {og['n_near_threshold']}")
    w(f"- Mean d_FR (T13): {og['mean_dfr_t13']:.4f}")
    w(f"- Mean d_FR (non-T13): {og['mean_dfr_non_t13']:.4f}")
    if og["mean_dfr_t13"] > 0 and og["mean_dfr_non_t13"] > 0:
        separation = og["mean_dfr_t13"] / og["mean_dfr_non_t13"]
        w(f"- **Separation ratio:** {separation:.2f}x")
    w()

    w("![Oracle Gap](oracle_gap_histogram.png)")
    w("![Fisher-Rao Comparison](fisher_rao_comparison.png)")
    w("![TVD by H¹](tvd_by_h1.png)")
    w()

    # --- 4. Phase 3: FCA ---
    w("## 4. FCA Gate Compression (Phase 3)")
    w()
    w(f"- **Total gates:** {phase3['n_total_gates']}")
    w(f"- **Non-constant gates:** {phase3['n_non_constant']}")
    if phase3["constant_gates"]:
        w(f"- **Constant gates (removed):** {', '.join(phase3['constant_gates'][:10])}")
        if len(phase3["constant_gates"]) > 10:
            w(f"  *... and {len(phase3['constant_gates']) - 10} more*")
    w()

    w("**Gate count gap:** The paper references ~65 binary structural gates; "
      f"this audit extracts {phase3['n_total_gates']} from the enriched pipeline JSON. "
      "The gap represents gates computed inside Prolog modules "
      "(immutability, temporality checks, etc.) not surfaced in the JSON export. "
      f"These {phase3['n_total_gates']} gates are a lower bound on the actual gate space.")
    w()

    w("### GF(2) Rank")
    w()
    gf2 = phase3["gf2"]
    w(f"- **Rank over GF(2):** {gf2['rank']} / {gf2['n_cols']} "
      f"({gf2['n_cols'] - gf2['rank']} redundant)")
    w(f"- **Null space dimension:** {gf2['null_dim']}")
    w(f"- **Compression ratio:** {gf2['rank'] / gf2['n_cols'] * 100:.0f}% "
      f"of gates are linearly independent over GF(2)")
    w()

    w("### Concept Lattice")
    w()
    fca = phase3["fca"]
    if fca.get("error"):
        w(f"**FCA error:** {fca['error']}")
    else:
        w(f"- **Concept count:** {fca['concept_count']}")
        w(f"- **Key concepts (non-trivial extent+intent):** {fca['n_key_concepts']}")
        w()
        if fca["top_concepts"]:
            w("#### Top Concepts by Extent Size")
            w()
            w("| Extent | Intent | Sample Gates |")
            w("|--------|--------|-------------|")
            for c in fca["top_concepts"][:15]:
                gates_str = ", ".join(c["intent_gates"][:5])
                if len(c["intent_gates"]) > 5:
                    gates_str += "..."
                w(f"| {c['extent_size']} | {c['intent_size']} | {gates_str} |")
            w()
    w()

    w("### Type-Relative Separation")
    w()
    if phase3["reducts"]:
        w("| Type Pair | n₁ | n₂ | Perfect Sep | Partial Sep | Top Separating Gates |")
        w("|-----------|----|----|-------------|-------------|---------------------|")
        for pair in sorted(phase3["reducts"]):
            r = phase3["reducts"][pair]
            top_gates = [g[0] for g in r.get("top_partial", [])][:3]
            w(f"| {pair} | {r['n_type1']} | {r['n_type2']} | "
              f"{r['n_perfect_separating']} | {r['n_partial_separating']} | "
              f"{', '.join(top_gates)} |")
        w()

    # --- 5. Cross-Phase Synthesis ---
    w("## 5. Cross-Phase Synthesis")
    w()

    # Find constraints appearing in multiple interesting categories
    t13_ids = set(phase2.get("t13_firing_ids", []))
    high_h1_ids = set(c["id"] for c in data["constraints"] if c["h1"] >= 5)
    overlap = t13_ids & high_h1_ids
    w(f"- Constraints with both T13 fire AND H¹≥5: **{len(overlap)}**")
    if overlap:
        w(f"  {', '.join(sorted(overlap)[:10])}")
    w()

    # Summary of what the indexing machinery actually contributes
    w("### Key Findings")
    w()
    w(f"1. **Spectral dominance:** The moderate→institutional edge (r₂₃²≈"
      f"{phase1['r_squared']['r23_sq']:.0f}) carries the vast majority of the "
      "Laplacian's spectral weight. This confirms the known institutional phase "
      "transition as the primary architectural feature.")
    w()
    w(f"2. **E(C) vs H¹:** Spearman ρ = {phase1['correlation']['spearman_r']:.4f} "
      f"(p = {phase1['correlation']['spearman_p']:.2e}). "
      f"{'Strong' if abs(phase1['correlation']['spearman_r']) > 0.5 else 'Moderate' if abs(phase1['correlation']['spearman_r']) > 0.3 else 'Weak'} "
      "correlation between obstruction energy and cohomological band.")
    w()
    w(f"3. **T13 precision:** {phase2['n_t13_firing']} / {phase2['n_h1_positive']} "
      f"H¹>0 constraints fire T13 ({phase2['t13_rate_of_h1_pos']*100:.1f}%). "
      f"Oracle gap: {og['n_h1_pos_not_t13']} H¹>0 constraints below threshold.")
    w()
    w(f"4. **Gate compression:** GF(2) rank {gf2['rank']}/{gf2['n_cols']} — "
      f"{gf2['null_dim']} gates are redundant over GF(2). "
      f"Concept lattice has {fca.get('concept_count', '?')} formal concepts.")
    w()

    # --- 6. Limitations ---
    w("## 6. Limitations and Caveats")
    w()
    w("1. **Gate coverage:** Only ~" + str(phase3['n_total_gates']) +
      " of ~65 structural gates extractable from JSON. "
      "Internal Prolog gates (immutability, temporality) not surfaced.")
    w()
    w("2. **MaxEnt replication:** Python MaxEnt uses epsilon-calibrated profiles "
      "for indexed (chi) distributions. This creates systematic shift for "
      "chi values far from epsilon range (especially institutional chi < 0).")
    w()
    w("3. **E(C) interpretation:** Obstruction energy conflates directionality "
      "deviation (per-constraint d ≠ canonical d) with genuine observer-dependence. "
      "Within-d-pattern correlations should be checked for cleaner signal.")
    w()
    w("4. **Spectral dominance:** The r₂₃² ≈ "
      f"{phase1['r_squared']['r23_sq']:.0f} term dominates. "
      "Residual spectral structure may not carry independent information "
      "beyond the known institutional phase transition.")
    w()

    return "\n".join(lines)


if __name__ == "__main__":
    main()
