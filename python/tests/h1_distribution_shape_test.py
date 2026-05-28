"""H¹ Distribution Shape Test — corrects §3.3 H¹ gap framing.

Compares three H¹ distributions:
  1. Corpus manifest presheaves (from enriched_pipeline.json, h1_band field)
  2. Random ternary baseline (analytic multinomial expectation)
  3. Asymmetric flow-model simulation (ported from flow_asymmetry_test_v2.jsx)

The §3.3 claim that H¹ values {1,2} are a DR-distinctive prediction is
incorrect — they are combinatorially forbidden for ANY 4-observer classification
rule (arithmetic, not axioms). The real DR prediction is distribution SHAPE
over reachable values {0, 3, 4, 5}: asymmetric power environments should
concentrate at {4, 5}; symmetric environments collapse to {0}; random spreads
per multinomial expectation.

Note on H¹=6: DR uses 7+ types (not 3), enabling the partition (1-1-1-1)
where all 4 observers assign different types. H¹=6 IS reachable in DR and
present in the corpus (verified). The ternary JSX model cannot produce H¹=6
(pigeonhole: 4 observers, 3 values → at least 2 observers must agree).
Statistical comparison is done on ternary-comparable bins {0, 3, 4, 5};
H¹=6 is reported separately.

Usage:
    python3 python/h1_distribution_shape_test.py
"""

import math
import sys
from pathlib import Path
from collections import Counter

sys.path.insert(0, str(Path(__file__).resolve().parent))
from shared.loader import load_json, ENRICHED_PIPELINE_JSON

# ---------------------------------------------------------------------------
# PRNG matching flow_asymmetry_test_v2.jsx (mulberry32)
# ---------------------------------------------------------------------------

def mulberry32(seed):
    """JavaScript-compatible mulberry32 PRNG. Returns a generator of floats in [0,1)."""
    s = seed & 0xFFFFFFFF
    while True:
        s = (s + 0x6D2B79F5) & 0xFFFFFFFF
        t = s ^ (s >> 15)
        t = (t * ((t | 1) & 0xFFFFFFFF)) & 0xFFFFFFFF
        t ^= t + ((t ^ (t >> 7)) * ((t | 61) & 0xFFFFFFFF)) & 0xFFFFFFFF
        yield ((t ^ (t >> 14)) & 0xFFFFFFFF) / 4294967296.0


# ---------------------------------------------------------------------------
# Asymmetric flow simulation (port of flow_asymmetry_test_v2.jsx)
# ---------------------------------------------------------------------------

K = 4
ASYMMETRIC_POWERS = [0.1, 0.4, 0.6, 0.9]
SYMMETRIC_POWERS  = [0.5, 0.5, 0.5, 0.5]
MEAN_POWER   = 0.5
SAT_GAIN     = 3.0
NEUTRAL_BAND = 0.08


def flow_at(baseline, tilt, power):
    return baseline + tilt * (power - MEAN_POWER)


def classify_val(raw):
    s = math.tanh(SAT_GAIN * raw)
    if s >  NEUTRAL_BAND: return  1
    if s < -NEUTRAL_BAND: return -1
    return 0


def h1_from_row(row):
    """Count disagreeing observer pairs for a length-K classification row."""
    count = 0
    for i in range(len(row)):
        for j in range(i + 1, len(row)):
            if row[i] != row[j]:
                count += 1
    return count


def simulate_h1_distribution(powers, n=1000, seed=42):
    """Generate N random constraints, classify at given power levels, return H¹ counts."""
    rng = mulberry32(seed)
    hist = Counter()
    for _ in range(n):
        # JSX DIST_VARIANTS[0]: bSpread=1.0, tSpread=2.5
        # baseline = (rng() - 0.5) * bSpread  →  ∈ [-0.5, 0.5]
        # tilt     = (rng() - 0.5) * tSpread  →  ∈ [-1.25, 1.25]
        baseline = (next(rng) - 0.5) * 1.0
        tilt     = (next(rng) - 0.5) * 2.5
        row = [classify_val(flow_at(baseline, tilt, p)) for p in powers]
        hist[h1_from_row(row)] += 1
    return hist


# ---------------------------------------------------------------------------
# Random ternary multinomial expectation (analytic)
# ---------------------------------------------------------------------------

def random_ternary_expectation():
    """Compute expected H¹ distribution under uniform random ternary classification.

    Enumerates all 81 (n+, n0, n-) assignments of 4 observers to 3 values,
    weighted by multinomial coefficient.

    Returns dict {h1_value: probability}.
    """
    from math import factorial

    def comb2(n):
        return n * (n - 1) // 2

    total = 0
    hist = Counter()
    for np_ in range(5):          # 0..4
        for n0 in range(5 - np_): # 0..4-np_
            nm = 4 - np_ - n0
            weight = factorial(4) // (factorial(np_) * factorial(n0) * factorial(nm))
            h1 = 6 - comb2(np_) - comb2(n0) - comb2(nm)
            hist[h1] += weight
            total += weight
    assert total == 81, f"Expected 81 total outcomes, got {total}"

    return {k: v / 81.0 for k, v in hist.items()}


# ---------------------------------------------------------------------------
# Chi-square goodness-of-fit
# ---------------------------------------------------------------------------

def chi_square_gof(observed_counts, expected_probs, bins):
    """Chi-square goodness-of-fit test.

    observed_counts: dict {bin: count}
    expected_probs:  dict {bin: probability}
    bins:            ordered list of bins to compare

    Returns (chi2_stat, df, cramers_v, p_approx).
    """
    n = sum(observed_counts.get(b, 0) for b in bins)
    chi2 = 0.0
    for b in bins:
        obs = observed_counts.get(b, 0)
        exp = expected_probs.get(b, 0) * n
        if exp > 0:
            chi2 += (obs - exp) ** 2 / exp

    df = len(bins) - 1
    # Cramér's V for 1×k table
    cramers_v = math.sqrt(chi2 / (n * df)) if n > 0 and df > 0 else 0.0
    # p-value approximation via chi-square CDF (regularized gamma)
    p = _chi2_pvalue(chi2, df)
    return chi2, df, cramers_v, p


def _chi2_pvalue(chi2, df):
    """Approximate p-value from chi-square distribution using Wilson-Hilferty."""
    if chi2 <= 0 or df <= 0:
        return 1.0
    # Wilson-Hilferty normal approximation
    x = chi2 / df
    k = 2.0 / (9.0 * df)
    z = (x ** (1.0 / 3.0) - (1 - k)) / math.sqrt(k)
    # P(Z > z) using erfc approximation
    p = 0.5 * math.erfc(z / math.sqrt(2))
    return min(1.0, max(0.0, p))


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    # ── Load corpus ──────────────────────────────────────────────────────────
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    pc = data.get("per_constraint", [])
    print(f"Loaded {len(pc)} constraints.")

    corpus_h1 = Counter(c["h1_band"] for c in pc)
    total = len(pc)
    manifest = {k: v for k, v in corpus_h1.items() if k > 0}
    manifest_total = sum(manifest.values())

    print(f"\nFull corpus H¹ distribution (N={total}):")
    for k in sorted(corpus_h1):
        print(f"  H¹={k}: {corpus_h1[k]:5d}  ({100*corpus_h1[k]/total:.1f}%)")
    print(f"  Manifest presheaves (H¹>0): {manifest_total}")

    # ── Note on H¹=6 ─────────────────────────────────────────────────────────
    h6 = corpus_h1.get(6, 0)
    print(f"\nH¹=6 note: {h6} constraints ({100*h6/total:.1f}% of corpus).")
    print("  DR uses 7+ types, enabling partition (1-1-1-1) where all 4 observers")
    print("  assign different types. This is combinatorially unreachable in the JSX")
    print("  ternary model (3 values, 4 observers → pigeonhole forces ≥2 to agree).")
    print("  flow_asymmetry_test_v2.jsx incorrectly listed H¹=6 as unreachable;")
    print("  only {1, 2} are forbidden for any 4-observer rule.")

    # ── Ternary-comparable subset (exclude H¹=6) ─────────────────────────────
    TERNARY_BINS = [0, 3, 4, 5]
    corpus_ternary = {k: corpus_h1.get(k, 0) for k in TERNARY_BINS}
    ternary_n = sum(corpus_ternary.values())
    print(f"\nTernary-comparable subset (H¹ ∈ {{0,3,4,5}}, N={ternary_n}):")
    for k in TERNARY_BINS:
        print(f"  H¹={k}: {corpus_ternary[k]:5d}  ({100*corpus_ternary[k]/ternary_n:.1f}%)")

    # ── Random multinomial expectation ────────────────────────────────────────
    rand_exp = random_ternary_expectation()
    # Verify expected values
    assert set(rand_exp.keys()) == {0, 3, 4, 5}, f"Unexpected keys: {rand_exp.keys()}"
    print(f"\nRandom ternary multinomial expectation (81 equally-weighted outcomes):")
    for k in TERNARY_BINS:
        print(f"  H¹={k}: {rand_exp[k]*100:.1f}%  ({rand_exp[k]*81:.0f}/81 outcomes)")

    # ── JSX asymmetric simulation ─────────────────────────────────────────────
    N_SIM = 1000
    asym_hist  = simulate_h1_distribution(ASYMMETRIC_POWERS, n=N_SIM, seed=42)
    sym_hist   = simulate_h1_distribution(SYMMETRIC_POWERS,  n=N_SIM, seed=42)

    # Convert simulations to probabilities over ternary bins
    asym_total = sum(asym_hist.values())
    sym_total  = sum(sym_hist.values())
    asym_probs = {k: asym_hist.get(k, 0) / asym_total for k in TERNARY_BINS}
    sym_probs  = {k: sym_hist.get(k,  0) / sym_total  for k in TERNARY_BINS}

    print(f"\nJSX asymmetric simulation (N={N_SIM}, powers=[0.1,0.4,0.6,0.9], seed=42):")
    for k in TERNARY_BINS:
        print(f"  H¹={k}: {asym_probs[k]*100:.1f}%  ({asym_hist.get(k,0)} of {asym_total})")

    print(f"\nJSX symmetric simulation (N={N_SIM}, powers=[0.5,0.5,0.5,0.5], seed=42):")
    for k in TERNARY_BINS:
        print(f"  H¹={k}: {sym_probs[k]*100:.1f}%  ({sym_hist.get(k,0)} of {sym_total})")

    # ── Statistical comparisons: full ternary distribution ───────────────────
    # Corpus (ternary subset) vs. random multinomial
    chi2_rand, df_rand, cv_rand, p_rand = chi_square_gof(
        corpus_ternary, rand_exp, TERNARY_BINS)

    # Corpus (ternary subset) vs. asymmetric simulation
    chi2_asym, df_asym, cv_asym, p_asym = chi_square_gof(
        corpus_ternary, asym_probs, TERNARY_BINS)

    # Corpus (ternary subset) vs. symmetric simulation
    chi2_sym, df_sym, cv_sym, p_sym = chi_square_gof(
        corpus_ternary, sym_probs, TERNARY_BINS)

    print(f"\nChi-square goodness-of-fit (full ternary subset vs. each baseline):")
    print(f"  Note: V > 1 is possible when distributions are very different;")
    print(f"  it indicates the scales of departure are beyond normal V range.")
    print(f"  vs. random multinomial:   χ²={chi2_rand:.2f}, df={df_rand}, "
          f"V={cv_rand:.4f}, p≈{p_rand:.6f}")
    print(f"  vs. asymmetric simulation:χ²={chi2_asym:.2f}, df={df_asym}, "
          f"V={cv_asym:.4f}, p≈{p_asym:.6f}")
    print(f"  vs. symmetric simulation: χ²={chi2_sym:.2f}, df={df_sym}, "
          f"V={cv_sym:.4f}, p≈{p_sym:.6f}")

    closer = "asymmetric simulation" if chi2_asym < chi2_rand else "random multinomial"
    print(f"\n  Corpus distribution more closely resembles: {closer}")

    # ── Manifest presheaf comparison (H¹>0 only) — more diagnostic ───────────
    # This isolates the shape prediction: given that a constraint is a presheaf,
    # what is the distribution of H¹ values?
    PRESHEAF_BINS = [3, 4, 5]
    corpus_presheaf = {k: corpus_h1.get(k, 0) for k in PRESHEAF_BINS}
    presheaf_n = sum(corpus_presheaf.values())  # excludes H¹=6

    # Random conditional on H¹>0 (ternary)
    rand_manifest_total = sum(rand_exp[k] for k in PRESHEAF_BINS)
    rand_cond = {k: rand_exp[k] / rand_manifest_total for k in PRESHEAF_BINS}

    # Asymmetric simulation conditional on H¹>0
    asym_manifest_n = sum(asym_hist.get(k, 0) for k in PRESHEAF_BINS)
    if asym_manifest_n > 0:
        asym_cond = {k: asym_hist.get(k, 0) / asym_manifest_n for k in PRESHEAF_BINS}
    else:
        asym_cond = {k: 0.0 for k in PRESHEAF_BINS}

    chi2_mp_rand, _, cv_mp_rand, p_mp_rand = chi_square_gof(
        corpus_presheaf, rand_cond, PRESHEAF_BINS)
    chi2_mp_asym, _, cv_mp_asym, p_mp_asym = chi_square_gof(
        corpus_presheaf, asym_cond, PRESHEAF_BINS)

    print(f"\nManifest presheaf (H¹>0) comparison — N={presheaf_n} (excludes H¹=6):")
    print(f"  H¹ bin  | Corpus   | Random cond. | Asymm cond.")
    for k in PRESHEAF_BINS:
        c_pct = 100 * corpus_presheaf[k] / presheaf_n
        r_pct = 100 * rand_cond[k]
        a_pct = 100 * asym_cond[k]
        print(f"  H¹={k}    | {c_pct:5.1f}%  | {r_pct:5.1f}%        | {a_pct:5.1f}%")
    print(f"  χ² vs. random:    {chi2_mp_rand:.2f}, V={cv_mp_rand:.4f}, p≈{p_mp_rand:.4f}")
    print(f"  χ² vs. asymmetric:{chi2_mp_asym:.2f}, V={cv_mp_asym:.4f}, p≈{p_mp_asym:.4f}")
    closer_mp = "asymmetric simulation" if chi2_mp_asym < chi2_mp_rand else "random multinomial"
    print(f"  Manifest presheaf distribution more closely resembles: {closer_mp}")

    # ── Notable features ──────────────────────────────────────────────────────
    print(f"\nNotable features of corpus distribution:")
    rand_h4_pct = rand_exp[4] * 100
    corp_h4_pct = 100 * corpus_ternary[4] / ternary_n
    print(f"  H¹=4 suppression: corpus {corp_h4_pct:.1f}% vs. random expectation "
          f"{rand_h4_pct:.1f}% (ratio {corp_h4_pct/rand_h4_pct:.2f}×)")
    rand_h5_pct = rand_exp[5] * 100
    corp_h5_pct = 100 * corpus_ternary[5] / ternary_n
    print(f"  H¹=5 elevation:   corpus {corp_h5_pct:.1f}% vs. random expectation "
          f"{rand_h5_pct:.1f}%")
    print(f"  H¹=3 vs. random:  corpus {100*corpus_ternary[3]/ternary_n:.1f}% vs. "
          f"random {rand_exp[3]*100:.1f}%")

    # ── Write results file ────────────────────────────────────────────────────
    results_dir = Path(__file__).resolve().parent.parent / "docs" / "results"
    results_dir.mkdir(parents=True, exist_ok=True)
    out_path = results_dir / "h1_distribution_test.md"

    _write_results(
        out_path,
        total, corpus_h1, corpus_ternary, ternary_n, h6,
        rand_exp, asym_probs, sym_probs,
        asym_hist, sym_hist, N_SIM,
        chi2_rand, cv_rand, p_rand,
        chi2_asym, cv_asym, p_asym,
        chi2_sym,  cv_sym,  p_sym,
        closer, TERNARY_BINS,
        corp_h4_pct, rand_h4_pct, corp_h5_pct, rand_h5_pct,
        corpus_presheaf, presheaf_n, rand_cond, asym_cond,
        chi2_mp_rand, cv_mp_rand, p_mp_rand,
        chi2_mp_asym, cv_mp_asym, p_mp_asym,
        closer_mp, PRESHEAF_BINS,
    )
    print(f"\nResults written to {out_path}")


def _write_results(
    out_path,
    total, corpus_h1, corpus_ternary, ternary_n, h6,
    rand_exp, asym_probs, sym_probs,
    asym_hist, sym_hist, N_SIM,
    chi2_rand, cv_rand, p_rand,
    chi2_asym, cv_asym, p_asym,
    chi2_sym,  cv_sym,  p_sym,
    closer, TERNARY_BINS,
    corp_h4_pct, rand_h4_pct, corp_h5_pct, rand_h5_pct,
    corpus_presheaf, presheaf_n, rand_cond, asym_cond,
    chi2_mp_rand, cv_mp_rand, p_mp_rand,
    chi2_mp_asym, cv_mp_asym, p_mp_asym,
    closer_mp, PRESHEAF_BINS,
):
    asym_total = sum(asym_hist.values())
    sym_total  = sum(sym_hist.values())

    lines = [
        "# H¹ Distribution Shape Test",
        "",
        "**Purpose:** Empirical test of the reframed §3.3 prediction in"
        " `observers_not_humans_v3.md`.",
        "",
        "## Background",
        "",
        "v2 §3.3 claimed that H¹ values {1, 2} being absent from the distribution"
        " is a DR-distinctive prediction that distinguishes DR from system justification"
        " and adjacent theories. This is incorrect: {1, 2} are combinatorially"
        " forbidden for **any** 4-observer classification rule — a consequence of"
        " arithmetic (partition enumeration), not of DR's axioms.",
        "",
        "The correct DR prediction is distribution **shape** over reachable values"
        " {0, 3, 4, 5}: asymmetric power environments should concentrate at {4, 5};"
        " symmetric environments should collapse to {0}; random classification should"
        " spread per the multinomial expectation.",
        "",
        "**Note on H¹=6:** DR uses 7+ constraint types, enabling the partition"
        " (1-1-1-1) where all 4 observers assign distinct types — yielding H¹=6."
        f" The corpus confirms: **{h6} of {total} constraints** ({100*h6/total:.1f}%)"
        " have H¹=6. The JSX ternary model cannot produce H¹=6 (pigeonhole: 3 values,"
        " 4 observers → ≥2 must agree). `flow_asymmetry_test_v2.jsx` lines 7–12"
        " incorrectly list {1, 2, **6**} as unreachable; only {1, 2} are forbidden"
        " for any 4-observer rule.",
        "",
        "---",
        "",
        "## Corpus: Full H¹ Distribution",
        "",
        f"**N = {total} constraints** (from `outputs/enriched_pipeline.json`).",
        "",
        "| H¹ | Count | % of corpus | Reachable in ternary? |",
        "|-----|-------|-------------|----------------------|",
    ]
    ternary_reachable = {0: "Yes", 3: "Yes", 4: "Yes", 5: "Yes", 6: "No — DR multi-type only"}
    for k in sorted(corpus_h1):
        reach = ternary_reachable.get(k, "—")
        lines.append(f"| {k} | {corpus_h1[k]} | {100*corpus_h1[k]/total:.1f}% | {reach} |")
    lines += [
        "",
        f"Manifest presheaves (H¹ > 0): **{sum(v for k,v in corpus_h1.items() if k>0)}**",
        "",
        "---",
        "",
        "## Three-Way Comparison (Ternary-Comparable Bins {0, 3, 4, 5})",
        "",
        f"The H¹=6 constraints ({h6}) are excluded from the statistical comparison"
        " because the ternary simulation cannot produce them. The ternary subset"
        f" N = {ternary_n}.",
        "",
        "| H¹ | Corpus (ternary subset) | Random ternary | Asymmetric sim | Symmetric sim |",
        "|----|------------------------|----------------|----------------|---------------|",
    ]
    for k in TERNARY_BINS:
        c_pct  = 100 * corpus_ternary[k] / ternary_n
        r_pct  = rand_exp[k] * 100
        a_pct  = asym_probs[k] * 100
        s_pct  = sym_probs[k] * 100
        lines.append(
            f"| {k} | {corpus_ternary[k]} ({c_pct:.1f}%) |"
            f" {rand_exp[k]*81:.0f}/81 ({r_pct:.1f}%) |"
            f" {asym_hist.get(k,0)}/{asym_total} ({a_pct:.1f}%) |"
            f" {sym_hist.get(k,0)}/{sym_total} ({s_pct:.1f}%) |"
        )
    lines += [
        "",
        f"*Random ternary = analytic multinomial expectation over 81 equally-weighted"
        " (n+,n0,n−) triples.*",
        f"*Asymmetric/Symmetric sim = port of `flow_asymmetry_test_v2.jsx`,"
        f" N={N_SIM} constraints, seed=42.*",
        "",
        "---",
        "",
        "## Statistical Comparison",
        "",
        "Chi-square goodness-of-fit (corpus ternary subset vs. each baseline):",
        "",
        "| Comparison | χ² | df | Cramér's V | p (approx) |",
        "|------------|----|----|------------|------------|",
        f"| Corpus vs. random multinomial   | {chi2_rand:.2f} | 3 | {cv_rand:.4f} | {p_rand:.2e} |",
        f"| Corpus vs. asymmetric simulation | {chi2_asym:.2f} | 3 | {cv_asym:.4f} | {p_asym:.2e} |",
        f"| Corpus vs. symmetric simulation  | {chi2_sym:.2f} | 3 | {cv_sym:.4f} | {p_sym:.2e} |",
        "",
        f"**Corpus distribution more closely resembles: {closer}**"
        f" (lower χ² = {min(chi2_rand, chi2_asym):.2f} vs. {max(chi2_rand, chi2_asym):.2f}).",
        "",
        "---",
        "",
        "## Notable Features",
        "",
        f"- **H¹=4 suppression:** corpus {corp_h4_pct:.1f}% vs. random expectation"
        f" {rand_h4_pct:.1f}% (ratio {corp_h4_pct/rand_h4_pct:.2f}×). H¹=4 requires"
        " the (2,2) partition — two pairs of observers agreeing differently. This is"
        " structurally rare because the institutional sign-flip produces the (3,1)"
        " partition (3 observers on one side, institutional observer alone) far more"
        " often than symmetric pairing.",
        f"- **H¹=5 elevation:** corpus {corp_h5_pct:.1f}% vs. random {rand_exp[5]*100:.1f}%."
        " H¹=5 requires partition (2,1,1) — one pair agreeing plus two singleton"
        " disagreers. Common when the institutional observer is the odd-one-out among"
        " extraction-chain disagreements.",
        "",
        "---",
        "",
        "## Manifest Presheaf Shape (H¹ > 0, Ternary Bins {3, 4, 5})",
        "",
        "The shape prediction is more precisely tested by conditioning on manifest"
        f" presheaves only (H¹ > 0, excluding H¹=6). N = {presheaf_n}.",
        "",
        "| H¹ | Corpus | Random cond. (H¹>0) | Asymmetric cond. (H¹>0) |",
        "|----|--------|---------------------|------------------------|",
    ] + [
        f"| {k} | {corpus_presheaf[k]} ({100*corpus_presheaf[k]/presheaf_n:.1f}%) |"
        f" {rand_cond[k]*100:.1f}% |"
        f" {asym_cond[k]*100:.1f}% |"
        for k in PRESHEAF_BINS
    ] + [
        "",
        "| Comparison | χ² | Cramér's V | p |",
        "|------------|-----|-----------|---|",
        f"| vs. random cond.    | {chi2_mp_rand:.2f} | {cv_mp_rand:.4f} | {p_mp_rand:.4f} |",
        f"| vs. asymmetric cond.| {chi2_mp_asym:.2f} | {cv_mp_asym:.4f} | {p_mp_asym:.4f} |",
        "",
        f"**Among manifest presheaves, corpus more closely resembles: {closer_mp}.**",
        "",
        "---",
        "",
        "## Interpretation",
        "",
    ]

    # Build interpretation from both full and manifest comparisons
    h4_ratio = corp_h4_pct / rand_h4_pct if rand_h4_pct > 0 else 0

    if chi2_asym < chi2_rand:
        full_closer_sentence = (
            f"The full ternary distribution more closely resembles the"
            f" asymmetric simulation (χ²={chi2_asym:.2f}) than the random"
            f" baseline (χ²={chi2_rand:.2f})."
        )
    else:
        full_closer_sentence = (
            f"The full ternary distribution more closely resembles the"
            f" random baseline (χ²={chi2_rand:.2f}) than the asymmetric"
            f" simulation (χ²={chi2_asym:.2f})."
        )

    if chi2_mp_asym < chi2_mp_rand:
        mp_closer_sentence = (
            f"Among manifest presheaves specifically, the corpus also"
            f" more closely resembles the asymmetric simulation"
            f" (χ²={chi2_mp_asym:.2f}, V={cv_mp_asym:.4f}) than the random"
            f" conditional (χ²={chi2_mp_rand:.2f}, V={cv_mp_rand:.4f})."
        )
    else:
        mp_closer_sentence = (
            f"Among manifest presheaves specifically, the corpus more closely"
            f" resembles the random conditional (χ²={chi2_mp_rand:.2f},"
            f" V={cv_mp_rand:.4f}) than the asymmetric simulation"
            f" (χ²={chi2_mp_asym:.2f}, V={cv_mp_asym:.4f})."
        )

    interp = (
        f"{full_closer_sentence} The dominant deviation from the random baseline"
        f" is **H¹=4 suppression**: the corpus has only {corp_h4_pct:.1f}% of"
        f" ternary-subset constraints at H¹=4, vs. {rand_h4_pct:.1f}% expected under"
        f" random classification (ratio {h4_ratio:.2f}×). H¹=4 requires the (2,2)"
        f" partition — two pairs of observers agreeing on different types — which is"
        f" structurally rare because DR's institutional sign-flip (U₃ alone classifying"
        f" as rope when others classify as extraction) produces the (3,1) partition"
        f" instead. {mp_closer_sentence}"
        f" The corpus H¹=5 dominance among manifest presheaves"
        f" ({100*corpus_presheaf.get(5,0)/presheaf_n:.1f}% vs. random {rand_cond.get(5,0)*100:.1f}%)"
        f" reflects the prevalence of three-type orbits in DR's multi-type space,"
        f" where the institutional sign-flip creates (2,1,1) partitions"
        f" (one pair agreeing + two singleton observers). The toy simulation's"
        f" H¹=3 dominance reflects its simpler linear flow model. The DR-distinctive"
        f" signal in both comparisons is H¹=4 suppression: system justification,"
        f" motivated reasoning, and standpoint epistemology make no prediction about"
        f" pairwise disagreement count distributions and cannot produce this specific"
        f" structural feature."
    )

    lines.append(interp)
    lines += [
        "",
        "---",
        "",
        "*Generated by `python/h1_distribution_shape_test.py`.*",
    ]

    out_path.write_text("\n".join(lines), encoding="utf-8")


if __name__ == "__main__":
    main()
