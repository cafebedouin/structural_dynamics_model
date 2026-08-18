# H¹ Distribution Shape Test

**Purpose:** Empirical test of the reframed §3.3 prediction in `observers_not_humans_v3.md`.

## Background

v2 §3.3 claimed that H¹ values {1, 2} being absent from the distribution is a DR-distinctive prediction that distinguishes DR from system justification and adjacent theories. This is incorrect: {1, 2} are combinatorially forbidden for **any** 4-observer classification rule — a consequence of arithmetic (partition enumeration), not of DR's axioms.

The correct DR prediction is distribution **shape** over reachable values {0, 3, 4, 5}: asymmetric power environments should concentrate at {4, 5}; symmetric environments should collapse to {0}; random classification should spread per the multinomial expectation.

**Note on H¹=6:** DR uses 7+ constraint types, enabling the partition (1-1-1-1) where all 4 observers assign distinct types — yielding H¹=6. The corpus confirms: **1 of 104 constraints** (1.0%) have H¹=6. The JSX ternary model cannot produce H¹=6 (pigeonhole: 3 values, 4 observers → ≥2 must agree). `flow_asymmetry_test_v2.jsx` lines 7–12 incorrectly list {1, 2, **6**} as unreachable; only {1, 2} are forbidden for any 4-observer rule.

---

## Corpus: Full H¹ Distribution

**N = 104 constraints** (from `outputs/enriched_pipeline.json`).

| H¹ | Count | % of corpus | Reachable in ternary? |
|-----|-------|-------------|----------------------|
| 0 | 28 | 26.9% | Yes |
| 2 | 4 | 3.8% | — |
| 3 | 22 | 21.2% | Yes |
| 4 | 3 | 2.9% | Yes |
| 5 | 31 | 29.8% | Yes |
| 6 | 1 | 1.0% | No — DR multi-type only |

Manifest presheaves (H¹ > 0): **61**

---

## Three-Way Comparison (Ternary-Comparable Bins {0, 3, 4, 5})

The H¹=6 constraints (1) are excluded from the statistical comparison because the ternary simulation cannot produce them. The ternary subset N = 84.

| H¹ | Corpus (ternary subset) | Random ternary | Asymmetric sim | Symmetric sim |
|----|------------------------|----------------|----------------|---------------|
| 0 | 28 (33.3%) | 3/81 (3.7%) | 437/1000 (43.7%) | 1000/1000 (100.0%) |
| 3 | 22 (26.2%) | 24/81 (29.6%) | 379/1000 (37.9%) | 0/1000 (0.0%) |
| 4 | 3 (3.6%) | 18/81 (22.2%) | 93/1000 (9.3%) | 0/1000 (0.0%) |
| 5 | 31 (36.9%) | 36/81 (44.4%) | 91/1000 (9.1%) | 0/1000 (0.0%) |

*Random ternary = analytic multinomial expectation over 81 equally-weighted (n+,n0,n−) triples.*
*Asymmetric/Symmetric sim = port of `flow_asymmetry_test_v2.jsx`, N=1000 constraints, seed=42.*

---

## Statistical Comparison

Chi-square goodness-of-fit (corpus ternary subset vs. each baseline):

| Comparison | χ² | df | Cramér's V | p (approx) |
|------------|----|----|------------|------------|
| Corpus vs. random multinomial   | 213.67 | 3 | 0.9208 | 1.40e-32 |
| Corpus vs. asymmetric simulation | 79.43 | 3 | 0.5614 | 2.19e-14 |
| Corpus vs. symmetric simulation  | 37.33 | 3 | 0.3849 | 1.59e-07 |

**Corpus distribution more closely resembles: asymmetric simulation** (lower χ² = 79.43 vs. 213.67).

---

## Notable Features

- **H¹=4 suppression:** corpus 3.6% vs. random expectation 22.2% (ratio 0.16×). H¹=4 requires the (2,2) partition — two pairs of observers agreeing differently. This is structurally rare because the institutional sign-flip produces the (3,1) partition (3 observers on one side, institutional observer alone) far more often than symmetric pairing.
- **H¹=5 elevation:** corpus 36.9% vs. random 44.4%. H¹=5 requires partition (2,1,1) — one pair agreeing plus two singleton disagreers. Common when the institutional observer is the odd-one-out among extraction-chain disagreements.

---

## Manifest Presheaf Shape (H¹ > 0, Ternary Bins {3, 4, 5})

The shape prediction is more precisely tested by conditioning on manifest presheaves only (H¹ > 0, excluding H¹=6). N = 56.

| H¹ | Corpus | Random cond. (H¹>0) | Asymmetric cond. (H¹>0) |
|----|--------|---------------------|------------------------|
| 3 | 22 (39.3%) | 30.8% | 67.3% |
| 4 | 3 (5.4%) | 23.1% | 16.5% |
| 5 | 31 (55.4%) | 46.2% | 16.2% |

| Comparison | χ² | Cramér's V | p |
|------------|-----|-----------|---|
| vs. random cond.    | 9.97 | 0.2983 | 0.0070 |
| vs. asymmetric cond.| 63.98 | 0.7558 | 0.0000 |

**Among manifest presheaves, corpus more closely resembles: random multinomial.**

---

## Interpretation

The full ternary distribution more closely resembles the asymmetric simulation (χ²=79.43) than the random baseline (χ²=213.67). The dominant deviation from the random baseline is **H¹=4 suppression**: the corpus has only 3.6% of ternary-subset constraints at H¹=4, vs. 22.2% expected under random classification (ratio 0.16×). H¹=4 requires the (2,2) partition — two pairs of observers agreeing on different types — which is structurally rare because DR's institutional sign-flip (U₃ alone classifying as rope when others classify as extraction) produces the (3,1) partition instead. Among manifest presheaves specifically, the corpus more closely resembles the random conditional (χ²=9.97, V=0.2983) than the asymmetric simulation (χ²=63.98, V=0.7558). The corpus H¹=5 dominance among manifest presheaves (55.4% vs. random 46.2%) reflects the prevalence of three-type orbits in DR's multi-type space, where the institutional sign-flip creates (2,1,1) partitions (one pair agreeing + two singleton observers). The toy simulation's H¹=3 dominance reflects its simpler linear flow model. The DR-distinctive signal in both comparisons is H¹=4 suppression: system justification, motivated reasoning, and standpoint epistemology make no prediction about pairwise disagreement count distributions and cannot produce this specific structural feature.

---

*Generated by `python/h1_distribution_shape_test.py`.*