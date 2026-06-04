# Metric Audit — Results

## Verdict: Audit 2: 4/5 metrics show hub2>hub1 by >=0.10

Positional structure found: **True**  
Metric F differs from Hamming: **False**  
Hub decomposition informative: **True**  
T-dominance recast verdict: **recast_warranted**  

Slice family: 24 slices, 276 total pairs, 253 non-degenerate.

## Audit 1: E-Weighted Hamming (Metric F) vs. Unweighted Hamming

Hamming–F collinearity: ρ = 0.986 (n=253). High collinearity expected (F = Hamming + E_diff).

| Metric | n | Hamming ρ | Metric F ρ | Difference (F − Hamm) |
|---|---|---|---|---|
| A: Extractive fraction (baseline) | 253 | 0.270 | 0.246 | -0.024 |
| B: Type entropy | 253 | 0.167 | 0.180 | 0.013 |
| C: Mountain fraction | 253 | 0.262 | 0.255 | -0.007 |
| D: Total variation distance | 253 | 0.536 | 0.565 | 0.028 |
| E: Cover-story flip rate | 99 | 0.249 | 0.279 | 0.030 |

Positive finding criterion: |difference| ≥ 0.05 in consistent direction across ≥ 3/5 metrics.

## Audit 2: Hub-Separated Predictors

Hub 1 vs Hub 2 collinearity: ρ = -0.088 (n=253). Interpretable.

| Metric | n | Hub 1 zero-ρ | Hub 2 zero-ρ | Hub 1 partial ρ | Hub 2 partial ρ | Hub2 − Hub1 |
|---|---|---|---|---|---|---|
| A: Extractive fraction (baseline) | 253 | -0.005 | 0.248 | 0.018 | 0.249 | 0.231 |
| B: Type entropy | 253 | -0.015 | 0.245 | 0.007 | 0.245 | 0.238 |
| C: Mountain fraction | 253 | 0.115 | 0.026 | 0.118 | 0.036 | -0.081 |
| D: Total variation distance | 253 | 0.169 | 0.427 | 0.230 | 0.450 | 0.221 |
| E: Cover-story flip rate | 99 | 0.040 | 0.247 | 0.061 | 0.251 | 0.190 |

## Audit 3: Hub-2-Spanning Partition

n_spanning: 141, n_internal: 112 (56% spanning).

T-axis partial ρ under Metric A:
  Spanning pairs: 0.147
  Internal pairs: 0.577
  Difference: -0.431 → **recast_warranted**

### Per-Axis Partial ρ: Spanning vs Internal (all metrics)

| Subset | Metric | n | P | T | E | S | Top-1 |
|---|---|---|---|---|---|---|---|
| Spanning | A: Extractive fraction (baseline) | 141 | 0.001 | 0.147 | 0.068 | 0.126 | T |
| Spanning | B: Type entropy | 141 | 0.109 | -0.304 | 0.157 | -0.123 | T |
| Spanning | C: Mountain fraction | 141 | 0.103 | 0.021 | -0.000 | 0.206 | S |
| Spanning | D: Total variation distance | 141 | 0.164 | 0.212 | 0.440 | -0.005 | E |
| Spanning | E: Cover-story flip rate | 60 | 0.105 | -0.116 | 0.287 | -0.072 | E |
| Internal | A: Extractive fraction (baseline) | 112 | 0.119 | 0.577 | -0.044 | -0.016 | T |
| Internal | B: Type entropy | 112 | 0.040 | 0.575 | 0.236 | 0.062 | T |
| Internal | C: Mountain fraction | 112 | -0.053 | -0.059 | 0.051 | 0.105 | S |
| Internal | D: Total variation distance | 112 | 0.230 | 0.529 | 0.553 | 0.145 | E |
| Internal | E: Cover-story flip rate | 39 | -0.064 | 0.257 | 0.466 | 0.176 | E |

## Methodological Self-Report

- Slice family: same 24-slice combined family as position_geometry_metric_sensitivity (10 Tier-1 + 14 Tier-2).
- Degenerate pairs excluded (n_extractive < 50 at Tier-1 or n_constraints < 5).
- Audit 1: zero-order Spearman(positional_dist, structural_dist); no partial-correlation control (metric_F is a linear combination of axis diffs and would be collinear with them as controls).
- Audit 2: partial Spearman with hub1_diff controlling for hub2_diff, and vice versa.
- Audit 3: effective_immutability encoded from constraint_indexing.pl lines 191-223; (civilizational, analytical) treated as dual (counts as spanning in all pairs).
- Audit 3 per-axis partial Spearman uses rank-residualization controlling for all other three axes within each subset.
- Hub-2 spanning classification: a pair is spanning if either slice has a dual output OR the two slices have different outputs (mountain vs rope).

## What This Evidence Does and Does Not Support

**Supports:**
- [from Audit 1] Whether E-weighted Hamming is a better positional-distance predictor of structural metrics than unweighted Hamming.
- [from Audit 2] Whether Hub 1 (P, S axes) and Hub 2 (T, E axes) variation capture statistically independent structural variance.
- [from Audit 3] Whether T-axis dominance under Metric A (extractive fraction) is concentrated in pairs that span Hub 2's mountain/rope boundary.
- [from code inspection] That v6.11's Axiom 2 notation d(P) understates E's role (d = g(P, E) in structural path); and σ(S(P)) is incorrect notation (S is independent of P).

**Does not support:**
- A claim that the implementation "privileges" any metric — it encodes no distance computation at all.
- An explanation of why P-axis partial ρ is empirically weak (no pure P-axis pairs in working family; untestable in this audit).
- A test of the framework's P-primacy claim about the binary sheaf/presheaf boundary (H¹ = 0 vs H¹ > 0); these audits do not compute H¹.
- A determination of which structural metric A-E operationalizes the framework's cover-story mechanism in its native terms.

