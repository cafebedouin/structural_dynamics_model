# Metric Audit — Stage 2: Audit Proposal

**Date:** 2026-05-08
**Follows from:** `metric_audit_recon.md`

---

## 1. What Is Being Tested

The recon established that the DR implementation encodes a two-hub positional structure:

- **Hub 1** (P × E → directionality d → χ; S → scope_modifier): continuous, all threshold-based classification variation is downstream.
- **Hub 2** (T × E → effective_immutability): discrete, determines mountain/rope classification boundary.
- **E-axis appears in both hubs**; no other axis does.
- **No explicit distance metric** exists in the codebase; any metric must be imposed analytically.

Three audits operationalize this structure and test its empirical consequences.

---

## 2. Audit 1 — E-Weighted Hamming (Metric F)

**Question:** Does weighting E at 2× in the positional distance metric change the observed axis-dominance results, relative to unweighted Hamming?

**Definition:** Metric F = Hamming distance on 4-tuple [P, T, E, S] with weights [1, 1, 2, 1]. So dist_F(i, j) = diff_P + diff_T + 2·diff_E + diff_S, where diff_X ∈ {0, 1}. Range: [0, 5].

**Contrast metric:** Unweighted Hamming = diff_P + diff_T + diff_E + diff_S. Range: [0, 4].

**Method:** For each non-degenerate pair, compute both metrics. For each structural metric A–E, compute the partial Spearman correlation between metric_F and structural distance, controlling for all four per-axis binary diffs. Do the same for unweighted Hamming. Compare the two ρ vectors across structural metrics.

**Note on category distinction:** Metric F is a *positional* distance metric — it measures how far apart two observer positions are in (P, T, E, S) space. It is not a *structural* metric (like A–E, which measure how different the constraint populations look at two slices). In the prior audits, per-axis binary diffs (P_diff, T_diff, E_diff, S_diff) served as the positional predictors. Metric F aggregates those diffs with E-weighting, testing whether the weighted aggregate is a better positional predictor than the unweighted aggregate. The structural metrics A–E remain the dependent variable throughout.

**What it can distinguish:**
- Whether the E-weighting changes partial correlation magnitudes for any of the four axes.
- Whether Metric F as a summary predictor explains more structural variance than unweighted Hamming.

**What it cannot distinguish:**
- Whether any observed change is due to E-weighting specifically vs. scale changes from the [0,5] vs. [0,4] range.
- The mechanistic source of any ranking change.

**Positive finding criterion:** Metric F partial ρ against structural distance (A–E each) differs from Hamming by ≥ 0.05 in a consistent direction across ≥ 3/5 structural metrics, AND the direction strengthens E-axis contribution relative to P-axis.

**Negative finding criterion:** Metric F and Hamming produce indistinguishable partial ρ profiles across structural metrics (|diff| < 0.05 uniformly).

---

## 3. Audit 2 — Hub-Separated Predictors

**Question:** Do Hub 1 and Hub 2 capture statistically independent structural variance? If yes, this explains the T+E vs P+S axis split visible in the A–E results as a two-hub decomposition, not a single-axis effect.

**Definition:**
- `hub1_diff(i, j)` = 1 if slices i and j differ on P OR S; 0 otherwise. (Hub 1 axes)
- `hub2_diff(i, j)` = 1 if slices i and j differ on T OR E; 0 otherwise. (Hub 2 axes)

**Method:** For each non-degenerate pair, compute hub1_diff and hub2_diff. For each structural metric A–E, compute partial Spearman of hub1_diff controlling for hub2_diff, and vice versa.

**Collinearity check:** Compute Spearman(hub1_diff, hub2_diff). If |ρ| > 0.7, the two predictors are too collinear to interpret independently — fall back to zero-order correlations and flag.

**What it can distinguish:**
- Whether Hub 1 variation (P or S changes) drives structural variation independently of Hub 2 variation (T or E changes).
- Whether the T+E dominance visible in A–E results reflects a unified Hub 2 mechanism rather than T and E acting independently.

**What it cannot distinguish:**
- Which specific axis within each hub (P vs S, or T vs E) drives the effect.
- Whether Hub 1 and Hub 2 interact nonlinearly.

**Positive finding criterion:** Hub 1 and Hub 2 partial ρ profiles differ by ≥ 0.10 across ≥ 3/5 structural metrics.

**Negative finding criterion:** Hub 1 and Hub 2 produce similar partial ρ profiles (|diff| < 0.10 across all metrics).

**Ambiguity criterion:** If hub1_diff and hub2_diff are collinear (|ρ| > 0.7), report zero-order correlations and flag; partial results uninterpretable.

---

## 4. Audit 3 — Hub-2-Spanning Partition

**Question:** Is T-axis dominance under Metric A concentrated in pairs that span Hub 2's mountain/rope boundary? If yes, "T-dominance" under extractive fraction is a mechanical boundary effect, not a continuous T-axis effect.

**Definition:** A slice pair is **Hub-2-spanning** if the two slices' (T, E) tuples produce different effective_immutability outputs (one produces mountain, the other produces rope). A pair is **Hub-2-internal** if both produce the same output.

**Encoding:** The effective_immutability table is encoded analytically from `constraint_indexing.pl` lines 191–223 as a Python dict. Key: (T, E). Values: 'mountain', 'rope', or 'dual' for (civilizational, analytical).

Treatment of dual:
- (civilizational, analytical) produces both mountain and rope (non-deterministic by design in the Prolog).
- A pair where either slice is dual is counted as **spanning** — the dual slice can produce mountain or rope, so any pair involving it spans the boundary by definition.
- Dual-dual pairs (U_4 × U_4_glob) are spanning.

**Method:** Partition all non-degenerate pairs into spanning and internal subsets. Within each subset, compute per-axis partial Spearman for each structural metric A–E.

**Key test:** T-axis partial ρ under Metric A in spanning pairs vs. internal pairs.

**What it can distinguish:**
- Whether T-axis partial ρ under Metric A is explained by the mountain/rope classification boundary.
- Whether the original "T-dominance dissolves" finding should itself be recharacterized.

**What it cannot distinguish:**
- Whether T-axis within the spanning subset is the driving axis (vs. E-axis, which also determines Hub 2 output).
- What drives variation within the internal subset.

**Three-band verdict:**
- T partial ρ difference (spanning − internal) ≥ 0.15 under Metric A: **recast warranted**. T-dominance is a Hub 2 boundary artifact.
- 0.07–0.15 difference: **suggestive**. Flagged with explicit qualification; not sufficient for full recast without further evidence.
- < 0.07 difference: **negative**. T-dominance is not concentrated at the Hub 2 boundary on this sample.

The higher threshold for Audit 3 (0.15 vs. 0.05 for Audit 1) is deliberate. "T-dominance is a boundary artifact" is a substantively larger claim than "E-weighting improves fit" and requires stronger evidence.

---

## 5. Relationship Between Audits

The three audits are complementary:

- **Audit 1** tests whether the E-weighting implied by dual-hub presence produces measurable positional-metric difference.
- **Audit 2** tests whether the two hubs act as independent sources of structural variance.
- **Audit 3** tests whether the most salient empirical finding (T-dominance under A) is a Hub 2 boundary effect.

None of the three can fully explain why P is empirically weak despite mechanically dominating Hub 1. The slice family has no pure P-axis pairs; partial correlation for P is anchored on pairs that co-vary P with T and/or E. This limitation is not resolvable within this audit.

---

## 6. Sample Sizes and Data

All three audits operate on the same 24-slice combined family (10 Tier-1 + 14 Tier-2 SOTU) used in `position_geometry_metric_sensitivity.py`. Same degenerate-pair exclusion (U_3_civ excluded from Tier-1 due to n_extractive=27; any pair where either slice is degenerate or n_constraints < 5 excluded).

Expected sample sizes:
- Audits 1 and 2: n ≈ 253 for structural metrics A–D; n ≈ 99 for Metric E (same-corpus pairs only).
- Audit 3: spanning and internal subsets derived from the same 253-pair set. Expected rough split: most pairs in the 10-slice Tier-1 family span the Hub 2 boundary (see recon §7); exact split depends on the 14 Tier-2 SOTU slices' (T, E) values.

---

## 7. Positive, Negative, and Ambiguous Signals

| Signal | Interpretation |
|---|---|
| Metric F partial ρ differs from Hamming by ≥ 0.05 consistently | E-weighted Hamming is a better positional predictor; supports E-dual-hub operationalization |
| Metric F ≈ Hamming | E-weighting adds nothing; the binary per-axis diffs already capture E's contribution |
| Hub 1 ≠ Hub 2 partial ρ profiles | Two hubs capture independent structural variance; T+E vs P+S split has a mechanistic explanation |
| Hub 1 ≈ Hub 2 partial ρ profiles | Two hubs are collinear in this slice family; decomposition is not empirically distinguishable |
| T partial ρ under A: spanning >> internal (≥ 0.15) | T-dominance is a Hub 2 boundary artifact; recast §4 accordingly |
| T partial ρ under A: spanning ≈ internal (< 0.07) | T-dominance is not a boundary artifact at this sample size |
| T partial ρ under A: spanning > internal (0.07–0.15) | Suggestive; flag but do not recast |

---

## 8. What the Audits Cannot Distinguish

Collectively, the three audits cannot:
- Explain P's empirical weakness (no pure-P-axis pairs in working family).
- Determine whether the E-weighting in Metric F reflects the "right" operationalization or is one of many valid E-emphasizing options.
- Test the framework's P-primacy claim about the binary sheaf/presheaf boundary (H¹ = 0 vs. H¹ > 0) — none of these audits compute H¹.
- Establish which of the five structural metrics (A–E) operationalizes the framework's cover-story mechanism in its native terms.

These limitations are not remedied by this audit and are acknowledged in the results.
