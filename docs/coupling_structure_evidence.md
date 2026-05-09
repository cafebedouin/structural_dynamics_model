# Coupling Structure and Position-Space Geometry: An Evidence Document

### Audit Findings on a 10-Slice Working Family of the Deferential Realism Apparatus

**cafebedouin@gmail.com**

---

#### Abstract

This document reports two empirical findings from audits of the Deferential Realism apparatus on a slice family adjacent to the canonical 4-point site. First, observer specification and structural variation in the extractive subgraph are coupled, and the coupling is forward-asymmetric: PTES distance between observer slices predicts structural distance (forward Spearman ρ = 0.350, p = 0.037) while structural-profile similarity does not predict classification agreement (reverse ρ = −0.121, n.s.). A sensitivity check establishes that what couples is position-space geometry rather than the apparatus's classification labels themselves: replacing ordinal PTES distance with empirical classification disagreement collapses the forward correlation to ρ = 0.010. Second, axis dominance in position-space geometry is metric-specific. Across five structural-distance metrics evaluated on a 24-slice expansion, T leads under extractive fraction (Metric A only); E leads under type entropy, total variation, and cover-story flip rate (Metrics B, D, E); S leads under the mountain-fraction negative control (Metric C). Cross-metric agreement is weak (ρ range 0.09–0.43, with one negative). T and E are most consistently top-two across metrics; P contributes near zero (max partial ρ = 0.150); S is variable. The original "T-axis dominance" finding from a prior audit pass is metric-specific to extractive fraction and does not generalize.

The document is bounded in what it claims. It does not test the binary sheaf/presheaf boundary on which the framework's primary P-primacy claim rests, does not re-compute canonical-site H¹ values on the 10-slice family, and does not stake a position on whether any specific structural metric operationalizes the framework's cover-story mechanism. A SOTU corpus check on mountain decoupling is reported in §5 with the corpus's known authoring limitation surfaced; conclusions are not drawn from it.

A subsequent implementation-adjacent audit (see `metric_audit_writeup.md`) addresses one question this document leaves open — whether the apparatus's implementation privileges a particular structural-distance metric. It does not. The relevant positional structure is a two-hub functional decomposition: Hub 1 (P, S → χ via sigmoid) and Hub 2 (T, E → discrete mountain/rope immutability). Hub 2 captures substantially more structural variance than Hub 1 under four of five metrics. The T-dominance under Metric A reported in §4 concentrates as a within-rope-group T effect, robust under E-fixed conditions (T partial ρ = 0.762 in the n=36 E-fixed internal cell). A code-paper notation discrepancy in v6.11's Axiom 2 was also surfaced: d depends on P and E (not P alone) in the structural derivation path, and σ is a function of S only (not S(P)). These findings are summarized in §4.4.

---

#### 1. Scope

The Deferential Realism apparatus computes per-observer classifications across a site of (Power, TimeHorizon, ExitOptions, Scope) tuples and reports observer-site disagreement through cohomological obstruction (H¹) and a rule cascade with FCR/FSM/FNL overrides. The framework's reported empirical findings — the dimensional hierarchy P > E ≈ S > T, the 91/760 split between all-constant and mixed presheaves, the institutional sign-flip mechanism — are computed on a canonical 4-point site (U₁, U₂, U₃, U₄) and a 156-point product-site expansion. This document reports findings on a different slice family: a 10-slice working set selected for population coverage from the corpus's 326 distinct (P, T, E, S) slices, expanded to 24 slices for the metric-sensitivity analysis.

Two findings are developed. §3 establishes that observer specification and structural variation are coupled on this slice family, and the coupling is forward-asymmetric and geometry-driven. §4 establishes that axis dominance in the underlying geometry is metric-specific: different operationalizations of "structural distance" produce different axis rankings, with no single ranking robust across metrics.

Four things are not done here. The binary sheaf/presheaf boundary, on which the framework's primary P-primacy claim rests, is not tested on this slice family. The framework's canonical-site H¹ values are not re-computed. The product-site within-block analysis is not replicated. And no claim is staked about which of the five structural-distance metrics, if any, operationalizes the framework's cover-story mechanism in its native terms. These restrictions are surfaced in §6 and noted as reconciliation pointers in §7.

The document is an evidence record, not a framework revision. It describes what the audit pass found at the resolution it operated; it does not propose alternative axiomatics or revise the framework's narrative about power-axis primacy. Reconciliation is deferred.

---

#### 2. Audit Infrastructure

##### 2.1 Slice Family

The 10-slice working set, drawn from the corpus's 326 distinct (P, T, E, S) slices by population coverage:

| Label | Power | T | Exit | Scope | n | Dominant type |
|---|---|---|---|---|---|---|
| U_4 | analytical | civilizational | analytical | universal | 2543 | mountain |
| U_3_imm | institutional | immediate | arbitrage | global | 1845 | rope |
| U_3_civ | institutional | civilizational | arbitrage | global | 1569 | piton |
| U_1 | powerless | biographical | trapped | global | 1204 | snare |
| U_2 | moderate | biographical | constrained | national | 1140 | tangled_rope |
| organized | organized | generational | constrained | global | 1036 | scaffold |
| U_1_nat | powerless | biographical | trapped | national | 889 | snare |
| U_4_glob | analytical | civilizational | analytical | global | 733 | tangled_rope |
| org_nat | organized | generational | constrained | national | 689 | scaffold |
| U_3_nat | institutional | immediate | arbitrage | national | 635 | rope |

A drift from the canonical 4-point site requires explicit acknowledgment. The framework's canonical U₃ is (institutional, generational, arbitrage, national); the 10-slice family's U_3_imm and U_3_civ use "immediate" and "civilizational" time-horizon values respectively. The canonical U₃ is not in this slice family. This means audit results computed here do not compose cleanly with the framework's canonical-site numbers — they are computed on a related but distinct slice family. The drift is acknowledged here and treated as a known methodological limitation throughout. Where audit findings appear to bear on canonical-site claims, they should be read as operating on a structurally adjacent but non-identical site.

The 24-slice expansion used in §4 adds 14 Tier-2 slices drawn from the SOTU corpus's 117 distinct slices. The expansion increases the resolution at which axis variation can be detected by adding slices that vary individual axes while holding others constant. The two corpora — main (3,335 constraints) and SOTU (189 constraints) — share zero constraint IDs by construction; same-corpus pairs are 127 of the 276 expansion pairs, with cross-corpus pairs valid for axis-level analyses but excluded for analyses requiring shared constraints (Metric E in §4, n = 99).

##### 2.2 Structural-Distance Metrics

Five metrics operationalize "structural distance" between two observer slices i and j, each evaluated on the constraints classified at both:

- *Metric A — Extractive fraction:* |frac_extractive(i) − frac_extractive(j)|, where extractive ∈ {rope, tangled_rope, snare}. The original audit baseline.
- *Metric B — Type entropy:* |H(p_i) − H(p_j)|, Shannon entropy over the six-type distribution at each slice.
- *Metric C — Mountain fraction:* |frac_mountain(i) − frac_mountain(j)|. Included as a negative control; mountain populations are concentrated at analytical positions and expected to produce weak axis correlations.
- *Metric D — Total variation distance:* TV(p_i, p_j) over the full 6-type distribution; the most general type-distance measure.
- *Metric E — Cover-story flip rate:* the rate of rope ↔ {tangled_rope, snare} flips across constraints classified at both slices. Defined only for same-corpus pairs (n = 99).

Metric E's relationship to the framework's cover-story mechanism is not asserted. The framework describes the cover-story as an institutional sign-flip in the χ formula at U₃, producing extraction-as-coordination; Metric E measures rope ↔ tangled_rope/snare flip incidence across slice pairs. Whether the latter operationalizes the former in the framework's native sense is left open in this document.

##### 2.3 Audit Passes

The findings draw from two completed audit passes. The BC coupling audit (§3) operates on the 10-slice Tier-1 family and tests three sub-claims: that structural variation is wide across the family, that observer-spec → structure correlation exceeds structure → observer-spec correlation, and that mountains decouple from observer specification. The position-geometry metric-sensitivity audit (§4) operates on the 24-slice expansion and tests whether the original audit's "T-axis dominance" finding generalizes across structural-distance metrics. A SOTU-specific mountain decoupling audit was also run and is reported in §5 with caveats.

---

#### 3. Coupling Structure

##### 3.1 Wide Variation Across the Slice Family (Pass 1)

The extractive subgraph at each slice is constructed from constraints classified as rope, tangled_rope, or snare at that slice, with edges drawn from the corpus's contamination network. Three structural metrics on the resulting subgraph: extractive homophily (edge-centric, EE / (EE + EN) where E = extractive, N = non-extractive), connected-component count, and average path length within the largest connected component.

Variation is wide across the 10 slices:

| Slice | n_extractive | homophily | n_components | LCC | avg_path |
|---|---|---|---|---|---|
| U_4 | 193 | 0.037 | 186 | 2 | 1.0 |
| U_3_imm | 1828 | 0.981 | 854 | 256 | 8.41 |
| U_3_civ | 27 | 0.040 | 25 | 2 | 1.0 |
| U_1 | 1201 | 0.998 | 614 | 166 | 6.78 |
| U_2 | 1117 | 0.997 | 776 | 46 | 5.48 |
| organized | 332 | 0.171 | 275 | 6 | 2.07 |
| U_1_nat | 884 | 1.000 | 512 | 66 | 4.63 |
| U_4_glob | 547 | 0.541 | 467 | 4 | 1.5 |
| org_nat | 352 | 0.402 | 287 | 7 | 1.90 |
| U_3_nat | 629 | 0.994 | 452 | 26 | 3.38 |

Homophily ranges from 0.037 (U_4) to 1.000 (U_1_nat); n_components has CV = 0.46; avg_path_length ranges from 1.0 (U_4, U_3_civ) to 8.41 (U_3_imm). One slice — U_3_civ, with n_extractive = 27 — is flagged degenerate and excluded from downstream pairwise analyses. The verdict on Pass 1's first sub-claim: structural variation across the slice family is wide on all three metrics, and coupling between observer specification and structural variation exists.

##### 3.2 Forward Asymmetry (Pass 2)

The coupling test computes, across the 45 ordered slice pairs (excluding pairs involving the degenerate slice), the Spearman correlation between PTES distance (Hamming on the 4-tuple) and structural distance (|homophily(i) − homophily(j)|, the cleanest of the three metrics from §3.1). This is the "forward" direction: does observer specification predict structure?

Forward correlation: ρ = 0.350, p = 0.037. The reverse direction — does structural-profile similarity predict empirical classification agreement? — gives ρ = −0.121, p = 0.43. Forward dominance: the forward correlation is significant; the reverse is not. The asymmetry is consistent with observer specification driving structural variation rather than structure driving classification.

A sensitivity check on the forward direction tests three operationalizations of PTES distance:

| PTES distance | Forward ρ | p |
|---|---|---|
| Hamming | 0.350 | 0.037 |
| Weighted (per-axis hierarchy weights) | 0.321 | 0.056 |
| Learned (empirical classification disagreement) | 0.010 | 0.954 |

The learned-distance collapse from 0.350 to 0.010 is the consequential result. The Hamming and weighted distances treat PTES tuples as ordinal positions in a discrete geometry; the learned distance replaces this with the rate at which two slices empirically disagree about constraint classifications. If what produced the forward correlation were the apparatus's labels themselves, the learned distance — which is the labels' joint disagreement structure — should preserve or strengthen the correlation. Instead, the correlation vanishes.

The reading: what couples observer specification to structural variation is the underlying position-space geometry of the (P, T, E, S) tuples, not the apparatus's classification outputs. Observer specifications close to each other in PTES space produce structurally similar extractive subgraphs; observer specifications close in label-disagreement space do not, because label disagreement is itself a downstream consequence of the geometric coupling, not its source. The coupling is geometry-driven.

The forward-correlation magnitude (ρ = 0.350) is modest, and the result should be read as supporting forward asymmetry on the 10-slice family rather than as a strong quantitative claim about coupling strength. The reverse direction's near-zero correlation is the more diagnostically clear half of the result.

---

#### 4. Position-Space Geometry Across Metrics

##### 4.1 Per-Metric Axis Rankings

The metric-sensitivity audit computes, for each of the five structural-distance metrics, the partial Spearman correlation between per-axis distance (1 if axis differs between two slices, 0 otherwise) and the metric's structural distance. Partial correlations control for the other three axes.

Partial correlations across 24 slices, 253 non-degenerate pairs (99 for Metric E, same-corpus only):

| Metric | n | P | T | E | S | Top-1 |
|---|---|---|---|---|---|---|
| A: Extractive fraction | 253 | 0.031 | **0.390** | 0.012 | 0.047 | T |
| B: Type entropy | 253 | −0.004 | 0.186 | **0.189** | −0.087 | E |
| C: Mountain fraction | 253 | 0.063 | 0.114 | 0.037 | **0.197** | S |
| D: Total variation | 253 | 0.150 | 0.362 | **0.474** | 0.053 | E |
| E: Cover-story flip rate | 99 | −0.023 | 0.109 | **0.360** | 0.077 | E |

T leads under one metric (A, the original baseline). E leads under three (B, D, E). S leads under one (C, the negative control). P does not lead under any metric, and its maximum partial correlation across all five is 0.150 (under Metric D).

The original audit pass that reported "T-axis dominance" used Metric A (extractive fraction). This finding does not generalize. Under any of the four other metrics tested, T is not the leading axis. The "T-dominance dissolves" verdict is correct in the strict sense: T does not dominate across metrics. It is also correct that T remains consistently in the top two — under Metrics B, D, and E it is second; under C it is second; only under A is it first. T is not unimportant; it is not dominant.

##### 4.2 Cross-Metric Agreement

If the five metrics were measuring the same underlying structural distance with different precisions, their pairwise correlations would be high. They are not.

Cross-metric Spearman correlations across slice pairs:

| Pair | ρ | n |
|---|---|---|
| A-B | 0.181 | 253 |
| A-C | 0.125 | 253 |
| A-D | 0.388 | 253 |
| A-E | −0.245 | 99 |
| B-C | 0.092 | 253 |
| B-D | 0.245 | 253 |
| B-E | −0.002 | 99 |
| C-D | 0.129 | 253 |
| C-E | 0.151 | 99 |
| D-E | 0.426 | 99 |

The strongest agreement (D–E, ρ = 0.426) is between total variation distance and cover-story flip rate. The most consequential disagreement is A–E (ρ = −0.245): pairs of slices with large extractive-fraction distance tend to have *lower* cover-story flip rates. The two metrics capture structurally different geometric features. Pairs where extractive fraction shifts most are not the same pairs where rope ↔ tangled_rope/snare flips happen most.

Two readings stack. First, "structural distance" between observer slices is not a single underlying quantity. The five metrics define five candidate operationalizations, and their weak inter-correlations mean they pick out different pairs as structurally distant. Aggregating across them is not supported by the cross-metric agreement structure; reporting per-metric rankings is.

Second, the specific pattern that emerges across metrics — T and E consistently top-two, P consistently weak (max partial ρ = 0.150), S variable — characterizes *what is robust* across metrics rather than *what dominates*. The robust pattern is: the geometry is multi-axis, with T and E most consistently visible to structural-distance metrics; P contributes little under any of these operationalizations; S behaves differently across metrics.

##### 4.3 What This Constrains

The §4 finding constrains how the prior audit's results should be cited. The "T-dominance" claim is an artifact of one metric choice (extractive fraction) and does not generalize across structural-distance metrics. The robust finding is that T and E are both prominent and that P is consistently weak under structural-distance metrics. Whether this composes with v6.11's P > E ≈ S > T hierarchy claim is taken up in §7; the framework's claim is about a different construct (within-block variation in the product-site analysis), and the audits here do not directly test it.

##### 4.4 Implementation-Adjacent Findings (Subsequent Audit)

A subsequent audit pass (`metric_audit_results.{md,json}`, written up in `metric_audit_writeup.md`) took up the question raised in §7 about whether the apparatus's implementation privileges a particular structural-distance metric. It does not — the implementation contains no distance computation between observer slices, and the per-classification operations are organized around a two-hub functional decomposition rather than a metric structure. The findings refine §4.1 in three respects.

**Two-hub architecture.** All observer-dependent variation in the apparatus enters through two subsystems. Hub 1 (`derive_directionality/3`) maps observer position to directionality d, passed through sigmoid f and `scope_modifier(S)` to produce χ; P dominates d's range (canonical values 0.00–1.00), with E contributing marginally via `exit_modulation` (range −0.03 to +0.05) and S contributing as a multiplicative scope modifier. Hub 2 (`effective_immutability(T, E, Perception)`) maps (T, E) tuples to mountain or rope. P and S do not appear in Hub 2; T does not appear in Hub 1. E is the only axis present in both hubs.

**Hub 2 dominates Hub 1 under structural-distance metrics.** When per-axis predictors are aggregated to hub-level (hub1_diff = 1 if slices differ on P or S; hub2_diff = 1 if slices differ on T or E), Hub 2 partial ρ exceeds Hub 1 partial ρ by 0.16–0.24 under Metrics A, B, D, and E. The exception is Metric C (mountain-fraction negative control), where Hub 1 leads slightly (0.118 vs 0.036). The two hub-level predictors are statistically independent (ρ between them = −0.088), confirming genuine decomposition. The §4.1 finding that "T and E are consistently top-two across metrics" reflects this hub-level decomposition: T and E dominate jointly because they jointly constitute Hub 2.

**T-dominance under Metric A is a within-rope-group T effect.** Partitioning the 24-slice pairs by whether the two slices' Hub-2 outputs differ (Hub-2-spanning, n=141) or match (Hub-2-internal, n=112), T partial ρ under Metric A is 0.577 in internal pairs and 0.147 in spanning pairs — opposite to the prior expectation that T-dominance would track the mountain/rope boundary. A T–E covariation robustness check restricting internal pairs to those with E_diff = 0 (n = 36, of which 14 have T_diff = 1) yields T partial ρ = 0.762 under Metric A and 0.733 under Metric D, with T_diff varying entirely through Tier-2 slices that break Tier-1's perfect T–E collinearity in the rope-rope subset. T-dominance under A reflects T-axis variation within the rope immutability group (immediate vs generational time horizons at fixed exit options), not crossings of the Hub-2 mountain/rope boundary.

**E's dual hub presence does not produce a measurable positional-metric advantage.** E-weighted Hamming [P=1, T=1, E=2, S=1], motivated by E's dual hub presence, does not improve over unweighted Hamming as a positional-distance predictor of any structural metric (all differences |F − Hamming| < 0.05, no consistent direction). The architectural fact that E participates in both hubs is real; its empirical signature on the slice family is not separable from unweighted aggregates.

**Code-paper notation discrepancy.** The audit additionally surfaced a discrepancy between v6.11's Axiom 2 — χ = ε × f(d(P)) × σ(S(P)) — and the implementation. In the structural derivation path used for constraints with explicit beneficiary/victim data, d depends on P and E jointly (via `power_role_heuristic` plus `exit_modulation`), making the correct notation d(P, E). The canonical fallback path used for constraints lacking structural data does have d = d(P) only. Separately, σ is a function of S read directly from the context — it is not a function of P. The v6.11 notation S(P) is incorrect. A v6.11 update memo with proposed corrections accompanies the metric audit paper.

---

#### 5. Methodological Observations

##### 5.1 Slice-Family Drift

The 10-slice Tier-1 family was selected for population coverage from the corpus's 326 distinct slices, not constructed as a strict superset of the framework's canonical 4-point site. The framework's U₃ — (institutional, generational, arbitrage, national) — is not in the working set; U_3_imm and U_3_civ use different time-horizon values. This means audit findings on the 10-slice family describe behavior on a slice family that overlaps but does not contain the canonical site. Any apparent bearing on canonical-site claims should be read as pertaining to an adjacent slice family, not a refinement of the canonical site itself. Recovering composability would require running the same passes on a slice family that contains the canonical 4 points as a subset.

##### 5.2 SOTU Mountain Check

A SOTU-specific mountain decoupling audit was run as a check on the 10-slice findings. The audit measured, across 151 SOTU constraints with claimed_type = mountain, the consistency of mountain classification across each constraint's recorded perspective set (5–10 perspectives per constraint). Mean consistency: 0.144. 151/151 mountain-claimed constraints had at least one extraction-chain classification at some recorded perspective. Axis ordering for non-canonical variation within mountain-involving pairs: E > T > P > S, identical to baseline non-mountain pairs.

The result is reported here for completeness but conclusions are not drawn from it. The SOTU corpus has a known authoring limitation: it attempts to write one constraint story per SOTU address when the underlying historical record properly supports multiple constraints per address. The expanded perspective sets per constraint — averaging 7 perspectives — partly reflect this compression: a single constraint identifier is being asked to carry classifications that may correspond to materially different constraint structures. Whether the 0.144 consistency reflects mountain classification's site-dependence or the corpus's authoring compression is not separable from this audit pass alone. The cross-corpus check (0 shared constraint IDs with the main corpus) prevents using the main corpus to disambiguate.

The contextuality paper's canonical-site mountain contextuality fraction — 11/403 = 0.027 on the 4-point canonical site — remains the framework's reported number and is not displaced by this check. The SOTU result is consistent with broader site-dependence of mountain classification but does not establish it.

##### 5.3 The Substitution Question on Metric E

Metric E (cover-story flip rate) measures the rate of rope ↔ tangled_rope/snare flips across same-corpus slice pairs. The framework's cover-story mechanism is described in v6.11 as the institutional sign-flip in the χ formula at U₃, producing extraction-as-coordination via f(d) < 0 at canonical d ≈ 0.10–0.15. Whether Metric E's flip-rate measure operationalizes this mechanism is left open in this document. The two are related — both concern the boundary between coordination-typed and extraction-typed classifications — but the framework's mechanism is specified as a per-constraint χ-layer property, while Metric E is a pair-of-slices flip-rate property. The relationship is at best lossy and may not preserve the framework's intended construct. No claim about the cover-story mechanism's "true driver" is made on the basis of Metric E's per-axis correlations.

##### 5.4 What Aggregating Across Metrics Would Require

Section 4's cross-metric agreement table shows that the five metrics are weakly inter-correlated. Reporting an aggregate "axis dominance" ranking — averaging across metrics, or majority-voting — would treat the metrics as estimates of a single underlying quantity, which the cross-metric correlations do not support. The per-metric rankings reported in §4.1 are what the data carries; an aggregate ranking is not.

---

#### 6. What the Evidence Does and Does Not Support

Supports, on the 10-slice Tier-1 family:

- Structural variation in the extractive subgraph is wide across the slice family.
- Observer specification and structural variation are coupled, with forward asymmetry (observer-spec → structure dominates structure → observer-spec).
- The coupling is geometry-driven: the forward correlation collapses when ordinal PTES distance is replaced with empirical classification disagreement.

Supports, on the 24-slice expansion:

- Axis dominance under structural-distance metrics is metric-specific. T leads under extractive fraction only; E leads under three metrics; S leads under the negative control.
- Cross-metric agreement is weak (ρ range 0.09–0.43, with one negative pair). The five metrics do not measure a single underlying structural distance.
- Across all five metrics, T and E are consistently in the top two, P is consistently weak (max partial ρ = 0.150), and S is variable.
- The prior audit's "T-axis dominance" finding is metric-specific to extractive fraction.

Does not support:

- A claim that any specific axis "really" drives classification. Different metrics surface different axes; the data does not license a single ranking.
- A challenge to v6.11's P-primacy claim about the binary sheaf/presheaf boundary. That claim is computed on a different construct (binary boundary stability under axis-modulation perturbations), and the audits here do not test it.
- A refutation of v6.11's mountain decoupling pattern at the canonical site. The contextuality paper's 11/403 = 0.027 figure stands; the SOTU check is corpus-limited.
- A re-attribution of the framework's cover-story mechanism to any specific axis. The Metric E findings do not necessarily speak to the framework's cover-story construct.

Adds, from the subsequent implementation-adjacent audit (§4.4):

- The implementation contains no distance computation between observer slices. Any positional metric used in audits is analytically imposed, not derived from the codebase.
- Hub 2 (T, E axes) captures more structural variance than Hub 1 (P, S axes) under 4/5 metrics; the two hubs are statistically independent predictors. T and E dominate jointly as Hub 2's contributing axes.
- T-dominance under Metric A is a within-rope-group T effect (T partial ρ = 0.762 in E-fixed internal pairs), driven empirically by Tier-2 slices that break Tier-1's T–E collinearity. It is not a Hub-2-boundary effect.
- v6.11's Axiom 2 notation contains two errors: d should be written d(P, E) for the structural derivation path, and σ should be written σ(S), not σ(S(P)). A v6.11 update memo addresses these.

---

#### 7. Reconciliation Pointers

The findings here describe behavior on a slice family adjacent to but distinct from v6.11's canonical 4-point site, computed under structural-distance metrics that are weakly inter-correlated. The implementation-adjacent question — whether the apparatus privileges a particular metric — was taken up in the subsequent metric audit (§4.4) and resolved: it does not, the implementation organizes around a two-hub functional decomposition rather than a metric structure. Three reconciliation pointers remain open.

First, replicating the audit passes on a slice family that contains the canonical 4 points as a subset would test whether the §3 and §4 findings persist when canonical-site behavior is included. The current 10-slice family's drift from canonical U₃ is the cleanest reason audit results don't compose with framework numbers; closing the drift is the first move.

Second, the framework's primary P-primacy claim is about the binary sheaf/presheaf boundary — whether a constraint has H¹ = 0 or H¹ > 0. v6.11's robustness checks (varying exit modulation 0×–3×, scope 0.5×–2.0×) confirm this claim on the framework's own terms. None of the audits here tests the binary boundary directly. A binary-boundary audit on the 10-slice family — recomputing H¹ at each slice and checking whether the binary classification is preserved — is the test that would let audit results bear on the framework's primary claim.

Third, v6.11's product-site analysis reports a hierarchy P > E ≈ S > T from within-block analysis on a 156-context product site. This is not the same construct as the structural-distance metrics in §4, nor is it the same construct as the hub-level decomposition in §4.4. Replicating the within-block analysis on the 10-slice family would test whether the hierarchy persists under the slice-family change, holding the within-block construct fixed. The hub-level finding (Hub 2 dominant under structural-distance metrics) does not contradict the within-block hierarchy claim, but does motivate distinguishing the two constructs explicitly in v6.11.

Until those three pieces are in place, the audit findings here support a narrower claim than they might appear to: on a slice family adjacent to the canonical site, under structural-distance metrics, observer specification and structural variation are coupled and geometry-driven, axis dominance is metric-specific, and the apparatus's two-hub implementation produces a Hub-2-dominant decomposition under four of five structural metrics. Whether the framework's canonical-site narrative survives unchanged under expansion to this slice family is open. The data in this document does not settle it.

---

#### Methodological Notes

The 10-slice Tier-1 family was selected for population coverage from the corpus's 326 distinct (P, T, E, S) slices. Slices were included if they had ≥ 600 classified constraints. The "degenerate" threshold for Pass 1 metrics was n_extractive < 50 (one slice, U_3_civ, met this threshold and was excluded from pairwise analyses). The 24-slice expansion adds 14 SOTU positions selected to extend axis variation; the SOTU corpus and main corpus share zero constraint IDs by construction, and Metric E is restricted to same-corpus pairs (n = 99 of 276).

The Pass 2 forward-direction PTES distance defaults to unweighted Hamming. The "weighted" variant uses per-axis hierarchy weights from v6.11's product-site analysis (P > E ≈ S > T order); the "learned" variant replaces ordinal distance with empirical classification disagreement rate. The forward-correlation collapse across these three is the sensitivity result reported in §3.2.

Partial Spearman correlations in §4 use rank-residualization, identical to the position-geometry audit's prior methodology. Per-axis distance is binary (1 if differs, 0 otherwise); structural distance is the metric-specific quantity defined in §2.2. Significance testing uses standard partial-correlation p-values; the Bonferroni-corrected threshold across 20 partial correlations (5 metrics × 4 axes) at α = 0.05 is p < 0.0025. Partial correlations clearing this threshold: Metric A T (p = 1.3 × 10⁻¹⁰), Metric C S (p = 0.002), Metric D T (p = 3.1 × 10⁻⁹), Metric D E (p = 1.5 × 10⁻¹⁵), Metric E E (p = 2.6 × 10⁻⁴). Metric B's two top axes (T at p = 0.003, E at p = 0.003) sit just above the corrected threshold and just below uncorrected α = 0.05; they should be read as suggestive rather than confirmed at corrected significance. Metric D P (p = 0.017) and the negative-direction correlations in Metric A (S, p = 0.46) and Metric B (S, p = 0.17) do not clear corrected significance.

The SOTU mountain check operates on author-recorded perspectives per constraint, not apparatus-computed classifications on a slice grid; the corpus's authoring convention and its limitation are described in §5.2.

---

#### References

Internal to the project:

- *Axioms and Consequences of Observer-Dependent Classification* (v6.11). The framework paper; source for the canonical 4-point site, Axioms 1–6, the dimensional hierarchy P > E ≈ S > T, and the product-site analysis this document does not replicate.
- *When Splitting Isn't Solving: Sheaves, Presheaves, and the Structure of Indexical Disagreement.* Source for the sheaf/presheaf boundary construct that the §7 reconciliation pointers reference.
- *The Sheaf-Theoretic Structure of Power-Indexed Constraints* (contextuality paper). Source for the canonical-site mountain contextuality fraction (11/403 = 0.027) cited in §5.2.
- *When Metrics Aren't Measurement: Cluster-Space Architecture and the Limits of Signature-Pathway Detection.* The companion paper whose findings this document complements; its companion-paper handoff to idea-site cohomology is not taken up here.
- BC Coupling Audit (`bc_coupling_audit.{md,json}`) and Position Geometry Metric Sensitivity (`position_geometry_metric_sensitivity.{md,json}`). The audit outputs this document reports.
- Idea-Site Structure Exploration (`idea_site_exploration.md`). Source for the 10-slice working family documentation.
- SOTU Mountain Decoupling (`sotu_mountain_decoupling.{md,json}`). Source for the §5.2 corpus check.
- Metric Audit (`metric_audit_recon.md`, `metric_audit_proposal.md`, `metric_audit_results.{md,json}`, `audit3_te_robustness.{md,json}`) and the writeup paper (`metric_audit_writeup.md`). Source for the §4.4 implementation-adjacent findings.
- Two-Hub Architecture (`two_hub_architecture.md`). Source for the implementation-level documentation referenced in §4.4.
