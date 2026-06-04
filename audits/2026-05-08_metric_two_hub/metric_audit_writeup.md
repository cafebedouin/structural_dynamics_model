# When T Isn't the Boundary: Implementation Structure and Position-Space Geometry in the Deferential Realism Apparatus

**cafebedouin@gmail.com**

---

#### Abstract

This paper reports an audit of the Deferential Realism apparatus's implementation to determine whether the (P, T, E, S) axis structure encodes an implicit privileged structural-distance metric, and what that structure implies for prior empirical findings on the 24-slice position-space family. The implementation contains no distance computation; the relevant positional structure is the two-hub functional decomposition documented in `docs/two_hub_architecture.md`. Three audits operationalize this structure empirically.

**Audit 1** tests whether E-weighted Hamming [1, 1, 2, 1] — motivated by E's dual hub presence — produces measurably different predictions of structural metrics than unweighted Hamming. It does not: all differences are below 0.05, with no consistent direction. E's dual hub presence does not translate into a measurable positional-metric advantage.

**Audit 2** decomposes axis variation into Hub 1 (P, S) and Hub 2 (T, E) predictors. Hub 2 dominates Hub 1 under 4/5 structural metrics (hub2 partial ρ exceeds hub1 by ≥ 0.10 under Metrics A, B, D, E). Hub 1 and Hub 2 are near-uncorrelated as predictors (ρ = −0.088), confirming genuine decomposition. The exception is mountain fraction (Metric C), where Hub 1 edges Hub 2 (0.118 vs 0.036).

**Audit 3** partitions the 24-slice pairs into Hub-2-spanning (different effective_immutability outputs) and Hub-2-internal (same output). The hypothesis that T-axis dominance under Metric A (extractive fraction) would concentrate in spanning pairs — tracking the mountain/rope boundary — is falsified. T partial ρ under Metric A is 0.577 in internal pairs and 0.147 in spanning pairs, a difference of −0.431 in the opposite direction. T-dominance under Metric A is driven by variation *within* the rope immutability group (immediate vs generational time horizons, holding exit options constant) rather than by the Hub-2 boundary. In spanning pairs, where T and E co-vary across the boundary, E dominates under Metrics D and E.

A code-paper discrepancy is additionally documented: v6.11's Axiom 2 states χ = ε × f(d(P)) × σ(S(P)), but the implementation has d = g(P, E) (exit_modulation coupling) and σ = σ(S) (S is independent of P). A v6.11 update memo is appended.

Stage 4 (implementation of Metric F as a sixth table row) is not executed: the Audit 1 condition (difference ≥ 0.05 consistently) was not met.

---

#### 1. Scope

The prior audit chain established two empirical findings on a 24-slice combined family: (1) observer specification and extractive-subgraph variation are forward-asymmetrically coupled, geometry-driven (Hamming ρ = 0.350, learned-distance ρ = 0.010); (2) axis dominance under structural-distance metrics is metric-specific, with T leading under extractive fraction and E leading under type entropy, total variation, and cover-story flip rate.

This audit takes up the open question: does the implementation encode a positional structure that could explain or recharacterize these findings? The audit does not re-test the coupling finding. It examines what the implementation's functional decomposition predicts about positional-distance metrics, and whether those predictions survive empirical contact.

Four things are not done here. The binary sheaf/presheaf boundary (H¹ = 0 vs. H¹ > 0) is not tested. The product-site within-block analysis is not replicated. The coupling finding's forward/reverse asymmetry is not re-examined. And no claim is staked about which structural metric operationalizes the framework's cover-story mechanism.

---

#### 2. Implementation Background

The classification entry point is `classify_from_metrics/6` in `prolog/drl_core.pl` (line 300). All observer-dependent variation enters through two subsystems, documented in `docs/two_hub_architecture.md`:

**Hub 1** (the sigmoid): `derive_directionality/3` maps observer position to directionality d, passed through sigmoid f(d) and scope_modifier(S) to produce χ. P dominates d's range: canonical values run from 0.00 (institutional) to 1.00 (powerless). E contributes marginally via exit_modulation (range −0.03 to +0.05). S multiplies χ via a discrete table [0.8, 1.2]. T is absent from Hub 1.

**Hub 2** (the immutability lookup): `effective_immutability(T, E, Perception)` maps (TimeHorizon, ExitOptions) to mountain or rope. Used in the mountain gate, snare gate cross-check, and rope gate. P and S are absent from Hub 2. E participates substantially: different exit_options values at the same time horizon can produce different immutability outputs (e.g., biographical/trapped → mountain; biographical/identity_locked → rope).

E is the only axis in both hubs. All other axes appear in exactly one. This asymmetry motivated Audit 1's E-weighted Hamming test and Audit 2's hub-separated decomposition.

**Slice-family coverage:** Of the 10 non-degenerate Tier-1 slices, three produce mountain immutability (U_1, U_2, U_1_nat — all biographical time horizon); four produce rope (U_3_imm, organized, org_nat, U_3_nat); two produce both (U_4, U_4_glob — civilizational/analytical, non-deterministic by implementation design). The 24-slice combined family produces 253 non-degenerate pairs: 141 Hub-2-spanning (56%) and 112 Hub-2-internal (44%).

---

#### 3. Audit 1: E-Weighted Hamming

Metric F = diff_P + diff_T + 2·diff_E + diff_S (E double-weighted). Unweighted Hamming = diff_P + diff_T + diff_E + diff_S. For each structural metric A–E, zero-order Spearman correlations against each positional metric are compared.

| Metric | n | Hamming ρ | Metric F ρ | F − Hamming |
|---|---|---|---|---|
| A: Extractive fraction | 253 | 0.270 | 0.246 | −0.024 |
| B: Type entropy | 253 | 0.167 | 0.180 | +0.013 |
| C: Mountain fraction | 253 | 0.262 | 0.255 | −0.007 |
| D: Total variation | 253 | 0.536 | 0.565 | +0.028 |
| E: Cover-story flip rate | 99 | 0.297 | 0.328 | +0.031 |

Hamming–F collinearity: ρ = 0.986 (F = Hamming + E_diff, so high collinearity is expected). All |F − Hamming| < 0.05; direction is mixed (3 positive, 2 negative). The positive-finding criterion (≥ 0.05 consistently across ≥ 3/5 metrics) is not met.

**Verdict:** E-weighted Hamming does not improve prediction of any structural metric relative to unweighted Hamming. The binary E_diff term already captured in unweighted Hamming accounts for E's contribution; doubling its weight adds noise. E's dual hub presence is architecturally real but does not translate into a measurable positional-metric advantage at this sample size and slice family.

---

#### 4. Audit 2: Hub-Separated Predictors

Hub 1 and Hub 2 predictors: hub1_diff = 1 if slices differ on P or S; hub2_diff = 1 if slices differ on T or E. Collinearity: ρ = −0.088 (low; results interpretable). Per-axis partial Spearman, each hub controlling for the other:

| Metric | n | Hub 1 zero-ρ | Hub 2 zero-ρ | Hub 1 partial ρ | Hub 2 partial ρ | Hub2 − Hub1 |
|---|---|---|---|---|---|---|
| A: Extractive fraction | 253 | −0.005 | 0.248 | 0.018 | 0.249 | **0.231** |
| B: Type entropy | 253 | −0.015 | 0.245 | 0.007 | 0.245 | **0.238** |
| C: Mountain fraction | 253 | 0.115 | 0.026 | 0.118 | 0.036 | −0.081 |
| D: Total variation | 253 | 0.169 | 0.427 | 0.230 | 0.450 | **0.221** |
| E: Cover-story flip rate | 99 | 0.066 | 0.240 | 0.087 | 0.246 | **0.159** |

Hub 2 dominates Hub 1 under four of five structural metrics. Under Metrics A and B, Hub 1 is near-zero in both zero-order and partial terms (zero-order ρ = −0.005 and −0.015). Hub 1 only contributes substantially under Metric D (total variation), where it still trails Hub 2 (0.230 vs 0.450).

The exception is Metric C (mountain fraction), where Hub 1 partial ρ = 0.118 vs Hub 2's 0.036. Mountain classification requires both low χ (Hub 1) and immutability = mountain (Hub 2) — yet Hub 1 here signals stronger. The most likely explanation: mountain fraction is dominated by the analytical/civilizational position (U_4), which varies from other slices primarily on P and S (Hub 1 axes). The Hub 2 immutability output at U_4 is dual (mountain and rope); changes in S (universal vs global) differentiate U_4 from U_4_glob and affect the overall mountain fraction without crossing the Hub 2 boundary.

**Verdict:** Hub 2 (T, E axes) captures substantially more structural variance than Hub 1 (P, S axes) under 4/5 structural metrics. The two hubs are near-uncorrelated as predictors, confirming genuine decomposition rather than collinearity-induced artifact. This result recharacterizes the prior finding of T and E as "consistently top-two" axes: T and E dominate because they are Hub 2's contributing axes, and Hub 2 dominates Hub 1 structurally. The prior finding's language of "T and E as axes" was not wrong, but it missed the mechanistic unity: T and E are top-two because they jointly constitute the hub that dominates structural variance.

**On P's empirical weakness:** The Hub 1 zero-order ρ is negative under Metrics A and B (−0.005 and −0.015). P is Hub 1's mechanically dominant contributor (canonical d range 0.00–1.00), yet Hub 1 is nearly uncorrelated with structural metrics A and B. This cannot be explained as a slice-family artifact alone (no pure P-axis pairs), because hub1_diff aggregates P and S together and is still near-zero. P contributes little under A and B not because the partial correlation doesn't isolate it but because Hub 1 as a whole contributes little. One candidate explanation: most extractive fraction variation across these slices is mountain vs non-mountain (Hub 2-determined), and within the non-mountain group, power scaling changes χ but may not change the extraction type substantially (rope remains rope; snare remains snare). Hub 1 drives within-class χ magnitude variation; it does not primarily drive between-class type-boundary crossings. This remains a hypothesis.

---

#### 5. Audit 3: Hub-2-Spanning Partition

##### 5.1 Setup

Each non-degenerate pair is classified as Hub-2-spanning (different effective_immutability outputs for the two slices) or Hub-2-internal (same output). Pairs involving a dual slice (civilizational/analytical) are classified as spanning. n_spanning = 141 (56%), n_internal = 112 (44%).

The hypothesis entering this audit: T-axis dominance under Metric A (extractive fraction) would concentrate in spanning pairs, because spanning pairs cross the Hub-2 mountain/rope boundary, and mountain-classified constraints have near-zero extractive fraction while rope/snare/tangled_rope-classified constraints are counted as extractive.

##### 5.2 Results

T-axis partial ρ under Metric A: spanning pairs = 0.147; internal pairs = 0.577. Difference = −0.431.

**The hypothesis is falsified.** T-dominance is more than three times stronger in internal pairs than spanning pairs. The mountain/rope Hub-2 boundary is not driving T-dominance.

Full per-axis table:

| Subset | Metric | n | P | T | E | S | Top-1 |
|---|---|---|---|---|---|---|---|
| Spanning | A: Extractive fraction | 141 | 0.001 | 0.147 | 0.068 | 0.126 | T |
| Spanning | B: Type entropy | 141 | 0.109 | −0.304 | 0.157 | −0.123 | T (neg) |
| Spanning | C: Mountain fraction | 141 | 0.103 | 0.021 | −0.000 | 0.206 | S |
| Spanning | D: Total variation | 141 | 0.164 | 0.212 | 0.440 | −0.005 | E |
| Spanning | E: Cover-story flip | 60 | 0.135 | −0.104 | 0.321 | −0.073 | E |
| Internal | A: Extractive fraction | 112 | 0.119 | **0.577** | −0.044 | −0.016 | T |
| Internal | B: Type entropy | 112 | 0.040 | **0.575** | 0.236 | 0.062 | T |
| Internal | C: Mountain fraction | 112 | −0.053 | −0.059 | 0.051 | 0.105 | S |
| Internal | D: Total variation | 112 | 0.230 | 0.529 | **0.553** | 0.145 | E |
| Internal | E: Cover-story flip | 39 | −0.089 | 0.240 | **0.470** | 0.270 | E |

The axis-dominance profiles split strikingly:

**Spanning pairs:** E dominates under Metrics D and E (0.440, 0.321). T is moderate under A (0.147) and negative under B (−0.304). S leads under C. The profile here reflects Hub 2's role in spanning pairs: E-axis variation (arbitrage vs analytical, trapped vs constrained) largely determines which side of the mountain/rope boundary a slice is on, so E's contribution in spanning pairs is a Hub-2-boundary effect.

**Internal pairs:** T dominates under Metrics A and B (0.577, 0.575) with very high magnitudes. E and T share the top under D. T is moderate under E (0.240). P contributes under D (0.230). The large T values in internal pairs are the source of T-dominance in the overall 253-pair results.

##### 5.3 Mechanistic Interpretation

Internal pairs include rope-rope pairs (both slices return rope from Hub 2) and mountain-mountain pairs (both return mountain). Within the mountain-mountain group, all three slices (U_1, U_2, U_1_nat) share biographical time horizon — T_diff = 0 for all mountain-mountain Tier-1 pairs. They cannot contribute T-axis information.

Within the rope-rope group, T varies: institutional/immediate (U_3_imm, U_3_nat) vs organized/generational (organized, org_nat). And the extractive fractions differ substantially: U_3_imm and U_3_nat are rope-dominant (high extractive fraction); organized and org_nat are scaffold-dominant (lower extractive fraction). Pairs like (U_3_imm, organized) and (U_3_nat, org_nat) have T_diff = 1 and large extractive fraction differences, driving T's strong partial ρ under Metric A within the internal subset.

The Tier-2 SOTU slices likely amplify this pattern: many SOTU slices are institutional or organized positions, and their T-axis variation within the same exit-option group (rope immutability) produces T-associated extractive fraction differences.

In spanning pairs, T and E covary across the Hub-2 boundary (biographical/trapped → mountain; immediate/arbitrage → rope). When partial correlation controls for E, T's marginal contribution in spanning pairs is reduced. E absorbs much of the variance that T and E jointly share at the boundary.

**The recast:** T-axis dominance under Metric A is not a Hub-2-boundary effect (mountain vs rope transition) but a within-Hub-2-group T effect — specifically, T-axis variation within the rope immutability group correlates with large extractive-fraction differences, driven by the organizational/institutional divide (scaffold-dominant vs rope-dominant positions). The Hub-2 boundary actually dilutes T's partial ρ by introducing E-T correlation at the mountain/rope crossing.

##### 5.4 Implications for Prior §4 Results

The prior audit's "T-dominance dissolves" verdict was correct in the sense that T ranks first under only 1/5 structural metrics (Metric A only). This audit clarifies what was driving that 1/5 T-dominance: not a general T-axis effect and not a Hub-2-boundary effect, but within-rope-group T variation (immediate vs generational) producing large extractive fraction differences.

This does not revise the "T-dominance dissolves" verdict — T-dominance under A is still metric-specific and does not generalize. It does revise the interpretive framing: the T-axis mechanism under Metric A is organizational-position differentiation within the rope immutability group, not a temporal-perception mechanism operating across the mountain/rope boundary.

---

#### 6. Summary of Findings

| Finding | Audit | Status |
|---|---|---|
| E-weighted Hamming improves positional-distance prediction | 1 | Negative |
| Hub 2 (T, E) dominates Hub 1 (P, S) under structural metrics | 2 | Confirmed (4/5 metrics) |
| Hub 1 and Hub 2 are statistically uncorrelated as predictors | 2 | Confirmed (ρ = −0.088) |
| T-dominance under Metric A concentrates in spanning pairs | 3 | Falsified |
| T-dominance under Metric A concentrates in internal pairs | 3 | Confirmed (diff = 0.431) |
| T-dominance is within-rope-group T effect | 3 | Supported (mechanism identified) |

---

#### 7. What This Evidence Does and Does Not Support

**Supports:**

- Hub 2 (T, E axes) captures substantially more structural variance than Hub 1 (P, S axes) under 4/5 structural metrics, with Hub 1 near-zero under Metrics A and B.
- T and E are empirically top-two because they jointly constitute the dominant hub, not because they are individually the strongest axes on independent grounds.
- T-axis dominance under Metric A (the original audit finding) is a within-Hub-2-group T effect, driven by T-axis variation within the rope immutability group (immediate vs generational, holding exit options constant).
- The mountain/rope Hub-2 boundary is not the source of T-dominance under Metric A — it dilutes T-axis signal by introducing E-T covariation.
- E's dual hub presence (architecturally documented) does not produce a measurable positional-metric advantage at this sample size.
- v6.11's Axiom 2 notation is imprecise: d depends on P and E (not P alone) in the structural derivation path, and σ is a function of S (not S(P)).

**Does not support:**

- A claim that any axis "really" drives classification in a framework-level sense. Hub 2 dominance is an empirical finding on structural-distance metrics; it does not overturn the framework's P-primacy claim about the binary sheaf/presheaf boundary.
- An explanation of why Hub 1 (P, S) is empirically near-zero under Metrics A and B, beyond the candidate hypothesis stated in §4.
- Any claim about which structural metric operationalizes the framework's cover-story mechanism.
- Stage 4 extension of the §3.2 sensitivity table: Metric F does not improve over Hamming, so no new table row is added.

**Limitations:**

- No pure P-axis pairs in the 24-slice family. P's contribution to Hub 1 is not isolable; hub1_diff aggregates P and S.
- The Tier-2 SOTU slices' contribution to internal vs spanning pair profiles was not separately analyzed. The 14 SOTU slices' T and E distributions may amplify or attenuate the within-rope-group T effect observed in Tier-1.
- The "within-rope-group T effect" mechanism is inferred from the pair-level patterns; it is not directly verified by a constraint-level analysis.

---

#### 8. Methodological Notes

Slice family: 24-slice combined family (10 Tier-1 main corpus + 14 Tier-2 SOTU), identical to `position_geometry_metric_sensitivity.py`. One degenerate Tier-1 slice (U_3_civ, n_extractive = 27). Pair-building and degenerate-pair exclusion identical to prior audits.

Audit 1: zero-order Spearman(positional_dist, structural_dist). No partial-correlation control is applied because Metric F is a linear combination of per-axis binary diffs and would be collinear with them as controls. Collinearity between Hamming and Metric F: ρ = 0.986 (F = Hamming + E_diff, so expected).

Audit 2: partial Spearman using rank-residualization, each hub controlling for the other. Hub1_diff and Hub2_diff are binary (1 if differs on any Hub 1 or Hub 2 axis respectively). Collinearity between predictors: ρ = −0.088.

Audit 3: effective_immutability table encoded from `constraint_indexing.pl` lines 191–223. Civilizational/analytical treated as dual — any pair involving a dual slice is classified as spanning. Per-axis partial Spearman within each subset uses rank-residualization controlling for the other three axes, identical to prior audit methodology.

---

#### Appendix: v6.11 Update Memo

**Proposed edits to `docs/deferential_realism_paper_v6.11.md`.**

**Finding 1: Axiom 2 notation — d(P) understates E's contribution.**

v6.11 states: "χ = ε × f(d(P)) × σ(S(P))"

The code has two derivation paths for d:
- **Structural path** (most constraints with beneficiary/victim data): d = `power_role_heuristic(P, HasBen, HasVic, BaseD)` + `exit_modulation(E)`. Exit_modulation range: −0.03 to +0.05; P's BaseD range: 0.00–1.00. E's Hub 1 contribution is marginal in magnitude but architecturally real.
- **Canonical fallback path** (constraints without structural data): d = `canonical_d_for_power(P)`. No E contribution. The notation d(P) accurately describes this path only.

**Proposed correction:** "χ = ε × f(d(P, E)) × σ(S)" with a note: "d is derived primarily from P (range 0.00–1.00 across power levels) with a marginal adjustment from E via exit_modulation (range ±0.05). The P-only attribution d(P) is accurate for the canonical fallback path; for constraints with explicit beneficiary/victim data, d = g(P, E)."

**Finding 2: Axiom 2 notation — S(P) is incorrect.**

v6.11 writes σ(S(P)), implying S depends on P. S is an independent axis; `scope_modifier(S)` reads S directly.

**Proposed correction:** Change σ(S(P)) to σ(S).

**Finding 3: Dimensional hierarchy — T and E as Hub 2.**

v6.11's dimensional hierarchy P > E ≈ S > T is computed from the within-block product-site analysis. This audit does not test that claim (it uses a different construct and slice family). However, the hub-separation finding adds context: T and E empirically dominate P and S under structural-distance metrics because T and E jointly constitute Hub 2, which captures substantially more structural variance than Hub 1. This does not contradict the hierarchy claim — the hierarchy is about a different construct — but it motivates a note distinguishing the within-block hierarchy from the hub-level variance decomposition.

**Optional note to add (§2 or a new methodological note):** "The dimensional hierarchy P > E ≈ S > T reflects within-block variation on the product site. At the hub level, T and E jointly constitute the effective-immutability mechanism (Hub 2), which empirically captures more structural-distance variance than the power-scaled extraction mechanism (Hub 1, P and S axes) across the 24-slice working family. These are different constructs — within-block axis priority vs. between-slice hub contribution — and should not be conflated."

**Finding 4: T-dominance under extractive fraction.**

§4 of `coupling_structure_evidence.md` reports T-axis dominance under Metric A (partial ρ = 0.390). This audit shows that dominance concentrates in Hub-2-internal pairs (ρ = 0.577) rather than Hub-2-spanning pairs (ρ = 0.147), indicating the effect is driven by T-axis variation within the rope immutability group (immediate vs generational, holding exit options constant), not by the mountain/rope classification boundary.

**Proposed addition to §4:** A note that the T-axis mechanism under Metric A is within-Hub-2-group variation, not a Hub-2-boundary effect. The "T-dominance dissolves" verdict stands; this audit adds that the 1/5-metric T-dominance that does exist is sourced in within-rope-group organizational position differentiation rather than temporal-perception boundary crossing.

---

*Audit conducted 2026-05-08. Scripts: `python/metric_audit.py`. Outputs: `outputs/metric_audit_results.{json,md}`.*
