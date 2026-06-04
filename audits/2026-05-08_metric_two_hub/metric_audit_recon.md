# Metric Audit — Stage 1: Reconnaissance

**Date:** 2026-05-08
**Scope:** Implementation-derived positional structure in the DR apparatus; relationship to prior empirical findings.

---

## 1. Codebase Structure

The classification pipeline has one entry point: `classify_from_metrics/6` in `prolog/drl_core.pl` (line 300). All twelve delegating modules route through this predicate. It takes `(Constraint, BaseEps, Chi, Supp, Context, Type)` and fires nine clauses in priority order: Mountain → Piton(dead-coord) → Snare → Scaffold → Rope → Tangled_Rope → Piton(standard) → Naturalized → Unknown.

The observer context is a 4-tuple: `context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))`. Context feeds two subsystems that are the only sources of observer-dependent variation in the pipeline.

The two-hub architecture is already documented in `docs/two_hub_architecture.md`; this recon does not replicate it. What is added here is an analysis of what that architecture implies for how position-space distance should be operationalized, and whether the implementation encodes any implicit privileged metric.

---

## 2. Axis Roles and Hub Membership

| Axis | Hub(s) | Mechanism | Mathematical character |
|---|---|---|---|
| P (agent_power) | Hub 1 only | `power_role_heuristic(P, HasBen, HasVic, BaseD)` → base directionality d | Discrete table; 6 atoms → [0.00, 1.00] range |
| T (time_horizon) | Hub 2 only | `effective_immutability(T, E, Perception)` lookup | Discrete table; no contribution to χ |
| E (exit_options) | Both hubs | Hub 1: `exit_modulation(E)` adjusts d (range: −0.03 to +0.05). Hub 2: `effective_immutability(T, E)` lookup | Hub 1 marginal; Hub 2 discrete/major |
| S (spatial_scope) | Hub 1 only | `scope_modifier(S)` multiplies χ | Discrete table [0.8, 1.2]; uncoupled from P, T, E |

E is the only axis that appears in both hubs. Its Hub 1 contribution (exit_modulation range ±0.05) is small relative to P's range over BaseD (0.00–1.00). Its Hub 2 contribution is the primary source of mountain/rope immutability variation. This asymmetry is consequential: E's Hub 1 presence is mechanically real but empirically minor; its Hub 2 presence is mechanically major.

---

## 3. The χ Formula: Implementation vs. v6.11 Axiom 2

v6.11 Axiom 2 states: **χ = ε × f(d(P)) × σ(S(P))**.

The implementation has:

```prolog
extractiveness_for_agent(Constraint, Context, Score) :-
    ...
    derive_directionality(Constraint, ResolvedContext, D),
    resolve_displacement(ResolvedPower, Delta),
    D_eff is max(0.0, min(1.0, D + Delta)),
    sigmoid_f(D_eff, PowerMod),
    scope_modifier(Scope, ScopeMod),
    Score is BaseScore * PowerMod * ScopeMod.
```

Two discrepancies from the Axiom 2 notation:

**Discrepancy 1: d depends on P and E, not just P.** `derive_directionality/3` uses the structural path when beneficiary/victim data is present: d = `power_role_heuristic(P, HasBen, HasVic, BaseD)` + `exit_modulation(E)`. The exit_modulation term is an E-axis contribution. In the canonical fallback path (no structural data): d = `canonical_d_for_power(P)` only — no E contribution. For the majority of constraints (corpus has beneficiary/victim data), d = g(P, E), not d(P). The v6.11 notation d(P) accurately describes only the fallback path.

**Discrepancy 2: σ is not a function of P.** v6.11 writes σ(S(P)), implying S depends on P. S is an independent axis in the context tuple; `scope_modifier(Scope)` reads S directly with no reference to P. The notation σ(S(P)) is incorrect — the parenthetical is misleading.

These are code-paper discrepancies, not implementation bugs. The implementation is the ground truth; the paper notation is imprecise. The corrected form is: **χ = ε × f(d(P, E)) × σ(S)**, where the d(P, E) coupling is minor in magnitude but real in structure.

---

## 4. Rule Cascade: No Axis Priority

The nine clauses of `classify_from_metrics/6` fire in type priority order (Mountain first, Unknown last). The ordering is a constraint-type ontology, not a (P, T, E, S) axis ordering. Within each clause, axes are consulted incidentally as needed by that type's gates:

- Mountain gate: checks BaseEps (ε, structural), `emerges_naturally(C)` (structural), and `effective_immutability_for_context(Context, mountain)` (Hub 2: T × E).
- Snare gate: checks Chi (Hub 1: P × E × S), BaseEps, Supp (structural), and `snare_immutability_check` (Hub 2 cross-stalk).
- Rope gate: checks Chi (Hub 1), BaseEps, and either `effective_immutability_for_context(Context, rope)` (Hub 2) or `emerges_naturally(C)`.

No (P, T, E, S) axis is consulted before another at the cascade level. P has no dedicated gate; it enters only through χ. T has no dedicated gate; it enters only through immutability. The cascade priority is a constraint-type ontology, not an axis hierarchy.

---

## 5. No Explicit Distance Metric

The Prolog implementation contains no predicate that computes distance between two observer contexts or two (P, T, E, S) tuples. Distance computations appear only in the Python audit scripts: unweighted Hamming and weighted Hamming [P=2, T=1, E=2, S=1] in `python/bc_coupling_audit.py` (lines ~196–197) — these are analytical tools, not part of the classifier.

The implementation-derived positional structure is therefore not a metric — it is a functional decomposition. The closest metric operationalization of that structure is E-weighted Hamming [P=1, T=1, E=2, S=1], reflecting E's dual hub presence. This is distinct from the existing weighted Hamming [2, 1, 2, 1] in bc_coupling_audit.py (which weighted P=2 as well as E=2).

---

## 6. Effective Immutability: Axis Coupling in Hub 2

The effective_immutability table (`constraint_indexing.pl`, lines 191–223) maps (T, E) → {mountain, rope}. Outputs for the 10-slice working family:

| Slice | T | E | Hub 2 output |
|---|---|---|---|
| U_4 | civilizational | analytical | {mountain, rope} (dual) |
| U_3_imm | immediate | arbitrage | rope |
| U_3_civ | institutional | arbitrage | rope |
| U_1 | biographical | trapped | mountain |
| U_2 | biographical | constrained | mountain |
| organized | generational | constrained | rope |
| U_1_nat | biographical | trapped | mountain |
| U_4_glob | civilizational | analytical | {mountain, rope} (dual) |
| org_nat | generational | constrained | rope |
| U_3_nat | immediate | arbitrage | rope |

Three slices see mountain (U_1, U_2, U_1_nat — biographical time horizon). Five slices see rope (U_3_imm, organized, org_nat, U_3_nat, and U_3_civ which is degenerate). Two slices are dual (U_4, U_4_glob — civilizational/analytical, non-deterministic by design).

**Slice-family drift finding:** The canonical U₃ = (institutional, generational, arbitrage, national). All three T-axis variants at arbitrage exit (immediate, generational, civilizational) produce rope via Hub 2:
- `effective_immutability(immediate, arbitrage, rope)` ✓
- `effective_immutability(generational, arbitrage, rope)` ✓
- `effective_immutability(civilizational, arbitrage, rope)` ✓

T-axis variation within the institutional/arbitrage position (the U₃ cluster) is Hub-2-invariant. The working family's U_3_imm and U_3_civ both return rope, as would canonical U₃. The drift from canonical U₃ does not affect Hub 2 behavior at this position.

---

## 7. The Hub-2-Spanning Hypothesis

The critical structural observation for the audit: the 10-slice working family divides naturally into "mountain-immutability" slices (biographical time horizon) and "rope-immutability" slices (immediate/generational/civilizational arbitrage or constrained), with two dual slices (civilizational/analytical).

For pairs that span the mountain/rope boundary (Hub-2-spanning pairs), Hub 2 is actively discriminating — the mountain gate opens for one slice and closes for the other. For pairs within the same Hub 2 output (Hub-2-internal pairs), Hub 2 produces the same output for both slices and classification differences must come from Hub 1 (χ variation).

**Hypothesis:** T-axis dominance under Metric A (extractive fraction) is driven by Hub-2-spanning pairs, because:
1. Mountain constraints have near-zero extractive fraction at mountain-seeing slices.
2. Many of the same constraints classify as extractive at rope-seeing slices.
3. The mountain/rope distinction is determined primarily by T×E (Hub 2), and T varies between the biographical (mountain) and generational/immediate/civilizational (rope) clusters.
4. When partial correlation controls for E, T's marginal contribution is precisely the T-axis variation within Hub 2's output — which is large for the biographical/non-biographical boundary but small within the rope cluster.

If T-dominance under A is concentrated in Hub-2-spanning pairs, the original "T-axis dominance" finding is better characterized as a Hub 2 boundary effect than a continuous T-axis effect.

---

## 8. Coverage Structure: No Pure P-Axis Pairs

The working family's 10 slices vary P, T, E, and S simultaneously; no pair differs on P alone. Every pair that differs on P also differs on at least one of T, E, or S. This means partial correlation is the correct tool for isolating P-axis contribution, but the partial correlation has limited discriminatory power for P specifically — there are no clean "P-only" pairs to anchor the estimate.

The empirically weak P-axis signal (max partial ρ = 0.150 across A–E) may reflect this structural limitation of the slice family as much as it reflects P's implementation role. P is Hub 1's dominant contributor (canonical d range 0.00–1.00), but that mechanical dominance cannot be tested in isolation on a slice family with no pure P-axis pairs. This is a known limitation of the slice family's construction (coverage-driven, not experimentally designed).

---

## 9. Reframe of the Open Question

The prompt asked whether the implementation privileges a particular structural-distance metric. The reconnaissance answer is: no — the implementation does not compute distances between slices at all, and nothing in the classifier corresponds to a Hamming or similar metric. What the implementation encodes is a two-hub functional decomposition:

- **Hub 1** (P, E → χ, with S as multiplicative): produces continuous extraction variation across observer positions.
- **Hub 2** (T, E → immutability): produces discrete categorical variation in mountain/rope classification.

The closest operationalization of this structure as a positional metric is **E-weighted Hamming [1, 1, 2, 1]** (E double-weighted, others equal), reflecting E's dual hub presence. This is not the structure itself — it is one point in the space of metrics that the structure motivates testing.

The more structurally informative operationalization is the **hub-separated predictor** (hub1_diff and hub2_diff as independent binary predictors), which tests whether Hub 1 and Hub 2 variation capture statistically independent structure in the corpus.

The highest-value test is the **Hub-2-spanning partition**: if T-axis dominance under Metric A concentrates in spanning pairs, the original §4 finding requires recharacterization.

---

## 10. Tractability Assessment

| Question | Tractable? | Method |
|---|---|---|
| Does E-weighted Hamming produce different axis-dominance than unweighted? | Yes | Python: recompute partial Spearman with metric_f as predictor |
| Do Hub 1 and Hub 2 capture independent structural variance? | Yes | Python: hub1_diff / hub2_diff as binary predictors |
| Is T-dominance under A concentrated in Hub-2-spanning pairs? | Yes | Python: encode immutability table, partition pairs, compute within-subset ρ |
| Why is P weak empirically despite dominating Hub 1? | Partially | Not testable on this slice family (no pure P-axis pairs); requires different slice family |
| Does the implementation match v6.11's χ formula exactly? | Yes (answered) | Code inspection: d(P, E) vs d(P), σ(S) vs σ(S(P)) |

The untestable question (P weakness) is flagged but not pursued. A pure-P-axis slice family would require running corpus queries for pairs of slices that differ only on agent_power — feasible but outside this audit's scope.
