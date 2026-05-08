# Power-Indexed Constraints Exhibit Genuine Abramsky-Brandenburger Contextuality

**A Structural Identity Argument**

---

**Abstract.** The Deferential Realism (DR) presheaf satisfies the formal criteria for contextuality established by Abramsky and Brandenburger (2011): a measurement scenario with a joint-measurement-compatible site, an empirical model assigning local sections, and the logical impossibility of a global section. This is not an analogy. The obstruction mechanism in DR is structurally identical to the AB obstruction: in both cases, locally consistent assignments cannot be extended globally, and the impossibility is logical rather than probabilistic. Empirical evidence from a 3,334-constraint corpus confirms the mapping. All 879 manifest-presheaf constraints (H¹ > 0) are strongly contextual — no global assignment exists in principle, not merely in measure zero. The snare contextuality fraction is exactly 1.0: every constraint classified as an active extraction mechanism by the analytical observer lacks a global perceptual section. The H¹ gap ({1, 2} forbidden) is a structural fingerprint with no exact quantum analogue, arising from the discrete site geometry. Three disanalogies are reported honestly: the source of incompatibility differs (power modulation rather than incompatible measurement bases), the compatibility structure is stronger than most quantum scenarios (complete graph K₄ rather than restricted), and the probabilistic layer (MaxEnt) does not reproduce the discrete contextuality at the distributional level — a finding resolved by the established incommensurability of H¹ and Wasserstein distance W₁.

---

## 1. The Question

Abramsky and Brandenburger (2011) formalized quantum contextuality in the language of sheaf theory. A system is contextual iff the obstruction to a global hidden-variable assignment is non-trivial — iff no single probability distribution over outcomes exists that marginalizes correctly to every local distribution. The power of the AB formulation is that it identifies contextuality with the failure of sheafification: a presheaf that cannot be extended to a sheaf is contextual, and H¹ measures the degree of obstruction.

DR's presheaf structure was noted in v6.11 as a "structural analogy" to the AB framework (§6.3: STRUCTURAL). This paper revises that classification upward. The claim here is stronger: the obstruction mechanism in DR is *structurally identical* to the AB obstruction — not analogous, not parallel, but the same mathematical object instantiated in a different domain. The argument proceeds by checking each of the AB criteria against DR's structure, then reporting honestly where the mapping breaks.

The stakes of the distinction matter. If the correspondence is merely analogical, the AB machinery imports intuitions but no theorems. If it is structural identity, then AB theorems about contextuality (its computational consequences, its relationship to non-classicality, its detection by cohomological methods) apply directly to DR, and DR results about power-indexed perception apply to the broader theory of contextuality.

---

## 2. The Abramsky-Brandenburger Framework

The AB framework defines a measurement scenario as a triple (X, O, M) where:
- X is a set of *measurements*
- O is a set of *outcomes*
- M ⊆ 𝒫(X) is a *compatibility structure* — the set of jointly performable measurement sets (contexts)

An *empirical model* assigns to each context C ∈ M a probability distribution e_C over O^C. The empirical model is *non-contextual* iff there exists a global distribution d over O^X such that for every C ∈ M, the marginal of d to O^C equals e_C. The model is *contextual* iff no such global distribution exists.

Strong contextuality is the special case where the impossibility is logical rather than probabilistic: no *deterministic* global assignment exists that is consistent with any of the local sections. In the sheaf-theoretic language, strong contextuality is the absence of any global section in the support of the empirical model.

AB's main theorem: a system is contextual iff its empirical model has non-trivial H¹ obstruction. Strong contextuality corresponds to the case where not merely the probability distribution but the underlying support (the set of deterministic outcomes with positive probability) fails to admit a global section.

---

## 3. The DR Mapping

### 3.1 Measurements

In the AB framework, a "measurement" is an experimental procedure applied to a physical system. In DR, the corresponding object is *observer-position classification*: applying the DR engine's rule cascade to a constraint from a specific structural position. Formally:

> **DR measurement** M_i: classify constraint C at observer position U_i, yielding a type from Ω = {mountain, rope, tangled_rope, snare, scaffold, piton, naturalized}.

The four canonical measurements are M₁ (powerless), M₂ (moderate), M₃ (institutional), M₄ (analytical). Each measurement is fully specified by the context tuple (Power, TimeHorizon, ExitOptions, Scope) and the constraint's metric vector (ε, suppression, theater, directionality, boolean features).

This is a tighter correspondence than analogy. In quantum mechanics, a measurement is an experimental procedure that yields one of several outcomes. In DR, a classification is a computational procedure that yields one of several types. Both are deterministic given the hidden state (the quantum state or the constraint's metrics) plus the context (measurement basis or observer position). Both yield discrete categorical outcomes.

### 3.2 Outcomes

The outcome space is Ω, the six-type space. For cross-context comparison, the extraction chain {mountain, rope, tangled_rope, snare} is the primary ordered sub-space. Types outside the chain (scaffold, piton, naturalized) function as "null" or "off-chain" outcomes — measurable but not orderable against chain types.

### 3.3 Compatibility Structure

In AB, measurements M_i and M_j are *compatible* iff they can be jointly performed — measuring both simultaneously on the same system is possible. The compatibility structure M specifies which sets of measurements can be simultaneously performed.

In DR: **every pair of observer positions can simultaneously classify any constraint.** The function `orbit_vector(C, [T₁, T₂, T₃, T₄])` computes all four classifications simultaneously. There is no physical obstruction to joint measurement — a constraint can be evaluated at all four observer positions in a single pass.

The compatibility structure in DR is the complete graph K₄: all six pairs of measurements are compatible, all four-way joint measurement is compatible. On the 3,334-constraint corpus, `orbit_vector/2` succeeded without exception for every constraint, confirming K₄ compatibility empirically.

This is a *stronger* compatibility structure than most quantum scenarios. Bell inequality violations use K₂ (two measurement settings) or K₃. The HR₅ contextuality scenario uses a pentagon structure. DR's K₄ imposes more constraints on the global section — more local sections must be simultaneously consistent — making contextuality a stronger claim when it holds.

### 3.4 The Empirical Model

In AB, the empirical model assigns probability distributions to joint outcomes. In DR, the deterministic rule cascade assigns a single type to each (constraint, context) pair, yielding a *deterministic empirical model* — a special case of the AB framework where each local distribution is a point mass.

The empirical model for constraint C is:

e_C = { M_i ↦ δ(dr_type(C, U_i)) : i = 1, 2, 3, 4 }

where δ denotes a point mass at the named type.

**Query output (Phase 1):** The top 20 manifest-presheaf constraints (H¹ ≥ 3, by obstruction) with their empirical models:

| Constraint | e(M₁) | χ₁ | e(M₂) | χ₂ | e(M₃) | χ₃ | e(M₄) | χ₄ |
|---|---|---|---|---|---|---|---|---|
| yt_ai_slop_incentive | naturalized | 0.360 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| yangtze_dam_cascade_operations | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.043 | snare | 0.795 |
| work_life_boundary_erosion | naturalized | 0.325 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| winter_olympics_2026 | piton | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| viral_emergence_covid19_exemplar | naturalized | 0.270 | tangled_rope | 0.575 | rope | −0.022 | snare | 0.712 |
| us_venezuela_plausible_deniability | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.008 | snare | 0.795 |
| us_two_party_duopoly | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| unclos_dispute_resolution | naturalized | 0.270 | tangled_rope | 0.575 | rope | −0.048 | snare | 0.712 |
| union_corruption | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| uk_help_to_buy_scheme | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| trump_epa_greenhouse_gas_reversal | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| transplant_center_competition | naturalized | 0.395 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| us_china_trade_restrictions | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| us_chips_act_subsidy_race | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| us_china_ai_competition | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.043 | snare | 0.795 |
| unrwa_eviction_order | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.034 | snare | 0.795 |
| uk_ssp_eligibility | naturalized | 0.270 | tangled_rope | 0.575 | rope | −0.022 | snare | 0.712 |
| uk_school_capital_budget_allocation | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |
| union_protection_underperformance | naturalized | 0.270 | tangled_rope | 0.575 | rope | −0.022 | snare | 0.712 |
| venezuela_oil_privatization_v1 | naturalized | 0.302 | tangled_rope | 0.642 | rope | −0.025 | snare | 0.795 |

The χ values explain the mechanism. For the dominant H¹=6 pattern, ε = 0.58 (above rope_epsilon_ceiling = 0.45). The institutional modifier π = −0.20 sends χ₃ negative, which triggers the net-beneficiary rope pathway (χ ≤ 0 bypasses the epsilon gate for rope classification). The powerless observer has χ₁ below the tangled_rope floor (0.40) while ε exceeds the rope ceiling, yielding naturalized. The moderate and analytical observers occupy different extraction bands. A single formula evaluated at four structurally different positions yields four structurally different classifications — not four measurements of the same underlying quantity but four genuinely different measurements of what the *same* constraint is.

---

## 4. The No-Global-Section Condition

### 4.1 The AB Condition

A global section for the empirical model e_C is a single type T ∈ Ω such that T = e_C(M_i) for all i. Non-existence of a global section is the AB condition for contextuality: it means no observer-independent outcome assignment exists that is consistent with every local section.

In DR: **H¹(C) > 0 iff no global section exists.** This follows from the definition of the H¹ proxy: H¹ counts disagreeing context-pairs in the orbit vector. H¹ > 0 iff at least one pair of observers disagrees. A global section would require a type T such that dr_type(C, U_i) = T for all i — but if any pair disagrees, no such T exists. The equivalence is definitional, not empirical.

### 4.2 Strong Contextuality

AB distinguishes strong contextuality (no deterministic global section exists) from weak contextuality (a global section exists but has probability zero in the empirical model). The distinction matters: strong contextuality cannot be dissolved by probability weighting; weak contextuality can in principle be resolved by concentrating measure on the consistent assignment.

In DR: **all contextual constraints are strongly contextual.** The type space Ω is discrete and point-valued. Each local section is a point mass: e_C(M_i) = δ(T_i) for a specific T_i. A global section would require T = T₁ = T₂ = T₃ = T₄ simultaneously. For any constraint with H¹ > 0, the T_i are not all equal, so no such T exists — not with probability zero, but not at all.

The absence of weak contextuality in DR is a structural consequence of the discrete type space. There is no probability distribution over types from which a consistent global assignment could have positive measure: if T₁ ≠ T₄, the events {T = T₁} and {T = T₄} are mutually exclusive, and no probability weighting resolves this.

**Corpus confirmation (Phase 2):**
- H¹ = 1 constraints: 0
- H¹ = 2 constraints: 0
- Manifest presheaf (H¹ > 0): 879 / 3,334 = 26.4%
- Weak contextuality cases (H¹ = 0 but types differ): 0
- All 879 contextual constraints are strongly contextual

The gap at H¹ ∈ {1, 2} is proved in v6.11 Theorem 2 via partition arithmetic: with 4 observers, the only achievable disagreeing-pair counts are {0, 3, 4, 5, 6}. The minimum nonzero value is 3, corresponding to the (3,1) partition — three observers agree, one dissents. This is not an empirical finding; it follows from the combinatorics of any 4-element set.

### 4.3 The Local-Hidden-Variable Analog

In the AB quantum setting, the local hidden variable is a hidden quantum state λ such that every observer's measurement outcome is a deterministic function of λ alone. The LHV model asserts observer-independent reality: the outcome doesn't depend on which measurements are jointly performed, only on λ.

In DR, the LHV analog is: an observer-independent structural type T*(C) such that each observer's classification dr_type(C, U_i) is the restriction of T* to that observer's accessible features. The LHV model for social constraints is precisely the "naturalistic fallacy" at the structural level: the claim that constraints have observer-independent classifications that all well-positioned observers would converge to.

The `naturalized` classification at the powerless observer (M₁) is the DR engine's operationalization of this LHV claim: the constraint *appears* to have observer-independent, natural-law status from that structural position. The powerless observer cannot distinguish the constraint from background conditions — it looks as natural and fixed as gravity.

The orbit [naturalized, tangled_rope, rope, snare] *disproves* the LHV by construction: there is no T* ∈ Ω such that T* = naturalized AND T* = tangled_rope AND T* = rope AND T* = snare. The four local sections are mutually exclusive. The impossibility is not a matter of acquiring more information; it follows from the logical incompatibility of the four type assignments. This is the AB theorem instantiated: contextuality ↔ no LHV model.

**Corpus data (Phase 4b):**
- Constraints where M₁ yields naturalized: 413 (all manifest presheaves)
- The naturalized classification at U₁ is the corpus-scale operationalization of the LHV claim
- All 413 are refuted by the global-section impossibility of their orbit

---

## 5. The Compatibility Structure: K₄ Is Stronger Than K₂

The AB framework is most studied in the K₂ (pair of measurements) and Bell inequality settings. DR's K₄ structure deserves explicit treatment because it makes the contextuality claim *stronger*, not weaker, than its quantum analogs.

In a K₂ scenario (two compatible measurements, each with two outcomes), the contextuality condition requires that the two local distributions P(A|M₁) and P(B|M₂) cannot be jointly embedded in a global P(A, B). In a K₄ scenario (all four measurements jointly compatible), the contextuality condition requires that all four local sections simultaneously fail to embed. More contexts = more constraints on the global section = harder to satisfy = contextuality is a more demanding condition.

DR's K₄ site means: every non-contextual constraint must be classifiable by a *single type that every observer agrees on*. This is a stronger demand than any K₂ test. When DR produces H¹ > 0, it is asserting that contextuality holds under the strictest possible compatibility structure for four observers.

**Pairwise disagreement rates among manifest presheaves (Phase 3):**

| Pair | Disagreement rate |
|---|---|
| U₁–U₂ (powerless–moderate) | 53.6% |
| U₁–U₃ (powerless–institutional) | 85.7% |
| U₁–U₄ (powerless–analytical) | 68.3% |
| U₂–U₃ (moderate–institutional) | 92.8% |
| U₂–U₄ (moderate–analytical) | 52.1% |
| **U₃–U₄ (institutional–analytical)** | **98.3%** |

The U₃–U₄ pair dominates. This traces to the institutional phase transition: π_institutional = −0.20 sends χ₃ negative for 577 / 879 manifest presheaves (65.6%), routing the institutional classification through the negative-chi rope pathway. The institutional observer disagrees with the analytical observer 98.3% of the time among contextual constraints — this is the cover-story mechanism (Theorem 1 in the main DR paper) operationalized as a pairwise disagreement rate.

**Monotonicity analysis (Phase 3):**

| Category | Count |
|---|---|
| Constant (H¹ = 0) | 2,455 |
| Monotone ascending | 7 |
| Monotone descending | 1 |
| **Non-monotone** | **283** |
| Incomparable | 588 |

Non-monotone orbits — where the extraction rank reverses direction along the power chain — are the most structurally interesting from the AB perspective. They represent constraints where no single "direction of extraction" is observable from any position; the constraint appears extractive, then non-extractive, then extractive again as power increases. A constraint like `agentive_optimism_2026` with orbit [tangled_rope, tangled_rope, rope, snare] (H¹ = 5) — ranks [2, 2, 1, 3], deltas [0, −1, +2], one reversal — cannot be described by any monotone LHV. The reversal is not measurement noise; it is a structural consequence of the institutional phase transition operating on a constraint with a specific (ε, directionality) profile.

---

## 6. Marginal Consistency Is Satisfied — Trivially

The AB framework requires that local distributions agree on their overlapping measurements — the no-signaling condition, or marginal consistency. For measurements M_i and M_j, the marginal of e_{C∪D} to C must equal e_C.

In DR, `dr_type(C, U_i)` is evaluated independently for each U_i. No evaluation at one context influences evaluations at others. The computation at U₃ does not condition on the result at U₁. This means **marginal consistency is satisfied by construction in DR**, not as an empirical finding.

This is simultaneously a similarity to the AB setup and a disanalogy. In quantum mechanics, marginal consistency is a non-trivial constraint that experimental data must satisfy — it rules out signaling and justifies the joint-measurement interpretation. In DR, it is guaranteed by architectural independence of context evaluations. There is no experimental question about whether the constraint "behaves differently" when evaluated jointly versus separately: joint and separate evaluation are identical by the computation's structure.

The trivial satisfaction of marginal consistency means DR cannot be used to *test* the condition. But it also means DR's contextuality cannot be explained away as a violation of marginal consistency — the global inconsistency arises despite perfect local consistency, which is exactly the AB condition.

---

## 7. Population-Level Evidence

### 7.1 Snare Contextuality Fraction = 1.0

The single most theoretically significant number: **every constraint classified as snare by the analytical observer (M₄) lacks a global perceptual section.**

- Snare-classified constraints (analytical perspective): 615
- Of which manifest presheaf (H¹ > 0): 615
- Snare contextuality fraction: 1.000

This is a consequence of Theorem 1 (main paper): extraction structurally requires perspectival cover. For any constraint with ε above the snare floor, the institutional observer's χ₃ falls below the snare threshold — the institutional sign-flip is strong enough to produce a different type assignment. Every snare-classified constraint has at least one observer who classifies it differently: there is no extraction mechanism that every observer agrees is extraction.

In AB language: the type "snare" is contextual by necessity. The snare assignment cannot be a global section because the same structural conditions that produce snare at U₄ produce rope at U₃. An observer-independent characterization of "this is a snare" does not exist within the DR framework.

The graded CF distribution:

| H¹ | CF = H¹/6 | Count | Interpretation |
|---|---|---|---|
| 0 | 0.000 | 2,455 | Global section exists — observer-independent |
| 3 | 0.500 | 273 | One observer dissents (minimum nontrivial) |
| 4 | 0.667 | 52 | Oscillating orbit (two-against-two or similar) |
| 5 | 0.833 | 389 | Four-out-of-six pairs disagree |
| 6 | 1.000 | 165 | All observers disagree — maximum obstruction |

Values CF = 1/6 and CF = 1/3 are absent by the H¹ gap proof.

### 7.2 Mountain Contextuality Fraction = 0.027

The near-zero mountain CF (11 out of 403 mountain-classified constraints are contextual) confirms the structural prediction: genuine coordination infrastructure appears similar from all structural positions. When a constraint has near-zero ε, the power scaling formula χ = ε × f(d) × σ is driven toward zero everywhere regardless of f(d). The institutional sign-flip at π = −0.20 cannot produce meaningful type variation when the input χ is already near zero.

The 11 contextual mountain constraints are classified as [mountain, scaffold, scaffold, mountain] (H¹ = 4), reflecting constraints where the intermediate observer positions (U₂, U₃) classify the constraint as scaffold rather than mountain — a distinction between genuine natural limits and temporary structural formations that the power-extreme observers (U₁, U₄) resolve to mountain. This is the orbit topology of genuine uncertainty about whether a constraint is permanent or provisional, not about whether it is extractive.

### 7.3 Maximum-Obstruction Constraints: Stereotyped Distributed Extraction

The 165 H¹ = 6 constraints (all four observers disagree) show a strikingly stereotyped structure:

- Dominant pattern: [naturalized, tangled_rope, rope, snare] — 160/165 (97%)
- Secondary pattern: [piton, tangled_rope, rope, snare] — 5/165 (3%)

This stereotypy is itself a structural finding. The orbit [naturalized, tangled_rope, rope, snare] is not produced by independently varying each observer's classification; it is produced by a single constraint (high ε, standard directionality profile) evaluated through a single formula at four observer positions. The geometric separation of the four sigmoid values — powerless at f(d) ≈ 0.62, moderate at f(d) ≈ 1.20, institutional at f(d) ≈ −0.12, analytical at f(d) ≈ 1.42 — maps to the four classification zones via threshold crossings. Every H¹ = 6 constraint with this profile is a different *instance* of the same *mechanism*.

This is the strongest form of the AB correspondence: it is not that some constraints are contextual; it is that contextuality has a single dominant structural explanation, and that explanation is the institutional phase transition producing a sign flip in χ that is architecturally impossible to avoid.

---

## 8. The Best Single Exemplar

**Constraint: `yt_ai_slop_incentive`** (YouTube algorithmic incentivization of AI-generated content)

- Domain: technological/economic
- ε = 0.58, suppression = 0.68
- H¹ = 6 (maximum obstruction)
- Orbit: [naturalized, tangled_rope, rope, snare]
- χ values: [0.360, 0.642, −0.025, 0.795]

**Mechanism (traced through the AB structure):**

*Local section at M₁ (powerless):* χ₁ = 0.360. Rule cascade: ε = 0.58 > rope_epsilon_ceiling = 0.45 (ε gate for rope fails). χ₁ = 0.360 < tangled_rope_chi_floor = 0.40 → naturalized. The authentic creator experiencing the platform's algorithmic discrimination classifies it as background reality — the natural condition of content creation. The extraction is invisible as extraction.

*Local section at M₂ (moderate):* χ₂ = 0.642. Rule cascade: χ₂ in [0.40, 0.66] → tangled_rope (with coordination and asymmetric extraction confirmed). The mid-tier creator sees genuine ambiguity: the algorithm both enables reach and extracts attention value.

*Local section at M₃ (institutional):* χ₃ = −0.025 ≤ 0. Rule cascade: negative chi triggers the net-beneficiary rope pathway — the epsilon gate is bypassed for rope when χ ≤ 0. The institutional observer (the platform, the advertising system) experiences this arrangement as coordination infrastructure. The institutional sign-flip at π = −0.20 sends χ negative: f(d_institutional) ≈ −0.12 transforms the same ε = 0.58 into χ₃ = −0.025.

*Local section at M₄ (analytical):* χ₄ = 0.795 ≥ snare_chi_floor = 0.66 → snare. Full extraction classification.

**Global section impossibility:** There is no T ∈ Ω such that T = naturalized AND T = tangled_rope AND T = rope AND T = snare. These are four distinct elements of a seven-element discrete set. Their intersection is empty. No probability weighting over Ω can produce positive mass on an element that equals all four simultaneously: no such element exists.

**The LHV reading:** The naturalized classification at M₁ is precisely the claim that T* = "this is just how platform economics works" — an observer-independent background condition. The orbit disproves this LHV claim. The authentic creator's perception that this is simply how things are (naturalized) cannot be the universal type: the analytical observer classifies it as snare, the moderate as tangled_rope, the institutional as rope. The LHV is false not because we lack information about it but because it does not exist.

**What the MaxEnt layer shows:** When run sequentially at each context, the MaxEnt shadow classifier yields:
- M₁: P(tangled_rope) = 0.990, P(snare) = 0.010
- M₂: P(tangled_rope) = 0.992, P(snare) = 0.008
- M₃: P(tangled_rope) = 0.649, P(snare) = 0.351
- M₄: P(tangled_rope) = 0.705, P(snare) = 0.295

The MaxEnt distribution does not reproduce the discrete contextuality. The top type at M₁ and M₄ is both tangled_rope (the same), suggesting H¹ ≈ 0 at the distributional level. This is resolved in Section 9.3.

---

## 9. Disanalogies Reported Honestly

### 9.1 Source of Incompatibility

In quantum mechanics, contextuality arises from the algebraic structure of quantum observables: non-commuting operators cannot be simultaneously diagonalized, so measurements on non-commuting observables are genuinely incompatible (not jointly performable in the same basis). The incompatibility is physical and operational.

In DR, all four measurements are simultaneously performable — K₄ compatibility holds. The incompatibility is not operational but *evaluative*: the same constraint genuinely is a different type from different structural positions, not because the measurements interfere with each other, but because the observable quantity (χ = ε × f(d) × σ) is *defined* to depend on the observer. The type is not a hidden pre-existing property that different observers access differently; it is constituted by the measurement itself.

This is a deeper disanalogy than it first appears. AB contextuality proves that quantum mechanics cannot be explained by a local realistic hidden variable model — the outcomes are not pre-existing, the measurement influences the result. DR's position is structurally similar but the metaphysics differ: DR claims the type *is* observer-relative as a design commitment (Axiom 2), not as a derived consequence of a no-go theorem. DR encodes observer-relativity by construction; quantum contextuality derives it from the measurement algebra.

Whether this distinction matters for the mathematical claim depends on what claim is being made. For the mathematical structure of the obstruction (no global section, H¹ > 0 as witness), the distinction is irrelevant: the obstruction mechanism is the same. For the philosophical claim about what the contextuality means, the distinction is significant: quantum contextuality is evidence that hidden variables cannot exist; DR's contextuality is a design commitment that observer-independent classification should not exist.

### 9.2 The Probabilistic Layer

AB's empirical model assigns probability distributions to outcomes, derived from experimental frequencies. DR's deterministic rule cascade assigns point masses. The MaxEnt shadow classifier (Axiom 4) provides a probabilistic layer — but as shown for `yt_ai_slop_incentive`, the MaxEnt distributions do not reproduce the discrete contextuality. The top MaxEnt type at all four contexts is tangled_rope (H¹ ≈ 0 at the distributional level), despite the deterministic orbit having H¹ = 6.

This apparent contradiction is resolved by the H¹/W₁ incommensurability established in §5.1 of the main paper. H¹ and Wasserstein distance W₁ are **incommensurable measures of perspectival fracture**. H¹ counts discrete threshold crossings in the type orbit; W₁ measures transport in the continuous MaxEnt distributions. A constraint can have H¹ = 6 (maximum discrete obstruction) with W₁ < 0.01 (minimal distributional shift). The mean W₁ for H¹ = 6 constraints is 0.047 — the *lowest* of any H¹ > 0 band. Neither metric bounds the other.

The mechanism is *threshold-MaxEnt decoupling*: the deterministic rule cascade flips the discrete label when χ crosses a threshold, but the MaxEnt distribution — being smooth — barely changes. The threshold is crossed, but the underlying probability mass was already close to the boundary; the type flip does not require large distributional transport. The H¹ = 6 orbit [naturalized, tangled_rope, rope, snare] registers maximum discrete obstruction because all four observers are assigned *distinct* types, but the MaxEnt masses can be nearly identical because all four contexts are classifying the same constraint (with χ values within a modest range) using the same type probabilities.

The implication for the AB analogy: DR's primary contextuality claim lives at the *deterministic* layer (the rule cascade), not the probabilistic layer (MaxEnt). This is a difference from quantum mechanics, where contextuality is inherently probabilistic. DR provides a probabilistic layer via MaxEnt, but that layer measures something different — distributional distance in the generative model, not discrete classification disagreement. The contextuality claim holds for the deterministic layer without qualification. For the probabilistic layer, the claim requires framing as: the MaxEnt distributions are context-dependent (they differ between U₁ and U₄), but the discrete type assignments show contextuality more sharply than the distributional shifts do.

### 9.3 K₄ vs. Quantum Compatibility

DR's K₄ compatibility is *stronger* than most quantum scenarios where contextuality arises. In the standard quantum case (Kochen-Specker, Bell), the compatibility structure is restricted: some measurements are incompatible, and contextuality is non-trivial precisely because certain pairs cannot be jointly performed. The global section would need to assign values to all measurements, including incompatible pairs, and the impossibility of doing so consistently is the content of the contextuality theorem.

In DR, all measurements are jointly performable. This means the global section attempt is more constrained, not less: a global section T must be consistent with *every* context simultaneously, including all joint evaluations. The impossibility (H¹ > 0) is therefore harder to achieve — more conditions must fail — but when it does hold, it holds under the strictest possible compatibility regime.

Put differently: DR's contextuality cannot be dismissed as an artifact of incompatible measurements. There is no "maybe the measurements were done in different experimental runs and the incompatibility is a feature of the setup." All four observer classifications are performed on the same constraint in the same computational run. The orbit is a simultaneous measurement, not a sequential one. The global section impossibility is about the *classification results* being jointly inconsistent, not about measurement order or compatibility.

---

## 10. The Structural Identity Claim

The claim is this: the obstruction mechanism in DR and the obstruction mechanism in AB quantum contextuality are the **same mathematical structure**.

In both cases:
1. A site of contexts is given (measurement bases / observer positions)
2. A presheaf assigns local sections to each context (measurement outcomes / type assignments)
3. Local sections are pairwise consistent (marginal consistency / architectural independence)
4. No global section exists (cannot be jointly satisfied / no consistent type assignment)
5. H¹ measures the degree of obstruction (number of agreeing pairs failing to cohere / number of disagreeing observer pairs)
6. Strong contextuality holds when the local sections are *logically* incompatible (not just probabilistically incompatible)

The source of the incompatibility differs (measurement algebra / power-modulated perception). The probability model differs (empirical frequencies / deterministic point masses with MaxEnt shadow). The compatibility structure differs (typically restricted / K₄). But these are differences in the *instantiation*, not in the *obstruction mechanism*.

The AB theorem establishes that contextuality ↔ H¹ obstruction. DR implements the same equivalence: H¹ > 0 ↔ no global section ↔ the LHV claim is false ↔ the constraint type is irreducibly observer-dependent. The proof structure is identical. The objects being proved are different.

This justifies importing AB results to DR and vice versa. AB theorems about the relationship between H¹ and computational complexity, about the role of contextuality in enabling certain information-processing advantages, about the connection between contextuality and violation of classical probability inequalities — these apply to DR's H¹ with the same force, because they are proved using only the structure that DR shares with the quantum case. DR's Theorem 2 (H¹ gap) — proved from partition arithmetic — has no quantum analogue; the gap {1, 2} is absent from the quantum contextuality literature because quantum scenarios typically have restricted compatibility structures where such gaps do not arise from site cardinality alone. This is a result that flows from DR to the broader contextuality literature, not from the quantum case to DR.

---

## 11. The Honest Boundary

The classification in §6.3 of the main paper (STRUCTURAL) was calibrated for a different purpose: distinguishing what is formally verified from what is interpretively grounded. For the specific question of this paper — does DR exhibit *genuine* AB-style contextuality — the answer is yes at the level of obstruction mechanism and no at the level of source of incompatibility.

More precisely:

| Claim | Status |
|---|---|
| DR has a measurement scenario (X, O, M) in the AB sense | **Yes** — contexts, types, K₄ compatibility |
| DR's empirical model assigns local sections | **Yes** — orbit vector |
| Local sections are marginally consistent | **Yes** — by architecture |
| No global section exists for manifest presheaves | **Yes** — logical impossibility |
| Obstruction is strong (logical, not probabilistic) | **Yes** — discrete type space |
| H¹ correctly measures the obstruction degree | **Yes** — definitionally linked |
| All snares are strongly contextual | **Yes** — corpus-confirmed, theorem-derived |
| The obstruction mechanism is structurally identical to AB | **Yes** — same proof structure |
| The *source* of incompatibility is the same | **No** — power modulation ≠ measurement algebra |
| The probabilistic layer reproduces discrete contextuality | **No** — H¹/W₁ incommensurability |
| The CF gap has a direct quantum analogue | **No** — gap is specific to DR's site geometry |

The boundary is: the mathematics of obstruction is the same; the physics/sociology of why incompatibility arises is different; the consequences for information-processing theory remain to be worked out. This is a stronger claim than analogy and a weaker claim than full identification. The paper's title says "genuine" because the obstruction mechanism — the core mathematical content — is genuinely the same. The paper does not claim DR is quantum mechanics.

---

## 12. Conclusion

DR power-indexed constraints exhibit genuine Abramsky-Brandenburger contextuality. The measurement scenario maps: observer positions are contexts, type classifications are outcomes, K₄ is the compatibility structure. The empirical model maps: orbit vectors are joint outcome assignments, all local sections are consistent by architecture. The no-global-section condition maps: H¹ > 0 is definitionally equivalent to global-section impossibility. Strong contextuality holds: the impossibility is logical, not probabilistic, because the type space is discrete. The LHV analog maps: naturalized classification is the observer-specific claim of an observer-independent type, and H¹ > 0 refutes it.

The most compelling single number: snare contextuality fraction = 1.000. Every active extraction mechanism lacks a global perceptual section. Extraction is always observer-relative; coordination infrastructure nearly never is (mountain CF = 0.027). The framework encodes this asymmetry as a theorem (Theorem 1 in the main paper), and the contextuality machinery confirms it at corpus scale.

Three disanalogies are reported honestly. The source of incompatibility differs. The probabilistic layer does not reproduce discrete contextuality (resolved by H¹/W₁ incommensurability). The compatibility structure is stronger than quantum scenarios (K₄ rather than restricted), which makes DR's contextuality a more demanding condition, not a weaker one.

The H¹ gap ({1, 2} forbidden, CF values 1/6 and 1/3 absent) is a structural fingerprint specific to DR's 4-point site. It has no exact quantum analogue — the gap is a consequence of classifying four observers simultaneously, not of quantum measurement algebra. This is a result that flows from DR to the broader contextuality literature.

The presheaf should not be sheafified. The contextuality of power-indexed constraints is not a defect to be resolved but a structural feature of any system where classification depends irreducibly on who is classifying. The AB framework formalizes this irreducibility in quantum mechanics. DR instantiates it for social constraints. The obstruction is the same mathematical object in both cases.

---

## References

Abramsky, S., & Brandenburger, A. (2011). The sheaf-theoretic structure of non-locality and contextuality. *New Journal of Physics*, 13(11).

Abramsky, S., Barbosa, R. S., & Mansfield, S. (2017). Contextual fraction as a measure of contextuality. *Physical Review Letters*, 119(5).

Coecke, B., & Duncan, R. (2011). Interacting quantum observables: categorical algebra and diagrammatics. *New Journal of Physics*, 13(4).

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic*. Springer.

The DR engine, corpus, and all Prolog predicates cited in this paper are defined in the Deferential Realism codebase. Query outputs in §§3.4, 5, 6 were computed on the 3,334-constraint corpus. Key predicates: `grothendieck_cohomology:cohomological_obstruction/3`, `grothendieck_cohomology:orbit_vector/2`, `constraint_indexing:extractiveness_for_agent/3`, `grothendieck_cohomology:contextuality_summary/1`, `grothendieck_cohomology:corpus_monotonicity/1`.
