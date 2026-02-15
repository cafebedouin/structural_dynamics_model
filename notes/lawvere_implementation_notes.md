# Lawvere Implementation Notes

## Categorical Coherence Audit — Deferential Realism Codebase

**Date:** 2026-02-14
**Scope:** All Prolog modules in the Structural Dynamics Model
**Purpose:** Validate and correct Gemini's categorical mappings; extend to subsystems added after Gemini's review (MaxEnt, abductive engine, trajectory mining, FPN iterator); establish rigor classifications for each mapping.

---

## Rigor Classification Scale

Every categorical mapping is classified as one of:

- **STRICT** — Formal categorical correspondence that holds mathematically. The code implements the categorical structure, and the correspondence can be verified by checking the relevant diagrams, identities, or universal properties.
- **STRUCTURAL** — Functionally analogous correspondence. The code exhibits the *behavior* of the categorical concept (e.g., fixed-point convergence, distribution over a classifier), but the full formal structure (e.g., unit/counit, triangle identities, Giry monad axioms) is not present or has not been proven.
- **LOOSE** — Evocative analogy only. The categorical vocabulary would mislead if taken literally. Useful for intuition but not for formal reasoning about the system.

---

## 1. Naming Landscape Audit

### 1.1 drl_core.pl — Domain Metaphors

The foundational module uses Deferential Realism's constraint typology:

| Name | Origin | Role |
|------|--------|------|
| `mountain` | Domain metaphor | Immutable natural law |
| `rope` | Domain metaphor | Legitimate coordination mechanism |
| `snare` | Domain metaphor | Extractive trap |
| `tangled_rope` | Domain metaphor | Coordination mechanism with hidden extraction |
| `scaffold` | Domain metaphor | Temporary coordination, has sunset clause |
| `piton` | Domain metaphor | Performative constraint (high theater) |
| `indexically_opaque` | Technical | Contradictory metrics — unclassifiable |
| `unknown` | Technical | Fallback when no threshold fires |

Helper predicates use these names directly: `is_mountain/3`, `is_snare/3`, etc. Context infrastructure uses generic names: `context/4`, `standard_context/1`, `valid_context/1`, `power_order/2`.

### 1.2 structural_signatures.pl — Mixed Naming

Three naming registers coexist:

- **Boltzmann engine:** `boltzmann_compliant/2`, `cross_index_coupling/2`, `boltzmann_invariant_mountain/2` — physics-derived names for naturality tests
- **Derived signatures:** `false_natural_law/2`, `coupling_invariant_rope/2`, `false_ci_rope/2` — composite names blending domain (rope, natural_law) with diagnostic function (false, coupling_invariant)
- **Purity layer:** `purity_score/2`, `structural_purity/2`, `factorization_subscore/2` — mathematical names for the meta-invariant

### 1.3 dirac_classification.pl — Physics Analogies

Consistently uses Dirac-inspired gauge theory vocabulary: `gauge_orbit/2`, `gauge_fixed/3`, `gauge_freedom/3`, `preserved_under_context_shift/2`, `dirac_class/3`. The analogy is productive: the gauge orbit *is* mathematically an orbit under the group of context automorphisms.

### 1.4 drl_modal_logic.pl — Mixed

- **Composition:** `composition_rule/3`, `composite_type/4` — algebraic names
- **Purity network:** `effective_purity/4`, `type_contamination_strength/2`, `type_immunity/2` — epidemiological metaphor
- **FPN iterator:** `fpn_run/2`, `fpn_ep/3`, `fpn_iterate/5` — abbreviation-based (FPN = Fixed-Point Network)
- **Modal reasoning:** `possibly/1`, `necessarily/1` — standard modal logic

### 1.5 maxent_classifier.pl — Information Theory

Consistently uses information-theoretic vocabulary: `maxent_distribution/3`, `maxent_entropy/3`, `gaussian_log_likelihood/4`, `normalize_log_probs/2`, `shannon_entropy/2`. The naming is precise and self-consistent.

### 1.6 abductive_engine.pl — Philosophical

Uses Peircean terminology: `abductive_run/2`, `abductive_hypotheses/3`, `abductive_genuine/2`, `abductive_artifacts/2`. Trigger names describe the phenomenon: `trigger_signature_override_artifact`, `trigger_deep_deception`, `trigger_confirmed_liminal`.

### 1.7 trajectory_mining.pl — Mathematical

The most mathematical naming in the codebase: `constraint_trajectory/3`, `trajectory_distance/4`, `structural_isomorphism/4`, `structural_family/2`, `cross_domain_twins/3`. Uses hierarchical agglomerative clustering (HAC) terminology correctly.

### 1.8 logical_fingerprint.pl — Mixed

Fingerprint metaphor (`logical_fingerprint/2`, `fingerprint_shift/2`, `fingerprint_voids/2`, `fingerprint_zone/2`) with structural terms (`shift_family/2`, `fingerprint_coupling/2`).

### 1.9 drl_lifecycle.pl — Ad Hoc

The least systematic naming: `drift_event/3`, `zone_migration`, `purity_drift`, `scan_constraint_drift/2`. "Drift" is overloaded across temporal change, purity degradation, and zone reclassification.

### 1.10 Cross-Module Inconsistencies

| Concept | Module A Name | Module B Name |
|---------|--------------|---------------|
| Evaluation across contexts | `gauge_orbit/2` (dirac) | `constraint_trajectory/3` (trajectory) |
| Structural equivalence | `dirac_class/3` (dirac) | `structural_family/2` (trajectory) |
| Context set | `standard_context/1` (dirac) | `standard_contexts/1` (trajectory, plural) |
| Purity | `purity_score/2` (signatures) | `effective_purity/4` (modal_logic) |
| Type distance | `type_distance/3` (trajectory) | implicit in `composition_rule/3` (modal_logic) |

---

## 2. Gemini Mapping Validation

### 2.1 Context as Site — [STRICT]

**Gemini's claim:** The context tuple `I = (Power, Time, Exit, Scope)` functions as a site in the topos-theoretic sense.

**Verification:** The context 4-tuple (`drl_core.pl:333`) parametrizes a finite poset category. The 4 standard contexts (`dirac_classification.pl:standard_context/1`) form the objects. The power ordering (`drl_core.pl:198–203`) defines morphisms: `powerless < moderate < powerful < organized < institutional < analytical`. Time and scope provide additional indexing axes.

This is a genuine site: a small category equipped with a coverage (the standard contexts serve as a covering family). The finite poset structure is the simplest non-trivial site, and the code implements it faithfully.

**Rigor: STRICT.** The poset category with standard contexts as cover is a well-defined site.

### 2.2 Classification as Presheaf — [STRICT, corrected from "sheaf"]

**Gemini's claim:** `dr_type/3` implements a sheaf over the site.

**Correction:** `dr_type/3` (`drl_core.pl:333`) is a **presheaf**, not a sheaf. A sheaf requires the gluing axiom: if local sections agree on overlaps, they glue to a global section. The entire diagnostic infrastructure (Dirac orbits, MaxEnt disagreements, abductive triggers) exists precisely because local sections *do not* glue — a constraint can classify as `mountain` from one context and `rope` from another. This perspectival gap is a feature, not a bug. The `gauge_orbit/2` predicate (`dirac_classification.pl:156`) collects all local sections; `preserved_under_context_shift/2` (`dirac_classification.pl:178`) tests whether gluing happens to hold for a particular constraint.

`dr_type(C, Context, Type)` maps each constraint to a type *at each context* — this is exactly presheaf evaluation: for each object of the site, assign a value from the type space. The restriction maps are implicit in the power ordering.

**Rigor: STRICT.** The presheaf structure is genuinely implemented. The correction from "sheaf" to "presheaf" is substantive: the gluing axiom's intentional violation is the system's central insight.

### 2.3 Types as Subobject Classifier — [LOOSE, corrected from "Heyting algebra"]

**Gemini's claim:** The 8 constraint types form a Heyting algebra (subobject classifier of a topos).

**Correction:** The type space is NOT a Heyting algebra. `composition_rule/3` (`drl_modal_logic.pl:192–219`) reveals two absorbing elements:

```prolog
composition_rule(mountain, _, mountain) :- !.    % line 192
composition_rule(_, mountain, mountain) :- !.    % line 193
composition_rule(piton, _, piton) :- !.          % line 215
composition_rule(_, piton, piton) :- !.          % line 216
```

Both `mountain` and `piton` absorb everything. A Heyting algebra has exactly one top element (⊤). Having two absorbing elements means `composition_rule` is not even a lattice operation, since `mountain ∘ piton` would need to equal both `mountain` and `piton` — but the cut on line 192 makes `mountain` win.

The composition is better described as a **priority cascade**: mountain > piton > snare > tangled_rope > rope > scaffold > unknown. This is a total preorder with priority-based resolution, not a lattice meet.

**What IS present:** The 8 types form a finite set with a priority ordering. `classify_from_metrics/6` (`drl_core.pl:252–321`) implements this as a deterministic cascade. The composition operation is associative and has an identity (`unknown`), making it a monoid — but not a lattice.

**Rigor: LOOSE.** The type space is a monoid under composition, not a Heyting algebra. The categorical language of subobject classifiers would mislead.

### 2.4 Signature Resolution as Priority Override — [LOOSE, corrected from "meet"]

**Gemini's claim:** `resolve_modal_signature_conflict/3` implements a meet (∧) operator on the type lattice.

**Correction:** `resolve_modal_signature_conflict/3` (`structural_signatures.pl:741–775`) is a priority-based dispatch table, not a lattice meet. It has no commutativity (`resolve(mountain, natural_law, R)` → `R = mountain`, but the signature argument is structurally different from the type argument), no associativity, and no connection to a lattice join that would make it part of a Heyting algebra.

The actual structure is: structural signatures can **override** metric-based classification. Three signatures override unconditionally (natural_law → mountain, false_natural_law → tangled_rope, coupling_invariant_rope → rope). Others override conditionally (only when metric classification is `mountain` or `unknown`). The fallback clause (`line 775`) preserves the metric classification.

**Rigor: LOOSE.** This is a conditional override dispatch, not a meet operator.

### 2.5 Power-Scaling as Parametric Family — [STRUCTURAL, corrected from "adjunction"]

**Gemini's claim:** Power scaling `χ = ε × π(P) × σ(S)` constitutes an adjoint functor pair, with `snare_immutability_check/1` as left adjoint (∃) and `boltzmann_invariant_mountain/2` as right adjoint (∀).

**Correction:** There is no formal adjunction here. An adjunction requires:
1. Two functors F ⊣ G between categories
2. A natural bijection Hom(F(A), B) ≅ Hom(A, G(B))
3. Unit η: Id → GF and counit ε: FG → Id satisfying triangle identities

`extractiveness_for_agent/3` (`constraint_indexing.pl`) computes `χ = ε × sigmoid(π(P)) × σ(S)`, which is a parametric family of scalings indexed by context. `snare_immutability_check/1` (`drl_core.pl:188`) is an existential quantifier over contexts (∃ context such that rope), and `boltzmann_invariant_mountain/2` is a universal quantifier (∀ dimensions, classification is mountain). These are quantifiers, not adjoint functors — the quantifier/adjunction correspondence (Lawvere) requires the quantifiers to satisfy the adjunction triangle identities, which has not been verified.

**What IS present:** The sigmoid scaling creates a genuine parametric family of type assignments indexed by power level. This is structurally analogous to a family of functors, but without formal proof of the adjunction.

**Rigor: STRUCTURAL.** The parametric family is real; the adjunction is unproven.

### 2.6 Boltzmann as Naturality — [STRICT]

**Gemini's claim:** The Boltzmann factorization test is a genuine naturality condition.

**Verification:** `cross_index_coupling/2` (`structural_signatures.pl:893`) tests whether `χ(P, S) ≈ f(P) × g(S)` — whether the classification map factorizes across the Power × Scope grid. This is exactly the naturality square condition: the diagram

```
(P₁, S) --P-shift--> (P₂, S)
   |                      |
 classify              classify
   |                      |
   v                      v
Type₁    --should=-->   Type₂ (predicted by P-shift alone)
```

commutes if and only if the classification factorizes. `boltzmann_compliant/2` (`structural_signatures.pl:824`) tests this with a coupling threshold, producing `compliant(Score)` or `non_compliant(Score, Threshold)`.

This is the strongest mapping in the codebase. The factorization test is a naturality condition by construction: it checks commutativity of the classification functor with respect to index-dimension morphisms.

**Rigor: STRICT.** This is genuine naturality testing.

### 2.7 FNL as Naturality Failure — [STRICT]

**Gemini's claim:** `false_natural_law/2` detects naturality failures.

**Verification:** `false_natural_law/2` (`structural_signatures.pl:1345`) fires when a constraint claims naturality (passes profile tests for mountain) but fails the Boltzmann compliance test (non-commutativity detected). This is precisely a naturality failure witness: the constraint's classification does not commute with context morphisms, yet it claims to be a natural law (which should be context-invariant).

The FNL evidence structure (`fnl_evidence(Claim, BoltzmannResult, CouplingScore, CoupledPairs, ExcessExtraction)`) records which naturality square fails and how. Similarly, `coupling_invariant_rope/2` (`structural_signatures.pl:1412`) is a naturality *certificate* — all four conditions pass.

**Rigor: STRICT.** FNL is a naturality failure detector; CI_Rope is a naturality certificate.

### 2.8 Contamination as Contravariant Flow — [STRUCTURAL]

**Gemini's claim:** `effective_purity/4` implements a contravariant functor.

**Verification:** `effective_purity/4` (`drl_modal_logic.pl:1500`) computes purity by subtracting contamination pressure from neighbors:

```prolog
RawEff is Intrinsic - TotalContam * Immunity
```

Contamination flows from lower-purity constraints to higher-purity ones (`line 1546: Delta is max(0.0, MyPurity - OtherPurity)`). This reversal of flow direction — contamination moves *against* the purity gradient — is structurally analogous to contravariance. The `type_contamination_strength/2` and `type_immunity/2` tables (`lines 1474–1494`) define the emission and absorption coefficients.

However, this is not a formal contravariant functor in the categorical sense. A contravariant functor reverses morphisms between categories; here, the "reversal" is of a scalar gradient, not of morphisms. The analogy is productive (it explains why high-purity constraints are vulnerable to low-purity neighbors) but not formally precise.

**Rigor: STRUCTURAL.** The gradient reversal is genuine; the functor formalization is absent.

---

## 3. Extended Mappings — New Subsystems

### 3.1 MaxEnt as Distribution on Ω — [STRUCTURAL]

**Predicate:** `maxent_distribution/3` (`maxent_classifier.pl:365`)

The MaxEnt classifier computes a probability distribution over the type space Ω = {mountain, rope, tangled_rope, snare, scaffold, piton} for each constraint at each context. Mathematically, this assigns to each constraint-context pair a point in the probability simplex Δ⁵.

This is structurally analogous to the Giry monad's action: the Giry monad sends a measurable space X to the space of probability measures on X, and `maxent_distribution/3` sends each stalk of the presheaf to a probability measure on Ω. However, the full Giry monad structure requires:
1. A unit η: X → GX (Dirac delta embedding) — partially present via the deterministic classifier
2. A multiplication μ: GGX → GX (distribution over distributions) — absent
3. Naturality of η and μ — unverified

The log-sum-exp normalization (`maxent_classifier.pl:260–272`) and Shannon entropy computation (`lines 351–358`) are standard probability theory. The signature override adjustment (`lines 297–324`) breaks the pure MaxEnt structure but is well-motivated as a Bayesian prior update.

**Rigor: STRUCTURAL.** The distribution on Ω is genuine; the Giry monad structure is incomplete.

### 3.2 Abductive Engine as Naturality Auditor — [STRUCTURAL]

**Predicate:** `abductive_run/2` (`abductive_engine.pl:600`)

The 8 trigger classes can be understood as cross-functor consistency checks:

1. `trigger_signature_override_artifact` — explains disagreement between the MaxEnt functor and the deterministic functor as a known override effect
2. `trigger_deep_deception` — detects when the profile functor (claims mountain) and the Boltzmann functor (says non-compliant) disagree
3. `trigger_metric_structural_divergence` — high MaxEnt entropy (metric ambiguity) but stable Dirac orbit (structural clarity)
4. `trigger_confirmed_liminal` — three independent functors (MaxEnt, Dirac, drift) all detect transition
5. `trigger_coverage_gap` — Dirac detects variance but mismatch detector misses it
6. `trigger_accelerating_pathology` — FPN equilibrium and temporal drift agree on contamination
7. `trigger_contamination_cascade` — multi-hop propagation worse than one-hop
8. `trigger_dormant_extraction` — clean metrics but extractive fingerprint voids

Each trigger synthesizes signals from 2–3 independent subsystems. Categorically, each trigger tests whether the diagram formed by these subsystems commutes — and reports when it doesn't (genuine finding) or explains why non-commutativity is expected (artifact).

The artifact/genuine distinction is itself categorically meaningful: artifacts are *expected* naturality failures (the override mechanism intentionally breaks commutativity), while genuine findings are *unexpected* naturality failures.

**Rigor: STRUCTURAL.** The cross-functor consistency checking is genuine, but the triggers are hand-crafted rather than derived from formal categorical constructions.

### 3.3 Trajectory Mining as Natural Transformation Families — [STRUCTURAL]

**Predicate:** `constraint_trajectory/3` (`trajectory_mining.pl:106`)

A trajectory collects the presheaf evaluation of a constraint across all 4 standard contexts, producing a vector of (type, chi, entropy, confidence) tuples. Two constraints with the same trajectory exhibit the same natural transformation behavior — they transform identically under context shifts.

`structural_isomorphism/4` (`trajectory_mining.pl:737`) tests whether two constraints are naturally isomorphic — same transformation behavior across the site. The isomorphism evidence checks shift matching, zone matching, void matching, coupling band matching, and trajectory distance.

`structural_family/2` (`trajectory_mining.pl:721`) groups constraints into equivalence classes under trajectory similarity via HAC clustering. These families are analogous to equivalence classes of natural transformations.

The 4-component weighted distance metric (`trajectory_mining.pl:313–325`) with shift, metric, stability, and pathology components provides a principled similarity measure that respects the categorical structure (shift distance measures type consistency, stability distance measures naturality health).

**Rigor: STRUCTURAL.** Trajectories genuinely capture transformation behavior across the site. The "natural transformation equivalence" framing is productive but not formally constructed as a functor category.

### 3.4 FPN as Terminal Coalgebra — [STRUCTURAL]

**Predicate:** `fpn_iterate/5` (`drl_modal_logic.pl:1890`)

The FPN iterator computes the greatest fixed point of the contamination endofunctor. Starting from intrinsic purities, it iteratively applies the contamination propagation operator (Jacobi-style simultaneous update) until convergence:

```prolog
fpn_iterate(Constraints, Context, K, MaxIter, Eps) :-
    findall(C-NewEP, (member(C, Constraints), fpn_compute_ep(C, Context, NewEP)), NewValues),
    fpn_jacobi_update(NewValues, Context, MaxDelta),
    (MaxDelta < Eps -> converged ; K >= MaxIter -> capped ; recurse).
```

The convergence proof depends on the contamination operator being monotone and bounded (`drl_modal_logic.pl:1514–1518`): contamination is non-decreasing, and purity is floored at 0.0. By the Knaster-Tarski theorem, a monotone operator on a complete lattice has a greatest fixed point, and iterating from the top (intrinsic purities) converges to it.

In coalgebraic terms, the greatest fixed point of an endofunctor on a category is the terminal coalgebra. The FPN equilibrium purities are the terminal coalgebra of the contamination endofunctor on the purity lattice.

**Rigor: STRUCTURAL.** The fixed-point computation is genuine and the convergence guarantee is sound. The coalgebraic framing is standard for greatest fixed points but has not been formally verified against the coalgebra axioms.

---

## 4. Corrections Log

1. **Sheaf → Presheaf** (§2.2): `dr_type/3` is a presheaf, not a sheaf. The gluing axiom is intentionally violated. This is the most important correction — the perspectival gap is the system's central diagnostic insight.

2. **Heyting algebra → Priority cascade monoid** (§2.3): The type space has two absorbing elements (`mountain` and `piton`), making `composition_rule/3` a monoid operation, not a lattice meet. A Heyting algebra requires a unique top element.

3. **Meet operator → Priority dispatch** (§2.4): `resolve_modal_signature_conflict/3` is a conditional override table, not a lattice meet. It lacks commutativity and associativity.

4. **Adjunction → Parametric family** (§2.5): The sigmoid scaling creates a parametric family of type assignments, but the adjunction triangle identities have not been verified. The existential/universal quantifier structure is suggestive but insufficient for a formal adjunction claim.

5. **Gemini's overall assessment was too generous**: Gemini called all 5 of its mappings "strong." Two are STRICT (Boltzmann naturality, FNL naturality failure), one is STRUCTURAL (contamination flow), and two are LOOSE (type space algebra, signature resolution). The Boltzmann engine IS the categorical heart of the system; the type space is emphatically not a Heyting algebra.

---

## 5. Open Questions

### 5.1 Could the type space be made into a genuine lattice?

The two-absorbing-element problem could be resolved by:
- Making `piton` non-absorbing (piton ∘ mountain = mountain, piton ∘ snare = snare)
- Or by treating `piton` as a separate axis (extraction-type × performance-type)
- Or by accepting that the type space is a bounded semilattice with two tops

Each option has different implications for the classification cascade. The current priority-based resolution is pragmatically correct — but it cannot be called a lattice.

### 5.2 Does the system implicitly enforce gluing?

The `preserved_under_context_shift/2` predicate identifies constraints where gluing *happens* to hold (singleton orbits). The abductive engine's `trigger_metric_structural_divergence` fires when MaxEnt entropy is high but the Dirac orbit is singleton — this is exactly a gluing success amid metric noise. If all constraints were gluing-compliant, the presheaf would be a sheaf. The system currently reports on gluing status but does not enforce it.

### 5.3 Is there a hidden adjunction in the sigmoid scaling?

The sigmoid function `χ = ε × sigmoid(π × P) × σ(S)` is monotone in P. If the mapping `Context ↦ sigmoid(π × P)` could be shown to have a right adjoint (a "forgetful" operation recovering the base extractiveness from the scaled version), this would establish a genuine Galois connection. The logarithm is the obvious candidate (log-sigmoid is the logit), but the multiplicative structure `ε × sigmoid(·) × σ(·)` may prevent clean factorization.

### 5.4 Is the abductive engine's artifact/genuine distinction a naturality criterion?

Artifacts are expected non-commutativity (the system intentionally breaks naturality via overrides). Genuine findings are unexpected non-commutativity. This suggests a formal criterion: a finding is "genuine" if it cannot be explained as the image of a known non-natural transformation (override). Making this precise would require formalizing the override mechanism as a natural transformation with known failure locus.

### 5.5 What categorical structure does the trajectory distance metric implement?

The 4-component weighted distance (shift, metric, stability, pathology) on the space of trajectories is a pseudometric. If the weights were chosen to make it a metric (which they approximately are, given non-degeneracy of the shift component), the HAC clustering would produce a genuine ultrametric tree. The relationship between this ultrametric and the categorical structure of natural transformation equivalence is unexplored.
