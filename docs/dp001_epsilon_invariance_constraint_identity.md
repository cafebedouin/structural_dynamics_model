# Design Principle DP-001: ε-Invariance and Constraint Identity

**Status:** Adopted  
**Date:** 2026-02-11  
**Supersedes:** Rejected proposal for `measurement_basis` as 5th context axis  
**Scope:** Constraint story authoring, context tuple specification, classification pipeline  

---

## Statement of Principle

The base extractiveness ε of a constraint story is an intrinsic property of the constraint, not an observer-relative quantity. If changing the observable used to evaluate a constraint changes ε, the observer is not looking at the same constraint from a different angle — they are looking at a different constraint.

Constraint identity in Deferential Realism requires ε-invariance. Two analyses describe the same constraint if and only if they agree on ε within noise tolerance. When they disagree, the framework models them as distinct constraint stories linked by `affects_constraint/2`, not as a single story parameterized by measurement basis.

---

## The Context Tuple Is Closed at Arity 4

The indexical context tuple is defined as:

```
I ::= (P, T, E, S)
```

where P is agent_power, T is time_horizon, E is exit_options, and S is spatial_scope. These four axes capture the observer's structural position relative to a constraint. The tuple is closed: no additional axes may be added without a formal revision of this principle.

The closure is not arbitrary. It follows from the classification function:

```
χ = f(d) × ε × σ(S)
```

where f(d) is the power-scaling function over directionality d (derived from P, T, E) and σ(S) is the theater ratio modulated by spatial scope. Every variable that determines classification is accounted for: ε is constraint-intrinsic, d is derived from the first three context axes, and σ(S) is derived from the fourth. There is no free parameter remaining for a fifth axis to influence.

This means: if ε is fixed and (P, T, E, S) are fixed, χ is determined, and classification is determined. No additional contextual variable can change the outcome. A proposed fifth axis that cannot change classification is inert, and inert parameters do not belong in the tuple.

---

## Architectural Proof

The rejected proposal argued for a `measurement_basis` axis: a fifth context element capturing which observable an agent uses to evaluate a constraint (e.g., eigenvalue statistics vs. eigenvector statistics in quantum chaos, aggregate statistics vs. individual trajectories in epidemiology, statutory text vs. enforcement practice in law).

Seven independent model analyses converged on the same structural refutation:

**Premise.** Suppose two observers A and B share identical (P, T, E, S) and evaluate the same constraint C, but use different observables. The measurement_basis proposal claims their classifications may differ.

**Case 1: ε differs between A and B.** Then A and B are evaluating constraints with different base extractiveness. Under ε-invariance, these are different constraints. The correct response is to decompose C into C_A and C_B with their respective ε values and link them via `affects_constraint/2`. No fifth axis is needed — the framework already has machinery for constraint networks.

**Case 2: ε is identical for A and B.** Then χ_A = f(d) × ε × σ(S) = χ_B, since d and S are also identical by assumption. Classification is a function of χ alone. Therefore classification_A = classification_B. The measurement basis produced no divergence. The axis is inert.

**Conclusion.** In Case 1, the axis is unnecessary (network decomposition handles it). In Case 2, the axis is powerless (classification cannot differ). There is no case where measurement_basis is both necessary and sufficient. The axis is a category error: it attempts to encode constraint identity differences as contextual variation.

---

## The ε-Invariance Test

When confronted with a constraint that appears to have observable-dependent classification, apply this test:

**Step 1.** Fix (P, T, E, S) across the two observables.

**Step 2.** Compute ε independently for each observable using the standard pipeline.

**Step 3.** Compare ε values.

- If the relative difference exceeds 1% or the absolute difference exceeds a domain-appropriate pre-registered δ: the observables are measuring different structural phenomena. **Decompose** into separate constraint stories. Link them with `affects_constraint/2`. Each story gets its own ε, its own classification, and its own perspectives. This is not proliferation — it is disambiguation of a conflated natural-language label.

- If ε values are equal within tolerance: classification is determined and cannot differ. If you believe classification should differ, re-examine whether ε was correctly estimated. The framework's job is to classify constraints accurately given their structural properties, not to preserve intuitions about how different measurement methodologies "should" produce different answers.

**There is no Step 4.** The test is exhaustive. Every case of apparent observable dependence resolves into one of these two branches.

---

## Worked Example: The BGS Conjecture

Physicists refer to "the BGS conjecture" as a single claim: quantum systems with chaotic classical limits exhibit universal statistical properties. But this label conflates two structurally distinct claims with different empirical status.

**Claim 1: Spectral universality.** Eigenvalue level spacings follow Random Matrix Theory predictions (GOE/GUE/GSE). This has been verified across Sinai billiards, hydrogen in strong magnetic fields, microwave cavities, nuclear resonance data, and quantum graphs for over 40 years. No clean counterexample exists.

- ε = 0.08. The constraint imposes almost no extractive burden on researchers. It is a robust empirical regularity.
- Classification: Mountain from all perspectives. This is a natural law.
- File: `constraint_bgs_spectral_universality.pl`

**Claim 2: Eigenvector thermalization.** Individual eigenstates look thermal (ETH compliance), out-of-time-order correlators show Lyapunov growth, and eigenbasis chaos is universal. This is contested. Counterexamples include Magan & Wu's Poissonian Hamiltonian ensembles, quantum kicked-top at critical coupling, and quantum many-body scars in Rydberg atom chains.

- ε = 0.42. Researchers who assume ETH universality face meaningful risk of being misled. The ETH mainstream exerts peer-review pressure against publishing non-thermal results.
- Classification: Tangled Rope (genuine coordination function in organizing quantum chaos research, plus asymmetric extraction from researchers whose systems violate the assumption).
- File: `constraint_bgs_eigenvector_thermalization.pl`

**Network structure:**

```prolog
affects_constraint(ehrenfest_barrier, bgs_spectral_universality).
affects_constraint(ehrenfest_barrier, bgs_eigenvector_thermalization).
affects_constraint(bgs_spectral_universality, bgs_eigenvector_thermalization).
```

The third link captures a real inferential dependency: spectral universality is routinely cited as evidence for eigenvector thermalization, creating downstream pressure on the second claim even though the two are logically independent.

Under the rejected measurement_basis proposal, these would have been a single constraint story with `measurement_basis(spectral)` and `measurement_basis(eigenvector)` as context modifiers. But their ε values differ by a factor of five. They are not the same mountain viewed from two angles. They are two different geological formations that happen to share a colloquial name.

---

## Constraint Identity Criteria

A constraint story in Deferential Realism has a stable identity if and only if:

1. **ε is invariant** across all legitimate methods of empirical evaluation.
2. **The constraint claim is falsifiable** — there exists an observation that could change ε.
3. **The beneficiary/victim structure is consistent** across evaluations (if applicable).

When a natural-language label covers multiple phenomena that fail criterion (1), the label is an index into a constraint family, not a constraint. Decompose into stories that individually satisfy all three criteria.

This means the framework's unit of analysis is the **stable-ε story**, not the natural-language concept. "The BGS conjecture" is a family. "Spectral universality" and "eigenvector thermalization" are constraints. "Quantum measurement" is a family. Its members (Copenhagen collapse, decoherent branching, Bohmian guidance, etc.) are constraints — if and only if they produce distinct ε values when applied to the same physical system.

---

## Proliferation Containment

Network decomposition creates a risk: if every theoretical variant becomes its own constraint story, the namespace may grow without bound. This risk is real but bounded by the ε-distinctness requirement.

**When decomposition is legitimate:** Two formulations are distinct constraints if they produce different ε values, are maintained by different research communities, have different canonical formalizations, and would be falsified by different observations. The eigenvalue/eigenvector split in BGS satisfies all four. So would Copenhagen vs. Many-Worlds if applied to a concrete system where their predictions about extractiveness diverge.

**When decomposition is illegitimate:** Two formulations that share ε, are used by the same community, differ only in vocabulary, and would be falsified by the same evidence are not distinct constraints. They are redundant stories that should be collapsed. Creating separate files for them is not disambiguation — it is namespace pollution.

**Standing rules for constraint families:**

1. Require `affects_constraint/2` links between all stories in a family. Orphan stories with no network connections are a code smell.
2. When creating a new story that claims kinship with an existing constraint, document how the ε values differ and why.
3. Periodically audit families for convergent ε values. If two stories' ε values have converged (due to new evidence, refined measurement, or better calibration), evaluate whether they should be merged.
4. The linter enforces context/4 arity (Rule 23). Any attempt to smuggle family-membership information into the context tuple will be caught and rejected.

---

## Anti-Patterns

**Do not extend context arity.** The context tuple is `context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))` with exactly four arguments. Linter Rule 23 enforces this. Historical violations in 11 testset files embedded `constraint_beneficiary` and `constraint_victim` inside context tuples, reaching arity 6. All have been corrected.

**Do not create measurement_basis modifiers.** No `measurement_basis(spectral)`, `measurement_basis(statistical)`, or equivalent terms belong in context, in `constraint_meta`, or in any predicate that modifies classification. Observable selection is handled by constraint decomposition, not parameterization.

**Do not treat ε as observer-relative.** If you find yourself writing `ε(Constraint, Basis1, Value1)` and `ε(Constraint, Basis2, Value2)` with Value1 ≠ Value2, you do not have one constraint with basis-dependent ε. You have two constraints. Decompose them.

**Do not use `constraint_meta` as a substitute for decomposition.** Metadata annotations like `constraint_meta(bgs_conjecture, dual_formulation)` describe relationships between constraints. They do not change classification. When two formulations produce different classifications, they need separate files with separate ε values, not metadata tags on a shared file.

**Do not use V(M) visibility functions.** A proposed alternative modeled measurement basis as a visibility filter: χ = f(d) × (ε × V(M)) × σ(S), where V(M) ≤ 1 represents how much of the "true" ε a given observable can detect. This collapses into ε(M) = ε × V(M), which is functionally a different ε per basis — the same situation that decomposition already handles. If V(M) < 1 for some basis, that basis is underestimating ε, which is measurement error, not measurement basis. The framework should encode the correct ε.

---

## Open Questions

This principle resolves the measurement_basis debate but leaves two questions for future work:

**1. Is ε ever genuinely epistemic?** The principle treats ε as ontological — a structural property of the constraint independent of who measures it. If a case is found where ε is irreducibly observer-dependent (not due to measurement error or conflated constraints), the principle would need revision. No such case has been identified despite adversarial search across physics, law, economics, and institutional theory. The architectural proof suggests it may be impossible under the current χ formula, but this has not been proven across all conceivable constraint domains.

**2. Does decomposition scale to deep theoretical disagreements?** The BGS split into two stories is tractable. Quantum measurement interpretations might produce five to ten stories. If a domain generates twenty or more stories from a single natural-language concept, the namespace cost may exceed the representational benefit. The ε-distinctness requirement provides a natural pruning criterion, but its effectiveness at scale has not been empirically validated. The standing rules above are designed to contain this risk, but they are policy, not proof.

---

## Revision History

| Date | Change |
|---|---|
| 2026-02-11 | Initial adoption. Rejected measurement_basis axis. Established ε-invariance as constraint identity criterion. Implemented BGS decomposition, context/4 enforcement (Rule 23), and 11 testset arity corrections. |
