# Girard's Linear Logic in Deferential Realism

## A Genuine Architectural Gap

**Date:** 2026-02-14
**Prerequisites:** *Categorical Architecture* (Lawvere document), *Grothendieck Framing*, *Noether Implementation*
**Status:** Identifies a genuine architectural gap — the codebase computes resource-like quantities but does not enforce resource discipline. Gemini's five claimed isomorphisms are all LOOSE. Linear Logic is Not Yet Implemented.

---

## 1. Linear Logic in Brief

Jean-Yves Girard's Linear Logic (1987) is a refinement of classical logic that tracks *resource consumption*. In classical logic, a proposition can be used any number of times — duplicated (contraction) or ignored (weakening) freely. Linear logic prohibits both: every assumption must be used exactly once.

The key connectives:

- **⊗ (Tensor / Multiplicative Conjunction):** A ⊗ B means "I have both A and B simultaneously." Using A and B together in a single proof step consumes both.
- **⅋ (Par / Multiplicative Disjunction):** The dual of tensor. A ⅋ B means "A and B are available but entangled — you get both whether you want them or not."
- **& (With / Additive Conjunction):** A & B means "I can choose A or I can choose B, but not both." An internal choice that consumes the resource of choosing.
- **⊕ (Plus / Additive Disjunction):** A ⊕ B means "I will receive either A or B, but I don't control which." An external choice.
- **⊸ (Linear Implication / Lollipop):** A ⊸ B means "consuming A produces B." The premises are consumed by the proof — you can't use A again after deriving B.
- **! (Of Course / Bang):** !A marks A as a reusable resource. !A can be duplicated and discarded freely — it reintroduces the classical structural rules for this specific resource.
- **? (Why Not):** The dual of !. ?A can be weakened (discarded) and contracted (duplicated). It models resources that can spread or be ignored.

The core discipline: without ! or ?, every resource must be used exactly once. A proof that fails to consume a premise is invalid. A proof that uses a premise twice is invalid. This resource accounting catches errors that classical logic misses — in particular, it distinguishes between "I have access to X" and "I can use X once."

The practical question for DR: does the codebase enforce this discipline for any of its "resources" (purity, energy, enforcement capacity, agent attention)? Or does it compute resource-like quantities without enforcing linear usage?

---

## 2. Gemini Mapping Validation

### 2.1 Mapping 1: `purity_adjusted_energy/4` as ⊗ (Tensor)

**Gemini's claim:** "This is a direct map to the Multiplicative Conjunction (Tensor ⊗). An action can be modeled as: AgencyEnergy ⊗ PurityState ⊸ NewPurityState."

**Predicate:** `purity_adjusted_energy/4` at `drl_modal_logic.pl:1196`

**What the code actually does:**

```prolog
purity_adjusted_energy(C, Context, BaseAction, energy_cost(BaseCost, Mult, Adjusted)) :-
    constraint_indexing:valid_context(Context),
    base_action_complexity(BaseAction, BaseCost),       % LOOKUP: table of fixed costs
    structural_signatures:purity_score(C, Purity),      % READ: purity from cache
    config:param(purity_energy_max_multiplier, MaxMult), % READ: config constant
    energy_multiplier(BaseAction, Purity, MaxMult, Mult),% COMPUTE: multiplier
    Adjusted is BaseCost * Mult.                         % ARITHMETIC: return number
```

This predicate is a **pure function**: it reads metrics, performs arithmetic, and returns a structure `energy_cost(BaseCost, Mult, Adjusted)`. It has no side effects. It does not modify any state. It does not decrement any budget. It does not track any running balance.

**The critical absence:** No predicate in the codebase consumes the returned energy cost. There is no `agency_budget(Agent, Budget)` that gets decremented by `Adjusted`. There is no check `Budget >= Adjusted` before an action is permitted. The energy cost is computed and returned — then nothing happens to it.

**Verdict: LOOSE.** This is resource *calculation*, not resource *consumption*. The tensor product A ⊗ B requires that both A and B are consumed by the proof. Here, purity is READ (not consumed) and energy is COMPUTED (not spent). The predicate is a morphism in the category of real-valued functions on the constraint space — specifically, a scalar multiplication of `BaseCost` by a purity-dependent multiplier. In Lawvere terms, it's an endomorphism on the action algebra. No tensor product is involved.

**What it actually is (Lawvere):** A scalar multiplication `E: Action × Constraint → ℝ` in the presheaf of action costs. Classical, functional, no resource semantics.

---

### 2.2 Mapping 2: `cross_index_coupling/2` as Proof Net Correctness

**Gemini's claim:** "This is precisely the function of a Proof Net's correctness criterion. A valid Proof Net is a geometric structure that guarantees a proof is free of circular reasoning and that all resources (axioms) are consumed exactly once."

**Predicate:** `cross_index_coupling/2` at `structural_signatures.pl:891`

**What the code actually does:**

```prolog
compute_cross_index_coupling(C, CouplingScore) :-
    coupling_test_powers(Powers),              % [powerless, moderate, institutional, analytical]
    coupling_test_scopes(Scopes),              % [local, national, global]
    findall(classified(P, S, Type),
        (member(P, Powers), member(S, Scopes),
         coupling_test_context(P, S, Ctx),
         classify_at_context(C, Ctx, Type)),
        Grid),
    count_coupling_violations(Grid, Powers, Scopes, Violations),
    CouplingScore is min(1.0, Violations / MaxViolations).
```

This predicate classifies a constraint across a 4×3 Power × Scope grid and measures the degree to which classification factorizes: does `type(P, S) ≈ f(P) × g(S)`? A violation occurs when the type changes across scopes for the same power level — indicating that power and scope are entangled in the classification.

The Lawvere document established this as a **naturality square test** [STRICT] (`lawvere_implementation_notes.md:§2.6`). The factorization condition is exactly the commutativity of the classification functor with respect to index-dimension morphisms.

**Is a naturality square test the same as a proof net correctness criterion?** No. They test different properties of different structures:

- **Naturality square:** Tests commutativity of a diagram `F(f) ∘ η_A = η_B ∘ G(f)` for a natural transformation η between functors F and G. This is a well-defined test on the 4×3 grid.
- **Proof net correctness:** Tests that a proof structure (a graph of axiom links and cut links) is (a) connected and (b) acyclic under every switching. This is Danos-Regnier's criterion for MLL proof nets.

These are different tests on different objects. The factorization test checks whether a *function* decomposes as a *product*. Proof net correctness checks whether a *graph* satisfies *switching acyclicity*. The only commonality is that both detect "structural defects" — but that is too vague to constitute an isomorphism.

**Verdict: LOOSE.** `cross_index_coupling/2` is a naturality test [STRICT, per Lawvere]. Calling it a proof net correctness criterion is pattern-matching on the surface ("tests for structural defects") while ignoring the mathematical content (commutativity vs. switching acyclicity). The Lawvere vocabulary is both more precise and more honest.

**What it actually is (Lawvere):** A naturality square test on the classification presheaf over the Power × Scope product site. STRICT categorical correspondence.

---

### 2.3 Mapping 3: Contamination as `?` (Why Not) Modality

**Gemini's claim:** "The spread of contamination is analogous to the 'Why Not?' (?) modality, which governs weakening and contraction. A low-purity constraint could be modeled as ?ContaminatedState, a resource that can be freely copied and spread."

**Predicates:** `effective_purity/4` at `drl_modal_logic.pl:1499`, `purity_contamination_pressure/4` at `drl_modal_logic.pl:1564`

**What the code actually does:**

```prolog
% effective_purity computation (simplified)
RawEff is Intrinsic - TotalContam * Immunity,
EffPurity is max(0.0, RawEff)

% contamination from one neighbor
Delta is max(0.0, MyPurity - OtherPurity),     % purity gradient
Attenuation is EdgeStrength * AttFactor,        % edge-weighted
RawContam is Delta * Attenuation * TypeFactor,  % scaled by type
Contam is min(Cap, RawContam)                   % capped per edge
```

Contamination is a **weighted influence propagation** mechanism. When a high-purity constraint neighbors a low-purity constraint, the high-purity constraint's effective purity is reduced by a capped, attenuated function of the purity gradient, edge strength, and neighbor type.

**Does contamination copy anything?** No. The `?` modality specifically enables **contraction** (duplicating a resource: from one copy of ?A, derive two copies of ?A) and **weakening** (discarding a resource: ignore ?A entirely). Contamination does neither:

- **No contraction:** A low-purity constraint does not *duplicate itself* into its neighbors. It *reduces* the neighbor's effective purity by a scalar amount. The contaminating constraint is unchanged; the contaminated constraint loses purity. This is influence propagation, not resource duplication.
- **No weakening:** A neighbor cannot *discard* the contamination. If the edge exists and the purity gradient is positive, contamination is applied. The only "defense" is type immunity (mountains have immunity = 0.0, meaning they fully absorb contamination without effect), but immunity is a fixed property, not a choice to discard.

The Lawvere document classified this as **contravariant flow** [STRUCTURAL] (`lawvere_implementation_notes.md:§2.8`): contamination moves *against* the purity gradient (from lower purity to higher purity). This is structurally analogous to contravariance but not a formal contravariant functor.

**Verdict: LOOSE.** Contamination is weighted influence propagation along a network, with attenuation, capping, and type-dependent sensitivity. Calling it the `?` modality imposes resource-theoretic vocabulary (copy, discard) on an operation that does neither. The Lawvere vocabulary (contravariant purity flow) and the Noether vocabulary (symmetry-breaking propagation) already describe this correctly.

**What it actually is (Lawvere/Noether):** Contravariant purity propagation [STRUCTURAL] — a monotone endofunctor on the purity lattice whose greatest fixed point is the FPN equilibrium [STRUCTURAL, per Lawvere §3.4].

---

### 2.4 Mapping 4: Tangled Rope as Additive (&) vs Multiplicative (⊗) Conjunction

**Gemini's claim:** "A tangled_rope is a formula like: CoordinationChoice_A & Extraction_B. The system's struggle to classify it reflects the fundamental difference between these two operators."

**Predicate:** `classify_from_metrics/6` at `drl_core.pl:292–305`

**What the code actually does:**

```prolog
classify_from_metrics(C, BaseEps, Chi, Supp, _Context, tangled_rope) :-
    config:param(tangled_rope_chi_floor, ChiFloor),
    config:param(tangled_rope_chi_ceil, ChiCeil),
    Chi >= ChiFloor, Chi =< ChiCeil,              % threshold range
    config:param(tangled_rope_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,                           % base extraction above floor
    config:param(tangled_rope_suppression_floor, MinS),
    Supp >= MinS,                                  % suppression above minimum
    requires_active_enforcement(C),                % boolean property
    narrative_ontology:has_coordination_function(C),% boolean: coordination
    narrative_ontology:has_asymmetric_extraction(C).% boolean: extraction
```

This is a **classical conjunction** of six conditions: three threshold comparisons on continuous metrics, and three boolean property checks. All six must be true for the classification to fire.

**Is there any resource semantics?** No. The `&` (additive conjunction, "with") in linear logic represents an *internal choice*: A & B means "you can use this as A or as B, but not both — choosing A consumes the resource and forecloses B." The `⊗` (tensor, multiplicative conjunction) means "you have both A and B simultaneously, and both are consumed."

The tangled_rope classification is neither. It checks `has_coordination_function(C) AND has_asymmetric_extraction(C)` — both must be true, no choice is involved, nothing is consumed. This is classical propositional conjunction: `P ∧ Q`. The "struggle to classify" tangled_ropes is not a struggle between additive and multiplicative conjunction; it's the fact that the constraint sits in a region of metric space where both coordination and extraction markers are present. The difficulty is *diagnostic* (how to act on a constraint that serves two functions), not *logical* (which conjunction operator applies).

**Verdict: LOOSE.** The tangled_rope classification is classical conjunction in a threshold cascade. Neither the additive nor the multiplicative distinction applies. In Lawvere terms, this is a stalk computation on the presheaf — evaluating the classification functor at a point where multiple threshold conditions fire simultaneously.

**What it actually is (Lawvere):** Stalk computation in the classification presheaf [STRICT]. The "tangled" quality is a property of the metric space region (hybrid zone between rope and snare thresholds), not of any logical connective.

---

### 2.5 Mapping 5: `action_composition_gate/3` as Gated Linear Implication

**Gemini's claim:** "The action_composition_gate adds a necessary premise: SufficientPurity ⊗ SufficientReformability ⊗ ReformAction ⊸ ReformedState."

**Predicate:** `action_composition_gate/3` at `drl_modal_logic.pl:1254`

**What the code actually does:**

```prolog
action_composition_gate(C, surgical_reform, gate(Pass, Reason)) :-
    structural_signatures:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_surgical_reform_gate, MinPurity),
        (   Purity >= MinPurity
        ->  (   reformability_score(C, ReformScore),
                ReformScore > 0.50
            ->  Pass = pass, Reason = purity_and_reformability_sufficient
            ;   Pass = fail, Reason = reformability_too_low
            )
        ;   Pass = fail, Reason = purity_below_surgical_threshold
        )
    ).
```

This is a **classical precondition check**. It reads two metrics (purity and reformability), compares each against a threshold, and returns `gate(pass, Reason)` or `gate(fail, Reason)`. No state is modified. No resource is consumed. The gate can be called repeatedly on the same constraint with identical results.

**Is this a linear implication?** No. A linear implication A ⊸ B requires that A is *consumed* by the derivation of B. After the proof, A is no longer available. Here, purity is READ, not consumed. After the gate check, the constraint's purity is unchanged. You can check the gate again and get the same answer. This is classical material implication `(P ≥ 0.30 ∧ R > 0.50) → permitted(surgical_reform)`, not linear implication.

Gemini's formula `SufficientPurity ⊗ SufficientReformability ⊗ ReformAction ⊸ ReformedState` would require that performing the reform *consumes* the purity and reformability — after the reform, the constraint would have less purity and less reformability. There is no mechanism in the codebase that implements this.

**Verdict: LOOSE.** This is a classical precondition check (boolean guard on a threshold). In Lawvere terms, it's a subobject inclusion: the gate defines a subspace of (constraint, action) pairs where the action is permitted.

**What it actually is (Lawvere):** A subobject filter on the action space — a predicate that restricts the domain of the action functor to constraints meeting purity/reformability conditions.

---

## 3. Genuine Linear Structure in the Codebase

### 3.1 State Mutation Patterns

A thorough search of the codebase reveals that all `retract`/`retractall`/`assertz` patterns fall into three categories:

1. **Cache management.** `clear_classification_cache/0` (`structural_signatures.pl:51`), `trajectory_cleanup/0` (`trajectory_mining.pl:83`), `maxent_cleanup/0` (`maxent_classifier.pl:88`), `fpn_cleanup/1` (`drl_modal_logic.pl`). These clear precomputed results between test runs. The pattern is: `retractall(cache(...))` at the start of a computation, `assertz(cache(...))` to store results, repeat. This is memoization, not resource consumption.

2. **Precomputation.** MaxEnt profiles, trajectory distances, FPN equilibrium purities. Facts are asserted once during a computation phase and read multiple times during reporting. The pattern is write-once, read-many — the opposite of linear (use-once).

3. **Data repair.** `data_repair.pl` extends the ontology by asserting inferred facts. Facts are added, never consumed.

**No predicate models resource depletion.** There is no pattern of `assert(resource(N))` → `retract(resource(N))` → `assert(resource(N-1))`. There is no "balance tracking" anywhere in the codebase.

### 3.2 Exactly-Once Enforcement

**Not present.** No predicate in the codebase enforces that a resource is used exactly once. Prolog's `once/1` meta-predicate appears but serves determinism (commit to first solution), not linear resource discipline. Cuts (`!`) serve determinism in the classification cascade. Neither has resource-tracking semantics.

### 3.3 Finite Resource Tracking

**Not present.** No predicate tracks a finite quantity that decreases when used. The candidates suggested by the task description:

- **Agent attention:** Not tracked. An agent can "enforce" any number of constraints simultaneously — there is no attention budget.
- **Trust:** Not a tracked quantity. Trust is not modeled in the codebase.
- **Enforcement capacity:** Not tracked as a finite resource. `requires_active_enforcement/1` is a boolean property — either a constraint requires enforcement or it doesn't. There is no "enforcement budget" that gets split across constraints.
- **Exit options:** Modeled as a categorical variable (`trapped`, `mobile`, `arbitrage`, `analytical`), not as a consumable quantity.

### 3.4 Action Predicates

`purity_adjusted_energy/4`, `action_composition_gate/3`, `purity_qualified_action/4`, `purity_scaffold_urgency/4` — all compute recommendations without tracking budgets. The energy costs computed by `purity_adjusted_energy/4` are returned as data structures but never deducted from any balance. The gates check conditions but don't lock resources. The qualified actions recommend behaviors but don't enforce exclusivity.

### 3.5 Drift and Lifecycle Predicates

Drift events (`drl_lifecycle.pl`) describe constraint degradation over time: extraction accumulation, coordination loss, coupling drift, purity drift. These are **observed metric changes**, not resource consumption. The metrics themselves are read from the ontology (historical measurements), not depleted by the observation. Drift detection is read-only analysis of temporal data.

### 3.6 Honest Conclusion

**There is no genuine linear structure in the codebase.** The system is entirely classical: predicates compute values from inputs, check conditions, and return results without consuming any resource. Every "resource-like" quantity (purity, energy, reformability, coupling) is a *computed metric* — derived from constraint properties, readable without side effects, stable across multiple queries.

This is not a criticism. Classical logic is the correct foundation for a diagnostic/classification system. The system's job is to *analyze* constraints and *recommend* actions, not to *execute* actions or *manage* resources. Resource discipline would be appropriate for an operational layer that actually reforms constraints, allocates enforcement capacity, or manages agent budgets. The current system is an analytical layer for which classical logic suffices.

---

## 4. What a Genuine Girard Layer Would Require

If linear logic were to add analytical power to the DR system, it would need to close the gap between *computing costs* and *enforcing budgets*. Here is what that would look like.

### 4.1 Ontological Expansion

Constraints would need new annotations:

```prolog
%% New ontological facts required:
agency_budget(Agent, Context, Budget).          % Finite resource per agent-context
enforcement_capacity(Agent, MaxConstraints).    % How many constraints an agent can enforce
reform_allocation(Agent, RemainingReforms).     % How many reforms an agent can attempt
attention_slots(Agent, Available, Total).        % Finite attention slots
```

These do not currently exist. The system has no concept of agent capacity limits.

### 4.2 New Predicates

```prolog
%% Linear resource tracking:
linear_action(C, Agent, Action, PreBudget, PostBudget) :-
    purity_adjusted_energy(C, Context, Action, energy_cost(_, _, Cost)),
    agency_budget(Agent, Context, PreBudget),
    PreBudget >= Cost,
    PostBudget is PreBudget - Cost,
    retract(agency_budget(Agent, Context, PreBudget)),
    assertz(agency_budget(Agent, Context, PostBudget)).

%% Exclusive allocation:
allocate_enforcement(Agent, C) :-
    attention_slots(Agent, Available, Total),
    Available > 0,
    NewAvailable is Available - 1,
    retract(attention_slots(Agent, Available, Total)),
    assertz(attention_slots(Agent, NewAvailable, Total)),
    assertz(enforcing(Agent, C)).

%% Linear implication: consuming a reform opportunity
consume_reform(Agent, C, Result) :-
    reform_allocation(Agent, N),
    N > 0,
    NewN is N - 1,
    retract(reform_allocation(Agent, N)),
    assertz(reform_allocation(Agent, NewN)),
    execute_reform(C, Result).
```

### 4.3 Modifications to Existing Predicates

- `action_composition_gate/3` would gain a budget parameter: not just "is this action permitted?" but "is this action permitted *given remaining resources*?"
- `purity_adjusted_energy/4` would return costs that are actually deducted from something.
- `coupling_aware_scaffold_need/3` would consider whether the agent has sufficient budget to provide the scaffold.
- `simulate_cut/3` would account for the cost of cutting and whether alternatives are affordable.

### 4.4 Questions the Linear Layer Would Answer

1. **"Can this agent afford this reform?"** — Currently unanswerable. The system computes reform costs but has no concept of affordability.
2. **"If we reform constraint A, can we still afford to reform constraint B?"** — Currently unanswerable. There is no resource exclusion — reforming A has no effect on the feasibility of reforming B.
3. **"What is the optimal allocation of limited enforcement capacity across constraints?"** — Currently unanswerable. The system doesn't model enforcement capacity as finite.
4. **"Which reforms should be prioritized given a finite budget?"** — Currently answerable only by cost ranking (sort by purity_adjusted_energy), not by budget-constrained optimization.
5. **"Does the agent have enough total energy to address all identified problems?"** — Currently unanswerable. There is no "total energy" concept.

### 4.5 Estimated Implementation Cost

- **Ontology expansion:** ~20 new facts per dataset (agency_budget, enforcement_capacity, reform_allocation, attention_slots per agent). Requires extending the data specification.
- **New predicates:** ~120–150 lines for linear resource tracking, exclusive allocation, budget-constrained action selection.
- **Modifications to existing predicates:** ~40–60 lines to thread budget parameters through `action_composition_gate/3`, `purity_scaffold_urgency/4`, and the action algebra.
- **Budget-constrained optimization:** ~80–100 lines for a greedy or knapsack-style reform prioritizer that operates under resource constraints.
- **Total:** ~260–330 lines of new code, plus ontology expansion in every dataset.

This is substantially more than Galois (~80 lines of trivially derivable computation) and on par with a new subsystem like the MaxEnt classifier or abductive engine. The ontology expansion is the more significant cost — every dataset would need agent resource data.

---

## 5. Where Girard Sits in the Framework Hierarchy

### 5.1 The Three-Tier Classification

The Wheeler document established three tiers:

- **Tier 1 (Formal Structure — New Mathematics):** Lawvere (presheaf topos), Grothendieck (cohomology, descent), Noether (symmetry conservation), Galois (coalition-consensus connection), Dirac (gauge orbits).
- **Tier 2 (Implemented Algorithms):** Jaynes (MaxEnt shadow classifier), Peirce (abductive engine).
- **Tier 3 (Interpretive Layer):** Wheeler ("It from Bit" — tells you what the numbers *mean*, not what they *are*).

### 5.2 Girard's Position: Not Yet Implemented

Girard does not fit neatly into any tier because the structure it describes **does not exist in the codebase.**

- It is not Tier 1 (formal structure already present). Unlike Noether, where symmetry detection was already implemented and merely needed recognition, the resource-tracking infrastructure that linear logic formalizes is absent.
- It is not Tier 2 (algorithm already running). Unlike Jaynes, where the MaxEnt classifier already computes distributions, there is no resource-accounting algorithm running.
- It is not Tier 3 (interpretive layer). Unlike Wheeler, where existing numbers gain philosophical meaning, there are no resource-consumption numbers to interpret.

**Girard is in a fourth category: NOT YET IMPLEMENTED.** It identifies a genuine architectural gap — the system computes costs without enforcing budgets — and provides the formal framework for closing that gap. Whether closing the gap is valuable depends on the system's intended use:

- **If DR remains an analytical/diagnostic tool** (classify constraints, detect anomalies, recommend actions), linear logic adds nothing. The system's job is to analyze, not to act, and analysis doesn't consume resources.
- **If DR becomes an operational/planning tool** (allocate enforcement capacity, prioritize reforms, manage agent budgets), linear logic provides the correct formalism for resource-constrained planning. The gap between "what should be done" and "what can be afforded" is exactly the gap that linear logic closes.

### 5.3 Comparison with Other Frameworks

| Framework | Status | Code Required | Key Finding |
|-----------|--------|---------------|-------------|
| Noether | Fully implemented | 0 lines | Symmetry detection → naturality testing → break classification → health measurement → propagation — all present |
| Galois | Mostly implemented | ~80 lines | Orbit side fully present; coalition side trivially computable; splitting degree is genuinely new |
| Wheeler | Interpretive layer | 0 lines | Three quantitative findings surfaced; no new computation |
| **Girard** | **Not yet implemented** | **~260–330 lines + ontology** | **Genuine architectural gap: costs computed but not enforced. Would require new ontological data and resource-tracking infrastructure** |

Girard is the first framework in this series that identifies something the system genuinely *lacks* rather than something it already *has* under different vocabulary.

---

## 6. Rigor Summary

| # | Gemini Mapping | Predicate | Gemini's Claim | Actual Structure (Lawvere) | Rigor |
|---|---|---|---|---|---|
| 1 | `purity_adjusted_energy/4` as ⊗ | `drl_modal_logic.pl:1196` | Tensor / resource consumption | Scalar multiplication on action algebra | **LOOSE** |
| 2 | `cross_index_coupling/2` as proof net | `structural_signatures.pl:891` | Proof net correctness criterion | Naturality square test [STRICT per Lawvere] | **LOOSE** (as proof net; STRICT as naturality) |
| 3 | Contamination as `?` modality | `drl_modal_logic.pl:1499, 1564` | Why Not — free copying/spreading | Contravariant purity propagation [STRUCTURAL per Lawvere] | **LOOSE** |
| 4 | Tangled rope as & vs ⊗ | `drl_core.pl:292` | Additive vs multiplicative conjunction | Classical conjunction in threshold cascade | **LOOSE** |
| 5 | `action_composition_gate/3` as ⊸ | `drl_modal_logic.pl:1254` | Gated linear implication | Classical precondition (subobject filter) | **LOOSE** |

**All five mappings are LOOSE.** This is the weakest showing of any framework analyzed. Gemini's analysis applied linear logic vocabulary to classical computation throughout. The predicates identified are real and important — but they are already correctly described by the Lawvere categorical vocabulary (naturality tests, presheaf evaluation, contravariant flow, scalar multiplication). Adding linear logic vocabulary does not improve the description; it degrades precision by importing resource-consumption semantics that the code does not implement.

### The Pattern

Gemini's error is systematic: it conflates *computing a quantity that describes a resource* with *consuming that resource*. Computing the energy cost of an action is not the same as spending energy. Computing purity is not the same as consuming purity. Checking a precondition is not the same as consuming a precondition. This conflation is the same error in all five mappings.

The error is understandable — the codebase uses resource-flavored vocabulary (energy, purity, contamination, cost, budget) and the computations do resemble resource accounting. But the resemblance is superficial. The system computes `f(x) = cost` and returns it. It never computes `balance = balance - cost`. The difference is between a calculator and a cash register.

### The Honest Finding

The gap Girard identifies is real. The system has a fully developed cost-computation infrastructure (purity-adjusted energy, action composition gates, reform urgency, scaffold need assessment) but no resource-enforcement infrastructure (budgets, allocations, exclusions, depletion tracking). Whether this gap matters depends on the system's purpose. For diagnostic analysis, classical logic suffices and resource discipline would add complexity without benefit. For operational planning under resource constraints, the gap is a genuine limitation, and Girard's linear logic is the correct formalism for closing it.
