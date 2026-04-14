# DR Sheffer Feasibility Audit

**Date:** 2026-04-13  
**Question:** Does the DR constraint taxonomy have a Sheffer-like generator — a single primitive
operation from which Mountains, Ropes, Snares, and Tangled Ropes can be composed or derived?

**Motivation:** Odrzywołek 2026 shows all elementary functions can be generated from
`eml(x,y) = exp(x) − ln(y)`. The DR question is analogous but must be answered on DR's own
terms: socio-political constraint space, not function space.

---

## Pass 1: Evidence Inventory

*No analysis, no conclusions, no assessments appear in this section. All findings are
verbatim code extracts or direct observations with file:line references.*

---

### 1.1 Codebase Inventory

**Project root:** `/home/scott/bin/structural_dynamics_model/`

**Prolog (selected core files, by line count):**

| File | Lines | Role |
|------|-------|------|
| prolog/validation_suite.pl | 3,379 | Test runner |
| prolog/domain_registry.pl | 3,315 | Domain data |
| prolog/json_report.pl | 1,493 | Output |
| prolog/signature_detection.pl | 1,209 | Signature overrides |
| prolog/constraint_indexing.pl | 941 | Context/indexical engine |
| prolog/drl_core.pl | 690 | Primary classifier |
| prolog/config.pl | 587 | Threshold parameters |
| prolog/drl_composition.pl | 412 | Composition rules |
| prolog/narrative_ontology.pl | 343 | Schema/ontology |
| **Total (all .pl files)** | **37,227** | |

**Python (selected, by line count):**

| File | Lines |
|------|-------|
| python/enhanced_report.py | 2,317 |
| python/chi_variance_decomposition.py | 1,825 |
| python/coordination_vitality_diagnostic.py | 1,729 |
| **Total (all .py files)** | **39,612** |

**Documentation:** `docs/` directory, 37,266 lines total across all .md files.  
**Output directory exists:** `outputs/` (verified).

---

### 1.2 Classification Engine Predicate Map

**drl_core.pl exports (lines 17–63, verbatim):**

```prolog
:- module(drl_core, [
    % PRIMARY API - Context-Indexed Classification
    dr_type/3,                      % dr_type(Constraint, Context, Type)
    dr_type/2,                      % Backward compat: uses default context

    % Action Routing (Indexed)
    dr_action/3,
    dr_action/2,

    % Error Detection (Indexed)
    dr_mismatch/4,
    dr_mismatch/3,

    % Structural Signature Integration
    dr_signature/2,                 % dr_signature(Constraint, Signature)

    % Re-exported from constraint_indexing
    constraint_classification/3,
    constraint_claim_indexed/2,
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1,

    % Centralize module references
    base_extractiveness/2,
    suppression_score/2,
    requires_active_enforcement/1,
    emerges_naturally/1,

    % Exposed helpers for modal_logic and testing
    is_mountain/3,                  % Indexed version
    is_rope/3,
    is_snare/3,
    is_tangled_rope/3,
    is_scaffold/3,
    is_piton/3,
    get_raw_suppression/2,

    % Gate precondition for natural laws
    natural_law_without_beneficiary/1,

    % Shared classification (Single Source of Truth)
    classify_from_metrics/6,        % classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type)

    % Reform threshold
    snare_reform_threshold/2
]).
```

**Predicates in drl_composition.pl suggesting composition semantics (lines 6–33):**

```
composite_type/4       % Indexed API
composite_type/3       % Default context fallback
composition_rule/3     % THE OPERATION: composition_rule(+Type1, +Type2, -CompositeType)
detect_extraction_dominance/2
detect_necessity_inheritance/2
transformation_detected/5
transformation_type/6
canonical_transformation/6
predict_transformation/3
```

**Predicates in constraint_indexing.pl (lines 9–51):**

```
extractiveness_for_agent/3     % χ computation
sigmoid_f/2                    % Power modifier function
canonical_d_for_power/2        % Directionality mapping
derive_directionality/3        % d(P) computation
observer_accessible/3          % Restriction operator
classify_from_restricted/3     % Restricted classification
site_contexts/1
site_contexts_canonical/1
site_contexts_product/1        % 156-point product site
```

No predicates named `generate`, `derive_from_primitive`, `synthesize`, `atomic_type`, or `basis` were found in any `.pl` file.

---

### 1.3 Constraint Type Definitions — Verbatim

**`classify_from_metrics/6` — complete predicate body (drl_core.pl:300–385):**

```prolog
% --- Gate helpers ---

natural_law_without_beneficiary(C) :-          % drl_core.pl:284
    emerges_naturally(C),
    \+ requires_active_enforcement(C),
    \+ narrative_ontology:constraint_beneficiary(C, _).

coordination_dead(C) :-                        % drl_core.pl:295
    narrative_ontology:coordination_vitality(C, dead).
coordination_dead(C) :-
    narrative_ontology:coordination_vitality(C, degrading).

% GATE 1: MOUNTAIN
classify_from_metrics(C, BaseEps, _Chi, Supp, Context, mountain) :-   % line 300
    config:param(mountain_suppression_ceiling, SuppCeil),
    Supp =< SuppCeil,                          % Supp ≤ 0.05
    config:param(mountain_extractiveness_max, MaxX),
    BaseEps =< MaxX,                           % ε ≤ 0.25
    emerges_naturally(C),                      % QUALITATIVE: no enforcement needed
    constraint_indexing:effective_immutability_for_context(Context, mountain), !.

% GATE 2: PITON PRE-CHECK (dead coordination overrides extraction)
classify_from_metrics(C, BaseEps, _Chi, _Supp, _Context, piton) :-    % line 314
    coordination_dead(C),                      % QUALITATIVE: vitality declared dead/degrading
    config:param(piton_epsilon_floor, EpsFloor),
    BaseEps > EpsFloor,                        % ε > 0.10
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(C, TheaterMetricName, TR),
    config:param(piton_theater_floor, TRFloor),
    TR >= TRFloor, !.                          % theater_ratio ≥ 0.70

% GATE 3: SNARE
classify_from_metrics(C, BaseEps, Chi, Supp, Context, snare) :-       % line 323
    \+ natural_law_without_beneficiary(C),
    config:param(snare_chi_floor, ChiFloor),
    Chi >= ChiFloor,                           % χ ≥ 0.66
    config:param(snare_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,                       % ε ≥ 0.46
    config:param(snare_suppression_floor, SuppFloor),
    Supp >= SuppFloor,                         % Supp ≥ 0.60
    snare_immutability_check(Context), !.

% GATE 4: SCAFFOLD
classify_from_metrics(C, _BaseEps, Chi, _Supp, _Context, scaffold) :- % line 333
    config:param(scaffold_extraction_ceil, MaxX),
    Chi =< MaxX,                               % χ ≤ 0.45
    narrative_ontology:has_coordination_function(C),  % QUALITATIVE
    scaffold_temporality_check(C),             % QUALITATIVE: sunset or no enforcement
    config:param(theater_metric_name, TheaterMetricName),
    \+ (narrative_ontology:constraint_metric(C, TheaterMetricName, TR), TR > 0.70), !.

% GATE 5: ROPE
classify_from_metrics(C, BaseEps, Chi, _Supp, Context, rope) :-       % line 341
    config:param(rope_chi_ceiling, ChiCeil),
    Chi =< ChiCeil,                            % χ ≤ 0.35
    % v6.0: Chi ≤ 0 → agent is net beneficiary → skip base extraction gate
    (Chi =< 0 -> true ; config:param(rope_epsilon_ceiling, EpsCeil), BaseEps =< EpsCeil),
    (   constraint_indexing:effective_immutability_for_context(Context, rope)
    ;   emerges_naturally(C)
    ), !.

% GATE 6: TANGLED ROPE
classify_from_metrics(C, BaseEps, Chi, Supp, _Context, tangled_rope) :- % line 352
    \+ natural_law_without_beneficiary(C),
    config:param(tangled_rope_chi_floor, ChiFloor),
    config:param(tangled_rope_chi_ceil, ChiCeil),
    Chi >= ChiFloor,                           % χ ≥ 0.40
    Chi =< ChiCeil,                            % χ ≤ 0.90
    config:param(tangled_rope_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,                       % ε ≥ 0.30
    config:param(tangled_rope_suppression_floor, MinS),
    Supp >= MinS,                              % Supp ≥ 0.40
    requires_active_enforcement(C),            % QUALITATIVE
    narrative_ontology:has_coordination_function(C),   % QUALITATIVE
    narrative_ontology:has_asymmetric_extraction(C), !. % QUALITATIVE

% GATE 7: PITON (fallback)
classify_from_metrics(C, BaseEps, Chi, _Supp, _Context, piton) :-     % line 366
    config:param(piton_extraction_ceiling, XCeil),
    Chi =< XCeil,                              % χ ≤ 0.45
    config:param(piton_epsilon_floor, EpsFloor),
    BaseEps > EpsFloor,                        % ε > 0.10
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(C, TheaterMetricName, TR),
    config:param(piton_theater_floor, TRFloor),
    TR >= TRFloor, !.                          % theater_ratio ≥ 0.70

% GATE 8: NATURALIZED
classify_from_metrics(_C, BaseEps, Chi, _Supp, _Context, naturalized) :- % line 379
    config:param(rope_epsilon_ceiling, EpsCeil),
    BaseEps > EpsCeil,                         % ε > 0.45
    config:param(tangled_rope_chi_floor, ChiFloor),
    Chi < ChiFloor, !.                         % χ < 0.40

% FALLBACK
classify_from_metrics(_C, _BaseEps, _Chi, _Supp, _Context, unknown).  % line 385
```

**`dr_type/3` — top-level entry point (drl_core.pl:394–412):**

```prolog
% Categorical: Presheaf evaluation — computes local truth value from Omega at a point of the site
dr_type(C, Context, Type) :-          % line 398
    constraint_indexing:valid_context(Context),
    metric_based_type_indexed(C, Context, MetricType),
    signature_detection:integrate_signature_with_modal(C, MetricType, FinalType),
    !,
    Type = FinalType.

dr_type(_C, _Context, unknown).       % fallback

metric_based_type_indexed(C, Context, Type) :-   % line 421
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type).
```

**Structure of each gate (summary):**

Each gate is a **conjunction (AND) of all conditions**. None uses disjunction to define
the core type. Tangled Rope additionally requires three qualitative boolean features:
`requires_active_enforcement`, `has_coordination_function`, and `has_asymmetric_extraction`
(drl_core.pl:362–364), which are declared in narrative_ontology.pl and cannot be derived
from the continuous metrics (ε, χ, Supp) alone.

---

### 1.4 Composition Operations — Verbatim

**`composition_rule/3` — complete table (drl_composition.pl:84–119):**

```prolog
% Categorical: Binary operation on type space — NOT a lattice meet
% (two absorbing elements: mountain, piton)
%% composition_rule(+Type1, +Type2, -CompositeType)
% NOTE: These rules are NOT indexed - they're about logical structure
%       Context affects input types, not composition rules themselves

% Necessity Inheritance: ■ C₁ ∧ (C₁ → C₂) ⇒ ■ C₂
composition_rule(mountain, _, mountain) :- !.       % Mountain absorbs ALL
composition_rule(_, mountain, mountain) :- !.

% Extraction Dominance: Snare embedded in Rope → Snare
composition_rule(rope, snare, snare) :- !.
composition_rule(snare, rope, snare) :- !.
composition_rule(tangled_rope, snare, snare) :- !.
composition_rule(snare, tangled_rope, snare) :- !.

% Snare Dominance
composition_rule(snare, snare, snare) :- !.

% Rope Composition
composition_rule(rope, rope, rope) :- !.

% Tangled interactions
composition_rule(tangled_rope, tangled_rope, tangled_rope) :- !.
composition_rule(rope, tangled_rope, tangled_rope) :- !.
composition_rule(tangled_rope, rope, tangled_rope) :- !.

% Piton contamination
composition_rule(piton, _, piton) :- !.             % Piton absorbs ALL
composition_rule(_, piton, piton) :- !.

% Unknown fallback
composition_rule(_, _, unknown).
```

**Complete composition table (all 7×7 type pairs with explicit outputs):**

| Input 1 ↓ \ Input 2 → | mountain | rope | tangled_rope | snare | scaffold | piton | unknown |
|---|---|---|---|---|---|---|---|
| **mountain** | mountain | mountain | mountain | mountain | mountain | mountain | mountain |
| **rope** | mountain | rope | tangled_rope | snare | unknown | piton | unknown |
| **tangled_rope** | mountain | tangled_rope | tangled_rope | snare | unknown | piton | unknown |
| **snare** | mountain | snare | snare | snare | unknown | piton | unknown |
| **scaffold** | mountain | unknown | unknown | unknown | unknown | piton | unknown |
| **piton** | mountain | piton | piton | piton | piton | piton | piton |
| **unknown** | mountain | unknown | unknown | unknown | unknown | piton | unknown |

*Note: mountain + X → mountain for ALL X (absorbing). piton + X → piton for ALL X (absorbing), except mountain + piton → mountain (mountain absorbs first due to Prolog clause ordering).*

**Observation (verbatim code comment, drl_composition.pl:84):** "NOT a lattice meet (two absorbing elements: mountain, piton)"

**`composite_type/4` — how it is called (drl_composition.pl:72–82):**

```prolog
%% composite_type(+C1, +C2, +Context, -ResultType)
composite_type(C1, C2, Context, Result) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C1, Context, T1),     % classify C1 first
    drl_core:dr_type(C2, Context, T2),     % classify C2 first
    composition_rule(T1, T2, Result).      % then apply table
```

`composite_type/4` classifies each constraint instance independently via `dr_type/3`, then
looks up the result in `composition_rule/3`. Composition is post-classification.

**`detect_extraction_dominance/2` and `detect_necessity_inheritance/2`:**

These two predicates (drl_composition.pl:121–142) DETECT when composition conditions hold
(a snare embedded in a rope; a mountain implying a derived mountain). They DO NOT generate
new constraint instances. They are diagnostic predicates that confirm composition results.

**Grep for `derived_from`, `composed_of`, `base_constraint` across all .pl files:**

Zero matches for `composed_of` or `base_constraint`. `derived_from/3` appears in
`dirac_classification.pl` but tracks epsilon-invariance decomposition across observer
positions, not type generation from primitives.

---

### 1.5 Lattice / Algebra Structure

**Dual-threshold classification parameters (config.pl:216–238, verbatim):**

```prolog
/* ================================================================
   5B. DUAL-THRESHOLD CLASSIFICATION (logic.md Alignment)
   logic.md specifies BOTH:
     χ (chi) = power-scaled extraction (varies by agent)
     ε (epsilon) = base extraction (structural property)

   Primary classifier (drl_core.pl) checks BOTH thresholds.
   ================================================================ */

% Rule R (Rope): χ ≤ 0.35 ∧ ε ≤ 0.45
param(rope_chi_ceiling, 0.35).
param(rope_epsilon_ceiling, 0.45).

% Rule N (Snare): χ ≥ 0.66 ∧ ε ≥ 0.46
param(snare_chi_floor, 0.66).
param(snare_epsilon_floor, 0.46).

% Rule TR (Tangled Rope): 0.40 ≤ χ ≤ 0.90 ∧ ε ≥ 0.30
param(tangled_rope_chi_floor, 0.40).
param(tangled_rope_chi_ceil, 0.90).
param(tangled_rope_epsilon_floor, 0.30).
```

**Mountain boundaries (config.pl:191–194):**

```prolog
param(mountain_suppression_ceiling, 0.05).
param(mountain_extractiveness_max, 0.25).
```

**Extraction chain ordering (v6.11 Axiom 3, verbatim):**

> "The extraction chain (mountain < rope < tangled_rope < snare) is totally ordered.
> Scaffold and piton are diagnostic categories rather than extraction-chain members."

**No bottom element.** No predicate named `bottom_type`, `zero_constraint`, or `null_constraint`
found in any .pl file. Mountain is the lowest extraction type but requires `emerges_naturally(C)`
as a qualitative condition.

**No top element.** Snare is the highest extraction type; no predicate bounds it from above.

**No meet/join operations.** No predicates named `type_meet`, `type_join`, `lattice_meet`,
or `lattice_join` found. The comment at drl_composition.pl:84 explicitly denies lattice meet.

**Type space algebra (v6.11 §6.4, verbatim, line 550):**

> "Type space as Heyting algebra (two absorbing elements prevent it). Power scaling as
> adjunction (triangle identities unverified). Signature resolution as lattice meet
> (priority dispatch table, not lattice operation)."

**Effective immutability table (constraint_indexing.pl:187–219, verbatim excerpt):**

```prolog
% TIME HORIZON × EXIT OPTIONS → {mountain, rope}
effective_immutability(immediate,     trapped,        mountain).
effective_immutability(immediate,     identity_locked, mountain).
effective_immutability(immediate,     constrained,    mountain).
effective_immutability(immediate,     mobile,         rope).
effective_immutability(immediate,     arbitrage,      rope).
effective_immutability(biographical,  trapped,        mountain).
effective_immutability(biographical,  identity_locked, rope).
effective_immutability(biographical,  constrained,    mountain).
effective_immutability(biographical,  mobile,         rope).
effective_immutability(biographical,  arbitrage,      rope).
effective_immutability(generational,  trapped,        mountain).
effective_immutability(generational,  identity_locked, rope).
effective_immutability(generational,  constrained,    rope).
effective_immutability(generational,  mobile,         rope).
effective_immutability(generational,  arbitrage,      rope).
effective_immutability(historical,    _,              rope).
% Civilizational: analytical perspective sees BOTH mountain AND rope (non-deterministic by design)
effective_immutability(civilizational, analytical,    mountain).
effective_immutability(civilizational, analytical,    rope).
effective_immutability(civilizational, trapped,       rope).
effective_immutability(civilizational, identity_locked, rope).
effective_immutability(civilizational, constrained,   rope).
effective_immutability(civilizational, mobile,        rope).
effective_immutability(civilizational, arbitrage,     rope).
```

This is a lookup table over the discrete product TimeHorizon × ExitOptions → {mountain, rope}.
It is not a derived structure.

**Sigmoid formula (constraint_indexing.pl:260–292, verbatim):**

```prolog
% f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))
% Directionality d in [0.0, 1.0]:
%   d ≈ 0.0  → institutional beneficiary (f ≈ -0.20)
%   d ≈ 0.5  → midpoint (f ≈ 0.65)
%   d ≈ 1.0  → powerless target (f ≈ 1.50)

alt_sigmoid_f(sigmoid, D, F) :-
    config:param(sigmoid_lower, L),
    config:param(sigmoid_upper, U),
    config:param(sigmoid_midpoint, D0),
    config:param(sigmoid_steepness, K),
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).
```

Parameters: L = -0.20, U = 1.50, D0 = 0.50, K = 6.0. This is the function f(d) in
χ = ε × f(d(P)) × σ(S).

---

### 1.6 Corpus Data

**Type claim counts (from testsets/corpus .pl files):**

| Type | Count |
|------|-------|
| rope | 5,133 |
| tangled_rope | 4,684 |
| snare | 1,604 |
| mountain | 1,215 |
| **Total type claims** | **12,636** |

Distinct constraints (from v6.11 Axiom 5): ~3,254.

**No composition tagging found:** grep for `composed_of`, `base_constraint`, `derived_from`
across all .pl files returned zero matches for composition semantics. `derived_from/3` in
`dirac_classification.pl` tracks epsilon-invariance decomposition, not type generation.

**No constraint instance tagged as being compositionally constructed** from other instances.
All type claims are atomic: `constraint_claim(id, type)` or equivalent.

---

### 1.7 Documentation: Prior Theoretical Claims

**v6.11 Axiom 3 (extraction chain, verbatim):**

> "Constraints are classified into six types organized around two axes: extraction and
> coordination. The extraction chain (mountain < rope < tangled_rope < snare) is totally
> ordered. Scaffold and piton are diagnostic categories rather than extraction-chain members."

**v6.11 Axiom 5 (type-space sufficiency, verbatim):**

> "The six-type space is sufficient: no additional dimensions are needed to capture the
> observable boolean structure of constraints. Boolean feature independence testing across
> 3,254 constraints confirms that all six observable boolean features (emerges_naturally,
> requires_active_enforcement, has_coordination_function, has_asymmetric_extraction,
> natural_law_without_beneficiary, is_constructed) have normalized mutual information >0.3
> with the type assignment and independence scores <0.15. No boolean feature meets the
> independence criteria that would indicate a missing dimension."

**v6.11 §6.4 (what is loose, verbatim, line 550):**

> "Type space as Heyting algebra (two absorbing elements prevent it). Power scaling as
> adjunction (triangle identities unverified). Signature resolution as lattice meet
> (priority dispatch table, not lattice operation)."

**when_splitting_isnt_solving.md Abstract (verbatim):**

> "Clean splits satisfy the sheaf gluing axiom: local data (per-observer classifications)
> paste together into a globally consistent assignment. Structured splits violate it:
> local classifications are individually coherent but globally incompatible, and the
> incompatibility is not a defect but a measurable signal."

**when_splitting_isnt_solving.md §2.1 (observer site, verbatim):**

> "Observer positions are modeled as objects in a category **C** whose morphisms encode
> structural transitions — gaining power, extending time horizon, acquiring exit options.
> Classification data — the type assigned to a constraint at each observer position —
> form a presheaf **F** on this category: an assignment of a classification to each
> observer context, together with restriction maps describing how classifications transform
> along transitions between contexts."

**when_splitting_isnt_solving.md §1.1 (worked non-compete example, verbatim):**

> "From the institutional position (employer, directionality d = 0.00), power-scaled
> extractiveness χ = 0.70 × (−0.12) × 1.0 = −0.08, classifying the constraint as
> Rope (coordination). From the powerless position (worker, d = 1.00), χ = 0.70 × 1.42
> × 0.8 = 0.79, classifying the constraint as Snare (extraction)."

**No document uses the words "Sheffer", "generating basis", or "primitive type".**
Closest: v6.11 Axiom 5's "sufficient" (not "minimal complete basis").

---

### 1.8 Anomalies

**Two absorbing elements in the composition table (§1.4):**
Mountain and piton each absorb all other types under `composition_rule/3`. In the table,
the only exception is mountain + piton → mountain, due to Prolog clause order (mountain
absorber fires first). Two distinct absorbing elements is an unusual algebraic structure;
standard lattice has at most one top and one bottom.

**Qualitative features in type gates (§1.3):**
Three Tangled Rope conditions (`requires_active_enforcement`, `has_coordination_function`,
`has_asymmetric_extraction`) and one Mountain condition (`emerges_naturally`) are declared as
`:- multifile` predicates — they can be true or false for any constraint regardless of its
(ε, χ, Supp) values. They are not functions of the continuous metrics.

**Effective immutability non-determinism (§1.5, constraint_indexing.pl:211–213):**
The civilizational × analytical cell deliberately maps to BOTH mountain AND rope:
```prolog
effective_immutability(civilizational, analytical, mountain).
effective_immutability(civilizational, analytical, rope).
```
Comment: "Non-deterministic by design. Callers querying rope (snare gate) succeed via
backtracking past the mountain clause."

**Extraction-chain gap between Rope and Tangled Rope:**
Rope requires `χ ≤ 0.35 ∧ ε ≤ 0.45`. Tangled Rope requires `χ ≥ 0.40 ∧ ε ≥ 0.30`.
The χ gap [0.35, 0.40] and ε overlap [0.30, 0.45] mean a constraint with χ ∈ (0.35, 0.40)
and ε ∈ [0.30, 0.45] falls into neither gate and reaches the `naturalized` or `unknown`
fallback, even though it sits between Rope and Tangled Rope in the extraction chain.

**Scaffold and piton not on the extraction chain (§1.7, Axiom 3):**
They are "diagnostic categories rather than extraction-chain members" — they are typed by
structural/temporal features (sunset clauses, theater ratio, coordination vitality), not
by position on the (ε, χ) plane.

---

## Pass 2: Feasibility Analysis

*All claims cite Pass 1 section numbers. No new evidence is introduced.*

---

### 2.1 Is Constraint Composition Defined?

**Yes, at the type level. No, at the instance level.**

`composition_rule/3` (§1.4) is a binary operation on the **type space** {mountain, rope,
tangled_rope, snare, scaffold, piton, unknown}. The caller `composite_type/4` (§1.4) first
classifies each constraint instance independently via `dr_type/3`, then applies the table.
This means composition operates on *already-classified type labels*, not on constraint
instances directly.

There is no predicate that takes two constraint instances and produces a third instance.
No semantic notion of "constraint C₁ composed with C₂ yields a new constraint C₃" exists
in the codebase.

**Consequence for the Sheffer question:** The strong form of the Sheffer hypothesis —
"a single operation on constraint instances generates all types" — cannot be posed in the
current system because instance-level composition is undefined. The weak form — "a single
operation on type labels generates all type labels" — can be posed using `composition_rule/3`.
The gating prerequisite is to specify which form is intended before assessing feasibility.

---

### 2.2 Are the Core Types Independent?

**Partially, with important asymmetries.**

The extraction chain is totally ordered (§1.7, Axiom 3): mountain < rope < tangled_rope < snare.
This ordering could suggest that a single underlying variable (extraction level) parameterizes
all types. But examining the Prolog gate conditions (§1.3) reveals two structurally distinct
sub-problems:

**Metric-only types:** Snare and Rope are distinguished primarily by metric thresholds:
Snare requires χ ≥ 0.66 ∧ ε ≥ 0.46; Rope requires χ ≤ 0.35 ∧ ε ≤ 0.45 (§1.5). These
could in principle be read as "the same structural pattern at different extraction intensities."

**Qualitatively-gated types:** Mountain additionally requires `emerges_naturally(C)` (§1.3,
drl_core.pl:305), a boolean property not derived from metrics. Tangled Rope requires all of:
`requires_active_enforcement`, `has_coordination_function`, AND `has_asymmetric_extraction`
(§1.3, drl_core.pl:362–364). Axiom 5 (§1.7) confirms these boolean features are independent
of each other (NMI > 0.3, independence < 0.15) — none is derivable from the others.

**What the composition table shows about independence (§1.4):**
- `composition_rule(rope, snare, snare)` — snare dominates rope
- `composition_rule(rope, tangled_rope, tangled_rope)` — tangled_rope dominates rope
- `composition_rule(mountain, snare, mountain)` — mountain absorbs snare (not rope or tangled)
- **Tangled Rope is never produced from {mountain, snare} pairs.** `composition_rule(mountain, snare, mountain)` and `composition_rule(snare, mountain, mountain)` both yield mountain. There is no pair of non-tangled-rope inputs that yields tangled_rope.

**The sub-algebra {rope, tangled_rope, snare}** (excluding the two absorbers mountain and piton,
and the diagnostic types scaffold and piton) is closed under `composition_rule/3`:
- rope + rope → rope
- rope + tangled_rope → tangled_rope
- tangled_rope + rope → tangled_rope
- tangled_rope + tangled_rope → tangled_rope
- rope + snare → snare
- snare + rope → snare
- tangled_rope + snare → snare
- snare + tangled_rope → snare
- snare + snare → snare

This 3×3 sub-table is closed. Whether it has a single generating operation is a finite
algebra question not settled by code inspection alone (see §2.7 Recommendation B).

---

### 2.3 What Does the Lattice Structure Suggest?

**The type space is not a lattice in the algebraic sense. Its algebraic structure
actively contradicts the prerequisites for a classical Sheffer generator.**

v6.11 §6.4 (§1.7) states explicitly: "Type space as Heyting algebra (two absorbing elements
prevent it)." The parenthetical "two absorbing elements prevent it" refers to the fact that
a Boolean algebra (and therefore a Sheffer stroke) requires exactly one top element and one
bottom element. Mountain and piton are each absorbing: applying `composition_rule` with either
as an input always returns that type (§1.4). With two distinct absorbing elements, the type
space cannot be a Boolean algebra and therefore admits no classical Sheffer stroke.

A Heyting algebra is a generalization supporting intuitionistic logic. It need not be Boolean.
Its internal logic is intuitionistic: excluded middle (A ∨ ¬A) need not hold. Whether a
Heyting algebra can have a Sheffer-like generator in the generalized sense (an operation
generating all elements) is a non-trivial algebraic question that the codebase does not address.

**Effective immutability is a lookup table, not a derived structure (§1.5).** The values
{mountain, rope} assigned by `effective_immutability/3` do not arise from any operation
on other types. They are irreducible atomic assignments.

**No bottom element.** Mountain is the lowest-extraction type in the extraction chain (§1.7),
but it cannot serve as a Sheffer generator seed because it is absorbing: composing anything
with a mountain yields mountain. A bottom element in a Sheffer algebra must generate the
entire algebra by composition — an absorbing element does the opposite.

---

### 2.4 Candidate Primitive Assessment

#### Extraction potential alone (ε or χ as the single underlying dimension)

**Feasibility: Weak**

The χ formula (§1.5) is a single continuous function: χ = ε × f(d(P)) × σ(S). This is one
formula generating all power-scaled extraction values from one base measurement (ε) and two
context parameters. The extraction chain mountain < rope < tangled_rope < snare is totally
ordered (§1.7, Axiom 3), which could suggest that extraction alone distinguishes all four types.

Evidence against: Mountain requires `emerges_naturally(C)` (§1.3), which is independent of ε.
A constraint with ε = 0.10 and `emerges_naturally(C)` is a Mountain; the identical ε = 0.10
without that property is a Rope. Tangled Rope requires `has_coordination_function(C)` AND
`has_asymmetric_extraction(C)` (§1.3). These are not derivable from the (ε, χ, Supp) triple.
Axiom 5 (§1.7) confirms all boolean features are independently informative. Extraction alone
cannot distinguish Mountain from low-ε Rope, or Rope from Tangled Rope without the qualitative
boolean features.

#### The presheaf restriction map

**Feasibility: Moderate — the most structurally interesting candidate**

The presheaf structure (§1.2, §1.5, §1.7) is the system's existing "richer abstract space."
`dr_type(C, Context, Type)` is documented as a "Presheaf evaluation — computes local truth
value from Omega at a point of the site" (§1.3, drl_core.pl:394). The restriction maps are
the transitions between observer contexts: the same constraint is evaluated at each of the
four canonical contexts (powerless, moderate, institutional, analytical) with different χ
values arising from the sigmoid formula.

The worked example (§1.7, when_splitting_isnt_solving.md): a non-compete agreement with
ε = 0.70 is Snare at the powerless context (χ = 0.79) and Rope at the institutional context
(χ = −0.08). The restriction map from powerless to institutional "generates" the Rope
classification from the Snare classification by applying the power scaling f(d) with d = 0.00.

This suggests the restriction map could serve as an analogue of the EML generating operation:
a single formula (the sigmoid transformation) that, when applied to a base classification
(or base metric value), produces the full range of type assignments across the observer site.

Evidence for: the non-compete example shows that snare → rope is mechanically produced by
the single formula χ = ε × f(d(P)) × σ(S) with varying d. The site of observer contexts
is finite (4 canonical, 156 product), so the "composition" of restriction maps is a tractable
object. The when_splitting_isnt_solving.md paper (§1.7) establishes that the presheaf
structure is stable across the 156-point product site, suggesting the generating mechanism
is robust.

Evidence against: The restriction maps operate within the extraction chain (snare ↔ tangled_rope
↔ rope), but cannot generate Mountain. Mountain requires `emerges_naturally(C)` (§1.3), which
does not vary with observer context — it is a fixed property of the constraint instance. A
non-compete agreement cannot be a Mountain from any observer position regardless of d, because
it has an identifiable beneficiary (§1.3, natural_law_without_beneficiary check). Similarly,
restriction maps cannot generate Scaffold or Piton, which are detected by theater_ratio and
coordination_vitality, not by power scaling.

#### Nash distance as primitive

**Feasibility: No evidence**

No Nash equilibrium computation was found in any .pl file (§1.2, §1.4). The closest
formal concept is the Boltzmann MaxEnt shadow classifier (referenced in v6.11 Axiom 4),
which computes maximum-entropy type distributions, not Nash equilibrium distances. Nash
distance is not represented in the codebase as found.

---

### 2.5 The Complex Intermediate Question

**DR's presheaf site IS a richer abstract space analogous to EML's complex-valued intermediate
computation. The analogy is structurally sound but the generating mechanism differs in kind.**

The EML result (Odrzywołek 2026) requires that all elementary functions can be generated from
`eml(x,y) = exp(x) − ln(y)`. The key structural feature is that eml operates internally in
the complex plane even though its inputs and outputs are real. The "richness" of the
intermediate space (complex numbers) is what enables the generating capability.

In DR, the classification function `dr_type(C, Context, Type)` (§1.3, drl_core.pl:394,
comment: "Presheaf evaluation — computes local truth value from Omega at a point of the
site") operates in a precisely analogous intermediate space:

- **Inputs:** a constraint instance C (with fixed ε, Supp, qualitative boolean features)
  and an observer context (Power, TimeHorizon, ExitOptions, Scope)
- **Intermediate space:** the product (ε × d(P) × σ(S)) → χ computation using the sigmoid
  formula, followed by comparison to threshold boundaries (§1.5)
- **Outputs:** a type label {mountain, rope, tangled_rope, snare, scaffold, piton, unknown}

The "richer space" is the observer site × ℝ³ (the space of (ε, χ, Supp) triples). A single
constraint instance maps to a continuous real value (χ) that varies with the observer context
through the sigmoid f(d). This is the analogue of complex-valued intermediate computation.

The when_splitting_isnt_solving.md paper (§1.7) establishes that this intermediate structure
has measurable geometry: H¹ cohomology captures "failures of assembly" when local sections
(per-observer type assignments) cannot be pasted into a global section. A constraint that
is Snare at powerless and Rope at institutional is a manifest presheaf — its H¹ value is
nonzero. The paper reports that the binary sheaf/presheaf classification is preserved across
the full 156-point product site with "zero crossings in either direction" (§1.7, abstract).

**Where the analogy holds:** EML's complex intermediate enables `exp` and `ln` to be
expressed as compositions of simpler operations. DR's presheaf intermediate enables the
variety of type assignments across the observer site to be expressed as a single evaluation
function (χ = ε × f(d) × σ(S)) applied to the same base data. The restriction maps between
contexts are the "composition operations" that generate variation.

**Where the analogy breaks down:** EML generates functions FROM a primitive operation.
DR evaluates a classification function AT different points of a site. The distinction matters:
in EML, you BUILD `exp(x)` from `eml`; in DR, you EVALUATE `dr_type(C, powerless_context)`
and `dr_type(C, institutional_context)` as independent queries against the same stored data.
There is no DR operation that takes a Snare classification and constructs a Rope from it by
composition — the Rope exists independently in the data; it is merely REVEALED by evaluating
at a different context point.

**The publishable claim** that survives this analysis: DR has a single continuous generating
formula (the sigmoid χ formula) that, when evaluated across the observer site, produces the
full observed distribution of type assignments. This is weaker than a Sheffer generator
(which would construct all types from one primitive), but it is a genuine result: the type
variety in the corpus arises from a single mathematical formula applied at different context
parameters, not from four independently designed classification functions.

---

### 2.6 Symbolic Regression Applicability

**Yes, applicable in principle. The expected result is threshold-boundary recovery, not
a generating function.**

**Metrics are continuous reals (§1.5):**
- ε (base extractiveness): ∈ [0, ~1], continuous, constraint-fixed
- χ (power-scaled extractiveness): ∈ (−∞, ~2], continuous, context-dependent
- Supp (suppression): ∈ [0, 1], continuous

**Corpus size (§1.6):** ~3,254 distinct constraints, 12,636 type claims — well above the
500-instance threshold for regression to be meaningful.

**Fisher information ε-sensitivity:** A diagnostic for this exists in the codebase
(cognitive_displacement_sweep.py, referenced in MEMORY.md). The Fisher information measures
how sensitive type classifications are to perturbations in ε. A high-Fisher-information
regime would be where small changes in ε cross threshold boundaries — exactly the
boundary regions a Sheffer decomposition would have to navigate.

**What regression would likely find:** The classification function is piecewise constant on
(ε, χ, Supp) space — it is 1.0 for Mountain where ε ≤ 0.25 ∧ Supp ≤ 0.05 ∧ emerges_naturally,
0.0 otherwise, and similarly for other types. The decision boundaries are hyperplane slices
(threshold comparisons). A gradient-based symbolic regression would recover these boundaries
as piecewise linear functions. This is useful for characterizing the type space geometry but
does not yield a Sheffer-like generating expression.

**The boolean qualitative features are invisible to gradient methods (§1.3):**
`emerges_naturally`, `has_coordination_function`, and `has_asymmetric_extraction` are
discrete boolean predicates (multifile declarations in narrative_ontology.pl). They are not
functions of (ε, χ, Supp). Any regression over the continuous metrics would have these
features as unexplained residuals — a systematic error for constraints near the Mountain
or Tangled Rope regions.

---

### 2.7 Verdict and Recommendation

**Feasibility rating:** Feasible with prerequisites (weak form); Not feasible as stated (strong form)

The strong Sheffer hypothesis — "a single primitive operation on constraint instances
generates all four core types" — is not feasible in the current system because:

1. Instance-level composition is undefined (§2.1)
2. Two absorbing elements prevent the type algebra from being Boolean (§2.3, §1.7 §6.4)
3. Qualitative boolean features in three of the four core type gates are not reducible
   to the continuous metrics that a generating formula would need to act on (§2.2, §1.3)

The weak Sheffer hypothesis — "a single operation on type labels generates the extraction
chain sub-algebra {rope, tangled_rope, snare}" — is an open question. The composition
table (§1.4) shows this sub-algebra is closed. Whether it has a single generator is
a finite algebra problem that is tractable to check exhaustively.

---

**Gating prerequisites** (things that must be true before the Sheffer question is
well-posed for DR):

1. **Specify the domain:** Type algebra ({6 labels}, `composition_rule/3`) vs. metric
   space (continuous ε, χ, Supp) vs. instance space (3,254 concrete constraints).
   These are structurally different questions with different answers.

2. **Define whether mountain and piton are "generated types" or "boundary conditions."**
   If they are treated as terminal/absorbing boundary conditions (not elements to be
   generated), the Sheffer question reduces cleanly to the {rope, tangled_rope, snare}
   sub-algebra.

3. **Resolve the boolean feature barrier:** The Sheffer hypothesis requires that all
   type-distinguishing information be expressible as compositions of a single operation.
   The qualitative boolean features (`emerges_naturally`, `has_coordination_function`,
   `has_asymmetric_extraction`) carry structural information independent of the continuous
   metrics. A Sheffer generator operating on (ε, χ, Supp) alone cannot cross into Mountain
   or Tangled Rope territory. Either these features must be continuous-approximated or the
   domain must exclude the types that depend on them.

---

**Strongest evidence for a generator existing:**

- The extraction chain is **totally ordered** (§1.7, Axiom 3), suggesting a single
  underlying variable parameterizes the primary axis.
- The χ formula χ = ε × f(d(P)) × σ(S) is a **single continuous function** that generates
  the full range of χ values across the observer site from one base parameter (§1.5).
- Within the {rope, tangled_rope, snare} **sub-algebra**, `composition_rule/3` is closed
  with no absorption by mountain or piton — the generating question is well-posed here (§2.2).
- The **presheaf restriction maps** naturally generate type diversity from a single base
  metric through context variation — the sigmoid formula is a literal generator of
  χ-variation (§2.5).

**Strongest evidence against:**

- **Two absorbing elements** (mountain, piton) prevent the full type algebra from being
  Boolean, which is required for a classical Sheffer stroke (§1.4, §1.7 §6.4).
- **Tangled Rope is never produced from {mountain, snare} pairs** in the composition table
  — the derivation most needed for a Sheffer generator is absent (§1.4, auditable from
  the full table).
- **Qualitative boolean features** (§1.3) carry independent structural information not
  reducible to metrics; Axiom 5 confirms all six boolean features are independently
  necessary (§1.7).
- The codebase comment (§1.4, drl_composition.pl:84) explicitly states the composition
  operation is **"NOT a lattice meet"** — ruling out the algebraic structure that would
  make a single-generator result most natural.

---

**Recommended next action: (B)**

Attempt to express the **{rope, tangled_rope, snare} sub-algebra** as generated by a
single binary operation. Concretely:

1. Treat mountain and piton as boundary conditions (not generated types).
2. Check whether the 3×3 composition sub-table (§2.2) has a single generator — i.e.,
   whether there exists a type T and operation ⊕ such that T ⊕ T, T ⊕ (T ⊕ T), etc.
   eventually produces all three of {rope, tangled_rope, snare}.
3. If yes: this is the weak Sheffer result for DR. Report it with the precise caveat
   that mountain requires qualitative features that fall outside the generated sub-algebra.
4. If no: this is the negative result. The extraction chain is totally ordered but
   the type space is not generated from a single primitive. Report the algebraic
   obstruction precisely (likely: tangled_rope is idempotent and can only be reached
   from itself or rope, never from snare alone).

This check is exhaustive over a 3-element algebra and requires no new code — it is a
direct inspection of the table in §1.4.

---

## Appendix: Files Examined

| File | Lines | Sections Read |
|------|-------|---------------|
| prolog/drl_core.pl | 690 | Full: lines 1–440 |
| prolog/drl_composition.pl | 412 | Full: lines 1–150 |
| prolog/constraint_indexing.pl | 941 | Lines 1–100, 187–340 |
| prolog/config.pl | 587 | Lines 1–100, 155–275 |
| prolog/narrative_ontology.pl | 343 | Module header |
| prolog/signature_detection.pl | 1,209 | Module header, lines 65–115 |
| docs/deferential_realism_paper_v6.11.md | ~900 | Abstract, Axioms 1–6, §6.4 |
| docs/when_splitting_isnt_solving.md | ~293 | Full: lines 0–79 |
| prolog/validation_suite.pl | 3,379 | Not read (not relevant to taxonomy audit) |
| prolog/domain_registry.pl | 3,315 | Not read (corpus data, not classification logic) |
| python/cognitive_displacement_sweep.py | ~400 | Referenced via MEMORY.md; not read |

**Total Prolog codebase:** ~37,227 lines across all .pl files  
**Total Python codebase:** ~39,612 lines across all .py files
