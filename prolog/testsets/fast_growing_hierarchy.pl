% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fast_growing_hierarchy
 *   human_readable: The Fast-Growing Hierarchy (FGH)
 *   domain: computational_theory/mathematical_foundations
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy (FGH) is a mathematical structure that
 *   classifies the growth rates of computable functions using ordinal
 *   indexing. For each ordinal α, the FGH defines a function f_α whose growth
 *   rate is strictly faster than all f_β for β < α (up to a limiting
 *   ordinal). The hierarchy is not a discovered empirical pattern but a
 *   logical consequence of transfinite recursion axioms. Once you fix the
 *   definition of ordinal indexing and recursive function composition, the
 *   hierarchy's structure is fully determined. This makes FGH a candidate for
 *   a pure mountain constraint — its existence and structure are independent
 *   of any observer, institutional arrangement, or measurement framework. The
 *   constraint exhibits zero degrees of freedom: changing the indexing scheme
 *   does not reorder the hierarchy (by proof), and denying the hierarchy's
 *   validity requires rejecting the axioms of recursion that underpin all
 *   computability theory. Extractiveness is minimal because no agent benefits
 *   disproportionately from the hierarchy's structure — it serves all
 *   computational theories equally as a neutral classification tool.
 *
 * KEY AGENTS:
 *   - Proof-Theoretic Logicians: Analytical observers who use FGH to classify proof complexity and ordinal strength of formal systems
 *   - Algorithm Designers: Practical users who apply FGH bounds to verify termination and resource usage
 *   - Formal Verification Systems: Automated tools (Coq, Agda) that embed FGH reasoning in type-theoretic termination checkers
 *   - Mathematical Foundations Community: Consensus-builders who maintain the axioms (ZFC, type theory) underlying ordinal definition
 *   - Computational Physics: Researchers exploring whether FGH's growth rates are physically realizable or exceed thermodynamic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.08).
domain_priors:suppression_score(fast_growing_hierarchy, 0.02).
domain_priors:theater_ratio(fast_growing_hierarchy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.08).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fast_growing_hierarchy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).
narrative_ontology:human_readable(fast_growing_hierarchy, "The Fast-Growing Hierarchy (FGH)").
narrative_ontology:topic_domain(fast_growing_hierarchy, "computational_theory/mathematical_foundations").

domain_priors:emerges_naturally(fast_growing_hierarchy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROOF-THEORETIC ANALYST (MOUNTAIN) — The FGH is a logical consequence of ordinal recursion and the definition of computability. Given any ordinal indexing scheme, the hierarchy's structure follows necessarily from the transfinite recursion definition. No negotiation or institutional enforcement possible — the mathematical structure is independent of measurement or framing. Accessibility collapse (0.92): The hierarchy's growth rates are formally verifiable through recursive definition. Resistance (0.08): Mathematical truth requires no enforcement or suppression.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL THEORIST (MOUNTAIN) — The FGH provides a rigorous classification of algorithmic growth rates that is independent of implementation details, hardware, or institutional preferences. Theorists can apply the hierarchy to bound-check algorithms and prove termination properties, but they cannot alter the hierarchy's structure itself. The constraint emerges from the mathematics, not from institutional practice. Extractiveness remains low because the hierarchy functions purely as a tool for understanding computation, with no extractive asymmetry.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL VERIFICATION SYSTEM (MOUNTAIN) — Proof assistants, type systems, and automated theorem provers depend on ordinal-indexed hierarchies to guarantee termination. The FGH structure is invariant to implementation choices — whether using Coq, Agda, or direct ordinal recursion, the hierarchy's growth rates are immutable. This perspective sees FGH as a natural law of computation itself, not a convention or institutional arrangement.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fast_growing_hierarchy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, ExtMetricName, E),
    domain_priors:suppression_score(fast_growing_hierarchy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fast_growing_hierarchy),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The FGH is a classification tool, not an extraction mechanism. No agent captures disproportionate value from the hierarchy's structure. Proof-theoretic logicians benefit equally with algorithm designers; no group can monopolize or gate-keep access to the hierarchy. The low value reflects that FGH serves all computational theories as a neutral standard. Suppression (0.02): Near-zero. The hierarchy requires no enforcement, coercion, or alternative suppression. Once the axioms of recursion are accepted, the hierarchy follows necessarily. Resistance to verification: extremely low (accessibility_collapse = 0.92) because the hierarchy is formally checkable via mechanical proof. Theater ratio (0.15): Low. While presentations of FGH can be pedagogically complex, the core mathematical claim is not performative. The theater derives only from the explanation difficulty for non-specialists — the underlying structure is pure function, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (analytical observer, computational theorist, formal system) converge on mountain classification. This is a uniform-type constraint where no perspectival gap emerges. The analyst and practitioner see the same invariant structure. The formal system implements the hierarchy without negotiation or institutional mediation. The absence of a perspectival gap is itself the diagnostic signal that FGH is a genuine mountain — it exhibits invariance across all measurement contexts and observer positions.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiary/victim structure because it is a pure mountain. There is no extraction flow, no asymmetric power, and no exit option asymmetry. All agents experience FGH identically: as a neutral mathematical framework independent of their institutional position. The derivation chain does not apply — mountains are not decomposed via directionality because they have no directionality. The constraint is invariant to the observer's power level, time horizon, and exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_representation_scheme,
    'Does the choice of ordinal notation system (Cantor normal form, Bachmann-Howard, etc.) affect the growth rates assigned by FGH, or is the hierarchy invariant to representation?',
    'Formal proof that growth rate orderings are isomorphic across all standard ordinal notation systems; demonstration that no notation system can reorder the hierarchy without violating transfinite recursion axioms',
    'If invariant: FGH is a pure mountain (representation-independent). If notation-dependent: FGH contains a rope component (coordination on notation standard). Current consensus: invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_representation_scheme, empirical, 'Ordinal representation invariance').

omega_variable(
    computational_realizability_floor,
    'Does the FGH hierarchy apply equally to all computable functions, or do physical limits (energy, time, matter density) create a practical floor where FGH indices exceed realizability?',
    'Correlation between FGH index and minimum energy/time/matter required to instantiate the function; determination of whether this floor is a natural law or a constraint on physical embodiment',
    'If natural law: FGH remains mountain. If physical constraint: there exists a ''practical FGH'' (faster-growing but unrealizable) vs ''realizable FGH'' (bounded by physics), suggesting a tangled rope where physics provides coordination role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_realizability_floor, empirical, 'Whether physical realizability constraints affect FGH classification').

omega_variable(
    foundational_axiom_independence,
    'Is the FGH hierarchy independent of the set-theoretic axioms (ZFC, constructive type theory, etc.) used to formalize it, or does the choice of foundation alter which functions qualify as ''in'' the hierarchy?',
    'Proof-theoretic analysis comparing FGH structure in ZFC, intuitionistic type theory, and weaker foundations; identification of which ordinal indices are framework-dependent',
    'If independent: pure mountain. If dependent: FGH is a scaffold over a foundational choice, suggesting governance role by mathematical community consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_independence, conceptual, 'Axiom system independence of FGH').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fast_growing_hierarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fgh_tr_t0, fast_growing_hierarchy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fgh_tr_t5, fast_growing_hierarchy, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fgh_tr_t10, fast_growing_hierarchy, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(fgh_be_t0, fast_growing_hierarchy, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fgh_be_t5, fast_growing_hierarchy, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(fgh_be_t10, fast_growing_hierarchy, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fast_growing_hierarchy, information_standard).
narrative_ontology:affects_constraint(fast_growing_hierarchy, ackermann_function_computability).
narrative_ontology:affects_constraint(fast_growing_hierarchy, proof_theoretic_ordinals).
narrative_ontology:affects_constraint(fast_growing_hierarchy, transfinite_recursion_axioms).

% DUAL FORMULATION NOTE:
% FGH is upstream of specific computational complexity constraints. The hierarchy itself is a pure mountain; specific applications (e.g., termination proofs for Ackermann-level functions) may exhibit rope or tangled-rope properties depending on institutional adoption and verification mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
