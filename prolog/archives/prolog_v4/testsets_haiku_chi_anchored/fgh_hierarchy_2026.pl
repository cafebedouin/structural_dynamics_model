% ============================================================================
% CONSTRAINT STORY: fgh_hierarchy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fgh_hierarchy_2026, []).

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
 *   constraint_id: fgh_hierarchy_2026
 *   human_readable: The Fast-Growing Hierarchy
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy is a mathematical structure that classifies
 *   computable functions by their growth rates, indexed by transfinite
 *   ordinals. It emerges from the formal theory of ordinal arithmetic and
 *   recursion theory, not from any institutional arrangement or coordination
 *   problem. The FGH demonstrates the properties of a natural law constraint
 *   in formal mathematics: its ordering is independent of the observer,
 *   cannot be negotiated or renegotiated, and applies universally to all
 *   computable functions. No agent — individual mathematician, research
 *   institution, or scientific community — possesses the capacity to create a
 *   function that violates the hierarchy's growth rate ordering without
 *   either (a) moving outside the class of computable functions, (b)
 *   redefining the ordinal indexing scheme, or (c) appealing to stronger
 *   axioms of set theory. The constraint is not enforced by suppression or
 *   coercion; it is entailed by logical structure. The theater ratio is
 *   minimal because there is no performative or ritualistic component: the
 *   hierarchy's existence is purely deductive.
 *
 * KEY AGENTS:
 *   - Research Mathematicians: Powerful agents within the mathematical domain (powerful/mobile) — can choose which problems to study, but cannot escape the FGH's ordering
 *   - Formal Logic Community: Institutional custodian (institutional/arbitrage) — establishes foundational frameworks; ultimately subject to the hierarchy's constraints
 *   - Computational Complexity Theorists: Analytical observers (analytical/analytical) — study growth rate classification but do not construct or negotiate the hierarchy
 *   - Applied Computer Scientists: Practical beneficiaries (powerful/mobile) — use FGH classification for complexity analysis but have no power over its structure
 *   - Universal Mathematical Structure: The constraint itself (analytical/analytical) — no agent; pure logical entailment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fgh_hierarchy_2026, 0.12).
domain_priors:suppression_score(fgh_hierarchy_2026, 0.03).
domain_priors:theater_ratio(fgh_hierarchy_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, 0.12).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fgh_hierarchy_2026, mountain).
narrative_ontology:human_readable(fgh_hierarchy_2026, "The Fast-Growing Hierarchy").
narrative_ontology:topic_domain(fgh_hierarchy_2026, "mathematical/computational").

domain_priors:emerges_naturally(fgh_hierarchy_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIVERSAL MATHEMATICAL OBSERVER (MOUNTAIN) — The FGH is a structurally defined hierarchy indexed by ordinal notation. The growth rate ordering is a mathematical necessity, not a convention or institutional choice. No agent can negotiate away the fact that f_ω(n) grows faster than any fixed-level function. ε=0.12, suppression=0.03, fully independent of who observes it.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH MATHEMATICIAN (MOUNTAIN) — Even mathematicians with significant agency and resources cannot evade the hierarchy's ordering. Attempts to construct functions that grow faster than the hierarchy are constrained by the very ordinal notation system that defines the hierarchy. The constraint is structural to the mathematical framework itself. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL LOGIC COMMUNITY (MOUNTAIN) — Mathematical institutions cannot negotiate the FGH's ordering even through collective action or resource allocation. The hierarchy emerges from the axioms of ordinal arithmetic and function composition. Attempts to 'create' faster-growing functions either remain within the hierarchy or require stepping outside the framework entirely (e.g., moving to higher set-theoretic universes). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: COMPUTATIONAL COMPLEXITY THEORIST (MOUNTAIN) — The FGH provides an axiomatic classification of computable function growth. The ordering respects logical derivability: if ordinal α < β in standard ordinal arithmetic, then f_α is provably dominated by f_β in Peano Arithmetic or stronger systems. This is not a convention; it follows from the consistency of the underlying logical framework. ε=0.12, suppression=0.03.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fgh_hierarchy_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, ExtMetricName, E),
    domain_priors:suppression_score(fgh_hierarchy_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fgh_hierarchy_2026),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fgh_hierarchy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The FGH does not extract resources from any agent; it classifies functions. No agent is coerced into accepting the hierarchy — it is voluntarily adopted because it is useful and true. The residual extractiveness reflects the effort cost of learning ordinal notation and the closure of alternative (weaker) classification schemes, but these are not extraction in the DR sense. Suppression (0.03): Negligible. The FGH suppresses no alternatives because it does not forbid them; it merely ranks them. Mathematicians remain free to study slow-growing functions, polynomial hierarchies, or non-ordinal classification schemes. The hierarchy does not exclude these — it situates them within its structure. Theater ratio (0.15): Very low. The FGH has minimal performative content. Its properties are stated axiomatically and verified deductively. There is no ritual, no social maintenance, no ambiguity about what the hierarchy claims. The slight non-zero value reflects the reality that even mathematical exposition requires pedagogical presentation and choice of notation, but these are not constitutive of the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification. This is the hallmark of a true natural law constraint: the ordering is invariant across all observational positions. Whether the observer is a mathematician choosing research directions (powerful/mobile), a logical community establishing foundations (institutional/arbitrage), a complexity theorist measuring asymptotic behavior (analytical/analytical), or the abstract mathematical structure itself (analytical/analytical), the FGH's growth rate ordering remains the same. The lack of perspectival gap is not a weakness of the model but confirmation that the constraint is structurally invariant. No agent can negotiate a different ordering without moving outside the mathematical framework entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint in the traditional sense. The FGH has no beneficiaries or victims because it is not an extraction or coordination mechanism. Every perspective yields d ≈ 0.5 to 0.72 (observer-neutral), f(d) ≈ 0.60 to 1.15 (moderate to high multiplier), but the resulting χ is negligible because ε itself is minimal. The constraint's power derives from its structural necessity, not from any agent's capacity to extract or suppress. The scope modifier σ remains 1.0 (universal) across all perspectives because the hierarchy's truth value is independent of spatial or temporal context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_notation_closure,
    'Is the FGH closed under all computable ordinal notations, or are there legitimate ordinal indexing schemes that escape the hierarchy?',
    'Formal proof that any computable ordinal notation system that indexes computable functions produces a subordinate ordering to the standard FGH, or discovery of a genuinely alternative hierarchy with different structural properties',
    'If closed: FGH is a true universal classifier (mountain confirmed). If open: there exist computationally irreducible ordinal systems that produce orthogonal function classifications (constraint fragments into multiple stories).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_notation_closure, empirical, 'Whether the FGH is closed under all computable ordinal notations').

omega_variable(
    set_theoretic_foundational_dependence,
    'Is the FGH''s ordering independent of the choice of foundational set theory (ZFC, NBG, constructible universe), or does it depend on impredicative set assumptions?',
    'Formal derivation of FGH in constructive mathematics and predicative set theory; identification of which ordinal levels require impredicative or non-constructive principles',
    'If independent: FGH is foundationally robust (stronger mountain). If dependent: the hierarchy is contingent on set-theoretic choices; different foundational frameworks produce different hierarchies (multiple constraint stories).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(set_theoretic_foundational_dependence, conceptual, 'Dependence of FGH ordering on foundational set theory').

omega_variable(
    physical_implementation_realizability,
    'Do the function growth rates in the FGH correspond to physically realizable computational resources, or do higher levels represent abstract classifications with no physical referent?',
    'Analysis of physical implementability of f_ω, f_{ω+1}, and higher ordinal-indexed functions given thermodynamic constraints; establishment of a physical ceiling on realizable growth rates',
    'If realizable: FGH is a natural constraint on computable universe behavior (mountain from physics perspective). If not: FGH is a pure mathematical hierarchy with no extractive or coordination role in physical reality (rope of pure mathematics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_implementation_realizability, empirical, 'Physical realizability of FGH growth rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fgh_hierarchy_2026, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fgh_tr_t0, fgh_hierarchy_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fgh_tr_t50, fgh_hierarchy_2026, theater_ratio, 50, 0.15).
narrative_ontology:measurement(fgh_tr_t100, fgh_hierarchy_2026, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(fgh_be_t0, fgh_hierarchy_2026, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fgh_be_t50, fgh_hierarchy_2026, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(fgh_be_t100, fgh_hierarchy_2026, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fgh_hierarchy_2026, information_standard).
narrative_ontology:affects_constraint(fgh_hierarchy_2026, peano_arithmetic_consistency).
narrative_ontology:affects_constraint(fgh_hierarchy_2026, turing_computability_class).

% DUAL FORMULATION NOTE:
% The FGH is a pure mathematical structure with no dual extraction/coordination decomposition. It appears as a Mountain universally because it is a logical entailment, not an institutional arrangement. Related constraints in the mathematical/computational domain (Peano consistency, Turing computability) use the FGH as a reference framework, making the FGH upstream in the dependency hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
