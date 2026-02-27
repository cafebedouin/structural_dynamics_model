% ============================================================================
% CONSTRAINT STORY: countable_infinity_cardinality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_countable_infinity_cardinality, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: countable_infinity_cardinality
 *   human_readable: Cardinality of Countably Infinite Sets (Aleph-0)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The principle that any set which can be put into a one-to-one
 *   correspondence with the natural numbers has the same cardinality,
 *   aleph-naught (ℵ₀), is a cornerstone of set theory. This constraint
 *   highlights a fundamental aspect of infinite sets, specifically their
 *   cardinality. It states that sets such as the set of integers, the set of
 *   rational numbers, and the set of algebraic numbers are all countably
 *   infinite and thus have the same cardinality as the natural numbers.
 *
 * KEY AGENTS:
 *   - Naive Set Theorist (powerless/trapped)
 *   - Analytical Observer (analytical/analytical)
 *   - Mathematics Community (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(countable_infinity_cardinality, 0.01).
domain_priors:suppression_score(countable_infinity_cardinality, 0.01).
domain_priors:theater_ratio(countable_infinity_cardinality, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(countable_infinity_cardinality, extractiveness, 0.01).
narrative_ontology:constraint_metric(countable_infinity_cardinality, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(countable_infinity_cardinality, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(countable_infinity_cardinality, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(countable_infinity_cardinality, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(countable_infinity_cardinality, mountain).
narrative_ontology:human_readable(countable_infinity_cardinality, "Cardinality of Countably Infinite Sets (Aleph-0)").
narrative_ontology:topic_domain(countable_infinity_cardinality, "mathematical/logical").

domain_priors:emerges_naturally(countable_infinity_cardinality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For someone with limited math background, the implications of countable infinity can be surprising and counter-intuitive. However, the mathematical truth remains unchanged.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% Axiomatic set theory dictates that all sets that can be put in bijection with the natural numbers have the same cardinality. This is a foundational principle.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The mathematical community accepts this principle as a foundation of set theory, and mathematics as a whole. No real disagreement or alternative theory exists.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(countable_infinity_cardinality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(countable_infinity_cardinality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(countable_infinity_cardinality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(countable_infinity_cardinality, ExtMetricName, E),
    domain_priors:suppression_score(countable_infinity_cardinality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(countable_infinity_cardinality),
    narrative_ontology:constraint_metric(countable_infinity_cardinality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(countable_infinity_cardinality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(countable_infinity_cardinality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater ratio are all low as this is a mathematical truth established within axiomatic set theory. It is a foundational principle, leading to the classification as a Mountain. The high accessibility collapse and low resistance reflect the strong consensus and provability of this concept within the mathematical community.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap as this is a fundamental mathematical truth. While a naive set theorist may initially find the implications counterintuitive, the principle itself remains unchanged and provable.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint is a fundamental mathematical truth and not subject to extraction or suppression. The analytical perspective emphasizes the formal mathematical nature of the claim, while the other perspectives reflect different levels of understanding and acceptance of this truth.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(countable_infinity_cardinality, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
