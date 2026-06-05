% ============================================================================
% CONSTRAINT STORY: three_body_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_three_body_problem, []).

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
 *   constraint_id: three_body_problem
 *   human_readable: Predictability Limit in the Three-Body Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The three-body problem is a fundamental limit on predictability in
 *   dynamical systems. Even with perfect knowledge of initial conditions, the
 *   long-term behavior of three interacting bodies can be chaotic and
 *   impossible to predict with certainty.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Sees the fundamental limit (analytical/analytical)
 *   - Individual Attempting Precise Prediction: Experiences the limit directly (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_problem, 0.15).
domain_priors:suppression_score(three_body_problem, 0.05).
domain_priors:theater_ratio(three_body_problem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_problem, extractiveness, 0.15).
narrative_ontology:constraint_metric(three_body_problem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(three_body_problem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(three_body_problem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(three_body_problem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(three_body_problem, mountain).
narrative_ontology:human_readable(three_body_problem, "Predictability Limit in the Three-Body Problem").
narrative_ontology:topic_domain(three_body_problem, "technological").

domain_priors:emerges_naturally(three_body_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an analytical observer, the predictability limit is a fundamental mathematical constraint.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% For someone trying to get a precise prediction of the three-body problem, it appears as a fundamental limit. They are trapped by the mathematics.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(three_body_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_problem, ExtMetricName, E),
    domain_priors:suppression_score(three_body_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(three_body_problem),
    narrative_ontology:constraint_metric(three_body_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(three_body_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(three_body_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the problem does not extract resources, it is merely a limitation. Suppression is also low as it doesn't actively suppress anything, it simply exists as a mathematical property.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives see it as a Mountain due to the inherent mathematical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is a fundamental property of the system, not an exploitable feature or a source of extraction. Therefore, directionality is not strongly applicable.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable, since it is a mountain
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(three_body_problem, 0, 100).

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
