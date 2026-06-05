% ============================================================================
% CONSTRAINT STORY: kolmogorov_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kolmogorov_complexity, []).

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
 *   constraint_id: kolmogorov_complexity
 *   human_readable: Kolmogorov Complexity Limit
 *   domain: technological
 *
 * SUMMARY:
 *   Kolmogorov Complexity defines the length of the shortest possible
 *   description of an object (e.g., a string of data).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kolmogorov_complexity, 0.1).
domain_priors:suppression_score(kolmogorov_complexity, 0.01).
domain_priors:theater_ratio(kolmogorov_complexity, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kolmogorov_complexity, extractiveness, 0.1).
narrative_ontology:constraint_metric(kolmogorov_complexity, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(kolmogorov_complexity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kolmogorov_complexity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kolmogorov_complexity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kolmogorov_complexity, mountain).
narrative_ontology:human_readable(kolmogorov_complexity, "Kolmogorov Complexity Limit").
narrative_ontology:topic_domain(kolmogorov_complexity, "technological").

domain_priors:emerges_naturally(kolmogorov_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Kolmogorov complexity is a fundamental limit on compression, thus a mountain.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Even institutions cannot overcome the limits of Kolmogorov complexity.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Analytical observer cannot change the inherent Kolmogorov complexity.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kolmogorov_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kolmogorov_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kolmogorov_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kolmogorov_complexity, ExtMetricName, E),
    domain_priors:suppression_score(kolmogorov_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kolmogorov_complexity),
    narrative_ontology:constraint_metric(kolmogorov_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kolmogorov_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kolmogorov_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kolmogorov_complexity, 0, 100).

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
