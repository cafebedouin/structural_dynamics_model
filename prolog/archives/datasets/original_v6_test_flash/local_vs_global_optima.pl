% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_local_vs_global_optima, []).

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
 *   constraint_id: local_vs_global_optima
 *   human_readable: The Existence of Local Optima in Non-Convex Spaces
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   In any non-convex optimization landscape, there exist local
 *   optima—solutions that are superior to all their immediate neighbors but
 *   are not the best possible solution (the global optimum). This is a
 *   fundamental constraint in many computational and mathematical problems.
 *
 * KEY AGENTS:
 *   - Powerless Search Algorithm: Primary target (powerless/trapped) - Unable to escape local optima due to limited resources.
 *   - Mathematical Understanding: Primary beneficiary (institutional/analytical) - understands the fundamental properties of non-convex spaces.
 *   - Analytical Observer: Analytical observer (analytical/analytical) - sees the full structure and its implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(local_vs_global_optima, 0.05).
domain_priors:suppression_score(local_vs_global_optima, 0.01).
domain_priors:theater_ratio(local_vs_global_optima, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(local_vs_global_optima, extractiveness, 0.05).
narrative_ontology:constraint_metric(local_vs_global_optima, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(local_vs_global_optima, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(local_vs_global_optima, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(local_vs_global_optima, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(local_vs_global_optima, mountain).
narrative_ontology:human_readable(local_vs_global_optima, "The Existence of Local Optima in Non-Convex Spaces").
narrative_ontology:topic_domain(local_vs_global_optima, "mathematical/computational").

domain_priors:emerges_naturally(local_vs_global_optima).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A search algorithm with limited resources is trapped within the landscape and cannot escape local optima.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From a mathematical perspective, the existence of local optima is an inherent property of non-convex spaces.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Analytical observer views this as inherent to the mathematics of non-convex spaces
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(local_vs_global_optima_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(local_vs_global_optima, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(local_vs_global_optima, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(local_vs_global_optima, ExtMetricName, E),
    domain_priors:suppression_score(local_vs_global_optima, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(local_vs_global_optima),
    narrative_ontology:constraint_metric(local_vs_global_optima, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(local_vs_global_optima, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(local_vs_global_optima_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because it's a mathematical property, not an actively enforced constraint. Suppression is low as well because one can, in principle, always find the global optimum given enough time or a clever algorithm. Theater ratio is low, as the phenomenon is fundamental and not performative.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as mountain because the existence of local optima is a fundamental mathematical property that is not dependent on the observer.
 *
 * DIRECTIONALITY LOGIC:
 *   The algorithms are the victim as they are being 'extracted' from, whereas the mathematical perspective benefits from understanding the concept, therefore has 'negative' extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable, as extractiveness is very low.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(local_vs_global_optima, 0, 100).

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
