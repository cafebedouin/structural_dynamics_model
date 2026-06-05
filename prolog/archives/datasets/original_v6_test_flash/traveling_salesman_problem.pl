% ============================================================================
% CONSTRAINT STORY: traveling_salesman_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_traveling_salesman_problem, []).

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
 *   constraint_id: traveling_salesman_problem
 *   human_readable: Computational Complexity of the Traveling Salesman Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The Traveling Salesman Problem (TSP) represents a fundamental
 *   computational limit due to its NP-hardness. While heuristic algorithms
 *   can find near-optimal solutions, finding the exact solution for large
 *   instances requires exponential time complexity. This inherent complexity
 *   acts as a constraint on various optimization processes.
 *
 * KEY AGENTS:
 *   - Powerless Solver: Limited by computational power (powerless/trapped)
 *   - Computer Scientist: Recognizes complexity class (institutional/analytical)
 *   - Analytical Observer: Sees inherent mathematical limit (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(traveling_salesman_problem, 0.15).
domain_priors:suppression_score(traveling_salesman_problem, 0.01).
domain_priors:theater_ratio(traveling_salesman_problem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(traveling_salesman_problem, extractiveness, 0.15).
narrative_ontology:constraint_metric(traveling_salesman_problem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(traveling_salesman_problem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(traveling_salesman_problem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(traveling_salesman_problem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(traveling_salesman_problem, mountain).
narrative_ontology:human_readable(traveling_salesman_problem, "Computational Complexity of the Traveling Salesman Problem").
narrative_ontology:topic_domain(traveling_salesman_problem, "technological").

domain_priors:emerges_naturally(traveling_salesman_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The solver is limited by the inherent computational complexity of the problem. They cannot escape the exponential time complexity for exact solutions.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The computer scientist understands the inherent computational limits, regardless of the algorithm or hardware used. This is a fundamental limitation.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% An analytical observer recognizes that the problem's complexity is a mathematical property, not a matter of opinion. The problem's complexity cannot be avoided with current computational models.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(traveling_salesman_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(traveling_salesman_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(traveling_salesman_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(traveling_salesman_problem, ExtMetricName, E),
    domain_priors:suppression_score(traveling_salesman_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(traveling_salesman_problem),
    narrative_ontology:constraint_metric(traveling_salesman_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(traveling_salesman_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(traveling_salesman_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the problem is a fundamental limit rather than a system extracting value. Suppression is also low as heuristic solutions are readily available. The problem's inherent difficulty makes it a mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap as all perspectives recognize the problem's inherent computational complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   The problem's computational complexity imposes limitations on all agents. There are no specific beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A - The problem is fundamentally a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(traveling_salesman_problem, 0, 100).

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
