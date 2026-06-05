% ============================================================================
% CONSTRAINT STORY: pythagorean_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pythagorean_theorem, []).

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
 *   constraint_id: pythagorean_theorem
 *   human_readable: Pythagorean Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Pythagorean Theorem is a fundamental relationship in Euclidean
 *   geometry among the three sides of a right triangle.
 *
 * KEY AGENTS:
 *   - The Naive Student (powerless/trapped)
 *   - The Mathematical Community (institutional/analytical)
 *   - The Analytical Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pythagorean_theorem, 0.01).
domain_priors:suppression_score(pythagorean_theorem, 0.01).
domain_priors:theater_ratio(pythagorean_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pythagorean_theorem, extractiveness, 0.01).
narrative_ontology:constraint_metric(pythagorean_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(pythagorean_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(pythagorean_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pythagorean_theorem, mountain).
narrative_ontology:human_readable(pythagorean_theorem, "Pythagorean Theorem").
narrative_ontology:topic_domain(pythagorean_theorem, "mathematical").

domain_priors:emerges_naturally(pythagorean_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a student just learning the theorem, it's an immutable rule.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% For the mathematical community, it's a foundational element of Euclidean geometry.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% As a fundamental theorem, it is universally true within its defined geometric space.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pythagorean_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pythagorean_theorem, ExtMetricName, E),
    domain_priors:suppression_score(pythagorean_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pythagorean_theorem),
    narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pythagorean_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pythagorean_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This is a mathematical theorem, not a social or economic construct. Therefore, extractiveness and suppression are minimal. The theorem emerges naturally from the axioms of Euclidean geometry.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the theorem as a Mountain, reflecting its fundamental and unchangeable nature within Euclidean geometry.
 *
 * DIRECTIONALITY LOGIC:
 *   As a fundamental theorem, the concept of beneficiaries and victims is not directly applicable. The theorem is a universally accepted truth within its domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The theorem's classification as a Mountain prevents mislabeling it as a pure extraction mechanism. It is a fundamental property of Euclidean space, not a tool for coercion or rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pythagorean_theorem, 0, 1).

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
