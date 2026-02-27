% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_algebra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_theorem_of_algebra, []).

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
 *   constraint_id: fundamental_theorem_of_algebra
 *   human_readable: Fundamental Theorem of Algebra (FTA)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Fundamental Theorem of Algebra (FTA) is a cornerstone of complex
 *   analysis, stating that every non-constant single-variable polynomial with
 *   complex coefficients has at least one complex root. This theorem is
 *   considered a fundamental mathematical truth.
 *
 * KEY AGENTS:
 *   - Naive Student: (powerless/trapped) - must accept the theorem as true.
 *   - Mathematical Community: (institutional/analytical) - relies on the theorem for further research.
 *   - Analytical Observer: (analytical/analytical) - views the theorem as a basic truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_algebra, 0.01).
domain_priors:suppression_score(fundamental_theorem_of_algebra, 0.01).
domain_priors:theater_ratio(fundamental_theorem_of_algebra, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_of_algebra, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_algebra, "Fundamental Theorem of Algebra (FTA)").
narrative_ontology:topic_domain(fundamental_theorem_of_algebra, "mathematical").

domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The theorem holds regardless of the student's understanding.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% The theorem is a cornerstone of complex analysis.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% The theorem is a fundamental truth within the structure of complex numbers and polynomials.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_of_algebra_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_theorem_of_algebra, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_algebra),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_theorem_of_algebra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are minimal because the FTA is a fundamental mathematical truth, and does not actively extract or suppress any agents. Theater ratio is low because the theorem's validity is universally accepted.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap as the theorem is consistently viewed as a fundamental truth across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   As a fundamental truth, the directionality is largely irrelevant. No agent is a clear beneficiary or victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The theorem cannot be mistaken for a snare or tangled rope. The FTA offers a coordination within a mathematical framework that offers structure to polynomial equations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_algebra, 0, 100).

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
