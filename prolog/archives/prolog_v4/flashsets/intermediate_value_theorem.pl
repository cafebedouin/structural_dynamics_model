% ============================================================================
% CONSTRAINT STORY: intermediate_value_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_value_theorem, []).

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
 *   constraint_id: intermediate_value_theorem
 *   human_readable: Accessibility Barrier to the Intermediate Value Theorem
 *   domain: technological
 *
 * SUMMARY:
 *   The Intermediate Value Theorem (IVT) states that for a continuous
 *   function f on a closed interval [a, b], if k is any number between f(a)
 *   and f(b), then there exists at least one number c in [a, b] such that
 *   f(c) = k. This is considered a fundamental theorem in mathematical
 *   analysis and is therefore classified as a mountain.
 *
 * KEY AGENTS:
 *   - Powerless Student: Subject to the rule with no means of changing it.
 *   - Mathematics Institution: Entity that upholds and teaches the rule.
 *   - Analytical Observer: Entity that understands and observes the rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_value_theorem, 0.15).
domain_priors:suppression_score(intermediate_value_theorem, 0.05).
domain_priors:theater_ratio(intermediate_value_theorem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_value_theorem, extractiveness, 0.15).
narrative_ontology:constraint_metric(intermediate_value_theorem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(intermediate_value_theorem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(intermediate_value_theorem, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(intermediate_value_theorem, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_value_theorem, mountain).
narrative_ontology:human_readable(intermediate_value_theorem, "Accessibility Barrier to the Intermediate Value Theorem").
narrative_ontology:topic_domain(intermediate_value_theorem, "technological").

domain_priors:emerges_naturally(intermediate_value_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a student with limited mathematical background, the IVT appears as an unchangeable truth. It's a given in their textbook.
constraint_indexing:constraint_classification(intermediate_value_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% For a mathematics institution, the IVT is a fundamental theorem upon which much of calculus is built. It is an immutable truth.
constraint_indexing:constraint_classification(intermediate_value_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The IVT is a fundamental theorem in real analysis. It's a mountain.
constraint_indexing:constraint_classification(intermediate_value_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_value_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(intermediate_value_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_value_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(intermediate_value_theorem, ExtMetricName, E),
    domain_priors:suppression_score(intermediate_value_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(intermediate_value_theorem),
    narrative_ontology:constraint_metric(intermediate_value_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(intermediate_value_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(intermediate_value_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The IVT is a mathematical theorem, thus it is considered a mountain. The extractiveness and suppression are low as the theorem is a statement of existence, not a mandate for action. The theater ratio is low as the theorem is mostly used as is, not as a performance.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap, as all agents view the IVT as a fundamental truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is not applicable as there are no beneficiaries or victims. The theorem is simply a statement of existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling because the IVT is a fundamental theorem, not a coordination mechanism or extraction scheme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_value_theorem, 0, 100).

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
