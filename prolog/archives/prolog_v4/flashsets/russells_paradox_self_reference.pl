% ============================================================================
% CONSTRAINT STORY: russells_paradox_self_reference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russells_paradox_self_reference, []).

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
 *   constraint_id: russells_paradox_self_reference
 *   human_readable: Russell's Paradox (Naive Set Theory Collapse)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Russell's Paradox arises from considering the set of all sets that do not
 *   contain themselves. This leads to a logical contradiction within naive
 *   set theory, demonstrating a fundamental limitation of the system. The
 *   paradox reveals that unrestricted set comprehension leads to
 *   inconsistencies.
 *
 * KEY AGENTS:
 *   - The Logician: Analytical observer who understands the paradox's implications.
 *   - The Novice Student: Represents anyone encountering the paradox, regardless of their mathematical sophistication.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russells_paradox_self_reference, 0.0).
domain_priors:suppression_score(russells_paradox_self_reference, 0.0).
domain_priors:theater_ratio(russells_paradox_self_reference, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, 0.0).
narrative_ontology:constraint_metric(russells_paradox_self_reference, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(russells_paradox_self_reference, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russells_paradox_self_reference, mountain).
narrative_ontology:human_readable(russells_paradox_self_reference, "Russell's Paradox (Naive Set Theory Collapse)").
narrative_ontology:topic_domain(russells_paradox_self_reference, "mathematical/logical").

domain_priors:emerges_naturally(russells_paradox_self_reference).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The paradox is a fundamental limit of naive set theory, regardless of any agent's preferences or power.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even a novice student cannot escape the logical contradiction. Their powerlessness does not alter the inherent limit.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russells_paradox_self_reference_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, ExtMetricName, E),
    domain_priors:suppression_score(russells_paradox_self_reference, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(russells_paradox_self_reference),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(russells_paradox_self_reference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both 0 as the paradox is a logical limit, not a coercive constraint. Theater ratio is 0 as there's no performative element. Accessibility collapse is high, and resistance low, because the paradox is readily derived once the foundational assumptions of naive set theory are understood. The paradox emerges naturally from these assumptions.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap, as the paradox is a universal logical truth. All observers, regardless of their power, time horizon, exit options, or spatial scope, will encounter the same contradiction within naive set theory.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the paradox is a logical limit and not a social or economic constraint, there are no beneficiaries or victims. The directionality is neutral, and not relevant to the classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Mountain because it represents a fundamental logical limitation. There is no possibility of misclassifying a coordination mechanism as pure extraction or vice versa. The paradox reveals a limit of the system, not an extraction from any agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russells_paradox_self_reference, 0, 1).

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
