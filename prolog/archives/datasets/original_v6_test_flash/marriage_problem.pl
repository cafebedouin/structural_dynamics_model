% ============================================================================
% CONSTRAINT STORY: marriage_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_problem, []).

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
 *   constraint_id: marriage_problem
 *   human_readable: The 37% Rule (Optimal Stopping Problem)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The 'Marriage Problem,' also known as the 'Secretary Problem,' is a
 *   classic example of the Optimal Stopping Problem. It demonstrates a
 *   mathematical constraint where an agent must choose the best candidate
 *   from a sequence of N options, each evaluated only once and in order. The
 *   optimal strategy involves rejecting the first 37% (1/e) of the candidates
 *   and then choosing the next candidate who is better than all previous
 *   ones. This constraint demonstrates a mathematical limit on
 *   decision-making under uncertainty.
 *
 * KEY AGENTS:
 *   - The Decision Maker: An agent trying to find the best option from a sequence of choices.
 *   - The Candidates: The sequence of options from which the decision maker chooses.
 *   - Analytical Observer: One who understands the mathematical proof.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_problem, 0.15).
domain_priors:suppression_score(marriage_problem, 0.05).
domain_priors:theater_ratio(marriage_problem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_problem, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_problem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(marriage_problem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_problem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(marriage_problem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_problem, mountain).
narrative_ontology:human_readable(marriage_problem, "The 37% Rule (Optimal Stopping Problem)").
narrative_ontology:topic_domain(marriage_problem, "mathematical/technological").

domain_priors:emerges_naturally(marriage_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The 37% rule is a provable mathematical theorem. Given the assumptions, the result is inescapable.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even a naive chooser is bound by the underlying math. The 37% rule represents an inherent limit to how one can make decisions in certain situations.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(marriage_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_problem, ExtMetricName, E),
    domain_priors:suppression_score(marriage_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_problem),
    narrative_ontology:constraint_metric(marriage_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a mathematical result, therefore extractiveness and suppression are low. The result emerges naturally from the assumptions of the problem. The 'marriage problem' is an inherent feature of sequential decision making.
 *
 * PERSPECTIVAL GAP:
 *   The 'perspectival gap' is minimal in this scenario because the constraint is a mathematical truth. All agents are, in theory, bound by the rule, although the direct experience might vary.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value is irrelevant since this is a mountain, which emerges naturally and cannot be changed. There is no beneficiary or victim. All agents are bound equally.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this scenario because this represents a mathematical constraint, and is not a social or political constraint subject to motivated manipulation or misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_problem, 0, 100).

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
