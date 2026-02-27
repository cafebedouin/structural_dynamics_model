% ============================================================================
% CONSTRAINT STORY: minimax_decision_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_minimax_decision_rule, []).

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
 *   constraint_id: minimax_decision_rule
 *   human_readable: Minimax Decision Rule
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Minimax is a decision rule from game theory for minimizing the possible
 *   loss in a worst-case scenario. Because it represents a mathematical and
 *   logical necessity, it is best classified as a Mountain.
 *
 * KEY AGENTS:
 *   - Any agent: The rule applies to all agents in situations of uncertainty.
 *   - Analytical observer: Identifies minimax as an inherent feature of rational decision-making.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(minimax_decision_rule, 0.15).
domain_priors:suppression_score(minimax_decision_rule, 0.02).
domain_priors:theater_ratio(minimax_decision_rule, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(minimax_decision_rule, extractiveness, 0.15).
narrative_ontology:constraint_metric(minimax_decision_rule, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(minimax_decision_rule, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(minimax_decision_rule, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(minimax_decision_rule, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(minimax_decision_rule, mountain).
narrative_ontology:human_readable(minimax_decision_rule, "Minimax Decision Rule").
narrative_ontology:topic_domain(minimax_decision_rule, "technological/mathematical").

domain_priors:emerges_naturally(minimax_decision_rule).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The rule is a mathematical necessity under conditions of uncertainty.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% The rule provides a guaranteed lower bound on expected outcomes.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% Mathematical truth applies regardless of perspective. The decision rule provides mathematical lower bound to outcomes.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(minimax_decision_rule_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(minimax_decision_rule, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(minimax_decision_rule, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(minimax_decision_rule, ExtMetricName, E),
    domain_priors:suppression_score(minimax_decision_rule, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(minimax_decision_rule),
    narrative_ontology:constraint_metric(minimax_decision_rule, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(minimax_decision_rule, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(minimax_decision_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Minimax is a mathematical concept that provides a guaranteed lower bound on expected outcomes in decision-making under uncertainty. It's primarily a descriptive rather than prescriptive rule, making it a mountain. The extractiveness is very low, as implementing minimax often involves forgoing potentially higher gains to reduce maximum loss. Suppression is also low, as alternative decision rules exist, and minimax may not always be optimal.
 *
 * PERSPECTIVAL GAP:
 *   There is little perspectival difference, as the rule is a mathematical/logical necessity. Any actor, regardless of power or time horizon, will recognize the value of minimax in mitigating worst-case scenarios.
 *
 * DIRECTIONALITY LOGIC:
 *   Due to being a mountain, no beneficiaries or victims are defined, therefore the structural derivation chain is not applicable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(minimax_decision_rule, 0, 100).

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
