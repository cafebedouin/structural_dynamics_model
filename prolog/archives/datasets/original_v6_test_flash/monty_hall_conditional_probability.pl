% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monty_hall_conditional_probability, []).

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
 *   constraint_id: monty_hall_conditional_probability
 *   human_readable: The Monty Hall Problem (Conditional Probability)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Monty Hall problem is a counter-intuitive probability puzzle that
 *   demonstrates how new information (a host revealing a non-winning option)
 *   alters the probability space. Despite its simple setup, it often leads to
 *   heated debate and illustrates the difficulty people have with conditional
 *   probability. The optimal strategy is always to switch doors, doubling
 *   your chances of winning.
 *
 * KEY AGENTS:
 *   - The Uninformed Player: Initially makes a random choice (powerless/trapped).
 *   - The Analytical Observer: Understands conditional probability (analytical/analytical).
 *   - The Mathematical Community: Holds the established solution (institutional/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monty_hall_conditional_probability, 0.1).
domain_priors:suppression_score(monty_hall_conditional_probability, 0.02).
domain_priors:theater_ratio(monty_hall_conditional_probability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, 0.1).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monty_hall_conditional_probability, mountain).
narrative_ontology:human_readable(monty_hall_conditional_probability, "The Monty Hall Problem (Conditional Probability)").
narrative_ontology:topic_domain(monty_hall_conditional_probability, "mathematical").

domain_priors:emerges_naturally(monty_hall_conditional_probability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a player who does not understand conditional probability, the problem appears intractable. However, the underlying mathematical principle remains a fixed constraint.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% An analytical observer, understanding conditional probability, recognizes the problem as a fixed mathematical truth. Switching doors doubles the probability of winning.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The mathematical community views the Monty Hall problem as a solved problem, a fixed truth within the domain of probability theory.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monty_hall_conditional_probability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, ExtMetricName, E),
    domain_priors:suppression_score(monty_hall_conditional_probability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monty_hall_conditional_probability),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monty_hall_conditional_probability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Very low, as it is a mathematical problem and not a social or economic constraint. Suppression: Very low, the mathematical truth is accessible, but understanding may be suppressed by cognitive biases. Theater Ratio: Very low, there is minimal performative activity associated with the problem.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who understand conditional probability and those who do not. The uninformed player perceives randomness, while the informed player sees a clear advantage in switching.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is essentially neutral as the problem is mathematical. However, those who incorrectly believe there is no advantage to switching are 'victims' of their misunderstanding.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not relevant as the Monty Hall problem is a fixed mathematical truth, not a social or economic system that could degrade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monty_hall_conditional_probability, 0, 100).

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
