% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cobra_effect, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cobra_effect
 *   human_readable: The Cobra Effect (Perverse Bounty Incentive)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Cobra Effect is a classic example of a perverse incentive, where a
 *   policy designed to solve a problem inadvertently makes it worse. This
 *   constraint story examines the different perspectives on this phenomenon
 *   and the factors that contribute to its emergence. The incentive, designed
 *   to solve a problem, gets exploited creating a worse problem.
 *
 * KEY AGENTS:
 *   - The Government: The institution implementing the incentive (institutional/arbitrage).
 *   - The Policy Goals: The intended beneficiaries of the policy (powerless/trapped).
 *   - Individuals Exploiting Incentive: Those who benefit from the unintended consequences (moderate/mobile).
 *   - Taxpayers: bear the cost of the policy and its unintended consequences (moderate/constrained).
 *   - Analytical Observer: Evaluates the overall impact (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cobra_effect, 0.6).
domain_priors:suppression_score(cobra_effect, 0.7).
domain_priors:theater_ratio(cobra_effect, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cobra_effect, extractiveness, 0.6).
narrative_ontology:constraint_metric(cobra_effect, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cobra_effect, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cobra_effect, tangled_rope).
narrative_ontology:human_readable(cobra_effect, "The Cobra Effect (Perverse Bounty Incentive)").
narrative_ontology:topic_domain(cobra_effect, "economic/political").

domain_priors:requires_active_enforcement(cobra_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cobra_effect, individuals_exploiting_incentive).
narrative_ontology:constraint_victim(cobra_effect, policy_goals).
narrative_ontology:constraint_victim(cobra_effect, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The policy goals are trapped, unable to exit the perverse incentives. They are the primary target of the cobra effect.
constraint_indexing:constraint_classification(cobra_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The government intends the bounty to be a rope, solving a coordination problem. But the actual outcome can be far from it. They have arbitrage in theory, they can change policy, but they are often slow to react.
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The individuals who exploit the incentive benefit from it, but their actions undermine the intended policy goals. They are mobile, able to adapt to changing incentives.
constraint_indexing:constraint_classification(cobra_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% The analytical observer sees the mixed coordination and extraction: The bounty was supposed to coordinate actors to a specific goal, but created perverse incentives that enriched some at the cost of the policy goal.
constraint_indexing:constraint_classification(cobra_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cobra_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cobra_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The unintended consequences of the incentive create a significant extraction from the policy goals and taxpayers. Suppression (0.70): The perverse incentive is difficult to reverse once it is in place. The incentive creates incentives for further entrenchment. Theater Ratio (0.30): The stated goals are subverted by the actions of the exploiters. The government often finds it hard to react appropriately, leading to policy failures.
 *
 * PERSPECTIVAL GAP:
 *   The government (institutional/arbitrage) may initially perceive the bounty as a helpful policy, however, as the policy goals (powerless/trapped) are subverted, it becomes a snare. The exploiter benefits, and so it becomes a tangle rope from his perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is computed from the structural position of the actors. Victims are taxpayers and those trying to achieve the policy goal. Beneficiaries are those gaming the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_design_robustness,
    'How robust is the incentive design to unintended consequences and exploitation?',
    'Extensive modeling and testing of the incentive under various scenarios and actor behaviors.',
    'If robust, the cobra effect is less likely. If fragile, the cobra effect is highly probable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_design_robustness, empirical, 'The degree to which incentive design considers unintended behaviors.').

omega_variable(
    monitoring_and_feedback,
    'How effectively is the incentive monitored and adjusted based on feedback and observed outcomes?',
    'Real-time data collection and analysis, combined with mechanisms for policy adjustment.',
    'Effective monitoring can mitigate the cobra effect. Poor monitoring allows the cobra effect to persist and worsen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monitoring_and_feedback, empirical, 'The extent to which the incentive''s impact is monitored.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cobra_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobr_tr_t0, cobra_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cobr_tr_t5, cobra_effect, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cobr_tr_t10, cobra_effect, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cobr_be_t0, cobra_effect, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cobr_be_t5, cobra_effect, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cobr_be_t10, cobra_effect, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cobra_effect, resource_allocation).
narrative_ontology:affects_constraint(cobra_effect, goodhart_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
