% ============================================================================
% CONSTRAINT STORY: riot_incentive_loop_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_riot_incentive_loop_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: riot_incentive_loop_2026
 *   human_readable: The Riot-Incentive Loop (State-Managed Chaos)
 *   domain: political/social/governance
 *
 * SUMMARY:
 *   This constraint models the feedback loop where state agencies deploy
 *   non-lethal chemical agents (tear gas) to incite panic and aggression,
 *   creating a self-fulfilling prophecy of riotous behavior. The initial
 *   intervention (deployment of chemical agents) is often framed as a
 *   necessary measure to maintain order, but it can inadvertently escalate
 *   tensions and lead to more extreme forms of protest. The cycle reinforces
 *   the need for more aggressive tactics, thus benefiting the state security
 *   apparatus at the expense of protestors and civil liberties.
 *
 * KEY AGENTS:
 *   - Protestors: Primary target (powerless/trapped) — bears costs of state violence and suppression of dissent.
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from increased budgets, powers, and social control justified by escalating conflict.
 *   - Community Leaders: Secondary actor (moderate/constrained) — constrained by state actions but also benefit from limited dialogue.
 *   - Civil Liberties Organizations: Monitor, report, and attempt to legally challenge state actions. Analytical observer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(riot_incentive_loop_2026, 0.6).
domain_priors:suppression_score(riot_incentive_loop_2026, 0.7).
domain_priors:theater_ratio(riot_incentive_loop_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(riot_incentive_loop_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(riot_incentive_loop_2026, tangled_rope).
narrative_ontology:human_readable(riot_incentive_loop_2026, "The Riot-Incentive Loop (State-Managed Chaos)").
narrative_ontology:topic_domain(riot_incentive_loop_2026, "political/social/governance").

domain_priors:requires_active_enforcement(riot_incentive_loop_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(riot_incentive_loop_2026, state_security_apparatus).
narrative_ontology:constraint_beneficiary(riot_incentive_loop_2026, private_security_contractors).
narrative_ontology:constraint_victim(riot_incentive_loop_2026, protestors).
narrative_ontology:constraint_victim(riot_incentive_loop_2026, civil_liberties).
narrative_ontology:constraint_victim(riot_incentive_loop_2026, community_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The protester, subjected to tear gas and other chemical agents, is trapped in a cycle of escalation. They lack the power to effectively resist or exit the situation immediately.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Community leaders are constrained by the state's actions but also benefit from limited dialogue, making this a form of rope with extraction.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The state security apparatus benefits from the escalated conflict by justifying increased budgets and powers, experiencing it as a coordinating mechanism to maintain control.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical perspective, recognizing that the loop creates both a control mechanism for the state and potential harm to civil society.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riot_incentive_loop_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(riot_incentive_loop_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(riot_incentive_loop_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The state extracts social order at the cost of civil liberties and individual well-being. The suppression factor (0.7) reflects the state's monopoly on the legitimate use of force and its ability to control narratives and suppress dissent. Theater ratio (0.3): Relatively low, since much of what happens on the ground is action rather than spectacle. Claimed type: Tangled rope, as state security is meant to provide coordination for its citizens, yet it leads to extraction via violent means.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge based on power and agency. The protester experiences a snare, while the state benefits, seeing it as a rope. The analytical observer views the tangled rope, acknowledging both the coordination and extraction effects.
 *
 * DIRECTIONALITY LOGIC:
 *   The State Security Apparatus experiences the constraint as a coordination mechanism. The protesters experience the constraint as pure extraction. The analytical observer sees a hybrid.
 *
 * MANDATROPHY ANALYSIS:
 *   Without understanding the perspectives, the state could assume its actions are fully justified. By labeling this as a Tangled Rope, it forces us to view both the intended coordination and potential extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_tear_gas,
    'What is the threshold at which chemical agents like tear gas cease to be a deterrent and become an incitement to riot?',
    'Empirical study of protest events, correlating chemical agent use with protest escalation and de-escalation.',
    'Determines whether the tactic is net coordination or net extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_tear_gas, empirical, 'Effectiveness of tear gas at controlling or inciting riots.').

omega_variable(
    transparency_accountability,
    'To what degree are state agencies accountable for the use of chemical agents during protests?',
    'Legal analysis of oversight mechanisms, independent investigations of riot control tactics.',
    'Determines the extent of the snare (pure extraction) component of the loop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_accountability, conceptual, 'Accountability of state agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riot_incentive_loop_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(riot_tr_t0, riot_incentive_loop_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(riot_tr_t5, riot_incentive_loop_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(riot_tr_t10, riot_incentive_loop_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(riot_be_t0, riot_incentive_loop_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(riot_be_t5, riot_incentive_loop_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(riot_be_t10, riot_incentive_loop_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
