% ============================================================================
% CONSTRAINT STORY: roman_colosseum_games
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_colosseum_games, []).

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
 *   constraint_id: roman_colosseum_games
 *   human_readable: The Spectacle of the Roman Colosseum
 *   domain: political/social
 *
 * SUMMARY:
 *   The state-sponsored games of the Roman Colosseum ("bread and circuses")
 *   functioned as a system of social and political control. The games
 *   provided entertainment for the masses while simultaneously distracting
 *   them from political issues and reinforcing the power of the Roman state.
 *   The games also served as a means for the Roman elite to display their
 *   wealth and status.
 *
 * KEY AGENTS:
 *   - Roman Plebs: Primary target (powerless/trapped) - subject to social control and distraction.
 *   - Roman Elite: Primary beneficiary (institutional/arbitrage) - benefits from stability and control.
 *   - Roman State: Secondary beneficiary (institutional/constrained) - uses games for political control, but constrained by costs
 *   - Gladiators: Both Victim and Beneficiary (moderate/constrained) - subject to violence, but also gain fame and wealth.
 *   - Political Dissent: Victim (powerless/trapped) - suppressed by the distraction of the games.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_colosseum_games, 0.65).
domain_priors:suppression_score(roman_colosseum_games, 0.75).
domain_priors:theater_ratio(roman_colosseum_games, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_colosseum_games, extractiveness, 0.65).
narrative_ontology:constraint_metric(roman_colosseum_games, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(roman_colosseum_games, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_colosseum_games, snare).
narrative_ontology:human_readable(roman_colosseum_games, "The Spectacle of the Roman Colosseum").
narrative_ontology:topic_domain(roman_colosseum_games, "political/social").

domain_priors:requires_active_enforcement(roman_colosseum_games).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_colosseum_games, roman_elite).
narrative_ontology:constraint_beneficiary(roman_colosseum_games, roman_state).
narrative_ontology:constraint_victim(roman_colosseum_games, roman_plebs).
narrative_ontology:constraint_victim(roman_colosseum_games, political_dissent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Roman plebs, lacking political power and economic mobility, are trapped within the system and subject to its extractive pressures.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The Roman elite benefits from the stability and control afforded by the games, and are able to exploit the system for their own gain.
constraint_indexing:constraint_classification(roman_colosseum_games, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Roman state, while initially benefiting from the games, eventually becomes burdened by the cost and theatrical nature of maintaining the spectacle.
constraint_indexing:constraint_classification(roman_colosseum_games, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Gladiators are both victimized by the games and gain wealth and fame from them.
constraint_indexing:constraint_classification(roman_colosseum_games, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% From an analytical perspective, the Colosseum games represent a complex system of social control with both coordinating and extracting functions.
constraint_indexing:constraint_classification(roman_colosseum_games, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_colosseum_games_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_colosseum_games, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_colosseum_games, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_colosseum_games, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_colosseum_games, TR),
    TR >= 0.70.

:- end_tests(roman_colosseum_games_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): The games extracted labor (gladiators, construction), resources (animals, materials), and political capital (suppression of dissent) from the system. Suppression (0.75): The games actively suppressed political dissent by distracting the plebs and reinforcing state power. Theater Ratio (0.85): The games were largely performative, with spectacle taking precedence over genuine social benefit.
 *
 * PERSPECTIVAL GAP:
 *   The plebs, trapped and powerless, see the games as a snare. The elite, benefiting from the system, see it as a rope. The state, constrained by costs, sees it as a piton. Gladiators, in a mixed position, see tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The plebs have no exit option and bear the cost of the spectacle (violence, distraction). The elite benefit from social control and distraction. The state benefits but is also constrained by the cost of maintaining the games. Gladiators have some agency but are still subject to the violence of the arena.
 *
 * MANDATROPHY ANALYSIS:
 *   The games are properly classified as a snare due to the suppression of political dissent and the lack of genuine benefit to the plebs. While some coordination occurred (social cohesion, distribution of resources), the dominant function was extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_opinion_influence,
    'To what degree did the games genuinely placate the plebs vs. creating resentment?',
    'Historical analysis of plebeian uprisings correlated with periods of game frequency/grandeur.',
    'If placating: the games were a rope, facilitating coordination between the state and the people. If resentful: the games were primarily a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_opinion_influence, empirical, 'The degree to which the games influenced popular opinion').

omega_variable(
    true_cost_of_games,
    'What was the actual cost to the Roman state of providing games?',
    'Detailed economic analysis of game costs including animal procurement, gladiator training, and infrastructure maintenance.',
    'If low: the games were a rope for the state, providing social control at minimal cost. If high: the games were a piton, becoming a burdensome and unsustainable tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_games, empirical, 'The actual cost to the Roman state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_colosseum_games, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roma_tr_t0, roman_colosseum_games, theater_ratio, 0, 0.6).
narrative_ontology:measurement(roma_tr_t50, roman_colosseum_games, theater_ratio, 50, 0.8).
narrative_ontology:measurement(roma_tr_t100, roman_colosseum_games, theater_ratio, 100, 0.85).

% Extraction over time
narrative_ontology:measurement(roma_be_t0, roman_colosseum_games, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(roma_be_t50, roman_colosseum_games, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(roma_be_t100, roman_colosseum_games, base_extractiveness, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_colosseum_games, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
