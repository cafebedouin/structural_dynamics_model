% ============================================================================
% CONSTRAINT STORY: monopoly_fp_house_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_fp_house_rule, []).

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
 *   constraint_id: monopoly_fp_house_rule
 *   human_readable: Monopoly 'Free Parking' House Rule
 *   domain: social/economic
 *
 * SUMMARY:
 *   This constraint is the common house rule in the game Monopoly where all
 *   fines and taxes are placed in the center of the board and collected by
 *   the player who lands on the "Free Parking" space. It is a departure from
 *   the original rules of the game.
 *
 * KEY AGENTS:
 *   - unlucky_players: primary victims (powerless/trapped) - those who pay the taxes.
 *   - lucky_player: primary beneficiary (powerful/arbitrage) - the one who lands on free parking and collects the funds.
 *   - game_designers: moderate perspective (moderate/constrained) - those who designed the game
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_fp_house_rule, 0.5).
domain_priors:suppression_score(monopoly_fp_house_rule, 0.7).
domain_priors:theater_ratio(monopoly_fp_house_rule, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_fp_house_rule, extractiveness, 0.5).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_fp_house_rule, tangled_rope).
narrative_ontology:human_readable(monopoly_fp_house_rule, "Monopoly 'Free Parking' House Rule").
narrative_ontology:topic_domain(monopoly_fp_house_rule, "social/economic").

domain_priors:requires_active_enforcement(monopoly_fp_house_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, lucky_player).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, unlucky_players).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The unlucky player who lands on a tax or chance/community chest that requires payment is forced to contribute to the free parking pot, with no chance of getting the money back unless they are very lucky. The cost is immediate.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The lucky player who happens to land on free parking gets the whole pot. Their benefit is immediate and relies on others' misfortune.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% The game designers intended for the bank to receive all the fees but find that players want to get those fees. It adds another element of the game to be excited about.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, this house rule acts as a snare, concentrating wealth (fines) and distributing it randomly to a single player.  The 'lottery' of free parking creates a distortion in the game's intended economic flows.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% From the perspective of the rule makers, the rule adds an element of excitement to the game.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_fp_house_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_fp_house_rule, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monopoly_fp_house_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because the rule concentrates the penalties in the Free Parking space. This money is supposed to go to the bank. The suppression is high because people want to get the money when they land on it. The designers intended to have this money go to the bank, so this suppression goes against the intention of the design. The theater ratio is low since there is not much performance associated with the rule.
 *
 * PERSPECTIVAL GAP:
 *   The unlucky player sees a snare since they are penalized, and the lucky player sees a rope since they gain. The designer sees the impact on the game and is constrained by the will of the player.
 *
 * DIRECTIONALITY LOGIC:
 *   The unlucky players are victims with no exit options and thus experience high extraction. The lucky player is a beneficiary with a clear arbitrage opportunity (collecting the pot). The game designers are moderate, with some influence over the game's rules but ultimately constrained by player preferences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_fairness,
    'Does the free parking house rule make the game more or less economically fair?',
    'Analyze the long-term distribution of wealth with and without the house rule.  Simulate many games.',
    'If it makes it less fair, it reinforces inequality. If it makes it more fair, it provides a slight redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_fairness, empirical, 'The impact of the rule on the economic fairness of the game.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_fp_house_rule, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_fp_house_rule, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t5, monopoly_fp_house_rule, theater_ratio, 5, 0.2).
narrative_ontology:measurement(mono_tr_t10, monopoly_fp_house_rule, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_fp_house_rule, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mono_be_t5, monopoly_fp_house_rule, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mono_be_t10, monopoly_fp_house_rule, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
