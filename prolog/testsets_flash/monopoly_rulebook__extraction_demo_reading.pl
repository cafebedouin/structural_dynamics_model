% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__extraction_demo_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook: Inevitable Wealth Concentration (Extraction Demo Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'extraction demo' reading of the Monopoly
 *   rulebook, where the game's mechanics are interpreted as an inevitable,
 *   structurally necessary demonstration of wealth concentration and player
 *   elimination inherent to monopoly capitalism. This reading asserts the
 *   rulebook's design as a 'mountain' of economic truth, with high
 *   extractiveness and suppression, and minimal theatricality. The game's
 *   duration (60-90 min) and winner-takes-all outcome are seen as integral to
 *   this pedagogical truth. This is one reading of the 'monopoly_rulebook'
 *   kernel, distinct from 'social_scaffold_reading' and
 *   'tournament_orthodoxy_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.85).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.9).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Inevitable Wealth Concentration (Extraction Demo Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '4c350a79-322c-41ba-ba4e-ea25c0f77ed1').
narrative_ontology:cs_kernel_codification('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', fixed_text).
narrative_ontology:cs_authority_grounding('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', lineage).
narrative_ontology:cs_interpretation_layer_present('4c350a79-322c-41ba-ba4e-ea25c0f77ed1').
narrative_ontology:cs_reading_relation('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', foundational, wealth_concentration_is_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_is_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', wealth_concentration_is_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', foundational, elimination_is_pedagogically_necessary).
narrative_ontology:cs_axiom_status(elimination_is_pedagogically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', elimination_is_pedagogically_necessary, instrumental).
narrative_ontology:cs_reference_frame('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', original_monopoly_rulebook_design).
narrative_ontology:cs_drift_state('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', contemporary_social_gaming_culture, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4c350a79-322c-41ba-ba4e-ea25c0f77ed1', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, dominant_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, economic_theorists_of_capitalism).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, struggling_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The player who accumulates all wealth and eliminates opponents, directly benefiting from the rulebook's design. Views the outcome as a natural consequence of the game's structure.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, dominant_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Players who lose all their assets and are forced out of the game. They bear the full cost of the game's extractive mechanics, with no recourse within the rules.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Players who are still in the game but are consistently losing ground, paying rents, and facing eventual elimination. Their options are to continue playing at a disadvantage or concede.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, struggling_players, payer,
    moderate, immediate, constrained, local).

% Academics and analysts who use the game's outcomes as a pedagogical tool or empirical demonstration of the inherent tendencies of monopoly capitalism, validating their theoretical frameworks.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, economic_theorists_of_capitalism, beneficiary,
    analytical, generational, analytical, universal).

% The creators of the game and its rulebook, who established the mechanics that lead to wealth concentration and player elimination. They maintain the rules as fundamental to the game's identity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_designers, agenda_setter,
    institutional, generational, mobile, global).

% Players who would prefer a more equitable game experience, with mechanisms to prevent total elimination or redistribute wealth. Their preferences are not accommodated by the strict rulebook, leading them to seek 'house rules' or alternative games.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, social_players_seeking_equity, excluded,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook provides a clear, unambiguous framework for competitive play, defining turns, transactions, and victory conditions, allowing multiple players to interact within a shared, predictable system.
% TRANSFER_FUNCTION: Transfers all wealth from losing players to the dominant player through rent payments, property acquisition, and bankruptcy mechanics, culminating in the elimination of all but one player.
% ABSENT_VOICES: Social players seeking equity and those who advocate for cooperative or less punitive game structures are absent from the rulebook's design and its strict interpretation. They would argue for modifications to prevent elimination and promote continued engagement.
% DISAPPEARANCE_RATIONALE: If the rulebook vanished, the game as a structured competitive activity would cease to exist. Players would be unable to coordinate actions, and the pedagogical demonstration of wealth concentration would be lost. Any new game would require a new set of rules.
% FOUNDING_PROBLEM: To create a board game that simulates economic competition and demonstrates the dynamics of property acquisition, rent extraction, and monopoly formation, culminating in a single winner.
% FOUNDING_PROBLEM_CORROBORATION: Game designers and economic theorists attest that the game continues to serve its original purpose as a simulation and pedagogical tool for understanding capitalist dynamics. The game's enduring popularity and its use in educational contexts corroborate this, even as social players contest its harshness.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, ExtMetricName, E),
    domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the game's core design is to transfer all wealth to one player, eliminating others. Suppression (0.9) is high because the rules offer no escape from this process; players are 'trapped' by the game's logic once committed. Theater ratio (0.05) is low because the game directly performs its stated function without significant performative overhead. Accessibility collapse (0.95) is high as alternatives to the game's core mechanics are almost entirely foreclosed within the rulebook itself. Resistance (0.1) is low within this reading, as the outcome is seen as inevitable, though external resistance (house rules) exists outside this strict interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dominant player and economic theorists, the rulebook is a 'mountain' demonstrating natural economic laws. From the perspective of eliminated or struggling players, it is a 'snare' of unavoidable extraction. This reading emphasizes the 'mountain' aspect, viewing the extraction as an inherent, unchangeable feature of the system being modeled.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant player and economic theorists are beneficiaries, as the constraint directly enables their success or validates their theories. Eliminated and struggling players are clear victims, bearing the full cost. Game designers are agenda-setters, having created the system. Social players seeking equity are excluded, as their desired modifications are outside the strict interpretation of the rulebook.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects the idea of mandatrophy. The 'mandate' of demonstrating wealth concentration is considered perpetually 'live' as long as monopoly capitalism exists. The constraint's persistence is not due to inertia but to its ongoing pedagogical value and its perceived structural truth. Any attempt to 'fix' or 'resolve' it would be seen as undermining its core purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_game,
    'Is the wealth concentration demonstrated by the Monopoly rulebook a genuine ''natural law'' of economics, or is it a constructed outcome of specific game design choices?',
    'Comparative analysis with other economic simulation games that employ different rule sets and yield different wealth distribution outcomes. If alternative designs produce different results, it suggests constructability.',
    'If constructed, the ''mountain'' claim for this reading is undermined, and the constraint would reclassify towards a ''snare'' or ''tangled_rope'' from the perspective of victims, as the extraction would be seen as a choice, not an inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_game, conceptual, 'Ambiguity between inherent economic truth and game design choice.').

omega_variable(
    reading_as_pedagogical_tool_vs_reality,
    'To what extent does this ''extraction demo'' reading accurately reflect the full complexity of real-world monopoly capitalism, or is it an oversimplified pedagogical model?',
    'Detailed comparison of game mechanics with empirical data and advanced economic models of real-world markets. Discrepancies would highlight the model''s limitations.',
    'If significantly oversimplified, the ''universal'' scope and ''emerges_naturally'' claims would be weakened, potentially shifting the ''economic_theorists_of_capitalism'' from beneficiary to a more ''observer'' role, and reducing the perceived ''mountain'' quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_pedagogical_tool_vs_reality, empirical, 'Accuracy of the game as a model for real-world economic systems.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.9) structural (the rules themselves) or internalized (players accepting the game''s premise)?',
    'Post-game player interviews: if players express a belief in the inevitability of their loss even when house rules are available, it suggests internalized suppression. If they only feel constrained by the rules, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — players carry the suppression with them after exit, reinforcing the ''mountain'' claim for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t1935, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1935, 0.05).
narrative_ontology:measurement(mono_tr_t1960, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(mono_tr_t1985, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(mono_tr_t2000, monopoly_rulebook__extraction_demo_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(mono_tr_t2010, monopoly_rulebook__extraction_demo_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(mono_tr_t2024, monopoly_rulebook__extraction_demo_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t1935, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1935, 0.85).
narrative_ontology:measurement(mono_be_t1960, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(mono_be_t1985, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1985, 0.85).
narrative_ontology:measurement(mono_be_t2000, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(mono_be_t2010, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(mono_be_t2024, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t1935, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1935, 0.9).
narrative_ontology:measurement(mono_su_t1960, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(mono_su_t1985, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(mono_su_t2000, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(mono_su_t2010, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(mono_su_t2024, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
