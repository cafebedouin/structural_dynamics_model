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
    narrative_ontology:constraint_vindicates/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint represents a specific reading of the Monopoly rulebook,
 *   interpreting it as a 'mountain' that inevitably leads to wealth
 *   concentration and player elimination, thereby demonstrating the
 *   'pedagogical truth' of monopoly capitalism. This reading emphasizes the
 *   structural necessity of the outcome, rather than individual player skill
 *   or social modifications. The high extractiveness and suppression reflect
 *   the inherent design of the game, which this reading asserts is
 *   unchangeable within its own framework. This is one reading of the
 *   'monopoly_rulebook' kernel; sibling readings include
 *   'social_scaffold_reading' and 'tournament_orthodoxy_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.85).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.92).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Inevitable Wealth Concentration (Extraction Demo Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'ce055372-5e8c-459d-a507-1ab6f31aa5fb').
narrative_ontology:cs_kernel_codification('ce055372-5e8c-459d-a507-1ab6f31aa5fb', fixed_text).
narrative_ontology:cs_authority_grounding('ce055372-5e8c-459d-a507-1ab6f31aa5fb', lineage).
narrative_ontology:cs_reading_relation('ce055372-5e8c-459d-a507-1ab6f31aa5fb', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ce055372-5e8c-459d-a507-1ab6f31aa5fb', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('ce055372-5e8c-459d-a507-1ab6f31aa5fb', foundational, wealth_concentration_is_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_is_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('ce055372-5e8c-459d-a507-1ab6f31aa5fb', wealth_concentration_is_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('ce055372-5e8c-459d-a507-1ab6f31aa5fb', foundational, elimination_is_structurally_necessary).
narrative_ontology:cs_axiom_status(elimination_is_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ce055372-5e8c-459d-a507-1ab6f31aa5fb', elimination_is_structurally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('ce055372-5e8c-459d-a507-1ab6f31aa5fb', monopoly_as_economic_model).
narrative_ontology:cs_drift_state('ce055372-5e8c-459d-a507-1ab6f31aa5fb', contemporary_social_gaming_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ce055372-5e8c-459d-a507-1ab6f31aa5fb', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, dominant_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, game_theory_analysts).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_pedagogical_truth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The player who accumulates all wealth and eliminates opponents, directly benefiting from the rulebook's design. They see the rules as a natural mechanism for demonstrating economic truths.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, dominant_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Players who lose all their assets and are removed from the game. They bear the full cost of the rulebook's extractive design, experiencing the 'inevitable wealth concentration' firsthand.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Observers who use the game's outcomes to illustrate principles of rent extraction and wealth concentration in capitalist systems. They benefit from the rulebook's clarity in demonstrating these 'pedagogical truths'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_theory_analysts, beneficiary,
    analytical, generational, analytical, universal).

% Players who prefer a more cooperative, less extractive game experience, often introducing 'house rules' to mitigate the harshness of the official rulebook. From this reading's perspective, their attempts to soften the game obscure its fundamental lessons.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, social_players, excluded,
    moderate, biographical, constrained, local).

% Entities that enforce the official rulebook strictly for competitive integrity and ranking purposes. While they enforce the rules, this reading views their focus on 'skill' as missing the deeper, structural lesson of extraction.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, tournament_organizers, excluded,
    organized, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, deterministic framework for economic competition and resource allocation, leading to a single winner.
% TRANSFER_FUNCTION: Concentrates all in-game assets from all players into the hands of a single dominant player through a series of transactions and eliminations.
% ABSENT_VOICES: Players advocating for redistribution, cooperative play, or alternative economic models are excluded by the rulebook's fundamental design, which prioritizes accumulation and elimination. Their voices are present in 'house rules' but are dismissed as obscuring the game's core lesson.
% DISAPPEARANCE_RATIONALE: If this specific reading of the rulebook vanished, the physical rulebook would remain, and other readings (e.g., social scaffold, tournament orthodoxy) would persist. The 'world' of the game would continue, but the specific pedagogical interpretation of inevitable wealth concentration would cease to be applied.
% FOUNDING_PROBLEM: To create a clear, engaging simulation of economic competition that demonstrates the dynamics of wealth accumulation and monopoly formation.
% FOUNDING_PROBLEM_CORROBORATION: Game designers and economic educators attest that the game effectively models these dynamics, providing a tangible demonstration of wealth concentration. Critical theorists and social scientists also corroborate its effectiveness as a model, albeit with a different normative interpretation.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_unchanged).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness (0.85) is high because the rulebook's design ensures all wealth is concentrated in one player, with no inherent redistribution mechanisms. Suppression (0.92) is high because the rules are absolute and cannot be circumvented within the game's official framework; player elimination is a structural outcome. Theater ratio is low (0.05) as the game's function is directly aligned with its stated purpose in this reading. Accessibility collapse is high (0.95) because once the rules are understood, the inevitability of wealth concentration becomes clear, leaving no viable alternative within the game's structure. Resistance is low (0.1) because, within this reading, the rules are seen as a 'natural law' of the game, not something to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'extraction_demo_reading', the rulebook is a mountain, an unchangeable demonstration of economic principles. Other readings, such as the 'social_scaffold_reading' (which sees the rulebook as a 'tangled rope' requiring social modification) or the 'tournament_orthodoxy_reading' (which sees it as a 'rope' for skill-based competition), would experience the same rulebook very differently. This divergence is central to the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant player and game theory analysts are beneficiaries, as they either win or gain analytical insight. Eliminated players are clear victims, losing all assets. Social players and tournament organizers are 'excluded' in this reading because their interpretations (social play, skill-based competition) are seen as obscuring the fundamental extractive truth the rulebook embodies.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts the constraint's mandate (demonstrating wealth concentration) is perpetually 'live' and structurally necessary, thus precluding mandatrophy. The classification as a mountain, despite clear beneficiaries, triggers FSM evaluation, which is consistent with the claim that the 'natural law' of the game benefits specific parties without being 'constructed' in a way that could be easily changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_game,
    'Is the Monopoly rulebook a genuine ''natural law'' demonstrating inevitable economic principles, or a constructed game that merely simulates them?',
    'Comparative analysis with other economic simulations and real-world economic data: if the game''s outcomes are consistently replicated across diverse, structurally different simulations and real economies, it supports the ''natural law'' claim. If outcomes are highly sensitive to rule variations, it supports ''constructed''.',
    'If ''constructed'', the constraint would reclassify from mountain to snare, as its ''naturalness'' would be revealed as a cover for the extraction it enables. If ''natural law'', the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_game, conceptual, 'Ambiguity between the game''s rules as a ''natural law'' of economics versus a designed simulation.').

omega_variable(
    pedagogical_truth_vs_normative_critique,
    'Does the game''s outcome primarily serve as a ''pedagogical truth'' about capitalism, or as a normative critique of its extractive nature?',
    'Analysis of player and observer interpretations: if the majority of players/observers internalize the outcome as an ''inevitable truth'', it supports the pedagogical claim. If it primarily generates resistance and calls for change, it supports the critique.',
    'If primarily a critique, the ''vindicated_propositions'' would shift, and the ''beneficiary'' role of ''game_theory_analysts'' might be re-evaluated based on their specific analytical stance. The core classification might remain mountain, but its social function would be re-framed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_vs_normative_critique, preference, 'Ambiguity in the primary social function of the game''s outcome: teaching inevitability vs. inspiring critique.').

omega_variable(
    reading_structural_delta_social_scaffold,
    'What specific structural elements would change if the ''social_scaffold_reading'' were adopted, and how would that alter the constraint''s classification?',
    'Empirical observation of games played with ''house rules'' (e.g., free parking jackpot, no interest on loans): quantify changes in extractiveness, elimination rates, and game duration.',
    'The ''social_scaffold_reading'' would likely reclassify the constraint as a ''tangled_rope'' or ''scaffold'', with lower extractiveness and suppression, as social norms would actively mitigate the rulebook''s harshness. This reading forecloses the ''extraction_demo_reading'' by asserting the rulebook is not an immutable ''mountain'' but a malleable social construct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta_social_scaffold, empirical, 'Impact of the ''social_scaffold_reading'' on the constraint''s structure and classification.').

omega_variable(
    reading_structural_delta_tournament_orthodoxy,
    'What specific structural elements would change if the ''tournament_orthodoxy_reading'' were adopted, and how would that alter the constraint''s classification?',
    'Analysis of tournament play data: examine how strict adherence to rules (no house rules) affects player skill vs. structural inevitability in determining outcomes.',
    'The ''tournament_orthodoxy_reading'' would likely reclassify the constraint as a ''rope'', emphasizing fair competition and skill, with lower perceived extractiveness and higher perceived coordination. This reading coexists with the ''extraction_demo_reading'' in that both acknowledge the official rules, but differ on their primary interpretation of the outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta_tournament_orthodoxy, empirical, 'Impact of the ''tournament_orthodoxy_reading'' on the constraint''s structure and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
