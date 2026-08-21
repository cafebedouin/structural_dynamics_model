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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Monopoly Rulebook: Inevitable Wealth Concentration
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Monopoly rulebook,
 *   interpreting it as a 'mountain' that inevitably leads to wealth
 *   concentration and player elimination, thereby demonstrating a
 *   'pedagogical truth' about monopoly capitalism. The high extractiveness
 *   and suppression reflect the game's design to enforce these outcomes, with
 *   minimal 'theater' as its function is direct. The claimed type is
 *   'mountain' because, from this reading's perspective, the outcomes are
 *   structurally inevitable given the rules, akin to natural law within the
 *   game's system.
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
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Inevitable Wealth Concentration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '5c4ece0b-c632-4c26-925f-b42b95f9006e').
narrative_ontology:cs_kernel_codification('5c4ece0b-c632-4c26-925f-b42b95f9006e', fixed_text).
narrative_ontology:cs_authority_grounding('5c4ece0b-c632-4c26-925f-b42b95f9006e', lineage).
narrative_ontology:cs_reading_relation('5c4ece0b-c632-4c26-925f-b42b95f9006e', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('5c4ece0b-c632-4c26-925f-b42b95f9006e', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('5c4ece0b-c632-4c26-925f-b42b95f9006e', foundational, wealth_concentration_is_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_is_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('5c4ece0b-c632-4c26-925f-b42b95f9006e', wealth_concentration_is_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('5c4ece0b-c632-4c26-925f-b42b95f9006e', foundational, elimination_is_pedagogical_truth).
narrative_ontology:cs_axiom_status(elimination_is_pedagogical_truth, holdable).
narrative_ontology:cs_axiom_grounding('5c4ece0b-c632-4c26-925f-b42b95f9006e', elimination_is_pedagogical_truth, instrumental).
narrative_ontology:cs_reference_frame('5c4ece0b-c632-4c26-925f-b42b95f9006e', original_game_design_intent).
narrative_ontology:cs_drift_state('5c4ece0b-c632-4c26-925f-b42b95f9006e', contemporary_game_studies_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c4ece0b-c632-4c26-925f-b42b95f9006e', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, dominant_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, game_design_theorists).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, casual_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The player who accumulates all wealth and eliminates opponents, directly benefiting from the rulebook's design for wealth concentration. Their success is a direct outcome of the game's structure.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, dominant_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Players who lose all their assets and are removed from the game, bearing the full cost of the rulebook's extractive mechanisms. Their elimination is a structural necessity for the game's conclusion.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Academics and analysts who use the game as a pedagogical tool to demonstrate the 'inevitability' of wealth concentration and rent extraction in certain economic systems. They benefit from the clarity of the game's structural outcome.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_design_theorists, beneficiary,
    analytical, generational, analytical, universal).

% Players who engage with the game but are not committed to its 'pedagogical truth,' often seeking social interaction. They experience the game's extractive nature as frustrating but may not fully grasp its structural inevitability, leading to eventual elimination.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, casual_players, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates players into a zero-sum economic simulation, providing clear rules for property acquisition, rent collection, and player elimination, leading to a definitive winner.
% TRANSFER_FUNCTION: Transfers all player wealth, property, and ultimately, participation, from losing players to the dominant player through rent collection and bankruptcy mechanics.
% ABSENT_VOICES: Players seeking cooperative or non-eliminatory game experiences are structurally excluded by the rulebook's design. They would advocate for alternative rules that promote collaboration or mitigate wealth concentration, but their perspective is antithetical to the game's core 'pedagogical truth.'
% DISAPPEARANCE_RATIONALE: If the rulebook vanished, the game as a demonstration of 'monopoly capitalism's pedagogical truth' would cease to exist. The specific, inevitable wealth concentration and elimination dynamics would disappear, requiring a new framework to illustrate such concepts.
% FOUNDING_PROBLEM: To create a clear, deterministic model demonstrating the inherent mechanisms of wealth concentration and rent extraction within a capitalist system, leading to a single dominant entity.
% FOUNDING_PROBLEM_CORROBORATION: Game design theorists and economic educators corroborate that the rulebook effectively serves its founding purpose as a pedagogical tool for illustrating wealth concentration. The game's consistent outcomes across countless plays serve as empirical corroboration from outside the immediate players.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) because the game's core mechanism is the transfer of wealth from many to one, culminating in total elimination. Suppression is also very high (0.92) as the rules actively prevent alternative outcomes (e.g., redistribution, cooperative play) and enforce player bankruptcy. The theater ratio is low (0.05) because the game's stated purpose (demonstrating wealth concentration) is directly and efficiently achieved by its mechanics, leaving little room for performative maintenance. Accessibility collapse is near total (0.95) as the game's structure makes alternative outcomes almost impossible once the rules are understood. Resistance is low (0.10) because players, once engaged, are largely bound by the rules, and any 'resistance' (e.g., house rules) is seen as deviating from the 'true' demonstration.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this 'extraction_demo_reading,' the game's outcomes are an inevitable consequence of its 'natural laws,' making it a mountain. However, other readings (e.g., 'social_scaffold_reading') would view the same rulebook as a 'snare' or 'tangled_rope' that requires social intervention to be playable, highlighting the constructed nature of its 'inevitability.' The engine's classification will measure this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant player and game design theorists are beneficiaries, as they either win or gain analytical insight from the game's structure. Eliminated and casual players are victims, bearing the costs of wealth transfer and exclusion. The 'mountain' classification reflects the belief that these outcomes are inherent to the rulebook's design, not arbitrary choices.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the constraint's mandate (demonstrating wealth concentration) is perpetually 'live' and structurally necessary, thus precluding mandatrophy. The high extractiveness is not a sign of decay but of the constraint's core function. The classification prevents mislabeling this 'pedagogical truth' as mere extraction by emphasizing its claimed inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_design,
    'Is the wealth concentration and elimination inherent in the Monopoly rulebook a ''natural law'' within the game''s system, or a constructed outcome of specific design choices?',
    'Comparative analysis with alternative game designs (e.g., cooperative board games, games with redistribution mechanics) to determine if similar ''economic truths'' can be demonstrated without such extreme outcomes.',
    'If constructed, the ''mountain'' classification for this reading would be challenged, potentially reclassifying it as a ''snare'' or ''tangled_rope'' that actively enforces a specific, non-inevitable outcome. If truly inherent, the mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_design, conceptual, 'Ambiguity between inherent game mechanics and deliberate design choices.').

omega_variable(
    pedagogical_truth_vs_entertainment,
    'Is the primary function of the Monopoly rulebook to demonstrate a ''pedagogical truth'' about capitalism, or is it primarily a form of entertainment, with its outcomes being a side effect?',
    'Surveying player motivations and game designers'' stated intentions, alongside analysis of how the game is used in educational vs. recreational contexts.',
    'If primarily pedagogical, the ''mountain'' classification (as a demonstration of an ''inevitable'' truth) is strengthened. If primarily entertainment, the high extractiveness might be re-evaluated as a design flaw or a source of frustration, weakening the ''mountain'' claim and potentially shifting it towards a ''snare'' from the player''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_vs_entertainment, preference, 'Ambiguity regarding the game''s ultimate purpose and its implications for classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.05).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.84).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monopoly_rulebook' kernel. This 'extraction_demo_reading' emphasizes the rulebook's role in demonstrating inevitable wealth concentration and rent extraction, contrasting with the 'social_scaffold_reading' (community correction) and 'tournament_orthodoxy_reading' (strategic skill).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
