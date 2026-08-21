% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__social_scaffold_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly House Rules as Social Scaffold
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the widespread adoption and enforcement of
 *   'house rules' in casual Monopoly games, which deviate from the official
 *   rulebook. These house rules (e.g., free parking jackpot, no auctions,
 *   money for landing on Go) function as a social scaffold, making the game
 *   more palatable and prolonging play, thereby preserving social cohesion
 *   among players. The constraint is a reading of the 'monopoly_rulebook'
 *   kernel, focusing on its social function rather than strict adherence or
 *   economic simulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.45).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.6).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly House Rules as Social Scaffold").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '745ec0f6-4a8f-414d-ac83-b85d83d728a6').
narrative_ontology:cs_kernel_codification('745ec0f6-4a8f-414d-ac83-b85d83d728a6', fixed_text).
narrative_ontology:cs_authority_grounding('745ec0f6-4a8f-414d-ac83-b85d83d728a6', practice).
narrative_ontology:cs_interpretation_layer_present('745ec0f6-4a8f-414d-ac83-b85d83d728a6').
narrative_ontology:cs_reading_relation('745ec0f6-4a8f-414d-ac83-b85d83d728a6', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('745ec0f6-4a8f-414d-ac83-b85d83d728a6', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('745ec0f6-4a8f-414d-ac83-b85d83d728a6', foundational, game_serves_social_cohesion).
narrative_ontology:cs_axiom_status(game_serves_social_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('745ec0f6-4a8f-414d-ac83-b85d83d728a6', game_serves_social_cohesion, conventional).
narrative_ontology:cs_axiom('745ec0f6-4a8f-414d-ac83-b85d83d728a6', foundational, rules_are_flexible_social_contracts).
narrative_ontology:cs_axiom_status(rules_are_flexible_social_contracts, holdable).
narrative_ontology:cs_axiom_grounding('745ec0f6-4a8f-414d-ac83-b85d83d728a6', rules_are_flexible_social_contracts, conventional).
narrative_ontology:cs_reference_frame('745ec0f6-4a8f-414d-ac83-b85d83d728a6', socially_playable_game).
narrative_ontology:cs_drift_state('745ec0f6-4a8f-414d-ac83-b85d83d728a6', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('745ec0f6-4a8f-414d-ac83-b85d83d728a6', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_members).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, players_seeking_quick_resolution).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, official_rulebook_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective of players who agree upon and enforce the house rules. They benefit from a more enjoyable, less confrontational, and longer-lasting game experience, preserving social cohesion. Their exit is constrained by the desire to play together.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_members, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, social_group_members, beneficiary).

% Individuals within the social group who might prefer the faster, more decisive outcomes of the official rules. They 'pay' by enduring longer game sessions and less strategic rigor, but their desire to participate in the social activity constrains their exit.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, players_seeking_quick_resolution, payer,
    moderate, immediate, constrained, local).

% The abstract concept of strict adherence to the published rules. It 'pays' by being consistently overridden or ignored in favor of house rules, losing its intended authority and impact on gameplay. It has no agency to resist.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, official_rulebook_fidelity, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, official_rulebook_fidelity).

% The entity that publishes the official Monopoly rulebook. They observe the widespread adoption of house rules but have no direct power or incentive to enforce strict adherence in private social settings.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, monopoly_publisher, observer,
    institutional, generational, analytical, global).

% Groups or individuals who organize competitive Monopoly play strictly by the official rules. They would object to house rules as undermining competitive integrity but are not present in the casual social setting where these house rules are applied.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_organizers, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable sustained, enjoyable social interaction and group cohesion around the game of Monopoly by mitigating the harsh, player-eliminating, and socially divisive aspects of the official rulebook.
% TRANSFER_FUNCTION: Transfers game longevity, social harmony, and continued participation among players, at the cost of strict adherence to the official rules, strategic depth, and potentially longer game duration.
% ABSENT_VOICES: Tournament players, competitive strategists, or purists who believe the game should be played strictly by the official rules to test skill and strategy. They are excluded from the casual social setting where house rules are adopted.
% DISAPPEARANCE_RATIONALE: If the house rules and their social enforcement vanished overnight, the social group would likely stop playing Monopoly together, or play much less frequently, due to the rapid elimination, boredom, and social friction caused by the official rules. The game's social function would collapse.
% FOUNDING_PROBLEM: The official Monopoly rules lead to rapid player elimination, long periods of inactivity for eliminated players, and a harsh, socially unpalatable endgame, which undermines the game's function as a shared social activity.
% FOUNDING_PROBLEM_CORROBORATION: Anecdotal evidence from countless family game nights, board game forums, and social science observations of game dynamics consistently corroborates the social problems arising from strict adherence to official Monopoly rules. This is attested by players across diverse social groups, not just the beneficiaries of house rules.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).
:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the 'cost' of deviating from the official rules, such as reduced strategic depth or prolonged game duration, which some players might find extractive. Suppression (0.60) is moderate, representing the social pressure within the group to conform to the house rules, which actively suppresses attempts to play strictly by the book. The theater ratio (0.40) indicates that while the official rulebook is present, a significant portion of the actual gameplay is performative adherence to a text that is largely ignored in practice. The scaffold type is appropriate as the house rules provide temporary support for social play, meant to be transitional in the sense that they adapt to the group's social needs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the social group, the house rules are a necessary and beneficial adaptation, a 'rope' or 'scaffold' for social play. From the perspective of a purist or tournament player, these same house rules would be seen as a 'snare' or 'piton' that degrades the game's integrity and strategic challenge. This story authors the 'social scaffold' reading, acknowledging the other perspectives through omegas and excluded stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The social group members are primary beneficiaries and agenda-setters, as they collectively establish and benefit from the house rules (low d). Players seeking quick resolution are payers, as they bear the cost of a longer, less decisive game (higher d). The 'official_rulebook_fidelity' is a conceptual victim, as its authority is undermined. Tournament organizers are excluded, as their strict adherence to rules is incompatible with this social reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_strategic_value,
    'Is the primary value of playing Monopoly derived from social interaction and cohesion, or from strategic challenge and competitive outcome?',
    'Empirical studies of player motivation and satisfaction in different play contexts (casual vs. tournament).',
    'If social value dominates, the scaffold classification is strongly supported. If strategic value dominates, the house rules might be reclassified as a snare for those seeking genuine competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_strategic_value, conceptual, 'Ambiguity in the game''s core purpose.').

omega_variable(
    rulebook_reading_divergence,
    'How would the classification change if the ''extraction_demo_reading'' or ''tournament_orthodoxy_reading'' of the Monopoly rulebook were adopted?',
    'Analyzing the structural properties (extraction, suppression, beneficiaries, victims) of each sibling reading as separate constraints.',
    'The ''extraction_demo_reading'' would likely classify as a Snare (high extraction, clear victims). The ''tournament_orthodoxy_reading'' would likely classify as a Rope or Mountain (low extraction, high fidelity, coordination around skill).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rulebook_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__social_scaffold_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__social_scaffold_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__social_scaffold_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__social_scaffold_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
