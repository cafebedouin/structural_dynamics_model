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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly Rulebook (Social Scaffold Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the Monopoly rulebook as a social scaffold,
 *   where house rules and implicit social agreements (like ending the game
 *   before total elimination) are crucial for maintaining group cohesion and
 *   playability. The official rules, if followed strictly, often lead to
 *   harsh, socially unpalatable outcomes. This reading emphasizes the role of
 *   community correction in injecting 'liquidity' (e.g., free parking money)
 *   and slowing elimination to preserve coordination and enjoyment among
 *   casual players. This is one reading of the 'monopoly_rulebook' kernel,
 *   distinct from readings that emphasize pure extraction or competitive
 *   orthodoxy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.45).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.2).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook (Social Scaffold Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '9d55a69b-6980-4600-901b-d4f9a2a365f5').
narrative_ontology:cs_kernel_codification('9d55a69b-6980-4600-901b-d4f9a2a365f5', fixed_text).
narrative_ontology:cs_authority_grounding('9d55a69b-6980-4600-901b-d4f9a2a365f5', practice).
narrative_ontology:cs_interpretation_layer_present('9d55a69b-6980-4600-901b-d4f9a2a365f5').
narrative_ontology:cs_reading_relation('9d55a69b-6980-4600-901b-d4f9a2a365f5', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d55a69b-6980-4600-901b-d4f9a2a365f5', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('9d55a69b-6980-4600-901b-d4f9a2a365f5', foundational, social_cohesion_over_textual_fidelity).
narrative_ontology:cs_axiom_status(social_cohesion_over_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('9d55a69b-6980-4600-901b-d4f9a2a365f5', social_cohesion_over_textual_fidelity, conventional).
narrative_ontology:cs_axiom('9d55a69b-6980-4600-901b-d4f9a2a365f5', secondary, game_duration_for_enjoyment).
narrative_ontology:cs_axiom_status(game_duration_for_enjoyment, holdable).
narrative_ontology:cs_axiom_grounding('9d55a69b-6980-4600-901b-d4f9a2a365f5', game_duration_for_enjoyment, instrumental).
narrative_ontology:cs_reference_frame('9d55a69b-6980-4600-901b-d4f9a2a365f5', socially_playable_game).
narrative_ontology:cs_drift_state('9d55a69b-6980-4600-901b-d4f9a2a365f5', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9d55a69b-6980-4600-901b-d4f9a2a365f5', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, competitive_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek an enjoyable social experience from Monopoly, prioritizing fun and interaction over strict adherence to rules or competitive victory. They benefit from house rules that prolong the game and mitigate harsh eliminations.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% The overall health and stability of the social group playing the game. This 'agent' benefits from rules that prevent conflict and maintain positive relationships, even if it means bending the official rules.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_cohesion, beneficiary,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).

% The literal text of the Monopoly rulebook, which mandates strict adherence to its rules, often leading to player elimination and wealth concentration. Its 'authority' is diminished by the widespread adoption of house rules.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, official_rulebook, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, official_rulebook).

% Prefer to play by the strict official rules, viewing house rules as diluting the strategic challenge and competitive integrity of the game. They 'pay' by having their preferred mode of play undermined by social pressure.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_players, payer,
    moderate, biographical, constrained, local).

% Observe how players actually interact with the game, noting the prevalence and function of house rules. Their analytical seat allows them to understand the gap between intended design and actual social play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_designers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction and enjoyment among players by mitigating the harsh, eliminative aspects of the official Monopoly rules, ensuring games last longer and remain fun for more participants.
% TRANSFER_FUNCTION: Transfers 'liquidity' (e.g., money from Free Parking) and 'time' (by slowing elimination) from the strict, zero-sum logic of the official rules to the social experience of the players, preserving group harmony.
% ABSENT_VOICES: Strict rule-followers and competitive players, who would argue for fidelity to the official text, are often socially pressured into accepting house rules to maintain group harmony. Their 'voice' is present but often overridden by the social scaffold.
% DISAPPEARANCE_RATIONALE: If the social scaffold (house rules, implicit agreements) disappeared, Monopoly games would become significantly shorter, harsher, and less enjoyable for casual players, leading to a decline in its use as a social activity and potentially fracturing social groups that rely on it for casual interaction.
% FOUNDING_PROBLEM: The original Monopoly rules, while designed to demonstrate the evils of monopolies, often led to player elimination and social friction, making the game unplayable or unenjoyable for many social groups.
% FOUNDING_PROBLEM_CORROBORATION: Numerous sociological studies of board game play, anecdotal evidence from player communities, and observations by game designers (outside the direct beneficiaries of the house rules) corroborate that the strict rules often create social problems that house rules mitigate, indicating the founding problem is still live.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate, reflecting the inherent zero-sum nature of Monopoly, but mitigated by house rules that redistribute wealth and prolong play. Suppression (0.20) is low because adherence to strict rules is often voluntary and easily overridden by social consensus. Theater ratio (0.10) is low as the house rules are genuinely functional in achieving social playability, not merely performative. The 'scaffold' classification is based on the idea that the house rules provide temporary support to make the game socially viable, with an implicit 'sunset clause' (ending the game before total elimination) to preserve relationships.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of casual players and social groups, the house rules are essential for the game's function as a social activity. From the perspective of a strict rules-adherent or a competitive player, these house rules might be seen as undermining the game's integrity or competitive balance. The engine's classification will reflect the structural reality of the social scaffold, which prioritizes group cohesion over strict adherence to the text.
 *
 * DIRECTIONALITY LOGIC:
 *   Social group cohesion and casual players are the primary beneficiaries, as the house rules prevent the game from becoming too harsh or ending too quickly, thus preserving the social activity. The 'official_rulebook' itself, if considered an agent, would be a target, as its strictures are softened. Competitive players might be considered payers, as the 'social scaffold' dilutes the strict competitive challenge.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine social scaffold, or is it an extraction mechanism (extraction_demo_reading) or a rigid competitive framework (tournament_orthodoxy_reading)?',
    'Empirical observation of game sessions: frequency of house rule application, player sentiment regarding elimination, and actual game duration. If house rules are rarely applied and elimination is frequent, it leans towards extraction_demo or tournament_orthodoxy. If house rules are consistently applied to prolong play and maintain social cohesion, this reading is corroborated.',
    'If resolved as extraction_demo, the constraint''s extractiveness is higher, and it reclassifies as a Snare. If resolved as tournament_orthodoxy, the constraint''s suppression is higher, and it reclassifies as a Rope (for competitive coordination) or even a Mountain (for immutable rules).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between social scaffold, extraction demonstration, and competitive orthodoxy readings of the Monopoly rulebook.').

omega_variable(
    sunset_clause_enforcement,
    'Is the ''sunset clause'' (the implicit agreement to end the game before total elimination) genuinely enforced by social pressure, or does it often fail, leading to harsh outcomes?',
    'Survey of player groups and observation of game endings: frequency of games ending by mutual agreement vs. total elimination, and player satisfaction with game length and outcome.',
    'If the sunset clause is frequently ignored, the scaffold function is weaker, and the constraint leans towards a more extractive or less coordinative type, as the social support mechanism fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforcement, empirical, 'Effectiveness of the implicit social sunset clause in Monopoly games.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__social_scaffold_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__social_scaffold_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__social_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monopoly_rulebook' kernel. This 'social_scaffold_reading' emphasizes the role of house rules in maintaining social cohesion and game playability, contrasting with readings focused on pure extraction or competitive fidelity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
