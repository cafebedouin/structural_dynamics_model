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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly Rulebook: Social Scaffold Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the role of 'house rules' in playing Monopoly,
 *   which act as a social scaffold to make the game playable for casual
 *   groups. The official rulebook, when played strictly, often leads to harsh
 *   outcomes and early player elimination, which can be detrimental to social
 *   cohesion. House rules, such as 'free parking jackpot' or 'no collecting
 *   rent in jail', inject liquidity and slow down elimination, preserving the
 *   social function of the game. This reading frames the house rules as a
 *   necessary adaptation for social playability, rather than a subversion of
 *   the game's core. The claimed type is 'scaffold' because these rules
 *   provide temporary support for a specific social context, meant to be
 *   transitional for the duration of a game session, and are actively
 *   enforced by social consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.45).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.55).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook: Social Scaffold Reading").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'c9445154-169d-4290-98c4-219dbc7b5e77').
narrative_ontology:cs_kernel_codification('c9445154-169d-4290-98c4-219dbc7b5e77', fixed_text).
narrative_ontology:cs_authority_grounding('c9445154-169d-4290-98c4-219dbc7b5e77', practice).
narrative_ontology:cs_interpretation_layer_present('c9445154-169d-4290-98c4-219dbc7b5e77').
narrative_ontology:cs_reading_relation('c9445154-169d-4290-98c4-219dbc7b5e77', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9445154-169d-4290-98c4-219dbc7b5e77', monopoly_rulebook__tournament_orthodoxy_reading, forecloses).
narrative_ontology:cs_axiom('c9445154-169d-4290-98c4-219dbc7b5e77', foundational, social_harmony_over_strict_rules).
narrative_ontology:cs_axiom_status(social_harmony_over_strict_rules, holdable).
narrative_ontology:cs_axiom_grounding('c9445154-169d-4290-98c4-219dbc7b5e77', social_harmony_over_strict_rules, conventional).
narrative_ontology:cs_axiom('c9445154-169d-4290-98c4-219dbc7b5e77', secondary, game_as_facilitator_of_interaction).
narrative_ontology:cs_axiom_status(game_as_facilitator_of_interaction, holdable).
narrative_ontology:cs_axiom_grounding('c9445154-169d-4290-98c4-219dbc7b5e77', game_as_facilitator_of_interaction, instrumental).
narrative_ontology:cs_reference_frame('c9445154-169d-4290-98c4-219dbc7b5e77', social_cohesion_play).
narrative_ontology:cs_drift_state('c9445154-169d-4290-98c4-219dbc7b5e77', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c9445154-169d-4290-98c4-219dbc7b5e77', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_groups).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_players).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, purists).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, competitive_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from prolonged, less confrontational gameplay that fosters social cohesion rather than friction. They are the primary reason house rules are adopted.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_groups, beneficiary,
    organized, biographical, mobile, local).

% Prefer a game that doesn't eliminate them quickly or harshly, allowing for longer engagement and enjoyment. They are often the majority in social settings.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% Bear the cost of rule deviation, feeling that the game's integrity and strategic depth are compromised by house rules. They may reluctantly participate or withdraw.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, purists, payer,
    moderate, biographical, constrained, local).

% Seek to exploit strict rules for strategic advantage and are frustrated by house rules that inject randomness or mitigate their skill-based advantages. They see it as 'not playing correctly'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_players, payer,
    powerful, biographical, constrained, local).

% Often the individual who introduces, proposes, or enforces house rules to maintain social harmony, manage expectations, and ensure the game is enjoyable for all participants.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_host, agenda_setter,
    moderate, immediate, mobile, local).

% Designed the original game with specific mechanics and outcomes in mind, but their intended experience is reinterpreted and adapted by social play. They observe the divergence from their design.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_designers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables diverse social groups to play a game designed for harsh elimination by adapting its rules to prioritize social cohesion, prolonged engagement, and a less confrontational experience.
% TRANSFER_FUNCTION: Transfers game longevity, social harmony, and reduced player friction from strict rule adherence and competitive outcomes. It redistributes 'fun' more evenly.
% ABSENT_VOICES: Strict competitive players who prefer unadulterated strategic depth are often marginalized or excluded from casual social games that employ house rules. The original rulebook designers' intent is also effectively 'absent' from the immediate social negotiation.
% DISAPPEARANCE_RATIONALE: If the social scaffold of house rules vanished, many social groups would likely stop playing Monopoly due to its inherent harshness, early eliminations, and potential for social friction. The game's role as a social lubricant would cease, and alternative, less confrontational games would likely take its place.
% FOUNDING_PROBLEM: The original Monopoly rulebook, when played strictly, often leads to early player elimination, short game durations, and social friction, making it unsuitable for casual, prolonged social gatherings and potentially damaging to group dynamics.
% FOUNDING_PROBLEM_CORROBORATION: Anecdotal evidence from countless game nights, board game forums, and sociological observations on game dynamics consistently corroborates that strict Monopoly play often leads to negative social outcomes, supporting the need for house rules to mitigate these issues. This is attested by players and observers outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) because while the game still has winners and losers, the house rules mitigate the harshest forms of wealth concentration and elimination. Suppression is moderate (0.55) as the official rules are still present, but their full, harsh force is suppressed by social norms and the active enforcement of house rules. Theater ratio is moderate (0.40) because players perform adherence to the 'idea' of Monopoly while actually following a modified, socially-negotiated rule set. Accessibility collapse is low (0.30) as house rules actively open up alternatives to the harsh official endgame, making the game more accessible to casual players. Resistance is low (0.20) because most players in a social setting prefer the house rules to the strict, often frustrating, official rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social groups, the house rules are a beneficial coordination mechanism. From the perspective of purists, they are a degradation of the game's integrity. The engine's classification will reflect this divergence based on the declared roles and metrics, showing a scaffold for beneficiaries and a more extractive type for targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Social groups and casual players are clear beneficiaries, experiencing prolonged engagement and reduced friction (low d). Purists and competitive players are targets, as their preferred mode of play is suppressed (high d). The game host acts as an agenda-setter, facilitating the adoption and enforcement of house rules. Rulebook designers are analytical observers, their original intent reinterpreted by practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_contract_vs_textual_fidelity,
    'Is the social contract of playability, as embodied by house rules, a legitimate adaptation of the game''s core design or a subversion of its intended mechanics?',
    'Analysis of player retention and satisfaction in groups playing with and without house rules, alongside a philosophical inquiry into the ''essence'' of game play versus social interaction.',
    'If a legitimate adaptation, the scaffold classification is robust. If a subversion, the constraint might lean towards a ''snare'' for purists, as their preferred play is actively suppressed for the benefit of others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_contract_vs_textual_fidelity, conceptual, 'Ambiguity regarding the legitimacy of social adaptation over strict rule adherence.').

omega_variable(
    strict_play_sustainability,
    'Can the game of Monopoly be played strictly according to the official rulebook in casual social settings without leading to social breakdown, player abandonment, or significant friction?',
    'Empirical studies observing social dynamics and player behavior in groups playing Monopoly under strict official rules versus with house rules.',
    'If strict play is unsustainable, it strengthens the ''scaffold'' argument for house rules as a necessary coordination mechanism. If sustainable, it weakens the justification for house rules, potentially reclassifying them as a ''snare'' for purists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strict_play_sustainability, empirical, 'Empirical question of whether strict Monopoly play is socially viable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mono_tr_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(mono_tr_t18, monopoly_rulebook__social_scaffold_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__social_scaffold_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__social_scaffold_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mono_be_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(mono_be_t18, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mono_su_t6, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(mono_su_t12, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(mono_su_t18, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(mono_su_t24, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
