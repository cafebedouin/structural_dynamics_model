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
 *   human_readable: Monopoly Rulebook (Social Scaffold Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the informal 'house rules' that emerge in
 *   casual Monopoly games, which modify the official rulebook to make the
 *   game more socially palatable and extend its duration. It is a 'social
 *   scaffold' because it temporarily (for the duration of a game session or a
 *   series of casual games) supports a coordination function (social play)
 *   that the underlying 'official' rulebook would undermine. The scaffold has
 *   a sunset clause in that it only applies to specific social contexts and
 *   can be removed or altered at will by the players. This is one reading of
 *   the 'monopoly_rulebook' kernel, focusing on its social function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.45).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.25).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.25).
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
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'b50ee49d-f7db-485b-9cea-cb1b2b45841d').
narrative_ontology:cs_kernel_codification('b50ee49d-f7db-485b-9cea-cb1b2b45841d', formalized).
narrative_ontology:cs_authority_grounding('b50ee49d-f7db-485b-9cea-cb1b2b45841d', practice).
narrative_ontology:cs_interpretation_layer_present('b50ee49d-f7db-485b-9cea-cb1b2b45841d').
narrative_ontology:cs_reading_relation('b50ee49d-f7db-485b-9cea-cb1b2b45841d', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('b50ee49d-f7db-485b-9cea-cb1b2b45841d', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('b50ee49d-f7db-485b-9cea-cb1b2b45841d', foundational, game_as_social_glue).
narrative_ontology:cs_axiom_status(game_as_social_glue, holdable).
narrative_ontology:cs_axiom_grounding('b50ee49d-f7db-485b-9cea-cb1b2b45841d', game_as_social_glue, conventional).
narrative_ontology:cs_axiom('b50ee49d-f7db-485b-9cea-cb1b2b45841d', secondary, mitigation_of_harsh_outcomes_is_good).
narrative_ontology:cs_axiom_status(mitigation_of_harsh_outcomes_is_good, holdable).
narrative_ontology:cs_axiom_grounding('b50ee49d-f7db-485b-9cea-cb1b2b45841d', mitigation_of_harsh_outcomes_is_good, instrumental).
narrative_ontology:cs_reference_frame('b50ee49d-f7db-485b-9cea-cb1b2b45841d', socially_playable_game).
narrative_ontology:cs_drift_state('b50ee49d-f7db-485b-9cea-cb1b2b45841d', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b50ee49d-f7db-485b-9cea-cb1b2b45841d', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, rulebook_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the game being a positive social experience rather than a source of conflict or rapid elimination. The house rules preserve the group's ability to play together over time, reinforcing social bonds.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_cohesion, beneficiary,
    institutional, generational, identity_locked, local).

% Enjoy a longer, more forgiving game experience where early mistakes don't lead to immediate elimination. They are less focused on strict adherence to rules and more on the social interaction. They can easily choose to play other games if this one becomes too harsh.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% Bear the cost of deviating from the official rules, which they see as undermining the game's intended challenge and strategic depth. They value fidelity to the text and competitive integrity, but often concede to house rules for social harmony.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_purists, payer,
    moderate, biographical, constrained, local).

% Observe how players adapt the game to fit social contexts, providing insights into emergent gameplay and the tension between formal rules and informal social contracts. They analyze the impact of house rules on game longevity and player engagement.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_designers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates prolonged social interaction and group cohesion by mitigating the harsh, eliminatory aspects of the official Monopoly rulebook, ensuring the game remains a socially enjoyable activity for casual groups.
% TRANSFER_FUNCTION: Transfers game longevity and social harmony to the group by diluting the financial extraction and player elimination mandated by the strict rules, effectively redistributing 'playtime' and 'inclusion' among participants.
% ABSENT_VOICES: Strict tournament players or those who view Monopoly as a serious economic simulation are often absent from casual social games; they would argue that house rules undermine the game's core lessons about capital accumulation and strategic play.
% DISAPPEARANCE_RATIONALE: If the social scaffold of house rules vanished, casual games would revert to the official, harsher rules, leading to rapid player elimination, shorter game durations, and increased social friction, likely resulting in the game being played less frequently or abandoned by many groups.
% FOUNDING_PROBLEM: The official Monopoly rulebook, when played strictly, often leads to rapid player elimination and social conflict, making the game unplayable or unenjoyable for many casual groups seeking prolonged social interaction.
% FOUNDING_PROBLEM_CORROBORATION: Numerous anecdotal accounts from casual player groups, board game forums, and sociological studies of play confirm that strict adherence to Monopoly rules often leads to negative social outcomes, corroborating the ongoing need for house rules to maintain social playability.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.45) because the house rules still involve some degree of 'extraction' from the game's original design intent (e.g., reducing the challenge for purists) but primarily serve to redistribute resources (money, playtime) to keep more players in the game. Suppression is low (0.25) as adherence to house rules is largely voluntary, driven by social pressure rather than coercion. Theater ratio is low (0.1) because the house rules are genuinely functional in achieving social coordination, not merely performative. The scaffold classification is supported by the explicit sunset clause (house rules apply only to a specific game session) and its clear beneficiary (social group cohesion).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of casual players, the house rules are essential for the game's social function, making it a 'rope' or 'scaffold'. From the perspective of rulebook purists, these same rules are an 'extraction' from the game's intended design, potentially making it a 'snare' on competitive integrity. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Social group cohesion and casual players are beneficiaries, as the house rules directly serve their interests in prolonged, enjoyable play. Rulebook purists are payers, as they 'pay' by compromising on strict adherence to the official rules. Game designers are observers, analyzing the emergent dynamics without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_competitive_framing,
    'Is the primary purpose of playing Monopoly social interaction or competitive economic simulation?',
    'Player surveys on motivations for playing, analysis of game session outcomes (e.g., frequency of ''friendly'' bankruptcies vs. aggressive eliminations).',
    'If primarily social, this scaffold reading is robust. If primarily competitive, the house rules are a ''snare'' on competitive integrity, and the ''tournament_orthodoxy_reading'' would be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_competitive_framing, conceptual, 'Ambiguity in the game''s core purpose influences constraint classification.').

omega_variable(
    house_rule_codification_level,
    'To what extent are these ''house rules'' formalized or consistently applied across different social groups?',
    'Ethnographic studies of play groups, content analysis of online forums discussing house rules.',
    'Higher formalization or consistency would strengthen the ''scaffold'' classification by demonstrating a more robust, albeit informal, institutional design. Lower consistency might suggest a more ''diffuse'' or ''ad-hoc'' coordination mechanism, potentially weakening the scaffold''s structural integrity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(house_rule_codification_level, empirical, 'The degree of formalization of informal rules.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''monopoly_rulebook'' kernel, how do the ''extraction_demo_reading'' and ''tournament_orthodoxy_reading'' structurally differ from this ''social_scaffold_reading''?',
    'Comparative analysis of the core axioms, beneficiaries, and victims declared in each reading''s constraint story.',
    'The ''extraction_demo_reading'' would likely show higher extractiveness and suppression, with ''capitalists'' as beneficiaries and ''bankrupt players'' as victims. The ''tournament_orthodoxy_reading'' would emphasize fidelity to text and competitive outcomes, with ''skilled players'' as beneficiaries and ''house rule players'' as victims. This omega documents the structural differences that justify separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documents the structural differences between sibling readings of the Monopoly rulebook kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__social_scaffold_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__social_scaffold_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__social_scaffold_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__social_scaffold_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'monopoly_rulebook' kernel. This 'social_scaffold_reading' focuses on the informal rules that enable social play, distinct from the 'extraction_demo_reading' (which emphasizes the game's inherent wealth concentration) and the 'tournament_orthodoxy_reading' (which prioritizes strict competitive adherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
