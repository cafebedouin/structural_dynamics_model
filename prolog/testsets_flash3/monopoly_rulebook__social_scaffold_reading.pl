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
 *   This constraint describes the 'social scaffold' reading of the Monopoly
 *   rulebook, where informal house rules (e.g., money on Free Parking, no
 *   rent in jail) are adopted to make the game more socially palatable and
 *   extend its duration. The original rulebook's harsh elimination mechanics
 *   are softened to preserve group cohesion and ensure a positive experience
 *   for casual players. This reading views the house rules as a necessary,
 *   temporary support structure for social coordination, rather than a
 *   deviation from a 'correct' competitive framework.
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
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '88a64176-6ac5-41e9-aa40-079f01d72f0b').
narrative_ontology:cs_kernel_codification('88a64176-6ac5-41e9-aa40-079f01d72f0b', fixed_text).
narrative_ontology:cs_authority_grounding('88a64176-6ac5-41e9-aa40-079f01d72f0b', practice).
narrative_ontology:cs_interpretation_layer_present('88a64176-6ac5-41e9-aa40-079f01d72f0b').
narrative_ontology:cs_reading_relation('88a64176-6ac5-41e9-aa40-079f01d72f0b', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('88a64176-6ac5-41e9-aa40-079f01d72f0b', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('88a64176-6ac5-41e9-aa40-079f01d72f0b', foundational, game_as_social_glue).
narrative_ontology:cs_axiom_status(game_as_social_glue, holdable).
narrative_ontology:cs_axiom_grounding('88a64176-6ac5-41e9-aa40-079f01d72f0b', game_as_social_glue, conventional).
narrative_ontology:cs_axiom('88a64176-6ac5-41e9-aa40-079f01d72f0b', foundational, prolonged_engagement_over_elimination).
narrative_ontology:cs_axiom_status(prolonged_engagement_over_elimination, holdable).
narrative_ontology:cs_axiom_grounding('88a64176-6ac5-41e9-aa40-079f01d72f0b', prolonged_engagement_over_elimination, instrumental).
narrative_ontology:cs_reference_frame('88a64176-6ac5-41e9-aa40-079f01d72f0b', socially_playable_game).
narrative_ontology:cs_drift_state('88a64176-6ac5-41e9-aa40-079f01d72f0b', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('88a64176-6ac5-41e9-aa40-079f01d72f0b', '').
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

% Benefits from the game being a positive social experience rather than a source of conflict. The house rules prevent the game from becoming too harsh, preserving the social fabric of the group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_cohesion, beneficiary,
    institutional, generational, identity_locked, local).

% Enjoy a longer, more forgiving game experience due to house rules that inject liquidity and slow elimination. They are less interested in strict adherence to the rulebook and more in the social interaction.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% May find the house rules dilute the strategic challenge or deviate from the 'true' game. They bear the cost of a less competitive, more drawn-out game, but often accept it for the sake of social harmony.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_purists, payer,
    moderate, biographical, constrained, local).

% Observe how players adapt the game to fit social contexts, providing insights into emergent gameplay and the tension between formal rules and social playability.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_designers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates a prolonged, enjoyable social activity by mitigating the harsh, eliminative aspects of the base game, ensuring all participants remain engaged for a longer duration.
% TRANSFER_FUNCTION: Transfers game resources (money, property) from eliminated players back into the game economy (e.g., via 'free parking' rules), and transfers social capital from strict adherence to rules towards group harmony and extended play.
% ABSENT_VOICES: Players who prefer a strictly competitive, high-stakes game are often marginalized or choose not to participate in groups that adopt these house rules; they would argue for fidelity to the original text and a more decisive outcome.
% DISAPPEARANCE_RATIONALE: If the social scaffold of house rules vanished, games would become shorter, more cutthroat, and lead to earlier player elimination, likely reducing the game's social appeal and frequency of play within casual groups. The social dynamics would shift dramatically.
% FOUNDING_PROBLEM: The original Monopoly rulebook, while designed for economic education, often leads to rapid player elimination and social friction in casual play, making it less suitable for sustained group entertainment.
% FOUNDING_PROBLEM_CORROBORATION: Anecdotal evidence from countless game nights, common adoption of similar house rules across diverse groups, and sociological studies of board game dynamics corroborate that the base game's harshness is a persistent social problem for casual play. Game designers also acknowledge this tension.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, representing the 'cost' of deviating from strict rules for social benefit. Suppression (0.25) is low, as these house rules are voluntarily adopted for social reasons, not coercively enforced. Theater ratio (0.1) is low because the house rules genuinely serve their intended function of improving social playability. The constraint is a scaffold because its justification is transitional (from harsh game to social activity) and it carries an implicit sunset (the game ends, the rules are re-negotiated next time).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of casual players and the social group, the house rules are a beneficial adaptation. From the perspective of rulebook purists, they might be seen as a degradation of the game's integrity. The engine's classification will reflect the scaffold nature from the perspective of social coordination, while acknowledging the 'cost' to competitive play.
 *
 * DIRECTIONALITY LOGIC:
 *   Social group cohesion and casual players are beneficiaries, as the house rules directly serve their interests in prolonged, enjoyable play. Rulebook purists, while often accepting the house rules for social harmony, are payers as they experience a diluted competitive experience. Game designers are observers, analyzing the emergent social dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as a pure rope (ignoring the deviation from the original rules) or a snare (it's not coercive). The 'mandate' of the house rules is to facilitate social play, which is still live. The sunset clause is implicit in the temporary nature of a single game session, after which rules can be re-negotiated or abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_competitive_priority,
    'Is the primary purpose of playing Monopoly social interaction or competitive victory?',
    'Survey player motivations, observe player behavior in mixed-rule environments, and analyze post-game sentiment. If social interaction consistently outweighs competitive drive, this reading is strongly supported.',
    'If competitive victory is primary, this scaffold reading would be reclassified as a tangled rope or snare from the perspective of competitive players, as it extracts competitive integrity for social harmony. If social interaction is primary, the scaffold classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_vs_competitive_priority, conceptual, 'Ambiguity in the game''s fundamental purpose (social vs. competitive).').

omega_variable(
    house_rule_codification_drift,
    'Do these ''house rules'' eventually become codified or institutionalized, losing their temporary, adaptive nature?',
    'Longitudinal study of gaming groups: track if house rules are written down, consistently applied across different groups, or taught as ''the real rules'' to new players. If so, the scaffold''s sunset clause may be eroding.',
    'If house rules become codified and permanent, the constraint would drift from a scaffold towards a rope (if still beneficial) or a piton (if function atrophies but rules persist by inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rule_codification_drift, empirical, 'Whether informal house rules drift towards formal codification.').


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
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monopoly_rulebook' kernel, focusing on its social adaptation. It is linked to 'monopoly_rulebook__extraction_demo_reading' and 'monopoly_rulebook__tournament_orthodoxy_reading' as sibling readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
