% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__tournament_orthodoxy_reading, []).

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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook: Tournament Orthodoxy Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'tournament orthodoxy' reading of the
 *   Monopoly rulebook. In this reading, the rulebook is seen as the
 *   definitive, immutable framework for competitive play, where strategic
 *   skill is paramount. 'House rules' are considered deviations that obscure
 *   the game's true competitive depth and are to be rejected for any serious
 *   comparison or ranking. The constraint functions as a coordination
 *   mechanism for a specific competitive community, ensuring a consistent
 *   standard for play.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.15).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook: Tournament Orthodoxy Reading").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, 'b032a294-c13f-410d-a6d3-9a95d9af0eef').
narrative_ontology:cs_kernel_codification('b032a294-c13f-410d-a6d3-9a95d9af0eef', fixed_text).
narrative_ontology:cs_authority_grounding('b032a294-c13f-410d-a6d3-9a95d9af0eef', practice).
narrative_ontology:cs_interpretation_layer_present('b032a294-c13f-410d-a6d3-9a95d9af0eef').
narrative_ontology:cs_reading_relation('b032a294-c13f-410d-a6d3-9a95d9af0eef', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('b032a294-c13f-410d-a6d3-9a95d9af0eef', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('b032a294-c13f-410d-a6d3-9a95d9af0eef', foundational, rulebook_text_is_immutable).
narrative_ontology:cs_axiom_status(rulebook_text_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('b032a294-c13f-410d-a6d3-9a95d9af0eef', rulebook_text_is_immutable, conventional).
narrative_ontology:cs_axiom('b032a294-c13f-410d-a6d3-9a95d9af0eef', foundational, strategic_skill_determines_outcome).
narrative_ontology:cs_axiom_status(strategic_skill_determines_outcome, holdable).
narrative_ontology:cs_axiom_grounding('b032a294-c13f-410d-a6d3-9a95d9af0eef', strategic_skill_determines_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('b032a294-c13f-410d-a6d3-9a95d9af0eef', pure_competitive_framework).
narrative_ontology:cs_drift_state('b032a294-c13f-410d-a6d3-9a95d9af0eef', contemporary_casual_play_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b032a294-c13f-410d-a6d3-9a95d9af0eef', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Players who value strategic depth and fair comparison in Monopoly. They benefit from a standardized rulebook that ensures consistent competitive conditions across games and tournaments. They voluntarily adhere to the rulebook to maintain the integrity of their competitive ecosystem.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community, beneficiary,
    organized, biographical, mobile, global).

% Entities responsible for setting up and running competitive Monopoly events. They enforce the strict rulebook interpretation to ensure fairness, comparability of results, and the legitimacy of their rankings. They benefit from the clarity and widely accepted standard the rulebook provides.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, generational, mobile, global).

% Players who prioritize social interaction and fun over strict competitive integrity, often using 'house rules' to mitigate harsh outcomes. They are not directly targeted by this constraint but are implicitly excluded from its competitive framework, as their preferred mode of play is deemed 'noise'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, excluded,
    moderate, immediate, mobile, local).

% Academics and researchers who study game mechanics, player psychology, and the social dynamics of rule systems. They analyze how different rule interpretations shape player behavior and game outcomes, providing an external, analytical perspective on the constraint.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_design_theorists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally understood and accepted standard for competitive Monopoly play, enabling fair comparison of skill and consistent tournament conditions.
% TRANSFER_FUNCTION: Transfers the 'cost' of adhering to a strict, immutable rule set from the individual player to the collective competitive community, in exchange for a shared, high-integrity competitive framework.
% ABSENT_VOICES: Casual players and proponents of 'house rules' are absent from the discourse of tournament orthodoxy. They would argue that strict adherence to the rulebook creates overly harsh, socially unplayable outcomes, and that house rules serve a vital social function. Their voices are excluded by the very definition of 'competitive integrity' this reading upholds.
% DISAPPEARANCE_RATIONALE: If the 'tournament orthodoxy' reading of the Monopoly rulebook vanished, competitive play would fragment. There would be no consistent standard for ranking players or comparing skill, leading to a collapse of organized tournaments and a loss of competitive integrity. The competitive community would either dissolve or reorganize around a new, agreed-upon standard.
% FOUNDING_PROBLEM: The problem of inconsistent play experiences and subjective outcomes in Monopoly, making fair competitive comparison and skill assessment impossible.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and the competitive community consistently attest that the problem of inconsistent play and the need for a clear competitive standard remains live. Game design theorists, from an external analytical seat, corroborate the structural need for clear rules in competitive environments, even if they might critique the specific outcomes of Monopoly's rules.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).
:- end_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because participation is voluntary, and the 'cost' is adherence to a shared standard for competitive integrity, which is self-imposed by the community. Suppression is low (0.15) as it primarily relies on social norms and community consensus rather than active enforcement. Theater ratio is negligible (0.05) as the constraint's function is direct and transparent: to provide a consistent competitive framework. Accessibility collapse is high (0.80) because once the 'correct' way to play for competitive purposes is understood, alternatives (house rules) are largely dismissed by this community. Resistance is low (0.10) within the competitive community, as adherence to the rulebook is a core tenet.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the competitive community and tournament organizers, this constraint is a pure Rope, enabling fair comparison and skill demonstration. From the perspective of casual players or those holding the 'social scaffold' reading, the strict adherence might be seen as overly rigid or even extractive of social enjoyment, but this reading explicitly rejects those framings as irrelevant to competitive play.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive Monopoly community and tournament organizers are the primary beneficiaries (d near 0.0), as the constraint directly enables their desired mode of play and event organization. There are no direct 'victims' in this reading, as participation is voluntary and the 'cost' is simply adherence to the agreed-upon standard for competitive integrity. Those who prefer house rules are not 'victims' but simply outside the scope of this particular competitive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine coordination mechanism (for a specific competitive purpose) as extractive. The low extractiveness and suppression, coupled with voluntary participation and clear benefits for the target community, align with a Rope. The constraint's mandate (to provide a consistent competitive framework) is live and actively desired by its beneficiaries, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism, or is its low extraction merely a ''reading'' that masks underlying extractive or socially corrective functions?',
    'Observe player behavior and community discourse in contexts where ''house rules'' are explicitly forbidden. If competitive play thrives and satisfaction remains high, this reading is robust. If players consistently seek to reintroduce house rules or abandon play, the reading is fragile.',
    'If robust, the constraint remains a Rope. If fragile, it may be a Snare (extraction_demo_reading) or Tangled Rope (social_scaffold_reading) where this reading serves as a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''tournament orthodoxy'' reading of the ''monopoly_rulebook'' kernel. Sibling readings include ''extraction_demo_reading'' (high extraction) and ''social_scaffold_reading'' (community correction).').

omega_variable(
    house_rules_as_noise_or_feature,
    'Are ''house rules'' truly noise obscuring competitive depth, or do they serve a legitimate social coordination function for casual play that this reading suppresses?',
    'Survey casual player communities on their satisfaction with house rules versus strict rulebook adherence. Analyze player retention and engagement in both contexts. If house rules correlate with higher engagement for casual players, this reading''s ''noise'' claim is empirically challenged.',
    'If house rules are a legitimate feature, this reading''s suppression of them would increase its effective extractiveness for casual players, potentially shifting it towards a Tangled Rope for that segment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rules_as_noise_or_feature, empirical, 'Ambiguity regarding the function of ''house rules'' in Monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monopoly_rulebook' kernel. It focuses on the competitive, skill-based interpretation, distinct from readings emphasizing extraction or social correction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
