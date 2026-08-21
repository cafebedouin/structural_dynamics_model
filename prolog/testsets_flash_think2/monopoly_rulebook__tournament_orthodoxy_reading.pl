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
    narrative_ontology:epsilon_provenance/5,
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
 *   Monopoly rulebook, where the rulebook is considered an immutable,
 *   legitimate framework for competitive play. Strategic skill is paramount,
 *   and any deviation (e.g., house rules) is seen as obscuring competitive
 *   depth. This reading emphasizes coordination around a shared standard for
 *   fair comparison and ranking, with minimal extraction from participants
 *   who voluntarily adhere to the competitive frame.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.1).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook: Tournament Orthodoxy Reading").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__tournament_orthodoxy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '6a1a7a29-e184-47be-8fbe-d703ebe2b630').
narrative_ontology:cs_kernel_codification('6a1a7a29-e184-47be-8fbe-d703ebe2b630', fixed_text).
narrative_ontology:cs_authority_grounding('6a1a7a29-e184-47be-8fbe-d703ebe2b630', practice).
narrative_ontology:cs_interpretation_layer_present('6a1a7a29-e184-47be-8fbe-d703ebe2b630').
narrative_ontology:cs_reading_relation('6a1a7a29-e184-47be-8fbe-d703ebe2b630', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a1a7a29-e184-47be-8fbe-d703ebe2b630', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('6a1a7a29-e184-47be-8fbe-d703ebe2b630', foundational, rulebook_as_immutable_competitive_framework).
narrative_ontology:cs_axiom_status(rulebook_as_immutable_competitive_framework, holdable).
narrative_ontology:cs_axiom_grounding('6a1a7a29-e184-47be-8fbe-d703ebe2b630', rulebook_as_immutable_competitive_framework, conventional).
narrative_ontology:cs_axiom('6a1a7a29-e184-47be-8fbe-d703ebe2b630', foundational, strategic_skill_determines_outcome).
narrative_ontology:cs_axiom_status(strategic_skill_determines_outcome, holdable).
narrative_ontology:cs_axiom_grounding('6a1a7a29-e184-47be-8fbe-d703ebe2b630', strategic_skill_determines_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('6a1a7a29-e184-47be-8fbe-d703ebe2b630', pure_competitive_integrity).
narrative_ontology:cs_drift_state('6a1a7a29-e184-47be-8fbe-d703ebe2b630', contemporary_competitive_scene, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6a1a7a29-e184-47be-8fbe-d703ebe2b630', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__tournament_orthodoxy_reading, casual_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a standardized, immutable rulebook that ensures fair competition and allows for objective skill comparison and ranking. Voluntary participation is key to its legitimacy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community, beneficiary,
    organized, biographical, mobile, global).

% Enforce the official rulebook strictly to maintain competitive integrity and legitimacy. Their authority derives from upholding the text's immutability for competitive play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    organized, biographical, constrained, regional).

% May find the strict rules less 'fun' or socially flexible, but accept them when participating in competitive events, valuing the integrity of the game over personal preferences. Their 'payment' is adherence to the strict text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, payer,
    moderate, immediate, mobile, local).

% Advocate for rule modifications (house rules) to enhance social play, mitigate harsh outcomes, or introduce new dynamics. They are structurally excluded from the discourse of competitive orthodoxy, where their proposals are seen as 'noise'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, house_rule_advocates, excluded,
    moderate, biographical, constrained, local).

% Observe how the rulebook functions in competitive play, analyzing its impact on strategy, balance, and player engagement, but do not directly participate in its enforcement or competitive interpretation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_designers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accepted, immutable framework for competitive play, allowing players to develop and compare strategic skill on a level playing field.
% TRANSFER_FUNCTION: Transfers legitimacy and competitive focus from informal 'house rules' or social adaptations to the official, immutable rulebook, ensuring consistency for ranking and comparison.
% ABSENT_VOICES: House rule advocates are absent from the competitive discourse; they would argue for flexibility and social adaptation over strict textual adherence, but their perspective is deemed irrelevant to competitive depth.
% DISAPPEARANCE_RATIONALE: If the rulebook's authority and immutability vanished, competitive tournaments would lose their standardized basis for skill comparison and ranking. The competitive community would fragment, and the game's status as a test of strategic skill would erode.
% FOUNDING_PROBLEM: The need for a consistent, universally accepted framework to enable fair competitive play and objective comparison of strategic skill in Monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers, professional players, and competitive gaming associations consistently attest to the ongoing necessity of a stable, immutable rulebook for competitive integrity. This corroboration comes from within the competitive community, but is widely accepted as a prerequisite for competitive play.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.05) reflects that participants are net beneficiaries of a stable competitive framework, willingly accepting its constraints for the sake of fair play. Suppression (0.1) is minimal, as adherence is largely voluntary for those seeking competitive engagement. Theater ratio (0.05) is low because the enforcement of rules directly serves the stated function of competitive integrity. The constraint is classified as a Rope due to its genuine coordination function and net benefit to participants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the competitive community, the rulebook is a pure coordination mechanism. From the perspective of house rule advocates, the same rulebook might be seen as overly rigid or even suppressive of social play, though this reading explicitly rejects that framing for competitive purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive Monopoly community is the primary beneficiary, gaining a stable and fair environment for skill-based competition. Tournament organizers act as agenda-setters, enforcing the rules to preserve this benefit. Casual players and house rule advocates, while 'paying' in terms of adherence or exclusion, are not victims in an extractive sense, as their participation in the competitive frame is voluntary or their alternative (social play) is readily available.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''tournament_orthodoxy_reading'' of the ''monopoly_rulebook'' kernel?',
    'Analysis of competitive community discourse, tournament regulations, and player testimonials to confirm adherence to the principles of immutable text authority and skill-based outcomes.',
    'If misidentified, the classification and metrics would be inaccurate, potentially conflating a coordination mechanism with an extractive or socially adaptive one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    sibling_impact_extraction_demo,
    'How would the ''extraction_demo_reading'' (rulebook instantiates inevitable wealth concentration) alter the structural analysis of this constraint?',
    'Adopting the ''extraction_demo_reading'' would require re-evaluating extractiveness and suppression from the perspective of wealth transfer and structural inequality inherent in the game''s design, rather than competitive fairness.',
    'If the ''extraction_demo_reading'' were adopted, this constraint would likely reclassify as a Snare or Tangled Rope, with significantly higher extractiveness and identifiable victims (e.g., eliminated players).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_impact_extraction_demo, conceptual, 'Impact of the ''extraction_demo_reading'' on classification.').

omega_variable(
    sibling_impact_social_scaffold,
    'How would the ''social_scaffold_reading'' (rulebook requires community correction via house rules) alter the structural analysis of this constraint?',
    'Adopting the ''social_scaffold_reading'' would shift focus to the social function of house rules in mitigating harsh outcomes and preserving player relationships, re-evaluating the ''noise'' aspect of house rules.',
    'If the ''social_scaffold_reading'' were adopted, this constraint would likely be seen as a Scaffold or Tangled Rope, with house rules acting as a coordination mechanism to prevent social collapse, and the ''official'' rulebook potentially seen as a source of social friction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_impact_social_scaffold, conceptual, 'Impact of the ''social_scaffold_reading'' on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monopoly_rulebook' kernel, each representing a distinct structural interpretation of the game's rules and their function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
