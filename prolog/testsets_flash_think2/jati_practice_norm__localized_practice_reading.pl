% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Norms
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes jati boundaries as dynamic, locally negotiated
 *   coordination norms, a reading that emphasizes their fluidity and adaptive
 *   function within communities. This perspective contrasts with views that
 *   see jati as fixed by scripture or reified by colonial administration. The
 *   low extractiveness and suppression reflect the empirical observation of
 *   continuous local adaptation and proliferation of categories, indicating a
 *   system driven by coordination rather than rigid enforcement.
 *
 * KEY AGENTS:
 *   - local_community_members: Beneficiary/Payer (moderate/constrained)
 *   - jati_elders_leaders: Agenda_setter (organized/constrained)
 *   - individuals_seeking_social_order: Beneficiary (powerless/identity_locked)
 *   - external_administrators_scholars: Observer (analytical/analytical)
 *   - orthodox_textual_scholars: Excluded (organized/constrained)
 *   - colonial_era_census_officials: Excluded (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.15).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.2).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '0d537e0b-7371-4445-a062-57909cfc07e6').
narrative_ontology:cs_kernel_codification('0d537e0b-7371-4445-a062-57909cfc07e6', distributed).
narrative_ontology:cs_authority_grounding('0d537e0b-7371-4445-a062-57909cfc07e6', practice).
narrative_ontology:cs_interpretation_layer_present('0d537e0b-7371-4445-a062-57909cfc07e6').
narrative_ontology:cs_reading_relation('0d537e0b-7371-4445-a062-57909cfc07e6', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d537e0b-7371-4445-a062-57909cfc07e6', jati_practice_norm__colonial_census_reading, forecloses).
narrative_ontology:cs_axiom('0d537e0b-7371-4445-a062-57909cfc07e6', foundational, jati_is_locally_negotiated).
narrative_ontology:cs_axiom_status(jati_is_locally_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('0d537e0b-7371-4445-a062-57909cfc07e6', jati_is_locally_negotiated, conventional).
narrative_ontology:cs_axiom('0d537e0b-7371-4445-a062-57909cfc07e6', foundational, jati_boundaries_are_fluid).
narrative_ontology:cs_axiom_status(jati_boundaries_are_fluid, holdable).
narrative_ontology:cs_axiom_grounding('0d537e0b-7371-4445-a062-57909cfc07e6', jati_boundaries_are_fluid, empirically_contingent).
narrative_ontology:cs_reference_frame('0d537e0b-7371-4445-a062-57909cfc07e6', dynamic_local_consensus).
narrative_ontology:cs_drift_state('0d537e0b-7371-4445-a062-57909cfc07e6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d537e0b-7371-4445-a062-57909cfc07e6', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_community_members).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, jati_elders_leaders).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, individuals_seeking_social_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, local_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the continuous negotiation and adaptation of jati norms, benefiting from the social order and mutual aid they provide, while bearing the costs of adherence and occasional renegotiation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_community_members, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, local_community_members, payer).

% Facilitate the local renegotiation of jati boundaries, uphold community norms, and mediate disputes. Their authority is derived from their role in maintaining social cohesion, which ties them to the local system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_elders_leaders, agenda_setter,
    organized, generational, constrained, local).

% Benefit from the clear social roles, expectations, and support networks provided by the jati system. Their identity and social standing are deeply intertwined with these norms, making exit from the system difficult without significant personal and social cost.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, individuals_seeking_social_order, beneficiary,
    powerless, immediate, identity_locked, local).

% Study and document the localized practices of jati, often from an academic or policy perspective, without directly participating in or being governed by the local negotiation processes.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, external_administrators_scholars, observer,
    analytical, generational, analytical, global).

% Adhere to a view of jati boundaries as fixed by scriptural varna frameworks. Their perspective is largely excluded from the actual local, dynamic renegotiation of norms, as their claims are not the basis for local practice.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, orthodox_textual_scholars, excluded,
    organized, civilizational, constrained, national).

% Historically attempted to reify and stabilize jati categories for administrative legibility. Their imposed, fixed categories are rejected by this reading, which emphasizes the organic, fluid nature of local practice. They are excluded from the interpretive framework of this reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_era_census_officials, excluded,
    institutional, generational, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for social interaction, marriage alliances, and occupational specialization within diverse local communities, allowing for adaptation to changing social and economic conditions.
% TRANSFER_FUNCTION: Transfers social capital, mutual aid, and a sense of shared identity among members, while also distributing the responsibility and effort of maintaining social cohesion through continuous negotiation.
% ABSENT_VOICES: Orthodox textual scholars, who would argue for a fixed, scriptural basis for jati, and historical colonial administrators, who sought to impose stable, enumerable categories, are both structurally excluded from the local, dynamic process of norm renegotiation.
% DISAPPEARANCE_RATIONALE: If these locally negotiated jati norms vanished overnight, the intricate social fabric of many communities would unravel, leading to significant disruption in social organization, marriage patterns, economic cooperation, and individual identity formation.
% FOUNDING_PROBLEM: Managing social complexity, facilitating inter-group relations, and allocating resources and responsibilities in diverse, localized communities, particularly in the absence of centralized state authority.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological field studies and sociological surveys consistently document the ongoing, adaptive role of jati norms in local social organization across various regions, corroborating that the problem of social coordination and adaptation remains live and is addressed by these practices.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.20) are consistent with a 'Rope' classification, reflecting that these norms primarily serve a coordination function with minimal coercive overhead. The 'theater_ratio' is low (0.10) because the practices are genuinely functional and adaptive, not performative. The 'accessibility_collapse' is moderate (0.30) as alternatives to specific local norms exist, but participating in some form of social ordering is often necessary. Resistance is low (0.10) because the system's flexibility allows for adaptation rather than direct opposition.
 *
 * PERSPECTIVAL GAP:
 *   The 'localized_practice_reading' emphasizes the bottom-up, adaptive nature of jati, which is experienced as beneficial coordination by local community members. This contrasts sharply with the 'orthodox_textual_reading' (fixed scriptural basis) and the 'colonial_census_reading' (externally imposed, reified categories), both of which would likely perceive the constraint as more rigid or extractive from their respective frames. The engine's classification of 'Rope' from this reading's metrics highlights the coordination function, which would be obscured by the other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Local community members and individuals seeking social order are beneficiaries, gaining social cohesion and identity. Jati elders and leaders act as agenda-setters, facilitating the coordination. There are no explicit victims in this reading, as the system is seen as mutually beneficial and adaptive. External scholars are observers, while orthodox textual scholars and colonial census officials are excluded, as their frameworks are not the basis of this localized practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_practice_grounding,
    'To what extent do local jati practices genuinely derive from or align with scriptural texts, as opposed to being purely emergent from local social negotiation?',
    'Comparative ethnographic studies analyzing the explicit justifications for local practices against scriptural interpretations, and historical linguistic analysis of textual influence on local discourse.',
    'If practices are found to be strongly textually derived, the ''orthodox_textual_reading'' gains empirical grounding, potentially shifting this constraint''s classification towards a ''Tangled Rope'' if textual enforcement introduces extraction. If purely emergent, this ''localized_practice_reading'' is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_vs_practice_grounding, empirical, 'Ambiguity regarding the grounding of jati norms in scripture versus local practice.').

omega_variable(
    colonial_reification_impact,
    'Has the historical reification of jati categories by colonial census and administration permanently altered local practice, making it less fluid than this reading suggests?',
    'Longitudinal ethnographic studies comparing pre-colonial accounts of jati fluidity with contemporary practices, and analysis of how state-level administrative categories (e.g., for affirmative action) influence local self-identification.',
    'If colonial reification is found to have significantly hardened boundaries, the ''colonial_census_reading'' gains explanatory power, and this constraint''s effective suppression and extractiveness might be higher than currently assessed, potentially shifting it towards a ''Tangled Rope'' or ''Snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_reification_impact, empirical, 'The lasting impact of colonial administrative reification on the fluidity of jati boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1950, jati_practice_norm__localized_practice_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(jati_tr_t1960, jati_practice_norm__localized_practice_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(jati_tr_t1970, jati_practice_norm__localized_practice_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(jati_tr_t1980, jati_practice_norm__localized_practice_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(jati_tr_t1990, jati_practice_norm__localized_practice_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__localized_practice_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jati_tr_t2010, jati_practice_norm__localized_practice_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(jati_tr_t2020, jati_practice_norm__localized_practice_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t1950, jati_practice_norm__localized_practice_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(jati_be_t1960, jati_practice_norm__localized_practice_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(jati_be_t1970, jati_practice_norm__localized_practice_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(jati_be_t1980, jati_practice_norm__localized_practice_reading, base_extractiveness, 1980, 0.16).
narrative_ontology:measurement(jati_be_t1990, jati_practice_norm__localized_practice_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__localized_practice_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(jati_be_t2010, jati_practice_norm__localized_practice_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(jati_be_t2020, jati_practice_norm__localized_practice_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1950, jati_practice_norm__localized_practice_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(jati_su_t1960, jati_practice_norm__localized_practice_reading, suppression_requirement, 1960, 0.19).
narrative_ontology:measurement(jati_su_t1970, jati_practice_norm__localized_practice_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(jati_su_t1980, jati_practice_norm__localized_practice_reading, suppression_requirement, 1980, 0.21).
narrative_ontology:measurement(jati_su_t1990, jati_practice_norm__localized_practice_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__localized_practice_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(jati_su_t2010, jati_practice_norm__localized_practice_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(jati_su_t2020, jati_practice_norm__localized_practice_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jati_practice_norm' kernel, emphasizing local, dynamic negotiation. It is distinct from the 'orthodox_textual_reading' (scriptural basis) and 'colonial_census_reading' (administrative reification), which represent different structural claims about jati.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
