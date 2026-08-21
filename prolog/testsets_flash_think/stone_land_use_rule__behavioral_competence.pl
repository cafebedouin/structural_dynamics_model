% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'Stone Land-Use Rule' as a live
 *   prohibition, actively enforced through daily spatial practice and
 *   institutional memory, ensuring community safety from recurring natural
 *   disasters. This reading emphasizes the rule's functional efficacy and the
 *   community's sustained behavioral compliance over 78 years, accepting the
 *   economic costs of not developing hazardous land. It is one reading of the
 *   'stone_land_use_rule' kernel, distinct from the 'commemorative_husk'
 *   reading which views the rule as a decayed symbol.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.2).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '6df6d980-d12b-4094-8fee-f9ce36a3cba6').
narrative_ontology:cs_kernel_codification('6df6d980-d12b-4094-8fee-f9ce36a3cba6', implicit).
narrative_ontology:cs_authority_grounding('6df6d980-d12b-4094-8fee-f9ce36a3cba6', practice).
narrative_ontology:cs_interpretation_layer_present('6df6d980-d12b-4094-8fee-f9ce36a3cba6').
narrative_ontology:cs_reading_relation('6df6d980-d12b-4094-8fee-f9ce36a3cba6', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('6df6d980-d12b-4094-8fee-f9ce36a3cba6', foundational, hazard_is_persistent).
narrative_ontology:cs_axiom_status(hazard_is_persistent, holdable).
narrative_ontology:cs_axiom_grounding('6df6d980-d12b-4094-8fee-f9ce36a3cba6', hazard_is_persistent, empirically_contingent).
narrative_ontology:cs_axiom('6df6d980-d12b-4094-8fee-f9ce36a3cba6', foundational, collective_safety_trumps_individual_land_use).
narrative_ontology:cs_axiom_status(collective_safety_trumps_individual_land_use, holdable).
narrative_ontology:cs_axiom_grounding('6df6d980-d12b-4094-8fee-f9ce36a3cba6', collective_safety_trumps_individual_land_use, deontological).
narrative_ontology:cs_reference_frame('6df6d980-d12b-4094-8fee-f9ce36a3cba6', community_resilience_through_memory).
narrative_ontology:cs_drift_state('6df6d980-d12b-4094-8fee-f9ce36a3cba6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6df6d980-d12b-4094-8fee-f9ce36a3cba6', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, community_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, institutional_memory_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, younger_generations).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, community_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, younger_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live safely due to the rule, avoiding recurring natural disasters. They accept the economic costs of not developing the prohibited land, internalizing the rule through shared memory and daily practice.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, community_residents, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, community_residents, payer).

% Actively transmit the knowledge of past disasters and the rationale for the land-use rule to younger generations. Their role is crucial for the rule's persistence and is tied to their professional or community identity.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, institutional_memory_keepers, agenda_setter,
    institutional, generational, identity_locked, local).

% Inherit the safety provided by the rule and the associated land-use restrictions. While they benefit, their direct memory of the founding disaster is absent, making them reliant on the institutional memory keepers for understanding the rule's necessity.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, younger_generations, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, younger_generations, payer).

% Are prevented from developing the economically attractive but hazardous land. They would seek to maximize land value but are constrained by the community's adherence to the rule.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, developers, excluded,
    powerful, immediate, mobile, local).

% Represent the alternative view that the rule has lost its behavioral force and is merely a symbolic gesture, detached from active compliance or hazard mitigation. They observe the rule's persistence but question its functional efficacy.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, commemorative_husk_advocates, observer,
    moderate, biographical, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents community members from building in a historically hazardous area, ensuring collective safety from recurring natural disasters by coordinating land-use practices.
% TRANSFER_FUNCTION: Transfers the cost of foregone land development opportunities from individuals to the collective benefit of safety and disaster avoidance, ensuring long-term community resilience.
% ABSENT_VOICES: Developers who would profit from building on the prohibited land, and those who prioritize short-term economic gain over long-term disaster resilience, are effectively excluded from influencing land-use decisions in this area.
% DISAPPEARANCE_RATIONALE: If the rule and its associated behavioral compliance vanished overnight, the community would eventually re-inhabit the hazardous zone, leading to a recurrence of the disasters the rule was designed to prevent, causing significant loss of life and property.
% FOUNDING_PROBLEM: Recurring catastrophic landslides and floods in a specific area, leading to significant loss of life and property, which threatened the long-term viability of the community.
% FOUNDING_PROBLEM_CORROBORATION: Geological surveys confirm the persistent natural hazard. Historical records of past disasters and oral histories from community elders (outside the direct beneficiaries of current land-use decisions) corroborate the rule's origin and the ongoing threat.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope due to its genuine coordination function (disaster prevention) and low extractiveness, as the costs are accepted for collective safety. Suppression is low because compliance is largely internalized through shared memory and social norms, rather than active coercion. Theater ratio is very low, indicating high functional integrity. The slight fluctuations in measurements over time reflect the natural ebb and flow of institutional memory and community vigilance, but overall stability in compliance.
 *
 * PERSPECTIVAL GAP:
 *   The 'behavioral_competence' reading sees the rule as a vital, active mechanism for survival, where compliance is a testament to collective wisdom. The 'commemorative_husk' reading, by contrast, would perceive the same physical marker as a mere relic, its original function lost to time and its behavioral force decayed. This divergence is rooted in differing interpretations of the community's current relationship to its past and the perceived immediacy of the hazard.
 *
 * DIRECTIONALITY LOGIC:
 *   Community residents are beneficiaries of safety but also bear the costs of foregone land use, placing them near symmetric. Institutional memory keepers are agenda-setters, benefiting from their valued role in maintaining community resilience. Younger generations are beneficiaries who inherit the safety but also the restrictions, relying on the memory keepers. Developers are excluded, bearing the cost of denied access to valuable land.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commemorative_husk_ambiguity,
    'Is the ''Stone Land-Use Rule'' genuinely a live behavioral prohibition, or has it decayed into a mere commemorative husk without active behavioral force?',
    'Empirical observation of land-use decisions in the prohibited zone over time, coupled with ethnographic studies of community members'' stated reasons for compliance or non-compliance. If development attempts increase or compliance becomes purely performative, the ''commemorative_husk'' reading gains support.',
    'If resolved as a ''commemorative_husk'', the constraint''s extractiveness would be higher (as the costs are borne for no functional benefit), suppression would be lower (as it''s not actively enforced), and its classification would shift towards Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemorative_husk_ambiguity, empirical, 'Distinguishing between active behavioral compliance and symbolic adherence to the land-use rule.').

omega_variable(
    intergenerational_knowledge_decay,
    'How effectively is the knowledge of the original hazard and the rule''s rationale transmitted across generations, particularly as direct memory of the disaster fades?',
    'Longitudinal studies of knowledge retention among younger generations, assessing their understanding of the hazard and the rule''s purpose, compared to older generations. Analysis of educational practices and storytelling traditions.',
    'If knowledge decay is significant, the rule''s persistence may become more reliant on passive habit or external enforcement, increasing its effective suppression and potentially shifting its classification towards a Tangled Rope or Piton as its coordination function weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_decay, empirical, 'The rate at which the founding knowledge supporting the rule erodes across generations.').

omega_variable(
    economic_pressure_threshold,
    'At what point would increasing economic pressure for land development override the community''s behavioral compliance with the rule?',
    'Comparative case studies of similar communities facing varying levels of economic development pressure, or agent-based modeling simulating economic incentives against social norms. Observing actual attempts to challenge or circumvent the rule due to economic factors.',
    'If economic pressure reaches a critical threshold, the rule''s low extractiveness and suppression could rapidly increase as compliance breaks down or requires active, coercive enforcement, potentially reclassifying it as a Snare or Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_pressure_threshold, empirical, 'The tipping point at which economic incentives overcome ingrained land-use norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ston_tr_t15, stone_land_use_rule__behavioral_competence, theater_ratio, 15, 0.06).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__behavioral_competence, theater_ratio, 30, 0.07).
narrative_ontology:measurement(ston_tr_t45, stone_land_use_rule__behavioral_competence, theater_ratio, 45, 0.07).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.06).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__behavioral_competence, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(ston_be_t45, stone_land_use_rule__behavioral_competence, base_extractiveness, 45, 0.15).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__behavioral_competence, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(ston_su_t45, stone_land_use_rule__behavioral_competence, suppression_requirement, 45, 0.23).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.21).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'stone_land_use_rule' kernel, focusing on its active behavioral efficacy. The 'commemorative_husk' reading is a sibling that views the rule as a symbolic relic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
