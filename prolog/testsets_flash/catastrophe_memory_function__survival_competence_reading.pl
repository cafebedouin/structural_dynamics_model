% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Preserves Survival-Competence (Commemorative Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a specific reading of ritual's function: that
 *   commemorative rituals, like Passover, serve to transmit survival
 *   competence and adaptive capacity across generations. It is not merely
 *   about remembering a past event, but about rehearsing the skills and
 *   mindset needed to overcome future challenges. This reading emphasizes the
 *   active, generative aspect of ritual in building resilience and
 *   facilitating institutional transformation and decentralized continuity in
 *   the face of catastrophe. The constraint is claimed as a Rope, reflecting
 *   its genuine coordination function and low extraction, as it primarily
 *   benefits participants by enhancing their collective adaptive capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival-Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '9fe1cd5e-619e-4af6-8381-e55b5b2ac182').
narrative_ontology:cs_kernel_codification('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', formalized).
narrative_ontology:cs_authority_grounding('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', lineage).
narrative_ontology:cs_interpretation_layer_present('9fe1cd5e-619e-4af6-8381-e55b5b2ac182').
narrative_ontology:cs_reading_relation('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', ritual_as_adaptive_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', foundational, intergenerational_knowledge_transmission_is_active).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transmission_is_active, holdable).
narrative_ontology:cs_axiom_grounding('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', intergenerational_knowledge_transmission_is_active, empirically_contingent).
narrative_ontology:cs_reference_frame('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', catastrophe_survival_paradigm).
narrative_ontology:cs_drift_state('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', contemporary_secular_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9fe1cd5e-619e-4af6-8381-e55b5b2ac182', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, adaptive_capacity_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, embodied_cognition_in_ritual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, internalizing its lessons and practices. They gain adaptive capacity and a sense of continuity, but are constrained by the social and cultural expectations of participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, community_members, beneficiary,
    organized, biographical, constrained, local).

% Inherit the ritual and its embedded knowledge, benefiting from the transmitted survival competence without direct participation in its creation. Their identity is shaped by the community's history and practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, identity_locked, local).

% Facilitate and interpret the ritual, ensuring its accurate transmission and relevance. They bear the responsibility of preserving the adaptive knowledge and guiding the community through its practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Study the ritual's function in transmitting survival competence, analyzing its structure, impact, and historical evolution. They do not participate in the ritual but provide external validation and theoretical frameworks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and behavior around past catastrophic events, enabling the community to transmit adaptive strategies and resilience across generations through embodied practice and narrative.
% TRANSFER_FUNCTION: Transfers embodied knowledge, emotional resilience, and practical strategies for navigating future crises from past generations to present and future community members.
% ABSENT_VOICES: Those who reject the historical narrative or the efficacy of ritual for adaptive transmission are often marginalized or leave the community; they would argue for more direct, secular forms of crisis preparation.
% DISAPPEARANCE_RATIONALE: If this ritual vanished, the community would lose a vital mechanism for collective memory and adaptive capacity. Future generations would be less prepared to face similar catastrophes, and the community's identity and resilience would be significantly diminished, leading to a reorganization of social structures and coping mechanisms.
% FOUNDING_PROBLEM: The problem of how to survive and rebuild after a catastrophic event, and how to ensure that the lessons learned are not lost but actively transmitted to future generations.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of resilient communities, historical accounts of post-catastrophe recovery, and psychological research on collective trauma and coping mechanisms corroborate the ongoing relevance of transmitting survival competence through cultural practices. These external sources attest that the problem of intergenerational adaptive capacity remains live.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15, declining to 0.10) because the primary 'cost' is participation, which is outweighed by the benefit of transmitted adaptive capacity. Suppression is very low (0.05) as participation is largely voluntary, driven by cultural and identity factors rather than coercion. Theater ratio is also low (0.1, declining to 0.05) because the ritual's function is genuinely active and instrumental in preparing for future challenges, with minimal performative overhead. The declining values over time reflect a hypothetical scenario where the direct memory of catastrophe fades, and the ritual becomes even more streamlined and efficient in its core function.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes survival competence, other readings might focus on mourning or hybrid functions. From the perspective of this 'survival competence' reading, the ritual is a net positive, low-extraction mechanism. Other readings might perceive different benefits or costs, but this story focuses solely on the D5 function.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, gaining adaptive capacity and resilience. Ritual leaders, while 'agenda setters,' also primarily serve the community's benefit by ensuring the ritual's integrity and transmission. There are no identifiable victims in this reading, as the 'costs' are diffuse and self-imposed through participation, leading to net benefits for all involved.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_ambiguity,
    'Is the primary function of this ritual truly the transmission of survival competence, or is it primarily a mourning practice, or a hybrid of both?',
    'Longitudinal ethnographic studies tracking post-catastrophe community resilience and adaptive behaviors, correlated with ritual participation and content analysis of ritual narratives for explicit adaptive instruction versus grief processing.',
    'If primarily mourning, the extractiveness might be higher (emotional labor without direct adaptive gain); if hybrid, the classification would need to account for both functions. This would shift the claimed_type and metric interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_ambiguity, conceptual, 'Ambiguity in the core function of the ritual (D5 vs. D1/D4 vs. hybrid).').

omega_variable(
    adaptive_capacity_measurement,
    'How can ''adaptive capacity for institutional transformation and decentralized continuity'' be empirically measured as an outcome of ritual participation?',
    'Development of robust sociological and psychological metrics for collective resilience, institutional flexibility, and decentralized decision-making, applied to communities with and without such rituals after experiencing similar shocks.',
    'Lack of clear empirical measurement weakens the claim of ''survival competence'' as a direct outcome, potentially increasing the perceived ''theater_ratio'' if the claimed benefit cannot be substantiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_capacity_measurement, empirical, 'Empirical challenge in measuring the claimed adaptive outcomes of ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.11).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, focusing on the transmission of survival competence. It is distinct from readings emphasizing mourning or hybrid functions, which would have different extractiveness profiles and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
