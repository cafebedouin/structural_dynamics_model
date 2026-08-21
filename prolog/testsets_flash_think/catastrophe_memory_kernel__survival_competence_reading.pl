% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Competence Training
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual practices within a persecuted
 *   community function to encode and transmit adaptive capacity for survival.
 *   It is one reading of the 'catastrophe_memory_kernel', focusing on the
 *   instrumental role of ritual in preparing for and responding to
 *   existential threats. The ritual serves as a form of collective survival
 *   training, ensuring that knowledge and resilience are passed across
 *   generations, even as it imposes costs on individuals and requires active
 *   enforcement of communal norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Competence Training").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '201ddd03-4993-4442-8297-636fd1ce2cd4').
narrative_ontology:cs_kernel_codification('201ddd03-4993-4442-8297-636fd1ce2cd4', implicit).
narrative_ontology:cs_authority_grounding('201ddd03-4993-4442-8297-636fd1ce2cd4', practice).
narrative_ontology:cs_interpretation_layer_present('201ddd03-4993-4442-8297-636fd1ce2cd4').
narrative_ontology:cs_reading_relation('201ddd03-4993-4442-8297-636fd1ce2cd4', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('201ddd03-4993-4442-8297-636fd1ce2cd4', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('201ddd03-4993-4442-8297-636fd1ce2cd4', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('201ddd03-4993-4442-8297-636fd1ce2cd4', foundational, ritual_as_survival_training).
narrative_ontology:cs_axiom_status(ritual_as_survival_training, holdable).
narrative_ontology:cs_axiom_grounding('201ddd03-4993-4442-8297-636fd1ce2cd4', ritual_as_survival_training, empirically_contingent).
narrative_ontology:cs_reference_frame('201ddd03-4993-4442-8297-636fd1ce2cd4', community_resilience_under_threat).
narrative_ontology:cs_drift_state('201ddd03-4993-4442-8297-636fd1ce2cd4', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('201ddd03-4993-4442-8297-636fd1ce2cd4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, community_members_bearing_ritual_costs).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, cultural_resilience_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, collective_trauma_adaptation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body that actively maintains and participates in the ritual practices. It benefits from the enhanced adaptive capacity and resilience against external threats, but also bears the collective burden of ritual adherence and the costs of non-assimilation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary).

% The recipients of the transmitted survival competence and collective memory. They inherit the adaptive strategies and resilience encoded in the ritual, but have no agency in its current maintenance or modification.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, regional).

% Individuals within the community who face strong pressure to conform to the ritual, but also experience the pull of assimilation into the dominant culture. They bear the social and emotional costs of maintaining distinct practices, and may face marginalization if they deviate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals, payer,
    powerless, biographical, constrained, local).

% The custodians and interpreters of the ritual, responsible for its correct transmission and adaptation. They benefit from the social cohesion and authority derived from their role, but also bear the heavy responsibility of ensuring the community's survival through ritual integrity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_elders_and_leaders, agenda_setter,
    powerful, generational, identity_locked, local).

% The broader societal context that exerts assimilation pressure on the persecuted community. While not directly involved in the ritual, its existence and policies create the conditions that make the ritual's survival function necessary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_dominant_culture, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and behavioral patterns for responding to existential threats, ensuring shared understanding of past catastrophes and rehearsed responses to ensure the community's survival.
% TRANSFER_FUNCTION: Transfers knowledge, emotional resilience, and practical survival strategies across generations within the community, from elders to youth, as a means of adaptive capacity.
% ABSENT_VOICES: Individuals who prioritize assimilation into the dominant culture, or those who find the ritual burdensome and question its efficacy, are often marginalized or leave the community, thus their voices are absent from the active maintenance of the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a vital mechanism for intergenerational knowledge transfer and collective identity, making it highly vulnerable to future persecutions and accelerating assimilation, fundamentally altering its survival trajectory.
% FOUNDING_PROBLEM: The existential threat of persecution and the need to preserve collective memory and adaptive strategies to ensure the community's long-term survival.
% FOUNDING_PROBLEM_CORROBORATION: Community historians, sociologists studying persecuted groups, and survivors' testimonies corroborate the ongoing relevance of persecution threats and the ritual's role in maintaining resilience. External academic analysis supports the claim that such rituals serve a vital adaptive function.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective action for survival (benefiting the community) while simultaneously imposing costs and requiring active enforcement (extracting from individuals who might prefer assimilation or find the practices burdensome). Extractiveness is moderate, reflecting the necessary costs of maintaining a distinct identity and rehearsing difficult memories, but it is not purely extractive given the functional benefit. Suppression is moderate, as social pressure and communal expectations are key to maintaining ritual adherence in the face of external assimilation pressures. The theater ratio is low, indicating that the ritual's functional purpose for survival is dominant over mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the persecuted community as a whole, the ritual is a vital coordination mechanism for survival. However, for individual members, particularly those drawn to assimilation, the same ritual can be experienced as a burdensome extraction of time, emotional energy, and social conformity. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The persecuted_community and future_generations are beneficiaries, receiving adaptive capacity and resilience. Assimilating_individuals and community_members_bearing_ritual_costs are victims, bearing the social and emotional costs of adherence and resisting assimilation. Community_elders_and_leaders act as agenda_setters, guiding the ritual's maintenance. The external_dominant_culture is an observer, whose presence creates the context for the ritual's necessity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_efficacy_empirical,
    'To what extent does the ritual actually transmit adaptive capacity, as opposed to merely preserving cultural forms?',
    'Longitudinal ethnographic studies comparing communities with varying ritual adherence under similar persecution pressures, measuring actual survival outcomes and adaptive behaviors.',
    'If efficacy is low, the constraint shifts towards a Piton (theatrical maintenance) or a Snare (pure extraction of time/resources for no functional benefit); if high, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_empirical, empirical, 'Whether the ritual tracks service cost or market power.').

omega_variable(
    burden_vs_benefit_balance,
    'Is the burden of maintaining the ritual (time, emotional cost, social pressure) proportional to the adaptive capacity it provides, from the perspective of individual participants?',
    'Surveys and qualitative interviews with community members, including those who have left or considered leaving, to assess perceived costs and benefits.',
    'If costs significantly outweigh benefits for many, the constraint leans more towards Snare (extraction); if benefits are widely perceived as outweighing costs, it reinforces Tangled Rope (coordination with necessary costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_vs_benefit_balance, preference, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    kernel_reading_focus,
    'Is this constraint primarily about transmitting survival competence, or is its dominant function boundary maintenance, symbolic continuity, or trauma encoding?',
    'Analysis of ritual content, participant testimonies, and historical context to determine the primary explicit and implicit goals of the practice within the community''s self-understanding.',
    'If the primary function is found to be different, the classification of this specific reading would be superseded by a more accurate one, potentially altering extractiveness and suppression metrics based on the new focus (e.g., boundary maintenance might imply higher suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus, conceptual, 'Ambiguity regarding the primary function of the ritual within the broader catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1970, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t1970, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(cata_be_t1980, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(cata_be_t1990, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(cata_be_t2010, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1970, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(cata_su_t1980, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1980, 0.53).
narrative_ontology:measurement(cata_su_t1990, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1990, 0.56).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(cata_su_t2010, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_kernel', focusing on ritual as a mechanism for transmitting adaptive capacity for persecution-survival. Sibling readings focus on boundary maintenance, symbolic continuity, and trauma encoding, each with distinct structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
