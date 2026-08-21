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
    narrative_ontology:affects_constraint/2,
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
 *   commemorative practices, like Passover, serve to transmit
 *   survival-competence and adaptive capacity for institutional
 *   transformation across generations. It is a 'rope' because it genuinely
 *   coordinates collective action and memory for the benefit of the
 *   community, with minimal extraction. The ritual provides a framework for
 *   understanding and responding to future catastrophes, fostering
 *   decentralized resilience. This reading emphasizes the practical, adaptive
 *   outcomes of ritual, distinct from its roles in mourning or identity
 *   maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival-Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '11bbbb9b-dd0f-48d7-95cc-d74b3dae336d').
narrative_ontology:cs_kernel_codification('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', formalized).
narrative_ontology:cs_authority_grounding('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', practice).
narrative_ontology:cs_interpretation_layer_present('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d').
narrative_ontology:cs_reading_relation('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', foundational, ritual_transmits_adaptive_capacity).
narrative_ontology:cs_axiom_status(ritual_transmits_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', ritual_transmits_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', secondary, decentralized_continuity_is_resilient).
narrative_ontology:cs_axiom_status(decentralized_continuity_is_resilient, holdable).
narrative_ontology:cs_axiom_grounding('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', decentralized_continuity_is_resilient, instrumental).
narrative_ontology:cs_reference_frame('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', catastrophic_event_survival_framework).
narrative_ontology:cs_drift_state('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', contemporary_secularization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('11bbbb9b-dd0f-48d7-95cc-d74b3dae336d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, adaptive_capacity_of_ritual).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_resilience_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, internalizing its lessons and practices for navigating future crises. Their identity is often intertwined with the community's shared memory and adaptive strategies. Exit means losing access to this collective resilience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, community_members, beneficiary,
    organized, biographical, identity_locked, local).

% Inherit the adaptive knowledge and social structures transmitted through the ritual, which enhances their capacity to respond to unforeseen challenges. They are passive recipients of this cultural inheritance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Facilitate the ritual, ensuring its accurate transmission and interpretation. They guide the community through the commemorative practices, reinforcing the lessons of survival and transformation. Their authority is derived from their role in preserving this vital function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders, agenda_setter,
    moderate, biographical, constrained, local).

% Study the ritual's function in transmitting adaptive capacity and institutional resilience. They analyze its effectiveness in preparing communities for future catastrophes and maintaining continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and behavior around past catastrophes, transmitting practical and psychological survival strategies across generations, enabling decentralized continuity and institutional transformation.
% TRANSFER_FUNCTION: Transfers adaptive knowledge, resilience, and a shared framework for interpreting and responding to crises from past generations to present and future community members.
% ABSENT_VOICES: Those who prioritize individual autonomy over collective identity might object to the 'identity_locked' nature of participation, viewing it as a constraint on personal freedom rather than a source of resilience. Their voices are often marginalized by the strong communal bonds the ritual reinforces.
% DISAPPEARANCE_RATIONALE: If this ritual vanished, the community would lose a crucial mechanism for collective learning and adaptive capacity. Future generations would be less prepared for crises, potentially leading to greater social fragmentation and institutional collapse in the face of adversity. The community's ability to transform and persist would be severely diminished.
% FOUNDING_PROBLEM: The original community faced a catastrophic event that threatened its existence, requiring the development of strategies for survival, recovery, and the prevention of future collapse.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies and historical records from outside the immediate community corroborate the existence of foundational catastrophes and the subsequent development of ritualized responses. Sociologists of religion attest to the ongoing function of such rituals in maintaining group cohesion and adaptive capacity in the face of contemporary challenges.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the primary function is to benefit the community through knowledge transmission, not to extract resources. Suppression is low (0.2) as participation is largely voluntary and driven by a desire for collective well-being, though social pressure to conform exists. Theater ratio is low (0.1) because the ritual's actions are directly tied to its stated purpose of transmitting survival skills, with little performative excess. Accessibility collapse is high (0.7) because the ritual provides a unique, deeply embedded pathway for this specific form of adaptive learning; alternatives are less effective. Resistance is low (0.05) as the community generally perceives the ritual as beneficial.
 *
 * PERSPECTIVAL GAP:
 *   While this reading focuses on survival competence, other readings might emphasize mourning or hybrid functions. The engine's classification of this specific reading as a 'rope' reflects its low extraction and high coordination for adaptive capacity, which might differ from classifications of other readings that emphasize more extractive or identity-bound aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are direct beneficiaries, receiving adaptive knowledge and resilience. Ritual leaders act as agenda-setters, guiding the transmission process, but their role is primarily facilitative rather than extractive. External observers analyze the function without direct participation or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine adaptive transmission as mere cultural inertia or extraction. The low extractiveness and high perceived benefit indicate that the mandate (transmitting survival competence) is actively fulfilled and valued by the community, rather than being an atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_primacy,
    'Is the primary function of this ritual truly the transmission of survival competence, or is it primarily a mourning practice or identity-maintenance mechanism?',
    'Longitudinal ethnographic studies comparing communities with similar rituals but different historical contexts, assessing which function correlates most strongly with community resilience and adaptive transformation post-catastrophe.',
    'If mourning or identity maintenance is primary, the extractiveness might be higher (e.g., social costs of non-conformity) and the classification could shift towards a Tangled Rope or Snare, depending on the degree of coercion in maintaining identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_primacy, conceptual, 'Ambiguity in the primary function of the ritual (survival vs. mourning vs. identity).').

omega_variable(
    identity_lock_vs_voluntary_adherence,
    'To what extent is ''identity_locked'' exit for community_members a genuine structural constraint versus a deeply internalized, voluntary adherence to a beneficial practice?',
    'Studies of individuals who have left such communities: do they report external barriers to exit, or primarily internal psychological costs related to loss of identity and belonging? If the latter, the ''suppression'' metric might be lower than currently assessed.',
    'If adherence is more voluntary, the ''suppression'' metric would decrease, potentially reinforcing the ''rope'' classification by reducing any perceived coercive element. If external barriers are significant, the ''suppression'' would remain high, and the constraint might lean towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_adherence, empirical, 'Distinguishing structural identity-lock from strong voluntary commitment.').


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
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 60, 0.21).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel. It focuses on the transmission of survival competence, distinct from mourning or hybrid functions. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
