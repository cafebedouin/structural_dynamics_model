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
 *   This constraint describes a specific reading of ritual's function: its
 *   role in transmitting survival competence and adaptive capacity across
 *   generations, particularly in the context of past catastrophes. It focuses
 *   on how commemorative rituals, like Passover, embody and rehearse
 *   strategies for institutional transformation and decentralized continuity,
 *   rather than solely mourning or identity maintenance. This reading
 *   emphasizes the practical, adaptive function of ritual memory.
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
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival-Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'd87daa6f-91ef-48d2-9898-6e711bfb9c56').
narrative_ontology:cs_kernel_codification('d87daa6f-91ef-48d2-9898-6e711bfb9c56', formalized).
narrative_ontology:cs_authority_grounding('d87daa6f-91ef-48d2-9898-6e711bfb9c56', lineage).
narrative_ontology:cs_interpretation_layer_present('d87daa6f-91ef-48d2-9898-6e711bfb9c56').
narrative_ontology:cs_reading_relation('d87daa6f-91ef-48d2-9898-6e711bfb9c56', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d87daa6f-91ef-48d2-9898-6e711bfb9c56', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('d87daa6f-91ef-48d2-9898-6e711bfb9c56', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('d87daa6f-91ef-48d2-9898-6e711bfb9c56', ritual_as_adaptive_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('d87daa6f-91ef-48d2-9898-6e711bfb9c56', foundational, decentralized_continuity_through_embodiment).
narrative_ontology:cs_axiom_status(decentralized_continuity_through_embodiment, holdable).
narrative_ontology:cs_axiom_grounding('d87daa6f-91ef-48d2-9898-6e711bfb9c56', decentralized_continuity_through_embodiment, conventional).
narrative_ontology:cs_reference_frame('d87daa6f-91ef-48d2-9898-6e711bfb9c56', adaptive_transmission_paradigm).
narrative_ontology:cs_drift_state('d87daa6f-91ef-48d2-9898-6e711bfb9c56', contemporary_secularization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d87daa6f-91ef-48d2-9898-6e711bfb9c56', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, internalizing the lessons of past catastrophe and rehearsing adaptive responses. They gain resilience and a sense of preparedness for future challenges, but are constrained by the social obligation to participate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, community_members, beneficiary,
    organized, biographical, constrained, local).

% Inherit the adaptive knowledge and survival strategies encoded in the ritual, which prepares them for potential future crises. Their identity is shaped by this transmitted memory, making exit from the tradition difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, identity_locked, local).

% Facilitate the ritual, ensuring its correct performance and the accurate transmission of its survival lessons. They bear the responsibility of preserving the tradition but also gain status and influence within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders, agenda_setter,
    moderate, biographical, constrained, local).

% Study the ritual's function in transmitting adaptive capacity, analyzing its effectiveness in preparing communities for future crises. They are outside the community and do not directly participate in the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and adaptive behavior, ensuring that lessons from past catastrophes are transmitted across generations, enabling the community to respond effectively to future threats.
% TRANSFER_FUNCTION: Transfers embodied knowledge, survival strategies, and a collective sense of resilience from past generations to present and future community members.
% ABSENT_VOICES: Those who prioritize individual autonomy over collective identity might object to the implicit identity-locking mechanism, arguing for more fluid forms of memory transmission. They are often outside the community or marginalized within it.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a vital mechanism for transmitting adaptive capacity and collective resilience. Future generations would be less prepared for crises, and the institutional memory for transformation would erode, leading to a more brittle social structure.
% FOUNDING_PROBLEM: The problem of how to ensure the long-term survival and adaptive capacity of a community in the face of recurring existential threats and catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians corroborate the ongoing need for adaptive capacity in communities facing environmental, social, or political instability. Community elders and leaders also attest to the live nature of this problem, citing contemporary challenges.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.15) because the primary function is coordination and benefit, not extraction. Any 'cost' is the effort of participation and adherence to tradition, which is largely self-imposed for the collective good. Suppression is minimal (0.05) as participation is largely voluntary, driven by shared belief and the perceived benefits of resilience. Theater ratio is low (0.1) because the ritual's function is genuinely active and effective in transmitting adaptive capacity, with little performative excess. Accessibility collapse is high (0.8) because the knowledge is deeply embedded in the ritual, making alternative, equally effective transmission paths difficult to access without the ritual itself. Resistance is low (0.05) as the community largely accepts the value of this function.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the same kernel (mourning_practice_reading, hybrid_transformation_reading) would emphasize different aspects, potentially leading to different extractiveness or suppression values if they focused on the emotional burden of mourning or the coercive aspects of identity maintenance. This reading, however, isolates the survival-competence function, which is largely beneficial.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, gaining adaptive capacity and resilience (low d). Ritual leaders, while bearing responsibility, also benefit from the perpetuation of the tradition and their role within it (low d). There are no direct 'victims' in this reading, as the constraint is seen as a net positive for all participants. The 'identity_locked' exit option for future generations reflects the deep integration of this memory into their collective and individual identities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_vs_mourning_primary_function,
    'Is the primary function of this ritual truly the transmission of survival competence, or is it primarily a mourning practice, with survival competence as a secondary effect?',
    'Comparative analysis of ritual content and participant testimonies: if the explicit narrative and embodied actions focus on ''how to act'' in crisis rather than ''how to grieve,'' it supports the survival-competence reading.',
    'If primarily mourning, the extractiveness might be higher (emotional cost) and the coordination function different (grief processing vs. adaptive strategy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_vs_mourning_primary_function, conceptual, 'Distinguishing the dominant function of the ritual.').

omega_variable(
    identity_lock_coercion_level,
    'To what extent does the ''identity_locked'' exit option for future generations represent a benign socialization process versus a subtle form of coercive identity fusion?',
    'Longitudinal studies of individuals who attempt to exit the tradition: if exit is met with severe social sanction or psychological distress beyond normal grief for lost community, it suggests a higher degree of coercion.',
    'If coercion is higher, the effective suppression for future generations would be higher, potentially shifting the classification for that seat towards a more extractive type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_coercion_level, empirical, 'Assessing the coercive dimension of identity-locked exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__survival_competence_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__survival_competence_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel. It focuses on the survival-competence aspect, distinct from mourning or hybrid functions. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
