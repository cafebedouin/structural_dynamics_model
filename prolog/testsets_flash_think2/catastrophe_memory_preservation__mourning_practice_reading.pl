% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation (Mourning Practice Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice focused on preserving the
 *   memory of a past catastrophe, primarily serving to maintain symbolic
 *   continuity and collective identity within a group. It is characterized by
 *   voluntary participation and the absence of direct operational transfer,
 *   functioning as a coordination mechanism for shared meaning and belonging.
 *   This is the 'mourning_practice_reading' of the broader
 *   'catastrophe_memory_preservation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation (Mourning Practice Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98').
narrative_ontology:cs_kernel_codification('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', implicit).
narrative_ontology:cs_authority_grounding('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', practice).
narrative_ontology:cs_interpretation_layer_present('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98').
narrative_ontology:cs_reading_relation('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', foundational, symbolic_continuity_is_identity).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', symbolic_continuity_is_identity, conventional).
narrative_ontology:cs_reference_frame('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', collective_identity_maintenance).
narrative_ontology:cs_drift_state('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', contemporary_secular_context, gap(stable, minor, false)).
narrative_ontology:cs_created_at('df3bbb55-cfcc-42d6-8b1a-3a085ebe9c98', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__mourning_practice_reading, in_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participants in the ritual who gain a sense of collective identity, shared memory, and emotional processing of past catastrophes. They 'pay' through their time, emotional investment, and adherence to ritual forms. Exiting means losing a core aspect of their social and personal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, in_group_members, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, in_group_members, payer).

% Those who guide and interpret the ritual, ensuring its proper execution and transmission across generations. Their authority is derived from their role in preserving the collective memory and identity. Exiting would mean abandoning their community role and spiritual calling.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_leaders, agenda_setter,
    organized, generational, constrained, regional).

% Academics (e.g., anthropologists, sociologists) who study the ritual's function in preserving memory and identity. They analyze its structure and effects without direct participation or benefit from its internal cohesion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% The broader societal context that does not participate in or necessarily recognize the ritual's specific function. Its existence provides alternative frameworks for identity and memory, but it does not actively suppress the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, secular_society, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective memory of a catastrophic event, fostering shared identity, emotional processing, and social cohesion among in-group members across generations.
% TRANSFER_FUNCTION: Transfers symbolic meaning, shared emotional experience, and a sense of belonging from the ritual practice to its participants. No direct material or operational transfer.
% ABSENT_VOICES: Individuals who have left the group or never joined, finding their identity and meaning through secular or alternative frameworks. They might view the ritual as anachronistic or irrelevant, but their absence does not directly challenge its internal function.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the collective identity and shared memory of the catastrophe, which are central to the group's cohesion and self-understanding, would significantly fragment and dissipate, leading to a profound reorganization of the group's social structure.
% FOUNDING_PROBLEM: To prevent the memory of a foundational catastrophic event from fading into oblivion, thereby ensuring the continuity of the group's identity, solidarity, and shared understanding of its history across successive generations.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of collective memory and social cohesion, as well as historical analyses of the group's persistence, corroborate that the need for such memory preservation remains live, independent of the ritual leaders' self-assertion.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low-to-moderate (0.25) because participation is largely voluntary, and the 'cost' is primarily time and emotional investment, not material extraction. Suppression is low (0.15) as there are no active enforcement mechanisms beyond social cohesion and identity-based pressure. The theater ratio is moderate (0.4) reflecting the inherently symbolic and performative nature of ritual in maintaining continuity. Accessibility collapse and resistance are low, consistent with an opt-in coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of in-group members, the ritual is a vital, low-cost mechanism for preserving their identity. From an external, secular perspective, it might appear as a purely symbolic act with no 'real' function, or even as a form of social inertia. The engine's classification will reflect the internal coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group members are both beneficiaries (gaining cohesion and identity) and payers (investing time/emotion), placing them near symmetric, though slightly benefiting due to the net positive outcome of identity formation. Ritual leaders are agenda-setters, facilitating the coordination. External observers are analytical, and secular society is excluded, as their frameworks are outside the ritual's scope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntariness_vs_identity_lock,
    'To what extent is participation in the ritual truly voluntary, versus being driven by an identity-lock mechanism where exiting means losing a core aspect of self?',
    'Longitudinal studies of ex-members'' post-exit identity formation and social integration, or comparative analysis with groups where similar rituals are genuinely optional.',
    'If identity-lock is strong, the effective suppression and extractiveness for in-group members would be higher than currently measured, potentially shifting the classification towards a Tangled Rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_vs_identity_lock, empirical, 'Ambiguity in the degree of voluntariness due to identity fusion.').

omega_variable(
    symbolic_efficacy_vs_operational_competence,
    'Does the ritual, beyond symbolic continuity, also implicitly preserve any operational threat-recognition or survival competence, as suggested by the ''survival_competence_reading''?',
    'Empirical studies correlating ritual adherence with actual group resilience or adaptive capacity in the face of new threats, or historical analysis of past crises.',
    'If operational competence is preserved, the constraint''s coordination function would be broader, and its value proposition higher, potentially shifting its classification towards a stronger Rope or even a Scaffold if the competence is transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_efficacy_vs_operational_competence, empirical, 'Ambiguity regarding the ritual''s functional scope beyond symbolic preservation.').

omega_variable(
    atrophy_vs_primary_function,
    'Is the current ''mourning practice'' function the ritual''s primary and intended role, or has it atrophied from a prior, more ''competence-preserving'' function, as posited by the ''hybrid_atrophy_reading''?',
    'Historical and archaeological evidence of the ritual''s evolution, comparing its forms and stated purposes across different eras.',
    'If significant atrophy is confirmed, the constraint might be reclassified as a Piton (if the original function is entirely lost but the ritual persists by inertia) or a Tangled Rope (if a new, less beneficial function has emerged).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_primary_function, empirical, 'Ambiguity regarding the historical evolution and functional shift of the ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.23).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 30, 0.16).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_preservation' kernel. It focuses on the ritual's role in maintaining symbolic continuity and collective identity, distinct from readings that emphasize operational competence or historical atrophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
