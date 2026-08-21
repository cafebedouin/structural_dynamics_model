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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation (Mourning Practice Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice focused on preserving the
 *   memory of a past catastrophe and reinforcing collective identity. It is a
 *   'mourning practice reading' of the broader
 *   'catastrophe_memory_preservation' kernel, emphasizing symbolic continuity
 *   and identity formation over the transmission of operational survival
 *   skills. Participation is largely voluntary, and the constraint functions
 *   primarily to coordinate shared meaning and social cohesion without
 *   significant material extraction or active suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation (Mourning Practice Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'd2b1d98e-c7d8-40c3-97b0-3186fbe8671c').
narrative_ontology:cs_kernel_codification('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', implicit).
narrative_ontology:cs_authority_grounding('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', practice).
narrative_ontology:cs_interpretation_layer_present('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c').
narrative_ontology:cs_reading_relation('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', foundational, symbolic_continuity_is_primary_function).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', symbolic_continuity_is_primary_function, deontological).
narrative_ontology:cs_axiom('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', foundational, operational_transfer_is_absent).
narrative_ontology:cs_axiom_status(operational_transfer_is_absent, holdable).
narrative_ontology:cs_axiom_grounding('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', operational_transfer_is_absent, empirically_contingent).
narrative_ontology:cs_reference_frame('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', collective_identity_through_shared_narrative).
narrative_ontology:cs_drift_state('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', contemporary_secular_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d2b1d98e-c7d8-40c3-97b0-3186fbe8671c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participants in the ritual who gain a sense of collective identity, shared meaning, and social cohesion. Their participation is voluntary, and they can leave the group or cease participation without direct coercion, though social ties may be affected.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, in_group_members, beneficiary,
    moderate, biographical, mobile, local).

% Individuals responsible for organizing, guiding, and preserving the ritual practices. They maintain the tradition and interpret its meaning for the community. Their role is bound by the community's expectations and the tradition itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_leaders, agenda_setter,
    organized, biographical, constrained, local).

% Academics (e.g., anthropologists, historians, sociologists) who study the ritual's function, evolution, and impact on collective memory and identity. They are outside the direct practice but analyze its structural properties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective memory of a past catastrophe and the formation of a shared group identity across generations, providing a common symbolic framework and fostering social cohesion.
% TRANSFER_FUNCTION: Transfers symbolic meaning, shared narratives, and social bonds among participants, reinforcing collective identity. There is no operational transfer of material resources or practical skills.
% ABSENT_VOICES: Individuals or groups who reject the historical narrative commemorated by the ritual, or who do not identify with the community it defines. They are simply outside the practice, not actively suppressed or excluded from a conversation they wish to join.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the primary mechanism for transmitting the collective memory of the catastrophe and reinforcing the group's identity would disappear. This would lead to the erosion of shared meaning, fragmentation of the community, and a dissipation of the common past, requiring the group to reorganize its identity and memory practices.
% FOUNDING_PROBLEM: To ensure the memory of a past catastrophe and its lessons are preserved across generations, and to maintain the collective identity of the group that was forged or defined by that event.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians, acting as external observers, corroborate the vital role of ritual in sustaining collective memory and identity, citing extensive cross-cultural evidence of similar practices. Community elders and ritual leaders also attest to the ongoing necessity of the practice for group cohesion.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.22, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low-to-moderate because while participation may involve time and emotional investment, it is generally opt-in and provides clear benefits in terms of identity and belonging. Suppression is low as there are no active enforcement mechanisms to compel participation or punish non-adherence beyond social consequences. The theater ratio is moderate, reflecting the inherently symbolic and performative nature of ritual, where the 'function' is often the performance itself. The slight increase in extractiveness and theater over time reflects a potential drift towards more performative aspects as direct memory fades, and social pressure to conform might subtly increase.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of in-group members, the ritual is a vital source of meaning and belonging, a pure coordination mechanism. From an external analytical perspective, its symbolic nature might be seen as having a higher 'theater ratio' compared to a purely functional constraint, but its coordination of identity remains clear.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group members are beneficiaries, gaining collective identity and cohesion. Ritual leaders act as agenda-setters, guiding the practice. There are no identifiable victims as participation is voluntary and no material extraction occurs. External observers analyze the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the ritual as pure extraction by emphasizing its genuine coordination function for collective identity and memory. While the 'hybrid_atrophy_reading' might suggest a decline from a more functional past, this 'mourning_practice_reading' asserts the current, stable function of symbolic preservation, which is a valid form of coordination. The low extractiveness and suppression support its classification as a Rope, indicating that its mandate for identity preservation is still live and beneficial to participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_primary_function_ambiguity,
    'Is this ritual primarily about symbolic continuity and collective identity (mourning practice reading), or does it retain latent operational survival competence (survival competence reading)?',
    'Empirical study of ritual content and participant outcomes: if participants demonstrate measurable improvements in practical threat-recognition or response skills, it supports the survival competence reading. If outcomes are primarily psychological and social, it supports the mourning practice reading.',
    'If the ritual retains significant operational survival competence, its classification might shift towards a different type (e.g., a Tangled Rope if there''s asymmetric extraction for that competence, or a Rope if it''s pure coordination for competence). If it is purely symbolic, the current Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_primary_function_ambiguity, conceptual, 'Ambiguity regarding the ritual''s primary function within the catastrophe memory preservation kernel.').

omega_variable(
    participation_voluntariness_ambiguity,
    'Is participation in the ritual truly voluntary, or are there subtle social pressures or identity-based costs for non-participation that elevate effective suppression?',
    'Sociological study of community dynamics and individual narratives of non-participants or ex-participants to identify implicit social sanctions or identity erosion associated with non-adherence.',
    'If significant social pressures or identity costs are identified, the effective suppression and extractiveness for in-group members would be higher than currently measured, potentially shifting the constraint towards a Tangled Rope if these pressures are asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(participation_voluntariness_ambiguity, empirical, 'Ambiguity regarding the true voluntariness of ritual participation and its impact on effective suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_preservation' kernel. This 'mourning_practice_reading' focuses on symbolic continuity and collective identity, distinct from the 'survival_competence_reading' (focused on practical skills) and the 'hybrid_atrophy_reading' (focused on historical evolution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
