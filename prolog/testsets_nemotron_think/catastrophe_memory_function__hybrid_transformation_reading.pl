% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Hybrid Ritual Function: Mourning-Practice and Survival-Competence Transmission
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story captures the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel. The reading asserts that ritual
 *   structures like the Passover seder are not merely commemorative
 *   (mourning-practice, D1/D4) nor merely instructional (survival-competence,
 *   D5) but a single integrated technology that does both simultaneously: the
 *   bitter herbs ARE the rehearsal. The constraint is the ritual form itself
 *   — its obligatory performance, its prescribed elements, its boundary
 *   maintenance. The claimed type is tangled_rope because the ritual
 *   genuinely coordinates (transmits adaptive capacity, maintains group
 *   coherence) AND extracts asymmetrically (obligatory emotional labor,
 *   exclusion of outsiders, authority concentration in tradition-bearers).
 *   Active enforcement is required: communities police participation,
 *   authorities police interpretation, boundaries police membership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Hybrid Ritual Function: Mourning-Practice and Survival-Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '44b83274-e5f0-4dd6-83d2-9fab6bfca060').
narrative_ontology:cs_kernel_codification('44b83274-e5f0-4dd6-83d2-9fab6bfca060', formalized).
narrative_ontology:cs_authority_grounding('44b83274-e5f0-4dd6-83d2-9fab6bfca060', lineage).
narrative_ontology:cs_interpretation_layer_present('44b83274-e5f0-4dd6-83d2-9fab6bfca060').
narrative_ontology:cs_reading_relation('44b83274-e5f0-4dd6-83d2-9fab6bfca060', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('44b83274-e5f0-4dd6-83d2-9fab6bfca060', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('44b83274-e5f0-4dd6-83d2-9fab6bfca060', foundational, ritual_integrates_mourning_and_survival).
narrative_ontology:cs_axiom_status(ritual_integrates_mourning_and_survival, holdable).
narrative_ontology:cs_axiom_grounding('44b83274-e5f0-4dd6-83d2-9fab6bfca060', ritual_integrates_mourning_and_survival, conventional).
narrative_ontology:cs_axiom('44b83274-e5f0-4dd6-83d2-9fab6bfca060', foundational, embodied_rehearsal_requires_loss_memory).
narrative_ontology:cs_axiom_status(embodied_rehearsal_requires_loss_memory, holdable).
narrative_ontology:cs_axiom_grounding('44b83274-e5f0-4dd6-83d2-9fab6bfca060', embodied_rehearsal_requires_loss_memory, instrumental).
narrative_ontology:cs_reference_frame('44b83274-e5f0-4dd6-83d2-9fab6bfca060', hybrid_ritual_function).
narrative_ontology:cs_drift_state('44b83274-e5f0-4dd6-83d2-9fab6bfca060', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44b83274-e5f0-4dd6-83d2-9fab6bfca060', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, tradition_bearing_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_participants_as_identity_holders).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, obligated_participants_bearing_emotional_labor).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, marginalized_members_excluded_by_ritual_boundary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, core_practitioner_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_innovators_reformers).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, core_practitioner_community).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, peripheral_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_as_dual_function_memory_technology).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_enables_institutional_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious authorities (rabbis, priests, elders) who curate, transmit, and adjudicate the ritual form. They bear responsibility for preserving the ritual's integrity across generations. Their authority is constituted through the ritual itself — they are 'made' by the tradition they guard. Exit would mean renunciation of vocational identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, tradition_bearing_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Committed community members for whom the ritual is a primary site of identity formation. They receive the adaptive mechanisms (survival competence) and loss-memory (mourning practice) as constitutive goods. They also bear the costs: obligatory participation, emotional labor of re-enactment, cognitive load of interpretation. Exit is experienced as identity fracture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, core_practitioner_community, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, core_practitioner_community, payer).

% Those who participate due to family, social, or cultural pressure but do not fully inhabit the ritual's meaning-frame. They bear the time, emotional, and conformity costs without the identity-integration benefits. Exit is socially costly (family rupture, community exclusion) but structurally possible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, peripheral_participants, payer,
    moderate, biographical, constrained, regional).

% Actors who adapt the ritual form (feminist seders, secular commemorations, interfaith adaptations). They benefit from the ritual's adaptive capacity — its D5 function — while reshaping its D1/D4 mourning content. They have mobile exit: they can create parallel ritual spaces.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_innovators_reformers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, ritual_innovators_reformers, agenda_setter).

% Those structurally excluded by the ritual's boundary norms (non-members, apostates, those who fail purity/lineage criteria). They would object to the ritual's boundary-maintenance function if present. Their exclusion is the ritual's condition of coherence — the 'bitter herbs' require a bounded 'we' who taste them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, excluded_outsiders, excluded,
    powerless, immediate, trapped, local).

% Scholars of ritual theory, collective memory, and religious studies who analyze the hybrid structure across traditions. They neither pay nor collect; they map the constraint's operation. Their analytical exit is costless but their frame shapes how the constraint is understood in broader discourse.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dual problem of (1) transmitting catastrophe memory across generations without dissolution of group identity (D1/D4: mourning-practice as boundary-maintenance) and (2) encoding adaptive survival mechanisms into embodied practice so the group can transform under catastrophe (D5: survival-competence as rehearsal). The Passover seder instantiates this: bitter herbs encode the loss; the performative retelling rehearses the exit.
% TRANSFER_FUNCTION: Moves three things: (a) emotional labor — participants re-enact catastrophe affect (bitter herbs, recitation of plagues) and this labor flows upward to sustain the tradition's vitality; (b) interpretive authority — elders/authorities control the authorized reading, and this authority flows downward as normative constraint; (c) adaptive information — the ritual encodes 'how we survived' as actionable schema (haste, preparation, selective memory) that flows laterally to each generation.
% ABSENT_VOICES: The dead of the catastrophe itself — the original victims whose loss the ritual memorializes. They cannot consent to how their memory is encoded. Also absent: those who perished because the adaptive mechanisms failed (the ones who didn't make it out). Their absence is structural: the ritual transmits the survivors' schema, not the non-survivors' experience.
% DISAPPEARANCE_RATIONALE: If the hybrid ritual vanished overnight, the tradition-bearing community would lose its primary technology for (a) calibrating collective grief without identity collapse and (b) transmitting catastrophe-survival schemas embodied rather than propositional. New memory-practices would emerge but would lack the embodied rehearsal component (D5) — the difference between reading a manual and running a fire drill. The community's catastrophe-response capacity would degrade measurably within one generation.
% FOUNDING_PROBLEM: How does a people survive catastrophe without either (a) dissolving into traumatic amnesia (forgetting the loss) or (b) freezing into traumatic fixation (becoming the wound)? The hybrid ritual was built to hold both: the mourning practice prevents amnesia; the survival rehearsal prevents fixation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by comparative ritual scholars (Bell, Rappaport, Handler) who document cross-cultural hybrid structures; by Holocaust survivors' testimony on Passover's function in displacement (Wiesel, Frankl); by indigenous elders describing ceremony as 'survival rehearsal' (Deloria, Cajete). Not solely attested by tradition authorities — the corroboration comes from outside the benefiting priestly/authority class.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but bounded costs: time, emotional labor, conformity pressure — not life-or-death extraction. Suppression (0.38) reflects social enforcement of participation and interpretation, not state coercion. Theater ratio (0.28) acknowledges that some ritual performance becomes hollow repetition, but the core remains functionally engaged. Accessibility collapse (0.52) — alternatives exist (secular memorials, therapy, historical study) but they lack the embodied rehearsal and communal calibration the ritual provides. Resistance (0.45) — reform movements, secularization, feminist/queer reinterpretations show active contestation. The measurement series shows gradual extraction accumulation and enforcement intensification over the interval, consistent with ritual institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the ritual is Mountain-like — natural law of the tradition. From the peripheral participant seat, it is Snare-like — obligatory performance with no exit. From the innovator seat, it is Rope-like — a coordination tool they can adapt. From the scholar seat, it is Tangled Rope — the hybrid structure is visible. The engine computes this divergence; the authored claim (tangled_rope) reflects the analytical seat's structural reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-bearing authorities are structural beneficiaries (d near 0.15) — they collect interpretive authority and vocational identity from the ritual. Core practitioners are near-symmetric (d ~0.45) — they pay emotional labor but receive identity-integration and adaptive schemas. Peripheral participants are targets (d ~0.75) — they pay costs without full benefits. Ritual innovators are beneficiaries with agenda-setter secondary role (d ~0.3) — they extract adaptive value while reshaping the form. Excluded outsiders are trapped (d ~0.9) — the ritual's boundary IS their exclusion. Scholars are analytical (d=0.5). The engine computes per-seat types from this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (surviving catastrophe without amnesia or fixation) remains live — new catastrophes (climate, displacement, pandemics) reactivate it. The ritual has not atrophied into piton because its D5 function (adaptive rehearsal) is still exercised in crises. However, theater ratio rise signals risk: when the mourning-practice (D1/D4) becomes purely performative without the survival-competence (D5) being tested, the constraint drifts toward piton. The hybrid reading prevents mislabeling: a pure mourning reading would miss the adaptive function; a pure survival reading would miss the grief-calibration function. Both single-function readings would misclassify the constraint's extraction profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the hybrid_transformation_reading a distinct structural reading of the catastrophe_memory_function kernel, or a synthesis that collapses the sibling readings'' distinctions?',
    'Test whether communities holding the hybrid reading produce ritual forms that cannot be decomposed into mourning-practice + survival-competence modules. If every hybrid ritual decomposes, the reading is synthetic not structural.',
    'If synthetic, the constraint story should be decomposed into two linked stories (mourning + survival) per ε-invariance. If structural, the hybrid story stands as a single constraint with its own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the hybrid reading describes a genuinely unified constraint or a conceptual synthesis of two constraints.').

omega_variable(
    extraction_as_necessary_cost,
    'Is the measured extractiveness (emotional labor, obligatory participation) the price of the coordination function (embodied rehearsal), or separable rent extracted by authorities?',
    'Compare ritual forms with high vs. low authority centralization. If extraction tracks authority concentration not rehearsal intensity, it''s rent. If extraction tracks rehearsal intensity across all forms, it''s necessary cost.',
    'If necessary cost, the constraint leans rope; if separable rent, it leans snare. The tangled_rope claim requires both to be present simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_as_necessary_cost, empirical, 'Whether extraction is inherent to the hybrid coordination function or imposed by authority structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (communal enforcement, exclusion) or internalized (participants'' self-policing through identity fusion)?',
    'Post-exit suppression trajectory: if former participants continue self-enacting ritual emotions after leaving the community, suppression has internalized component.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_transformation_reading) and its two siblings form the catastrophe_memory_function constraint family. The mourning_practice_reading isolates D1/D4 (bitter herbs as pure loss-memory); the survival_competence_reading isolates D5 (seder as pure rehearsal). This reading asserts the Passover structure integrates both in a single non-decomposable mechanism — the bitterness IS the rehearsal. The ε values differ: mourning_reading ε≈0.25 (lower extraction, primarily coordination), survival_reading ε≈0.35 (moderate extraction, instrumental), hybrid_reading ε=0.42 (highest extraction, dual function demands more labor).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, organized, 0.45).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, moderate, 0.65).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, powerless, 0.9).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
