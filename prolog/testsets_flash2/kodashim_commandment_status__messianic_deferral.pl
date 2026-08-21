% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'messianic deferral' reading of the
 *   Kodashim commandment status, where the sacrificial laws are considered
 *   temporally suspended but not obsolete. Study of these laws is maintained
 *   as a form of readiness for their future restoration in the Messianic era.
 *   This reading generates moderate extractiveness from the opportunity cost
 *   of diverting resources from present needs, and relies on active
 *   enforcement (rabbinic authority, communal norms) to maintain its focus on
 *   future contingency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.6).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'b0fc954b-16a6-4d91-a301-2d737199712a').
narrative_ontology:cs_kernel_codification('b0fc954b-16a6-4d91-a301-2d737199712a', fixed_text).
narrative_ontology:cs_authority_grounding('b0fc954b-16a6-4d91-a301-2d737199712a', lineage).
narrative_ontology:cs_interpretation_layer_present('b0fc954b-16a6-4d91-a301-2d737199712a').
narrative_ontology:cs_reading_relation('b0fc954b-16a6-4d91-a301-2d737199712a', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('b0fc954b-16a6-4d91-a301-2d737199712a', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('b0fc954b-16a6-4d91-a301-2d737199712a', foundational, commandment_temporally_suspended_not_obsolete).
narrative_ontology:cs_axiom_status(commandment_temporally_suspended_not_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('b0fc954b-16a6-4d91-a301-2d737199712a', commandment_temporally_suspended_not_obsolete, theological).
narrative_ontology:cs_axiom('b0fc954b-16a6-4d91-a301-2d737199712a', foundational, study_maintains_readiness_for_future_restoration).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_future_restoration, holdable).
narrative_ontology:cs_axiom_grounding('b0fc954b-16a6-4d91-a301-2d737199712a', study_maintains_readiness_for_future_restoration, theological).
narrative_ontology:cs_reference_frame('b0fc954b-16a6-4d91-a301-2d737199712a', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('b0fc954b-16a6-4d91-a301-2d737199712a', contemporary_secular_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b0fc954b-16a6-4d91-a301-2d737199712a', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_movements).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the laws of Kodashim, emphasizing their future relevance and the importance of study as preparation for the Messianic era. Their authority and intellectual careers are bound to the continuity of this tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the deferral reading as it provides a framework for their aspirations and justifies their focus on future redemption. The continued study of Kodashim reinforces their worldview and gives their movement a concrete, if deferred, purpose.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_movements, beneficiary,
    organized, civilizational, identity_locked, global).

% Represents the opportunity cost of resources (time, intellectual effort, communal focus) diverted from addressing immediate social, ethical, or spiritual needs to the study of presently inoperative laws. These needs are subordinated to future readiness.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, present_generation_needs).

% Are expected to engage in the study of Kodashim, even if they cannot perform the commandments. This requires significant time and intellectual investment, which may feel abstract or disconnected from their daily lives, creating a burden without direct present benefit.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Analyze the sociological and theological implications of maintaining a deferred commandment, questioning whether the emphasis on future restoration serves to maintain institutional power or genuinely prepares for a future state.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, critical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous intellectual and spiritual tradition around the laws of Kodashim, ensuring that knowledge and readiness for their restoration are preserved across generations, coordinating the community's focus on a shared future vision.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual capital (time, focus, scholarly effort) from the present generation's immediate needs and concerns towards the maintenance of a deferred future religious practice, primarily benefiting rabbinic scholars and messianic movements.
% ABSENT_VOICES: Those who prioritize immediate social justice, ethical action, or contemporary spiritual fulfillment over the study of presently inoperative laws. Their concerns are often framed as lacking proper religious perspective or commitment to tradition.
% DISAPPEARANCE_RATIONALE: If the messianic deferral reading vanished, the intellectual and institutional infrastructure built around Kodashim study would collapse, messianic movements would lose a key theological anchor, and communal resources would likely reorient towards present-day concerns, fundamentally altering the religious landscape.
% FOUNDING_PROBLEM: The destruction of the Temple rendered the sacrificial commandments inoperative, creating a crisis of religious practice and continuity for a central aspect of Jewish law.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and historical texts attest to the crisis of Temple destruction. While some contemporary scholars question the *necessity* of maintaining the study in its current form, the historical problem of inoperative commandments remains a live theological challenge for all branches of Judaism.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the significant opportunity cost of intellectual and communal resources dedicated to a practice that cannot be performed in the present. Suppression (0.6) is also moderate, as rabbinic authority and communal expectations actively enforce the importance of this study, discouraging alternative interpretations or a shift in focus. Theater ratio (0.1) is low, as the study is genuinely seen as preparation, not mere performance. Accessibility collapse is high (0.7) because the theological framework makes it difficult to conceive of alternatives to this deferral, and resistance is low (0.15) due to the strong communal and intellectual commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and messianic movements, this is a necessary and beneficial coordination mechanism for preserving tradition and preparing for redemption. From the perspective of present-generation needs and some lay adherents, it represents a diversion of resources and an abstract burden, making it feel more extractive. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and messianic movements are beneficiaries, as their authority and worldview are reinforced by this reading. Present-generation needs and lay adherents are victims, bearing the opportunity costs and intellectual burdens. The 'identity_locked' exit for scholars and movements reflects their deep commitment to this tradition, making exit unthinkable without a fundamental reorientation of their identity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'How can the opportunity cost of dedicating resources to Kodashim study, versus addressing present-day needs, be more precisely quantified?',
    'Sociological studies of resource allocation within religious communities, economic analysis of time/effort investment, and surveys of adherent priorities.',
    'A higher quantified opportunity cost would increase the measured extractiveness, potentially shifting the classification towards a Snare if the coordination function is deemed insufficient to justify the cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantifying the true cost of deferred practice.').

omega_variable(
    future_restoration_likelihood,
    'What is the empirical likelihood of the Temple''s restoration and the re-institution of sacrificial practices?',
    'Geopolitical analysis, archaeological findings, and shifts in religious-political movements. This is an empirical question about future events.',
    'A demonstrably low likelihood would undermine the ''readiness'' justification, increasing the perceived theater ratio and extractiveness, potentially reclassifying it as a Piton or Snare if the deferral is seen as purely inertial or extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_restoration_likelihood, empirical, 'The empirical basis for the ''future restoration'' claim.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for rabbinic scholars and messianic movements a genuine internal commitment versus a structural coercion enforced by institutional norms?',
    'Comparative studies of scholars in traditions with and without such deferred practices, analysis of career paths and social sanctions for those who deviate from the deferral reading.',
    'If primarily coercive, the suppression metric would be higher, and the directionality for these beneficiaries would shift towards symmetric or even target, indicating a more extractive constraint even for those who appear to benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, conceptual, 'Distinguishing genuine identity commitment from institutional coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.08).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.09).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.1).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.1).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.1).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__messianic_deferral, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__messianic_deferral, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__messianic_deferral, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
