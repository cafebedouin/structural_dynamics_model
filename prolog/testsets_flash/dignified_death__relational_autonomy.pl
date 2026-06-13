% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Dignified Death: Relational Autonomy Model
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'relational autonomy' reading of dignified
 *   death, where dignity emerges from the patient's embeddedness in a network
 *   of relationships (family, community, clinicians). Decision authority is
 *   distributed across a triad with procedural safeguards, rather than
 *   residing solely with the individual or the medical team. This reading
 *   aims to coordinate complex end-of-life decisions, but its procedural
 *   overhead and limitations on absolute individual autonomy mean it carries
 *   a moderate level of extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.25).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Dignified Death: Relational Autonomy Model").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '608f2d1c-8777-47ff-bfe1-281a14db7604').
narrative_ontology:cs_kernel_codification('608f2d1c-8777-47ff-bfe1-281a14db7604', formalized).
narrative_ontology:cs_authority_grounding('608f2d1c-8777-47ff-bfe1-281a14db7604', expertise).
narrative_ontology:cs_interpretation_layer_present('608f2d1c-8777-47ff-bfe1-281a14db7604').
narrative_ontology:cs_reading_relation('608f2d1c-8777-47ff-bfe1-281a14db7604', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('608f2d1c-8777-47ff-bfe1-281a14db7604', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('608f2d1c-8777-47ff-bfe1-281a14db7604', foundational, dignity_is_relationally_constituted).
narrative_ontology:cs_axiom_status(dignity_is_relationally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('608f2d1c-8777-47ff-bfe1-281a14db7604', dignity_is_relationally_constituted, deontological).
narrative_ontology:cs_axiom('608f2d1c-8777-47ff-bfe1-281a14db7604', secondary, shared_decision_making_is_optimal).
narrative_ontology:cs_axiom_status(shared_decision_making_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('608f2d1c-8777-47ff-bfe1-281a14db7604', shared_decision_making_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('608f2d1c-8777-47ff-bfe1-281a14db7604', integrated_triadic_decision_making).
narrative_ontology:cs_drift_state('608f2d1c-8777-47ff-bfe1-281a14db7604', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('608f2d1c-8777-47ff-bfe1-281a14db7604', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_family_clinician_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, vulnerable_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_patients_seeking_sole_authority).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, clinicians_seeking_sole_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a shared decision-making process that integrates medical expertise, patient values, and family support, aiming for a holistic and dignified end-of-life experience. This group is coordinated by the procedural safeguards.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_family_clinician_triad, beneficiary,
    organized, biographical, constrained, local).

% May experience frustration or a sense of disempowerment if their individual desire for absolute self-determination is constrained by the need to integrate family and clinician perspectives. Their autonomy is not denied but is contextualized relationally.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patients_seeking_sole_authority, payer,
    powerless, immediate, identity_locked, local).

% May find their medical authority challenged or diluted by the requirement to engage in extensive dialogue and consensus-building with patients and families, especially when medical best practice conflicts with patient/family wishes. They bear the procedural overhead.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinicians_seeking_sole_authority, payer,
    powerful, biographical, constrained, local).

% Benefit from the safeguards that prevent their exploitation or abandonment, ensuring their relational context provides support and advocacy, rather than leaving them solely responsible for complex end-of-life decisions.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, vulnerable_patients, beneficiary,
    powerless, immediate, trapped, local).

% Administer and interpret the procedural safeguards, mediating disputes and ensuring the relational autonomy framework is applied consistently and ethically within healthcare institutions. They set the agenda for how dignity is understood in practice.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_committees, agenda_setter,
    institutional, generational, analytical, national).

% Provides the ultimate enforcement mechanism for the procedural safeguards, adjudicating cases where the relational autonomy model is challenged or violated. Its rulings shape the interpretation and application of the constraint.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the complex and often conflicting values, interests, and expertise of patients, families, and clinicians in end-of-life decision-making, aiming for a shared understanding of dignity and a consensual path forward.
% TRANSFER_FUNCTION: Transfers decision-making authority from any single party (patient, family, or clinician) to a shared, procedurally mediated process, distributing the burden and responsibility of end-of-life choices.
% ABSENT_VOICES: Patients who demand absolute, unconstrained individual autonomy in end-of-life decisions, and clinicians who believe medical expertise alone should dictate care, are structurally constrained by this model. They would argue for a simpler, more direct decision pathway.
% DISAPPEARANCE_RATIONALE: If the relational autonomy framework vanished, end-of-life decisions would likely revert to either a purely individualistic model (patient alone) or a paternalistic model (clinician alone), leading to increased conflict, ethical dilemmas, and potentially less dignified outcomes for many patients.
% FOUNDING_PROBLEM: The problem of ensuring a dignified death in complex medical situations, where individual autonomy, family values, and medical ethics often clash, leading to fragmented care and moral distress.
% FOUNDING_PROBLEM_CORROBORATION: Bioethics literature, medical professional organizations, and patient advocacy groups consistently attest to the ongoing challenge of balancing these competing values, confirming the founding problem remains live. Legal precedents also reflect the need for structured approaches to these dilemmas.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.38) reflects the procedural burden and the 'cost' of compromising absolute individual or professional authority for the sake of relational integration. Suppression (0.25) is low, as the constraint primarily functions through coordination and dialogue, with enforcement focused on ensuring fair process rather than coercing outcomes. Theater ratio (0.10) is low, indicating the procedural safeguards are genuinely functional, not merely performative. Accessibility collapse (0.40) is moderate; while alternatives to this model exist (pure individual autonomy, pure medical paternalism), they are often seen as less desirable or ethically problematic in complex cases. Resistance (0.15) is low, as the model is widely accepted in bioethics, though individual cases may generate friction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the coordinated triad, this is a beneficial rope, ensuring dignity through shared responsibility. From the perspective of an isolated patient demanding absolute self-determination, it might feel like a tangled rope, where the coordination function comes with a cost to their individual will. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient-family-clinician triad and vulnerable patients are beneficiaries, as the model aims to protect and empower them through shared decision-making. Isolated patients seeking sole authority and clinicians seeking sole authority are payers, as they must cede some individual control to the collective process. Bioethics committees and the legal system act as agenda-setters, administering and enforcing the procedural safeguards.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_vs_individual_dignity,
    'Is dignity fundamentally an individual attribute or does it emerge from relational context?',
    'Philosophical consensus on the nature of personhood and moral status, or empirical studies on patient experience of dignity in different decision-making models.',
    'If dignity is purely individual, the ''relational autonomy'' model''s extraction from individual autonomy would be reclassified as more severe; if relational, the extraction is a necessary cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relational_vs_individual_dignity, conceptual, 'The fundamental nature of dignity (individual vs. relational).').

omega_variable(
    procedural_burden_vs_benefit,
    'Does the procedural overhead of relational autonomy disproportionately burden certain stakeholders (e.g., patients in distress, overwhelmed families) relative to the benefits of shared decision-making?',
    'Empirical studies on the lived experience of patients and families under this model, assessing decision fatigue, emotional burden, and perceived dignity outcomes.',
    'If the burden is disproportionate, the extractiveness and suppression metrics for certain payer seats (e.g., isolated_patients_seeking_sole_authority) would be higher, potentially shifting their per-seat classification towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_burden_vs_benefit, empirical, 'Balance of procedural burden and benefit in relational autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (procedural requirements) or internalized (social pressure to conform to family/clinician wishes)?',
    'Post-exit suppression trajectory: if patients report feeling suppressed even after formal procedural requirements are met, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, potentially shifting their seat to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in end-of-life decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1980, dignified_death__relational_autonomy, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(dign_tr_t1990, dignified_death__relational_autonomy, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__relational_autonomy, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__relational_autonomy, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(dign_tr_t2020, dignified_death__relational_autonomy, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(dign_tr_t2024, dignified_death__relational_autonomy, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t1980, dignified_death__relational_autonomy, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(dign_be_t1990, dignified_death__relational_autonomy, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(dign_be_t2000, dignified_death__relational_autonomy, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(dign_be_t2010, dignified_death__relational_autonomy, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(dign_be_t2020, dignified_death__relational_autonomy, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(dign_be_t2024, dignified_death__relational_autonomy, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1980, dignified_death__relational_autonomy, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(dign_su_t1990, dignified_death__relational_autonomy, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(dign_su_t2000, dignified_death__relational_autonomy, suppression_requirement, 2000, 0.23).
narrative_ontology:measurement(dign_su_t2010, dignified_death__relational_autonomy, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(dign_su_t2020, dignified_death__relational_autonomy, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(dign_su_t2024, dignified_death__relational_autonomy, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
