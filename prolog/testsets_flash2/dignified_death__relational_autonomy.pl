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
    narrative_ontology:epsilon_provenance/5,
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
 *   death, where dignity is understood to emerge from a patient's social and
 *   familial context, and end-of-life decision authority is distributed
 *   across a patient-family-clinician triad, supported by procedural
 *   safeguards. It aims to balance individual self-determination with the
 *   importance of relationships and medical expertise, contrasting with
 *   readings that prioritize absolute individual autonomy or the sanctity of
 *   life above all else.
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
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Dignified Death: Relational Autonomy Model").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6').
narrative_ontology:cs_kernel_codification('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', formalized).
narrative_ontology:cs_authority_grounding('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', expertise).
narrative_ontology:cs_interpretation_layer_present('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6').
narrative_ontology:cs_reading_relation('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', foundational, dignity_emerges_from_relational_context).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', dignity_emerges_from_relational_context, deontological).
narrative_ontology:cs_axiom('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', secondary, shared_decision_making_enhances_dignity).
narrative_ontology:cs_axiom_status(shared_decision_making_enhances_dignity, holdable).
narrative_ontology:cs_axiom_grounding('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', shared_decision_making_enhances_dignity, instrumental).
narrative_ontology:cs_reference_frame('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', balanced_relational_decision_making).
narrative_ontology:cs_drift_state('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', contemporary_bioethics_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ef7ad0a4-1e69-46ee-bc01-5505da6f8bf6', '').
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

% Benefits from a shared decision-making process that respects the patient's wishes within the context of their relationships and medical expertise. This triad is the core unit of decision-making, ensuring a holistic approach to dignity.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_family_clinician_triad, beneficiary,
    organized, biographical, constrained, local).

% Protected from potential coercion or isolation by ensuring family and clinician input, preventing decisions made under duress or without full understanding of alternatives. Their dignity is affirmed through inclusion and support.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, vulnerable_patients, beneficiary,
    powerless, immediate, identity_locked, local).

% May experience frustration or delay if they wish to exercise sole, unmediated authority over end-of-life decisions, as this model requires engagement with family and clinicians. They bear the procedural overhead of shared decision-making.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patients_seeking_sole_authority, payer,
    moderate, immediate, constrained, local).

% Must integrate patient and family perspectives, which can be challenging if they believe their medical expertise should be paramount. They bear the burden of facilitating complex discussions and mediating differing views.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinicians_seeking_sole_authority, payer,
    powerful, biographical, constrained, local).

% Administer and interpret the procedural safeguards for shared decision-making, ensuring that the relational autonomy model is applied consistently and fairly. They provide guidance and resolve disputes within the triad.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_committees, agenda_setter,
    institutional, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates end-of-life decision-making among patient, family, and clinicians, ensuring that decisions are informed, supported, and respectful of the patient's relational context, preventing isolated or purely medicalized choices.
% TRANSFER_FUNCTION: Transfers decision authority from any single party (patient, family, or clinician) to a shared, procedurally safeguarded process, distributing the responsibility and ensuring broader input.
% ABSENT_VOICES: Patients who are truly isolated and lack a supportive family or social network may find it difficult to fully engage with this model, potentially leading to their perspectives being underrepresented despite safeguards. Also, those who believe in absolute individual autonomy might feel their voice is suppressed by the requirement for relational engagement.
% DISAPPEARANCE_RATIONALE: If this model vanished, end-of-life decisions would likely revert to either a purely individualistic autonomy model (potentially isolating patients) or a more paternalistic medical model, leading to increased conflict, ethical dilemmas, and potentially less dignified outcomes for many patients.
% FOUNDING_PROBLEM: End-of-life decisions often led to conflict, patient isolation, or medical paternalism, failing to adequately address the complex interplay of individual wishes, family dynamics, and medical realities, leading to undignified deaths.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists, patient advocacy groups, and many healthcare professionals attest that the problem of balancing individual autonomy with relational context in end-of-life care remains a live and complex challenge, requiring ongoing coordination and safeguards.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.38) is moderate, reflecting the procedural overhead and potential for individual preferences to be mediated by the triad, which can feel like a cost to those seeking unmediated authority. Suppression (0.25) is relatively low, as the model primarily relies on coordination and shared understanding, though it does suppress purely individualistic or paternalistic approaches. The claimed type is 'rope' because it genuinely solves a complex coordination problem, benefiting the relational network and vulnerable patients, despite the moderate costs to those who prefer a different decision-making structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the patient-family-clinician triad, this constraint is a beneficial coordination mechanism that enhances dignity. However, from the perspective of an isolated patient seeking sole authority, it might be experienced as a 'tangled rope' due to the perceived extraction of their absolute decision-making power and the procedural hurdles.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient-family-clinician triad and vulnerable patients are clear beneficiaries, as the model aims to protect and empower them through shared decision-making. Isolated patients seeking sole authority and clinicians seeking sole authority are 'payers' in terms of procedural burden and the need to compromise their preferred decision-making model. Bioethics committees act as agenda-setters, administering the safeguards.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine coordination (balancing complex interests in end-of-life care) as pure extraction. The moderate extractiveness and low suppression indicate that while there are costs, they are largely associated with the coordination function itself, rather than coercive rent-seeking. The 'live' status of the founding problem further supports its ongoing relevance as a coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_burden_vs_benefit,
    'Does the procedural burden of triad decision-making outweigh the relational benefits for all patients, especially those with limited social support?',
    'Empirical studies on patient and family satisfaction, decision regret, and perceived dignity outcomes across diverse patient populations, including those with varying levels of social support.',
    'If the burden consistently outweighs the benefit for a significant subgroup, the constraint''s extractiveness for those individuals would be higher, potentially shifting its classification towards a ''tangled rope'' for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_burden_vs_benefit, empirical, 'Assessing the net impact of relational decision-making on different patient groups.').

omega_variable(
    autonomy_relationality_framing,
    'Is the concept of ''relational autonomy'' a genuine synthesis, or does it implicitly subordinate individual autonomy to relational considerations?',
    'Conceptual analysis and philosophical debate on the coherence and implications of relational autonomy, particularly in cases where individual and relational interests diverge significantly.',
    'If it''s found to implicitly subordinate individual autonomy, the constraint''s suppression of individual choice would be higher, and its classification might shift towards a ''tangled rope'' or ''snare'' for the individual patient seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_relationality_framing, conceptual, 'Examining the philosophical grounding and potential biases of the relational autonomy framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dign_be_t5, dignified_death__relational_autonomy, base_extractiveness, 5, 0.37).
narrative_ontology:measurement(dign_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(dign_be_t15, dignified_death__relational_autonomy, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(dign_su_t5, dignified_death__relational_autonomy, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(dign_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(dign_su_t15, dignified_death__relational_autonomy, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.25).


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
