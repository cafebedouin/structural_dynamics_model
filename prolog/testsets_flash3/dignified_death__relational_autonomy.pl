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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Dignified Death: Relational Autonomy Model
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'relational autonomy' reading of dignified
 *   death, where dignity emerges from a patient's relational context, and
 *   end-of-life decision authority is distributed across the patient, their
 *   family, and clinicians, guided by procedural safeguards. This contrasts
 *   with 'autonomy primary' (individual self-determination) and 'sanctity
 *   primary' (intrinsic value of life). This reading aims to solve the
 *   coordination problem of complex end-of-life care by integrating multiple
 *   perspectives, with moderate extraction arising from the procedural
 *   overhead and the 'cost' to those who prefer simpler, more absolute models
 *   of decision-making.
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
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '52749d19-5598-40a6-b9f3-2dd2c52bded8').
narrative_ontology:cs_kernel_codification('52749d19-5598-40a6-b9f3-2dd2c52bded8', formalized).
narrative_ontology:cs_authority_grounding('52749d19-5598-40a6-b9f3-2dd2c52bded8', practice).
narrative_ontology:cs_interpretation_layer_present('52749d19-5598-40a6-b9f3-2dd2c52bded8').
narrative_ontology:cs_reading_relation('52749d19-5598-40a6-b9f3-2dd2c52bded8', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('52749d19-5598-40a6-b9f3-2dd2c52bded8', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('52749d19-5598-40a6-b9f3-2dd2c52bded8', foundational, dignity_is_relationally_constituted).
narrative_ontology:cs_axiom_status(dignity_is_relationally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('52749d19-5598-40a6-b9f3-2dd2c52bded8', dignity_is_relationally_constituted, deontological).
narrative_ontology:cs_axiom('52749d19-5598-40a6-b9f3-2dd2c52bded8', foundational, shared_decision_making_enhances_wellbeing).
narrative_ontology:cs_axiom_status(shared_decision_making_enhances_wellbeing, holdable).
narrative_ontology:cs_axiom_grounding('52749d19-5598-40a6-b9f3-2dd2c52bded8', shared_decision_making_enhances_wellbeing, empirically_contingent).
narrative_ontology:cs_reference_frame('52749d19-5598-40a6-b9f3-2dd2c52bded8', deliberative_triad_consensus).
narrative_ontology:cs_drift_state('52749d19-5598-40a6-b9f3-2dd2c52bded8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('52749d19-5598-40a6-b9f3-2dd2c52bded8', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_family_clinician_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, vulnerable_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_patient_advocates).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, pure_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This collective entity (patient, their family, and medical team) is the primary decision-making unit, responsible for navigating complex end-of-life choices through shared deliberation and procedural safeguards. They benefit from a framework that supports comprehensive, context-sensitive care.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_family_clinician_triad, agenda_setter,
    organized, biographical, constrained, local).

% Patients whose capacity for autonomous decision-making is compromised by illness, pain, or social isolation. They benefit from the inclusion of family and clinicians in decision-making, which provides safeguards against exploitation or neglect that a pure autonomy model might miss.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, vulnerable_patients, beneficiary,
    powerless, immediate, identity_locked, local).

% Advocates who prioritize individual patient autonomy above all else, viewing family or clinician input as potential infringements on self-determination. They bear the 'cost' of shared decision-making, as their preferred model of absolute individual control is constrained by the relational framework.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patient_advocates, payer,
    moderate, biographical, constrained, national).

% Philosophers and legal scholars who champion a model of dignity rooted solely in individual self-determination. They see the distributed authority of relational autonomy as a dilution of the patient's ultimate right to decide, particularly in end-of-life contexts.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, pure_autonomy_advocates, payer,
    powerful, generational, constrained, global).

% Those who believe in the intrinsic, inviolable value of life, regardless of suffering or consent. While relational autonomy emphasizes dignity, it still permits certain end-of-life decisions (e.g., withdrawal of life support) that sanctity advocates oppose. They are excluded from the core premise of decision-making authority.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_of_life_advocates, excluded,
    institutional, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex end-of-life decisions by distributing authority and responsibility across the patient, their family, and the medical team, ensuring that decisions are made with comprehensive understanding of the patient's values, context, and medical realities.
% TRANSFER_FUNCTION: Transfers decision-making authority from a single individual (patient or clinician) to a deliberative triad, aiming to optimize for patient well-being and dignity within a relational context, rather than maximizing individual choice or preserving life at all costs.
% ABSENT_VOICES: Advocates for a pure sanctity-of-life perspective are largely excluded from the core deliberative framework, as their foundational premise often precludes certain end-of-life choices that relational autonomy considers. Similarly, extreme individual autonomy advocates find their preferred model of absolute patient control constrained.
% DISAPPEARANCE_RATIONALE: If relational autonomy vanished, end-of-life decision-making would likely revert to either a pure individual autonomy model (potentially leaving vulnerable patients exposed) or a paternalistic medical model (disempowering patients and families), or a pure sanctity model (denying patient choice). The complex, nuanced approach to dignity would be lost, leading to different, potentially less humane, outcomes.
% FOUNDING_PROBLEM: End-of-life decisions are inherently complex, involving medical facts, personal values, family dynamics, and societal norms. Neither pure individual autonomy nor medical paternalism adequately addresses the full scope of patient dignity, especially for vulnerable individuals.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists, patient advocacy groups, and medical professional organizations widely corroborate that the problem of balancing autonomy, beneficence, and relational context in end-of-life care remains live and challenging. Case studies and ethical reviews from outside the immediate triad support the ongoing need for such a framework.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Rope because it genuinely solves a complex coordination problem (balancing multiple values and actors in end-of-life care) with moderate extraction. The extraction (0.38) primarily comes from the procedural complexity and the perceived 'loss of control' for those who advocate for pure individual autonomy. Suppression (0.25) is low, as the framework relies on deliberation and consensus-building rather than coercion, though it does suppress alternative, more absolute decision models. Theater ratio is low (0.1) as the framework's stated function aligns well with its actual operation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the patient-family-clinician triad, this is a highly effective coordination mechanism that enhances dignity. From the perspective of pure autonomy advocates, it represents an infringement on individual rights, even if well-intentioned. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient-family-clinician triad and vulnerable patients are beneficiaries, as the framework is designed to protect and empower them through shared decision-making. Advocates for pure individual autonomy are payers, as their preferred model is constrained. Sanctity-of-life advocates are excluded, as their fundamental premise often falls outside the scope of this decision-making framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_vs_individual_dignity,
    'To what extent does ''relational dignity'' genuinely enhance patient well-being, versus merely complicating decision-making for the sake of process?',
    'Qualitative studies on patient and family satisfaction, longitudinal outcomes for vulnerable patients under this model versus pure autonomy models, and ethical review of cases where the triad''s decision diverges from the patient''s stated preference.',
    'If relational dignity is found to be primarily procedural overhead without clear benefit, the extractiveness for individual autonomy advocates would be higher, potentially shifting the classification towards a Tangled Rope. If it consistently enhances well-being, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_vs_individual_dignity, empirical, 'Assessing the true benefit of relational dignity versus procedural burden.').

omega_variable(
    suppression_of_pure_autonomy,
    'Is the ''suppression'' of pure individual autonomy a necessary cost of protecting vulnerable patients, or an undue infringement on competent individuals'' rights?',
    'Legal and ethical analysis of cases where a competent patient''s wishes are overridden or significantly influenced by family/clinician input, and comparison with jurisdictions that prioritize pure autonomy. This is a conceptual and preference-based question.',
    'If deemed an undue infringement, the suppression metric for pure autonomy advocates would be re-evaluated as higher, potentially pushing the constraint towards a Snare from their seat. If deemed necessary protection, the current Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_pure_autonomy, conceptual, 'Evaluating the ethical justification for constraining pure individual autonomy.').

omega_variable(
    kernel_reading_distinction,
    'Is ''dignified_death__relational_autonomy'' truly a distinct constraint from ''dignified_death__autonomy_primary'', or merely a nuanced variant?',
    'Analysis of legal precedents and clinical guidelines: if the decision-making process and outcomes consistently differ in ways that cannot be reconciled within a single framework, the distinction is robust. If they converge in practice, the readings may be less distinct.',
    'If not distinct, the two readings should be merged into a single constraint with a broader scope, potentially altering its overall extractiveness and classification. If distinct, the current decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the boundary between relational autonomy and pure autonomy readings of dignified death.').


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
narrative_ontology:measurement(dign_su_t15, dignified_death__relational_autonomy, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dignified_death' kernel. Each reading represents a distinct structural claim about how dignity is constituted and decision authority is distributed at the end of life. They are linked to reflect their shared conceptual origin and ongoing contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
