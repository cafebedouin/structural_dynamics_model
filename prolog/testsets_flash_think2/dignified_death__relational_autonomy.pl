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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decisions
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'relational autonomy' reading of
 *   end-of-life dignity, where decision authority is distributed across a
 *   patient-family-clinician triad with procedural safeguards. It is claimed
 *   as a Rope, reflecting its primary function as a coordination mechanism
 *   for complex ethical decisions. The metrics reflect the inherent overhead
 *   and emotional labor of such a process, which can be perceived as
 *   extractive by some participants, but is intended to ensure a more
 *   dignified outcome for all involved. This story is one reading of the
 *   'dignified_death' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.45).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decisions").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'e7485150-f9f4-4f4f-9674-a6ffbff272ee').
narrative_ontology:cs_kernel_codification('e7485150-f9f4-4f4f-9674-a6ffbff272ee', formalized).
narrative_ontology:cs_authority_grounding('e7485150-f9f4-4f4f-9674-a6ffbff272ee', practice).
narrative_ontology:cs_interpretation_layer_present('e7485150-f9f4-4f4f-9674-a6ffbff272ee').
narrative_ontology:cs_reading_relation('e7485150-f9f4-4f4f-9674-a6ffbff272ee', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('e7485150-f9f4-4f4f-9674-a6ffbff272ee', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('e7485150-f9f4-4f4f-9674-a6ffbff272ee', foundational, dignity_is_relational).
narrative_ontology:cs_axiom_status(dignity_is_relational, holdable).
narrative_ontology:cs_axiom_grounding('e7485150-f9f4-4f4f-9674-a6ffbff272ee', dignity_is_relational, deontological).
narrative_ontology:cs_axiom('e7485150-f9f4-4f4f-9674-a6ffbff272ee', foundational, shared_decision_authority_is_ethical).
narrative_ontology:cs_axiom_status(shared_decision_authority_is_ethical, holdable).
narrative_ontology:cs_axiom_grounding('e7485150-f9f4-4f4f-9674-a6ffbff272ee', shared_decision_authority_is_ethical, conventional).
narrative_ontology:cs_reference_frame('e7485150-f9f4-4f4f-9674-a6ffbff272ee', holistic_patient_care_framework).
narrative_ontology:cs_drift_state('e7485150-f9f4-4f4f-9674-a6ffbff272ee', contemporary_bioethical_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e7485150-f9f4-4f4f-9674-a6ffbff272ee', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, clinicians).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_burdened_by_triad_complexity).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, families_experiencing_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patient).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, family).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_principle).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, holistic_patient_care).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The individual whose life is ending. Benefits from a dignified, supported decision-making process that integrates their values. Bears the emotional and cognitive burden of participating in complex, shared decisions at a vulnerable time.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient, payer).

% Provides emotional support and context for the patient's values. Benefits from a process that ensures their loved one's dignity and provides a framework for grief. Bears the emotional and cognitive burden of shared decision-making, especially when internal conflicts arise.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, family, payer).

% Provides medical expertise, prognosis, and care options. Benefits from ethical clarity and a structured process for navigating complex end-of-life situations. Bears the burden of procedural overhead, emotional labor, and potential legal/ethical scrutiny.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinicians, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, clinicians, beneficiary).

% The broader social and emotional context surrounding the patient. Benefits from the preservation of dignity and ethical integrity within the community. Its identity is deeply intertwined with the values of care and respect for life transitions.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, relational_network, beneficiary,
    organized, generational, identity_locked, local).

% Patients who, despite the framework's intent, find the multi-party decision-making process overwhelming, confusing, or emotionally exhausting, feeling their individual voice is diluted by the need for consensus or procedural steps.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_burdened_by_triad_complexity, payer,
    powerless, immediate, trapped, local).

% Family units where pre-existing disagreements or differing interpretations of the patient's wishes are amplified by the shared decision-making requirement, leading to prolonged distress and procedural deadlock.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, families_experiencing_conflict, payer,
    powerless, immediate, constrained, local).

% Advocate for the patient's absolute right to self-determination, potentially viewing the family and clinician involvement as an infringement on individual liberty. They are excluded from the direct decision-making triad but influence policy debates.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, pure_autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% Advocate for the intrinsic value of life, opposing any intentional hastening of death regardless of consent or suffering. They are excluded from the direct decision-making triad but influence policy debates and public opinion.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, pure_sanctity_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure end-of-life decisions are made with respect for the patient's values, while integrating the perspectives and support of their family and the medical expertise of clinicians, preventing isolated or purely medicalized decisions.
% TRANSFER_FUNCTION: Transfers decision-making authority from a single locus (patient or clinician) to a shared, deliberative process, distributing the emotional and ethical burden across the triad. It also transfers the cost of procedural overhead to all participants.
% ABSENT_VOICES: Advocates for pure patient autonomy might argue for less family/clinician involvement, while advocates for the sanctity of life might object to any decision that intentionally hastens death, regardless of process. Both are often excluded from the direct decision-making triad, operating at a policy or advocacy level.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, end-of-life decisions would likely revert to either a purely individualistic model (patient-only) or a paternalistic/sanctity-driven model, leading to increased conflict, ethical distress, and potentially less dignified outcomes for patients and their families.
% FOUNDING_PROBLEM: The problem of ensuring dignified end-of-life care that respects individual wishes, acknowledges the social context of illness, and integrates medical expertise, avoiding both isolated patient suffering and paternalistic medical overreach.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists, medical professional bodies, and patient advocacy groups widely attest that balancing these complex factors remains a live and critical problem in contemporary healthcare, supported by ongoing research into patient and family experiences.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness (0.38) is moderate, reflecting the procedural overhead, time commitment, and emotional labor required for genuine shared decision-making, which can feel like a burden. Suppression (0.45) is also moderate, stemming from the need for adherence to procedural safeguards, legal requirements, and the implicit pressure to conform to the triad's deliberative process. Theater ratio (0.15) is low, as the safeguards and deliberative processes are generally functional and serve their stated purpose. Accessibility collapse (0.55) indicates that while pure individual autonomy or pure sanctity-of-life approaches are constrained, they are not entirely foreclosed as conceptual alternatives. Resistance (0.40) reflects the inherent friction and potential for disagreement within the triad, as well as ongoing philosophical debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the relational network, this constraint is a vital coordination mechanism that upholds dignity. However, from the perspective of a patient or family member burdened by the complexity or internal conflict, the same constraint can feel highly extractive and suppressive, even if its intent is benign. Clinicians may experience it as a necessary but demanding ethical framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient, family, clinicians, and the broader relational network are all structural beneficiaries, gaining from a more ethical and supported decision-making process. However, the patient and family also act as payers, bearing the emotional and cognitive costs of the complex process. Clinicians, as agenda-setters, also bear significant procedural and emotional costs. Those who find the triad's complexity overwhelming or experience internal family conflict become victims of the process's inherent friction. Advocates for pure autonomy or sanctity are excluded from the direct decision-making process, as their core tenets are not fully compatible with the distributed authority model.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_autonomy_kernel_reading,
    'This constraint is one reading of the ''dignified_death'' kernel. What would be the structural changes if the ''autonomy_primary'' or ''sanctity_primary'' readings were adopted?',
    'Analysis of legal and ethical frameworks based on alternative readings, comparing their beneficiary/victim sets and enforcement mechanisms.',
    'If ''autonomy_primary'' were adopted, the patient would be the primary beneficiary/agenda-setter, family/clinicians would have reduced roles, and those seeking shared decisions might become victims. If ''sanctity_primary'' were adopted, life-termination decisions would be foreclosed, and patients/families seeking such options would be victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relational_autonomy_kernel_reading, conceptual, 'Structural implications of alternative readings of the dignified death kernel.').

omega_variable(
    procedural_burden_vs_dignity,
    'Is the procedural overhead and emotional burden of shared decision-making a necessary cost for achieving dignity, or does it become an extractive burden that diminishes dignity for some?',
    'Qualitative studies of patient and family experiences with the triad decision-making process, assessing perceived burden versus perceived dignity and support.',
    'If the burden is found to significantly diminish dignity for a substantial portion of participants, the effective extractiveness of the constraint would be higher, potentially shifting its classification towards a Tangled Rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_burden_vs_dignity, empirical, 'Whether the costs of relational autonomy genuinely contribute to dignity or become an undue burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.11).
narrative_ontology:measurement(dign_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.12).
narrative_ontology:measurement(dign_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.13).
narrative_ontology:measurement(dign_tr_t40, dignified_death__relational_autonomy, theater_ratio, 40, 0.14).
narrative_ontology:measurement(dign_tr_t50, dignified_death__relational_autonomy, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dign_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(dign_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(dign_be_t40, dignified_death__relational_autonomy, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(dign_be_t50, dignified_death__relational_autonomy, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dign_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(dign_su_t30, dignified_death__relational_autonomy, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(dign_su_t40, dignified_death__relational_autonomy, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(dign_su_t50, dignified_death__relational_autonomy, suppression_requirement, 50, 0.45).


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
