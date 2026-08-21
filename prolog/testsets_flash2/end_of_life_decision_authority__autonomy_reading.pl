% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individual's End-of-Life Autonomy
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of end-of-life decision
 *   authority, asserting that competent individuals have sovereign control
 *   over their own death. It is framed as a Rope, as it primarily facilitates
 *   coordination between patient wishes and medical practice, with relatively
 *   low extraction from those who benefit. The extraction that exists is
 *   primarily borne by patients who are denied access to aid-in-dying due to
 *   lingering legal or institutional barriers, or by healthcare professionals
 *   navigating complex ethical landscapes. The core idea is to empower
 *   individuals, shifting authority from external bodies to the patient.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.35).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.2).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual's End-of-Life Autonomy").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '8bbaec65-af44-4b0d-a23f-f5e99f8088d2').
narrative_ontology:cs_kernel_codification('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', formalized).
narrative_ontology:cs_authority_grounding('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', lineage).
narrative_ontology:cs_interpretation_layer_present('8bbaec65-af44-4b0d-a23f-f5e99f8088d2').
narrative_ontology:cs_reading_relation('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', secondary, relief_of_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', relief_of_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', enlightenment_individual_rights).
narrative_ontology:cs_drift_state('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', contemporary_medical_technology_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8bbaec65-af44-4b0d-a23f-f5e99f8088d2', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_patient_rights).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, patients_denied_aid_in_dying).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with decision-making capacity who wish to exercise control over the timing and manner of their death, particularly in the face of intractable suffering. They benefit from legal frameworks that affirm their right to choose.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_patients, beneficiary,
    moderate, immediate, constrained, local).

% Individuals who, despite meeting competency criteria and expressing a desire for aid-in-dying, are denied access due to legal restrictions or institutional policies. They bear the cost of prolonged suffering against their will.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, patients_denied_aid_in_dying, payer,
    powerless, immediate, trapped, local).

% Physicians and other medical staff who are tasked with assessing patient competency, diagnosing terminal illness, and, where legally permitted, facilitating aid-in-dying. They navigate ethical guidelines and legal requirements, sometimes facing moral distress.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals, agenda_setter,
    institutional, biographical, constrained, local).

% Organizations that champion patient autonomy and the right to self-determination in end-of-life decisions. They benefit from legal precedents and policy changes that align with their mission, seeing their values vindicated.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_patient_rights, beneficiary,
    organized, generational, mobile, national).

% Organizations that often hold theological objections to intentional life-ending, viewing it as a violation of divine law or the sanctity of life. While they influence public discourse, their direct authority over individual medical decisions is limited in secular legal frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions, excluded,
    institutional, civilizational, identity_locked, global).

% Hospital or institutional bodies that review complex ethical cases, including end-of-life decisions. They provide guidance and ensure compliance with policy, often balancing competing ethical principles.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_committees, observer,
    institutional, biographical, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for competent individuals to make and enact their own end-of-life decisions, coordinating medical practice with patient wishes to ensure dignified and self-determined death.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority regarding one's own death from external authorities (medical, legal, religious) to the competent individual, along with the responsibility for the consequences of that choice.
% ABSENT_VOICES: Patients who are suffering but lack the capacity to express their wishes, or those who are vulnerable to coercion, are often not adequately represented in the discourse, leading to a focus on 'competent' autonomy that may overlook their needs. Religious institutions, while vocal, are often excluded from direct legal authority over individual medical choices.
% DISAPPEARANCE_RATIONALE: If the principle of individual autonomy over end-of-life decisions vanished, medical practice would revert to paternalistic models, patients would lose control over their dying process, and legal battles over the right to die would intensify, fundamentally altering the landscape of medical ethics and patient rights.
% FOUNDING_PROBLEM: Patients faced prolonged suffering and loss of dignity at the end of life, with medical decisions often made by others without sufficient regard for individual wishes or values.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, legal scholars, and many medical professionals corroborate that the problem of respecting patient autonomy in end-of-life care remains live, particularly as medical technology can prolong life beyond what many patients desire. This is attested by ongoing legislative efforts and court cases in various jurisdictions.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).
:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the costs borne by those denied access to aid-in-dying and the ethical burdens on healthcare providers. Suppression (0.20) is relatively low, as the constraint aims to remove barriers rather than impose them, though some suppression exists from legal restrictions in certain jurisdictions. Theater ratio (0.10) is low, indicating that the stated function of respecting autonomy largely aligns with actual practice where the constraint is active. The metrics reflect a system that, while not perfectly frictionless, largely serves its stated coordination purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent patients, this constraint is a clear Rope, enabling self-determination. From the perspective of patients denied aid-in-dying, it functions as a Snare, trapping them in unwanted suffering. Healthcare professionals experience it as a Tangled Rope, balancing patient wishes with professional ethics and legal boundaries. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent patients are the primary beneficiaries (d near 0.0), as the constraint directly empowers their choices. Advocacy groups also benefit from the vindication of their principles. Patients denied aid-in-dying are the victims (d near 1.0), bearing the cost of the constraint's incomplete or contested application. Healthcare professionals act as agenda-setters, implementing and navigating the constraint, experiencing a mix of professional duty and ethical challenge. Religious institutions are excluded, as their moral framework is often at odds with the secular legal basis of this autonomy reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_competence,
    'How is ''competence'' defined and assessed in practice, and does this definition inadvertently exclude individuals who genuinely desire aid-in-dying but fall outside narrow criteria?',
    'Empirical study of competence assessment protocols and their application across diverse patient populations, including those with fluctuating capacity or non-standard communication methods.',
    'If competence criteria are found to be overly restrictive, the effective victim set (patients denied aid-in-dying) would be larger, increasing the constraint''s effective extractiveness for that group. This would shift the classification for those individuals closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_competence, empirical, 'Ambiguity in the practical definition and assessment of patient competence for end-of-life decisions.').

omega_variable(
    slippery_slope_risk,
    'Does the implementation of this autonomy reading lead to an expansion of aid-in-dying to vulnerable populations who are not truly competent or are subject to coercion, as argued by ''vulnerability_protection_reading''?',
    'Longitudinal epidemiological studies in jurisdictions with aid-in-dying laws, tracking trends in patient demographics, reasons for seeking aid, and incidence of coercion or undue influence.',
    'If a ''slippery slope'' is empirically demonstrated, the ''vulnerability_protection_reading'' would gain significant corroboration, potentially leading to a re-evaluation of the autonomy reading''s ethical permissibility or the introduction of more stringent safeguards, increasing suppression for some beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'The risk of unintended expansion of aid-in-dying to vulnerable populations, as a consequence of prioritizing individual autonomy.').

omega_variable(
    moral_distress_of_providers,
    'To what extent does this autonomy reading impose moral distress on healthcare professionals who have conscientious objections to facilitating aid-in-dying, and what are their effective exit options?',
    'Qualitative and quantitative studies on healthcare provider experiences in jurisdictions with aid-in-dying laws, focusing on rates of moral distress, burnout, and access to conscientious objection provisions.',
    'If moral distress is widespread and exit options (e.g., referral to other providers) are insufficient, the constraint could be seen as imposing an unacknowledged cost on healthcare professionals, shifting their seat''s classification towards a Payer or even a Snare, despite their role as agenda-setters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_distress_of_providers, empirical, 'The unacknowledged costs and moral burdens placed on healthcare providers by the autonomy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__autonomy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, attachment_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'end_of_life_decision_authority' kernel. This 'autonomy_reading' emphasizes individual self-determination, while 'sanctity_reading' prioritizes intrinsic value of life and 'vulnerability_protection_reading' focuses on safeguards against coercion. Each represents a distinct structural claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
