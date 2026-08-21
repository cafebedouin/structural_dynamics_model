% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Mandates
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate legitimacy. It asserts that the ethical and legal justification
 *   for public health mandates (e.g., vaccination, masking) depends on a
 *   careful balancing act: the severity of the disease, the safety and
 *   efficacy of the intervention, and the availability of less restrictive
 *   alternatives. This reading aims to coordinate public health goals with
 *   individual liberties, but its application can still result in significant
 *   extraction from individuals, making it a Tangled Rope. The metrics
 *   reflect the coercive nature of mandates and the contestation around their
 *   application, especially during periods of crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.65).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.75).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Principle for Public Health Mandates").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '23269aa9-c444-4e5d-beb1-3f6443837933').
narrative_ontology:cs_kernel_codification('23269aa9-c444-4e5d-beb1-3f6443837933', formalized).
narrative_ontology:cs_authority_grounding('23269aa9-c444-4e5d-beb1-3f6443837933', lineage).
narrative_ontology:cs_interpretation_layer_present('23269aa9-c444-4e5d-beb1-3f6443837933').
narrative_ontology:cs_reading_relation('23269aa9-c444-4e5d-beb1-3f6443837933', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('23269aa9-c444-4e5d-beb1-3f6443837933', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('23269aa9-c444-4e5d-beb1-3f6443837933', foundational, mandates_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(mandates_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('23269aa9-c444-4e5d-beb1-3f6443837933', mandates_must_be_least_restrictive, instrumental).
narrative_ontology:cs_axiom('23269aa9-c444-4e5d-beb1-3f6443837933', foundational, collective_good_conditional_on_individual_harm).
narrative_ontology:cs_axiom_status(collective_good_conditional_on_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('23269aa9-c444-4e5d-beb1-3f6443837933', collective_good_conditional_on_individual_harm, deontological).
narrative_ontology:cs_reference_frame('23269aa9-c444-4e5d-beb1-3f6443837933', ethical_legal_proportionality_framework).
narrative_ontology:cs_drift_state('23269aa9-c444-4e5d-beb1-3f6443837933', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23269aa9-c444-4e5d-beb1-3f6443837933', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they interpret and apply the proportionality principle to justify and enforce mandates. They benefit from a framework that allows for intervention while providing ethical and legal guardrails.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of compliance with mandates (e.g., vaccination, testing, masking) or face penalties for non-compliance. Their bodily autonomy is conditionally infringed, but the proportionality principle aims to limit this infringement.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate, payer,
    powerless, biographical, constrained, national).

% Benefit from reduced disease transmission and protection from severe outcomes due to mandates. They are often the primary target of public health interventions, but typically lack direct agency in the mandate-setting process.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, generational, constrained, national).

% Actively campaign for individual choice and against compelled medical interventions. While not directly involved in policy-making, their advocacy influences public discourse and legal challenges, often pushing for stricter interpretations of proportionality.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% Serve as arbiters of the proportionality principle, reviewing the legality and ethical justification of public health mandates. They assess whether mandates meet criteria of necessity, effectiveness, and least restrictiveness.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ethical and legal justification of public health mandates by requiring consideration of disease severity, vaccine safety/efficacy, and less restrictive alternatives, thereby balancing collective health with individual liberties.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from the collective (especially vulnerable groups) to individuals (via compliance with mandates), but only when this transfer is deemed proportional to the public health threat and individual infringement.
% ABSENT_VOICES: Those who hold an absolute view of bodily autonomy, asserting that no collective benefit can justify compelled medical intervention, would object to any mandate, regardless of proportionality. They are often excluded from the core policy-making process but exert influence through protest and litigation.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health mandates would either become overly coercive (if public health primary dominates unchecked) or largely ineffective (if individual autonomy dominates without limits), leading to a less ethical and potentially more chaotic public health landscape. The framework for legitimate intervention would collapse.
% FOUNDING_PROBLEM: To establish a principled framework for state intervention in individual medical decisions during public health crises, ensuring such interventions are both effective and ethically justifiable, avoiding arbitrary or excessive coercion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and international public health organizations (e.g., WHO, Council of Europe) consistently emphasize the ongoing challenge of balancing individual rights and collective health, corroborating the live status of this foundational problem. This corroboration comes from outside the direct mandate-setting bodies.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is substantial because even 'proportional' mandates impose costs and restrictions on individuals. Suppression (0.75) is high due to the coercive nature of state mandates, which often carry legal penalties for non-compliance. Theater ratio (0.15) is low, indicating that the principle is genuinely applied and debated, rather than being a mere performance. Accessibility collapse (0.5) is moderate, as alternatives are considered, but ultimately, the mandate requires compliance. Resistance (0.7) is high, reflecting ongoing public and legal challenges to mandates, particularly when their proportionality is questioned.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this principle is a necessary and ethical coordination mechanism. From the perspective of individuals subject to mandates, it can still feel highly extractive and suppressive, even if 'justified' by proportionality. The engine's computation of per-seat types will reflect this divergence, showing a more extractive experience for those targeted by mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries, as the principle provides a framework for legitimate intervention that protects collective health. Individuals subject to mandates are victims, as they bear the direct costs and infringements on autonomy. Medical autonomy advocates are excluded, as their absolute stance on autonomy is not fully integrated into this balancing framework, though their arguments influence its application. Constitutional courts act as observers, evaluating the application of the principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What objective thresholds define ''disease severity,'' ''sufficient safety/efficacy,'' or ''less restrictive alternatives'' in practice?',
    'Development of universally accepted, evidence-based metrics and legal precedents that clarify these thresholds across different jurisdictions and pathogens.',
    'Clearer thresholds would reduce arbitrary application, potentially lowering perceived extractiveness and resistance. Ambiguity allows for greater discretion, which can be leveraged for more extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining the criteria for proportionality.').

omega_variable(
    empirical_evidence_standard_for_mandates,
    'What level of empirical evidence (e.g., for vaccine efficacy or disease transmission) is required to trigger or justify a mandate under the proportionality principle?',
    'Establishment of a legally binding, transparent evidentiary standard for public health interventions, subject to independent scientific review.',
    'A high, consistent evidentiary standard would limit mandates to situations with strong scientific backing, potentially reducing perceived suppression. A low or inconsistent standard could allow for more easily justified, and thus more frequent, mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_standard_for_mandates, empirical, 'The evidentiary bar for justifying mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mand_tr_t1925, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1925, 0.08).
narrative_ontology:measurement(mand_tr_t1950, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(mand_tr_t1975, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement(mand_tr_t2000, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(mand_tr_t2025, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(mand_be_t1925, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1925, 0.5).
narrative_ontology:measurement(mand_be_t1950, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(mand_be_t1975, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(mand_be_t2000, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(mand_be_t2025, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(mand_su_t1925, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement(mand_su_t1950, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(mand_su_t1975, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(mand_su_t2000, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mand_su_t2025, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
