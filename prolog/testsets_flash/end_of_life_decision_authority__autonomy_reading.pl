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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: End-of-Life Decision Authority (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of end-of-life decision
 *   authority, asserting that competent individuals have sovereign control
 *   over their own death. It is a 'rope' because it facilitates coordination
 *   between patients, families, and healthcare providers around patient
 *   wishes, with relatively low extraction. However, it carries a low but
 *   increasing extractiveness from those whose suffering is prolonged by
 *   denial of access, and a low suppression from those who would oppose it on
 *   other grounds. This reading is one of several competing interpretations
 *   of the broader 'end_of_life_decision_authority' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.3).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.2).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "End-of-Life Decision Authority (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '875083bb-072f-4de7-87ef-b5ff526ce4d8').
narrative_ontology:cs_kernel_codification('875083bb-072f-4de7-87ef-b5ff526ce4d8', formalized).
narrative_ontology:cs_authority_grounding('875083bb-072f-4de7-87ef-b5ff526ce4d8', lineage).
narrative_ontology:cs_interpretation_layer_present('875083bb-072f-4de7-87ef-b5ff526ce4d8').
narrative_ontology:cs_reading_relation('875083bb-072f-4de7-87ef-b5ff526ce4d8', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('875083bb-072f-4de7-87ef-b5ff526ce4d8', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('875083bb-072f-4de7-87ef-b5ff526ce4d8', foundational, individual_self_determination_absolute).
narrative_ontology:cs_axiom_status(individual_self_determination_absolute, holdable).
narrative_ontology:cs_axiom_grounding('875083bb-072f-4de7-87ef-b5ff526ce4d8', individual_self_determination_absolute, deontological).
narrative_ontology:cs_axiom('875083bb-072f-4de7-87ef-b5ff526ce4d8', secondary, relief_of_suffering_paramount).
narrative_ontology:cs_axiom_status(relief_of_suffering_paramount, holdable).
narrative_ontology:cs_axiom_grounding('875083bb-072f-4de7-87ef-b5ff526ce4d8', relief_of_suffering_paramount, instrumental).
narrative_ontology:cs_reference_frame('875083bb-072f-4de7-87ef-b5ff526ce4d8', patient_rights_movement_era).
narrative_ontology:cs_drift_state('875083bb-072f-4de7-87ef-b5ff526ce4d8', contemporary_policy_debates, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('875083bb-072f-4de7-87ef-b5ff526ce4d8', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).

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
 *   The extractiveness (0.3) is low because the primary function is to empower individuals, but it is non-zero due to the costs borne by families and the 'victims' who are denied access despite their autonomous wishes. Suppression (0.2) is low as the constraint is generally accepted in many jurisdictions, but active enforcement is required to overcome resistance from those who hold alternative moral frameworks. Theater ratio (0.05) is very low, indicating that the constraint's stated purpose (autonomy) aligns closely with its actual operation. Accessibility collapse (0.6) is moderate, as alternatives (e.g., palliative care, natural death) still exist, but the specific alternative of autonomous control over death is collapsed if the constraint is not present. Resistance (0.15) is low but persistent, primarily from religious and vulnerability-focused groups.
 *
 * PERSPECTIVAL GAP:
 *   Competent patients experience this as a liberating rope, while families may experience it as a difficult coordination. Healthcare professionals navigate a complex ethical landscape, acting as facilitators. Patients denied access experience it as a snare. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent patients are clear beneficiaries (d=0.0-0.1). Healthcare professionals facilitating these decisions are also beneficiaries, as it clarifies their ethical obligations (d=0.1-0.2). Families are payers, bearing emotional costs (d=0.6-0.7). Suffering-prolonged patients denied access are victims (d=0.9-1.0). Religious institutions are excluded, their views suppressed by the legal framework (d=0.8-0.9). Vulnerable populations advocates are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying problem of patient autonomy in end-of-life care remains live. The challenge is not obsolescence but the ongoing contestation of its ethical and social implications, particularly regarding the 'slippery slope' argument. The classification as a rope reflects its genuine coordination function, preventing mislabeling as pure extraction, while acknowledging the costs and suppressed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_risk,
    'Does the expansion of end-of-life autonomy lead to a ''slippery slope'' where vulnerable individuals are subtly coerced or pressured into choosing death?',
    'Longitudinal epidemiological studies tracking rates of assisted dying across different demographic groups, coupled with qualitative research on patient decision-making processes in jurisdictions with liberalized laws.',
    'If a ''slippery slope'' is empirically demonstrated, the constraint''s effective extractiveness and suppression would be significantly higher for vulnerable populations, potentially reclassifying it as a tangled_rope or snare for those groups. If not, the autonomy reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'The risk that expanded autonomy leads to coercion for vulnerable groups.').

omega_variable(
    moral_status_of_autonomy,
    'Is individual autonomy an absolute moral principle in end-of-life decisions, or is it one value among others (e.g., sanctity of life, community welfare) that must be balanced?',
    'Philosophical and theological debate, and societal consensus-building through democratic processes and evolving ethical norms. No purely empirical resolution.',
    'If autonomy is not absolute, the constraint''s justification as a pure rope is weakened, and its interaction with other values (as embodied in sibling readings) becomes more central to its classification. This would shift the conceptual framing of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_autonomy, conceptual, 'The foundational moral weight of individual autonomy in end-of-life contexts.').

omega_variable(
    denial_of_access_extraction,
    'What is the true extent of suffering prolonged by denial of access to end-of-life options, and how does this ''extraction'' weigh against the benefits of autonomy for those who can access it?',
    'Systematic collection of data on patient requests for end-of-life options, reasons for denial, and subsequent patient outcomes (e.g., prolonged suffering, clandestine actions).',
    'If the ''extraction'' from denied access is found to be substantial and widespread, the overall extractiveness of the constraint (as implemented) would be higher, pushing it closer to a tangled_rope or even snare for the ''suffering_prolonged_patients'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denial_of_access_extraction, empirical, 'Quantifying the harm from denial of autonomous end-of-life choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(end__tr_t1985, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1985, 0.02).
narrative_ontology:measurement(end__tr_t2000, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(end__tr_t2024, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(end__be_t1985, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(end__be_t2000, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(end__be_t2024, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(end__su_t1985, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(end__su_t2000, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(end__su_t2024, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, attachment_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_decision_authority' kernel. Its ε value differs significantly from the 'sanctity_reading' (lower extraction) and 'vulnerability_protection_reading' (different suppression mechanisms), necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
