% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Authority: Autonomy Reading
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint asserts that individual autonomy is the primary moral
 *   ground for a patient's right to control the circumstances and timing of
 *   their death when facing unbearable suffering. It aims to empower patients
 *   and reduce paternalistic restrictions. The 'autonomy_reading' is one
 *   interpretation of the broader 'end_of_life_authority' kernel, which is
 *   contested by 'sanctity_reading' (emphasizing the intrinsic value of life)
 *   and 'slippery_slope_mechanism' (warning of unintended consequences of
 *   expanding end-of-life options). This reading's structural delta is the
 *   inclusion of suffering-prolonged patients in the victim set and a high
 *   suppression of paternalistic restrictions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.3).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.2).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Authority: Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '1daa50bc-775c-4960-8259-294fbe50820c').
narrative_ontology:cs_kernel_codification('1daa50bc-775c-4960-8259-294fbe50820c', formalized).
narrative_ontology:cs_authority_grounding('1daa50bc-775c-4960-8259-294fbe50820c', expertise).
narrative_ontology:cs_interpretation_layer_present('1daa50bc-775c-4960-8259-294fbe50820c').
narrative_ontology:cs_reading_relation('1daa50bc-775c-4960-8259-294fbe50820c', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1daa50bc-775c-4960-8259-294fbe50820c', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('1daa50bc-775c-4960-8259-294fbe50820c', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('1daa50bc-775c-4960-8259-294fbe50820c', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('1daa50bc-775c-4960-8259-294fbe50820c', foundational, unbearable_suffering_justifies_relief).
narrative_ontology:cs_axiom_status(unbearable_suffering_justifies_relief, holdable).
narrative_ontology:cs_axiom_grounding('1daa50bc-775c-4960-8259-294fbe50820c', unbearable_suffering_justifies_relief, deontological).
narrative_ontology:cs_reference_frame('1daa50bc-775c-4960-8259-294fbe50820c', patient_centered_care_paradigm).
narrative_ontology:cs_drift_state('1daa50bc-775c-4960-8259-294fbe50820c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1daa50bc-775c-4960-8259-294fbe50820c', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_with_unbearable_suffering).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, advocacy_groups_for_patient_rights).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice_due_to_paternalism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because it primarily facilitates coordination around patient choice, with relatively low extraction (0.3) and suppression (0.2) for those who align with its principles. The extraction arises from the administrative and legal overhead of establishing and verifying eligibility, and the emotional burden on healthcare providers. Suppression is low as it aims to remove existing paternalistic barriers, rather than impose new ones. Theater ratio is low (0.05) as the stated purpose (patient autonomy) is largely aligned with its operation. The temporal measurements show a slight increase in extractiveness as implementation complexities arise, but a decrease in suppression as the framework becomes more established.
 *
 * PERSPECTIVAL GAP:
 *   For patients seeking to exercise this right, it is a pure coordination mechanism. For healthcare providers who morally object, it can feel like a coercive imposition. For those who hold the 'sanctity_reading', this constraint is seen as a fundamental violation, not a coordination. The engine will compute these divergences based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients with unbearable suffering are primary beneficiaries (d near 0.0) as the constraint empowers their choice. Advocacy groups also benefit by seeing their principles codified. Patients denied choice due to paternalism are victims (d near 1.0) as the constraint aims to remove the extraction of their autonomy. Healthcare providers are agenda-setters, balancing patient requests with ethical guidelines (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint addresses a persistent problem of patient disempowerment at the end of life. Its mandate is live, and its function is to provide a framework for autonomous decision-making. It prevents mislabeling genuine patient empowerment as extraction by focusing on the removal of existing barriers rather than the imposition of new ones. The 'slippery_slope_mechanism' sibling reading, however, raises a mandatrophy concern about potential future drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of individual autonomy, or is it a step towards broader, less constrained end-of-life options?',
    'Longitudinal study of policy evolution in jurisdictions adopting autonomy-based end-of-life frameworks, specifically tracking changes in eligibility criteria and scope of application.',
    'If it remains stable within its stated scope, it reinforces the ''rope'' classification. If it consistently expands beyond its initial intent, it would suggest a ''tangled_rope'' or ''snare'' dynamic, where the initial coordination story serves as a cover for broader extraction or control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''autonomy_reading'' of the ''end_of_life_authority'' kernel. Sibling readings include ''sanctity_reading'' (intrinsic value of life) and ''slippery_slope_mechanism'' (empirical expansion beyond initial scope). The core disagreement is on the moral permissibility and practical consequences of intentional life-ending based on patient choice.').

omega_variable(
    suffering_definition_ambiguity,
    'How is ''unbearable suffering'' objectively defined and measured, and who holds the ultimate authority in this determination?',
    'Development of standardized, multi-disciplinary assessment protocols for suffering, coupled with clear legal and ethical guidelines on patient vs. physician authority in final determination.',
    'If the definition remains subjective and physician-centric, it introduces a paternalistic element, potentially increasing suppression for patients whose suffering is not ''recognized''. If patient self-determination is paramount, it reinforces autonomy and reduces suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suffering_definition_ambiguity, conceptual, 'Ambiguity in defining ''unbearable suffering'' can lead to inconsistent application and potential for paternalistic override of patient autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, medical_paternalism_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_authority' kernel, focusing on individual autonomy. It is linked to the 'sanctity_reading' (which emphasizes the intrinsic value of life) and the 'slippery_slope_mechanism' (which models the empirical expansion of end-of-life options).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
