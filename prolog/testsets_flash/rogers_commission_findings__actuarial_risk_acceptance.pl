% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Findings: Actuarial Risk Acceptance
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint represents the 'actuarial risk acceptance' reading of the
 *   Rogers Commission findings, where the core requirement is to quantify
 *   failure probabilities and have them accepted by informed decision-makers.
 *   This allows for continued operations under known risks, shifting the
 *   emphasis from absolute safety to managed risk. It is a tangled rope
 *   because it provides a coordination function (allowing complex operations
 *   to proceed) but also extracts from categorical safety norms and
 *   engineering advocates by forcing them into a probabilistic framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.65).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.7).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Findings: Actuarial Risk Acceptance").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'fa0edfab-be53-4c8a-b71e-1d6787ef4b74').
narrative_ontology:cs_kernel_codification('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', formalized).
narrative_ontology:cs_authority_grounding('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', lineage).
narrative_ontology:cs_interpretation_layer_present('fa0edfab-be53-4c8a-b71e-1d6787ef4b74').
narrative_ontology:cs_reading_relation('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', rogers_commission_findings__engineering_absolute_threshold, influences).
narrative_ontology:cs_reading_relation('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_axiom('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', foundational, quantifiable_risk_is_manageable_risk).
narrative_ontology:cs_axiom_status(quantifiable_risk_is_manageable_risk, holdable).
narrative_ontology:cs_axiom_grounding('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', quantifiable_risk_is_manageable_risk, empirically_contingent).
narrative_ontology:cs_axiom('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', foundational, informed_acceptance_legitimizes_risk).
narrative_ontology:cs_axiom_status(informed_acceptance_legitimizes_risk, holdable).
narrative_ontology:cs_axiom_grounding('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', informed_acceptance_legitimizes_risk, conventional).
narrative_ontology:cs_reference_frame('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', post_challenger_accountability_framework).
narrative_ontology:cs_drift_state('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', contemporary_operational_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa0edfab-be53-4c8a-b71e-1d6787ef4b74', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_management).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the framework allows for operations with known, non-zero failure probabilities, effectively extracting 'safety margin' from the system. Suppression is also high as it actively suppresses alternative, more stringent safety paradigms (e.g., 'no known failure modes'). The theater ratio is moderate, as the documentation and acceptance process can become performative, prioritizing compliance with the process over genuine risk reduction. The metrics reflect a system that, while providing a coordination function, has become substantially extractive of safety margins and suppressive of dissenting safety philosophies.
 *
 * PERSPECTIVAL GAP:
 *   Mission planners and program management experience this as a necessary coordination mechanism for complex operations, allowing them to proceed responsibly. Engineering safety advocates and categorical safety norms experience it as an extractive force that compromises fundamental safety principles by legitimizing known risks.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and program management are beneficiaries as they gain the ability to continue operations and manage political pressure. Engineering safety advocates and categorical safety norms are victims as their preferred, more stringent safety standards are sidelined. Informed decision-makers are payers, as they bear the ultimate responsibility for accepting the quantified risks.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to prevent future Challenger-like disasters by ensuring risk accountability) is still live. However, the 'actuarial risk acceptance' reading risks mandatrophy if the process becomes purely performative, allowing high-risk operations to continue under the guise of 'informed acceptance' without genuinely reducing risk. The rising theater ratio and extractiveness over time suggest a drift towards this outcome, where the form of accountability (quantification and acceptance) displaces the substance (actual safety).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_quantification_accuracy,
    'How accurate and comprehensive are the documented failure probabilities, especially for novel or complex systems?',
    'Independent, adversarial auditing of risk models and data, comparing predicted vs. actual failure rates over time.',
    'If quantification is systematically inaccurate or incomplete, the ''informed decision-making'' is compromised, making the constraint a snare by creating a false sense of security and legitimizing unacceptable risks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(risk_quantification_accuracy, empirical, 'Accuracy of risk quantification in practice.').

omega_variable(
    decision_maker_independence,
    'Are the ''informed decision-makers'' truly independent in their acceptance of risk, or are they subject to political, budgetary, or mission-driven pressures that compel acceptance?',
    'Analysis of decision-making contexts, including internal communications, budget allocations, and career incentives for decision-makers.',
    'If decision-makers are not independent, their ''acceptance'' is coerced, transforming the constraint into a snare by making the acceptance process a cover for pre-determined operational goals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decision_maker_independence, empirical, 'Independence of risk acceptance decisions.').

omega_variable(
    actuarial_vs_categorical_framing,
    'Is the shift from categorical safety (e.g., ''no known failure modes'') to actuarial risk acceptance (e.g., ''acceptable probability of failure'') a conceptual necessity for complex systems, or a preference-driven choice that prioritizes mission over safety?',
    'Philosophical and ethical analysis of safety paradigms, and comparison of outcomes in domains that maintain categorical vs. actuarial approaches.',
    'If a preference-driven choice, the constraint''s extractiveness from categorical safety norms is a policy choice, not an inherent necessity. If a conceptual necessity, the extraction is an unavoidable cost of operating complex systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actuarial_vs_categorical_framing, conceptual, 'Conceptual basis for actuarial risk framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1986, 0.3).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(roge_tr_t2006, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2006, 0.38).
narrative_ontology:measurement(roge_tr_t2016, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(roge_tr_t2024, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(roge_be_t2006, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement(roge_be_t2016, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(roge_be_t2024, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1986, 0.6).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1996, 0.65).
narrative_ontology:measurement(roge_su_t2006, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(roge_su_t2016, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(roge_su_t2024, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_standards).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, mission_launch_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Rogers Commission findings, which is a kernel with multiple interpretations. Other readings include 'engineering_absolute_threshold' and 'management_compliance_narrative'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
