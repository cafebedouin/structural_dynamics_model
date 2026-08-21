% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation Requirement
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint describes the requirement for continuous, multi-mechanism
 *   competence occupation in high-reliability organizations, where competence
 *   is maintained through a hybrid approach of simulations, refreshers,
 *   procedural reinforcement, and line audits. A key feature is the lack of
 *   consensus on the optimal configuration of these mechanisms, leading to
 *   perpetual research and adaptation. This constraint is a reading of the
 *   'competence_occupation' kernel, specifically the 'hybrid_occupation'
 *   reading, which emphasizes the necessity of diverse, ongoing methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.7).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.75).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.7).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation Requirement").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '977d5b51-bb33-4d74-86b1-6550c6c7a5ed').
narrative_ontology:cs_kernel_codification('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', formalized).
narrative_ontology:cs_authority_grounding('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', expertise).
narrative_ontology:cs_interpretation_layer_present('977d5b51-bb33-4d74-86b1-6550c6c7a5ed').
narrative_ontology:cs_reading_relation('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', foundational, competence_is_multi_faceted).
narrative_ontology:cs_axiom_status(competence_is_multi_faceted, holdable).
narrative_ontology:cs_axiom_grounding('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', competence_is_multi_faceted, empirically_contingent).
narrative_ontology:cs_axiom('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', foundational, skill_decay_is_continuous).
narrative_ontology:cs_axiom_status(skill_decay_is_continuous, holdable).
narrative_ontology:cs_axiom_grounding('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', skill_decay_is_continuous, empirically_contingent).
narrative_ontology:cs_reference_frame('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', continuous_adaptive_competence).
narrative_ontology:cs_drift_state('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('977d5b51-bb33-4d74-86b1-6550c6c7a5ed', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, competence_assurance_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, human_factors_researchers).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, front_line_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizations_bearing_training_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, front_line_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must ensure operational competence to prevent catastrophic failures. They bear significant costs for implementing and maintaining multi-mechanism training programs and audits, but benefit from enhanced safety, reputation, and regulatory compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, global).

% Mandate and oversee competence requirements, often pushing for comprehensive, multi-faceted approaches. They benefit from public trust and reduced incident rates, but face political and economic pressure regarding the burden of compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Develop and deliver the various training components (simulations, refreshers, procedural reinforcement). They directly profit from the continuous demand for diverse training mechanisms, with little incentive to simplify or reduce scope.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers, beneficiary,
    organized, biographical, mobile, global).

% Are the direct targets of continuous training, refreshers, and audits. They invest significant time and effort, often experiencing stress from the constant assessment. While they benefit from maintaining competence for personal safety and career progression, they have limited agency over the training's design or intensity.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, front_line_operators, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, front_line_operators, beneficiary).

% Responsible for designing, implementing, and auditing competence maintenance programs within organizations. Their function is directly tied to the multi-mechanism requirement, benefiting from its complexity and continuous nature, but they also bear the operational burden.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, competence_assurance_departments, agenda_setter,
    organized, biographical, constrained, regional).

% Allocate substantial budgets to meet the multi-mechanism competence requirements. They face pressure to optimize costs but are constrained by regulatory mandates and the imperative to maintain safety, making exit from the complex training regime difficult.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, organizations_bearing_training_costs, payer,
    powerful, biographical, constrained, national).

% Conduct research into skill decay, training effectiveness, and optimal competence maintenance strategies. The lack of consensus on optimal configuration fuels their research agenda and funding opportunities, making them beneficiaries of the ongoing complexity.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, human_factors_researchers, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, human_factors_researchers, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a baseline of operational competence across complex, high-risk domains by mandating diverse training and assessment methods, thereby coordinating safety standards and professional readiness.
% TRANSFER_FUNCTION: Transfers significant resources (time, money, attention) from front-line operators and organizations to training programs, audit processes, and competence assurance functions, as the price of maintaining perceived safety and regulatory compliance.
% ABSENT_VOICES: Operators advocating for simpler, more efficient training methods with proven efficacy, and organizations seeking to reduce training overhead without compromising safety, are often marginalized in discussions dominated by regulators and training providers.
% DISAPPEARANCE_RATIONALE: If the requirement for continuous multi-mechanism competence occupation vanished, organizations would likely reduce training investments, leading to skill degradation, increased human error, and a rise in incidents across high-reliability domains. The entire safety ecosystem would reorganize around a lower, less regulated standard of competence.
% FOUNDING_PROBLEM: Preventing skill decay and ensuring readiness for rare, high-consequence events in complex systems where single training methods proved insufficient or where competence was not continuously maintained.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports, independent safety board recommendations, and academic studies on human error and skill retention consistently corroborate the ongoing challenge of maintaining competence in high-risk environments, supporting the 'live' status of the founding problem.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (maintaining safety and competence) but involves significant, asymmetric extraction. Extractiveness is high (0.7) due to the substantial costs of implementing and managing multiple, often overlapping, training and audit mechanisms, especially in the absence of an 'optimal' configuration. Suppression is high (0.75) because compliance is often mandatory for operators and organizations, with limited alternatives for simpler or cheaper competence maintenance. Theater ratio is moderate (0.4) as some mechanisms may be implemented more for compliance or perceived comprehensiveness than for proven efficacy, given the lack of consensus. The metrics show a trend of increasing extractiveness and suppression over time, reflecting the growing complexity and cost of maintaining competence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of regulators and training providers, this multi-mechanism approach is a necessary and beneficial coordination to ensure safety. From the perspective of front-line operators and organizations, it is an increasingly burdensome and extractive system, where the costs may outweigh the marginal safety benefits, especially given the lack of consensus on optimal configuration. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations, safety regulators, training providers, competence assurance departments, and human factors researchers are beneficiaries, as they either directly profit from the system (training providers, researchers) or gain reputation, compliance, and safety (HROs, regulators, competence departments). Front-line operators and organizations bearing training costs are the primary payers, experiencing the direct burden of time, effort, and financial outlay. The 'identity_locked' exit for operators reflects their professional identity being tied to maintaining competence within this system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_ambiguity,
    'What is the optimal configuration of multi-mechanism competence occupation, balancing efficacy and cost?',
    'Longitudinal, comparative studies across different high-reliability organizations and industries, coupled with advanced human factors research to identify the most effective and efficient combinations of training and assessment.',
    'Resolution could lead to a significant reduction in extractiveness and theater ratio if a more efficient, evidence-based configuration is adopted, potentially reclassifying the constraint towards a Rope or Scaffold if the transition is managed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimal_configuration_ambiguity, empirical, 'Uncertainty regarding the most effective and efficient mix of competence maintenance mechanisms.').

omega_variable(
    simulation_sufficiency_challenge,
    'Does advanced simulation-based training alone constitute sufficient exercise to occupy the competence kernel, as claimed by the ''simulation_sufficiency'' reading?',
    'Empirical validation of simulation-only training regimes against real-world performance and incident rates, compared to hybrid approaches. This would involve rigorous, controlled studies in operational environments.',
    'If simulations are proven sufficient, the ''hybrid_occupation'' reading''s justification for multi-mechanism complexity would be undermined, potentially reducing extractiveness and suppression, and shifting the constraint towards a simpler, less burdensome form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_sufficiency_challenge, empirical, 'Challenge to the necessity of multi-mechanism approach by the ''simulation_sufficiency'' sibling reading.').

omega_variable(
    real_incident_necessity_challenge,
    'Is it true that only actual catastrophic incidents provide the authentic conditions necessary to fully occupy the competence kernel, as claimed by the ''real_incident_necessity'' reading?',
    'Analysis of post-incident competence trajectories and comparison with pre-incident training effectiveness. This is difficult to resolve empirically due to ethical and practical constraints, often relying on retrospective analysis and expert judgment.',
    'If real incidents are uniquely necessary, the ''hybrid_occupation'' reading''s claim of continuous exercise being sufficient would be challenged, potentially leading to a re-evaluation of training goals and a shift in focus towards incident response rather than prevention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_incident_necessity_challenge, conceptual, 'Challenge to the sufficiency of continuous exercise by the ''real_incident_necessity'' sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.35).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.4).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'competence_occupation' kernel, focusing on the hybrid, multi-mechanism approach. It is linked to 'simulation_sufficiency' and 'real_incident_necessity' as sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
