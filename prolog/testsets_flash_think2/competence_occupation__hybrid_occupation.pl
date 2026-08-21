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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation Model
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint describes the requirement for high-reliability
 *   organizations to maintain operational competence through a continuous,
 *   multi-mechanism training regime (simulations, refreshers, procedural
 *   reinforcement, line audits), in the absence of a clear consensus on the
 *   optimal configuration of these mechanisms. It is one reading of the
 *   'competence_occupation' kernel, emphasizing the hybrid, adaptive nature
 *   of competence maintenance. The constraint is claimed as a Rope by its
 *   proponents (ensuring safety), but its metrics reflect a Tangled Rope due
 *   to the significant, often unoptimized, extraction of resources and the
 *   active enforcement required to maintain its complex structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.68).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.75).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation Model").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'c5d6f572-81d6-4fd5-971b-a3302420a37e').
narrative_ontology:cs_kernel_codification('c5d6f572-81d6-4fd5-971b-a3302420a37e', formalized).
narrative_ontology:cs_authority_grounding('c5d6f572-81d6-4fd5-971b-a3302420a37e', expertise).
narrative_ontology:cs_interpretation_layer_present('c5d6f572-81d6-4fd5-971b-a3302420a37e').
narrative_ontology:cs_reading_relation('c5d6f572-81d6-4fd5-971b-a3302420a37e', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('c5d6f572-81d6-4fd5-971b-a3302420a37e', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_axiom('c5d6f572-81d6-4fd5-971b-a3302420a37e', foundational, competence_is_dynamic_and_decays).
narrative_ontology:cs_axiom_status(competence_is_dynamic_and_decays, holdable).
narrative_ontology:cs_axiom_grounding('c5d6f572-81d6-4fd5-971b-a3302420a37e', competence_is_dynamic_and_decays, empirically_contingent).
narrative_ontology:cs_axiom('c5d6f572-81d6-4fd5-971b-a3302420a37e', foundational, multi_modal_reinforcement_is_necessary).
narrative_ontology:cs_axiom_status(multi_modal_reinforcement_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c5d6f572-81d6-4fd5-971b-a3302420a37e', multi_modal_reinforcement_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('c5d6f572-81d6-4fd5-971b-a3302420a37e', continuous_competence_maintenance).
narrative_ontology:cs_drift_state('c5d6f572-81d6-4fd5-971b-a3302420a37e', contemporary_adaptive_safety_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c5d6f572-81d6-4fd5-971b-a3302420a37e', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, public).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, procedural_design_experts).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, training_departments).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_personnel).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizations_bearing_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining high levels of operational safety and competence. They implement and fund the multi-mechanism training, benefiting from reduced risk and regulatory compliance, but bear significant costs.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, national).

% Oversee and enforce safety standards, advocating for robust competence maintenance. They benefit from improved safety records and public trust, and their mandates drive the complexity of the training requirements.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the direct burden of designing, implementing, and managing complex, multi-mechanism training programs. They face budget constraints and the challenge of integrating diverse methods without clear optimal guidance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_departments, payer,
    moderate, biographical, constrained, local).

% Must continuously engage in various training activities (simulations, refreshers, audits) to maintain their competence and employment. They invest significant time and effort, often without direct input on training design.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_personnel, payer,
    moderate, biographical, identity_locked, local).

% Benefits from the enhanced safety and reliability of critical services (e.g., aviation, nuclear power, healthcare) that result from maintained operational competence. Their trust is essential for the legitimacy of high-reliability organizations.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, public, beneficiary,
    organized, generational, mobile, national).

% Provide specialized simulation technologies and services, profiting from the continuous demand for multi-mechanism training. They advocate for the inclusion and advancement of simulation in competence occupation models.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, simulation_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Consultants and academics specializing in human factors and procedural reinforcement, who benefit from the ongoing need for expertise in designing and auditing training protocols. They contribute to the complexity of the hybrid model.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, procedural_design_experts, beneficiary,
    powerful, biographical, arbitrage, global).

% Individuals or small groups within organizations who argue for less complex or costly training approaches, often citing budget pressures or perceived inefficiencies. Their voices are typically marginalized in the face of safety mandates and expert consensus on multi-modality.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, advocates_for_simpler_training, excluded,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures critical operational competence is continuously maintained across high-stakes, dynamic environments by integrating diverse training methods (simulation, refreshers, procedural reinforcement, line audits), thereby preventing skill decay and reducing the risk of catastrophic failure.
% TRANSFER_FUNCTION: Transfers significant resources (financial budget, personnel time, cognitive load) from high-reliability organizations, training departments, and operational personnel to various training vendors, expert consultants, and internal oversight functions, in exchange for maintained competence and reduced systemic risk.
% ABSENT_VOICES: Advocates for simpler, more cost-effective, or less frequent training models are often excluded from the discourse, as the prevailing expert consensus and regulatory pressure favor comprehensive, multi-mechanism approaches. Their arguments about efficiency or alternative learning paradigms are typically overridden by safety imperatives.
% DISAPPEARANCE_RATIONALE: If the requirement for continuous, multi-mechanism competence occupation vanished overnight, organizations would likely revert to simpler, cheaper, and less frequent training. This would lead to accelerated skill decay, increased human error rates, and a higher probability of catastrophic incidents in high-reliability domains, fundamentally reorganizing the safety landscape.
% FOUNDING_PROBLEM: The persistent challenge of maintaining high-stakes operational competence in dynamic, complex environments where single training methods prove insufficient, skill decay is a constant threat, and human error can have catastrophic consequences.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports, human factors research, independent safety audits, and regulatory findings consistently highlight the ongoing need for robust, multi-faceted competence maintenance. This corroborates the problem's live status from sources outside the direct beneficiaries of the training industry.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is driven by the high cost and resource intensity of implementing and managing multiple, diverse training mechanisms without a clear optimal path, leading to potential inefficiencies and over-engineering. Suppression (0.75) is high due to strong regulatory mandates and the perceived necessity of comprehensive safety measures, which limit organizational autonomy in training design. The theater ratio (0.40) reflects that some training activities might be performed more for compliance or to demonstrate 'due diligence' rather than for their proven marginal effectiveness, especially given the lack of consensus on optimal configuration. The increasing trend in all metrics over the interval reflects the growing complexity and cost of competence maintenance as new threats emerge and regulatory expectations rise.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of safety regulators and expert consultants, this hybrid model is a necessary, evolving Rope that ensures public safety. From the perspective of training departments and operational personnel, it can feel like a Tangled Rope, where the genuine coordination function is intertwined with substantial, often inefficient, extraction of resources due to the lack of clear optimal guidance and the continuous addition of new requirements.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and safety regulators act as agenda-setters and beneficiaries, gaining from reduced risk and enhanced public trust, but also bearing significant costs. Training departments and operational personnel are primary payers, investing substantial time and resources. Simulation vendors and procedural design experts are clear beneficiaries, profiting from the demand for their services. The public is a diffuse beneficiary of enhanced safety. Advocates for simpler training are excluded, their perspectives suppressed by the dominant narrative of comprehensive, multi-modal necessity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_ambiguity,
    'Is there a truly optimal configuration of multi-mechanism competence occupation, or is the ''lack of consensus'' a permanent, irreducible feature of this domain?',
    'Longitudinal, comparative studies across diverse high-reliability organizations, coupled with advanced modeling of skill decay and training effectiveness, to identify convergent best practices or fundamental limits to optimization.',
    'If an optimal configuration is found, the extractiveness and theater ratio could decrease significantly as resources are allocated more efficiently. If it''s irreducible, the current high extraction and theater may be inherent to the problem, shifting the classification towards a more ''natural'' (though still costly) state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_ambiguity, conceptual, 'Whether the ''lack of consensus'' on optimal training configuration is a resolvable problem or an inherent characteristic.').

omega_variable(
    cost_benefit_justification,
    'Is the high cost and complexity of multi-mechanism training justified by the marginal safety gains, or does it represent an over-engineered, diminishing-returns response to risk?',
    'Rigorous cost-benefit analysis comparing the financial and human resource investment in hybrid training against quantifiable reductions in incident rates, severity, and long-term societal costs, accounting for counterfactuals.',
    'If the cost-benefit ratio is unfavorable, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function diminished, potentially shifting it closer to a Snare. If favorable, the current extraction would be seen as a necessary cost of a robust Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_justification, empirical, 'Evaluation of whether the high cost of hybrid training yields proportional safety benefits.').

omega_variable(
    measurement_validity_fragmentation,
    'Are the diverse training mechanisms (simulation, audits, refreshers) truly measuring and reinforcing a coherent, unified competence kernel, or are they creating fragmented, performative compliance that does not translate to integrated operational effectiveness?',
    'Integrated assessment frameworks that correlate performance across different training modalities with real-world operational outcomes, identifying discrepancies or gaps in competence transfer and integration.',
    'If fragmentation is significant, the effective coordination function of the constraint is lower than perceived, increasing its effective extractiveness and theater ratio, as resources are spent on activities that do not genuinely enhance integrated competence. This would push the classification towards a more extractive Tangled Rope or even a Piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_validity_fragmentation, empirical, 'Whether multi-mechanism training genuinely integrates competence or leads to fragmented, performative compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.33).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.36).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.68).

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
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, safety_regulatory_compliance).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, human_factors_research_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
