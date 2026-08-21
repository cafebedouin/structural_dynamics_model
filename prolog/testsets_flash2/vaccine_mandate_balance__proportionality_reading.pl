% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Principle
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of vaccine
 *   mandates, asserting that mandates are permissible only when disease
 *   severity, transmission risk, and vaccine safety meet strict
 *   proportionality thresholds, and robust exemptions are provided. This
 *   reading aims to balance public health imperatives with individual
 *   liberties, making the legitimacy of mandates context-dependent rather
 *   than categorical. The metrics reflect a constraint that, when properly
 *   applied, is moderately extractive and suppressive, as it still limits
 *   individual choice but with justification and safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.4).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.3).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Principle").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '2f1fe188-9351-4b5a-857b-345ce6e58b61').
narrative_ontology:cs_kernel_codification('2f1fe188-9351-4b5a-857b-345ce6e58b61', formalized).
narrative_ontology:cs_authority_grounding('2f1fe188-9351-4b5a-857b-345ce6e58b61', lineage).
narrative_ontology:cs_interpretation_layer_present('2f1fe188-9351-4b5a-857b-345ce6e58b61').
narrative_ontology:cs_reading_relation('2f1fe188-9351-4b5a-857b-345ce6e58b61', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2f1fe188-9351-4b5a-857b-345ce6e58b61', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('2f1fe188-9351-4b5a-857b-345ce6e58b61', foundational, state_intervention_requires_proportionality).
narrative_ontology:cs_axiom_status(state_intervention_requires_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('2f1fe188-9351-4b5a-857b-345ce6e58b61', state_intervention_requires_proportionality, deontological).
narrative_ontology:cs_axiom('2f1fe188-9351-4b5a-857b-345ce6e58b61', foundational, individual_liberty_is_defeasible_by_collective_harm).
narrative_ontology:cs_axiom_status(individual_liberty_is_defeasible_by_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('2f1fe188-9351-4b5a-857b-345ce6e58b61', individual_liberty_is_defeasible_by_collective_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('2f1fe188-9351-4b5a-857b-345ce6e58b61', liberal_democratic_constitutionalism).
narrative_ontology:cs_drift_state('2f1fe188-9351-4b5a-857b-345ce6e58b61', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f1fe188-9351-4b5a-857b-345ce6e58b61', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_exemptions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_low_risk_tolerance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease severity, transmission risk, and vaccine safety data to determine if mandates meet proportionality thresholds. They implement and enforce mandates when justified, balancing public health with individual liberties.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced disease transmission and protection against severe illness, especially when they cannot be vaccinated or have compromised immune systems. Their safety depends on high population-level immunity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Bear the cost of navigating exemption processes and may face social or professional restrictions even with valid exemptions. They are subject to the mandate's framework but are protected by its proportionality requirements.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_exemptions, payer,
    moderate, immediate, constrained, local).

% May feel compelled to vaccinate against their personal risk assessment or philosophical objections, even if they qualify for no formal exemption. They bear the psychological cost of perceived coercion, but the proportionality principle aims to limit this.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_low_risk_tolerance, payer,
    moderate, biographical, constrained, local).

% Review the legality and constitutionality of vaccine mandates, ensuring they adhere to proportionality principles and respect fundamental rights. Their rulings shape the interpretation and application of this constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for state intervention in public health that balances collective well-being with individual rights, ensuring mandates are justified by evidence and limited in scope.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy (the decision to vaccinate) to the collective for public health benefit, but only when strict proportionality criteria are met. The burden of proof for necessity rests on the state.
% ABSENT_VOICES: Those who advocate for absolute bodily autonomy, regardless of public health risk, are often excluded from the core decision-making process, as their position fundamentally rejects the premise of collective health interventions. Similarly, those who advocate for absolute public health authority, regardless of individual impact, are also constrained by this reading.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, vaccine mandates could become arbitrary or excessively coercive, leading to widespread public distrust, legal challenges, and potentially ineffective public health interventions due to lack of public buy-in. The balance between individual rights and collective good would collapse.
% FOUNDING_PROBLEM: To prevent arbitrary state overreach in public health crises while enabling effective collective action against infectious diseases, ensuring that interventions are necessary, effective, and minimally intrusive.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, human rights organizations, and medical ethicists widely corroborate the ongoing need for such a principle, citing historical abuses of state power and the potential for disproportionate responses to health crises. This corroboration comes from outside the direct beneficiaries of mandates.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) and suppression (0.3) are moderate because this reading acknowledges a legitimate, albeit limited, role for state coercion in public health. It is not zero, as individual choice is still constrained. However, it is not high, as the proportionality principle itself acts as a check on excessive extraction. The theater ratio is low (0.1) because the principle is intended to be genuinely applied, not merely performative. Resistance is moderate (0.5) because even proportional mandates will face some opposition from those prioritizing individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities would view this as a necessary and just framework for collective action, while individuals subject to mandates might still perceive it as an infringement, even if proportional. The proportionality principle itself is the mechanism for bridging this gap, but it does not eliminate the tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they gain the ability to protect public health within a legitimate framework. Vulnerable populations are clear beneficiaries. Individuals seeking exemptions or with low risk tolerance are payers, as they bear the direct or indirect costs of compliance or navigating the system. Constitutional courts act as observers, adjudicating the application of the principle.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by requiring ongoing justification for mandates based on current epidemiological and safety data. If the underlying conditions (disease severity, transmission risk, vaccine safety) change, the mandate's legitimacy under this principle would erode, preventing it from persisting beyond its functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''severity,'' ''transmission risk,'' and ''vaccine safety'' objectively measured and weighted to determine proportionality, and who decides these thresholds?',
    'Establishment of independent, transparent scientific advisory bodies with clear methodologies for risk assessment and public deliberation processes for threshold setting.',
    'Lack of clear, agreed-upon metrics and decision-makers can lead to arbitrary application of the principle, increasing perceived extractiveness and suppression. Clearer metrics would strengthen the constraint''s legitimacy and reduce resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in the operationalization of proportionality criteria.').

omega_variable(
    exemption_robustness_definition,
    'What constitutes ''robust'' exemptions, and how are they balanced against the collective benefit of a mandate?',
    'Legal precedents and ethical guidelines that define the scope and process for medical, religious, and philosophical exemptions, with clear appeal mechanisms.',
    'Weak exemptions increase extractiveness and suppression for individuals, potentially pushing the constraint towards a Snare. Overly broad exemptions could undermine the public health coordination function, making the constraint ineffective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_definition, conceptual, 'Defining the scope and strength of exemptions.').

omega_variable(
    reading_framing_contest,
    'Is this constraint a genuine ''proportionality'' principle, or is it a ''public_health_primary'' reading dressed up with proportionality language to gain legitimacy?',
    'Analysis of actual mandate implementation: if mandates are consistently applied even when proportionality thresholds are marginally met or contested, it suggests a ''public_health_primary'' framing. If mandates are frequently withdrawn or modified due to proportionality concerns, it supports this reading.',
    'If it''s a disguised ''public_health_primary'' reading, the true extractiveness and suppression would be higher, and the claimed type of Rope would be a misrepresentation, likely computing as a Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_contest, conceptual, 'Distinguishing genuine proportionality from rhetorical cover for public health primacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, public_health_emergency_powers).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, individual_rights_protections).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel. It is linked to sibling readings 'bodily_autonomy_primary' and 'public_health_primary' which represent alternative framings of the same underlying tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
