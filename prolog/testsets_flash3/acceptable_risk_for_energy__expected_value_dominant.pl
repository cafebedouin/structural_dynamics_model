% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Acceptable Risk for Energy: Expected Value Dominant Reading
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expected value dominant' reading of
 *   acceptable risk in energy policy, where annual expected costs and climate
 *   benefits are the primary determinants of acceptability. Rare,
 *   high-consequence events are weighted by their probability-consequence
 *   product, normalizing them within a calculable framework. This reading is
 *   crucial for the nuclear industry, as it allows nuclear power to exit the
 *   victim set when its expected value is favorable compared to alternatives,
 *   and frames waste disposal as a solvable engineering challenge rather than
 *   an insurmountable existential threat. It also implies a relatively low
 *   suppression of alternative tail-risk framings, as the framework aims to
 *   integrate them quantitatively.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.3).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.2).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Acceptable Risk for Energy: Expected Value Dominant Reading").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '15d16c91-ffb7-4117-89f3-6ff2708804f3').
narrative_ontology:cs_kernel_codification('15d16c91-ffb7-4117-89f3-6ff2708804f3', formalized).
narrative_ontology:cs_authority_grounding('15d16c91-ffb7-4117-89f3-6ff2708804f3', expertise).
narrative_ontology:cs_interpretation_layer_present('15d16c91-ffb7-4117-89f3-6ff2708804f3').
narrative_ontology:cs_reading_relation('15d16c91-ffb7-4117-89f3-6ff2708804f3', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('15d16c91-ffb7-4117-89f3-6ff2708804f3', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('15d16c91-ffb7-4117-89f3-6ff2708804f3', foundational, risk_is_quantifiable_by_expected_value).
narrative_ontology:cs_axiom_status(risk_is_quantifiable_by_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('15d16c91-ffb7-4117-89f3-6ff2708804f3', risk_is_quantifiable_by_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('15d16c91-ffb7-4117-89f3-6ff2708804f3', foundational, climate_benefits_outweigh_quantified_tail_risks).
narrative_ontology:cs_axiom_status(climate_benefits_outweigh_quantified_tail_risks, holdable).
narrative_ontology:cs_axiom_grounding('15d16c91-ffb7-4117-89f3-6ff2708804f3', climate_benefits_outweigh_quantified_tail_risks, instrumental).
narrative_ontology:cs_reference_frame('15d16c91-ffb7-4117-89f3-6ff2708804f3', rational_actor_decision_theory).
narrative_ontology:cs_drift_state('15d16c91-ffb7-4117-89f3-6ff2708804f3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('15d16c91-ffb7-4117-89f3-6ff2708804f3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a risk framework that quantifies and normalizes rare events, allowing for the economic viability and expansion of nuclear power projects. This framework makes waste disposal an engineering challenge with a calculable cost, rather than an existential threat.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry, beneficiary,
    institutional, generational, mobile, global).

% Supports nuclear energy as a low-carbon power source, and thus favors a risk assessment that prioritizes climate benefits and annual expected costs over hypothetical catastrophic tails, which could impede decarbonization efforts.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates, beneficiary,
    organized, civilizational, constrained, global).

% Bear the residual, albeit low-probability, risks of nuclear accidents and the long-term burden of waste storage. While the expected value framework quantifies these risks, it may not fully capture their lived experience or intergenerational equity concerns.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities, payer,
    powerless, generational, trapped, local).

% Apply the expected value methodology to assess and regulate energy projects. They are responsible for developing and enforcing safety standards based on this quantitative framework, balancing economic and safety considerations.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, risk_analysts_and_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Often advocate for communities disproportionately affected by energy infrastructure. They would challenge the expected value framework for potentially downplaying localized, cumulative, and intergenerational impacts in favor of aggregate, probabilistic benefits.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_justice_groups, excluded,
    organized, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative framework for evaluating the risks and benefits of energy projects, enabling consistent decision-making across diverse technologies and scenarios, particularly for long-term climate goals.
% TRANSFER_FUNCTION: Transfers the burden of rare, high-consequence events into a calculable, manageable risk within an annual budget, effectively transferring the 'tail risk' from an unquantifiable existential threat to an actuarial cost borne by society, offset by climate benefits.
% ABSENT_VOICES: Advocates for catastrophic tail risk and intergenerational equity are often marginalized in purely expected-value-driven assessments. They would argue that certain risks, due to their irreversibility or scale, cannot be adequately captured by probability × consequence products.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would become highly subjective and contentious, particularly for technologies like nuclear power. Investment in long-term, high-capital projects would be paralyzed by unquantifiable fears, and climate mitigation strategies would lose a key analytical tool.
% FOUNDING_PROBLEM: How to rationally compare and manage risks from different energy sources (e.g., fossil fuels, nuclear, renewables) and balance them against economic development and environmental protection goals, especially for events with low probability but high potential impact.
% FOUNDING_PROBLEM_CORROBORATION: Risk analysts, economists, and many policymakers attest that the problem of rational risk comparison remains live and central to energy planning. While environmental groups may contest the framework's application, they generally acknowledge the need for a systematic approach to risk.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the framework primarily serves a coordination function for rational decision-making, rather than directly extracting rents. Suppression is also low (0.2) as the framework is a widely accepted analytical tool, though it does implicitly suppress non-quantifiable risk concerns. Theater ratio is low (0.1) as the calculations are generally taken seriously, not merely performed for show. The framework's persistence is driven by its utility in complex decision-making, not by coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nuclear industry and climate advocates, this framework is a rational, necessary tool for progress. From the perspective of local communities and environmental justice groups, it may appear to downplay their specific vulnerabilities and intergenerational concerns. The engine's classification will reflect these differing experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and climate mitigation advocates are beneficiaries, as this framework supports their objectives. Local communities near facilities are victims, as they bear the residual risks that, while quantified, remain a burden. Risk analysts and regulators act as agenda-setters, applying and enforcing this framework. Environmental justice groups are excluded, as their qualitative concerns about equity and irreversible harm are not fully captured by the expected value model.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantification_bias,
    'Does the expected value framework inherently bias decision-making towards quantifiable risks and benefits, potentially marginalizing non-quantifiable values (e.g., intergenerational equity, intrinsic environmental value)?',
    'Qualitative social science research on decision-making processes in energy policy, examining how non-quantifiable values are (or are not) integrated alongside expected value analyses.',
    'If a significant bias is found, the framework''s effective extractiveness from marginalized groups (e.g., local communities) could be higher than measured, as their concerns are systematically undervalued.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantification_bias, conceptual, 'Bias towards quantifiable metrics in risk assessment.').

omega_variable(
    tail_risk_underestimation,
    'Are the probabilities and consequences of rare, catastrophic events (e.g., nuclear meltdown, long-term waste leakage) accurately estimated within the expected value framework, or are they systematically underestimated?',
    'Independent, long-term epidemiological and environmental studies, and expert elicitation from diverse scientific fields, to re-evaluate the true probabilities and consequences of extreme events.',
    'If tail risks are significantly underestimated, the framework''s classification could shift towards a ''snare'' for affected communities, as the true burden of risk is hidden by flawed quantification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_risk_underestimation, empirical, 'Accuracy of tail risk quantification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(acce_tr_t1985, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(acce_be_t1985, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(acce_su_t1985, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1985, 0.18).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, nuclear_waste_disposal_regulation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, carbon_emission_targets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
