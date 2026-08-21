% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected Value Dominant Energy Risk Assessment
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint defines acceptable risk in energy policy as minimizing
 *   aggregate expected harm, primarily using mortality-per-TWh metrics. It is
 *   one reading of the broader 'acceptable_risk_energy' kernel, focusing on
 *   quantitative, probability-weighted outcomes. This framework, while
 *   appearing objective, inherently suppresses alternative risk framings
 *   (e.g., catastrophic tail risk, option value) and extracts from energy
 *   pathways (like fossil fuels) that score poorly on its chosen metric.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.7).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.8).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected Value Dominant Energy Risk Assessment").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'bb34bd49-dd78-43c4-ab32-6e482d09e596').
narrative_ontology:cs_kernel_codification('bb34bd49-dd78-43c4-ab32-6e482d09e596', formalized).
narrative_ontology:cs_authority_grounding('bb34bd49-dd78-43c4-ab32-6e482d09e596', expertise).
narrative_ontology:cs_interpretation_layer_present('bb34bd49-dd78-43c4-ab32-6e482d09e596').
narrative_ontology:cs_reading_relation('bb34bd49-dd78-43c4-ab32-6e482d09e596', acceptable_risk_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('bb34bd49-dd78-43c4-ab32-6e482d09e596', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('bb34bd49-dd78-43c4-ab32-6e482d09e596', foundational, aggregate_expected_harm_is_primary_metric).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('bb34bd49-dd78-43c4-ab32-6e482d09e596', aggregate_expected_harm_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('bb34bd49-dd78-43c4-ab32-6e482d09e596', foundational, probabilistic_discounting_is_valid).
narrative_ontology:cs_axiom_status(probabilistic_discounting_is_valid, holdable).
narrative_ontology:cs_axiom_grounding('bb34bd49-dd78-43c4-ab32-6e482d09e596', probabilistic_discounting_is_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('bb34bd49-dd78-43c4-ab32-6e482d09e596', rational_decision_theory_framework).
narrative_ontology:cs_drift_state('bb34bd49-dd78-43c4-ab32-6e482d09e596', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bb34bd49-dd78-43c4-ab32-6e482d09e596', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, risk_analysts_decision_theorists).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, communities_reliant_on_fossil_fuels).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and apply the quantitative methodologies for assessing aggregate expected harm, providing the intellectual and technical foundation for this risk framework. They benefit from the framework's adoption as it validates their expertise.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, risk_analysts_decision_theorists, agenda_setter,
    analytical, generational, analytical, global).

% Advocate for policies that reduce aggregate expected harm, particularly from air pollution and other chronic health impacts of energy production. This framework aligns with their goals by giving full weight to these harms.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_advocates, beneficiary,
    organized, generational, mobile, global).

% Benefit from a framework that highlights the high expected harm of fossil fuels, making renewable energy sources appear more favorable by comparison due to their lower expected mortality-per-TWh.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear significant costs as their operations, particularly coal and oil, are identified as having high expected aggregate harm due to air pollution and mining accidents. This framework drives policy and investment away from them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries, payer,
    institutional, biographical, constrained, global).

% Experience economic and social disruption as policies guided by this framework lead to the decline of fossil fuel industries, impacting jobs and local economies. Their identity is often tied to these industries.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, communities_reliant_on_fossil_fuels, payer,
    powerless, biographical, identity_locked, local).

% Benefit from the probabilistic discounting of low-probability, high-consequence accidents (e.g., Chernobyl, Fukushima), which makes nuclear power's expected harm profile competitive. However, they still face public perception challenges not fully captured by this metric.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates, payer).

% Are marginalized by this framework because it discounts low-probability, high-impact events (like nuclear meltdowns or climate tipping points) by their probability, rather than giving them overriding weight. They would argue for a different decision rule.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% Are sidelined by a framework that seeks to identify optimal pathways based on a single metric, as their focus is on maintaining diverse energy options to preserve flexibility under deep uncertainty, even if some options appear suboptimal by expected value.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, option_value_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative methodology for comparing and prioritizing risks across diverse energy generation pathways, enabling consistent policy decisions based on aggregate expected harm.
% TRANSFER_FUNCTION: Shifts policy preference, investment, and regulatory burden away from energy pathways with high expected aggregate harm (e.g., fossil fuels) towards those with lower expected harm (e.g., renewables, nuclear with probability-discounted tail risks).
% ABSENT_VOICES: Advocates for catastrophic risk avoidance (who would prioritize tail risks over aggregate expected value) and those who prioritize maintaining option value under deep uncertainty are structurally excluded, as their core premises are not fully integrated into this framework. Communities whose livelihoods are tied to high-expected-harm industries also lack a voice in the framework's construction.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would revert to more fragmented, qualitative, and politically driven risk assessments. There would be no consistent, widely accepted quantitative basis for comparing the diverse risks of different energy technologies, leading to less coherent public health and safety outcomes and potentially a resurgence of high-expected-harm pathways.
% FOUNDING_PROBLEM: Inconsistent, qualitative, and often politically biased risk assessments across different energy technologies, leading to suboptimal public health and safety outcomes and inefficient resource allocation in the energy sector.
% FOUNDING_PROBLEM_CORROBORATION: International scientific bodies (e.g., IPCC, WHO), national academies of science, and independent public health organizations consistently highlight the need for robust, quantitative, and consistent risk assessment in energy policy to protect public health and the environment. This corroboration comes from outside the direct beneficiaries of specific energy pathways.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.70) is high because the framework imposes significant costs on certain energy pathways by de-legitimizing them based on their expected harm profile, leading to reduced investment and increased regulation. Suppression (0.80) is also high, as it actively marginalizes and excludes alternative risk assessment methodologies and the voices that champion them. The theater ratio (0.10) is low, reflecting that the framework is genuinely applied and its calculations are taken seriously, rather than being a mere performance. Accessibility collapse is high (0.75) because it significantly narrows the range of 'acceptable' energy options. Resistance is moderate (0.60) from those whose interests are harmed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of risk analysts and public health advocates, this framework is a rational, objective tool for societal benefit. From the perspective of fossil fuel industries or catastrophic risk advocates, it is an extractive and suppressive mechanism that unfairly disadvantages their preferred or prioritized energy pathways. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk analysts and public health advocates are beneficiaries, as the framework validates their methods and achieves their goals of reducing aggregate harm. Fossil fuel industries and communities reliant on them are targets, bearing the costs of de-prioritization. Catastrophic risk and option value advocates are excluded, as their core concerns are not given full weight. Nuclear power advocates are complex: they benefit from probability discounting of tail risks but may still face public resistance not captured by the metric.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, standalone risk assessment framework, or is it one reading of the ''acceptable_risk_energy'' kernel?',
    'Analysis of policy debates and academic literature: if competing frameworks are actively discussed as alternatives to this one, it confirms its status as a reading within a contested kernel.',
    'If confirmed as a reading, the classification gains context from its relationship to sibling readings, highlighting the contestability of its foundational premises. If standalone, its classification is evaluated in isolation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as the ''expected_value_dominant'' reading of the ''acceptable_risk_energy'' kernel.').

omega_variable(
    mortality_data_completeness,
    'Are the mortality-per-TWh metrics used in this framework truly comprehensive, or do they omit significant categories of harm (e.g., long-term ecological damage, social disruption, non-fatal illnesses)?',
    'Epidemiological and ecological studies that quantify a broader range of health and environmental impacts, and their translation into comparable metrics.',
    'If significant harms are omitted, the framework''s ''expected value'' is underestimated for certain pathways, potentially shifting the classification towards higher extraction from affected populations and a stronger Snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortality_data_completeness, empirical, 'Completeness of mortality-per-TWh metrics in capturing all relevant harms.').

omega_variable(
    ethical_discounting_validity,
    'Is the ethical premise of probabilistically discounting low-probability, high-consequence events universally valid, or does it conflict with ethical frameworks that prioritize catastrophic avoidance or intergenerational equity?',
    'Philosophical and ethical discourse analysis, and public deliberation on acceptable risk principles. This is a preference-based resolution.',
    'If the discounting premise is widely rejected, the framework''s legitimacy erodes, increasing resistance and potentially reclassifying it as a Snare for those who bear un-discounted catastrophic risks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_discounting_validity, preference, 'Ethical validity of probabilistic discounting in risk assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__expected_value_dominant, theater_ratio, 5, 0.1).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.1).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__expected_value_dominant, theater_ratio, 15, 0.1).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
