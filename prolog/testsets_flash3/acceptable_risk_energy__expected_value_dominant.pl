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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Acceptable Risk: Expected Value Dominant (Energy Policy)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint represents the 'expected_value_dominant' reading of
 *   acceptable risk in energy policy. It asserts that acceptable risk is
 *   achieved by minimizing aggregate expected harm, typically quantified as
 *   mortality per terawatt-hour (TWh) across all energy pathways. This
 *   framework heavily discounts low-probability, high-impact events (like
 *   nuclear accidents) in favor of addressing statistically certain, diffuse
 *   harms (like air pollution from fossil fuels). The constraint's operation
 *   leads to the suppression of energy sources with high expected mortality,
 *   even if they offer other benefits, and elevates those with low expected
 *   mortality. This reading is one of several competing interpretations of
 *   'acceptable risk' in energy policy, each with different implications for
 *   policy and resource allocation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.25).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.7).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.25).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Acceptable Risk: Expected Value Dominant (Energy Policy)").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '5df63ec5-7ba8-43ab-b4af-27ec4cf116bb').
narrative_ontology:cs_kernel_codification('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', formalized).
narrative_ontology:cs_authority_grounding('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', expertise).
narrative_ontology:cs_interpretation_layer_present('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb').
narrative_ontology:cs_reading_relation('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', foundational, risk_is_probability_times_consequence).
narrative_ontology:cs_axiom_status(risk_is_probability_times_consequence, holdable).
narrative_ontology:cs_axiom_grounding('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', risk_is_probability_times_consequence, empirically_contingent).
narrative_ontology:cs_axiom('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', foundational, aggregate_harm_minimization_is_optimal).
narrative_ontology:cs_axiom_status(aggregate_harm_minimization_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', aggregate_harm_minimization_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', rational_actor_expected_utility).
narrative_ontology:cs_drift_state('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5df63ec5-7ba8-43ab-b4af-27ec4cf116bb', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, economic_planners).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policies that prioritize the reduction of aggregate, statistically significant harms like air pollution from fossil fuels, aligning with their mission to improve population health outcomes. They provide data and advocacy for this risk framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_advocates, beneficiary,
    organized, generational, mobile, national).

% Utilize this framework to make rational resource allocation decisions in energy policy, aiming to maximize societal welfare by minimizing quantifiable expected costs (including mortality). They set the policy agenda based on these calculations.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, economic_planners, agenda_setter,
    institutional, generational, constrained, national).

% Bear significant costs as their operations are heavily penalized by this framework due to high mortality-per-TWh metrics from air pollution and accidents. They face pressure to reduce emissions or transition to other energy sources, with limited options for continued operation under the old risk profile.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Suffer economic and social disruption as policies driven by this framework lead to the decline of coal-fired power and mining. Their livelihoods are directly impacted by the suppression of fossil fuel pathways, with few alternative economic opportunities.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer,
    powerless, biographical, trapped, local).

% Benefit from this framework as the low expected mortality-per-TWh of nuclear power (discounting rare catastrophic events by probability) makes it appear highly favorable compared to fossil fuels. They use these metrics to argue for nuclear expansion.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_power_advocates, beneficiary,
    organized, generational, mobile, national).

% Observe the application of this framework, noting that while it reduces aggregate harm, it may not adequately address the disproportionate impact of energy infrastructure on marginalized communities, even if the overall expected value is minimized. They advocate for more granular risk assessments.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, environmental_justice_groups, observer,
    organized, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent, quantitative framework for comparing diverse energy risks (e.g., air pollution, accidents, climate change impacts) on a common metric (mortality per TWh), enabling rational policy decisions to minimize overall societal harm.
% TRANSFER_FUNCTION: Transfers societal resources and political capital away from energy pathways with high expected mortality (e.g., fossil fuels) towards those with lower expected mortality (e.g., renewables, nuclear), based on quantitative risk assessment.
% ABSENT_VOICES: Communities disproportionately affected by specific energy projects (even if aggregate risk is low) or those who prioritize avoiding any chance of catastrophic, irreversible harm (regardless of probability) are often marginalized in this aggregate, expected-value-driven discourse. Their concerns are not easily captured by mortality-per-TWh metrics.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy would lose its primary quantitative basis for comparing risks. Decisions would likely revert to more qualitative, politically driven, or single-hazard-focused approaches, leading to a different mix of energy sources and a different distribution of risks and benefits.
% FOUNDING_PROBLEM: Energy policy lacked a consistent, objective method for comparing the diverse and complex risks associated with different energy sources, leading to inconsistent decision-making and suboptimal public health outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Academic risk analysts and public health organizations corroborate the ongoing need for a consistent risk assessment framework. While the specific metrics and their application are debated, the underlying problem of comparing diverse energy risks remains live.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low because the framework aims for societal benefit, but it does extract from industries and communities whose activities are deemed high-risk. Suppression (0.70) is high because this framework actively marginalizes and disincentivizes energy pathways that do not meet its criteria, requiring active policy enforcement (e.g., regulations, subsidies, carbon pricing) to shift the energy mix. Theater ratio is low (0.10) as the framework is genuinely applied in policy decisions, not merely performed. Accessibility collapse is high (0.80) because once this framework is adopted, alternative risk assessment methodologies or energy pathways become difficult to justify. Resistance (0.30) comes from industries and communities negatively impacted by the framework's policy implications.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of economic planners and public health advocates, this framework is a rational, objective tool for societal benefit. From the perspective of fossil fuel industries and affected communities, it is a coercive mechanism that devalues their livelihoods and imposes costs based on a narrow definition of risk. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health advocates and economic planners are beneficiaries, as the framework aligns with their goals of optimizing societal health and welfare. Nuclear power advocates also benefit, as nuclear's low expected mortality per TWh makes it favorable. Fossil fuel industries and coal mining communities are victims, bearing the costs of policies that suppress their activities. Environmental justice groups act as observers, analyzing the framework's impact on equity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_risk_weighting,
    'Does the expected-value approach adequately account for low-probability, high-impact catastrophic risks (e.g., nuclear meltdowns, long-term climate tipping points) by simply multiplying probability by consequence, or do such risks require a different weighting?',
    'Development of a societal consensus on the ethical treatment of catastrophic risk, or empirical evidence demonstrating that expected-value approaches consistently lead to suboptimal outcomes when catastrophic risks are present.',
    'If catastrophic risks are deemed to require higher weighting, the framework''s classification of nuclear power might shift, and its overall legitimacy could be challenged, potentially leading to a reclassification towards a ''tangled_rope'' if the framework is maintained coercively despite its perceived inadequacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophic_risk_weighting, conceptual, 'Whether expected-value models appropriately handle catastrophic risks.').

omega_variable(
    externalities_quantification_accuracy,
    'Are all relevant externalities (e.g., long-term ecological damage, social disruption from resource extraction, geopolitical instability) accurately and comprehensively quantified in the mortality-per-TWh metrics?',
    'Improved interdisciplinary research and standardized methodologies for quantifying a broader range of energy-related externalities, followed by independent audits of the metrics used in policy decisions.',
    'If significant unquantified externalities exist, the ''expected value'' derived from current metrics would be misleading, potentially leading to misallocation of resources and a re-evaluation of the framework''s claimed beneficence. This could increase its perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalities_quantification_accuracy, empirical, 'Accuracy and completeness of externality quantification in risk metrics.').

omega_variable(
    intergenerational_equity_discounting,
    'Does the discounting of future harms in expected-value calculations adequately address intergenerational equity, particularly for long-lived pollutants or waste products?',
    'Ethical and economic debates leading to revised societal discount rates for intergenerational harms, or the adoption of alternative decision frameworks that explicitly prioritize future generations'' well-being.',
    'If current discounting practices are found to be inequitable, the framework''s long-term policy recommendations could be challenged, potentially shifting its perceived beneficiaries and victims over generational time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_discounting, preference, 'Ethical implications of discounting future harms in expected-value models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is the 'expected_value_dominant' reading of the 'acceptable_risk_energy' kernel. It focuses on minimizing aggregate expected harm. It coexists with 'catastrophic_tail_dominant' (prioritizing extreme events) and 'option_value_preserving' (maintaining flexibility), which represent alternative approaches to energy risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
