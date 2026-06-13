% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Framework (Energy)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The expected-value-dominant reading of acceptable risk in energy policy
 *   operationalizes acceptable risk as the energy technology pathway that
 *   minimizes aggregate expected mortality per unit energy produced
 *   (mortality-per-TWh). Under this reading, a coal plant killing 10,000
 *   people per year from air pollution but producing high energy output can
 *   be counted as 'more acceptable' than a nuclear plant with zero chronic
 *   pollution deaths but a 0.001% chance per year of killing 100,000 people,
 *   because the expected value of the nuclear pathway's mortality is lower.
 *   The framework privileges incumbent fossil fuel systems by treating their
 *   chronic, diffuse harm as 'natural background' acceptable risk while
 *   subjecting rare, catastrophic risks to explicit probability discounting.
 *   This creates asymmetric suppression: fossil fuel harm is normalized;
 *   nuclear and renewable alternatives are evaluated against an implicit bar
 *   that privileges the status quo. This is ONE READING of a contested
 *   kernel—acceptable risk in energy policy—that competes with
 *   catastrophic-tail-dominant and option-value-preserving readings. Each
 *   reading operationalizes 'acceptable' differently and produces different
 *   policy outcomes. The constraint described here is the structure and
 *   political economy of the expected-value reading itself.
 *
 * KEY AGENTS:
 *   - fossil_fuel_industry: Primary beneficiary and co-agenda-setter; framework legitimizes incumbent coal and gas production.
 *   - energy_policy_economists: Beneficiary; framework aligns with training and professional rewards.
 *   - incumbent_baseload_operators: Beneficiary; framework delays transition and renewable capital reallocation.
 *   - fossil_fuel_affected_populations: Primary victims; chronic pollution harm counted but normalized as acceptable.
 *   - nuclear_accident_risk_bearers: Victims; low-probability risks discounted and suppressed.
 *   - climate_impact_populations: Civilizational victims; framework delays decarbonization.
 *   - environmental_health_advocates: Payers (organizational costs to contest the metric).
 *   - decision_theorists_non_expected_value_school: Excluded from policy authority.
 *   - regulatory_agencies: Institutional agenda-setters; framework embedded in official guidance.
 *   - climate_scientists: Observers; measure lock-in effects on decarbonization timeline.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Framework (Energy)").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '450f4bbb-e048-416f-996d-804905ee212e').
narrative_ontology:cs_kernel_codification('450f4bbb-e048-416f-996d-804905ee212e', fixed_text).
narrative_ontology:cs_authority_grounding('450f4bbb-e048-416f-996d-804905ee212e', expertise).
narrative_ontology:cs_interpretation_layer_present('450f4bbb-e048-416f-996d-804905ee212e').
narrative_ontology:cs_reading_relation('450f4bbb-e048-416f-996d-804905ee212e', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('450f4bbb-e048-416f-996d-804905ee212e', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('450f4bbb-e048-416f-996d-804905ee212e', foundational, aggregate_expected_mortality_primacy).
narrative_ontology:cs_axiom_status(aggregate_expected_mortality_primacy, holdable).
narrative_ontology:cs_axiom_grounding('450f4bbb-e048-416f-996d-804905ee212e', aggregate_expected_mortality_primacy, instrumental).
narrative_ontology:cs_axiom('450f4bbb-e048-416f-996d-804905ee212e', foundational, probability_weighting_of_rare_catastrophes).
narrative_ontology:cs_axiom_status(probability_weighting_of_rare_catastrophes, holdable).
narrative_ontology:cs_axiom_grounding('450f4bbb-e048-416f-996d-804905ee212e', probability_weighting_of_rare_catastrophes, empirically_contingent).
narrative_ontology:cs_reference_frame('450f4bbb-e048-416f-996d-804905ee212e', transparent_energy_choice_via_commensurable_metrics).
narrative_ontology:cs_drift_state('450f4bbb-e048-416f-996d-804905ee212e', contemporary_renewable_cost_decline_and_climate_urgency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('450f4bbb-e048-416f-996d-804905ee212e', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, energy_policy_economists).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, incumbent_baseload_operators).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_affected_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_accident_risk_bearers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, climate_impact_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).

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
 *   Extractiveness (0.68 at interval end) is high because the framework systematically privileges incumbent fossil systems and delays renewable acceleration, concentrating long-term energy policy authority in the hands of those who benefit from the present system. Suppression (0.79) is higher still because the framework's authority rests on active, continuous enforcement: alternative frameworks must be suppressed methodologically ('not rigorous'), institutionally (excluded from regulatory bodies), and epistemically (treated as ideological rather than analytical). Theater (0.42) is moderate-to-low because the framework does perform genuine coordination—it provides commensurable metrics and transparent calculation—but an increasingly large share of the enforcement effort is dedicated to defending the status quo against renewable acceleration and climate urgency rather than to authentic energy choice optimization. The measurement series shows suppression_requirement rising steadily as climate science evidence accumulates and renewable cost curves decline: the institutional machinery must work harder to hold the framework in place against competing evidence and policy pressure. The measurement grid is one shared time series; all three metrics are authored at every point for alignment.
 *
 * PERSPECTIVAL GAP:
 *   The expected-value framework produces radically different perceived types depending on where an agent sits. For the industry and policy economists, it appears as Rope: genuine coordination solving the problem of transparent energy choice. For fossil-fuel-affected populations, it appears as Snare: a pure extraction mechanism that counts their harm but then declares it acceptable. For climate scientists, it appears as Scaffold: a transitional arrangement whose sunset clause should have fired (the founding problem of transparent energy choice is solved; the framework's continued use now serves only to delay necessary decarbonization). For regulatory agencies, it appears as Piton: an atrophied coordination function maintained theatrically to justify accumulated institutional commitment. The engine's per-seat classification will capture this divergence; the narrative context and stakeholder situation descriptions establish why the divergence is structurally real, not observational noise.
 *
 * DIRECTIONALITY LOGIC:
 *   See above under perspectival_gap; the directionality values are derived from beneficiary/victim declarations and exit options, with no overrides needed—the structural positions are clear enough that the derivation chain should produce the right d values without special intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The expected-value framework exhibits classic mandatrophy: the founding mandate is to enable transparent, rational choice among energy pathways using commensurable metrics. This mandate is LIVE—the problem of comparing energy safety is real and unsolved by pure politics. BUT the framework's continued use now serves primarily to defend incumbent fossil fuel systems against renewable acceleration and climate urgency, not to optimize genuine energy choice. A policy-maker using the framework to choose a new energy mix would correctly apply it; a regulatory apparatus using it to justify the continuation of coal and gas production is using an atrophied function maintained by institutional inertia (and political economy). This divergence between founding purpose (transparent choice) and current function (incumbent defense) is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_vs_distribution_commensurability,
    'Are chronic, diffuse deaths (air pollution, mining injuries) commensurable with rare, catastrophic deaths (nuclear accidents, climate tipping points) under a single aggregate mortality metric, or are the value systems incommensurable?',
    'Philosophical analysis of decision theory axioms and empirical study of how affected populations and policy-makers trade off aggregate vs. concentrated harm across different framings.',
    'If the harm types are incommensurable (cannot be reduced to a single scalar), the framework presupposes a particular value system rather than discovering an objective metric. Accepting incommensurability would shift energy policy to approaches that honor multiple value systems (multi-criteria analysis, precautionary principle) rather than expected-value dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aggregation_vs_distribution_commensurability, conceptual, 'Whether aggregate and concentrated risk can be meaningfully compared on a single mortality scale.').

omega_variable(
    asymmetric_probability_discounting,
    'Why are fossil fuel chronic deaths weighted at full probability (nearly certain, thus counted at 1.0) while nuclear accident deaths are probability-weighted (low probability, thus heavily discounted), even though both are empirical mortality events?',
    'Formal decision theory analysis of the axiom structure: examining whether probability-weighting is applied consistently across all risk types or selectively deployed to favor incumbent technologies.',
    'If probability-weighting is asymmetric, the framework''s apparent objectivity masks a built-in bias toward incumbent systems. Consistent application of probability-weighting to all harm types (including climate tipping points) would shift the framework''s policy implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_probability_discounting, empirical, 'Whether probability discounting is applied consistently or asymmetrically across risk types.').

omega_variable(
    temporal_scope_and_climate_lock_in,
    'How does the framework''s implicit time horizon (decadal to multi-decadal energy system evolution) interact with climate impact horizons (civilizational, multi-century)? Is the framework''s mortality-per-TWh metric adequate for decisions that affect 100+ year climate trajectories?',
    'Formal analysis of the framework''s temporal scope and comparison with integrated assessment models and climate tipping-point research; empirical measurement of lock-in effects from framework-justified fossil fuel plant construction.',
    'If the framework inadequately captures civilizational-timescale harm, the policy decisions it justifies will systematically underweight climate risk. Expansion of the time horizon would shift the framework toward renewable acceleration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_and_climate_lock_in, empirical, 'Whether the framework''s temporal scope is adequate for climate-relevant energy policy.').

omega_variable(
    normalization_of_chronic_harm,
    'Is the normalization of fossil fuel chronic mortality as ''background acceptable risk'' a feature of the framework''s design, or an outcome of historical accident and incumbent system evolution?',
    'Historical analysis of how mortality-per-TWh metrics were developed and adopted; comparison with frameworks that normalize renewable or nuclear chronic harm instead.',
    'If normalization is a historical accident, the framework could be reframed to normalize renewable chronic harm and count fossil fuel harm as exceptional. If it is a design feature, the framework requires wholesale replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_of_chronic_harm, conceptual, 'Whether chronic harm normalization is a design choice or historical path-dependent outcome.').

omega_variable(
    suppression_as_enforcement_vs_suppression_as_internalization,
    'Is the measured suppression (0.79) primarily structural (regulatory bodies exclude alternative frameworks, fund expected-value research, enforce metric usage in official guidance) or internalized (decision-makers and scientists genuinely believe the framework is the most rigorous approach)?',
    'Ethnographic study of regulatory decision-making, career incentives, and scientific training; comparison of how expected-value and alternative frameworks are treated in journals, funding agencies, and policy bodies.',
    'If suppression is primarily structural, policy reform requires institutional change (fund alternative frameworks, include diverse theoretical traditions in regulatory bodies). If internalized, the barrier is cognitive and cultural, requiring broader paradigm shift in how risk is conceived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_enforcement_vs_suppression_as_internalization, empirical, 'Whether suppression of alternative frameworks operates structurally or through internalized commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__expected_value_dominant, theater_ratio, 5, 0.32).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.36).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__expected_value_dominant, theater_ratio, 15, 0.39).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__expected_value_dominant, theater_ratio, 25, 0.41).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The 'acceptable risk in energy policy' kernel decomposes into three distinct constraint stories, each operationalizing 'acceptable' according to a different normative commitment and decision-theoretic framework. EXPECTED_VALUE_DOMINANT (this story) minimizes aggregate expected mortality; CATASTROPHIC_TAIL_DOMINANT prioritizes avoiding worst-case outcomes even at higher expected aggregate harm; OPTION_VALUE_PRESERVING maintains decision flexibility under deep uncertainty. These three readings coexist as live, competing positions in energy policy discourse. Each produces different victim/beneficiary structures and different suppression mechanisms. No single reading logically forecloses the others; instead, they coexist within different institutional, scientific, and political factions. The constraint stories are linked via network.affects_constraints to show that policy decisions favoring one reading systematically disadvantage the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
