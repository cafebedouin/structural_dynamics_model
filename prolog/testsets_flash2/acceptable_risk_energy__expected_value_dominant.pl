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
 *   This constraint represents the 'expected_value_dominant' reading of
 *   acceptable energy risk, where policy prioritizes minimizing aggregate
 *   expected harm (e.g., mortality per TWh) across all energy pathways. This
 *   framework fully weights the diffuse, chronic harms of fossil fuels (air
 *   pollution, mining deaths) while discounting low-probability, high-impact
 *   events (e.g., nuclear accidents) by their probability. It is a
 *   'tangled_rope' because it genuinely coordinates risk comparison but
 *   extracts from fossil fuel industries and associated communities by
 *   systematically de-legitimizing their pathways, requiring active
 *   enforcement through regulatory and policy mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected Value Dominant Energy Risk Assessment").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'af88bb74-817b-4b94-948d-3b3a967602be').
narrative_ontology:cs_kernel_codification('af88bb74-817b-4b94-948d-3b3a967602be', formalized).
narrative_ontology:cs_authority_grounding('af88bb74-817b-4b94-948d-3b3a967602be', expertise).
narrative_ontology:cs_interpretation_layer_present('af88bb74-817b-4b94-948d-3b3a967602be').
narrative_ontology:cs_reading_relation('af88bb74-817b-4b94-948d-3b3a967602be', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('af88bb74-817b-4b94-948d-3b3a967602be', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('af88bb74-817b-4b94-948d-3b3a967602be', foundational, aggregate_expected_harm_is_primary_metric).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('af88bb74-817b-4b94-948d-3b3a967602be', aggregate_expected_harm_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('af88bb74-817b-4b94-948d-3b3a967602be', foundational, probabilistic_discounting_is_valid_for_low_probability_events).
narrative_ontology:cs_axiom_status(probabilistic_discounting_is_valid_for_low_probability_events, holdable).
narrative_ontology:cs_axiom_grounding('af88bb74-817b-4b94-948d-3b3a967602be', probabilistic_discounting_is_valid_for_low_probability_events, empirically_contingent).
narrative_ontology:cs_reference_frame('af88bb74-817b-4b94-948d-3b3a967602be', rational_actor_expected_utility_maximization).
narrative_ontology:cs_drift_state('af88bb74-817b-4b94-948d-3b3a967602be', contemporary_deep_uncertainty_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('af88bb74-817b-4b94-948d-3b3a967602be', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and apply the methodologies for calculating expected value of harm, often influencing policy decisions by framing risk in these terms. Their professional identity is tied to quantitative risk assessment.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, risk_analysts, agenda_setter,
    institutional, generational, constrained, global).

% Adopt and implement energy policies based on expected value risk assessments, often facing pressure from various industry and advocacy groups. They benefit from a clear, quantifiable metric for decision-making.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from this framework as it discounts the low-probability, high-impact risks of nuclear power (e.g., meltdowns) in favor of its low expected mortality per TWh, making it appear safer than fossil fuels.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Benefit from this framework as their energy sources typically have very low expected mortality per TWh, reinforcing their position as the safest options.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear significant costs as the high expected mortality from air pollution and mining accidents associated with fossil fuels are fully weighted, leading to increased regulation, taxation, and public pressure against their operations.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Suffer from the economic decline and job losses as policies shift away from fossil fuels due to their high expected harm. Their livelihoods are directly tied to an industry deemed high-risk by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer,
    powerless, biographical, trapped, local).

% Are the direct victims of the aggregate expected harm from fossil fuels, experiencing health issues and reduced quality of life. They bear the costs of the 'acceptable risk' calculation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations, payer,
    powerless, immediate, trapped, local).

% Argue that expected value calculations fail to adequately account for low-probability, high-impact catastrophic events (e.g., nuclear meltdowns, climate tipping points) that could have existential consequences. Their concerns are systematically downweighted by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_theorists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantifiable metric (mortality per TWh) for comparing the safety of diverse energy technologies, enabling policy makers to make decisions based on a common risk language.
% TRANSFER_FUNCTION: Transfers social and political legitimacy, and thus resources, away from energy sources with high expected aggregate harm (e.g., fossil fuels) towards those with lower expected harm (e.g., nuclear, renewables).
% ABSENT_VOICES: Catastrophic risk theorists and those advocating for option value preservation are marginalized; they would argue for frameworks that prioritize avoiding extreme outcomes or maintaining flexibility, rather than solely minimizing expected value.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would lose a primary quantitative justification. Debates would shift to qualitative risk factors, catastrophic potential, and long-term flexibility, leading to a significant re-evaluation of energy portfolios and potentially different investment patterns.
% FOUNDING_PROBLEM: Energy policy decisions were often made on qualitative, emotional, or politically motivated grounds, lacking a consistent, objective method to compare the safety profiles of vastly different energy sources.
% FOUNDING_PROBLEM_CORROBORATION: Risk analysts and policy makers attest that the problem of inconsistent risk comparison remains live, and this framework provides a necessary tool. Critics, however, argue that while the original problem was real, this framework's dominance now suppresses alternative, equally valid risk perspectives.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the framework's application leads to significant economic and social costs for fossil fuel industries and communities, who are forced to internalize the full 'expected' cost of their operations. Suppression (0.78) is high because this framework actively suppresses alternative risk assessment methodologies (e.g., those prioritizing catastrophic tails or option value) and leads to policies that constrain fossil fuel development. Theater ratio is low (0.1) as the framework is genuinely applied in policy, not merely performed. Accessibility collapse is moderate (0.7) as alternative risk frameworks exist but are less influential in mainstream policy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear and renewable advocates, this framework is a legitimate 'rope' that correctly identifies the safest energy pathways. From the perspective of fossil fuel industries and communities, it is a 'snare' that unfairly targets their livelihoods by selectively emphasizing certain types of harm while downplaying others (e.g., economic disruption from rapid energy transition). The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk analysts and policy makers act as agenda-setters, benefiting from a clear decision metric. Nuclear and renewable energy advocates are beneficiaries, as their preferred technologies fare well under this metric. Fossil fuel industries and communities, along with populations affected by air pollution, are the primary payers/victims, bearing the costs of the framework's application. Catastrophic risk theorists are excluded, as their concerns are systematically downweighted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide objective risk comparison is still live, but its dominance has led to a form of 'mandatrophy' where the method itself has become a tool for extraction, rather than pure coordination. The initial coordination function (standardizing risk comparison) is now intertwined with an extractive function (de-legitimizing certain energy pathways).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discounting_catastrophic_risk,
    'Does the expected value framework adequately account for low-probability, high-impact catastrophic events, or does its probabilistic discounting fundamentally misrepresent their true risk?',
    'Development of robust decision-making frameworks under deep uncertainty that explicitly incorporate catastrophic tail risks without relying solely on expected value, and their adoption in policy.',
    'If inadequate, the framework''s classification would shift towards a ''snare'' for populations exposed to catastrophic risks, and its legitimacy would be severely undermined, potentially leading to a re-evaluation of nuclear energy''s ''safety''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discounting_catastrophic_risk, conceptual, 'The conceptual adequacy of expected value for catastrophic risks.').

omega_variable(
    scope_of_harm_metrics,
    'Are the mortality-per-TWh metrics comprehensive enough to capture all relevant harms (e.g., ecosystem damage, social disruption, intergenerational equity), or do they selectively emphasize certain types of harm?',
    'Expansion of risk assessment metrics to include a broader range of environmental, social, and ethical considerations, and their integration into policy decisions.',
    'If too narrow, the framework''s ''beneficiaries'' might be understating the true costs of their preferred energy sources, and the ''victims'' might be bearing unacknowledged harms, shifting the classification towards a ''snare'' for those excluded harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_metrics, empirical, 'The comprehensiveness of mortality-per-TWh metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(acce_tr_t1985, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(acce_be_t1985, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(acce_su_t1985, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'acceptable_risk_energy' kernel, focusing on expected value minimization. It is linked to sibling readings 'catastrophic_tail_dominant' and 'option_value_preserving' which offer alternative risk assessment frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
