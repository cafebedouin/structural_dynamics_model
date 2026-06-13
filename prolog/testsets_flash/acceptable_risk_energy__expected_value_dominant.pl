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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Expected Value Dominant Energy Risk Assessment
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint defines acceptable risk in energy policy as minimizing
 *   aggregate expected harm, primarily using mortality-per-TWh metrics. It is
 *   a specific reading of the broader 'acceptable_risk_energy' kernel,
 *   emphasizing statistical averages over catastrophic tails or option value.
 *   This reading leads to policies that favor energy sources with low average
 *   mortality (e.g., nuclear, renewables) and disfavor those with high
 *   average mortality (e.g., fossil fuels due to air pollution), even if the
 *   latter have no catastrophic tail risk. The fossil fuel industry and
 *   communities reliant on it are the primary victims, bearing the costs of
 *   transition and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.6).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.7).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected Value Dominant Energy Risk Assessment").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'b15edaf6-dd82-4106-8088-27f3556be808').
narrative_ontology:cs_kernel_codification('b15edaf6-dd82-4106-8088-27f3556be808', formalized).
narrative_ontology:cs_authority_grounding('b15edaf6-dd82-4106-8088-27f3556be808', expertise).
narrative_ontology:cs_interpretation_layer_present('b15edaf6-dd82-4106-8088-27f3556be808').
narrative_ontology:cs_reading_relation('b15edaf6-dd82-4106-8088-27f3556be808', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('b15edaf6-dd82-4106-8088-27f3556be808', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('b15edaf6-dd82-4106-8088-27f3556be808', foundational, risk_is_aggregate_expected_value).
narrative_ontology:cs_axiom_status(risk_is_aggregate_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('b15edaf6-dd82-4106-8088-27f3556be808', risk_is_aggregate_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('b15edaf6-dd82-4106-8088-27f3556be808', foundational, mortality_is_primary_risk_metric).
narrative_ontology:cs_axiom_status(mortality_is_primary_risk_metric, holdable).
narrative_ontology:cs_axiom_grounding('b15edaf6-dd82-4106-8088-27f3556be808', mortality_is_primary_risk_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('b15edaf6-dd82-4106-8088-27f3556be808', rational_decision_theory_framework).
narrative_ontology:cs_drift_state('b15edaf6-dd82-4106-8088-27f3556be808', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b15edaf6-dd82-4106-8088-27f3556be808', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_agencies).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, energy_consumers_in_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for policies that reduce aggregate mortality from energy production, using mortality-per-TWh as a key metric. They shape regulations and public discourse based on this principle.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of regulations, carbon taxes, and public disfavor driven by this risk assessment principle. Their business model is directly challenged, leading to reduced investment and market share.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Experience job losses, economic decline, and social disruption as policies based on this principle accelerate the phase-out of coal. Their options for economic transition are limited.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer,
    powerless, generational, trapped, local).

% Benefit from this principle as nuclear power typically has very low mortality-per-TWh, making it a favored option despite its catastrophic tail risks. They gain political and financial support.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Benefit from this principle as renewable energy sources (solar, wind) also have very low mortality-per-TWh, driving policy support and investment towards their sector.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% May face higher energy prices or reduced reliability during the transition away from fossil fuels, as new infrastructure is built and old systems are retired. Their choices are limited by available energy options.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_consumers_in_transition, payer,
    moderate, immediate, constrained, national).

% Argue that expected value calculations fail to adequately account for low-probability, high-impact events (e.g., nuclear meltdowns, climate tipping points). Their perspective is marginalized by this principle's focus on aggregate expected harm.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_theorists, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, public_health_agencies).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative framework for comparing the health impacts of different energy technologies, enabling coordinated policy decisions to minimize public health burdens.
% TRANSFER_FUNCTION: Transfers societal costs associated with air pollution and occupational hazards from fossil fuel production (borne by the public) to the fossil fuel industries and their consumers, by disincentivizing those energy pathways.
% ABSENT_VOICES: Catastrophic risk theorists and advocates for energy system resilience (option value) are largely absent from the core decision-making process when this principle is dominant; they would argue for different risk metrics and policy priorities.
% DISAPPEARANCE_RATIONALE: If this principle vanished, energy policy would lose a primary quantitative justification for phasing out fossil fuels. Investment would shift, regulatory pressure would ease, and the energy transition would slow or reverse, leading to a reorganization of energy markets and public health outcomes.
% FOUNDING_PROBLEM: The problem of comparing disparate health and environmental risks across a complex energy portfolio to make rational policy decisions.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations and international bodies (e.g., WHO, IPCC) corroborate the ongoing need for such a framework to address the health impacts of energy. While the specific methodology is contested, the underlying problem of comparative risk assessment remains live.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.6) reflects the costs imposed on industries and communities whose energy pathways are deemed 'unacceptable' by this metric. Suppression (0.7) is high because this principle requires active policy enforcement (regulations, subsidies, carbon pricing) to shift energy portfolios away from high-mortality-per-TWh sources. Theater ratio is low (0.1) as the principle is genuinely applied in policy decisions, not merely for show. Accessibility collapse is moderate (0.6) as alternatives are not completely foreclosed but are heavily disincentivized. Resistance is moderate (0.4) from affected industries and communities.
 *
 * PERSPECTIVAL GAP:
 *   Advocates of this reading (e.g., public health agencies, some environmental groups) experience it as a rational, beneficial coordination mechanism for public good. Fossil fuel industries and related communities experience it as an extractive snare, as their livelihoods are directly targeted by policies derived from this principle. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and advocates for nuclear/renewable energy are beneficiaries (d near 0.0) as their policy preferences are codified and their goals advanced. Fossil fuel industries and coal mining communities are targets (d near 1.0) as their operations are directly curtailed and costs imposed. Energy consumers in transition are also targets, bearing the costs of shifting infrastructure and potentially higher energy prices during the transition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, universally applicable risk assessment principle, or one reading of the ''acceptable_risk_energy'' kernel?',
    'Analysis of policy debates and expert testimony: if alternative, structurally distinct principles are actively advocated and applied, it is a reading.',
    'If a reading, its classification is contingent on the acceptance of its underlying axioms, and its persistence is subject to contestation from sibling readings. If a universal principle, its classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''expected_value_dominant'' reading of the ''acceptable_risk_energy'' kernel.').

omega_variable(
    catastrophic_tail_delta,
    'How would the victim set and suppression mechanisms change if the ''catastrophic_tail_dominant'' reading were adopted?',
    'Simulate policy outcomes under the sibling reading: identify which energy pathways would be suppressed and which populations would bear the costs of that suppression.',
    'The victim set would shift to include populations reliant on energy sources with low-probability, high-impact risks (e.g., nuclear power), and suppression would target those pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_tail_delta, conceptual, 'The ''catastrophic_tail_dominant'' reading would re-weight risks, potentially shifting the victim set and suppression targets.').

omega_variable(
    option_value_delta,
    'How would the beneficiary structure and policy flexibility change if the ''option_value_preserving'' reading were adopted?',
    'Analyze policy frameworks designed for deep uncertainty: identify which actors benefit from maintaining diverse energy portfolios and which constraints are relaxed to enable this flexibility.',
    'The beneficiary set would expand to include actors advocating for diverse energy portfolios, and the constraint would shift to prioritize flexibility over immediate cost minimization, potentially reducing suppression on some pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_delta, conceptual, 'The ''option_value_preserving'' reading would prioritize flexibility, altering beneficiary structures and potentially reducing suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.12).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.11).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, renewable_energy_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'acceptable_risk_energy' kernel. Its ε value differs significantly from sibling readings due to different risk prioritization and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
