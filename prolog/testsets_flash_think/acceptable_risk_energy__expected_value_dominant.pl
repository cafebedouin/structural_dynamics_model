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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Acceptable Risk: Expected Value Dominant Reading
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint represents the 'expected_value_dominant' reading of the
 *   broader 'acceptable_risk_energy' kernel. It defines acceptable risk as
 *   minimizing aggregate expected harm across all energy pathways, primarily
 *   using mortality-per-TWh metrics. This approach gives full weight to
 *   deaths from fossil fuel air pollution and mining, while discounting
 *   low-probability, high-impact events (like nuclear accidents) by their
 *   probability. The framework actively suppresses energy pathways that do
 *   not align with this minimization principle, particularly fossil fuels,
 *   leading to a high degree of extraction from and resistance by those
 *   industries. The claimed type is 'tangled_rope' because it provides a
 *   coordination function (rational risk comparison) but achieves it through
 *   asymmetric extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Acceptable Risk: Expected Value Dominant Reading").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '60354cba-e971-44c7-ae4a-3140a3c32c37').
narrative_ontology:cs_kernel_codification('60354cba-e971-44c7-ae4a-3140a3c32c37', formalized).
narrative_ontology:cs_authority_grounding('60354cba-e971-44c7-ae4a-3140a3c32c37', expertise).
narrative_ontology:cs_interpretation_layer_present('60354cba-e971-44c7-ae4a-3140a3c32c37').
narrative_ontology:cs_reading_relation('60354cba-e971-44c7-ae4a-3140a3c32c37', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('60354cba-e971-44c7-ae4a-3140a3c32c37', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('60354cba-e971-44c7-ae4a-3140a3c32c37', foundational, aggregate_expected_harm_is_primary_metric).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('60354cba-e971-44c7-ae4a-3140a3c32c37', aggregate_expected_harm_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('60354cba-e971-44c7-ae4a-3140a3c32c37', secondary, mortality_per_twh_is_sufficient_metric).
narrative_ontology:cs_axiom_status(mortality_per_twh_is_sufficient_metric, holdable).
narrative_ontology:cs_axiom_grounding('60354cba-e971-44c7-ae4a-3140a3c32c37', mortality_per_twh_is_sufficient_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('60354cba-e971-44c7-ae4a-3140a3c32c37', rational_decision_theory_framework).
narrative_ontology:cs_drift_state('60354cba-e971-44c7-ae4a-3140a3c32c37', contemporary_energy_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('60354cba-e971-44c7-ae4a-3140a3c32c37', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, risk_analysts_and_policymakers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, society_at_large).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_dependent_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors define and apply the 'acceptable risk' framework, prioritizing aggregate expected harm minimization. They benefit from the clarity and quantitative rigor of this approach, which grounds their policy recommendations in a seemingly objective metric.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, risk_analysts_and_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Bear significant costs as their operations (mining, air pollution) contribute heavily to the 'mortality-per-TWh' metric. This framework drives policies that suppress their pathways, requiring costly mitigation or divestment. Their exit options are limited by sunk costs and market structure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Strongly advocate for this framework as it aligns with their mission to reduce preventable deaths and illnesses. They benefit from policies that prioritize public health outcomes by de-emphasizing other energy policy considerations.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_organizations, beneficiary,
    organized, generational, mobile, global).

% Benefit from this framework because renewable energy sources generally have lower mortality-per-TWh metrics compared to fossil fuels, bolstering the case for their expansion and displacing higher-risk alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Experience economic and social disruption as policies driven by this framework lead to the decline of fossil fuel industries, impacting jobs and local economies. They bear the costs of transition without direct benefit from the aggregate harm reduction.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_dependent_communities, payer,
    powerless, biographical, trapped, local).

% Benefits diffusely from the overall reduction in aggregate expected harm and improved public health outcomes, but individual members may not perceive direct benefits or may bear indirect costs (e.g., higher energy prices, job losses in specific sectors).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, society_at_large, beneficiary,
    powerless, generational, constrained, universal).

% Their concerns about low-probability, high-impact events (e.g., nuclear accidents, climate tipping points) are systematically downweighted or excluded by a framework focused solely on aggregate expected value. They are outside the dominant policy conversation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% Their arguments for maintaining diverse energy pathways to preserve flexibility under deep uncertainty are marginalized by a framework that seeks to optimize for a single, current expected value metric. They are not part of the core decision-making process.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, option_value_advocates, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative method for comparing the health and safety risks of different energy technologies, enabling policymakers to make decisions aimed at minimizing overall societal harm.
% TRANSFER_FUNCTION: Transfers the burden of risk (and associated costs of mitigation or suppression) from pathways with high mortality-per-TWh metrics (e.g., fossil fuels) to those with lower metrics (e.g., renewables), and from specific industries/communities to society at large (via diffuse benefits).
% ABSENT_VOICES: Advocates for catastrophic risk avoidance and for preserving option value in energy policy are structurally excluded. They would argue for different weighting of risks and for valuing flexibility, but their frameworks are not dominant in this decision-making paradigm.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would immediately lose their primary quantitative justification. Different risk assessment methods (e.g., prioritizing catastrophic tails, or valuing option flexibility) would likely emerge, leading to a significant shift in energy investment, regulation, and the overall energy mix, with profound impacts on industries and public health outcomes.
% FOUNDING_PROBLEM: The problem of comparing disparate energy technologies with different risk profiles to make rational, public-health-oriented policy decisions.
% FOUNDING_PROBLEM_CORROBORATION: Risk analysts and public health organizations attest that the problem of comparing energy risks remains live and complex. While fossil fuel industries dispute the weighting of their risks, the need for a comparative framework is widely acknowledged by independent scientific bodies and international organizations.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because the framework imposes significant costs on industries and communities associated with higher mortality-per-TWh energy sources, without fully compensating them for the transition. Suppression is also high (0.75) due to the active policy and regulatory measures required to de-prioritize and phase out these pathways. Theater ratio is low (0.15) as the methodology is largely functional and data-driven, with little performative maintenance. Accessibility collapse is high (0.70) because alternative risk assessment frameworks are marginalized. Resistance is moderate (0.60) from affected industries and communities. The temporal measurements show a gradual increase in both extractiveness and suppression as this framework gained dominance in energy policy discourse over the past decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of risk analysts and public health organizations, this framework is a rational, beneficial coordination mechanism. From the perspective of fossil fuel industries and dependent communities, it is an extractive and suppressive force. The engine's computation of per-seat classification will reflect this divergence, with beneficiaries experiencing a 'rope-like' function and victims experiencing a 'snare-like' function, despite the overall 'tangled_rope' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk analysts and policymakers, along with public health and renewable energy advocates, are beneficiaries (low d) as this framework aligns with their goals and empowers their positions. Fossil fuel industries and dependent communities are clear targets (high d) as they bear the direct costs of suppression and transition. Society at large is a diffuse beneficiary of reduced aggregate harm, but also an indirect payer through potential economic shifts. Advocates for alternative risk frameworks are excluded, experiencing high d due to their marginalization.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct reading of the ''acceptable_risk_energy'' kernel, or merely a policy preference within a broader consensus?',
    'Analysis of policy debates and academic literature to identify fundamental, irreconcilable differences in axiomatic assumptions or core methodologies compared to sibling readings.',
    'If not a distinct reading, the kernel decomposition is invalid, and this constraint should be merged or re-contextualized within a single, broader ''acceptable_risk_energy'' constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s status as a distinct kernel reading.').

omega_variable(
    catastrophic_vs_expected_value_weighting,
    'How should low-probability, high-impact events (e.g., nuclear accidents, climate tipping points) be weighted in comparison to aggregate expected mortality?',
    'Development of a unified risk assessment methodology that explicitly incorporates both expected value and catastrophic tail risk, with transparent weighting parameters.',
    'If catastrophic tail risks are given higher weight, policies would shift towards avoiding those specific events, potentially increasing aggregate expected harm from other sources. This would fundamentally alter the constraint''s policy outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_vs_expected_value_weighting, preference, 'Ambiguity in weighting catastrophic vs. expected risks.').

omega_variable(
    option_value_vs_expected_value_priority,
    'Should energy policy prioritize minimizing current aggregate expected harm, or maintaining flexibility and diversity of pathways for future uncertainty?',
    'Development of decision-making frameworks that explicitly quantify the ''option value'' of maintaining diverse energy portfolios against the costs of not optimizing for current expected harm.',
    'If option value is prioritized, policies might support less ''optimal'' (by expected value) energy sources to ensure future adaptability, leading to a different energy mix and potentially higher current aggregate expected harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_vs_expected_value_priority, preference, 'Ambiguity in prioritizing current optimization vs. future flexibility.').

omega_variable(
    mortality_data_uncertainty,
    'What is the true uncertainty range for mortality-per-TWh metrics, especially for long-term and diffuse impacts like air pollution?',
    'Improved epidemiological studies, more robust long-term environmental monitoring, and standardized uncertainty quantification in risk models.',
    'Significant upward revision of uncertainty could weaken the quantitative certainty of this framework, opening space for alternative risk assessment methods or increasing resistance from affected industries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortality_data_uncertainty, empirical, 'Uncertainty in underlying mortality data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(acce_tr_t1995, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(acce_be_t1995, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(acce_su_t1995, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.15).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
