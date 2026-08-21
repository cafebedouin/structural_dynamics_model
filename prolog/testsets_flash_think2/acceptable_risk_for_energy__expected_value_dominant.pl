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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Acceptable Risk for Energy: Expected Value Dominant Reading
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expected_value_dominant' reading of the
 *   'acceptable_risk_for_energy' kernel. It posits that annual expected costs
 *   and climate benefits are the primary determinants of risk acceptability,
 *   with rare events weighted strictly by their probability-consequence
 *   product. This methodology aims to provide a rational, calculable basis
 *   for energy policy decisions, particularly concerning large-scale
 *   infrastructure like nuclear power. The claimed type is 'rope' as it
 *   functions as a coordination mechanism for decision-making, but its
 *   application can lead to outcomes perceived as extractive by those whose
 *   concerns are downplayed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.35).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.6).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Acceptable Risk for Energy: Expected Value Dominant Reading").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '6e18bfff-5c33-4a58-906a-f9c37b9e1941').
narrative_ontology:cs_kernel_codification('6e18bfff-5c33-4a58-906a-f9c37b9e1941', formalized).
narrative_ontology:cs_authority_grounding('6e18bfff-5c33-4a58-906a-f9c37b9e1941', expertise).
narrative_ontology:cs_interpretation_layer_present('6e18bfff-5c33-4a58-906a-f9c37b9e1941').
narrative_ontology:cs_reading_relation('6e18bfff-5c33-4a58-906a-f9c37b9e1941', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('6e18bfff-5c33-4a58-906a-f9c37b9e1941', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('6e18bfff-5c33-4a58-906a-f9c37b9e1941', foundational, risk_is_probability_times_consequence).
narrative_ontology:cs_axiom_status(risk_is_probability_times_consequence, holdable).
narrative_ontology:cs_axiom_grounding('6e18bfff-5c33-4a58-906a-f9c37b9e1941', risk_is_probability_times_consequence, empirically_contingent).
narrative_ontology:cs_axiom('6e18bfff-5c33-4a58-906a-f9c37b9e1941', foundational, societal_welfare_maximization_via_quantification).
narrative_ontology:cs_axiom_status(societal_welfare_maximization_via_quantification, holdable).
narrative_ontology:cs_axiom_grounding('6e18bfff-5c33-4a58-906a-f9c37b9e1941', societal_welfare_maximization_via_quantification, instrumental).
narrative_ontology:cs_reference_frame('6e18bfff-5c33-4a58-906a-f9c37b9e1941', rational_choice_theory_framework).
narrative_ontology:cs_drift_state('6e18bfff-5c33-4a58-906a-f9c37b9e1941', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6e18bfff-5c33-4a58-906a-f9c37b9e1941', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, proponents_of_expected_value_analysis).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_policy_makers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, proponents_of_catastrophic_tail_risk).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, general_public).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics, economists, and risk analysts who advocate for and apply methodologies that quantify risks and benefits using expected value, benefiting from its clarity, calculability, and perceived rationality in policy debates.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, proponents_of_expected_value_analysis, agenda_setter,
    institutional, generational, arbitrage, global).

% Government officials and regulatory bodies who utilize this framework to justify energy decisions, seeking consistent, quantifiable outcomes that balance economic development, climate goals, and public safety. They benefit from a clear decision-making heuristic.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_policy_makers, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, energy_policy_makers, agenda_setter).

% Developers and operators of nuclear power plants who benefit when this framework shows nuclear energy as an acceptable risk, facilitating project approvals and investment by downplaying the unique characteristics of low-probability, high-consequence events.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry, beneficiary,
    powerful, biographical, mobile, global).

% Researchers and advocates who argue that low-probability, high-consequence events (e.g., nuclear meltdowns, long-term waste storage failures) should dominate risk calculus due to their irreversibility and intergenerational burden, regardless of their expected value. They bear the cost of their concerns being marginalized by the dominant framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, proponents_of_catastrophic_tail_risk, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, proponents_of_catastrophic_tail_risk, excluded).

% Groups focused on long-term ecological and societal impacts, whose emphasis on non-quantifiable values, intergenerational equity, and the precautionary principle is often sidelined by a framework that prioritizes quantifiable expected values.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates, excluded).

% Agencies tasked with ensuring public safety, who apply this framework to set and enforce safety standards for energy projects, balancing the calculated risks and benefits within the established methodology.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, public_safety_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from a stable energy supply and climate benefits, but bears the residual risks (calculated as acceptable by this framework) and potential costs of rare, high-consequence events, often without direct input into the risk assessment methodology.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, general_public, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, general_public, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative framework for evaluating complex energy risks and benefits, enabling consistent decision-making across diverse projects and policies by aggregating costs and benefits into a single metric.
% TRANSFER_FUNCTION: Transfers the burden of managing low-probability, high-consequence risks from decision-makers to a calculable, aggregated cost, often borne by the general public or future generations, in exchange for perceived optimal resource allocation.
% ABSENT_VOICES: Proponents of alternative risk framings (e.g., those prioritizing irreversibility, intergenerational equity, or non-quantifiable values) are structurally marginalized; they would argue for a more holistic or precautionary approach but are excluded from the dominant quantitative discourse.
% DISAPPEARANCE_RATIONALE: If this dominant expected-value framework vanished overnight, energy policy decisions would become highly contentious and potentially paralyzed. Different risk perspectives would clash without a common metric, leading to inconsistent, ad-hoc choices and significant delays in critical energy infrastructure development.
% FOUNDING_PROBLEM: To establish a rational, consistent, and defensible method for making decisions about large-scale energy infrastructure projects (e.g., nuclear power, fossil fuel extraction) that involve complex trade-offs between economic benefits, climate impacts, and low-probability, high-consequence risks.
% FOUNDING_PROBLEM_CORROBORATION: Energy economists, policy analysts, and engineering safety experts from various institutions (including government agencies, industry bodies, and independent research organizations) corroborate the ongoing need for a consistent risk assessment framework, even while acknowledging disputes over specific weighting and inclusion criteria.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is moderate (0.35) because while the framework aims for optimal societal outcomes, its inherent simplification of complex risks can lead to a de facto extraction from those who bear unquantified or marginalized costs. Suppression is moderate-high (0.60) as this framework actively marginalizes or excludes alternative risk assessment methodologies, particularly those emphasizing catastrophic tail risks or non-quantifiable values. Theater ratio is low (0.10) because the methodology is genuinely applied and functional, not merely performative. Accessibility collapse is moderate-high (0.65) as the dominance of this framework makes it difficult for alternative risk framings to gain traction in policy debates. Resistance is moderate (0.50) from environmental groups and tail-risk proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its proponents, this framework is a rational and efficient coordination mechanism for complex energy decisions. From the perspective of its victims, it is a tool that systematically justifies projects by downplaying certain risks, effectively extracting from those who prioritize non-quantifiable or catastrophic outcomes. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Proponents of expected value analysis, energy policy makers, and the nuclear industry are beneficiaries, as the framework provides a clear, defensible path for project approval and policy justification. Proponents of catastrophic tail risk and environmental advocates are victims, as their concerns are systematically downplayed or excluded from the dominant calculus. The general public is both a beneficiary (from stable energy supply) and a payer (bearing residual, calculated risks).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''expected_value_dominant'' reading of the ''acceptable_risk_for_energy'' kernel?',
    'Comparison with expert interpretations of risk assessment methodologies in energy policy, particularly how rare events are weighted and how nuclear risk is framed.',
    'If misidentified, the analysis of inter-reading relationships and the overall kernel contest would be distorted, leading to incorrect classification of the kernel''s dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verification of kernel reading instantiation.').

omega_variable(
    scope_of_quantifiability,
    'Can all relevant consequences (e.g., intergenerational equity, ecosystem services, long-term social disruption) be adequately quantified and integrated into an expected value calculation without distortion or systematic bias?',
    'Development of comprehensive, interdisciplinary methodologies for valuing non-market goods and long-term impacts, and empirical studies on the accuracy of such valuations in policy outcomes.',
    'If significant aspects remain unquantifiable or are distorted, the ''expected value'' becomes a partial, rather than comprehensive, measure, increasing the effective extraction from those bearing these unmeasured costs. This would push the constraint towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_quantifiability, empirical, 'Whether the expected value framework truly captures all relevant costs and benefits.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of catastrophic tail-risk framing structural (due to institutional adoption of expected value analysis) or internalized (due to analysts'' genuine belief in its superiority)?',
    'Post-policy-change analysis: if alternative framings gain traction after institutional mandates for expected value analysis are relaxed, it suggests structural suppression. If the preference for expected value persists among practitioners, it suggests internalized belief.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the preference for expected value persists even without explicit institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1980, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1980, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1980, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1990, 0.57).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, nuclear_power_plant_licensing).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, fossil_fuel_project_approvals).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, climate_change_mitigation_policy).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
