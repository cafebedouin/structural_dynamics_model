% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Acceptable Risk: Option-Value-Preserving Energy Portfolio Mandate
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint is the institutionalized policy framework that defines
 *   acceptable risk in energy planning by mandating technology-neutral
 *   portfolio maintenance to preserve decision flexibility under deep
 *   uncertainty. It requires that both nuclear and fossil-with-CCS pathways
 *   remain viable alongside renewables, enforced through capacity markets,
 *   retirement restrictions, and technology-neutral reliability standards.
 *   The framework sits between two competing normative extremes: a
 *   catastrophic-tail-dominant reading that would prioritize precautionary
 *   shutdown of risky pathways, and an expected-value-dominant reading that
 *   would optimize purely on current mortality-per-TWh metrics and likely
 *   pick a single cheapest winner.
 *
 * KEY AGENTS:
 *   - national_energy_planner: agenda_setter (institutional/constrained) â sets portfolio mandates and enforces technology neutrality
 *   - baseload_technology_providers: beneficiary (organized/constrained) â collect capacity payments and avoided retirement
 *   - ratepayers_and_taxpayers: primary payer (moderate/constrained) â fund redundant capacity through regulated rates
 *   - climate_vulnerable_communities: secondary payer (powerless/trapped) â bear incremental climate and health costs from kept-open fossil pathways
 *   - rapid_decarbonization_advocates: excluded voice (organized/constrained) â structurally sidelined by neutrality mandates
 *   - robustness_analysts: observer (analytical/analytical) â supply intellectual framework without collecting rents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.45).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.55).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Acceptable Risk: Option-Value-Preserving Energy Portfolio Mandate").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '7e00f487-06d6-488b-800b-3c00bb9818ee').
narrative_ontology:cs_kernel_codification('7e00f487-06d6-488b-800b-3c00bb9818ee', formalized).
narrative_ontology:cs_authority_grounding('7e00f487-06d6-488b-800b-3c00bb9818ee', expertise).
narrative_ontology:cs_interpretation_layer_present('7e00f487-06d6-488b-800b-3c00bb9818ee').
narrative_ontology:cs_reading_relation('7e00f487-06d6-488b-800b-3c00bb9818ee', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('7e00f487-06d6-488b-800b-3c00bb9818ee', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('7e00f487-06d6-488b-800b-3c00bb9818ee', foundational, deep_uncertainty_justifies_option_preservation).
narrative_ontology:cs_axiom_status(deep_uncertainty_justifies_option_preservation, holdable).
narrative_ontology:cs_axiom_grounding('7e00f487-06d6-488b-800b-3c00bb9818ee', deep_uncertainty_justifies_option_preservation, empirically_contingent).
narrative_ontology:cs_axiom('7e00f487-06d6-488b-800b-3c00bb9818ee', secondary, technology_neutrality_as_robustness_principle).
narrative_ontology:cs_axiom_status(technology_neutrality_as_robustness_principle, holdable).
narrative_ontology:cs_axiom_grounding('7e00f487-06d6-488b-800b-3c00bb9818ee', technology_neutrality_as_robustness_principle, instrumental).
narrative_ontology:cs_reference_frame('7e00f487-06d6-488b-800b-3c00bb9818ee', robust_portfolio_maintenance).
narrative_ontology:cs_drift_state('7e00f487-06d6-488b-800b-3c00bb9818ee', renewable_cost_revolution_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e00f487-06d6-488b-800b-3c00bb9818ee', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, baseload_technology_providers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers_and_taxpayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates technology-neutral capacity markets and retirement restrictions to prevent premature closure of nuclear and fossil-with-CCS plants. Justifies this as preserving option value under deep uncertainty about future technology costs, climate sensitivity, and policy regimes. Sets the planning standards that define acceptable risk and enforces portfolio diversity requirements.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, national_energy_planner, agenda_setter,
    institutional, generational, constrained, national).

% Receive capacity payments and avoided retirement orders that keep nuclear and fossil plants economically viable. Their long-lived assets would face accelerated closure under expected-value-optimized or catastrophic-tail-dominant frameworks. Actively advocate for the option-value preservation narrative.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, baseload_technology_providers, beneficiary,
    organized, biographical, constrained, national).

% Fund redundant baseload capacity through regulated utility rates and public subsidies. Bear the opportunity cost of capital not allocated to the currently lowest-cost or fastest-decarbonizing pathway. Cannot opt out of the integrated grid or the policy-driven cost allocation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers_and_taxpayers, payer,
    moderate, biographical, constrained, national).

% Bear incremental health and climate damages from fossil pathways kept operational longer than they would be under stricter risk frameworks. Their exposure is increased by the delay in full transition, yet they are not seated in the planning processes that trade off their harms against option value.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_vulnerable_communities, payer,
    powerless, generational, trapped, global).

% Argue for immediate retirement of fossil fuel infrastructure based on catastrophic climate risk. Their position is formally acknowledged but structurally sidelined by technology-neutrality mandates that treat fossil-with-CCS as an equally valid option to preserve.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, rapid_decarbonization_advocates, excluded,
    organized, generational, constrained, global).

% Supply the decision-theoretic modelsâreal options theory, robust decision making, deep uncertainty frameworksâthat justify maintaining multiple pathways. They neither collect rents from the constraint nor bear its direct costs, but provide the intellectual architecture the agenda setter cites.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, robustness_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, baseload_technology_providers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents premature lock-in to a single energy technology or pathway when the probability distributions over future technology costs, climate sensitivity, and policy regimes are deeply uncertain. Coordinates diverse actors around a portfolio approach that preserves reversibility and avoids fragile single-technology dependence.
% TRANSFER_FUNCTION: Moves capital and risk-bearing capacity from ratepayers, taxpayers, and climate-vulnerable communities to baseload technology providers and grid reliability reserves, in exchange for maintained operable capacity across multiple fuel types and continued decision flexibility.
% ABSENT_VOICES: Rapid decarbonization advocates who would prioritize immediate fossil fuel retirement, and pure expected-value optimizers who would allocate all capital to the currently cheapest technology, are structurally sidelined by technology-neutrality mandates and portfolio requirements that enforce multi-pathway viability.
% DISAPPEARANCE_RATIONALE: If the portfolio mandate vanished, capital would rapidly reallocate toward current cost-optimized or precautionary-extreme technologies, baseload providers would face accelerated retirement, grid planning would abandon redundant capacity, and the intellectual framework of robustness-based planning would lose institutional traction.
% FOUNDING_PROBLEM: Energy planning in the late twentieth and early twenty-first centuries faced deep uncertainty about future technology costs, climate impacts, and geopolitical energy security, creating a collective risk of premature lock-in to suboptimal or fragile single-technology dependence.
% FOUNDING_PROBLEM_CORROBORATION: Independent decision-analysts and robustness researchers attest the deep-uncertainty problem is still live for long-horizon infrastructure; climate scientists and renewable-energy economists attest the uncertainty has narrowed sufficiently to justify winner-picking. Corroboration is split across disciplinary boundaries outside the beneficiary set.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because maintaining redundant capacity imposes real costs on payers while delivering genuine option value against uncertainty. Suppression is moderate (0.55) because the framework must actively resist both winner-take-all optimization and precautionary shutdown; its persistence depends on institutional enforcement rather than participant consensus. Theater ratio rises over the interval (0.15 to 0.35) because the economic case for redundancy weakens as renewable costs fall and climate urgency grows, causing an increasing share of portfolio maintenance to serve incumbent protection rather than robustness. Resistance is substantial (0.60) because both extremes contest the middle path. The measurement series share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   Payer seatsâratepayers and climate-vulnerable communitiesâexperience the constraint as extractive inertia that forces them to fund competitors' survival and extends fossil exposure. Beneficiary seatsâbaseload providersâexperience it as necessary robustness investment that prevents premature destruction of valuable assets. The agenda setter sits closer to symmetric but leans beneficiary because its institutional legitimacy is coupled to the framework's survival. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Baseload technology providers are the structural beneficiaries: they receive capacity payments and avoided retirement, giving them low directionality toward the beneficiary pole. Ratepayers and taxpayers bear the direct cost of redundant capacity, giving them high directionality toward the target pole. Climate-vulnerable communities are trapped targets with the highest effective extraction because they cannot exit the climate system and suffer amplified harms from kept-open fossil pathways. The robustness analyst seat is analytical with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination functionâavoiding technology lock-in under genuine uncertaintyâis real and historically justified. Without the victim declaration and the rising theater ratio, the constraint would present as a Rope. However, the presence of identifiable payers funding redundant capacity, combined with increasing performative maintenance as the uncertainty premise weakens, establishes the hybrid Tangled Rope classification. The framework prevents mislabeling by requiring both the coordination story and the asymmetric extraction to be present and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deep_uncertainty_resolution,
    'Has the deep uncertainty that justified maintaining multiple energy pathways actually resolved for key technologies, or merely shifted to different variables?',
    'Comparative technology-cost forecasting and climate-sensitivity meta-analysis: if probability distributions have tightened sufficiently that option value no longer dominates expected value, the coordination rationale weakens.',
    'If uncertainty has resolved, the constraint''s extraction component likely dominates its coordination component, warranting reclassification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_resolution, empirical, 'Whether the empirical premise of deep uncertainty remains valid').

omega_variable(
    incumbent_capture_vs_robustness,
    'Is the portfolio mandate genuinely preserving socially valuable options, or has it been captured by baseload incumbents to extract rents from captive ratepayers?',
    'Disaggregate capacity-payment flows to marginal plants versus genuinely uncertain technology classes; compare maintained capacity to robust decision-model recommendations.',
    'If capture dominates, directionality for baseload providers shifts further toward beneficiary and the constraint''s coordination story becomes cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_vs_robustness, conceptual, 'Coordination function versus incumbent capture ambiguity').

omega_variable(
    kernel_reading_instability,
    'Does the option_value_preserving reading depend on institutional suppression of its siblings, or can it coexist without active enforcement against them?',
    'Observe jurisdictions where expected-value or catastrophic-tail readings dominate: does option-value planning collapse organically, or does it persist as a minority position?',
    'If it collapses without enforcement, the reading is a scaffold; if it persists only through suppression, the constraint is more extractive than coordinated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_instability, conceptual, 'Reading stability and enforcement dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acce_tr_t4, acceptable_risk_energy__option_value_preserving, theater_ratio, 4, 0.18).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__option_value_preserving, theater_ratio, 8, 0.22).
narrative_ontology:measurement(acce_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.25).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__option_value_preserving, theater_ratio, 16, 0.28).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.32).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(acce_be_t4, acceptable_risk_energy__option_value_preserving, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__option_value_preserving, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(acce_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__option_value_preserving, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(acce_su_t4, acceptable_risk_energy__option_value_preserving, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__option_value_preserving, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(acce_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__option_value_preserving, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, expected_value_dominant).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel decomposes into three structurally distinct constraints (readings) because each reading assigns a different referent to acceptable risk, a different beneficiary/victim structure, and a different epsilon. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
