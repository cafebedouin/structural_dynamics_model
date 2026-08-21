% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative: Portfolio Optimization Reading (Nuclear Inclusion)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio optimization' reading of the
 *   climate mitigation imperative, which asserts that all low-carbon energy
 *   sources, including nuclear, are necessary to achieve climate goals and
 *   ensure grid reliability. It frames nuclear power as a vital component for
 *   baseload generation, complementing intermittent renewables. This reading
 *   is distinct from those that prioritize cost-effectiveness
 *   (opportunity_cost_reading) or systemic transformation
 *   (systems_transition_reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.45).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.3).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative: Portfolio Optimization Reading (Nuclear Inclusion)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'd1ea8402-6283-4051-99ae-e70f75dfa0cd').
narrative_ontology:cs_kernel_codification('d1ea8402-6283-4051-99ae-e70f75dfa0cd', formalized).
narrative_ontology:cs_authority_grounding('d1ea8402-6283-4051-99ae-e70f75dfa0cd', expertise).
narrative_ontology:cs_interpretation_layer_present('d1ea8402-6283-4051-99ae-e70f75dfa0cd').
narrative_ontology:cs_reading_relation('d1ea8402-6283-4051-99ae-e70f75dfa0cd', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1ea8402-6283-4051-99ae-e70f75dfa0cd', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('d1ea8402-6283-4051-99ae-e70f75dfa0cd', foundational, all_low_carbon_sources_necessary).
narrative_ontology:cs_axiom_status(all_low_carbon_sources_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d1ea8402-6283-4051-99ae-e70f75dfa0cd', all_low_carbon_sources_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d1ea8402-6283-4051-99ae-e70f75dfa0cd', foundational, nuclear_provides_reliable_baseload).
narrative_ontology:cs_axiom_status(nuclear_provides_reliable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('d1ea8402-6283-4051-99ae-e70f75dfa0cd', nuclear_provides_reliable_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('d1ea8402-6283-4051-99ae-e70f75dfa0cd', diversified_low_carbon_portfolio).
narrative_ontology:cs_drift_state('d1ea8402-6283-4051-99ae-e70f75dfa0cd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d1ea8402-6283-4051-99ae-e70f75dfa0cd', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives policy support, subsidies, and regulatory pathways for new plant construction and operation, framed as essential for baseload power and carbon reduction. Benefits from the 'all-of-the-above' low-carbon strategy.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Benefit from the stability and reliability nuclear power offers for baseload generation, reducing intermittency challenges associated with renewables. This reading supports their operational mandate for grid stability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, biographical, constrained, national).

% Advocate for any and all low-carbon technologies, including nuclear, to meet urgent climate targets. They see nuclear as a necessary component of a diversified low-carbon energy portfolio.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates, beneficiary,
    organized, civilizational, constrained, global).

% Faces increasing regulatory pressure, carbon pricing, and market displacement as low-carbon sources, including nuclear, are prioritized. This constraint directly targets their market share and operational viability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry, payer,
    institutional, biographical, constrained, global).

% Argue that nuclear power diverts resources and attention from faster, cheaper, and safer renewable energy deployment. They bear the cost of policy and financial support for nuclear that they believe could be better spent on renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates, payer,
    organized, generational, constrained, global).

% Responsible for crafting energy policy that balances climate goals, energy security, and economic viability. This reading provides a framework for justifying continued investment in nuclear alongside renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, policymakers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and global efforts to decarbonize energy systems by ensuring a stable, low-carbon baseload power supply, thereby reducing reliance on fossil fuels and stabilizing grids amidst increasing renewable penetration.
% TRANSFER_FUNCTION: Directs public funds, regulatory support, and political capital towards nuclear energy development and operation, shifting resources away from fossil fuels and, to some extent, from exclusive focus on renewables.
% ABSENT_VOICES: Communities disproportionately affected by nuclear waste storage or potential accidents, who would argue for alternative, less risky energy pathways. Their concerns are often marginalized in national security and climate urgency narratives.
% DISAPPEARANCE_RATIONALE: If this constraint (the imperative to include nuclear in the low-carbon portfolio) vanished, energy policy would rapidly shift away from nuclear, leading to significant divestment, plant closures, and a re-prioritization of renewable-only pathways. Grid stability strategies would need fundamental rethinking, and climate mitigation timelines might be re-evaluated based on a narrower technology set.
% FOUNDING_PROBLEM: The urgent need to mitigate climate change while maintaining energy security and grid reliability, especially given the intermittency of many renewable sources.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies (e.g., IEA, IPCC), national energy departments, and many scientific bodies corroborate the live status of the problem, emphasizing the need for diverse low-carbon sources to meet climate targets and ensure energy security. This corroboration comes from outside the direct beneficiaries of nuclear power.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).
:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs imposed on fossil fuel industries and the opportunity costs perceived by renewable-only advocates, as resources are directed towards nuclear. Suppression (0.30) is moderate, as this reading actively counters narratives that exclude nuclear, but does not fully suppress dissent. Theater ratio (0.10) is low, as the policy support for nuclear is genuinely aimed at climate mitigation and grid stability, not primarily for performative reasons. The metrics are stable over the interval, reflecting a consistent policy stance.
 *
 * PERSPECTIVAL GAP:
 *   Policymakers and grid operators experience this as a necessary, balanced approach to a complex problem, ensuring energy security while decarbonizing. Renewable-only advocates, however, experience it as an extractive diversion of resources and a perpetuation of centralized, risky energy systems. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry, grid operators, and climate mitigation advocates are beneficiaries, as this reading directly supports their interests or goals. The fossil fuel industry is a clear victim, facing displacement. Renewable-only advocates are also victims, as they perceive a diversion of resources from their preferred solutions. Policymakers act as agenda-setters, implementing policies aligned with this portfolio approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_cost_effectiveness,
    'Is nuclear power truly cost-effective and timely for climate mitigation compared to alternative low-carbon sources, considering full lifecycle costs and deployment speed?',
    'Comprehensive, independent economic analysis comparing levelized cost of electricity (LCOE), capital intensity, and construction timelines of nuclear vs. renewable-plus-storage systems, including grid integration costs.',
    'If nuclear is found to be significantly less cost-effective or slower to deploy, the ''necessity'' claim of this reading would be weakened, potentially shifting policy towards renewables-only and reclassifying the constraint as more extractive due to inefficient resource allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_cost_effectiveness, empirical, 'Uncertainty regarding the true economic and temporal efficiency of nuclear power in climate mitigation.').

omega_variable(
    grid_reliability_alternatives,
    'Are there viable, scalable, and cost-effective non-nuclear alternatives (e.g., advanced storage, demand-side management, grid modernization) to provide baseload reliability in a high-renewable grid?',
    'Pilot projects and large-scale deployments of advanced grid technologies and storage solutions, coupled with modeling of their performance in diverse grid conditions.',
    'If such alternatives prove robust, the ''necessity'' of nuclear for baseload would be challenged, potentially reducing the perceived coordination function of this constraint and increasing its perceived extractiveness from those advocating for alternative reliability solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_reliability_alternatives, empirical, 'Uncertainty about the availability and efficacy of non-nuclear baseload alternatives.').

omega_variable(
    centralization_vs_decentralization_preference,
    'Is the preference for large-scale, centralized energy infrastructure (like nuclear) a technical necessity for grid stability or a political/ideological preference for a particular energy system architecture?',
    'Analysis of energy policy decisions in different political systems and their outcomes, alongside public discourse analysis on energy system design preferences.',
    'If primarily a preference, the ''necessity'' claim of this reading would be reframed as a value choice, potentially increasing perceived extraction from those advocating for decentralized, democratically controlled energy systems (as per the ''systems_transition_reading'').',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centralization_vs_decentralization_preference, preference, 'Ambiguity between technical necessity and political preference for centralized energy systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
