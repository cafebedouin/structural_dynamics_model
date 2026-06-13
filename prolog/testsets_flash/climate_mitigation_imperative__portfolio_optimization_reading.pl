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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative: Portfolio Optimization Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio optimization' reading of the
 *   climate mitigation imperative, which asserts that all low-carbon energy
 *   sources, including nuclear, must be maximized to achieve decarbonization
 *   goals while ensuring grid reliability. It frames nuclear power as a
 *   necessary component for baseload stability, distinguishing it from other
 *   readings that prioritize cost-effectiveness or decentralized systems. The
 *   constraint actively supports nuclear development and penalizes fossil
 *   fuels.
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
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative: Portfolio Optimization Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '67fc8655-5208-4f0f-b129-ec32e233a9f0').
narrative_ontology:cs_kernel_codification('67fc8655-5208-4f0f-b129-ec32e233a9f0', formalized).
narrative_ontology:cs_authority_grounding('67fc8655-5208-4f0f-b129-ec32e233a9f0', expertise).
narrative_ontology:cs_interpretation_layer_present('67fc8655-5208-4f0f-b129-ec32e233a9f0').
narrative_ontology:cs_reading_relation('67fc8655-5208-4f0f-b129-ec32e233a9f0', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('67fc8655-5208-4f0f-b129-ec32e233a9f0', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('67fc8655-5208-4f0f-b129-ec32e233a9f0', foundational, all_low_carbon_sources_necessary).
narrative_ontology:cs_axiom_status(all_low_carbon_sources_necessary, holdable).
narrative_ontology:cs_axiom_grounding('67fc8655-5208-4f0f-b129-ec32e233a9f0', all_low_carbon_sources_necessary, empirically_contingent).
narrative_ontology:cs_axiom('67fc8655-5208-4f0f-b129-ec32e233a9f0', foundational, nuclear_provides_essential_baseload).
narrative_ontology:cs_axiom_status(nuclear_provides_essential_baseload, holdable).
narrative_ontology:cs_axiom_grounding('67fc8655-5208-4f0f-b129-ec32e233a9f0', nuclear_provides_essential_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('67fc8655-5208-4f0f-b129-ec32e233a9f0', diversified_decarbonization_portfolio).
narrative_ontology:cs_drift_state('67fc8655-5208-4f0f-b129-ec32e233a9f0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67fc8655-5208-4f0f-b129-ec32e233a9f0', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_developers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, energy_security_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_reliability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives policy support, subsidies, and regulatory pathways for new plant construction and existing plant life extension, justified by its low-carbon baseload capacity. Benefits from the framing that all low-carbon sources must be maximized.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Faces increasing regulatory pressure, carbon pricing, and divestment campaigns as the imperative to maximize low-carbon sources reduces its market share and political legitimacy. Its long-term viability is threatened by this constraint.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry, payer,
    institutional, generational, constrained, global).

% Benefits from the overall push for low-carbon energy, but competes with nuclear for investment and grid integration priority. This reading supports their growth but does not prioritize them over other low-carbon sources.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Provide the scientific basis for the climate mitigation imperative, emphasizing the urgency of decarbonization. Their role is to assess the efficacy of various energy portfolios in meeting climate targets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% Benefit from the stability and reliability that nuclear baseload provides to the electrical grid, especially as intermittent renewables grow. They advocate for a diverse energy portfolio to maintain grid stability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, generational, constrained, national).

% Argue against nuclear power due to concerns about waste, safety, and proliferation, advocating for a purely renewable energy future. Their concerns are often sidelined in discussions focused on maximizing all low-carbon sources for baseload reliability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, environmental_advocates_anti_nuclear, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and international energy policy towards a common goal of decarbonization by establishing a framework for evaluating and deploying diverse low-carbon energy technologies, ensuring grid stability during the transition.
% TRANSFER_FUNCTION: Directs public and private investment, regulatory support, and political capital towards nuclear power and other low-carbon sources, while imposing costs (e.g., carbon taxes, regulatory hurdles) on the fossil fuel industry.
% ABSENT_VOICES: Environmental advocates who prioritize rapid, decentralized renewable deployment and oppose nuclear power are often marginalized in policy discussions dominated by the 'all-of-the-above' low-carbon portfolio approach. Their concerns about nuclear waste and safety are acknowledged but not given decisive weight in this framing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the policy landscape for energy would fragment. Investment in nuclear power would likely decline without the 'baseload necessity' argument, and the urgency for decarbonization might lose its unified strategic direction, leading to a less coordinated and potentially slower energy transition.
% FOUNDING_PROBLEM: The urgent need to mitigate climate change by rapidly decarbonizing global energy systems while maintaining energy security and grid reliability.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, international energy agencies, and most national governments corroborate the live status of the climate mitigation problem. Grid operators and energy security experts corroborate the need for reliable baseload power during the transition, supporting the inclusion of nuclear.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).

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
 *   The extractiveness (0.45) reflects the costs imposed on fossil fuels and the subsidies/support directed to nuclear, which are substantial but framed as necessary for climate goals. Suppression (0.30) is moderate, as it involves regulatory hurdles for fossil fuels and active promotion of nuclear, but does not fully eliminate alternatives or dissent. Theater ratio (0.10) is low, as the policy genuinely aims for decarbonization, with minimal performative elements. The increasing trend in extractiveness and suppression reflects the growing urgency of climate action and the hardening of policy against high-carbon sources.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nuclear industry and grid operators, this constraint is a necessary Rope, coordinating a complex energy transition. From the fossil fuel industry's perspective, it is a Snare, actively extracting their market share. From anti-nuclear environmentalists, it is a Tangled Rope, coordinating decarbonization but with unacceptable risks. The engine will compute these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and grid operators are clear beneficiaries (d near 0.0) due to direct support and the validation of their operational models. The fossil fuel industry is the primary target (d near 1.0) due to regulatory and economic pressures. Renewable developers are also beneficiaries, but their position is less central to this specific reading's unique claims. Environmental advocates (anti-nuclear) are excluded, as their concerns are not central to this 'maximize all low-carbon' framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_cost_effectiveness,
    'Is nuclear power truly cost-effective and timely enough to contribute meaningfully to climate mitigation goals compared to alternative low-carbon sources?',
    'Comparative lifecycle cost analysis, including construction time, operational costs, waste disposal, and decommissioning, against renewables plus storage over a 20-year horizon.',
    'If nuclear is found to be significantly less cost-effective or slower to deploy, the ''necessity'' claim of this reading would weaken, potentially shifting policy towards other low-carbon sources and reducing the constraint''s support for nuclear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_cost_effectiveness, empirical, 'Empirical debate over nuclear''s economic and deployment viability for climate mitigation.').

omega_variable(
    baseload_necessity_reassessment,
    'Is the concept of ''baseload'' itself becoming obsolete with advancements in grid storage, demand-side management, and distributed energy resources?',
    'Technological and economic modeling of future grid architectures with high renewable penetration and advanced storage solutions, assessing their ability to provide continuous, reliable power without traditional baseload.',
    'If baseload is found to be largely replaceable, the core justification for nuclear''s necessity in this reading would erode, potentially reclassifying the constraint''s support for nuclear as a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_reassessment, empirical, 'Conceptual shift in grid management challenging the traditional baseload requirement.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''portfolio optimization'' framing a genuine technical necessity for climate mitigation, or does it serve to legitimize specific industrial interests (e.g., nuclear) under the guise of climate urgency?',
    'Analysis of policy advocacy by different industry groups, funding flows, and the historical evolution of energy policy narratives, particularly in response to scientific consensus on climate change.',
    'If primarily driven by industrial interests, the constraint''s ''claimed_type'' as a Rope would be challenged, potentially reclassifying it as a Tangled Rope or Snare from an analytical perspective, highlighting the extractive component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Ambiguity between technical necessity and interest-group capture in the ''all-of-the-above'' climate strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2030, 0.12).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2040, 0.13).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2050, 0.15).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2030, 0.45).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2040, 0.48).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2050, 0.5).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2030, 0.35).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2040, 0.38).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2050, 0.4).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
