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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation via All Low-Carbon Sources (Portfolio Optimization Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio optimization' reading of the
 *   climate mitigation imperative, asserting that maximizing all low-carbon
 *   energy sources, including nuclear for reliable baseload, is necessary to
 *   achieve climate goals. It frames climate action as a technology-neutral
 *   effort to reduce carbon intensity. The constraint is claimed as a
 *   'tangled_rope' because it coordinates a broad energy transition but
 *   involves significant extraction from fossil fuel industries and
 *   taxpayers, and suppresses alternative approaches (e.g., renewables-only).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.6).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation via All Low-Carbon Sources (Portfolio Optimization Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'ca2fd9ef-f31f-469c-953c-7a2094ce51a2').
narrative_ontology:cs_kernel_codification('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', formalized).
narrative_ontology:cs_authority_grounding('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', expertise).
narrative_ontology:cs_interpretation_layer_present('ca2fd9ef-f31f-469c-953c-7a2094ce51a2').
narrative_ontology:cs_reading_relation('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', foundational, carbon_intensity_is_primary_metric).
narrative_ontology:cs_axiom_status(carbon_intensity_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', carbon_intensity_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', foundational, baseload_reliability_is_non_negotiable).
narrative_ontology:cs_axiom_status(baseload_reliability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', baseload_reliability_is_non_negotiable, empirically_contingent).
narrative_ontology:cs_reference_frame('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', technology_neutral_decarbonization).
narrative_ontology:cs_drift_state('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', contemporary_energy_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca2fd9ef-f31f-469c-953c-7a2094ce51a2', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, anti_nuclear_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, taxpayers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, climate_science_consensus).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, energy_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for setting climate policy, allocating subsidies, and regulating energy markets to achieve decarbonization goals. They balance economic, environmental, and energy security concerns, often facing political pressure from various industry and advocacy groups.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, governments, agenda_setter,
    institutional, biographical, constrained, national).

% Receives policy support, subsidies, and regulatory pathways for new plant construction and existing plant operation, justified by its low-carbon baseload generation capacity. Faces high capital costs and public perception challenges.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Benefits from policies promoting low-carbon energy, including subsidies and market mechanisms that favor renewable deployment. Contributes to decarbonization but faces challenges with intermittency and grid integration.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Faces increasing regulatory burdens, carbon pricing, and eventual phase-out policies due to their high carbon emissions. They resist these measures, advocating for continued use of fossil fuels, often citing energy security or economic impact.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industries, payer,
    institutional, biographical, constrained, global).

% Supports policies that aggressively reduce carbon emissions, including the deployment of all low-carbon sources. They push for stronger climate action and hold governments accountable to their commitments.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates, beneficiary,
    organized, generational, analytical, global).

% Opposes nuclear power due to concerns about safety, waste disposal, proliferation, and cost. They advocate for a purely renewable energy transition and face suppression of their arguments within the 'all low-carbon sources' narrative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, anti_nuclear_activists, payer,
    moderate, biographical, constrained, national).

% Bear the financial costs of energy transition policies, including subsidies for nuclear and renewables, through taxes or higher energy prices. Their influence is diffuse, and individual exit options are limited.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, taxpayers, payer,
    powerless, immediate, trapped, national).

% Are the ultimate beneficiaries of successful climate mitigation, inheriting a more stable climate and environment. They have no direct agency but are often invoked in policy justifications.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_mitigation_imperative__portfolio_optimization_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly decarbonize the energy sector and ensure energy security by deploying a comprehensive portfolio of all available low-carbon technologies, including nuclear, to meet urgent climate targets.
% TRANSFER_FUNCTION: Transfers investment, subsidies, and regulatory support from public funds and fossil fuel industries to nuclear and renewable energy sectors, while imposing costs and phase-out requirements on high-carbon energy production.
% ABSENT_VOICES: Advocates for a purely renewable energy transition (without nuclear) or those prioritizing immediate cost-effectiveness over long-term baseload reliability often find their concerns marginalized or dismissed within the 'all low-carbon sources' framework.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, energy policy would likely revert to prioritizing the cheapest available sources (often fossil fuels), or a slower, less comprehensive transition, leading to significantly higher greenhouse gas emissions and greater climate risk. The global energy landscape and climate trajectory would fundamentally reorganize.
% FOUNDING_PROBLEM: The urgent need to mitigate anthropogenic climate change by drastically reducing greenhouse gas emissions while simultaneously ensuring a reliable and secure energy supply for growing global demand.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change, as articulated by the IPCC, national science academies, and international energy agencies, consistently corroborates the urgency of climate change and the necessity of comprehensive decarbonization efforts. This is attested by a broad range of independent scientific and policy bodies outside the direct beneficiaries of specific energy technologies.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65 at end) reflects the costs imposed on fossil fuel industries and the public subsidies for low-carbon technologies. Suppression (0.75 at end) is high due to active policy measures to phase out high-carbon sources and to counter arguments against nuclear power. The theater ratio is low (0.23) as the primary goal of climate mitigation is genuine, though some rhetoric may downplay costs or risks. The increasing trend in extractiveness and suppression over the interval reflects the growing urgency of climate action and the hardening of policies against high-carbon sources.
 *
 * PERSPECTIVAL GAP:
 *   Governments and the low-carbon industries (nuclear, renewables) perceive this as a necessary coordination mechanism for planetary survival and economic opportunity. Fossil fuel industries and anti-nuclear activists experience it as an extractive and suppressive force, threatening their existence or values. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Governments are agenda-setters, balancing various interests. Nuclear and renewable industries are beneficiaries, receiving support. Fossil fuel industries and anti-nuclear activists are targets/payers, bearing costs and facing suppression. Taxpayers are diffuse payers. Future generations are ultimate beneficiaries, but without direct agency. The 'all sources' approach aims to distribute benefits across a broader set of low-carbon technologies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_necessity_empirical,
    'Is nuclear power truly necessary for reliable baseload in a fully decarbonized grid, or can advanced renewable energy systems with storage achieve comparable reliability and cost-effectiveness?',
    'Long-term empirical data from large-scale grid deployments in diverse geographies, coupled with techno-economic modeling of future energy systems incorporating advanced storage and demand-side management.',
    'If nuclear is found not to be strictly necessary, the justification for its inclusion in the ''all sources'' portfolio weakens, potentially shifting resources towards renewables and storage, and altering the constraint''s beneficiary structure and extractiveness from taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_necessity_empirical, empirical, 'Empirical necessity of nuclear for baseload reliability in future grids.').

omega_variable(
    subsidy_vs_extraction_boundary,
    'Are the subsidies and policy supports for nuclear and other low-carbon sources primarily necessary coordination costs for a public good (climate mitigation), or do they constitute extractive transfers to specific industries?',
    'Detailed cost-benefit analysis comparing public investment to the social and environmental returns, and benchmarking against alternative investment strategies. Regulatory review of industry profit margins and market power.',
    'If found to be primarily extractive, the constraint''s effective extraction (χ) from taxpayers would be higher, and its classification might shift closer to a Snare for the public seat, even if the overall climate goal remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_vs_extraction_boundary, conceptual, 'Distinguishing coordination costs from extractive transfers in low-carbon energy subsidies.').

omega_variable(
    technology_neutrality_vs_bias,
    'Is the ''technology-neutral'' framing of this portfolio approach genuinely neutral, or does it implicitly favor certain established technologies (e.g., nuclear) over emerging or decentralized alternatives?',
    'Analysis of policy implementation details, funding allocations, and regulatory hurdles across different low-carbon technologies. Examination of lobbying efforts and political influence of incumbent energy sectors.',
    'If a bias is detected, the ''portfolio optimization'' claim might be seen as a cover story for maintaining existing industrial structures, increasing the constraint''s theater_ratio and potentially shifting its classification towards a Tangled Rope or Snare for disfavored alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_neutrality_vs_bias, conceptual, 'Assessing the true neutrality of the ''all low-carbon sources'' policy framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(clim_tr_t2005, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2005, 0.17).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2030, 0.23).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(clim_be_t2005, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2005, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_phaseout_policies).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
