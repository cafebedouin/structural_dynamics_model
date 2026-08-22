% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Opportunity Cost Constraint on Nuclear Deployment in Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The 'opportunity cost reading' of the climate mitigation imperative holds
 *   that mitigation must be optimized for carbon abated per dollar per year —
 *   a speed metric that structurally disadvantages nuclear energy due to its
 *   high capital intensity and long deployment timeline. This reading gained
 *   dominance in climate finance after Paris (2015) as a coordination device
 *   to align fragmented capital pools. Over 2015-2035, the metric hardened
 *   from a heuristic into an eligibility criterion in green taxonomies,
 *   development bank lending, and grid interconnection rules. The constraint
 *   extracts from nuclear actors (industry, labour, committed jurisdictions)
 *   to benefit renewable and storage deployment. It is a tangled rope: it
 *   solves a genuine coordination problem (comparable metric for climate
 *   finance) while asymmetrically extracting from a specific technology
 *   class. Active enforcement is required — the metric is maintained by
 *   active exclusion of nuclear from green finance frameworks, not by market
 *   preference alone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.72).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.68).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Opportunity Cost Constraint on Nuclear Deployment in Climate Mitigation").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '12694169-3d70-45d1-9e86-1fb93755d5c2').
narrative_ontology:cs_kernel_codification('12694169-3d70-45d1-9e86-1fb93755d5c2', formalized).
narrative_ontology:cs_authority_grounding('12694169-3d70-45d1-9e86-1fb93755d5c2', extraction).
narrative_ontology:cs_interpretation_layer_present('12694169-3d70-45d1-9e86-1fb93755d5c2').
narrative_ontology:cs_reading_relation('12694169-3d70-45d1-9e86-1fb93755d5c2', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('12694169-3d70-45d1-9e86-1fb93755d5c2', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('12694169-3d70-45d1-9e86-1fb93755d5c2', foundational, deployment_speed_maximization_as_primary_metric).
narrative_ontology:cs_axiom_status(deployment_speed_maximization_as_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('12694169-3d70-45d1-9e86-1fb93755d5c2', deployment_speed_maximization_as_primary_metric, instrumental).
narrative_ontology:cs_axiom('12694169-3d70-45d1-9e86-1fb93755d5c2', foundational, capital_intensity_penalty_in_mitigation_ranking).
narrative_ontology:cs_axiom_status(capital_intensity_penalty_in_mitigation_ranking, holdable).
narrative_ontology:cs_axiom_grounding('12694169-3d70-45d1-9e86-1fb93755d5c2', capital_intensity_penalty_in_mitigation_ranking, empirically_contingent).
narrative_ontology:cs_reference_frame('12694169-3d70-45d1-9e86-1fb93755d5c2', post_paris_finance_fragmentation).
narrative_ontology:cs_drift_state('12694169-3d70-45d1-9e86-1fb93755d5c2', post_2030_integration_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12694169-3d70-45d1-9e86-1fb93755d5c2', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_storage_firms).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, distributed_energy_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_allocators_favouring_speed).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_labour_unions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, jurisdictions_committed_to_nuclear_new_build).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive accelerated capital allocation and policy support when mitigation frameworks prioritize deployment speed per dollar. Their technologies (solar, wind, batteries) have shorter lead times and lower capital intensity than nuclear, so they capture the investment flow diverted from nuclear new-build. They can pivot across jurisdictions and project scales relatively freely.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Provide the firming capacity that makes high-renewable grids viable, directly benefiting from the 'speed per dollar' metric that disadvantages nuclear. They shape grid-integration standards and market rules that reinforce the speed metric. Their exit is mobile — they deploy where policy and capital align.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_storage_firms, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, grid_storage_firms, agenda_setter).

% Gain access to capital and regulatory space for community-owned solar, wind, and storage when mitigation funding follows the fastest-deployment rule. Their exit is constrained by local grid interconnection rules and municipal governance, but they are net beneficiaries of the constraint's capital allocation logic.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, distributed_energy_communities, beneficiary,
    moderate, biographical, constrained, local).

% Control multilateral development banks, green bond frameworks, and national climate finance facilities that explicitly rank projects by carbon abated per dollar per year. They set the metric that makes nuclear structurally uncompetitive. Their exit is arbitrage-grade — they can reallocate across technologies and geographies at will.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_allocators_favouring_speed, agenda_setter,
    institutional, generational, arbitrage, global).

% Lose access to climate finance and policy support when the 'fastest deployment per dollar' metric is applied. Their projects have 10-15 year lead times and high upfront capital, so they score poorly on the metric even though lifetime carbon is low. They bear the cost of stranded development pipelines and workforce attrition. Exit is constrained by regulatory licensing, supply chain lock-in, and the sunk cost of specialized workforce.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer).

% Represent workers whose specialized skills (nuclear construction, operation, regulation) are not transferable to renewable deployment at scale. When nuclear pipelines shrink, these workers face career dislocation, not simple retraining. Their identity is fused to the nuclear enterprise — professional culture, safety culture, community. Exit is identity-locked: leaving means abandoning a professional self-concept.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_labour_unions, payer,
    organized, biographical, identity_locked, national).

% Argue that grid reliability requires firm dispatchable capacity that renewables+storage cannot yet deliver at system scale in all climates. They bear the risk of reliability failures if the speed metric drives premature nuclear退出. Their exit is constrained — they can advocate for technology-neutral metrics but face institutional momentum toward the speed metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates, payer,
    moderate, biographical, constrained, regional).

% Countries (e.g., France, China, UAE, Poland) with active nuclear new-build programs tied to national decarbonization strategies. They face higher financing costs and diplomatic pressure when climate finance frameworks adopt the speed metric. Exit is trapped — reversing nuclear commitments implies stranded assets, energy security gaps, and geopolitical realignment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, jurisdictions_committed_to_nuclear_new_build, payer,
    institutional, generational, trapped, national).

% Model whole-system costs over multi-decadal horizons. They see the speed metric as myopic: it optimizes for early deployment but may increase total system cost if firm capacity gaps require expensive overbuild or fossil fallback. They do not collect or pay; they analyze.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, integrated_resource_planners, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate finance toward the mitigation pathway that delivers the most carbon abatement per dollar per year, using a single comparable metric to align disparate capital pools (development banks, green bonds, national budgets) around a common ranking.
% TRANSFER_FUNCTION: Moves capital and policy support from nuclear new-build (high capital intensity, long lead time) to renewable generation and storage (low capital intensity, short lead time). The transfer is enacted through finance eligibility criteria, grid interconnection queues, and permitting rules that operationalize the carbon-per-dollar-per-year metric.
% ABSENT_VOICES: Future generations who bear climate damages if the speed metric proves myopic (e.g., if firm capacity gaps extend fossil use). Also absent: nuclear-adjacent communities in the Global South that might have pursued nuclear for development + decarbonization but are excluded from climate finance by the metric. They are not in the room where the metric is designed.
% DISAPPEARANCE_RATIONALE: If the speed-per-dollar constraint vanished overnight, climate finance would revert to technology-neutral or portfolio-based allocation. Nuclear new-build pipelines would regain access to green bonds and development bank funding. Grid planning would shift from 'maximum renewables + storage' to 'reliability-constrained least cost'. The global mitigation portfolio would reorganize around a different optimization target.
% FOUNDING_PROBLEM: Post-Paris (2015) mitigation finance was fragmented and uncomparable: every fund used its own criteria, leading to capital misallocation and slow aggregate deployment. The 'carbon per dollar per year' metric was proposed as a universal comparator to accelerate the rate of decarbonization.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (IEA, GCF, major green bond verifiers) attest the metric accelerated renewable deployment and created a common language for climate finance. Critics (NEA, IAEA, several national energy ministries) attest the metric systematically discriminates against capital-intensive firm capacity, creating a structural blind spot for grid reliability. Independent systems analysts (MIT Energy Initiative, NREL) confirm the metric's myopia for long-horizon system optimization but note it served a real coordination need in 2015-2020.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) is high because the constraint diverts a large and growing share of global climate finance away from nuclear toward renewables/storage based on a metric that nuclear structurally cannot satisfy. Suppression (0.68) is substantial because the constraint's persistence depends on active policy choices: nuclear exclusion from EU taxonomy (until 2022 partial reversal), World Bank lending bans, green bond framework eligibility rules. Theater ratio (0.42) is moderate — the speed metric genuinely accelerated early renewable deployment (real coordination), but an increasing share of enforcement activity now serves to defend the metric against portfolio-optimization critiques rather than to accelerate deployment per se. Accessibility collapse (0.58) reflects that alternative metrics (system LCOE, reliability-adjusted cost) exist but are marginalized in finance governance. Resistance (0.55) is significant from nuclear states, IAEA/NEA, and labour unions, but has not reversed the metric's dominance in finance.
 *
 * PERSPECTIVAL GAP:
 *   From the finance allocator seat, the constraint is pure coordination: a necessary metric to align capital. From the nuclear labour seat, it is extraction enforced by a metric that ignores their identity-locked exit. From the jurisdiction seat, it is a sovereignty constraint imposed by external finance governance. The engine computes these divergent seat types from the structural data; the claimed_type (tangled_rope) reflects the author's assessment that both coordination and extraction are real and irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate finance allocators (agenda_setter, institutional, arbitrage exit) are structural beneficiaries — they control the metric and capture the coordination rents of being the standard-setters. Renewable developers and storage firms (organized/powerful, mobile exit) are beneficiaries — they receive the capital flow. Distributed energy communities (moderate, constrained exit) benefit but with less agency. Nuclear industry (institutional, constrained exit) and nuclear labour (organized, identity_locked exit) are victims — they bear the capital diversion and career dislocation. Jurisdictions committed to nuclear (institutional, trapped exit) are victims — they face stranded assets and financing penalties. Baseload advocates (moderate, constrained exit) bear reliability risk. Integrated resource planners (analytical) observe the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, slow climate finance) was real in 2015. The metric solved it. By 2035, the problem has mutated: deployment speed is no longer the binding constraint (renewables are now the cheapest marginal addition in most markets); the binding constraint is system integration and firm capacity. The constraint persists because the metric has become the basis for institutional authority (finance allocators' legitimacy) and capital allocation infrastructure (green bond frameworks, taxonomy rules). Mandatrophy is unresolved — the constraint's mandate has outlived its function but the arrangement persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speed_metric_myopia,
    'Does the carbon-per-dollar-per-year metric systematically undervalue firm capacity and long-duration storage, creating a structural blind spot that increases total system cost over multi-decadal horizons?',
    'Counterfactual system modeling: compare least-cost portfolios under speed-metric constraint vs. technology-neutral reliability-constrained optimization across multiple climate scenarios and grid topologies.',
    'If the metric increases total system cost by >15% in high-renewable-penetration scenarios, the constraint''s coordination function is net-harmful at the system level — it would reclassify toward snare. If the gap is <5%, the coordination benefit outweighs the myopia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_metric_myopia, empirical, 'Whether the speed metric''s myopia for firm capacity creates net system harm.').

omega_variable(
    nuclear_victim_status_ambiguity,
    'Is nuclear''s victim status under this constraint structural (the metric inherently penalizes capital intensity) or contingent (nuclear''s cost and timeline are solvable with modular designs, regulatory reform, and supply chain learning)?',
    'Track SMR/advanced nuclear deployment timelines and costs in jurisdictions that maintain technology-neutral finance (e.g., UAE, China, US DOE loan programs). If SMRs achieve <5 year deployment and <$5000/kW, the victim status is contingent.',
    'If contingent, the constraint is a transitional tangled_rope that resolves as nuclear adapts. If structural, nuclear remains a permanent victim class under this metric — the constraint is a stable tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_victim_status_ambiguity, empirical, 'Whether nuclear''s disadvantage under the speed metric is inherent or solvable.').

omega_variable(
    coordination_extraction_boundary,
    'At what point does the enforcement of the speed metric (exclusion of nuclear from green finance, taxonomy, procurement) exceed the coordination benefit it provides?',
    'Measure the marginal coordination benefit (additional renewable deployment enabled by the metric) vs. marginal extraction cost (nuclear capacity foregone, reliability risk increased) annually. The crossover point is where the constraint shifts from net-coordination to net-extraction.',
    'Identifies the temporal boundary for mandatrophy resolution. If crossover occurred pre-2025, the constraint has been a snare for a decade. If post-2030, it remains a tangled_rope with unresolved mandatrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The temporal boundary where coordination becomes extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''climate mitigation imperative'' kernel admit a single authoritative reading, or is the contest between opportunity_cost, portfolio_optimization, and systems_transition readings structurally irreducible?',
    'Analyze whether any reading forecloses another within a single governance framework (e.g., can a jurisdiction simultaneously optimize for speed-per-dollar AND require portfolio diversity for reliability?). Map the logical compatibility of the three readings'' core axioms.',
    'If readings are logically compatible (coexist_with), the kernel is a genuine multi-reading commitment system. If one forecloses another, the kernel is a site of structural conflict where only one reading can be institutionalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s readings are structurally compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2015, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2018, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2021, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2024, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2027, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2027, 0.4).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2030, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2035, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2015, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2018, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2021, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2024, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2027, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2027, 0.68).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2030, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2030, 0.7).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2035, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2035, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2015, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2018, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2021, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2021, 0.48).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2024, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2027, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2027, 0.63).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2030, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2030, 0.66).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2035, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2035, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, green_taxonomy_nuclear_exclusion).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, development_bank_energy_lending_rules).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, grid_interconnection_queue_priority_rules).

% DUAL FORMULATION NOTE:
% This constraint (opportunity_cost_reading) and its siblings (portfolio_optimization_reading, systems_transition_reading) form a constraint family decomposing the 'climate_mitigation_imperative' kernel. Each reading has a distinct ε: opportunity_cost (0.72, high extraction from nuclear), portfolio_optimization (lower ε, nuclear as beneficiary), systems_transition (moderate ε, nuclear and centralized renewables as victims). The ε-invariance principle requires separate stories because the metric (carbon-per-dollar-per-year vs. system-reliability-cost vs. decentralization-index) changes the extraction profile fundamentally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
