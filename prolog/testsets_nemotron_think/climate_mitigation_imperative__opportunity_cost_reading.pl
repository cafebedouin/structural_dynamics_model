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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Opportunity Cost Imperative: Fastest Carbon Reduction Per Dollar Per Year
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The opportunity_cost_reading instantiates the
 *   climate_mitigation_imperative kernel as a constraint that prioritizes
 *   deployment speed per dollar above all other metrics. It argues that
 *   nuclear's high capital intensity (typically $6-9/W) and long lead times
 *   (10-15 years) mean each dollar spent on nuclear avoids less carbon per
 *   year than the same dollar spent on wind, solar, and storage. This reading
 *   treats the mitigation problem as a rate optimization: maximize
 *   d(CO2_avoided)/d($)/dt. Nuclear enters the victim set because capital
 *   allocated to it is diverted from faster alternatives; renewables are
 *   beneficiaries because they capture the redirected capital. The constraint
 *   is actively enforced through policy mechanisms: green taxonomies
 *   excluding nuclear, financing rules favoring short-lead-time projects, and
 *   procurement targets keyed to annual deployment rates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Opportunity Cost Imperative: Fastest Carbon Reduction Per Dollar Per Year").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '3c70317f-1e3b-4cf2-a7e8-33a2c1834595').
narrative_ontology:cs_kernel_codification('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', distributed).
narrative_ontology:cs_authority_grounding('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', expertise).
narrative_ontology:cs_interpretation_layer_present('3c70317f-1e3b-4cf2-a7e8-33a2c1834595').
narrative_ontology:cs_reading_relation('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', foundational, carbon_per_dollar_per_year_optimization).
narrative_ontology:cs_axiom_status(carbon_per_dollar_per_year_optimization, holdable).
narrative_ontology:cs_axiom_grounding('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', carbon_per_dollar_per_year_optimization, empirically_contingent).
narrative_ontology:cs_axiom('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', secondary, deployment_speed_trumps_firm_capacity_value).
narrative_ontology:cs_axiom_status(deployment_speed_trumps_firm_capacity_value, holdable).
narrative_ontology:cs_axiom_grounding('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', deployment_speed_trumps_firm_capacity_value, empirically_contingent).
narrative_ontology:cs_reference_frame('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', urgent_decarbonization_imperative).
narrative_ontology:cs_drift_state('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', contemporary_energy_modeling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c70317f-1e3b-4cf2-a7e8-33a2c1834595', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, near_term_emission_reducers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, ratepayer_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_host_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, near_term_emission_reducers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, marginal_abatement_rate_optimization).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, deployment_speed_as_primary_metric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the bulk of redirected mitigation capital: production tax credits, investment tax credits, green bond eligibility, streamlined permitting, and priority grid interconnection. Their business models align with the opportunity cost metric — short lead times, modular deployment, rapidly falling costs. Exit is mobile: they can deploy globally wherever policy favors the metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).

% Entities (utilities, corporates, municipalities) mandated to hit near-term emission targets. They benefit from the metric because it points to the fastest compliance path. They also pay indirectly when system costs rise from integration challenges (storage, transmission) that the metric does not fully capture. Exit is constrained by regulatory mandates.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, near_term_emission_reducers, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, near_term_emission_reducers, payer).

% Advocate for lowest near-term electricity rates. The opportunity cost metric favors technologies with lowest LCOE and shortest deployment, which correlates with near-term rate impacts. They benefit from avoided nuclear cost overruns but may pay for integration costs later. Exit is constrained — captive ratepayers cannot choose their utility's resource mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, ratepayer_advocates, beneficiary,
    moderate, immediate, constrained, national).

% Bears the extraction: higher cost of capital (green bond exclusion), longer permitting timelines, no production tax credit parity, and policy frameworks that treat nuclear as 'not additional' in clean energy standards. Their exit is constrained — massive sunk costs, site-specific licensing, and workforce specialization make pivoting difficult. They argue the metric ignores firm capacity value.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    powerful, biographical, constrained, national).

% Communities dependent on nuclear plants for tax base, high-wage employment, and local economy. When plants close or new builds are cancelled due to policy disadvantage, they lose revenue with no replacement. Exit is trapped — geographic immobility, specialized workforce, and loss of community identity. They are not consulted in the metric choice.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_host_communities, payer,
    moderate, biographical, trapped, local).

% Grid operators, reliability councils, and industrial consumers who value firm capacity. They pay when the metric drives portfolios toward variable renewables without adequate firm capacity, increasing blackout risk or requiring expensive overbuild/storage. Their exit is constrained — they must operate the grid that results from policy choices. They argue the metric is incomplete.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates, payer,
    organized, generational, constrained, national).

% Design and enforce the opportunity cost metric through green taxonomies, clean energy standards, financing rules, and procurement targets. They justify it as maximizing mitigation per public dollar. They face pressure from both renewable and nuclear constituencies. Their exit is analytical — they can change the metric but face political cost.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Produce the integrated assessment models (IAMs) and system optimization models that underpin the opportunity cost metric. They supply the analytical infrastructure but do not directly capture gains. Their exit is analytical — they can change modeling assumptions but face peer review and institutional inertia. Some now model firm capacity value, challenging the metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_modelers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs scarce mitigation capital to the technologies that avoid the most carbon per dollar per year, solving the collective action problem of maximizing near-term emission reduction rate across a fragmented global energy system.
% TRANSFER_FUNCTION: Moves capital (public subsidies, private finance, green bond proceeds, permitting priority, grid interconnection queue position) from nuclear projects (slow, capital-intensive) to wind, solar, and storage projects (fast, modular). The transfer is enacted through policy mechanisms that adopt carbon-per-dollar-per-year as the primary eligibility criterion.
% ABSENT_VOICES: Nuclear host communities (trapped, local) and future generations who may face reliability shortfalls from variable-heavy portfolios (not yet born, no voice). Also excluded: Global South nations where nuclear may be the only firm low-carbon option but lack capital for renewable+storage overbuild — they are not in the room when the metric is set by OECD policymakers and modelers.
% DISAPPEARANCE_RATIONALE: If the opportunity cost metric vanished overnight, green taxonomies would likely include nuclear, financing would equalize, and nuclear new-build pipelines would expand. Capital would flow to both nuclear and renewables based on broader criteria (reliability, land use, lifecycle impacts). The renewable industry would lose its policy preference; nuclear communities would regain economic viability. The mitigation portfolio would rearrange toward technology neutrality.
% FOUNDING_PROBLEM: Post-2010 climate policy faced a perceived urgency gap: emissions were rising, renewable costs were falling rapidly, and nuclear new builds were consistently over budget and behind schedule. The opportunity cost metric emerged as a decision rule to maximize near-term carbon reduction given finite political capital and public funds.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) includes nuclear in most illustrative mitigation pathways but notes its deployment is below model projections. IEA (2023) reports nuclear construction starts at 20-year lows while renewable deployment accelerates. Nuclear industry (World Nuclear Association) attests the problem is not urgency but policy bias. Independent systems analysts (e.g., MIT Energy Initiative 'Future of Nuclear' 2018) corroborate that firm capacity value is underweighted in pure LCOE/rate metrics. No single outside arbiter has settled the dispute.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects substantial capital diversion from nuclear to renewables — nuclear projects face higher cost of capital, exclusion from green bonds, and longer permitting; renewables receive production tax credits, streamlined permitting, and priority grid interconnection. Suppression (0.45) is moderate: nuclear is not banned but is structurally disadvantaged in policy frameworks that adopt the opportunity cost metric. Theater ratio (0.25) is low-moderate: the metric has genuine analytical grounding in integrated assessment models, but its elevation to primary policy criterion serves institutional interests (renewable industry, near-term political cycles). Accessibility collapse (0.48) reflects that nuclear remains technically available but economically non-viable under the metric. Resistance (0.52) captures nuclear industry lobbying, host community advocacy, and reliability-focused pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable industry / near-term emission reducer seat, the constraint is genuine coordination (rope-like): it solves the collective action problem of maximizing mitigation rate. From the nuclear industry / host community seat, it is extraction (snare-like): a metric chosen to disadvantage their technology. From the policymaker seat, it is a tangled rope: the coordination function (maximizing carbon reduction rate) is real, but the metric choice extracts from nuclear to benefit renewables. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable industry and ratepayer advocates are structural beneficiaries (d ≈ 0.1-0.2): they receive capital flows, policy preference, and lower near-term rates. Nuclear industry and host communities are structural targets (d ≈ 0.8-0.9): they bear higher financing costs, exclusion from green finance, and community economic loss. Baseload reliability advocates are partial targets (d ≈ 0.6): their technical argument is acknowledged but subordinated to the rate metric. Policymakers are agenda_setters (d ≈ 0.3): they design the metric but face political pressure from both sides. Energy modelers are observers (d ≈ 0.5): they supply the analytical infrastructure but do not directly capture gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgent decarbonization) is live and escalating. The opportunity_cost_reading claims the metric remains optimal; portfolio_optimization_reading claims the problem has shifted to reliability; systems_transition_reading claims the problem was never just carbon. The constraint shows rising extractiveness over 2010-2030 as renewable costs fall and the metric's advantage over nuclear widens — this is not mandatrophy (the function is not atrophied) but metric entrenchment: the original coordination metric becomes more extractive as the technology landscape shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is the opportunity_cost_reading a distinct constraint from its sibling readings of the climate_mitigation_imperative kernel, or a parameter variation within a single constraint?',
    'Apply the ε-invariance test: if measuring the constraint by carbon-per-dollar-per-year gives substantially different extractiveness/suppression than measuring by portfolio_reliability or system_decentralization, they are distinct constraints.',
    'If distinct, each reading gets its own constraint story with independent ε and classification; if not, the kernel is a single constraint with observer-relative metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Whether the three kernel readings are structurally distinct constraints per ε-invariance.').

omega_variable(
    optimization_metric_dispute,
    'Is marginal carbon abatement per dollar per year the correct optimization metric for mitigation policy, or does it exclude value of firm capacity, grid stability, and long-term optionality?',
    'Compare system-level integrated resource plans that internalize firm capacity value against levelized cost of energy + storage portfolios; observe whether opportunity_cost metric changes when full system costs are modeled.',
    'If firm capacity value materially changes the optimal portfolio, the opportunity_cost_reading''s metric is incomplete and its extractiveness on nuclear is partially artifactual; if not, the metric stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_metric_dispute, empirical, 'Whether the opportunity cost metric captures full system value or excludes nuclear''s reliability contribution.').

omega_variable(
    nuclear_victim_status,
    'Is nuclear''s exclusion from opportunity_cost-optimal portfolios extraction (victim) or efficient allocation (non-victim)?',
    'Track capital flows: if nuclear projects are denied financing available to renewables with equal or lower carbon-per-dollar-per-year, that is extraction; if nuclear simply cannot meet the metric on its own terms, it is efficient allocation.',
    'If extraction, the constraint is tangled_rope or snare with nuclear as victim; if efficient allocation, nuclear is not a victim and the constraint may be rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_victim_status, empirical, 'Whether nuclear''s position in the constraint is victim (extraction) or non-competitive (efficient outcome).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2010, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2015, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2020, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2025, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2025, 0.24).
narrative_ontology:measurement(climate_mitigation_opp_cost_tr_t2030, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2030, 0.25).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2010, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2015, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2020, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2025, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(climate_mitigation_opp_cost_be_t2030, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2010, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2015, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2020, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2025, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2025, 0.44).
narrative_ontology:measurement(climate_mitigation_opp_cost_su_t2030, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2030, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, green_finance_taxonomy).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, renewable_portfolio_standards).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, nuclear_licensing_reform).

% DUAL FORMULATION NOTE:
% Part of the climate_mitigation_imperative constraint family (kernel decomposition per ε-invariance). This reading optimizes for deployment rate; portfolio_optimization_reading optimizes for portfolio reliability; systems_transition_reading optimizes for system architecture. The three readings have different ε, different victim/beneficiary sets, and different type classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, institutional, 0.3).
constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, powerful, 0.85).
constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
