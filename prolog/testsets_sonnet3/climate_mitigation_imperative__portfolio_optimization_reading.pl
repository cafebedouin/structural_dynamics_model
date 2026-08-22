% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Portfolio Optimization Reading of Climate Mitigation (Technology-Neutral Carbon Intensity Standard)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the portfolio-optimization reading of the climate
 *   mitigation imperative kernel: mitigation requires maximizing deployment
 *   of ALL low-carbon sources, and nuclear power is treated as structurally
 *   necessary for reliable baseload capacity that intermittent renewables
 *   cannot yet fully substitute. Under this reading, the operative policy
 *   metric is carbon intensity per unit of firm capacity, deliberately made
 *   technology-neutral so nuclear qualifies for the same subsidy,
 *   loan-guarantee, and capacity-market mechanisms as wind and solar. This is
 *   a distinct constraint from the sibling opportunity_cost_reading (which
 *   treats nuclear's capital intensity and multi-year timelines as
 *   net-harmful to the fastest-deployment-per-dollar goal) and the
 *   systems_transition_reading (which treats nuclear as perpetuating
 *   extractive centralization incompatible with decentralized, democratically
 *   controlled energy systems). Each reading has its own beneficiary/victim
 *   structure and its own epsilon; this file authors only the
 *   portfolio-optimization reading's epsilon, holding it invariant rather
 *   than averaging across the kernel contest.
 *
 * KEY AGENTS:
 *   - nuclear_industry: primary beneficiary (organized/arbitrage) — captures subsidy and market access under technology-neutral standard
 *   - existing_nuclear_utilities: beneficiary and co-agenda-setter (institutional/arbitrage) — shapes standard design, collects capacity payments
 *   - fossil_fuel_incumbents: primary target (powerful/constrained) — excluded from qualifying capacity, faces phase-out
 *   - coal_dependent_regions: diffuse victim (powerless/trapped) — bears concentrated local economic cost
 *   - grid_reliability_operators: agenda-setting beneficiary (institutional/constrained) — sets procurement rules favoring firm capacity
 *   - renewable_only_advocates: excluded voice — contests the baseload-necessity premise itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio Optimization Reading of Climate Mitigation (Technology-Neutral Carbon Intensity Standard)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '423a460c-943f-40c5-88a3-0f0a145822f3').
narrative_ontology:cs_kernel_codification('423a460c-943f-40c5-88a3-0f0a145822f3', distributed).
narrative_ontology:cs_authority_grounding('423a460c-943f-40c5-88a3-0f0a145822f3', distributed).
narrative_ontology:cs_reading_relation('423a460c-943f-40c5-88a3-0f0a145822f3', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('423a460c-943f-40c5-88a3-0f0a145822f3', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('423a460c-943f-40c5-88a3-0f0a145822f3', foundational, technology_neutrality_by_carbon_intensity).
narrative_ontology:cs_axiom_status(technology_neutrality_by_carbon_intensity, holdable).
narrative_ontology:cs_axiom_grounding('423a460c-943f-40c5-88a3-0f0a145822f3', technology_neutrality_by_carbon_intensity, instrumental).
narrative_ontology:cs_axiom('423a460c-943f-40c5-88a3-0f0a145822f3', foundational, firm_baseload_capacity_is_necessary_not_optional).
narrative_ontology:cs_axiom_status(firm_baseload_capacity_is_necessary_not_optional, holdable).
narrative_ontology:cs_axiom_grounding('423a460c-943f-40c5-88a3-0f0a145822f3', firm_baseload_capacity_is_necessary_not_optional, empirically_contingent).
narrative_ontology:cs_reference_frame('423a460c-943f-40c5-88a3-0f0a145822f3', multi_technology_decarbonization_portfolio).
narrative_ontology:cs_drift_state('423a460c-943f-40c5-88a3-0f0a145822f3', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('423a460c-943f-40c5-88a3-0f0a145822f3', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, existing_nuclear_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, advanced_reactor_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, coal_dependent_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, stranded_fossil_asset_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, carbon_intensity_as_sole_relevant_metric).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives loan guarantees, production tax credits, and technology-neutral clean energy standard eligibility under this reading's carbon-intensity-only framing. Lobbies to keep the standard defined by emissions-per-kWh rather than deployment speed, capital cost, or ownership structure, since those axes would disadvantage nuclear relative to wind and solar.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, arbitrage, national).

% Operate large baseload plants that receive premium payments or credits once the policy regime treats carbon intensity as the sole qualifying metric. Advocate directly for the regulatory standard's design, testifying that the grid requires firm low-carbon capacity that only nuclear can currently provide at scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, existing_nuclear_utilities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, existing_nuclear_utilities, agenda_setter).

% Small modular reactor and next-generation nuclear firms depend on this reading's framing to access the same subsidy pools as renewables. Their commercial viability is directly tied to whether policy treats baseload firmness as a mitigation requirement rather than an engineering preference among several.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, advanced_reactor_developers, beneficiary,
    moderate, generational, constrained, national).

% System operators responsible for grid stability endorse the baseload-necessity framing because it justifies procurement rules that keep dispatchable capacity on the books. They set interconnection and capacity-market rules that structurally favor generators meeting the 'firm' criterion nuclear satisfies.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_operators, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_operators, agenda_setter).

% Coal and gas generators are the arrangement's primary target: the technology-neutral carbon-intensity standard is specifically designed to exclude them from the same subsidy and market-access mechanisms nuclear receives. They face phase-out mandates, carbon pricing, and loss of capacity-market eligibility justified by the same portfolio-maximization logic that admits nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, national).

% Communities whose local economies and tax bases depend on coal plants and mines bear concentrated employment and revenue losses as the technology-neutral standard accelerates fossil retirement without necessarily directing equivalent replacement investment to their locality. Geographic and economic immobility limits meaningful exit.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, coal_dependent_regions, payer,
    powerless, biographical, trapped, regional).

% Investors and utilities holding fossil generation assets face write-downs as the carbon-intensity standard reclassifies their capacity as non-qualifying. They can divest or seek regulatory relief but absorb the transition cost the standard imposes on their existing capital base.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, stranded_fossil_asset_holders, payer,
    moderate, biographical, constrained, national).

% Advocates who believe wind, solar, and storage alone can meet reliability needs faster and cheaper than nuclear are structurally sidelined by this reading's baseload-necessity premise, which treats their preferred pathway as insufficient by definition rather than as a contestable empirical claim.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates, excluded,
    organized, generational, constrained, national).

% Electricity customers fund nuclear cost overruns and capacity payments through rates or taxes, while also benefiting from reduced blackout risk and lower long-run emissions if the reliability claim holds. Their exit is limited to relocation or, where available, retail choice programs that rarely span full grid regions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers, beneficiary).

% Model levelized system costs, deployment timelines, and reliability tradeoffs across technology portfolios. Their analyses are cited selectively by all sides of the kernel contest to support divergent readings of what mitigation actually requires.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decarbonization investment and permitting priority around a single technology-neutral metric — carbon intensity per unit of firm, dispatchable capacity — so that all low-carbon sources, including nuclear, can compete for the same finite subsidy and grid-access pool without being excluded by category.
% TRANSFER_FUNCTION: Moves subsidy dollars, loan guarantees, capacity-market payments, and regulatory priority from fossil fuel generators (and the regions dependent on them) to nuclear operators and developers, justified by the shared carbon-intensity criterion that both technologies are measured against but only one satisfies going forward.
% ABSENT_VOICES: Renewable-only advocates who dispute the baseload-necessity premise are excluded from the standard-setting conversation because the reading treats firmness as a settled engineering requirement rather than a contested policy choice; ratepayers in coal regions have limited standing in federal or state clean-energy-standard rulemakings compared to organized utility and nuclear-industry interveners.
% DISAPPEARANCE_RATIONALE: If the technology-neutral carbon-intensity standard were replaced by a renewables-only or fastest-deployment-per-dollar criterion, nuclear subsidy eligibility, capacity-market treatment, and loan guarantee programs would collapse or be redirected; fossil incumbents would face a different (likely faster) phase-out schedule under the sibling opportunity-cost reading, and coal-region transition funding would be allocated on different terms entirely.
% FOUNDING_PROBLEM: Grid operators and climate modelers observed that intermittent renewables alone, without storage at sufficient scale and cost, could not guarantee reliability during multi-day low-wind/low-solar periods, and that meeting emissions targets on the required timeline appeared to need every available low-carbon technology deployed simultaneously rather than sequenced by cost.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability operators and national laboratory system-cost studies attest the reliability gap is real and ongoing, supporting the founding problem as live. Independent energy-systems researchers outside the nuclear industry (some cited by the sibling opportunity_cost_reading) dispute that nuclear specifically is required to close it, arguing storage, transmission expansion, and demand flexibility are closing the gap faster and cheaper — making the founding problem's nuclear-specific framing contested rather than settled.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) is moderate: the standard genuinely solves a real coordination problem (grid reliability under decarbonization) but does so by directing scarce subsidy dollars away from fossil incumbents toward nuclear in a way that is contestable rather than obviously efficient — hence tangled_rope rather than pure rope. Suppression (0.38) is lower than a pure extraction story because fossil incumbents retain political and market avenues to contest the standard (litigation, lobbying for grandfather clauses, slower phase-out timelines), and renewable-only advocates can still contest the baseload premise in public discourse even though excluded from rulemaking. Theater ratio (0.22) is low-moderate: most of the activity is genuine investment and regulatory design, though a growing share of nuclear-industry advocacy performs 'reliability necessity' rhetoric beyond what current system-cost studies unambiguously support. All three metrics are authored on one shared six-point grid from t=0 to t=20, tracking the standard's gradual entrenchment as more jurisdictions adopt technology-neutral clean energy standards.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-industry and grid-operator seats, this reading is straightforward coordination: solving a real reliability gap by refusing to arbitrarily exclude a low-carbon technology. From the fossil-incumbent and coal-region seats, the same technology-neutral framing operates as a targeted exclusion mechanism dressed in neutral language — carbon intensity as a metric was selected, in part, because it is the axis on which nuclear can compete and fossil cannot. The engine should compute divergent per-seat types from this same structural data without this file adjudicating which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-sector stakeholders derive low d (near beneficiary end): they receive subsidy, loan guarantees, and market access directly, with arbitrage-grade exit into a favorable regulatory environment they helped shape. Fossil incumbents derive high d (near target end): they are structurally excluded from the same mechanisms by the metric's design, with constrained exit given sunk capital and regulatory lock-in. Coal-dependent regions get an even higher effective d despite lower base power, because geographic and economic trapping (exit_options: trapped) amplifies the extraction they experience relative to mobile capital holders. Ratepayers sit closer to symmetric — they fund the transfer but also plausibly receive reliability benefits, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grid reliability during renewables intermittency) is contested rather than resolved or dead — some analysts affirm it remains live and nuclear-specific, others argue storage and transmission have substantially closed the gap, making continued nuclear-specific subsidy an arrangement that has outlived (or never had) the necessity it claims. Classifying this as tangled_rope rather than snare or rope preserves that contest: it registers the genuine coordination function (a real reliability gap existed and existing renewables-only portfolios have documented multi-day gaps in some grids) while also registering the asymmetric transfer to a politically organized beneficiary class away from a disfavored one, under enforcement (regulatory standard-setting, subsidy administration) that requires active maintenance to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_status,
    'Is nuclear power structurally necessary for grid reliability under deep decarbonization, or has storage-plus-transmission-plus-demand-flexibility closed the reliability gap sufficiently that the baseload-necessity premise is now obsolete?',
    'Longitudinal system-cost and reliability studies comparing grids that pursued renewables-plus-storage-only pathways against those that retained or expanded nuclear capacity, controlling for grid size, geography, and interconnection.',
    'If the necessity claim is empirically false or increasingly false over time, the technology-neutral standard''s exclusion of fossil incumbents in favor of nuclear loses its coordination justification and the arrangement shifts further toward pure extraction (snare) for the nuclear-industry beneficiary class. If true, the tangled_rope coordination function is substantiated and the extraction is more clearly the necessary cost of solving a real problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_status, empirical, 'Whether nuclear baseload is technically necessary or contingently claimed.').

omega_variable(
    kernel_reading_selection_is_political,
    'Is the choice among the three sibling readings (portfolio_optimization, opportunity_cost, systems_transition) a resolvable empirical question, or is it irreducibly a values/institutional-interest question about which axis (speed-per-dollar, technology-neutrality, or decentralization) should govern climate policy?',
    'No single resolution mechanism exists; this is documented as a conceptual/preference-type omega rather than an empirical one, since the three readings do not disagree primarily about facts but about which optimization criterion is normatively correct for ''mitigation.''',
    'If irreducibly a preference question, then no future data resolves the kernel contest and the three constraint files remain permanently coexisting siblings rather than converging toward one ''correct'' reading — this is registered in cs_structure.reading_relations as coexists_with rather than forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_is_political, conceptual, 'Whether the kernel contest is empirically resolvable or a permanent values dispute.').

omega_variable(
    capture_vs_genuine_coordination,
    'To what extent did the nuclear industry''s lobbying causally shape the technology-neutral carbon-intensity metric itself, as opposed to that metric being independently selected by climate scientists and grid engineers on its analytical merits?',
    'Legislative and regulatory-history analysis tracing the origin of technology-neutral clean energy standard language, cross-referenced against nuclear-industry lobbying disclosure records and the timing of standard adoption relative to industry advocacy campaigns.',
    'If the metric was substantially shaped by industry advocacy to select for nuclear''s comparative advantage, the coordination function is partly cover for capture, pushing the classification toward snare. If the metric predates and is independent of that advocacy, the tangled_rope''s coordination component is more clearly genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_vs_genuine_coordination, empirical, 'Whether the technology-neutral metric was industry-shaped or independently derived.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'climate mitigation imperative' kernel per the epsilon-invariance principle. Each sibling authors a distinct epsilon: this file (portfolio_optimization_reading) authors 0.42 for a technology-neutral, nuclear-inclusive standard; opportunity_cost_reading would author a different epsilon for the same standing arrangement viewed through a fastest-deployment-per-dollar lens (likely higher, since nuclear's capital intensity and timeline would be read as actively harmful delay); systems_transition_reading would author yet another epsilon emphasizing centralization harms independent of carbon intensity. All three link to each other via affects_constraints since they compete for the same policy real estate and a shift in one reading's political dominance structurally pressures resource availability for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
