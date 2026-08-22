% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Opportunity Cost Reading: Fastest Deployment per Dollar
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel: the climate
 *   mitigation imperative. The opportunity-cost reading frames mitigation as
 *   requiring fastest deployment per dollar of capital, treating nuclear's
 *   multi-decade timelines and capital intensity as net-harmful opportunity
 *   costs that divert resources from renewables, storage, and grid
 *   technologies that deploy faster and cheaper on the margin. The standing
 *   arrangement under contest (the referent of extractiveness) is the set of
 *   capital allocation, policy priority, and investment-guidance decisions
 *   that treat nuclear and renewables as interchangeable low-carbon sources
 *   rather than evaluating them on deployment-speed-per-dollar efficiency.
 *   This reading asserts that treating them as equivalent — despite their
 *   radically different capital and timeline profiles — extracts value from
 *   renewable deployment and the carbon-per-year metric. Sibling readings
 *   contest this: portfolio-optimization reading asserts all low-carbon
 *   sources are necessary for reliability and system redundancy;
 *   systems-transition reading asserts the mitigation imperative subordinates
 *   speed to democratized energy control, which nuclear perpetuates as a
 *   centralized architecture.
 *
 * KEY AGENTS:
 *   - Renewable energy developers (solar, wind, distributed): primary beneficiaries of fastest-per-dollar framing; deployment is faster at lower per-dollar cost; this reading vindicates their technology choice.
 *   - Nuclear industry and baseload advocates: victims in this reading's frame; capital diverted to renewables; their timelines and cost profiles are structurally disadvantaged under opportunity-cost logic.
 *   - Climate scientists and carbon-urgency advocates: beneficiaries insofar as fastest-per-dollar aligns with their emphasis on cumulative carbon avoidance in the 2026–2036 critical window.
 *   - Grid operators and reliability engineers: payers (constrained to rebuild grid architecture to handle distributed renewables); also partially beneficiaries if faster deployment reduces stranded assets and grid-modernization urgency.
 *   - Policy makers and energy department officials: agenda-setters; choose which reading to operationalize through deployment incentives, tax treatment, and investment guidance.
 *   - Capital markets and energy investors: enforcer of the constraint through capital allocation; where deployment-speed logic prevails, renewable projects attract capital faster, starving nuclear of financing.
 *   - Energy-dependent industries and electricity consumers: partial victims (higher grid costs during transition); partial beneficiaries (cheaper renewables pass through over time).
 *   - Analytical observer (decarbonization science): observes the measurement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.41).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Opportunity Cost Reading: Fastest Deployment per Dollar").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'b19c0328-a5d9-490b-bcad-8c474a53f536').
narrative_ontology:cs_kernel_codification('b19c0328-a5d9-490b-bcad-8c474a53f536', formalized).
narrative_ontology:cs_authority_grounding('b19c0328-a5d9-490b-bcad-8c474a53f536', extraction).
narrative_ontology:cs_interpretation_layer_present('b19c0328-a5d9-490b-bcad-8c474a53f536').
narrative_ontology:cs_reading_relation('b19c0328-a5d9-490b-bcad-8c474a53f536', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('b19c0328-a5d9-490b-bcad-8c474a53f536', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('b19c0328-a5d9-490b-bcad-8c474a53f536', foundational, fastest_per_dollar_carbon_metric_primacy).
narrative_ontology:cs_axiom_status(fastest_per_dollar_carbon_metric_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b19c0328-a5d9-490b-bcad-8c474a53f536', fastest_per_dollar_carbon_metric_primacy, instrumental).
narrative_ontology:cs_axiom('b19c0328-a5d9-490b-bcad-8c474a53f536', secondary, capital_fungibility_nuclear_renewable_substitution).
narrative_ontology:cs_axiom_status(capital_fungibility_nuclear_renewable_substitution, holdable).
narrative_ontology:cs_axiom_grounding('b19c0328-a5d9-490b-bcad-8c474a53f536', capital_fungibility_nuclear_renewable_substitution, empirically_contingent).
narrative_ontology:cs_reference_frame('b19c0328-a5d9-490b-bcad-8c474a53f536', capital_scarcity_decarbonization_urgency).
narrative_ontology:cs_drift_state('b19c0328-a5d9-490b-bcad-8c474a53f536', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b19c0328-a5d9-490b-bcad-8c474a53f536', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, distributed_solar_wind_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_decentralization_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_power_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, capital_intensive_energy_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, baseload_energy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_scientists_and_urgency_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_operators_and_reliability_engineers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, energy_dependent_industries).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, grid_operators_and_reliability_engineers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, energy_dependent_industries).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and distributed generation firms benefit from the faster-per-dollar metric because it operationalizes their competitive advantage (lower capital, faster deployment timelines). Policy incentives (tax credits, renewable mandates, grid interconnection priority) are structured around the opportunity-cost reading. They deploy projects at 2–4 year timelines with $1–2 billion capital per 1 GW, directly competing with nuclear's 10–15 year timelines and $10–15 billion per plant. Capital markets increasingly allocate investment based on the faster-per-dollar metric, which accelerates their deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Nuclear utilities, reactor manufacturers, and fuel suppliers face systematic disadvantage under the opportunity-cost reading. Their capital requirements and long timelines mean every nuclear project must justify itself against the faster-per-dollar metric, which is structurally unfavorable to centralized, capital-intensive technology. They operate under regulatory and public-consent constraints that make rapid scaling difficult. Their exit options are constrained: existing nuclear expertise does not transfer cleanly to renewables; workforce and supplier ecosystems are locked into nuclear-specific skills; regulatory relationships and political capital are nuclear-specific.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Grid operators, utilities planning for long-term reliability, and energy-security advocates argue that baseload generation (whether nuclear, hydro, or natural gas with carbon capture) is essential for grid stability. They pay the cost of the opportunity-cost reading by being forced to defend their value proposition against a single metric (carbon-per-dollar) that does not capture system-reliability benefits. They argue that intermittent renewables require either massive storage infrastructure or flexible backup generation, both of which impose system costs not captured by the faster-per-dollar metric. Their secondary benefit is that they are consulted in grid-planning conversations and can advocate for system designs that hedge between renewables and baseload.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, baseload_energy_advocates, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, baseload_energy_advocates, beneficiary).

% Climate and energy researchers who emphasize the 2026–2036 critical decarbonization window benefit from the opportunity-cost reading because it operationalizes their technical findings about cumulative carbon avoidance. The reading supports their advocacy that deployment speed and capital efficiency are the binding constraints. They publish research, advise policy makers, and serve on energy commissions. Their analytical position gives them credibility but limited direct control over energy investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_scientists_and_urgency_advocates, beneficiary,
    analytical, civilizational, analytical, global).

% They pay costs from rapid renewable deployment without corresponding investments in storage and grid modernization: reliability risk, frequency-regulation challenges, inertia loss, and dispatch complexity. They benefit from avoided stranded assets (rapid renewable deployment can retire coal plants faster than planned) and from clarity on long-term grid direction. They operate under regulatory mandate to maintain reliability, which constrains their ability to favor one reading over another. Their exit is constrained by regional regulation and asset lock-in; their power is powerful within regional grid planning.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_operators_and_reliability_engineers, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, grid_operators_and_reliability_engineers, beneficiary).

% Institutional investors, venture capital, and energy funds determine where capital flows in response to policy incentives and perceived return profiles. Under the opportunity-cost reading and associated policy frameworks (renewable tax credits, carbon pricing, grid interconnection priority), capital has systematically flowed toward renewable projects. Capital is globally mobile, and the faster-per-dollar metric provides a simple decision rule: renewable projects with shorter timelines and lower capital requirements get funded; nuclear projects require longer justification and higher risk premiums. This agenda-setting function (choosing which projects to fund based on the metric) is the mechanism that enforces the constraint on the ground.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, capital_markets_and_energy_investors, agenda_setter,
    institutional, biographical, mobile, global).

% Manufacturing, agriculture, and data-center operators pay costs during grid transition (higher electricity prices during renewable integration, reliability risk). They benefit from long-term cheaper renewables if deployment accelerates faster than baseline projections. Their exit is constrained by regional location and energy-intensive operations; their power is organized through industry associations and utility regulation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_dependent_industries, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, energy_dependent_industries, beneficiary).

% End-user electricity customers pay for grid transition costs (possibly higher rates during renewable buildout if storage lags) and benefit from long-term cost reductions if renewable deployment achieves aggressive scaling. They have no direct voice in energy investment decisions but their aggregate demand and price sensitivity shape utility planning. Their exit is trapped: they depend on the regional grid and cannot switch providers in most jurisdictions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers, beneficiary).

% Government energy departments, regulatory commissions, and legislative bodies set policy frameworks (deployment incentives, carbon pricing, grid interconnection rules, R&D funding). The opportunity-cost reading is operationalized through their decisions: tax credits for renewables, deployment mandates, carbon pricing that penalizes high-carbon sources, and R&D priority tilting toward renewable and storage technologies. They benefit from political clarity (the faster-per-dollar reading is easy to communicate to voters) and from rapid emissions reductions if the reading's assumptions hold. They pay costs in the form of political opposition from baseload advocates, reliability risk if deployment outpaces grid infrastructure, and opportunity cost if the portfolio-optimization reading proves correct (i.e., if reliability becomes the binding constraint and the omission of nuclear becomes costly).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, policy_makers_and_energy_officials, agenda_setter,
    institutional, biographical, mobile, national).

% Parties who would argue for a balanced portfolio (renewables, nuclear, hydro, storage, flexible generation) are structurally marginalized in the opportunity-cost reading: their position requires arguing against the faster-per-dollar metric, which sounds like arguing against urgency. They have platforms in some policy contexts (France, China, South Korea prioritize nuclear) but are excluded from the dominant energy-policy conversation in regions (US, EU, Germany) where the opportunity-cost reading prevails. If they were at the table, they would contest the metric itself and propose alternative metrics (carbon-per-reliable-megawatt, cost-per-grid-stability, cumulative-carbon-over-30-years) that would shift the constraint's structure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, portfolio_optimization_advocates, excluded,
    powerful, generational, mobile, global).

% Decentralization, energy-democracy, and community-energy advocates would argue that both the opportunity-cost reading and portfolio-optimization reading accept an extractive centralized energy system and merely debate which centralized technology to deploy. They are partially excluded from mainstream energy policy (they have some platforms in local government and cooperative movements) but are marginalized from national energy-strategy conversations. If they were structurally included, they would propose measuring constraints differently: decentralization-feasibility, community-control ratios, democratic-decision-making depth.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, systems_transition_advocates, excluded,
    moderate, civilizational, mobile, local).

% An external analytical perspective that measures the constraint's operation without being embedded in any faction's interests. Observes the flow of capital, the operative policy metrics, the distribution of beneficiary and victim positions, and the resistance dynamics.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocate limited capital and limited deployment-capacity to maximize cumulative greenhouse-gas reduction per unit invested within the 2026–2036 critical decarbonization window, using a single, clear metric (carbon-per-dollar-per-year) to resolve competing claims for capital.
% TRANSFER_FUNCTION: Moves investment capital from nuclear projects (which have higher per-dollar costs and longer timelines) to renewable and storage projects (which have lower per-dollar costs and faster timelines). The operative transfer is a priority ordering in capital allocation, grid-interconnection queues, and policy incentives, not a direct monetary flow, but the effect is a reallocation of finite deployment capacity.
% ABSENT_VOICES: Portfolio-optimization advocates who would argue for including nuclear in the investment mix based on system-reliability metrics are partially excluded from dominant policy conversations in the US and EU (where the opportunity-cost reading prevails), though they retain platforms in France, China, and South Korea. Systems-transition advocates who would dispute the entire centralized-energy framing are largely excluded from national energy-strategy conversations, retaining presence primarily in local government and cooperative sectors. If these voices were structurally included, they would propose alternative metrics and challenge the foundational assumptions of the reading.
% DISAPPEARANCE_RATIONALE: If the opportunity-cost reading disappeared and policy shifted to portfolio-optimization logic or systems-transition logic, capital would immediately reallocate: nuclear projects would re-enter the financing queue, baseload-reliability considerations would re-weight grid-planning decisions, and policy incentives would no longer systematically favor renewables. Grid operators would re-architect deployment timelines to prioritize system stability alongside emissions reduction. Distributed renewable and battery-storage projects would face increased competition for capital and policy support. The energy sector's organizational structure, investment patterns, and policy landscape would substantially rearrange.
% FOUNDING_PROBLEM: We have limited capital and limited deployment capacity to decarbonize; which technology allocation strategy maximizes carbon avoidance per unit invested within the critical 2026–2036 window, given that cumulative emissions in this period disproportionately affect long-term climate outcomes?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by climate models from the IPCC, independent energy research (IEA, IRENA, NREL), and peer-reviewed literature on carbon budgets and critical-window urgency. Climate scientists (outside the energy-finance industry) consistently attest that deployment speed in the 2026–2036 period is a binding constraint on cumulative carbon outcomes. Energy-efficiency researchers and modelers independent of the nuclear and renewable industries corroborate that per-dollar and per-year metrics are relevant to decarbonization strategy. No corroboration comes from within the nuclear industry itself (which argues the founding problem is over-stated and reliability concerns justify slower but more comprehensive deployment), but that disagreement is about the metric's relative weight, not about the existence of capital and timeline constraints.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is set at 0.68 because the reading operationalizes a specific metric (carbon-per-dollar-per-year) that systematically disadvantages nuclear relative to renewables, regardless of absolute carbon outcomes. This is extractive in the sense that nuclear's real contributions to decarbonization are discounted by the faster-per-dollar metric — capital that could have built nuclear is extracted and redirected to renewable projects. The metric is a constructed choice, not a natural law: portfolio-optimization reading would use a different metric (carbon-per-reliable-megawatt, or cumulative carbon over 30 years), under which nuclear's contribution would rise and extraction would fall. Suppression is moderate (0.41) because the fastest-per-dollar framing is contested but not coercively silenced — nuclear advocates have platforms and research funding; they simply operate under a constraint that requires justifying their position against the faster-per-dollar metric rather than simply asserting portfolio diversity. Theater is low (0.22) because the debate is substantive: real tradeoffs between speed and reliability, real empirical questions about capital fungibility and grid transition pathways. Neither side is purely performative; both are defending legitimate technical positions. Accessibility-collapse is moderate (0.52) because alternatives DO exist once the reading is understood — parties can argue for portfolio-optimization or systems-transition readings, and the fastest-per-dollar reading does not foreclose them, merely constrains the debate to a specific metric. Resistance is high (0.73) because substantial organized interests (nuclear industry, baseload advocates, grid operators valuing reliability) mount active resistance to the opportunity-cost framing and continue to argue for nuclear inclusion. The measurement series shows extractiveness rising from 0.48 at interval-start (2006: pre-climate urgency, nuclear valued) to 0.68 observed at 2026 (climate urgency vindicated opportunity-cost reading; capital flows accelerate toward renewables). Theater remains low, suggesting substantive technical content rather than drift into mere defense. Suppression plateaus (0.35→0.41), indicating that resistance persists but constraint does not harden further — the political economy stabilizes around two live readings rather than one foreclosing the other.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable developers' position, the faster-per-dollar reading is a fundamental truth of energy physics: limited capital, limited time, fastest deployment wins. From the nuclear industry's position, the reading is a constructed metric that ignores real system needs (baseload, grid reliability, industrial heat). From climate scientists' position, it is a necessary operational constraint to maximize cumulative carbon avoidance in the critical 2026–2036 window. From grid operators' position, it is a constraint that forces them to re-architect for distributed sources while losing the centralized dispatchability they engineered into baseload grids. The engine computes these divergences from the structural data (beneficiary/victim, exit options, power atoms). The authored claim and metrics are independent: I claim this reading is a tangled_rope (real coordination function — faster deployment DOES reduce emissions — plus asymmetric extraction from nuclear advocates), but the metrics describe extractiveness rising over time (0.48→0.68), suggesting the reading is gaining institutional power and the extraction is amplifying, not stabilizing. This divergence is exactly the measurement the corpus exists to take: a claimed coordination function that computes as increasingly extractive is a candidate for mandatrophy or for reclassification as the reading's operational frame shifts from coordination to selective acceleration.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers sit near d=0.0 (full beneficiary): faster-per-dollar metric directly supports their capital access and deployment speeds. Baseload advocates and nuclear industry sit near d=1.0 (full target): the metric extracts value from their resource-allocation choices and constraints their competitive positioning. Grid operators sit near d=0.5 (symmetric): they benefit from faster renewable deployment (avoiding stranded coal assets, grid modernization urgency) but pay costs (grid stability during transition, storage infrastructure). Policy makers sit near d=0.3–0.4 (partial beneficiary): they benefit from opportunity-cost clarity and rapid emissions reductions, but pay costs in reliability risk and industry disruption. Capital markets are the enforcer (d derives from directionality toward the constraint's operation, not traditional beneficiary/victim): they respond to the metric by allocating capital; their d depends on whether they are renewable-focused (d≈0.1) or diversified (d≈0.5). The energy sector's audience is climate scientists and urgency-advocates (d≈0.1 beneficiary); they vindicate the reading's core premise. The diversion here is that beneficiary set is NOT concentrated on a single agent — the constraint benefits a diffuse coalition (renewable firms, climate advocates, policy-urgent advocates, capital markets underweighting nuclear risk). Victim set IS more concentrated (nuclear industry, baseload-reliability engineers), which creates asymmetric extraction pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for the opportunity-cost reading is: 'We have limited capital and limited time to decarbonize; which technology pathway maximizes carbon avoidance per unit invested?' The reading declares the founding-problem status as 'live' — the constraint between capital scarcity and decarbonization urgency remains acute in 2026. However, there is a mandatrophy risk embedded in the reading's operation: if and when renewable deployment accelerates such that the binding constraint shifts from 'fastest-per-dollar' to 'system reliability' or 'grid integration complexity,' the opportunity-cost reading's core logic no longer resolves the constraint. At that point, the reading would become a cover story for preferential allocation to renewables, not a solution to a real resource-scarcity problem. The measurement series shows extractiveness plateauing at 0.68 observed (not rising further after t=20), which is an early signal: the reading may be stabilizing into a power-asymmetry arrangement rather than adapting to changing conditions. If renewable deployment does accelerate faster than predicted, and grid reliability and integration become the binding constraints (rather than capital scarcity), the reading's mandate becomes obsolete but the policy arrangement persists — that is mandatrophy. The six-questions disappearance verdict ('world_rearranges') supports this: if the opportunity-cost reading disappeared and policy shifted to portfolio-optimization logic, capital would immediately flow back toward nuclear projects, and grid planners would re-architect around baseload stability. That rearrangement is too significant for the mandate to be naturally false-summit; it is genuinely consequential. The theater-ratio plateau (0.22 observed, holding steady) suggests the reading is substantive, not devolving into performance — the debate remains technical and real. Mandatrophy would manifest as theater_ratio rising sharply (performance defensive arguments replacing technical substance) or as extractiveness decoupling from any measurable speedup in renewable deployment (extraction persisting despite the founding problem being solved). Neither signal is present yet; the constraint is not yet mandatrophied. However, the omega variables flag that this status is fragile: if capital-fungibility proves false (nuclear capital is separate), or if discount rates favor long-term portfolio diversity over short-term speed, the founding problem's status shifts to 'contested' or 'dead,' and mandatrophy risk rises sharply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the climate mitigation imperative require fastest-deployment-per-dollar as the organizing principle, or does it permit/require portfolio optimization across all low-carbon sources?',
    'Empirical climate modeling: compare cumulative carbon avoidance over 10-year and 30-year horizons across scenarios (pure-renewables acceleration vs. mixed nuclear+renewables portfolio) using identical carbon-value assumptions and discount rates.',
    'If fastest-per-dollar scenarios avoid more cumulative carbon in the critical 2026–2036 window, this reading''s constraint is vindicated and nuclear''s capital diversion becomes demonstrably net-harmful. If portfolio mixing avoids more cumulative carbon over the full 30-year horizon, the portfolio-optimization reading displaces this reading structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether fastest-per-dollar deployment is the binding constraint or portfolio diversity is.').

omega_variable(
    capital_fungibility_assumption,
    'Is capital between nuclear and renewable projects genuinely fungible, or does nuclear attract investment that would not otherwise go to energy infrastructure?',
    'Historical capital-flow analysis: track actual investment patterns in countries that expanded nuclear (France, South Korea, China) vs. renewable-only regions (Denmark, Costa Rica) controlling for GDP, energy demand, and policy incentives. Does nuclear crowd out renewables or mobilize incremental energy capital?',
    'If capital is fungible and zero-sum, nuclear diverts resources from renewables and this reading holds at high confidence. If nuclear mobilizes new capital streams, the diversion story breaks and this reading''s extraction claim weakens substantially — nuclear becomes a complement, not a substitute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_fungibility_assumption, empirical, 'Whether nuclear and renewable capital pools are separate or compete in a unified market.').

omega_variable(
    time_horizon_discount_rate,
    'At what carbon-value-per-year and discount rate does the opportunity cost of delay outweigh nuclear''s build-time and cost premium?',
    'Sensitivity analysis on climate models: vary the damage function (marginal cost of carbon per ton per year) and discount rate from extreme mitigation-urgent (3%/yr, $500/ton) to technology-neutral (7%/yr, $50/ton); map the region where fastest-per-dollar flips to portfolio-optimized.',
    'High sensitivity to discount rate and carbon valuation means this reading''s strength is epistemic-preference-dependent, not empirically robust across framings. Low sensitivity means the fastest-per-dollar imperative holds across reading variations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(time_horizon_discount_rate, preference, 'The reading''s robustness to assumptions about urgency and damage valuation.').

omega_variable(
    systems_transition_vs_decarbonization_priority,
    'Does the climate mitigation imperative require rapid carbon avoidance on any pathway, or does it subordinate speed to democratized/decentralized energy control?',
    'Comparative governance analysis: where renewable-only systems were deployed fastest (Denmark 2000–2015, Costa Rica 2010–2020) vs. nuclear-mixed systems (France, South Korea), did decentralization outcomes differ? Does democratic control correlate with renewable predominance, or is it orthogonal to carbon trajectories?',
    'If decentralization and speed are compatible (renewable systems can decarbonize faster AND democratize), this reading and the systems-transition reading coexist on different priority axes. If they conflict (fastest decarbonization requires centralized nuclear), the systems-transition reading forecloses this reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systems_transition_vs_decarbonization_priority, conceptual, 'Whether fastest-per-dollar and systems-transition are complementary or competing interpretations of the mitigation imperative.').

omega_variable(
    extraction_mechanism_ambiguity,
    'Is the measured suppression (0.41) structural — enforced by capital markets, energy security doctrine, or policy bias — or internalized — nuclear advocates accept the faster-per-dollar critique as legitimate but argue other values (reliability, workforce stability, industrial continuity) outweigh it?',
    'Post-policy-shift trajectory: if carbon taxes or deployment incentives shift to pure-renewable priority, do nuclear advocates mount sustained political resistance, or do they acknowledge the constraint and migrate to renewables? Persistence of organized resistance post-incentive-shift indicates structural suppression; acceptance indicates internalized rather than structurally suppressed disagreement.',
    'If suppression is internalized, this reading''s constraint is weaker — it is contestable doctrine, not coerced arrangement. If structural, the suppression value stands and this reading''s tangled-rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Whether suppression of faster-per-dollar framing is structural or internalized disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested climate-mitigation-imperative kernel. The opportunity-cost reading frames mitigation as requiring fastest deployment per dollar, treating nuclear's capital intensity and timelines as net-harmful diversions from renewable acceleration. The portfolio-optimization reading argues all low-carbon sources are necessary for system reliability; the systems-transition reading argues mitigation requires decentralized energy control, not just decarbonized sources. These are three distinct constraints on the same kernel — they have different ε values, different victim/beneficiary sets, and different suppression mechanisms. Each story models the constraint's operation under one reading; the network edges indicate the readings' structural relationships to each other. The three stories are siblings in a constraint family, not variations of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
