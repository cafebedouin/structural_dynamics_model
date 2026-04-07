% ============================================================================
% CONSTRAINT STORY: shipping_emissions_cap_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shipping_emissions_cap_allocation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shipping_emissions_cap_allocation
 *   human_readable: Shipping Emissions Cap Allocation System
 *   domain: environmental_policy/maritime_economics
 *
 * SUMMARY:
 *   The International Maritime Organization's Energy Efficiency Existing Ship
 *   Index (EEDI) and subsequent Carbon Intensity Indicator (CII) regulations
 *   create a global shipping emissions cap allocation system that coordinates
 *   decarbonization while extracting rents from smaller operators and
 *   developing nations. The constraint exhibits tangled-rope characteristics:
 *   genuine coordination function (prevents commons tragedy of carbon
 *   emissions racing to bottom) combined with asymmetric extraction
 *   (capital-rich, efficient operators gain windfall cap allocations while
 *   capital-poor operators face steep retrofit costs). The system distributes
 *   initial cap allocations based on historical emissions, which preserves
 *   existing market dominance. Suppression is structural: developing-nation
 *   fleets are locked into global shipping routes by economic dependence and
 *   capital constraints; compliance costs are passed through to ports and
 *   cargo customers, not absorbed by the regulation's designers. Theater
 *   ratio reflects IMO's formal authority combined with functional capture by
 *   shipping industry interests; EU implementation has higher functional
 *   content (actual enforcement) but narrower scope (EU-flagged vessels
 *   only), creating leakage to less-regulated registries. The constraint
 *   demonstrates how a coordination mechanism (global emissions standard) can
 *   embed extraction through allocation rule design.
 *
 * KEY AGENTS:
 *   - Developing Nation Shipping Fleets: Primary victim (powerless/trapped) — operates older vessels, cannot exit, faces disproportionate per-tonne compliance costs
 *   - Small Independent Operators: Secondary victim (moderate/constrained) — high retrofit capital requirements, limited access to alternative fuel infrastructure, constrained route choices
 *   - Incumbent Large Shipowners: Primary beneficiary (institutional/arbitrage) — dominate efficient vessel market, receive windfall cap allocations, can fund fuel-switching, have arbitrage options
 *   - European Regulatory Bodies: Co-beneficiary and enforcer (powerful/arbitrage) — EU shipowners benefit from cap system, EU maintains regulatory authority, active enforcement by EU ports and member states
 *   - Coastal Communities: Secondary victim (moderate/constrained) — benefit from reduced shipping pollution but face port infrastructure costs and shipping diversion to less-regulated jurisdictions
 *   - International Maritime Organization: Institutional actor (institutional/arbitrage) — maintains formal regulatory authority but functionally captured; enforcement delegated to regional bodies
 *   - Green Shipping Coalition: Organized observers (organized/mobile) — support system as temporary scaffold with sunset through technology transition; have exit path via alternative regulation regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shipping_emissions_cap_allocation, 0.58).
domain_priors:suppression_score(shipping_emissions_cap_allocation, 0.62).
domain_priors:theater_ratio(shipping_emissions_cap_allocation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shipping_emissions_cap_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(shipping_emissions_cap_allocation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shipping_emissions_cap_allocation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shipping_emissions_cap_allocation, tangled_rope).
narrative_ontology:human_readable(shipping_emissions_cap_allocation, "Shipping Emissions Cap Allocation System").
narrative_ontology:topic_domain(shipping_emissions_cap_allocation, "environmental_policy/maritime_economics").

domain_priors:requires_active_enforcement(shipping_emissions_cap_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shipping_emissions_cap_allocation, incumbent_large_shipowners).
narrative_ontology:constraint_beneficiary(shipping_emissions_cap_allocation, european_regulatory_bodies).
narrative_ontology:constraint_victim(shipping_emissions_cap_allocation, developing_nation_fleets).
narrative_ontology:constraint_victim(shipping_emissions_cap_allocation, small_independent_operators).
narrative_ontology:constraint_victim(shipping_emissions_cap_allocation, coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATION SHIPPING FLEET (SNARE) — Cannot exit global shipping routes; trapped by capital constraints and vessel age. Must bear disproportionate compliance costs while operating older, less-efficient vessels. No alternative income source or mode switch possible. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL INDEPENDENT OPERATORS (TANGLED ROPE) — High barriers to exit: ship retrofitting is capital-intensive; alternative fuels require infrastructure not yet widely deployed; must maintain routes for economic viability. Cap system does coordinate global emissions reduction, reducing commons tragedy. But asymmetric extraction: smaller operators face steeper per-vessel costs than consolidated competitors. Benefits from coordination exist but are swamped by compliance burden.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT LARGE SHIPOWNERS (ROPE) — Primary beneficiaries of cap allocation system. Can achieve compliance through fleet efficiency improvements and fuel switching at lower per-tonne cost than competitors. Have arbitrage options: can trade excess caps, invest in zero-emission vessels, exit unprofitable routes. Experience system as coordination of collective action problem with incidental benefit (cap windfall if efficient). Net positive extraction running toward this agent.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN REGULATORY BODIES / EU SHIPPING INDUSTRY (TANGLED ROPE) — Primary enforcer and beneficiary. EU shipowners dominate flag registries and have capital to comply. Regulatory bodies benefit from coordination narrative (climate action) while extracting rents through cap allocation privileges. Active enforcement required to maintain asymmetry. Both coordination (global emissions standard) and extraction (disproportionate benefit to northern operators) present.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COASTAL COMMUNITIES AND PORT OPERATORS (TANGLED ROPE) — Face coordination benefit (reduced local air pollution from shipping) alongside extraction (port infrastructure costs shift to local governments; shipping diversion to less-regulated ports increases in peripheral regions). Some capacity to influence through port authority decisions but constrained by international shipping requirements.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL GREEN SHIPPING COALITION (SCAFFOLD) — Organized NGO and progressive shipowner coalition. Sees cap system as temporary coordination mechanism with sunset: as alternative fuel infrastructure matures and zero-emission vessel technology commercializes, the cap-and-trade architecture should transition to performance mandates. Currently accepts extraction burden as necessary short-term cost for system legitimacy. Has exit path through emerging regulatory regimes. Sunset clause rationale: 10-15 year horizon for alternative fuels infrastructure to mature.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL MARITIME ORGANIZATION (PITON) — Formally responsible for global shipping standards but politically captured by shipping industry. The cap allocation system is performative: high theater of international cooperation, but actual enforcement delegated to regional bodies (EU, China) with conflicting incentives. IMO maintains narrative authority while abdicating functional enforcement. Theater derives from consensus requirement and flag-state discretion undermining cap integrity.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From civilizational/universal lens, some carbon budget constraint is thermodynamically immutable: global shipping emissions cannot stay constant while global cargo volumes grow. Any allocation system must triage cargo or carbon. This perspective risks naturalizing the specific cap allocation regime as inherent to decarbonization, obscuring that alternative allocation schemes (per-nautical-mile, per-tonne-cargo, per-crew) would produce different distributions of extraction.
constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shipping_emissions_cap_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shipping_emissions_cap_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shipping_emissions_cap_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shipping_emissions_cap_allocation, TR),
    TR >= 0.70.

:- end_tests(shipping_emissions_cap_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The cap allocation system genuinely reduces global commons tragedy by setting binding emissions ceilings, justifying baseline ε contribution. But the allocation rule—based on historical emissions—systematically advantages operators with efficient fleets and penalizes operators locked into older tonnage. Compliance costs are non-linear with operator scale, creating scale economies that benefit large operators. The 0.58 value reflects that extraction is structural (baked into allocation formula) but not total (alternative fuel pathways exist, albeit expensive). Suppression (0.62): Moderate-high. Developing operators face suppression through capital constraints (can't afford retrofit), path dependence (locked into older vessels), and global route dependence (shipping is essential, can't relocate). IMO's consensus-based governance and flag-state discretion suppress enforcement, allowing leakage. But suppression is not total—ports and flag states have enforcement levers, and some operators can escape through technology access or route optimization. Theater ratio (0.48): Moderate. IMO maintains formal narrative authority, which is performative (consensus requirement, flag-state escape hatches). But EU implementation shows functional content—actual enforcement through port state control and flag control. Theater has increased slightly as IMO's role has become more ceremonial relative to regional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Developing-nation victims perceive snare (trapped, no exit, pure extraction). Incumbent beneficiaries perceive rope (mobile, arbitrage options, coordination benefit). Moderate operators perceive tangled rope (some exit, mixed coordination and extraction). Green coalition perceives scaffold (organized, mobile, sunset visible). IMO perceives piton (formal authority, functional degradation). Analytical observer risks mountain (naturalizing contingent allocation rule). The perspectival gap is maximized between trapped and arbitrage actors—same constraint, opposite experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Large shipowners (d ≈ 0.15): Beneficiaries with arbitrage exit. Low directionality → low f(d) ≈ 0.0 → experienced χ is suppressed. Developing operators (d ≈ 0.92): Victims with trapped exit. High directionality → high f(d) ≈ 1.35 → experienced χ is amplified. Moderate operators (d ≈ 0.60): Mixed victims/partial beneficiaries with constrained exit. Mid-range d → mid-range f(d) ≈ 0.70 → experienced χ is moderate. The global scope (σ=1.2) scales all χ values upward, reflecting that cap evasion and arbitrage operate at international scale, making enforcement harder and extraction easier.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing the genuine coordination function (emissions ceiling) from the contingent extraction mechanism (allocation rule favoring incumbents). The claim 'this is necessary coordination' is true; the claim 'this specific allocation rule is necessary' is false. Alternative allocation schemes (per-nautical-mile, per-tonne-cargo, per-crew, grandfathering with transfer windows) would maintain coordination while changing extraction distribution. The tangled-rope classification correctly asserts that the constraint contains BOTH coordination and extraction, not that one can be eliminated without the other. The scaffold perspective shows the transition path: as technology maturity increases, the cap system can phase into performance mandates, reducing the extraction component without losing the coordination function. The false mountain classification from the analytical observer is a diagnostic signal: claims that 'allocation is necessary to science' are often cover stories for institutional preferences about whose carbon counts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_year_gaming,
    'How sensitive is extraction distribution to the choice of baseline year for cap allocation?',
    'Counterfactual cap allocation under alternative baseline years (2018, 2019, 2020, 2021); analysis of how fleet composition shifts affect which operators receive surplus or deficit caps',
    'If baseline sensitivity is high: the cap allocation is meaningfully arbitrary, and extraction is contingent on political choice, not natural law. If low: baseline choice has only marginal effect on distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_year_gaming, empirical, 'Sensitivity of cap allocation to baseline year choice').

omega_variable(
    fuel_switching_cost_asymmetry,
    'Do compliance costs scale linearly with vessel age and flag state, or do structural factors create non-linear cost curves that systematically advantage large operators?',
    'Cost-benefit analysis of fuel-switching technologies (LNG retrofit, biofuel adoption, zero-emission propulsion) disaggregated by vessel age, size, and operator capital access; identification of scale thresholds where per-tonne compliance cost drops',
    'If non-linear with strong scale effects: extraction mechanism is structural (small operators face step-change cost increase). If linear: extraction is recompensable through financial transfers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fuel_switching_cost_asymmetry, empirical, 'Cost asymmetry in fuel-switching across vessel classes').

omega_variable(
    leakage_rate_observability,
    'What proportion of shipping traffic diverts to unregulated or less-regulated jurisdictions (flag-hopping, transshipment gaming), and how does this leakage correlate with operator size and capital access?',
    'Tracking of vessel flag changes and transshipment hub utilization pre- and post-cap implementation; analysis of diversion patterns by operator size and geographic origin',
    'If leakage is high and concentrated among developing operators: cap system fails functionally (emissions reduction illusory) and extraction is severe (costs imposed without benefit). If leakage is low and distributed: system is functioning, extraction is moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leakage_rate_observability, empirical, 'Leakage rate and distribution of flag-hopping behavior').

omega_variable(
    alternative_fuel_infrastructure_readiness,
    'What is the trajectory of alternative fuel (green methanol, ammonia, hydrogen) infrastructure maturation, and does it align with the scaffold perspective''s 10-15 year sunset horizon?',
    'Infrastructure development tracking: bunkering port buildout, production capacity scaling, cost curve learning rates; comparison with infrastructure needs for full fleet transition',
    'If infrastructure matures ahead of schedule: scaffold sunset is realistic, extraction timeline is compressible. If infrastructure lags: sunset is aspirational, extraction persists longer than acknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fuel_infrastructure_readiness, empirical, 'Maturation timeline of alternative fuel infrastructure').

omega_variable(
    redistribution_mechanism_effectiveness,
    'Are the financial transfers from cap revenue actually flowing to developing-nation operators for compliance support, or do institutional capture and transaction costs prevent redistribution?',
    'Tracking of cap trading revenue and how it is allocated; measurement of funds actually reaching compliance assistance programs for small and developing operators; institutional analysis of transfer bureaucracies',
    'If redistribution is effective: extraction is partially mitigated and system moves toward rope. If redistribution fails: extraction is confirmed and system remains snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_mechanism_effectiveness, empirical, 'Actual redistribution of cap trading revenue to developing operators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shipping_emissions_cap_allocation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shec_tr_t0, shipping_emissions_cap_allocation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shec_tr_t3, shipping_emissions_cap_allocation, theater_ratio, 3, 0.41).
narrative_ontology:measurement(shec_tr_t6, shipping_emissions_cap_allocation, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(shec_be_t0, shipping_emissions_cap_allocation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shec_be_t3, shipping_emissions_cap_allocation, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(shec_be_t6, shipping_emissions_cap_allocation, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shipping_emissions_cap_allocation, resource_allocation).
narrative_ontology:affects_constraint(shipping_emissions_cap_allocation, maritime_fuel_decarbonization_infrastructure).
narrative_ontology:affects_constraint(shipping_emissions_cap_allocation, port_state_control_enforcement).
narrative_ontology:affects_constraint(shipping_emissions_cap_allocation, flag_registry_competition).

% DUAL FORMULATION NOTE:
% The shipping emissions cap allocation is downstream of the broader maritime decarbonization constraint but represents a distinct structural mechanism. The upstream constraint has ε reflecting the physical impossibility of maintaining current emissions while growing cargo; this constraint has ε reflecting the institutional choice to allocate caps based on historical emissions rather than other possible criteria. The two constraints are linked: the cap system's extractiveness depends on alternative fuel infrastructure maturity (affects_constraints entry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shipping_emissions_cap_allocation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
