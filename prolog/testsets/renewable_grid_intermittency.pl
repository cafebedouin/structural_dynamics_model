% ============================================================================
% CONSTRAINT STORY: renewable_grid_intermittency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renewable_grid_intermittency, []).

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
 *   constraint_id: renewable_grid_intermittency
 *   human_readable: Renewable Grid Intermittency and Grid Coordination
 *   domain: energy/infrastructure
 *
 * SUMMARY:
 *   Renewable grid intermittency creates a structural tension between the
 *   technical variability of weather-dependent generation and the requirement
 *   for real-time grid balance. This is not primarily a physics problem
 *   (renewable sources cannot be made to generate on demand) but a
 *   coordination and market design problem: how to distribute the costs of
 *   balancing variable supply across agents, which agents bear the balancing
 *   burden, and who captures the rents from providing balancing services. The
 *   constraint exhibits tangled rope structure: there is genuine coordination
 *   function (variable renewable supply is valuable for decarbonization) and
 *   asymmetric extraction (balancing costs are concentrated on powerless
 *   agents like grid operators and low-income consumers, while benefits
 *   accrue to organized actors like fossil fuel operators and renewable
 *   producers). The extractiveness trajectory (0.32 → 0.58 over 15 years)
 *   reflects increasing renewable penetration and deepening imbalance between
 *   variable supply and balancing infrastructure. The theater ratio
 *   trajectory (0.22 → 0.38) reflects that legacy dispatch protocols and
 *   reserve margin calculations are becoming decoupled from actual grid
 *   physics as renewable penetration exceeds the design assumptions of
 *   traditional grid planning. The constraint has a clear sunset logic:
 *   battery storage cost declines, demand-side flexibility technologies
 *   mature, and transmission capacity expands, all reducing the structural
 *   need for dispatchable fossil fuel backup. The scaffold perspective shows
 *   a path where organized agents are building alternative balancing
 *   mechanisms (distributed storage, demand response, smart grids) that will
 *   render the current intermittency constraint much less extractive within
 *   10-20 years.
 *
 * KEY AGENTS:
 *   - Grid Reliability: Primary victim (powerless/trapped) — abstract collective good bearing full cost of intermittency-driven instability; no self-advocacy mechanism
 *   - Low-Income Electricity Consumers: Primary victim (powerless/trapped) — disproportionately bear blackout costs and ancillary service fees; unable to self-generate or switch providers
 *   - Distributed Prosumers and Microgrids: Secondary victim (moderate/constrained) — face interconnection barriers and curtailment requirements; also benefit from grid coordination function
 *   - Renewable Energy Producers: Organized beneficiary (organized/mobile) — capture renewable-generated electricity sales while extracting grid balancing costs; have geographic and policy arbitrage options
 *   - Natural Gas and Coal Operators: Powerful beneficiary (powerful/arbitrage) — capture rents from guaranteed backup demand; experience intermittency as market-enriching
 *   - Grid Operators and Transmission Companies: Institutional beneficiary (institutional/arbitrage) — extract ancillary service rents while solving technical balancing problem; have strong agency
 *   - Storage and Demand Response Coalition: Organized solver (organized/constrained) — building alternative balancing mechanisms with near-term upward constraints but generational sunset trajectory
 *   - Legacy Dispatch Protocols: Institutional artifact (institutional/arbitrage) — performative regulatory framework persisting through inertia despite being sub-optimal for renewable-heavy grids
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renewable_grid_intermittency, 0.58).
domain_priors:suppression_score(renewable_grid_intermittency, 0.52).
domain_priors:theater_ratio(renewable_grid_intermittency, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renewable_grid_intermittency, extractiveness, 0.58).
narrative_ontology:constraint_metric(renewable_grid_intermittency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(renewable_grid_intermittency, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renewable_grid_intermittency, tangled_rope).
narrative_ontology:human_readable(renewable_grid_intermittency, "Renewable Grid Intermittency and Grid Coordination").
narrative_ontology:topic_domain(renewable_grid_intermittency, "energy/infrastructure").

domain_priors:requires_active_enforcement(renewable_grid_intermittency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renewable_grid_intermittency, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(renewable_grid_intermittency, coal_and_natural_gas_operators).
narrative_ontology:constraint_beneficiary(renewable_grid_intermittency, grid_operators).
narrative_ontology:constraint_victim(renewable_grid_intermittency, grid_reliability).
narrative_ontology:constraint_victim(renewable_grid_intermittency, distributed_prosumers).
narrative_ontology:constraint_victim(renewable_grid_intermittency, low_income_electricity_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRID RELIABILITY (SNARE) — The abstract collective good of grid stability cannot exit the intermittency problem and bears full cost of blackout risk, frequency instability, and cascading failures. Grid reliability is powerless, trapped in the physical constraint, with no self-advocacy mechanism. Maximum experienced extraction — the commons is sacrificed to manage the coordination problem.
constraint_indexing:constraint_classification(renewable_grid_intermittency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOW-INCOME CONSUMERS (SNARE) — Trapped by infrastructure dependency and inability to self-generate or switch providers. Bear disproportionate share of grid instability costs (blackouts, voltage fluctuations damaging appliances) and ancillary service fees passed through bills. No exit capacity; extraction concentrated on powerless agents.
constraint_indexing:constraint_classification(renewable_grid_intermittency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DISTRIBUTED PROSUMERS (TANGLED ROPE) — Constrained by technical requirements for grid interconnection, regulatory barriers, and capital costs. But also benefit from coordination mechanisms: utility interconnection enables retail electricity sales, net metering provides income, and grid access provides stability buffer. Significant extraction (curtailment requirements, grid service fees) but also genuine coordination benefit.
constraint_indexing:constraint_classification(renewable_grid_intermittency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GRID OPERATORS (ROPE) — Institutional beneficiaries with arbitrage options (demand response contracts, frequency ancillary markets, renewable curtailment payments). Experience the constraint as a coordination problem they solve via real-time balancing, reserve margins, and dispatch algorithms. Net beneficiaries — extract rents through ancillary service pricing while solving the technical problem. Low effective extraction from their perspective because they have agency and profitable exit routes.
constraint_indexing:constraint_classification(renewable_grid_intermittency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: RENEWABLE PRODUCERS (TANGLED ROPE) — Organized collective with mobile options (geographic arbitrage via transmission corridors, subsidy/tax credit pursuit across jurisdictions, storage investment). But constrained by intermittency forecasting requirements, curtailment orders, and grid service charges. Mixed experience: coordination function (variable renewable supply is valuable) alongside asymmetric extraction (bearing real-time balancing costs).
constraint_indexing:constraint_classification(renewable_grid_intermittency, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: FOSSIL FUEL OPERATORS (ROPE) — Powerful institutional actors with strong arbitrage: renewable intermittency creates guaranteed demand for dispatchable backup generation. Extract rents through peaking-plant operation, capacity payments, and fast-ramping services. Experience constraint as coordination mechanism securing their market position. Effectively negative extraction from their perspective — the intermittency enriches them.
constraint_indexing:constraint_classification(renewable_grid_intermittency, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: STORAGE/DEMAND RESPONSE (SCAFFOLD) — Organized agents (battery manufacturers, demand response aggregators, EV charging networks, industrial load flexibility) see intermittency as a temporary coordination challenge with structural sunset: battery costs are declining, EV penetration is rising, smart grid technologies are scaling, and industrial demand-side flexibility is improving. Constrained by current technology and regulatory frameworks, but with clear exit path as storage and flexibility scale. Extraction is temporary — expected to decline sharply within 10-15 years as storage dominates balancing function.
constraint_indexing:constraint_classification(renewable_grid_intermittency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: LEGACY DISPATCH PROTOCOLS (PITON) — Traditional grid planning and dispatch rules (N-1 contingency analysis, fixed reserve margins, synchronous generator inertia requirements) are increasingly performative as renewable penetration rises. The protocols persist through regulatory inertia and system operator familiarity despite being neither optimally functional for renewable-heavy systems nor replaced by newer approaches. Theater ratio near 0.5 (traditional reserve calculations are becoming decoupled from actual grid stress).
constraint_indexing:constraint_classification(renewable_grid_intermittency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, intermittency is a structural feature of weather-dependent generation: solar output varies by cloud cover and solar angle, wind output varies by wind speed, both have diurnal/seasonal patterns. These are immutable physical constraints on the renewable energy source itself. However, the structural data shows that intermittency is NOT a mountain — it is a tangled_rope coordination problem with extractive layering. The 'inherent to renewables' framing naturalizes what is actually a contingent grid architecture and market design choice.
constraint_indexing:constraint_classification(renewable_grid_intermittency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renewable_grid_intermittency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renewable_grid_intermittency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renewable_grid_intermittency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(renewable_grid_intermittency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(renewable_grid_intermittency, TR),
    TR >= 0.70.

:- end_tests(renewable_grid_intermittency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The intermittency constraint generates real balancing costs (reserve margins, fast-ramping capacity, frequency regulation services) that are not inherent to the renewable energy source itself but to the grid architecture and market design. Fossil fuel operators capture rents from providing balancing services (capacity payments, energy market scarcity pricing during high-demand/low-wind periods), while the costs of balancing variability are spread across powerless agents (grid reliability, low-income consumers, distributed prosumers). The extractiveness is not as extreme as a pure snare (0.72+) because the coordination function is genuine — renewables are valuable for decarbonization — and because organized actors (renewable producers, storage developers) are building alternative solutions. The trajectory from 0.32 to 0.58 reflects deepening imbalance as renewable penetration increases without commensurate expansion of balancing infrastructure, creating temporary extraction layering. Suppression (0.52): Moderate-high. Barriers to exit from the grid dependency include physical infrastructure lock-in, regulatory interconnection requirements, and capital cost barriers to distributed generation. But suppression is not extreme (≥0.60) because some exit options exist: distributed prosumers can partially self-generate, demand response can reduce peak load exposure, and transmission can partially smooth geographic variability. Theater ratio (0.38): Moderate-low. Legacy reserve margin calculations and dispatch protocols have some performative content (traditional N-1 contingency analysis becomes harder to apply with variable renewable supply), but the grid balancing function itself is genuine and necessary. The theater reflects outdated regulatory frameworks (capacity auction design, ancillary service definitions) that persist despite misalignment with renewable-dominant systems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the powerless victim (snare: 'we are trapped in blackout risk') and the fossil fuel operator (rope: 'this is a lucrative coordination problem') is acute. The beneficiary sees coordination; the victim sees extraction. The grid operator sees technical challenge (rope); the low-income consumer sees cost burden (snare). The renewable producer sees mixed opportunity (tangled rope — they capture sales but bear balancing penalties); the storage developer sees temporary challenge with sunset (scaffold — their technology will replace the old constraint). This multiperspectival structure is diagnostic: if all perspectives produced the same classification, the constraint would be misspecified. The perspectival gaps reveal the real structural tensions: powerless agents bearing costs that organized actors profit from, temporary institutional arrangements (legacy protocols) persisting alongside emerging alternatives (storage, demand response), and false naturalization of contingent constraints as immutable laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from the agent's structural position relative to the extraction flow. Powerless victims (grid reliability, low-income consumers) trapped with no exit options have d ≈ 0.95 → f(d) ≈ 1.42, experiencing maximum effective extraction chi. Distributed prosumers constrained with partial exit options have d ≈ 0.65 → f(d) ≈ 1.00, experiencing moderate extraction. Renewable producers organized with geographic/policy mobility have d ≈ 0.40 → f(d) ≈ 0.40, experiencing moderate extraction despite being partial victims (they bear grid service costs). Natural gas operators with arbitrage options and beneficiary status have d ≈ 0.05 → f(d) ≈ -0.12, experiencing negative effective extraction (the constraint enriches them). Grid operators with institutional power and arbitrage have d ≈ 0.10 → f(d) ≈ -0.02, experiencing minimal or negative extraction. The scope modifier σ(S) scales regional constraints (σ=0.9) slightly less than national (σ=1.0), reflecting that intermittency is partially smoothable through geographic diversification of renewable sources. At continental scope (σ=1.1), intermittency extraction amplifies — larger systems have harder timing mismatches between supply and demand.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that intermittency is NOT a natural law (mountain) but a contingent coordination problem with extractive layering (tangled rope). The false summit appears when observers naturalize the constraint as 'renewables are inherently intermittent, so intermittency is immutable.' This is true as physics but false as economics: the constraint is not the intermittency of the renewable source but the choice to use grid architecture and market design that concentrates balancing costs on powerless agents. Alternative architectures (distributed storage, demand response, geographic smoothing via transmission) are technically feasible and becoming economically viable. The structure is tangled rope, not mountain: genuine coordination function (renewables are valuable) alongside asymmetric extraction (balancing costs concentrated on powerless agents). The scaffold perspective confirms the sunset: battery costs are declining exponentially, storage will become the primary balancing mechanism, and the current extraction mechanism will collapse as alternatives scale. The piton perspective reveals that legacy protocols are becoming decoupled from actual grid physics, indicating institutional inertia rather than functional necessity. The snare perspective from powerless agents is the most diagnostically important: it shows who actually bears the costs and has no voice in the constraint design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_cost_trajectory_threshold,
    'At what battery cost ($/kWh) does storage-dominant balancing become economically superior to dispatchable fossil fuel backup, triggering the scaffold sunset?',
    'Technology trajectory analysis; comparative levelized cost of storage vs natural gas peaking plants at 2030, 2040, 2050 time points',
    'If storage costs fall below threshold before 2035: scaffold sunset is real and extraction timeline collapses. If not until post-2045: temporary extraction persists through mid-century as legacy dispatchable generation locks in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(storage_cost_trajectory_threshold, empirical, 'Battery cost threshold for dispatchable generation replacement').

omega_variable(
    transmission_network_bottleneck,
    'Is renewable intermittency primarily a local balancing problem or a transmission infrastructure problem? Do high-capacity transmission corridors connecting geographically distributed renewable sites reduce intermittency sufficiently to change the constraint classification?',
    'System simulation analysis: compare grid stability metrics with current transmission capacity vs. scenarios with 2x or 3x transmission capacity and geographic smoothing',
    'If transmission-solvable: intermittency reclassifies from snare/tangled_rope to rope for most agents (coordination problem only, no extraction). If not: intermittency is irreducible, and storage becomes mandatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_network_bottleneck, empirical, 'Whether transmission expansion can resolve intermittency').

omega_variable(
    dispatchable_extraction_asymmetry,
    'Are fossil fuel operators genuinely capturing rents from intermittency-driven backup demand, or are capacity payment regulations and competitive wholesale markets reducing those rents to approximate competitive levels?',
    'Financial analysis of fossil fuel operator returns on capacity during high-renewable penetration periods; comparison of actual revenue streams vs. competitive margin benchmarks',
    'If capturing significant rents: rope perspective confirmed (net beneficiaries). If rents are competed away: fossil operators are also constrained, reclassifying to constrained rather than arbitrage exit, and their perspective becomes tangled_rope rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchable_extraction_asymmetry, empirical, 'Whether dispatchable operators capture intermittency rents').

omega_variable(
    demand_response_participation_ceiling,
    'What fraction of total grid load can realistically be made flexible via demand response, dynamic pricing, and load shifting without exceeding consumer willingness-to-pay or industrial competitiveness constraints?',
    'Empirical pilot program data from demand response aggregators; economic modeling of price-elasticity of demand at different end-use categories',
    'If ceiling is high (>40% of load): demand-side flexibility + storage can largely solve intermittency, confirming scaffold sunset. If ceiling is low (<15%): demand-side flexibility is supplementary, and other solutions (storage, transmission, curtailment) are mandatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_response_participation_ceiling, empirical, 'Realistic fraction of grid load that can become flexible').

omega_variable(
    cascading_failure_risk_metrics,
    'How has grid cascading failure risk (probability and impact of multi-state blackouts) actually changed as renewable penetration has increased? Is risk increasing, stable, or declining?',
    'Historical grid stability data; probabilistic risk assessment of major interconnects; correlation analysis between renewable penetration and NERC/regional blackout incident frequency',
    'If risk is rising: suppression metric may be understated, constraint may reclassify upward (higher snare proportion). If risk is stable or declining: suppression is overstated, extraction is lower, constraint reclassifies downward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cascading_failure_risk_metrics, empirical, 'Actual cascading failure risk trend with renewable penetration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renewable_grid_intermittency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rgi_tr_t0, renewable_grid_intermittency, theater_ratio, 0, 0.22).
narrative_ontology:measurement(rgi_tr_t5, renewable_grid_intermittency, theater_ratio, 5, 0.3).
narrative_ontology:measurement(rgi_tr_t10, renewable_grid_intermittency, theater_ratio, 10, 0.38).
narrative_ontology:measurement(rgi_tr_t15, renewable_grid_intermittency, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(rgi_be_t0, renewable_grid_intermittency, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rgi_be_t5, renewable_grid_intermittency, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rgi_be_t10, renewable_grid_intermittency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rgi_be_t15, renewable_grid_intermittency, base_extractiveness, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renewable_grid_intermittency, resource_allocation).
narrative_ontology:affects_constraint(renewable_grid_intermittency, fossil_fuel_lock_in).
narrative_ontology:affects_constraint(renewable_grid_intermittency, grid_capacity_investment_cycles).
narrative_ontology:affects_constraint(renewable_grid_intermittency, electricity_affordability_regressive_impact).

% DUAL FORMULATION NOTE:
% Renewable grid intermittency is a cluster constraint with multiple decomposable sub-constraints: (1) solar/wind variability (physics-level, ε ≈ 0.08, mountain) vs. (2) grid balancing market design (institutional, ε ≈ 0.58, tangled_rope) vs. (3) fossil fuel backup economics (rent extraction, ε ≈ 0.45, rope from beneficiary perspective). This story focuses on the market design and extraction mechanism (#2). Upstream physics constraint is inert; downstream impacts on fossil lock-in and consumer affordability are separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renewable_grid_intermittency, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
