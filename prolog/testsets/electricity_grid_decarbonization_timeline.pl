% ============================================================================
% CONSTRAINT STORY: electricity_grid_decarbonization_timeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electricity_grid_decarbonization_timeline, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electricity_grid_decarbonization_timeline
 *   human_readable: Electricity Grid Decarbonization Timeline Constraint
 *   domain: energy_policy/infrastructure/climate
 *
 * SUMMARY:
 *   The electricity grid decarbonization timeline constraint structures the
 *   political economy of energy transition across most developed economies.
 *   Nations have committed to net-zero emissions by 2050, with intermediate
 *   targets (EU 2030, US 2035 for power sector), but actual decarbonization
 *   rates lag targets by 5-10 years. This gap is not primarily
 *   thermodynamic—modern grid simulations demonstrate that 80%+ renewable
 *   grids are technically feasible with storage and demand response. Instead,
 *   the gap is extractive: incumbent fossil fuel generators, natural gas
 *   infrastructure operators, and grid operators benefit from slow timelines
 *   that extend asset life, delay competition, and preserve regulatory
 *   arrangements. Renewable energy developers and climate mitigation goals
 *   bear costs through delayed markets, interconnection barriers, and
 *   transmission bottlenecks. The constraint exhibits all features of tangled
 *   rope: genuine grid coordination requirements coexist with asymmetric
 *   extraction. The coal plant retirement ritual (piton perspective) shows
 *   theater_ratio rising from 0.48 to 0.72 over 15 years—announced
 *   retirements are repeatedly delayed, creating performative transition
 *   narratives that substitute for actual decarbonization. The distributed
 *   energy coalition (scaffold perspective) sees a sunset path: battery costs
 *   have fallen 90% in 15 years; at current trends, distributed storage
 *   reaches grid-scale cost parity by 2035-2040, making centralized fossil
 *   baseload economically obsolete. The thermodynamic natural law view is a
 *   false summit—the binding constraint is political economy, not physics.
 *
 * KEY AGENTS:
 *   - Incumbent Fossil Fuel Generators: Primary beneficiaries (institutional/arbitrage) — capture value through extended asset life, capacity payments, and delayed competition
 *   - Natural Gas Infrastructure Operators: Primary beneficiaries (institutional/arbitrage) — benefit from framing gas as 'transition fuel' and building long-lived infrastructure
 *   - Climate Mitigation Goals: Primary victim (powerless/trapped) — scientific and political commitments cannot exit; delay compounds exponentially in required decarbonization speed
 *   - Renewable Energy Developers: Secondary victim (moderate/constrained) — face interconnection delays, transmission bottlenecks, curtailment; also benefit from decarbonization mandates that create market
 *   - Grid Operators: Institutional gatekeeper (organized/constrained) — enforce reliability standards that protect fossil generation; constrained by regulatory framework
 *   - Distributed Energy + Storage Coalition: Organized disruptors (organized/mobile) — building alternative architectures with visible sunset path as costs collapse
 *   - Energy Ratepayers: Diffuse victim (powerless/trapped) — bear costs of delayed transition through higher electricity prices from extended coal/gas operation and inefficient infrastructure
 *   - Coal Community Workers: Secondary victim (powerless/trapped) — community economic lock-in compounds individual retirement decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electricity_grid_decarbonization_timeline, 0.58).
domain_priors:suppression_score(electricity_grid_decarbonization_timeline, 0.62).
domain_priors:theater_ratio(electricity_grid_decarbonization_timeline, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electricity_grid_decarbonization_timeline, extractiveness, 0.58).
narrative_ontology:constraint_metric(electricity_grid_decarbonization_timeline, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(electricity_grid_decarbonization_timeline, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electricity_grid_decarbonization_timeline, tangled_rope).
narrative_ontology:human_readable(electricity_grid_decarbonization_timeline, "Electricity Grid Decarbonization Timeline Constraint").
narrative_ontology:topic_domain(electricity_grid_decarbonization_timeline, "energy_policy/infrastructure/climate").

domain_priors:requires_active_enforcement(electricity_grid_decarbonization_timeline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electricity_grid_decarbonization_timeline, incumbent_fossil_fuel_generators).
narrative_ontology:constraint_beneficiary(electricity_grid_decarbonization_timeline, natural_gas_infrastructure_operators).
narrative_ontology:constraint_beneficiary(electricity_grid_decarbonization_timeline, grid_stability_operators).
narrative_ontology:constraint_victim(electricity_grid_decarbonization_timeline, climate_mitigation_goals).
narrative_ontology:constraint_victim(electricity_grid_decarbonization_timeline, renewable_energy_developers).
narrative_ontology:constraint_victim(electricity_grid_decarbonization_timeline, energy_ratepayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE MITIGATION COMMITMENTS (SNARE) — Nations and scientific bodies committed to net-zero targets by 2050 face a structural trap: the decarbonization timeline is physically constrained by grid infrastructure lifespans (30-40 years for fossil plants, 20-30 for transmission), yet political economy pressures systematically extend asset life through license renewal, retrofitting, and reliability arguments. The victims cannot exit—the climate commitment is existential. Maximum extraction: delay compounds in time, making later action exponentially more costly.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY DEVELOPERS (TANGLED ROPE) — These agents both benefit from decarbonization mandates (which create market demand) and bear extraction through grid interconnection delays, transmission bottlenecks, and curtailment requirements. They have moderate agency through project development and regulatory advocacy, but face real constraints: capital requirements, permitting complexity, and grid operator gatekeeping. The constraint is hybrid—coordination function (integrating renewables into grid) exists alongside asymmetric extraction (developers bear integration costs while fossil generators receive capacity guarantees).
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOSSIL FUEL GENERATORS (ROPE) — From their perspective, the decarbonization timeline is a coordination mechanism that structures market transitions. They benefit from stranded-asset compensation, extended operation windows, capacity payment guarantees, and natural gas transition pathways. They experience the constraint as beneficial negotiation—extracting value through claims of grid reliability requirements and transition smoothness. Exit options include asset sales, geographic arbitrage to deregulated markets, or diversification into gas infrastructure.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: GRID OPERATOR CONSORTIA (TANGLED ROPE) — System operators coordinate legitimate technical requirements (ramping, reserve capacity, frequency stability) while also enforcing incumbent preferences through interconnection standards and dispatch rules. These organizations have coordination function (genuine grid stability requirements exist) but also extract through gatekeeping: slow permitting, restrictive technical standards, and reliability arguments that protect fossil generation. Constrained exit—operators cannot simply adopt alternative rules without regulatory authorization.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DISTRIBUTED ENERGY + STORAGE COALITION (SCAFFOLD) — Organized actors (battery manufacturers, distributed solar providers, microgrid developers) are building alternative decentralized architectures that bypass traditional grid operator gatekeeping. They see the centralized decarbonization timeline as a temporary coordination failure with sunset: as battery costs collapse and distributed resources mature, the traditional transmission + fossil baseload model becomes economically obsolete. Timeline to sunset: 15-20 years as distributed storage reaches grid-scale economics. Temporary suppression (high upfront costs, regulatory barriers) is tolerated because exit path is visible.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COAL PLANT RETIREMENT RITUAL (PITON) — Individual coal plant retirements are increasingly theatrical: announced retirements are delayed, operations are extended through temporary waivers, and decommissioning timelines are indefinitely postponed. The ritual of 'planned transition' persists through institutional inertia (regulatory frameworks built around coal capacity, union agreements, community economic dependence) despite the economic reality that new coal plants are uncompetitive. Piton derives from theater_ratio: the performative content of retirement planning exceeds its functional content. The constraint remains due to sunk-cost narratives and political path dependence, not operational necessity.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a purely thermodynamic perspective, the decarbonization timeline is constrained by physical limits: renewable generation is intermittent (requiring storage or dispatchable backup), dispatchable zero-carbon sources are limited (nuclear, geothermal, hydroelectric), and energy storage has round-trip efficiency losses (~70-85% for lithium batteries). This view treats the timeline constraint as emergent from thermodynamic reality. However, structural data (suppression, theatrical retirement planning, incumbent gatekeeping) reveals this as false summit: the binding constraint is not thermodynamics but political economy. Contemporary storage + renewable economics demonstrate technical feasibility for 80%+ decarbonization by 2035 in many regions; delay is extractive, not thermodynamic.
constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electricity_grid_decarbonization_timeline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electricity_grid_decarbonization_timeline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electricity_grid_decarbonization_timeline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electricity_grid_decarbonization_timeline, TR),
    TR >= 0.70.

:- end_tests(electricity_grid_decarbonization_timeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction mechanism is timeline delay—each 5-year postponement of fossil plant retirement extends sunk-cost infrastructure, forces higher costs in later rapid decarbonization, and generates $100B+ in windfall value to incumbent generators (avoided early retirement, extended capacity payments). The extractiveness is not total because genuine grid stability requirements exist and some renewable deployment occurs even within delayed timelines. Suppression (0.62): High. Multiple mechanisms suppress alternatives: (1) Regulatory gatekeeping—grid operators control interconnection and dispatch rules; (2) Financial barriers—renewables face higher capital costs despite lower operating costs; (3) Incumbent lobbying—fossil generators fund regulatory capture and anti-renewable framing; (4) Sunk-cost narratives—existing infrastructure creates path dependence in planning. Suppression rises over time as incumbent interests consolidate and incumbent plants are retrofitted for extended life. Theater ratio (0.68): High. Coal plant retirement announcements are increasingly theatrical—lifespans are repeatedly extended through waivers and temporary operations. Grid reliability arguments provide a veneer of technical necessity, but comparative analysis (Denmark, California) shows high-renewable grids are stable. Decarbonization 'plans' proliferate without binding implementation timelines. The theater increases over the measurement interval as the gap between announced and actual retirements grows.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: incumbent generators see rope (beneficial coordination enabling transition), renewables see tangled rope (mixed coordination and extraction), climate goals see snare (locked in, no exit), grid operators see tangled rope (genuine technical coordination + incumbent gatekeeping), distributed energy coalition see scaffold (temporary problem with visible sunset), coal plant retirement sees piton (performative ritual), and analytical observer risks mountain (false summit). The perspectival gap is diagnostic: the beneficiary perspective (rope) and victim perspective (snare/trapped) diverge on the same structural facts. This divergence reveals that the constraint is not naturally coordinated—it is extractive political economy masquerading as technical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's position in the extraction flow. Incumbent generators are beneficiaries (d ≈ 0.10-0.20): they benefit from timeline delay and have high exit optionality (asset sales, geographic arbitrage to deregulated markets, gas conversion). Their effective extraction chi is negative—the constraint transfers value to them. Renewable developers are victims (d ≈ 0.60-0.70): they depend on decarbonization timelines for markets but face gatekeeping barriers; constrained exit (capital-intensive, location-specific). Climate goals are pure victims (d ≈ 0.95): maximum extraction—each year of delay increases decarbonization costs exponentially; no exit option. Grid operators occupy a hybrid position (d ≈ 0.40): they enforce rules that protect incumbents (extracting from renewables) while also coordinating genuine technical requirements (some benefit flows from stability coordination). Energy ratepayers are diffuse victims (d ≈ 0.85): they cannot organize collectively but bear cost of extended fossil operation. The analytical observer at civilizational scale risks d ≈ 0.72 (pure observer) but risks misclassifying the constraint as natural law rather than extractive political economy.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING MANDATROPHY: The classification cannot resolve between pure coordination and pure extraction by examining base properties alone because the constraint genuinely contains both. The mandatrophy resolves through perspectival composition: (1) Does the constraint solve a genuine collective action problem? YES—grid stability and phase-in of renewables present legitimate coordination challenges. (2) Does it distribute costs asymmetrically? YES—fossil generators benefit from timeline delay while climate goals and renewables bear costs. (3) Is there active enforcement maintaining the asymmetry? YES—regulatory gatekeeping, interconnection delays, and dispatch rules systematically favor incumbents. Therefore, tangled rope is the correct classification. The false mountain perspective (thermodynamic necessity) is detected through network contradiction: contemporary grid simulations show 80%+ renewable grids are physically feasible; the timeline constraint is enforced politically, not thermodynamically. The piton perspective (theatrical retirement) shows that the constraint's functional content has degraded relative to its performative content—a diagnostic sign of inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_requirement_authenticity,
    'How much of the claimed grid stability requirement for fossil fuel baseload is genuine technical necessity vs. incumbent-driven regulatory construct?',
    'Comparative analysis: blackout/stability incidents in high-renewable grids (Denmark 80% wind, California 50%+ renewables) vs fossil-dependent grids; controlled experiments with demand-side response and advanced storage; engineering studies on required reserve margins with modern control systems',
    'If genuine: suppression values rise (justified technical constraints); decarbonization timeline is physically constrained. If constructed: suppression values fall; timeline is extractive delay, not technical requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_requirement_authenticity, empirical, 'Whether grid stability claims reflect genuine or constructed requirements').

omega_variable(
    transmission_bottleneck_origin,
    'Is slow transmission buildout a natural consequence of engineering complexity and planning cycles, or an incumbent gatekeeping mechanism?',
    'Historical analysis of transmission approval timelines pre/post-deregulation; comparison of approval speed for fossil-plant-serving vs renewable-serving lines; regulatory process tracing for denials and delays; cost analysis of alternative faster-build technologies',
    'If natural: renewable extraction timeline is honest constraint. If gatekeeping: transmission delays are extractive tool; alternative technologies and faster processes could exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_bottleneck_origin, empirical, 'Whether transmission bottlenecks are technical or institutional').

omega_variable(
    distributed_storage_maturity_timeline,
    'Will distributed battery + solar architectures reach grid-scale cost parity and reliability with centralized generation fast enough to enable true decarbonization by 2035-2040?',
    'Tracking battery cost curves against grid-service price points; pilot projects demonstrating full-day renewable coverage with storage; market adoption rates in competitive regions (Australia, Chile, California)',
    'If yes: scaffold perspective is structurally sound, sunset is real. If no: centralized grid coordination remains necessary longer, timeline constraint is binding, decarbonization is slower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributed_storage_maturity_timeline, empirical, 'Timeline for distributed storage cost-competitiveness').

omega_variable(
    coal_community_economic_lock,
    'How much of coal plant lifecycle extension is driven by community economic lock-in vs incumbent leverage and rent-seeking?',
    'Comparative case studies of coal plant closures with/without just transition investment; political analysis of retirement delay justifications; tracking of community economic rebound in successful transition regions',
    'If lock-in dominant: suppression is justified; slower timeline needed for social transition. If incumbent rent-seeking dominant: suppression is extractive; faster timeline is feasible with adequate transition support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_community_economic_lock, empirical, 'Attribution of coal plant extension to economic lock-in vs incumbent leverage').

omega_variable(
    natural_gas_transition_trap,
    'Is natural gas framed as ''transition fuel'' a genuine bridge technology or a 30-year lock-in mechanism that re-entrench fossil infrastructure?',
    'Tracking of natural gas infrastructure buildout vs retirement timelines; comparison of actual gas-plant retirements to planned dates; economic analysis of gas stranded-asset risk if 2035-2040 decarbonization targets are met',
    'If genuine bridge: gas plants can retire on decarbonization timeline. If lock-in: gas infrastructure creates sunk-cost inertia, extending fossil generation 10-15 years beyond optimal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_gas_transition_trap, empirical, 'Whether natural gas serves as transition bridge or lock-in mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electricity_grid_decarbonization_timeline, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_decarbonize_tr_t0, electricity_grid_decarbonization_timeline, theater_ratio, 0, 0.48).
narrative_ontology:measurement(elec_decarbonize_tr_t5, electricity_grid_decarbonization_timeline, theater_ratio, 5, 0.58).
narrative_ontology:measurement(elec_decarbonize_tr_t10, electricity_grid_decarbonization_timeline, theater_ratio, 10, 0.68).
narrative_ontology:measurement(elec_decarbonize_tr_t15, electricity_grid_decarbonization_timeline, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(elec_decarbonize_be_t0, electricity_grid_decarbonization_timeline, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(elec_decarbonize_be_t5, electricity_grid_decarbonization_timeline, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(elec_decarbonize_be_t10, electricity_grid_decarbonization_timeline, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(elec_decarbonize_be_t15, electricity_grid_decarbonization_timeline, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electricity_grid_decarbonization_timeline, resource_allocation).
narrative_ontology:boltzmann_floor_override(electricity_grid_decarbonization_timeline, 0.18).
narrative_ontology:affects_constraint(electricity_grid_decarbonization_timeline, renewable_interconnection_bottleneck).
narrative_ontology:affects_constraint(electricity_grid_decarbonization_timeline, natural_gas_stranded_assets).
narrative_ontology:affects_constraint(electricity_grid_decarbonization_timeline, electricity_price_suppression).
narrative_ontology:affects_constraint(electricity_grid_decarbonization_timeline, coal_community_economic_lock).

% DUAL FORMULATION NOTE:
% The decarbonization timeline is upstream of multiple domain-specific constraints. Renewable interconnection bottlenecks are downstream—they depend on whether grid operators enforce gatekeeping rules. Natural gas infrastructure buildout is a parallel extraction mechanism with shared beneficiaries (fossil fuel operators). Coal community lock-in is a secondary victim mechanism amplifying suppression. Price suppression results from extended fossil generation displacing renewables. All are linked by incumbent extraction strategy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electricity_grid_decarbonization_timeline, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
