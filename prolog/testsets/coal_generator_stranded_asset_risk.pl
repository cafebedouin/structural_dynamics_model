% ============================================================================
% CONSTRAINT STORY: coal_generator_stranded_asset_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coal_generator_stranded_asset_risk, []).

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
 *   constraint_id: coal_generator_stranded_asset_risk
 *   human_readable: Coal Generator Stranded Asset Risk and Energy Transition Lock-In
 *   domain: energy_economics/regulatory_policy
 *
 * SUMMARY:
 *   Coal generator stranded asset risk creates a structural lock-in between
 *   incumbent utility companies, regulatory authorities, coal-dependent
 *   communities, and ratepayers that delays energy transition while
 *   concentrating costs on powerless agents. The constraint exhibits hybrid
 *   coordination-extraction dynamics: cost-of-service regulation genuinely
 *   solves the coordination problem of capital recovery and grid reliability
 *   investment, but the mechanism's structure systematically privileges
 *   capital-intensive coal infrastructure over flexible renewable
 *   alternatives. Stranded asset cost-sharing through ratepayer charges
 *   suppresses renewable deployment investment and extends coal plant
 *   operational lifespans beyond economic viability, delaying decarbonization
 *   while concentrating extraction on agents (climate system, future
 *   generations, renewable investors, low-income ratepayers) who cannot exit.
 *   The constraint's theater ratio (0.68) reflects that much institutional
 *   activity is narrative justification—'clean coal,' 'grid stability
 *   requirements,' 'baseload reliability'—rather than operational
 *   problem-solving. The extractiveness trajectory shows increasing
 *   extraction over the measurement interval (0.32 → 0.58) as renewable cost
 *   curves drop faster than regulatory frameworks adjust, widening the gap
 *   between competitive reality and protected coal economics.
 *
 * KEY AGENTS:
 *   - Coal Utility Company: Primary beneficiary (institutional/arbitrage) — cost-of-service regulation ensures full asset recovery through ratepayer charges; can lobby for regulatory protection and cost allocation
 *   - Coal-Dependent Community: Primary victim (powerless/trapped) — economically locked into coal consumption; local tax base and employment dependent on plant continued operation; cannot exit without severe local economic shock
 *   - Electricity Ratepayers: Secondary victim (moderate/constrained) — bear stranded cost recovery charges; exit paths limited (retail choice unavailable in most US jurisdictions, rooftop solar blocked by regulations, relocation economically unfeasible)
 *   - Renewable Energy Investors: Secondary victim (moderate/constrained) — face grid access barriers, transmission congestion, rate-setting that favors incumbent utilities; constrained but not trapped entry into market
 *   - Public Utility Commission: Institutional actor (organized/constrained) — subject to regulatory capture by incumbents while managing genuine coordination problems (grid reliability, capital recovery, rate stability); active enforcer of stranded asset recovery
 *   - Climate System and Future Generations: Victim (powerless/trapped) — cannot exit fossil fuel lock-in; bear concentrated costs of deferred decarbonization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — distinguishes physical decarbonization constraint (genuine mountain) from regulatory stranded asset mechanism (contingent institutional arrangement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coal_generator_stranded_asset_risk, 0.58).
domain_priors:suppression_score(coal_generator_stranded_asset_risk, 0.62).
domain_priors:theater_ratio(coal_generator_stranded_asset_risk, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coal_generator_stranded_asset_risk, extractiveness, 0.58).
narrative_ontology:constraint_metric(coal_generator_stranded_asset_risk, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coal_generator_stranded_asset_risk, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coal_generator_stranded_asset_risk, tangled_rope).
narrative_ontology:human_readable(coal_generator_stranded_asset_risk, "Coal Generator Stranded Asset Risk and Energy Transition Lock-In").
narrative_ontology:topic_domain(coal_generator_stranded_asset_risk, "energy_economics/regulatory_policy").

domain_priors:requires_active_enforcement(coal_generator_stranded_asset_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coal_generator_stranded_asset_risk, coal_plant_operators).
narrative_ontology:constraint_beneficiary(coal_generator_stranded_asset_risk, incumbent_utilities).
narrative_ontology:constraint_beneficiary(coal_generator_stranded_asset_risk, finance_backstop_regulators).
narrative_ontology:constraint_victim(coal_generator_stranded_asset_risk, renewable_energy_investors).
narrative_ontology:constraint_victim(coal_generator_stranded_asset_risk, grid_modernization_capacity).
narrative_ontology:constraint_victim(coal_generator_stranded_asset_risk, climate_mitigation_capacity).
narrative_ontology:constraint_victim(coal_generator_stranded_asset_risk, electricity_ratepayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL-DEPENDENT COMMUNITY (SNARE) — Trapped by economic dependency on coal employment and local tax base. Cannot exit without catastrophic local economic shock. Extraction is maximal: the constraint locks the community into subsidizing stranded assets through continued coal consumption while barring alternative economic development paths.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CLIMATE AND FUTURE GENERATIONS (SNARE) — Cannot exit fossil fuel lock-in. Bears the full cost of stranded asset risk dynamics that delay energy transition. Maximum extraction: deferred decarbonization concentrated on agents without voice in present decisions.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RENEWABLE ENERGY INVESTORS (TANGLED ROPE) — Constrained by regulatory barriers, grid access limitations, and rate-setting that favors incumbent utilities. Experience both extraction (access barriers, higher capital costs due to grid bottlenecks) and coordination benefits (renewable integration driving grid modernization). Significant but not maximal extraction.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COAL UTILITY COMPANY (ROPE) — Experiences the constraint as pure coordination of asset recovery. Deployed capital for coal plants during rational prior regulatory environment. Extraction flows toward this agent: cost-of-service regulation ensures recovery of stranded costs through ratepayer payment. Primary beneficiary with full exit arbitrage (can lobby for regulatory protection, securitize assets, request rate increases).
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Organized institutional actor facing genuine coordination problem (grid reliability during transition) but subject to extractive capture by incumbent utilities. Cost-of-service regulation genuinely coordinates reliable power supply, but the mechanism's structure systematically privileges capital-intensive coal infrastructure over flexible renewable sources. Active enforcement of stranded asset recovery mechanisms against taxpayer exit.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COAL INDUSTRY ADVOCACY (PITON) — Institutional structures (trade associations, state coal councils, research institutions funded by coal revenue) maintain high theater: 'clean coal,' 'carbon capture ready,' 'baseload reliability' narratives persist despite technology stalling and market obsolescence. Primary function (representing industry interests) has largely degraded to preventing exit rather than advancing viable business models. Theater ratio 0.85 — most activity is narrative maintenance rather than operational problem-solving.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PHYSICAL DECARBONIZATION CONSTRAINT (MOUNTAIN) — From the analytical perspective, the physical carbon budget available for remaining energy infrastructure is mathematically fixed by climate physics. Stranded asset risk is a consequence of deploying capital into assets that cannot operate within this budget. The constraint appears as immutable natural law — you cannot construct an energy system that burns coal and meets climate targets. However, the structural data reveals this as false summit: the constraint is implemented through contingent regulatory arrangements (cost-of-service recovery, captive ratepayers, bailout mechanisms) rather than physics.
constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coal_generator_stranded_asset_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coal_generator_stranded_asset_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coal_generator_stranded_asset_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coal_generator_stranded_asset_risk, TR),
    TR >= 0.70.

:- end_tests(coal_generator_stranded_asset_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from multiple victims (ratepayers, renewable investors, climate system) to benefit incumbents. However, extraction is not maximal (snare-level ≥0.66) because regulatory mechanisms must maintain legitimacy by funding grid improvements and ensuring reliable service — extraction cannot reach snare levels without triggering political backlash. The extractiveness trajectory shows increasing values (0.32 → 0.58) as market dynamics create tension with regulatory protections, widening the gap between renewable competitiveness and protected coal economics. Suppression (0.62): High. Significant barriers to exit include: cost-of-service regulation preventing retail choice, regulatory barriers to distributed solar deployment, transmission access denial for renewable generators, rate structures that don't reflect time-of-use value, and stranded cost charges that make alternative energy sources artificially expensive. Suppression operates through regulatory rather than physical means — ratepayers are trapped not by geography but by regulatory structure. Theater ratio (0.68): High. 'Clean coal,' 'carbon capture ready,' 'baseload reliability,' and 'grid stability' narratives consume significant institutional resources despite technical stagnation. Trade associations, utility PR, and friendly think tanks generate continuous narrative justification for coal plant retention despite economic obsolescence. Theater has increased over the interval as reality diverged from narrative.
 *
 * PERSPECTIVAL GAP:
 *   Coal utility company sees rope (coordination of capital recovery and grid reliability funding); coal community sees snare (economic lock-in with no realistic exit); electricity ratepayers see tangled rope (receive grid reliability benefits but pay stranded costs); renewable investors see tangled rope (mixed access/barrier coordination); regulators see tangled rope (genuine coordination challenges plus utility capture); climate/future generations see snare (pure extraction through deferred decarbonization); analytical observer risks false summit (naturalizing contingent regulatory lock-in as immutable physics). The gap between utility and community perspectives is maximum: the same regulatory structure appears as legitimate coordination to the beneficiary and as pure extraction trap to the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal utilities: beneficiaries with full arbitrage (can lobby, securitize, relocate) → low d → negative χ. Coal communities: trapped victims → high d → high χ (maximum extraction). Ratepayers: constrained victims (exit blocked by regulation but possible at cost) → intermediate-high d → moderate-high χ. Renewable investors: constrained victims with organized potential → intermediate d → moderate χ. Regulators: organized actors with constrained exit (political accountability) as both enforcers and subject to capture → mixed d depending on directionality override. Climate system: trapped victim → maximum d → maximum χ. The suppression mechanism operates through regulatory structure: cost-of-service recovery, retail access restrictions, transmission congestion pricing, distributed solar permitting barriers. Suppression could be reduced through regulatory change (retail choice expansion, renewable grid priority, true cost accounting for carbon) but is currently 0.62 due to incumbent institutional lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that stranded asset risk is genuinely hybrid: coal-dependent communities DO receive coordination benefits (grid stability, reliable electricity supply), and regulators DO face genuine coordination problems (capital recovery, rate stability, load balancing). The tangled rope classification is correct. However, the constraint also exhibits clear asymmetric extraction: benefits accrue to utilities and investors with exit options, while costs concentrate on trapped agents (coal communities, ratepayers, climate system) with no exit path. The mandatrophy resolution is that stranded asset risk is not a 'is this coordination or extraction?' question but 'who bears the coordination costs?' The constraint is coordination-FOR-UTILITIES-AND-INVESTORS, extraction-FROM-RATEPAYERS-AND-CLIMATE. The energy transition would more accurately be classified as scaffold (temporary support for transition period) if regulatory structures changed to: (1) limit cost-of-service recovery to actual transition period, (2) enable retail choice to allow ratepayer exit, (3) prioritize grid access for renewables, (4) accelerate plant retirement timelines. Currently, the constraint blocks this sunset through incumbent capture of regulatory process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coal_plant_lifetime_economics,
    'Do coal plants become economically uncompetitive before physical end-of-life, or does subsidized cost-of-service recovery extend operational lifespans beyond economic viability?',
    'Comparative analysis of plants operating under cost-of-service vs competitive market structures; correlation between regulatory regime and plant retirement timing vs levelized cost of new renewable capacity',
    'If suppression extends lifespans artificially: stranded asset risk is manufactured by regulatory structure (extraction mechanism stronger). If plants retire naturally before subsidy requirements: extraction is lower than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coal_plant_lifetime_economics, empirical, 'Whether regulatory recovery mechanisms artificially extend coal plant operation').

omega_variable(
    ratepayer_exit_capacity,
    'Can electricity ratepayers exit the stranded asset recovery mechanism through retail choice, distributed solar, or relocation, or are exit costs prohibitive?',
    'Analysis of retail choice penetration rates; cost differential between rooftop solar + battery vs grid electricity in regulated vs deregulated regions; relocation data for households exiting coal-dependent utility service territories',
    'If exit is mobile (retail choice available, solar affordable): ratepayers are constrained not trapped, classification shifts toward lower extraction. If exit is prohibitively expensive (rooftop solar blocked, relocation economically unfeasible): ratepayers approach trapped status, extraction increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratepayer_exit_capacity, empirical, 'Whether ratepayers can exit coal stranded asset cost-sharing').

omega_variable(
    regulatory_capture_persistence,
    'Are incumbent utilities capturing regulatory processes through structural advantages (revolving door staffing, information asymmetry) or through legitimate policy preferences among elected and appointed regulators?',
    'Analysis of regulatory decision patterns; tracking of staff flows between utilities and PUCs; comparison of regulatory outcomes in jurisdictions with strong vs weak lobbying disclosure and cooling-off periods',
    'If capture is structural (asymmetric information, revolving door): extraction mechanism is robust and persists. If capture is preference-based: regulatory change (new administration, rate case outcome) could shift extraction significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_persistence, conceptual, 'Whether regulatory capture is structural or preference-based').

omega_variable(
    stranded_cost_magnitude_bounds,
    'What is the true economic value of coal plant assets under climate policy scenarios, and how much of stranded cost is genuine economic loss vs regulatory rent transfer?',
    'Portfolio analysis of coal plants under 2°C carbon budget constraints; comparison of shutdown costs vs renewable replacement costs; accounting for avoided fuel, O&M, and environmental costs',
    'If stranded costs are genuine (assets truly worthless under climate policy): cost allocation across stakeholders is distributive not extractive. If stranded costs are inflated (regulatory recovery exceeds genuine loss): extraction component is larger than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_cost_magnitude_bounds, empirical, 'True economic magnitude of coal stranded assets vs regulatory cost-shifting').

omega_variable(
    renewable_grid_integration_barriers,
    'Are grid integration barriers (transmission congestion, frequency regulation, ramping capacity) genuine technical coordination problems or regulatory artifacts maintained to justify coal retention?',
    'Technical analysis of grid stability requirements under high renewable penetration; cost comparison of battery storage vs coal plants for frequency regulation; examination of jurisdictions with >70% renewable penetration (e.g., Uruguay, Denmark)',
    'If barriers are technical: coal extraction of grid reliability coordination is justified. If barriers are regulatory artifacts: coal plant justifications are theater masking rent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_grid_integration_barriers, empirical, 'Whether grid integration barriers justify coal plant retention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coal_generator_stranded_asset_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coal_stranded_tr_t0, coal_generator_stranded_asset_risk, theater_ratio, 0, 0.42).
narrative_ontology:measurement(coal_stranded_tr_t5, coal_generator_stranded_asset_risk, theater_ratio, 5, 0.55).
narrative_ontology:measurement(coal_stranded_tr_t10, coal_generator_stranded_asset_risk, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(coal_stranded_be_t0, coal_generator_stranded_asset_risk, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(coal_stranded_be_t5, coal_generator_stranded_asset_risk, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(coal_stranded_be_t10, coal_generator_stranded_asset_risk, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coal_generator_stranded_asset_risk, resource_allocation).
narrative_ontology:boltzmann_floor_override(coal_generator_stranded_asset_risk, 0.18).
narrative_ontology:affects_constraint(coal_generator_stranded_asset_risk, renewable_grid_integration_barriers).
narrative_ontology:affects_constraint(coal_generator_stranded_asset_risk, electricity_rate_design_lock_in).
narrative_ontology:affects_constraint(coal_generator_stranded_asset_risk, utility_regulatory_capture_mechanism).

% DUAL FORMULATION NOTE:
% Coal stranded asset risk decomposes into three structurally distinct constraints: (1) physical decarbonization requirement (mountain), (2) regulatory cost-recovery mechanism protecting incumbents (tangled rope / snare depending on perspective), (3) grid modernization coordination problem (rope with extraction overlay). Stranded asset risk as commonly discussed conflates these three; the decomposition reveals that the extraction mechanism is not decarbonization itself but the regulatory architecture that decouples decarbonization from incumbent cost-bearing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coal_generator_stranded_asset_risk, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
