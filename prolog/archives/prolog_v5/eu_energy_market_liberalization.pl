% ============================================================================
% CONSTRAINT STORY: eu_energy_market_liberalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_energy_market_liberalization, []).

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
 *   constraint_id: eu_energy_market_liberalization
 *   human_readable: EU Energy Market Liberalization and Grid Coordination
 *   domain: energy/political_economy
 *
 * SUMMARY:
 *   EU energy market liberalization (initiated 1996-2009 through successive
 *   directives) created a dual structure: competitive wholesale markets
 *   overlaid on regulated transmission networks. The constraint reveals
 *   fundamental tension between short-term price efficiency (competitive
 *   markets' strength) and long-term grid stability coordination (markets'
 *   weakness). The architecture generates extraction from household consumers
 *   and coal-dependent regions while creating genuine coordination benefits
 *   for large industrialists, traders, and renewable generators. Theater has
 *   accumulated through regulatory proliferation — balancing mechanisms,
 *   capacity markets, cross-border auction procedures — that claim to prevent
 *   extraction but largely perform visibility while extraction mechanisms
 *   persist. The constraint exhibits lifecycle drift: initial extractiveness
 *   (0.32, Year 0) reflected genuine coordination problem; extractiveness has
 *   risen to 0.58 as rent-seeking layers (capacity payments, balancing
 *   charges, ancillary service premiums) accumulate faster than grid
 *   modernization reduces actual costs. Theater ratio drifted from 0.42 to
 *   0.65 as regulatory response to crises produced compliance overhead
 *   without reducing extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Household Consumers: Primary victims (powerless/trapped) — experience price increases without ability to switch or exit; benefit largely illusory except in high-competition urban markets
 *   - Coal-Dependent Regions: Secondary victims (moderate/constrained) — trapped by regional economic structure; benefit from transition subsidies but face commodity price exposure and job losses
 *   - Large Industrial Consumers: Primary beneficiaries (powerful/arbitrage) — capture competitive pricing, demand-side revenues, and relocation arbitrage across price zones
 *   - Energy Traders: Beneficiaries (institutional/arbitrage) — profit from price volatility and cross-border arbitrage enabled by market transparency
 *   - Transmission System Operators: Mixed agents (powerful/constrained) — benefit from increased throughput but constrained by mandatory renewable acceptance and balancing obligations
 *   - Renewable Energy Producers: Secondary beneficiaries (powerful/mobile) — benefit from market access but face grid integration extraction through balancing charges
 *   - Regulatory Apparatus: Institutional inertia (institutional/arbitrage) — maintains performative rules that substitute for competitive pressure; retains arbitrage capacity but lacks political will to reshape
 *   - Analytical Observer: Sees false natural law (analytical/analytical) — risks naturalizing market-structural extraction as immutable physics of grid coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_energy_market_liberalization, 0.58).
domain_priors:suppression_score(eu_energy_market_liberalization, 0.52).
domain_priors:theater_ratio(eu_energy_market_liberalization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_energy_market_liberalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_energy_market_liberalization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eu_energy_market_liberalization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_energy_market_liberalization, tangled_rope).
narrative_ontology:human_readable(eu_energy_market_liberalization, "EU Energy Market Liberalization and Grid Coordination").
narrative_ontology:topic_domain(eu_energy_market_liberalization, "energy/political_economy").

domain_priors:requires_active_enforcement(eu_energy_market_liberalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_energy_market_liberalization, large_industrial_consumers).
narrative_ontology:constraint_beneficiary(eu_energy_market_liberalization, energy_traders).
narrative_ontology:constraint_beneficiary(eu_energy_market_liberalization, renewable_energy_producers).
narrative_ontology:constraint_victim(eu_energy_market_liberalization, small_household_consumers).
narrative_ontology:constraint_victim(eu_energy_market_liberalization, grid_stability_maintenance).
narrative_ontology:constraint_victim(eu_energy_market_liberalization, fossil_fuel_dependent_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD CONSUMER (SNARE) — Small residential users face deregulated prices without switching ability. Supplier concentration, lock-in contracts, and lack of technical knowledge prevent exit. Market liberalization's alleged benefits (choice, competition) exist only on paper. Trapped in high-cost extraction.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COAL-DEPENDENT REGION (TANGLED ROPE) — Benefits from energy market stability and job preservation through transition funding. Simultaneously extracted through commodity price exposure and loss of guaranteed coal plant revenue. Constrained by dependence on transitional subsidies; cannot exit without terminal regional economic collapse.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE INDUSTRIAL CONSUMER (ROPE) — Benefits from competitive wholesale pricing, demand-side bidding, and direct grid contracts. High exit capacity (can hedge, relocate, self-generate). Liberalization creates genuine coordination function: transparent price signals improve allocation across Europe. Net beneficiary.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENERGY TRADER (ROPE) — Benefits from price volatility and market liquidity. Coordinates supply and demand across borders through arbitrage. Full exit capacity (can trade any commodity). Liberalization solves their coordination problem: transparent markets enable profitable inter-regional flows.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSMISSION SYSTEM OPERATOR (TANGLED ROPE) — Benefits from increased throughput revenues and reduced stranded costs from legacy contracts. Simultaneously extracted by non-dispatchable renewable injections that increase grid management costs without corresponding revenue. Constrained by regulatory mandate to accept all renewable generation; cannot exit coordination function.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGULATORY APPARATUS (PITON) — Maintains extensive compliance theater (market monitoring, balancing mechanisms, capacity markets) that substitutes for genuine competitive pressure. Regulations proliferate faster than enforcement capacity. The theater persists through institutional inertia — original coordination function (preventing monopoly abuse) degraded as market structure evolved. Regulators retain arbitrage capacity (can reshape rules) but lack political will.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: RENEWABLE ENERGY COALITION (SCAFFOLD) — Benefits from market access and subsidies during transition. Experiences extraction through grid integration costs and balancing charges. Organized agents (cooperatives, grid operators) see this as temporary coordination problem with sunset: grid modernization and storage deployment are building alternative integration pathways. Expects extraction to decline as flexibility mechanisms mature. Sunset: 15-20 years for grid integration costs to decline to baseload parity.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational perspective, the tension between commodity price efficiency and grid stability is immutable. Competitive markets optimize short-term allocation; physical grids require long-term coordination. This fundamental tension cannot be eliminated, only managed. However, structural data contradicts this naturalization — the tension is exacerbated by specific institutional choices (real-time pricing without demand-side flexibility, scarcity pricing during crises) rather than inherent to market economics.
constraint_indexing:constraint_classification(eu_energy_market_liberalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_energy_market_liberalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_energy_market_liberalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_energy_market_liberalization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_energy_market_liberalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_energy_market_liberalization, TR),
    TR >= 0.70.

:- end_tests(eu_energy_market_liberalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The household consumer experiences extraction through supplier consolidation (retail margins concentrated among few suppliers), lock-in contracts, and information asymmetries that liberalization's transparency promises never remedied. Large industrial consumers experience the opposite — they gained from market opening. Regional concentration of extraction (high in countries with weak regulatory enforcement: Hungary, Poland) vs low extraction (Denmark, Netherlands with strong consumer protections) shows extraction is not inherent to liberalization but depends on enforcement choices. Suppression (0.52): Moderate. Household consumers face real barriers to exit — switching costs, contract lock-in, supplier concentration — but barriers are not total. Some households can switch; some can self-generate or shift consumption. The suppression reflects rational supplier strategies to recreate barriers that regulation attempts to remove. Theater ratio (0.65): Moderate-high. EU energy regulations (balancing market rules, capacity mechanisms, cross-border auction procedures, renewables integration targets) perform extensive ritual of market design without necessarily preventing extraction. Balancing charges claimed as technical necessity; capacity payments claimed as investment incentive; auction procedures claimed as transparency — all true, but overlay is theater because the underlying extraction mechanisms (supplier consolidation, information asymmetry, stranded costs imposed on households) persist.
 *
 * PERSPECTIVAL GAP:
 *   The original research expectation was that liberalization would reduce extraction to rope (pure coordination). The empirical outcome shows tangled rope: genuine coordination benefits exist (traders improve allocation, large consumers get competitive pricing) but are paired with severe extraction (household consumers trapped, coal regions destabilized). The perspectival gap is stark: large industrials see rope (competitive markets solving allocation problems); households see snare (price traps with no exit). The coal region sees tangled rope (benefits from transition support but trapped by commodity exposure). The regulatory apparatus increasingly sees piton (its own rules performative rather than functional). The analytical observer at civilizational scale risks seeing mountain (grid stability vs market efficiency is immutable tension), but structural evidence shows this is naturalization — the tension is exacerbated by specific institutional choices, not inherent to physics. The gap reveals that 'market liberalization' as a single constraint is actually four distinct constraints with different ε values: wholesale market coordination (ε ≈ 0.15, pure rope), retail consumer market (ε ≈ 0.68, snare), grid stability coordination (ε ≈ 0.55, tangled rope), and regulatory theater (ε ≈ 0.62, piton). The decomposition would separate these.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. Large industrial consumers and traders have high exit capacity (arbitrage — can hedge, relocate, trade alternatives) and appear as beneficiaries (they gained from market opening), so d ≈ 0.15-0.20, producing low or negative effective extraction chi from their perspective. Household consumers have low exit capacity (trapped — cannot switch without cost, cannot relocate, cannot self-generate economically) and appear as victims (they lost from market opening as retail competition collapsed), so d ≈ 0.90-0.95, producing high chi from their perspective. Coal regions have constrained exit (medium cost to relocate, cannot stay economically viable in deindustrialization) and are both beneficiaries (transition subsidies) and victims (commodity price exposure), so d ≈ 0.55-0.65, producing moderate chi reflecting the mixed position. TSOs are powerful (institutional) but constrained by regulatory mandate, so d ≈ 0.45-0.55, producing moderate chi. Regulatory apparatus is institutional with arbitrage capacity, so d ≈ 0.10-0.15, producing low chi from its perspective (it captures compliance overhead revenues).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification resolves the mandatrophy by showing that liberalization succeeded at creating one genuine coordination mechanism (wholesale market enabling cross-border trade) while failing to create another (retail market delivering consumer choice). The constraint is not 'is liberalization extractive or coordinative' but 'which parts are which, and at what stage of lifecycle?' Year 0 extractiveness (0.32) reflects genuine coordination dominance — early liberalization did improve wholesale efficiency. Year 16 extractiveness (0.58) reflects extraction accumulation as rent-seeking layers (supplier consolidation, balancing charges, capacity payments) outpaced efficiency gains. Theater ratio drift (0.42 → 0.65) reveals that regulatory response to extraction crises produced compliance overhead rather than extraction reduction. The tangled rope classification holds because: (1) genuine coordination function exists (wholesale markets, cross-border trade), (2) asymmetric extraction coexists (household traps, coal region destabilization), (3) active enforcement required (regulatory oversight of retail markets, balancing mechanisms, capacity payments), (4) extraction does not depend solely on beneficiary/victim relationship but on whether enforcement mechanisms actually constrain extraction or perform constraint. The scaffold perspective (7 years until grid modernization reduces balancing costs) provides exit pathway for some victims (renewables coalitions, regions with storage deployment) but not for others (households in supplier-concentrated markets, coal regions in structural deindustrialization). The false mountain perspective (grid stability vs efficiency immutable) reveals exactly the authorial trap: naturalizing what is actually a contingent regulatory choice (real-time pricing without demand flexibility, scarcity pricing during crises, balancing charges on renewable injection).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_transition_cost,
    'How much of household price increases represents extractive profit-taking vs legitimate transition costs (grid modernization, renewable integration, stranded asset write-downs)?',
    'Decompose bill components: wholesale commodity cost, transmission and distribution margin, renewable support charges, system balancing costs, retail margin. Compare retail margins across EU countries; identify outliers. Track wholesale cost correlation with retail price changes.',
    'If extraction accounts for >30% of price increase: snare classification confirmed for households. If extraction <15%: reclassify as legitimate coordination problem (rope or scaffold). Mid-range (~20-25%): tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_transition_cost, empirical, 'Proportion of household price increases from extraction vs legitimate transition costs').

omega_variable(
    coal_region_exit_scenario,
    'Can coal-dependent regions transition to energy sector employment without regional economic collapse, or is current extraction (low coal prices, job losses) inescapable regardless of liberalization reversal?',
    'Comparative analysis of region-exit scenarios: early-adopting transition regions (Germany Ruhr Valley, Poland Silesia) tracking employment, wage replacement, housing prices, out-migration over 10-year transition windows. Counterfactual: what happens if EU reverses liberalization and guarantees coal demand?',
    'If regions can transition successfully: classification shifts to scaffold with genuine sunset. If transitions are failing: classification shifts to snare (trapped by structural deindustrialization, not by liberalization per se). Engine must distinguish policy-driven extraction from structural obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_region_exit_scenario, empirical, 'Whether coal regions can transition economically without regional collapse').

omega_variable(
    grid_stability_cost_attribution,
    'Are increased balancing and ancillary service costs driven by renewable integration (structural cost) or by market fragmentation and under-investment in flexibility (extractive inefficiency)?',
    'Compare TSO balancing costs across high-renewable vs fossil-heavy grids, controlling for grid topology and interconnection. Track historical trends: did balancing costs rise before or after renewable penetration? Comparison with countries that integrated renewables under different market structures (Denmark cooperative model vs German wholesale model).',
    'If structural cost dominates: scaffold perspective confirmed — costs decline as flexibility matures. If extractive inefficiency dominates: tangled rope perspective confirmed — grid operators capture balancing premiums while claiming technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_cost_attribution, empirical, 'Whether grid balancing cost increases are structural or extractive').

omega_variable(
    regulatory_theater_vs_effectiveness,
    'Do EU energy market regulations (capacity mechanisms, balancing rules, cross-border auction procedures) actually prevent monopoly abuse and ensure efficiency, or are they performative overhead that substitutes for competitive market pressure?',
    'Compare jurisdictions with heavy vs light regulatory burden (Spain/France heavy regulation vs Nordic light regulation) on key metrics: price volatility, consumer bill impacts, market concentration, grid reliability. Track compliance cost vs measured prevention benefit.',
    'If regulations effective: piton classification rejected — regulatory apparatus is rope. If regulations performative: piton classification confirmed — theater ratio >0.70 justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_theater_vs_effectiveness, empirical, 'Whether market regulations deliver functional benefit or are performative').

omega_variable(
    household_switching_barriers,
    'Are household switching barriers (information costs, lock-in contracts, supplier concentration) remediable through policy intervention (mandatory information standards, contract standardization) or are they structural (rational suppliers will recreate barriers)?',
    'Natural experiments: track household switching rates and bill savings after policy interventions (contract standardization in Netherlands, switching cost caps in UK). Correlation analysis: do barriers scale with supplier concentration? Can barriers be lowered without suppliers offsetting through other mechanisms (higher base tariff)?',
    'If barriers remediable: snare classification may shift to tangled rope with active policy enforcement. If barriers structural and recreated: snare classification confirmed — extraction is market-structural, not policy-fixable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(household_switching_barriers, empirical, 'Whether household switching barriers are policy-remediable or structurally persistent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_energy_market_liberalization, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eueml_tr_t0, eu_energy_market_liberalization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eueml_tr_t8, eu_energy_market_liberalization, theater_ratio, 8, 0.54).
narrative_ontology:measurement(eueml_tr_t16, eu_energy_market_liberalization, theater_ratio, 16, 0.65).
narrative_ontology:measurement(eueml_tr_t4, eu_energy_market_liberalization, theater_ratio, 4, 0.48).
narrative_ontology:measurement(eueml_tr_t12, eu_energy_market_liberalization, theater_ratio, 12, 0.6).

% Extraction over time
narrative_ontology:measurement(eueml_be_t0, eu_energy_market_liberalization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eueml_be_t8, eu_energy_market_liberalization, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(eueml_be_t16, eu_energy_market_liberalization, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(eueml_be_t4, eu_energy_market_liberalization, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(eueml_be_t12, eu_energy_market_liberalization, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_energy_market_liberalization, resource_allocation).
narrative_ontology:boltzmann_floor_override(eu_energy_market_liberalization, 0.18).
narrative_ontology:affects_constraint(eu_energy_market_liberalization, eu_coal_region_transition).
narrative_ontology:affects_constraint(eu_energy_market_liberalization, household_energy_poverty).
narrative_ontology:affects_constraint(eu_energy_market_liberalization, grid_stability_renewable_integration).

% DUAL FORMULATION NOTE:
% EU energy market liberalization decomposes into four structurally distinct constraints: eu_energy_wholesale_market_coordination (ε≈0.15, rope), eu_retail_energy_market (ε≈0.68, snare), eu_grid_stability_renewable_coordination (ε≈0.55, tangled rope), and eu_energy_regulatory_theater (ε≈0.62, piton). This story represents the aggregate constraint across all four. The wholesale market coordination is upstream (enables the others); retail extraction is downstream (depends on wholesale structure); grid stability is parallel (affects all); regulatory theater overlays all three. The network links capture cross-constraint effects: household energy poverty is a victim-side manifestation of retail extraction; coal region transition is exacerbated by commodity price exposure in liberalized wholesale markets; grid stability coordination is complicated by renewable injections created by market incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_energy_market_liberalization, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
