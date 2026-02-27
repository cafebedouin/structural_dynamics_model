% ============================================================================
% CONSTRAINT STORY: sa_renewable_price_differential
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sa_renewable_price_differential, []).

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
 *   constraint_id: sa_renewable_price_differential
 *   human_readable: SA Renewable Price Arbitrage Proxy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   South Australia's renewable penetration of 84% (as of February 2026) has
 *   driven wholesale electricity prices to $37/MWh, the lowest in the
 *   National Electricity Market (NEM), while NSW remains at $75/MWh. This
 *   price differential creates a structural constraint that functions
 *   simultaneously as a coordination mechanism for grid decarbonization and
 *   an extraction mechanism for coal generators and transmission-constrained
 *   regions. The constraint operates through institutional layers: renewable
 *   energy policy and subsidies coordinate capacity investment; electricity
 *   market settlement amplifies the price signal; transmission bottlenecks
 *   between states create arbitrage opportunities; capacity payment
 *   mechanisms redistribute wealth from thermal to renewable operators. The
 *   constraint is neither purely extractive nor purely coordinating — it is a
 *   hybrid (tangled rope) that reveals how decentralized energy transition
 *   creates regional winners and losers within a coupled wholesale market.
 *   The indexical classification varies dramatically across observer
 *   positions: coal generators in NSW see snare dynamics (trapped, declining
 *   revenue); SA renewable operators see rope dynamics (benefiting from
 *   coordination); AEMO sees tangled rope (coordinating grid stability while
 *   absorbing cost); and low-income SA consumers see secondary rope benefits
 *   (lower bills) mediated through retail competition. The theater component
 *   (0.35) reflects that traditional capacity market mechanisms increasingly
 *   perform no genuine scarcity function — they exist as institutional
 *   structures funding coal generator transition costs, not as real
 *   stabilization enforcement.
 *
 * KEY AGENTS:
 *   - SA Renewable Operators: Primary beneficiary (institutional/arbitrage) — capture wholesale price advantage and reserve market premiums; can exit or renegotiate terms
 *   - NSW Coal Generators: Primary victim (powerless/trapped) — stranded assets experiencing revenue compression; locked into NEM by long-term contracts and asset economics; no exit
 *   - AEMO Grid Operator: Dual function (organized/constrained) — benefits from coordination (lower dispatch costs); bears asymmetric cost (grid stability engineering, capacity payment administration)
 *   - SA Low-Income Consumers: Secondary beneficiary (moderate/mobile) — experience lower retail bills if wholesale advantage passes through; can switch retailers if prices rise
 *   - Interstate Industrial Consumers (NSW/VIC): Secondary victim (moderate/constrained) — trapped by transmission constraints; bear redistribution costs through capacity payments without receiving wholesale benefit
 *   - Net-Zero Transition Authority: Organized coalition (organized/constrained) — sees constraint as accelerating decarbonization; tolerates extraction as cost of transition; mandated to operate NEM despite distributional consequences
 *   - NEM Capacity Market: Institutional mechanism (institutional/arbitrage) — performs theatrical capacity pricing; redistributes wealth without core stabilization function
 *   - Analytical Observer: System-level view (analytical/analytical) — identifies constraint as permanent structural feature of decentralized renewable systems with constrained interconnection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sa_renewable_price_differential, 0.52).
domain_priors:suppression_score(sa_renewable_price_differential, 0.48).
domain_priors:theater_ratio(sa_renewable_price_differential, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sa_renewable_price_differential, extractiveness, 0.52).
narrative_ontology:constraint_metric(sa_renewable_price_differential, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sa_renewable_price_differential, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sa_renewable_price_differential, tangled_rope).
narrative_ontology:human_readable(sa_renewable_price_differential, "SA Renewable Price Arbitrage Proxy").
narrative_ontology:topic_domain(sa_renewable_price_differential, "economic/technological").

domain_priors:requires_active_enforcement(sa_renewable_price_differential).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, sa_renewable_operators).
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, sa_low_income_consumers).
narrative_ontology:constraint_victim(sa_renewable_price_differential, nsw_coal_generators).
narrative_ontology:constraint_victim(sa_renewable_price_differential, nem_grid_stability_fund).
narrative_ontology:constraint_victim(sa_renewable_price_differential, interstate_industrial_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NSW COAL GENERATOR (SNARE) — Trapped by stranded asset economics and interstate transmission constraints. Cannot exit the NEM without massive capital loss. Experiences SA's renewable capacity as a compression mechanism that forces their marginal costs below recovery thresholds. High suppression: regulatory lock-in, long-term contracts, and grid architecture constraints prevent exit. Maximum experienced extraction — no arbitrage, no alternative market.
constraint_indexing:constraint_classification(sa_renewable_price_differential, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AEMO GRID OPERATOR (TANGLED ROPE) — Benefits from genuine coordination function: SA's renewable surplus enables lower baseload dispatch requirements, reducing system inertia risk. But also bears asymmetric extraction: AEMO must design voltage stability protocols, frequency support contracts, and capacity payments to absorb renewable intermittency. Constrained exit: AEMO is mandated to operate the NEM; cannot exit the coordination problem. Dual function: coordination (inertia stabilization) plus enforcement cost (grid reliability engineering).
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SA RENEWABLE OPERATORS (ROPE) — Primary beneficiary. Experiences the price differential as pure coordination benefit: their marginal cost recovery is lower in SA's oversupplied wholesale market, but they can arbitrage interstate transmission capacity and reserve market premiums. High exit optionality: can shift output timing, stack reserve contracts, or negotiate PPAs outside the NEM if spot prices collapse. Net benefit without suppression — they can exit if terms degrade.
constraint_indexing:constraint_classification(sa_renewable_price_differential, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SA LOW-INCOME CONSUMERS (ROPE) — Secondary beneficiary. SA wholesale price advantage ($37/MWh vs $75/MWh NSW) translates into lower retail electricity bills through competitive retail supply. Mobile exit: can switch retailers if prices rise; benefit is real but conditional on retail market functioning. Theater component (0.35) reflects regulatory price caps that perform much of the benefit without relying on wholesale coordination alone.
constraint_indexing:constraint_classification(sa_renewable_price_differential, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERSTATE INDUSTRIAL (NSW/VIC) (SNARE) — Trapped by transmission bottlenecks and spot price coupling. Cannot exit the NEM or reroute supply. SA's renewable capacity compresses their regional wholesale prices intermittently but does not reach their region in volume — stranded benefit. Instead, they bear capacity payment and grid stabilization cost redistribution. Constrained exit: long-term industrial contracts lock them into NEM supply; cannot arbitrage alternatives.
constraint_indexing:constraint_classification(sa_renewable_price_differential, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NET-ZERO TRANSITION AUTHORITY (SCAFFOLD) — Organized agents (climate policy, renewable subsidy schemes, grid modernization mandates) see the price differential as a temporary coordination failure during transition. SA's renewable penetration is the intended outcome of policy support; the extraction (suppressed coal generator profitability, grid cost redistribution) is tolerated because it accelerates decarbonization. Sunset clause: as gas and hydrogen peaking capacity are deployed across the NEM and as interstate transmission capacity increases (Snowy 2.0, Marinus Link), the price differential will compress and grid stability costs will decline. Estimated sunset: 8-15 years.
constraint_indexing:constraint_classification(sa_renewable_price_differential, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: NEM CAPACITY MARKET (PITON) — Performs a capacity payment and reserve pricing function that theoretically coordinates supply adequacy but is increasingly theatrical: renewable capacity additions outpace demand, making traditional reserve margins obsolete. Capacity prices persist through regulatory inertia (AEMO capacity declarations) rather than genuine scarcity function. Theater ratio contributes to overall 0.35: much of the price differential is absorbed by capacity mechanism charges that don't directly pay for grid stability (the stated function) but instead redistribute wealth to coal generators experiencing revenue loss.
constraint_indexing:constraint_classification(sa_renewable_price_differential, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (ELECTRICITY MARKET STRUCTURE) (TANGLED ROPE) — From a 30-year market design perspective, the SA price differential reveals a genuine structural tension: renewable markets require transmission infrastructure and demand flexibility investments (coordination function) but also create regional scarcity rents that extract from stranded thermal generators and interstate consumers (extraction function). The constraint is not a temporary mismatch but a permanent structural feature of decentralized renewable systems with constrained interconnection. Effective extraction (0.52) reflects the market's net impact: real coordination benefit to system-level decarbonization plus genuine extraction from thermal assets and transmission-constrained regions.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sa_renewable_price_differential_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sa_renewable_price_differential, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sa_renewable_price_differential, TR),
    TR >= 0.70.

:- end_tests(sa_renewable_price_differential_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The price differential between SA ($37/MWh) and NSW ($75/MWh) directly reflects extraction from coal generators experiencing revenue compression. However, the extraction is not total suppression of an alternative — coal generators retain revenue from capacity markets, reliability obligations, and government transition support payments. The 0.52 value reflects that the constraint produces measurable redistribution (coal revenue down ~30-40% from pre-renewable baseline) without eliminating the coal generator entirely. The trajectory from 0.32 to 0.52 over 6 years reflects progressive deepening of the differential as SA renewable capacity additions continue and coal capacity retirement lags. Suppression (0.48): Moderate. Coal generators face significant barriers to exit (stranded assets, long-term contracts, regulatory lock-in) but not total suppression. Some exit occurs (Loy Yang A retirement announced); some pivot to gas peaking occurs. Interstate industrial consumers face transmission bottleneck suppression but can apply for demand-side participation or long-term PPAs. Moderate suppression also reflects policy support for coal transition (government purchase agreements, early retirement funding) that creates alternative revenue paths. Theater ratio (0.35): Low-moderate. The constraint's primary function is genuine: renewable integration requires grid stabilization engineering. However, capacity payment mechanisms that fund much of the redistribution are increasingly performative — AEMO capacity declarations assume scarcity that does not exist given renewable oversupply. The low (0.35) value reflects that the core wholesale price mechanism is functional (real supply-demand dynamics) while administrative layers are theatrical (capacity payments disconnected from grid stability need).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a dramatic perspectival gap driven by structural position relative to the price differential. SA renewable operators see pure coordination benefit (rope) — their marginal cost recovery is lower and they can arbitrage the price gap through dispatch timing and reserve markets. NSW coal generators see extraction (snare) — the same mechanism that benefits renewables compresses their revenue with no exit option. AEMO sees both coordination and extraction (tangled rope) — lower dispatch costs and reduced inertia requirements coordinate the grid, but AEMO must engineer expensive stabilization mechanisms to absorb renewable intermittency. Low-income SA consumers see secondary rope benefits mediated through retail competition — the wholesale advantage translates to lower bills if retailers pass through the benefit. Interstate industrial consumers see snare dynamics — they are trapped by transmission constraints that prevent them from accessing SA's low prices while bearing redistribution costs. The net-zero transition authority sees the constraint as a temporary scaffold — regional price compression is the intended consequence of renewable subsidies and is tolerated as a transition cost. The NEM capacity market institution sees its own function as degraded (piton) — capacity prices persist through regulatory inertia rather than genuine scarcity function. The analytical observer sees permanent tangled rope — renewable electricity markets structurally produce regional extraction unless transmission infrastructure can be expanded to compress spatial price differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's position relative to the extraction flow: Who benefits from the price differential, and who bears the cost? SA renewable operators are beneficiaries with arbitrage options (low d → negative chi from their perspective; constraint subsidizes them). NSW coal generators are victims with trapped exit (high d → high chi from their perspective; constraint extracts from them). AEMO is a victim of enforcement cost (benefits from coordination, costs from grid engineering) with constrained exit (moderate d). Low-income consumers are secondary beneficiaries with mobile exit (low d via beneficiary status, offset by moderate power level). Interstate industrial consumers are victims with constrained exit (high d). The transition authority is a beneficiary with constrained exit (moderate d — benefits from acceleration, costs from distributional fairness). The capacity market institution benefits from regulatory lock-in (arbitrage) with institutional power, but its function is degraded. The engine derives d automatically from beneficiary/victim declarations and exit options; the perspectival gap emerges from how different agents experience the same structural mechanism (renewable supply growth in a transmission-constrained market).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine coordination from extraction within the same market mechanism. The coordination function is real: renewable energy substitution reduces system marginal costs, lowers wholesale prices, and enables grid decarbonization. The extraction function is also real: coal generators experience revenue compression without consensual exit; interstate consumers bear cost redistribution without benefit access; transmission bottlenecks create scarcity rents. The tangled rope classification correctly captures both: the constraint requires active enforcement (AEMO dispatch, capacity market administration, transmission access rules) to function, it produces asymmetric benefit (SA renewables, NSW coal losers), and it has a genuine coordination component (grid stabilization). The mandatrophy would be created if we classified this as rope-only (pure coordination) or snare-only (pure extraction). The tangled rope classification with beneficiary (SA renewables, consumers) and victim (coal generators, interstate consumers) declarations accurately reflects the hybrid structure. The piton perspective on the capacity market is diagnostic: the theater gate reveals that administrative mechanisms are increasingly performative — capacity prices do not drive new capacity investment (renewable targets and subsidies do) and do not reflect actual scarcity (oversupply dominates). This teatricality is NOT the constraint itself but a nested institution within the market design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interstate_transmission_investment_threshold,
    'At what level of interstate transmission capacity expansion does the SA-NSW price differential compress below economically meaningful extraction levels (< $25/MWh)?',
    'Modeling of Snowy 2.0 and Marinus Link capacity completion timelines; simulation of spot price convergence at 5 GW, 10 GW, and 15 GW incremental interstate capacity',
    'If threshold < 10 GW: scaffold sunset is rapid (5-8 years), tangled rope becomes rope. If threshold > 15 GW: extraction persists through entire forecast period (15+ years), snare dynamics harden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interstate_transmission_investment_threshold, empirical, 'Transmission capacity threshold for price differential compression').

omega_variable(
    renewable_curtailment_enforcement,
    'Is the price differential enforced by actual physical renewable curtailment and dispatch restrictions, or is it an artifact of financial market settlement and capacity payments?',
    'Audit of AEMO dispatch data: proportion of SA renewable curtailment vs NSW coal curtailment; analysis of opportunity cost (uncompensated energy that could have been dispatched)',
    'If enforced via curtailment: suppression value (0.48) understates coercion on renewable operators; true suppression may be 0.65+. If financial artifact: extraction is more subtle (redistribution via capacity market) and tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_curtailment_enforcement, empirical, 'Whether price differential is enforced via curtailment or financial settlement').

omega_variable(
    coal_generator_exit_catalysis,
    'Does the SA price differential accelerate thermal generator retirement decisions (making it a transition acceleration mechanism) or does it trap marginal generators in zombie operation (making it extraction from stranded assets)?',
    'Longitudinal analysis of announced coal retirements 2024-2026; correlation between plant-level revenue pressure and exit timing; comparison with counterfactual retirement timeline',
    'If accelerating: extraction is a feature of transition policy (mandatrophy resolves: tangled rope is correct). If trapping: extraction is a bug that prevents efficient asset exit (mandatrophy tension remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_generator_exit_catalysis, empirical, 'Whether price differential accelerates or traps thermal generator exit').

omega_variable(
    consumer_retail_market_passthrough,
    'Do SA retail electricity providers pass through the wholesale price advantage to consumers, or is the benefit captured by retailers as margin?',
    'Quarterly retail price data for equivalent consumer profiles across SA and NSW; analysis of retail margin evolution 2024-2026; investigation of retail competition intensity',
    'If 80%+ passthrough: SA low-income consumer benefit is real (rope classification holds). If 40%- passthrough: benefit is captured by retailers (rope→snare transition for consumers), and beneficiary list should be narrowed to SA_renewable_operators only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_retail_market_passthrough, empirical, 'Retail passthrough of wholesale price advantage to consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sa_renewable_price_differential, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_renew_tr_t0, sa_renewable_price_differential, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sa_renew_tr_t3, sa_renewable_price_differential, theater_ratio, 3, 0.32).
narrative_ontology:measurement(sa_renew_tr_t6, sa_renewable_price_differential, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(sa_renew_be_t0, sa_renewable_price_differential, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sa_renew_be_t3, sa_renewable_price_differential, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(sa_renew_be_t6, sa_renewable_price_differential, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sa_renewable_price_differential, resource_allocation).
narrative_ontology:affects_constraint(sa_renewable_price_differential, coal_generator_stranded_asset_risk).
narrative_ontology:affects_constraint(sa_renewable_price_differential, nem_transmission_bottleneck).
narrative_ontology:affects_constraint(sa_renewable_price_differential, renewable_grid_intermittency).

% DUAL FORMULATION NOTE:
% The SA price differential constraint family consists of three structurally distinct claims: (1) renewable_capacity_effect (low ε ~0.08, mountain: renewable capacity additions are a natural consequence of cost decline), (2) sa_renewable_price_differential (moderate ε ~0.52, tangled rope: the price difference creates extraction and coordination), (3) coal_generator_stranded_asset_risk (high ε ~0.65, snare: thermal generators face irreversible revenue loss without policy support). Each story has different ε and different perspectives. The family is linked because (1) causes (2) which causes (3), but they are distinct constraints with distinct failure modes and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sa_renewable_price_differential, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
