% ============================================================================
% CONSTRAINT STORY: commodity_price_momentum_herd
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commodity_price_momentum_herd, []).

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
 *   constraint_id: commodity_price_momentum_herd
 *   human_readable: Commodity Price Momentum Herd Behavior Constraint
 *   domain: economic/finance/markets
 *
 * SUMMARY:
 *   The commodity price momentum herd is a globally-distributed extraction
 *   mechanism where correlated trading by speculators and algorithmic funds
 *   creates self-reinforcing price trends that disconnect from supply-demand
 *   fundamentals, transferring wealth from physical commodity users and
 *   least-developed economies to financial speculators. The constraint
 *   exhibits a six-perspective span from pure extraction (food-insecure
 *   developing economies trapped in price volatility) through coordination
 *   mechanisms (exchanges solving price discovery) to performance narratives
 *   (efficient markets doctrine). The extractiveness score of 0.58 reflects
 *   moderate-to-high extraction that has grown over the 15-year interval as
 *   leverage ratios in derivatives markets have increased and algorithmic
 *   trading has enabled faster herd coordination. The suppression score of
 *   0.52 reflects significant barriers to exit (commodity users cannot avoid
 *   price exposure) but not total immobility (hedging tools exist for those
 *   who can afford them, and large buyers do coordinate). The theater ratio
 *   of 0.48 indicates the constraint has genuine coordination function
 *   (exchanges, price discovery, risk distribution) but increasingly
 *   performative justification (the claim that this momentum is efficient
 *   price discovery is empirically degraded by crashes and disconnection from
 *   fundamentals).
 *
 * KEY AGENTS:
 *   - Physical Commodity Users (Farmers, Manufacturers, Utilities): Primary victims (powerless/trapped) — must purchase inputs at momentum-inflated prices with no exit
 *   - Developing Economies & Food-Insecure Populations: Primary victims (powerless/trapped) — face acute price crises with zero hedging capacity
 *   - Momentum Traders & Algorithmic Funds: Primary beneficiaries (institutional/arbitrage) — profit from herd coordination and price trends
 *   - Derivatives Speculators: Primary beneficiaries (institutional/arbitrage) — extract via leveraged positions that amplify momentum
 *   - Commodity Exchange Operators: Secondary beneficiary (organized/arbitrage) — gain from trading volume and volatility without bearing extraction cost
 *   - Large Industrial Hedgers: Mixed position (moderate/constrained) — benefit from coordination function but constrained by hedging costs
 *   - Central Banks & Monetary Authorities: Mixed position (institutional/constrained) — solve price discovery problem but trapped in managing stagflationary effects
 *   - Regulatory Reformers: Organized victims (organized/constrained) — can influence rules but face financial sector resistance
 *   - Efficient Markets Narrative: Institutional justification (institutional/arbitrage) — naturalizes momentum as rational without empirical verification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commodity_price_momentum_herd, 0.58).
domain_priors:suppression_score(commodity_price_momentum_herd, 0.52).
domain_priors:theater_ratio(commodity_price_momentum_herd, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commodity_price_momentum_herd, extractiveness, 0.58).
narrative_ontology:constraint_metric(commodity_price_momentum_herd, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commodity_price_momentum_herd, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commodity_price_momentum_herd, tangled_rope).
narrative_ontology:human_readable(commodity_price_momentum_herd, "Commodity Price Momentum Herd Behavior Constraint").
narrative_ontology:topic_domain(commodity_price_momentum_herd, "economic/finance/markets").

domain_priors:requires_active_enforcement(commodity_price_momentum_herd).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commodity_price_momentum_herd, momentum_traders).
narrative_ontology:constraint_beneficiary(commodity_price_momentum_herd, algorithmic_funds).
narrative_ontology:constraint_beneficiary(commodity_price_momentum_herd, derivatives_speculators).
narrative_ontology:constraint_victim(commodity_price_momentum_herd, price_stability).
narrative_ontology:constraint_victim(commodity_price_momentum_herd, physical_commodity_users).
narrative_ontology:constraint_victim(commodity_price_momentum_herd, developing_economies).
narrative_ontology:constraint_victim(commodity_price_momentum_herd, food_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL COMMODITY USERS (SNARE) — Farmers, manufacturers, utilities, food processors cannot exit commodity price exposure without abandoning their core operations. Price momentum extraction runs against them directly: they must purchase inputs at inflated momentum prices, compressing margins. No alternatives exist for essential inputs like crude oil, wheat, or copper. Maximum extraction experienced.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMIES & FOOD SECURITY (SNARE) — Least-developed countries face price momentum in food and energy commodities with zero hedging capacity and no exit. Momentum spikes create acute food price crises, triggering malnutrition and social unrest. Structural entrapment: subsistence farmers and urban poor cannot wait out the cycle. This perspective shows pure extraction with suppression through geographic and economic powerlessness.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: HEDGERS & LONG-TERM BUYERS (TANGLED ROPE) — Large industrial buyers and coordinated purchasing groups (airlines, refineries, food manufacturers) benefit from coordination: pooled procurement, futures contracts, and hedging reduce individual exposure. But momentum extracts asymmetrically — smaller players cannot access the same hedging tools. This perspective shows genuine coordination function (pooled buying power reduces per-unit costs, shared price discovery) alongside asymmetric extraction (access to hedging is itself a rent-capturing mechanism).
constraint_indexing:constraint_classification(commodity_price_momentum_herd, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMODITY EXCHANGE OPERATORS (ROPE) — Exchanges benefit from increased trading volume and volatility (more contracts traded, higher fees) but genuinely solve coordination problems: standardized contracts, price discovery through transparent order books, and centralized clearing reduce search costs and counterparty risk. The extraction they enjoy is offset by real coordination service. They have full arbitrage exit: could migrate to different assets if commodity momentum declined. Experience constraint as coordination mechanism.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MOMENTUM TRADERS & DERIVATIVES SPECULATORS (ROPE) — Primary beneficiaries. The constraint solves their coordination problem: they need correlated entry/exit signals, trend-following indicators, and sufficient price momentum to make returns. The momentum herd is a profitable equilibrium for these agents. They experience the constraint as pure coordination: shared signals (technical indicators, news interpretation, algorithmic triggers) enable profitable synchronization. Exit: can shift to other asset classes instantly. Zero experienced extraction — this is the mechanism that extracts value from physically trapped agents, leaving speculators as net beneficiaries.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY & TRANSPARENCY REFORMERS (SCAFFOLD) — Organized actors (central banks, commodity market regulators, anti-corruption NGOs) see the momentum herd as a temporary coordination failure addressable through regulatory intervention: position limits, circuit breakers, transparency requirements for large derivatives positions, and commodity speculation taxes. This perspective sees the constraint as having a sunset: as regulations strengthen and real-asset buffers are rebuilt, the momentum mechanism loses force. Extraction is moderate because reformers have agency and can influence rule-making, though reform is politically constrained by financial sector influence.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CENTRAL BANKS & MONETARY AUTHORITIES (TANGLED ROPE) — These institutions solve a genuine coordination problem: price discovery for commodities is global and requires unified mechanisms to prevent local hoarding and information fragmentation. Central banks benefit from their price-setting influence (using commodity prices as inflation signals). But they are also victims of the extraction: commodity price spikes create stagflationary pressures that constrain monetary policy. They cannot fully exit this constraint (commodities are part of global economy) but have significant agency through policy tools. Constrained exit reflects their partial captivity to international commodity market dynamics.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: EFFICIENT MARKET HYPOTHESIS NARRATIVE (PITON) — The institutional story that commodity price momentum reflects rational expectation aggregation and efficient price discovery is largely performative at civilizational scale. The narrative persists through inertia (it justifies unfettered derivatives trading) but is empirically degraded: momentum crashes, flash crashes, and multi-year price swings that disconnect from supply/demand fundamentals contradict the efficiency claim. The EMH maintains the momentum constraint by naturalizing herd behavior as rational. Theater ratio high because the justification is ritualistic (invoking 'markets aggregate information') rather than functionally verified (prices do not actually converge to fundamental values).
constraint_indexing:constraint_classification(commodity_price_momentum_herd, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some price momentum is inherent to market microstructure: any market with traders using correlated signals will exhibit trends, and the gap between sentiment and value realization is mathematically inevitable. This perspective risks naturalizing what is a contingent institutional arrangement (leverage limits, algorithmic trading rules, derivatives market structure) as an immutable property of how markets work. The engine will detect this as a false summit, revealing that the 'inherent to markets' framing masks the specific regulatory choices that enable momentum extraction.
constraint_indexing:constraint_classification(commodity_price_momentum_herd, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commodity_price_momentum_herd_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commodity_price_momentum_herd, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commodity_price_momentum_herd, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commodity_price_momentum_herd, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commodity_price_momentum_herd, TR),
    TR >= 0.70.

:- end_tests(commodity_price_momentum_herd_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, rising over interval. The momentum constraint extracts value from physical commodity users through price spikes that compress their margins. Extractiveness is not maximal (0.7+) because (1) the herd can disperse and momentum can reverse, creating winners and losers asymmetrically, and (2) large-scale coordinated hedging by major buyers does reduce their exposure, showing partial coordination function offsetting pure extraction. The 15-year rise from 0.32 to 0.62 reflects increasing leverage in commodity derivatives markets and acceleration of algorithmic trading — more capital chasing correlated signals amplifies momentum. Suppression (0.52): Moderate-to-high. Physical commodity users face high costs to hedge (1-2% of transaction value for small players), making them effectively trapped. But suppression is not total because exits exist for large players and the constraint is not enforced by legal prohibition — it emerges from market microstructure. Theater ratio (0.48): Moderate. The efficient markets narrative (commodity prices reflect rational expectation aggregation) is performative because actual price behavior shows mean reversion, crashes, and multi-year disconnection from fundamentals. But the constraint has genuine coordination content: commodity exchanges do solve price discovery problems and enable risk distribution across counterparties. Theater is rising because algorithmic trading is increasingly opaque, making the claim that this represents human-driven rational aggregation more hollow.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival divergence: beneficiaries (momentum traders) classify it as Rope (coordination mechanism that solves their entry/exit signal problem), while trapped victims (developing economies) classify it as Snare (pure extraction with no alternative). Large industrial hedgers see Tangled Rope (genuine coordination benefits — pooled procurement — alongside extraction costs). Regulatory reformers see Scaffold (temporary problem solvable through rule changes like position limits). Central banks see Tangled Rope (solve price discovery, but trapped in managing volatility). Commodity exchanges see Rope (genuine transaction cost reduction and risk distribution). The piton classification (Efficient Markets doctrine) reveals the degraded justification: the narrative that this constraint represents rational information aggregation is maintained through institutional inertia despite empirical disconfirmation. The mountain perspective risks naturalizing contingent regulatory choices (leverage ratios, algorithmic trading rules, derivatives market structure) as inherent to how markets work. The perspectival gaps are diagnostic: the same constraint is read as coordination, extraction, or natural law depending on structural position — the framework's job is to show why each reading is correct from within its context.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position. Momentum traders and speculators are net beneficiaries with low d (they capture extraction value). Physical commodity users are trapped victims with high d (they bear the extraction cost). Exchanges have arbitrage exit options (d ≈ 0.2) and benefit from coordination function. Regulators are organized but constrained (d ≈ 0.45) — they can influence rules but financial sector lobbying limits their reach. Central banks are institutional but constrained by international markets (d ≈ 0.35) — they benefit from price discovery function but suffer stagflationary effects. Developing economies have no exit options and are trapped by essential commodity needs (d ≈ 0.95). The spread of d values across agents produces strong perspectival gaps: beneficiaries see pure coordination (Rope), trapped agents see pure extraction (Snare), mixed agents see tangled coordination-extraction hybrid (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Tangled Rope classification is required to avoid mislabeling mixed mechanisms. If analysis assigned Rope globally, it would miss the extraction borne by developing economies and small commodity users. If analysis assigned Snare globally, it would miss the genuine coordination problems (price discovery, counterparty risk distribution, pooled hedging) that exchanges and large players solve. The Tangled Rope classification captures that (1) genuine coordination exists (exchanges, price discovery mechanisms), (2) asymmetric extraction exists (momentum speculators extract from trapped commodity users), and (3) active enforcement is required (leverage rules, position limits, derivatives transparency) to maintain the coordination without cascading extraction. The mandatrophy also reveals that the piton and mountain perspectives are false summits: they naturalize contingent regulatory arrangements (current leverage limits, algorithmic trading rules) as either institutional inertia or laws of nature, when the structural constraint is actually an institution (derivatives markets with specific rules) that could be reformed. The framework shows why single-perspective analysis fails: from the beneficiary's view (Rope), momentum is efficient. From the victim's view (Snare), it is pure harm. Both are correct from within their structural context — the unified analysis is the presheaf of perspectives, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_versus_speculative_price,
    'What proportion of current commodity price levels reflects genuine supply/demand fundamentals versus speculative momentum?',
    'Variance decomposition of commodity returns into fundamentals-driven (production, consumption, inventory, weather) and momentum-driven (technical indicators, positioning data, algorithmic triggers) components. Cross-validation with historical periods of strict speculation bans (1970s controls) and compare price volatility and autocorrelation structure.',
    'If fundamentals > 70%: momentum constraint is weak coordination problem (Rope from more perspectives). If momentum > 40%: constraint is extraction mechanism (Snare from more perspectives). Implies different policy efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_versus_speculative_price, empirical, 'Proportion of commodity price driven by speculation versus fundamentals').

omega_variable(
    hedging_effectiveness_threshold,
    'Below what participation threshold in commodity futures markets do hedging costs become prohibitively expensive, and real-economy users become trapped?',
    'Analysis of hedging cost differentials: compare futures costs for institutional bulk hedgers (0.1% notional) versus small/medium commodity users (0.5-2% notional). Identify minimum volume at which hedging becomes economically viable. Cross-reference with SME survey data on actual hedging participation rates by firm size.',
    'If threshold is low (100+ ton minimum): only largest players can hedge effectively, trap is severe. If threshold is high (10,000+ ton minimum): most commodity users are trapped by unaffordable hedging. Determines true size of victimized population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hedging_effectiveness_threshold, empirical, 'Hedging cost threshold that traps commodity users').

omega_variable(
    momentum_factor_persistence,
    'Is commodity price momentum a stable, persistent risk factor (implying genuine market microstructure) or a regime-dependent artifact of current leverage/algorithmic rules?',
    'Time-series analysis of momentum factor returns across pre-2000 (low leverage, manual trading), 2000-2008 (rising leverage), 2008-2015 (post-crisis regulations), and 2015-present periods. Test for structural breaks in momentum returns, volatility, and autocorrelation. Compare momentum persistence across commodity types with different leverage constraints.',
    'If persistent across all regimes: momentum is fundamental market property (closer to Mountain). If regime-dependent: momentum is artifact of specific regulatory/leverage environment (closer to Tangled Rope or Scaffold with sunset). Determines whether momentum constraint is reversible or inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(momentum_factor_persistence, empirical, 'Whether momentum is persistent factor or regime-dependent artifact').

omega_variable(
    information_cascade_versus_fundamental_disagreement,
    'When traders herd in commodity momentum, are they cascading on correlated signals (information cascade) or do they genuinely disagree about fundamentals but coordinate on trend-following as a tiebreaker?',
    'Survey and interview momentum traders to elicit their belief distributions about commodity values. Compare stated beliefs with position clustering patterns. Analyze whether coordinated positioning precedes or follows news events (cascade behavior) versus whether it persists during no-news periods (independent signal agreement).',
    'If cascade: herd is fragile and can reverse sharply (high extraction volatility). If genuine disagreement: herd may be more stable (lower extraction volatility but higher average extraction). Affects policy intervention efficacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(information_cascade_versus_fundamental_disagreement, empirical, 'Whether herding is information cascade or coordinated disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commodity_price_momentum_herd, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpmo_tr_t0, commodity_price_momentum_herd, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cpmo_tr_t5, commodity_price_momentum_herd, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cpmo_tr_t10, commodity_price_momentum_herd, theater_ratio, 10, 0.48).
narrative_ontology:measurement(cpmo_tr_t15, commodity_price_momentum_herd, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(cpmo_be_t0, commodity_price_momentum_herd, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cpmo_be_t5, commodity_price_momentum_herd, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cpmo_be_t10, commodity_price_momentum_herd, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cpmo_be_t15, commodity_price_momentum_herd, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commodity_price_momentum_herd, resource_allocation).
narrative_ontology:affects_constraint(commodity_price_momentum_herd, agricultural_commodity_price_volatility).
narrative_ontology:affects_constraint(commodity_price_momentum_herd, energy_market_financialization).
narrative_ontology:affects_constraint(commodity_price_momentum_herd, sovereign_debt_commodity_dependency).

% DUAL FORMULATION NOTE:
% The commodity price momentum herd is structurally linked to broader financialization dynamics. Upstream constraints include leverage availability in derivatives markets (sets the horsepower of momentum). Downstream constraints include agricultural volatility (specific manifestation in food prices), energy market instability (specific manifestation in oil), and sovereign debt crises in commodity-dependent economies (structural consequence of volatility). This story models the coordination-extraction hybrid at the derivatives level; component stories should model specific commodity types and their local institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commodity_price_momentum_herd, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
