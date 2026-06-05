% ============================================================================
% CONSTRAINT STORY: liquidity_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liquidity_illusion, []).

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
 *   constraint_id: liquidity_illusion
 *   human_readable: The Exit Door Mirage
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The liquidity illusion is a structural constraint where markets display
 *   continuous bid-ask quotes, tight spreads, and apparent depth during
 *   normal conditions, creating confidence that capital is easily mobilized.
 *   This apparent liquidity is real most of the time — market makers fulfill
 *   quoted prices in ordinary trading. However, the illusion reveals itself
 *   during stress: when volume concentrates on one side (forced selling,
 *   funding shock, contagion), the displayed depth evaporates and the 'exit
 *   door' locks. Retail investors and locked-in capital believe they can exit
 *   at any time based on historical experience and visible orderbooks.
 *   Exchange operators and brokers profit from the illusion by capturing
 *   spreads and order flow that would not exist if true depth were visible.
 *   Regulators and fintech innovators are building alternatives (circuit
 *   breakers, real-time transparency, blockchain settlement) that would make
 *   the illusion structurally impossible by forcing display of true
 *   constraints. This constraint demonstrates how a pure coordination
 *   mechanism (price discovery) becomes a hybrid extraction system when
 *   combined with information asymmetry, theater of liquidity, and
 *   suppression of alternative markets.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — believe displayed liquidity reflects genuine exit capacity; experience catastrophic losses during stress when exit door vanishes
 *   - Locked-in Capital Holders: Primary victims (powerless/trapped) — pension funds, savings accounts, insurance reserves with redemption windows that create forced buyers/sellers during stress
 *   - Market Integrity (Epistemic Victim): Primary victim (powerless/trapped) — price discovery becomes corrupted by flash crashes and liquidity-driven dislocations; cannot exit or appeal
 *   - Market Makers and HFT: Primary beneficiaries (institutional/arbitrage) — profit from bid-ask spreads, order flow information, and the ability to withdraw supply when stress hits
 *   - Exchange Operators and Brokers: Primary beneficiaries (institutional/arbitrage) — capture transaction fees, maintain theater of continuous quotes, benefit from regulatory mandate requiring exchange listing
 *   - Regulatory Bodies (SEC, ESMA, central banks): Secondary agents (organized/constrained) — building circuit breakers and transparency mandates as sunset mechanism
 *   - Fintech/DeFi Innovators: Secondary agents (organized/constrained) — developing alternative settlement layers and transparent liquidity provision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liquidity_illusion, 0.52).
domain_priors:suppression_score(liquidity_illusion, 0.65).
domain_priors:theater_ratio(liquidity_illusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, 0.52).
narrative_ontology:constraint_metric(liquidity_illusion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(liquidity_illusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liquidity_illusion, tangled_rope).
narrative_ontology:human_readable(liquidity_illusion, "The Exit Door Mirage").
narrative_ontology:topic_domain(liquidity_illusion, "economic/technological").

domain_priors:requires_active_enforcement(liquidity_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liquidity_illusion, market_makers).
narrative_ontology:constraint_beneficiary(liquidity_illusion, insider_traders).
narrative_ontology:constraint_beneficiary(liquidity_illusion, exchange_operators).
narrative_ontology:constraint_victim(liquidity_illusion, retail_investors).
narrative_ontology:constraint_victim(liquidity_illusion, locked_capital_holders).
narrative_ontology:constraint_victim(liquidity_illusion, market_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Believes liquidity display reflects genuine exit capacity. During normal trading, bid-ask spreads are tight and volumes appear deep. When stress hits (flash crash, forced redemptions, contagion), the exit door vanishes. Trapped with no alternative market. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86. Pure extraction: illusion of liquidity extracts capital via lock-in.
constraint_indexing:constraint_classification(liquidity_illusion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL FUND MANAGER (TANGLED ROPE) — Coordinates portfolio rebalancing and risk management through liquid trading (genuine coordination function). Yet also experiences suppression: quarterly redemption windows, lockup periods, and hidden position constraints mean exit is constrained, not free. Liquidity illusion extracts through redemption lags. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55. Mixed: coordination benefit + extraction cost.
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET MAKER (ROPE) — Experiences liquidity provision as pure coordination. They profit from bid-ask spreads during normal markets; the illusion is profitable precisely because it enables their arbitrage function. Can exit or shift exposure in microseconds. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; sees constraint as enabling their market-making service.
constraint_indexing:constraint_classification(liquidity_illusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (SEC, ESMA, central banks, fintech innovators) are building circuit breakers, real-time transparency, and alternative settlement layers (blockchain, T+0) that reduce reliance on bid-ask theater. These reforms have a sunset: as transparent liquidity metrics and on-chain settlement mature, the illusion becomes structurally impossible. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.30. Low effective extraction because coalition has agency and sees an exit path (regulatory/technical maturation).
constraint_indexing:constraint_classification(liquidity_illusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EXCHANGE LISTING SYSTEM (PITON) — Primary function (price discovery via continuous matching) has been partly displaced by dark pools, algorithmic routing, and off-exchange trading, yet exchanges maintain the theater of continuous quotes and open orderbooks. theater_ratio=0.68 satisfies piton gate. Institutional inertia: exchanges persist because regulatory mandate and habit require them, not because they are the functional hub. Beneficiaries (exchange operators, brokers) maintain the illusion through regulatory capture.
constraint_indexing:constraint_classification(liquidity_illusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXCHANGE OPERATORS / BROKERS (TANGLED ROPE) — Coordinate market access and price discovery (genuine coordination function). Also extract rents through order flow sales, maker-taker fees, and privileged access to liquidity data. Constrained by regulatory requirements and competition from alternative venues. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.28. Moderate extraction; they see the constraint as necessary for maintaining market infrastructure credibility.
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, liquidity is inherently quantity-dependent: in stress scenarios, any finite market has a liquidity cliff because the order book has depth, not infinite supply. This perspective naturalizes liquidity illusion as an inevitable feature of finite markets. However, the structural data (ε=0.52, suppression=0.65, theater=0.68) contradicts pure mountain classification — the gap between real depth and displayed depth is not an immutable law but a contingent design choice (bid/ask display rules, circuit breaker thresholds, dark pool opacity). The engine will identify this as a false summit.
constraint_indexing:constraint_classification(liquidity_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liquidity_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liquidity_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liquidity_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liquidity_illusion, TR),
    TR >= 0.70.

:- end_tests(liquidity_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The illusion extracts capital through two mechanisms: (1) lock-in during stress (retail sells at worst prices, market makers capture spread). (2) Rent extraction on spreads during normal periods (justified as compensation for liquidity provision but enabled by information asymmetry). The 0.52 value reflects that extraction is real but not total — genuine liquidity provision occurs most of the time, so beneficiaries cannot extract continuously. Suppression (0.65): High. Significant barriers to true exit include: (a) regulatory mandate requiring exchange listing (alternative venues constrained by SEC approval), (b) dark pool opacity hiding true depth, (c) circuit breaker halts preventing exit during stress, (d) settlement lags (T+1 or T+2 forcing holding period), (e) informational barriers (retail cannot see institutional flows). Suppression is not total because brokers can execute off-exchange and some retail platforms offer alternatives. Theater ratio (0.68): High. The primary performance is the continuous quote display — the orderbook visible to all. This theater is performative because: (a) many visible bids/asks are phantom (cancelled within milliseconds), (b) displayed depth does not indicate execution-at-depth probability during stress, (c) circuit breakers halt trading during stress, preventing the implied exit. The theater increased over the interval (from 0.42 to 0.68) as high-frequency trading expanded, creating more ephemeral quotes and widening the gap between apparent and real depth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap between beneficiaries and victims. Retail investors and locked-in capital holders see a Snare: they believed they could exit but cannot during stress. Regulatory reformers see a Scaffold: circuit breakers, transparency requirements, and blockchain settlement are building ways to eliminate the illusion. Market makers see a Rope: they genuinely provide liquidity coordination services and profit fairly from bid-ask spreads. Exchange operators see a Piton: the exchange listing system is increasingly performative (dark pools and OTC handle more volume) yet persists through regulatory mandate. The analytical observer risks seeing a Mountain (finite markets always have liquidity cliffs) but the structural data reveals this as a false summit: the gap between real and apparent depth is not inherent but a design choice (quote display rules, dark pool opacity, circuit breaker thresholds). The perspectival gap is intentional: the beneficiaries (market makers, exchanges) maintain the illusion because transparency would collapse their profit margins.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail investors: Victim + trapped → d≈0.92, f(d)≈1.38. Cannot exit during crisis and have no information about true depth. Maximal extraction. Locked-in capital: Victim + trapped → d≈0.90, f(d)≈1.35. Forced sellers during stress; redemption windows create involuntary timing. Near-maximal extraction. Market makers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Can exit instantly and profit from others' forced trading. Net beneficiary. Exchange operators: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Regulatory mandate ensures their role; they profit from theater. Net beneficiary. Fund managers: Both + constrained → d≈0.68, f(d)≈1.05. Benefit from liquidity coordination but trapped by redemption windows and lockups. Mixed. Regulators: Organized + constrained → d≈0.35, f(d)≈0.35. Have power to reform but constrained by regulatory capture and need for market confidence. Low extraction. Market integrity: Victim + trapped → d≈0.93, f(d)≈1.40. Corrupted by flash crashes and liquidity-driven mispricings; cannot organize or exit. Maximal extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING THE EXTRACTION/COORDINATION TENSION: The liquidity illusion resolves mandatrophy by showing that a genuine coordination mechanism (price discovery through continuous matching) becomes an extraction system when combined with (a) information asymmetry, (b) theater (displayed depth ≠ real depth), and (c) suppression (alternatives constrained by regulation or cost). The beneficiaries profit precisely because they can exit when victims cannot — the asymmetry is structural, not accidental. Regulators are not just improving a coordination system; they are reducing the extraction rent by making true constraints visible (circuit breakers force public acknowledgment of liquidity cliffs; real-time transparency reveals depth limitations; blockchain settlement removes hidden intermediaries). The Scaffold perspective is real: reforms are sunset mechanisms because they target the theater (making depth truthful), not the underlying physics (finite markets have finite liquidity). Once true constraints are visible, the illusion cannot extract anymore — victims will self-select out or demand compensation for real risk. The constraint's high theater ratio (0.68) is the smoking gun: if liquidity provision were purely coordination, there would be no need for performative quotes. The theater is the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_vs_apparent_depth,
    'What fraction of displayed liquidity is real (executable at stated price) vs apparent (phantom bids/asks, fleeting quotes)?',
    'High-frequency analysis of quote lifetimes, cancellation rates, and fill success rates at claimed depths; comparison across venue (exchange vs dark pool vs OTC) and market condition (normal vs stressed)',
    'If real depth >> apparent: liquidity is reliable (Rope/Scaffold from more perspectives). If apparent >> real: illusion is structural and severe (Snare/Piton prevalence increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_vs_apparent_depth, empirical, 'Ratio of real to apparent liquidity depth').

omega_variable(
    crisis_contagion_mechanism,
    'When liquidity evaporates in one asset class or venue, what structural mechanism prevents contagion to others? Is it genuine compartmentalization or illusion?',
    'Analysis of correlation in bid-ask spreads, volume drying, and price impact across assets and venues during flash crashes, regulatory events, and funding shocks (2008, 2020 COVID, 2023 SVB)',
    'If genuine compartmentalization: crises are venue-local (Rope/Scaffold logic applies). If contagion is universal: the liquidity network is illusory and interdependent (Snare logic applies across the market).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crisis_contagion_mechanism, empirical, 'Whether liquidity crises are venue-local or systemic').

omega_variable(
    regulatory_circuit_breaker_effectiveness,
    'Do circuit breakers (trading halts, position limits, margin requirements) actually prevent liquidity evaporation or merely delay and intensify it?',
    'Comparative analysis of price recovery, volatility persistence, and retail losses in markets with vs without circuit breakers; correlation of halt duration with ultimate price discovery accuracy',
    'If effective: circuit breakers support the Scaffold perspective (temporary problem being solved). If ineffective: they are performative theater (Piton prevalence increases) and may worsen ultimate dislocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_circuit_breaker_effectiveness, empirical, 'Whether circuit breakers prevent or merely delay liquidity crises').

omega_variable(
    blockchain_settlement_scalability,
    'Can on-chain settlement (Ethereum, Solana, Bitcoin L2s) achieve settlement finality at sufficient speed and cost to eliminate liquidity illusion through transparency?',
    'Measurement of block time, confirmation latency, transaction cost per settlement, and orderbook depth for DEXs (Uniswap, dYdX) vs centralized exchanges (Coinbase, Kraken) under stress conditions',
    'If scalable: blockchain provides genuine alternative (Scaffold sunset real; transition to Rope/transparent pricing possible). If not: on-chain settlement becomes niche (illusion persists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(blockchain_settlement_scalability, empirical, 'Whether blockchain settlement can replace traditional liquidity provision').

omega_variable(
    insider_information_extraction,
    'How much of the profitability of market makers and high-frequency traders depends on private information flow (dark pool order flow, regulatory filings before public release) vs genuine liquidity provision?',
    'Analysis of trading profitability under regimes with different information access (pre-trade transparency, dark pool restrictions, delayed reporting); comparison of HFT profitability across venues with different surveillance',
    'If information dominates: extraction is severe and structural (Snare from retail perspective confirmed). If liquidity provision dominates: Rope logic applies and extraction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insider_information_extraction, empirical, 'Information advantage vs liquidity provision as driver of market maker profits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liquidity_illusion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liq_tr_t0, liquidity_illusion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(liq_tr_t10, liquidity_illusion, theater_ratio, 10, 0.55).
narrative_ontology:measurement(liq_tr_t20, liquidity_illusion, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(liq_be_t0, liquidity_illusion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(liq_be_t10, liquidity_illusion, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(liq_be_t20, liquidity_illusion, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liquidity_illusion, resource_allocation).
narrative_ontology:affects_constraint(liquidity_illusion, flash_crash_contagion).
narrative_ontology:affects_constraint(liquidity_illusion, stablecoin_death_spiral).
narrative_ontology:affects_constraint(liquidity_illusion, margin_call_cascade).

% DUAL FORMULATION NOTE:
% The liquidity illusion is upstream of specific crisis manifestations (flash crashes, stablecoin unravels, margin cascades). Each downstream constraint has its own ε value reflecting the severity of the specific failure mode; the liquidity illusion (ε=0.52) represents the structural precondition that makes all of them possible. Reforms addressing the illusion (transparency, circuit breakers) would prevent or mitigate all downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liquidity_illusion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
