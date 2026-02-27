% ============================================================================
% CONSTRAINT STORY: coinbase_crypto_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coinbase_crypto_volatility, []).

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
 *   constraint_id: coinbase_crypto_volatility
 *   human_readable: Centralized Exchange Market Structure on Volatile Assets
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Coinbase's centralized exchange model creates a structural constraint
 *   that extracts value from retail traders through latency arbitrage, spread
 *   capture, and order flow monetization while providing genuine coordination
 *   services (price discovery, liquidity aggregation, regulatory compliance).
 *   The constraint is embedded in the inherent volatility of crypto assets —
 *   high volatility creates both demand for trading and opportunities for
 *   predatory extraction. Volatility is exogenous (driven by speculative
 *   inflows, regulatory uncertainty, technological innovation), but
 *   Coinbase's market structure choices (millisecond-latency order books,
 *   maker-taker fee structures, order flow sales to market makers) are
 *   deliberate. The retail trader is trapped in Coinbase's ecosystem by
 *   network effects, regulatory lock-in, and lack of functionally equivalent
 *   alternatives. The exchange operator benefits from the same volatility
 *   that harms retail traders. Market makers and HFTs benefit from the
 *   latency structure. Long-term holders are constrained but can exit through
 *   self-custody or alternative venues. Decentralized finance protocols are
 *   building alternatives but face liquidity fragmentation and technical
 *   friction. The regulatory framework (SEC/FINRA rules applied to crypto)
 *   creates performative theater — compliance structures that justify the
 *   centralized model rather than optimize market efficiency.
 *
 * KEY AGENTS:
 *   - Coinbase / centralized exchange operators: Primary beneficiary (institutional/arbitrage) — captures order flow value, spread revenue, and custody fees; can exit by pivoting market structure or deploying to other venues
 *   - Retail traders: Primary victim (powerless/trapped) — lack latency advantage, face adverse selection from market makers, cannot exit due to network effects and regulatory barriers; absorb most extraction
 *   - Market makers and high-frequency traders: Secondary beneficiary (powerful/arbitrage) — exploit latency and information asymmetries; have arbitrage exits across multiple venues
 *   - Long-term holders: Secondary victim (moderate/constrained) — depend on Coinbase for price discovery and custody but can reduce exposure through alternative platforms; constrained by switching costs
 *   - Decentralized finance protocols: Organized alternative (organized/constrained) — building alternative market infrastructure (DEXs, AMMs) but constrained by liquidity fragmentation and regulatory uncertainty
 *   - Regulators and compliance frameworks: Institutional actor (institutional/constrained) — maintain performative theater of traditional securities regulation applied to native digital assets; constrained by uncertainty about crypto governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.52).
domain_priors:suppression_score(coinbase_crypto_volatility, 0.68).
domain_priors:theater_ratio(coinbase_crypto_volatility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coinbase_crypto_volatility, extractiveness, 0.52).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coinbase_crypto_volatility, tangled_rope).
narrative_ontology:human_readable(coinbase_crypto_volatility, "Centralized Exchange Market Structure on Volatile Assets").
narrative_ontology:topic_domain(coinbase_crypto_volatility, "economic/technological").

domain_priors:requires_active_enforcement(coinbase_crypto_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, exchange_operator).
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, high_frequency_traders).
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, market_makers).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, retail_traders).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, long_term_holders).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, price_discovery_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL TRADER (SNARE) — Locked into Coinbase's order flow ecosystem to access crypto markets. Cannot exit without losing market access. Faces adverse selection from latency arbitrage and information asymmetry. Experiences the constraint as pure extraction with minimal coordination benefit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LONG-TERM HOLDER (TANGLED ROPE) — Depends on Coinbase for custody and price discovery but can reduce exposure through self-custody or alternative exchanges. Constrained by switching costs and regulatory uncertainty. Perceives mixed coordination (price feeds, liquidity) and extraction (custody fees, spread capture). d≈0.68, f(d)≈1.00, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXCHANGE OPERATOR (ROPE) — Experiences the constraint as pure coordination: volatility creates trading demand, fee revenue, and network effects. Has arbitrage exit (can deploy capital to other venues, pivot market structure, or exit markets). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET MAKER / HFT FIRM (ROPE) — Leverages volatility and order flow information for profit. Has arbitrage exits across multiple venues. Experiences the constraint as pure coordination benefit: volatility = revenue. Minimal suppression from their perspective — they control information asymmetries. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZED FINANCE COALITION (TANGLED ROPE) — Organized effort to build alternative market infrastructure (DEXs, AMMs, decentralized custody). Constrained by liquidity fragmentation, regulatory risk, and network effects of centralized exchanges. Perceives the centralized structure as both coordination source (price feeds used as oracles) and extraction target (trying to disintermediate Coinbase). d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.43.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Traditional securities regulation (SEC/FINRA rules) applied to crypto via performative compliance frameworks (AML, KYC). Coinbase maintains regulatory theater to justify centralized model, but the framework itself is degraded — designed for equities and derivatives, not native digital assets. theater_ratio=0.55 reflects moderate performative content. Regulation persists through inertia, not because it optimally governs crypto markets.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, any market requires some form of clearing and settlement, and any settlement mechanism has latency. This latency creates information asymmetries and extraction opportunities. The constraint appears immutable: volatility + centralized clearing = structural extraction. However, structural data (ε=0.52, suppression=0.68, theater=0.55) reveals this as a false summit — the extractiveness is high because of DESIGN CHOICES (order flow sales, spread capture, latency prioritization), not because clearing is impossible. True price discovery mechanisms (like batch auctions) could reduce ε below 0.25.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coinbase_crypto_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coinbase_crypto_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coinbase_crypto_volatility, TR),
    TR >= 0.70.

:- end_tests(coinbase_crypto_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Coinbase captures 0.10-0.50% spreads (vs 0.01-0.05% on equities), sells retail order flow for ~$100-300M annually, and charges custody/withdrawal fees. This is material extraction from retail traders. However, extractiveness is not maximal (0.66+) because genuine coordination services (price discovery, liquidity aggregation, regulatory infrastructure) are bundled with extraction. The extractiveness has grown from 0.35 at launch (when it was primarily a price-aggregation service) to 0.52 now (as order flow monetization and maker-taker structures have been implemented). Suppression (0.68): High. Retail traders cannot easily exit due to: (1) network effects (98% of crypto trading volume on major CEXs), (2) regulatory lock-in (need KYC/AML verification), (3) technical friction on DEXs (gas fees, liquidity fragmentation, UX complexity), (4) information asymmetry (retail traders cannot detect that they're being adversely selected by market makers). Suppression has increased from 0.50 to 0.68 as DEX alternatives have improved but remain immature. Theater ratio (0.55): Moderate. Regulatory compliance (AML, KYC, Dodd-Frank-derived position limits) is 40-50% theater — it justifies the centralized intermediary structure and creates barriers to entry for competitors, but it doesn't optimize market efficiency. The remaining 45-60% represents genuine coordination services (settlement, custody, price feeds). Theater has increased slightly as regulatory requirements have grown more elaborate while market structure has moved toward predatory extraction (HFT-optimized latency, order flow sales).
 *
 * PERSPECTIVAL GAP:
 *   Retail traders experience pure extraction (Snare) with d≈0.92 — they absorb spreads, adverse selection, and order flow monetization with minimal ability to exit. Long-term holders experience mixed coordination and extraction (Tangled Rope) with d≈0.68 — they benefit from price discovery and liquidity but pay custody fees and face switching costs. Market makers experience pure coordination (Rope) with d≈0.10 — they benefit from latency structure and order flow with no suppression from their perspective. The exchange operator experiences pure coordination (Rope) with d≈0.08 — volatility creates fee revenue and network effects. Decentralized finance protocols experience mixed coordination and extraction (Tangled Rope) with d≈0.52 — they can access Coinbase price feeds for oracle data (coordination) but are excluded from user liquidity (extraction). The analytical observer risks seeing the constraint as a Mountain (immutable property of electronic markets), but the structural data (ε=0.52, suppression=0.68, theater=0.55) reveals this as a false summit — the high extractiveness comes from DESIGN CHOICES, not laws of nature. Batch auction mechanisms (like Call auctions or Frequent Batch Auctions) could achieve ε < 0.25 while providing identical coordination services.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail trader: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit Coinbase ecosystem due to network effects, regulatory requirements (need KYC to access any major venue), and DEX immaturability. Long-term holder: Victim + constrained → d≈0.68, f(d)≈1.00. High extraction but can reduce exposure through self-custody or alternative venues (though with friction). Exchange operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can deploy capital to other markets or pivot exchange architecture. Market maker: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Can access multiple venues and extract information asymmetry premium. DeFi coalition: Organized victim + constrained → d≈0.52, f(d)≈0.68. Mixed experience. Constrained by liquidity fragmentation and technical friction; can organize but cannot easily reach critical mass. Regulatory framework: Institutional actor + constrained → d≈0.42, f(d)≈0.40. Constrained by jurisdiction limitations and outdated legal frameworks. Piton classification comes from theater gate (≥0.70 would be piton, this is 0.55 so not quite), but the theater is high enough to flag degraded functionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination services from extraction mechanisms. Coinbase provides real coordination (price discovery, liquidity aggregation, regulatory infrastructure) that justifies some fee extraction — a pure Rope from the beneficiary perspective. However, the organization has layered additional extraction mechanisms (latency-optimized order routing, maker-taker fee structures, order flow sales) that are NOT coordination services — these are pure Snare components that exploit information asymmetries and network lock-in. The constraint is Tangled Rope because BOTH coordination and predatory extraction are active and required. The coordination function (price aggregation) cannot be easily separated from the extraction mechanism (latency-driven adverse selection) in the current architecture. Batch auction mechanisms or encrypted order books could achieve the coordination without the extraction, pushing ε toward 0.20 and classification toward pure Rope. The constraint's Tangled Rope classification correctly reflects that it mixes genuine market-making services with predatory order flow monetization, and mandatrophy is resolved by recognizing that the beneficiary's 'coordination' justification is partly true but obscures predatory layering.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latency_arbitrage_inevitability,
    'Is latency-driven information asymmetry an inherent feature of any electronic exchange, or a design choice that can be minimized through technological (batch auctions, encrypted order flow) or institutional (maker-taker fee structures) mechanisms?',
    'Comparative analysis of exchange architectures: millisecond-latency order books (Coinbase, NYSE) vs batch auction venues (Call markets, blockchain-based sealed-bid auctions). Measurement of effective spreads and adverse selection rates across mechanisms.',
    'If inherent: the constraint is closer to Mountain (ε < 0.30). If design choice: the constraint is pure Snare (ε > 0.60) because Coinbase actively chooses latency-favoring architecture despite lower-latency alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latency_arbitrage_inevitability, empirical, 'Whether latency arbitrage is inherent or a design choice').

omega_variable(
    volatility_extraction_coupling,
    'Does the exchange operator actively manipulate volatility to increase extraction (e.g., through market-making conflicts of interest, order routing decisions, or circuit breaker timing), or is volatility exogenous and the operator merely extracts from the volatility that exists?',
    'Market microstructure analysis: volatility clustering around Coinbase announcements, fee changes, margin liquidation events. Compare volatility on Coinbase vs other venues for identical assets. Analyze order-routing patterns for evidence of manipulation.',
    'If actively manipulated: the constraint is Snare + fraud (extraction + deception). If exploiting exogenous volatility: the constraint remains Tangled Rope (mixed coordination and extraction of natural volatility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volatility_extraction_coupling, empirical, 'Whether exchange actively manipulates volatility for extraction').

omega_variable(
    custody_network_effect_necessity,
    'Is the network effect that locks users into Coinbase (due to lack of interoperable custody standards) a natural consequence of distributed ledger technology, or an artifact of regulatory capture and deliberate incompatibility with alternative infrastructure?',
    'Analysis of technical interoperability: Can users move custody between Coinbase and decentralized wallets seamlessly? What custodial standards (BIP39, account abstraction) could be implemented? Comparison with historical precedent: did email or TCP/IP networks need regulatory lock-in to achieve network effects?',
    'If natural: suppression ≥0.70 (legitimate barrier). If artifact: suppression should be ≥0.80 (Snare territory), indicating regulatory capture as the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_network_effect_necessity, empirical, 'Whether custody lock-in is technical necessity or regulatory artifact').

omega_variable(
    decentralized_alternative_maturity,
    'Are decentralized exchanges (Uniswap, dYdX, Curve) currently functionally equivalent to centralized exchanges for the majority of retail traders, or do they present material friction (gas fees, capital efficiency, user experience) that makes them trapped alternatives rather than true exit options?',
    'Comparative UX study: time to trade, total cost of ownership (gas fees + slippage), liquidity availability, feature parity. Longitudinal tracking of liquidity migration from CEX to DEX. Measurement of retail trader switching costs.',
    'If equivalent: retail traders have mobile exit options, pushing d down and classification toward Rope. If material friction: traders remain trapped, d stays high, classification remains Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralized_alternative_maturity, empirical, 'Whether DEX alternatives are mature enough to enable true exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coinbase_crypto_volatility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbvol_tr_t0, coinbase_crypto_volatility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cbvol_tr_t5, coinbase_crypto_volatility, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cbvol_tr_t10, coinbase_crypto_volatility, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cbvol_be_t0, coinbase_crypto_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbvol_be_t5, coinbase_crypto_volatility, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(cbvol_be_t10, coinbase_crypto_volatility, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coinbase_crypto_volatility, resource_allocation).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, stable_coin_peg_stability).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, crypto_custody_standards).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, regulatory_capture_fintech).

% DUAL FORMULATION NOTE:
% The centralized exchange structure is downstream of (1) crypto asset volatility (exogenous), (2) regulatory requirements that mandate intermediaries, and (3) technical limitations of decentralized consensus. Each upstream constraint has its own ε value. The Coinbase structure extracts from all three sources simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coinbase_crypto_volatility, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
