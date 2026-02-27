% ============================================================================
% CONSTRAINT STORY: coinbase_crypto_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Centralized crypto exchanges like Coinbase create a market structure that
 *   simultaneously solves a genuine coordination problem (aggregating
 *   liquidity across dispersed participants) and extracts rents from the
 *   volatility this aggregation produces. The constraint is neither pure
 *   coordination nor pure extraction, but a hybrid that depends entirely on
 *   the observer's structural position. For retail traders, the exchange is a
 *   snare: they are trapped by network effects and must pay for market access
 *   through spreads, slippage, and forced settlement during high-volatility
 *   windows. For market makers and exchange operators, it is coordination:
 *   the infrastructure enables profitable arbitrage and reduces counterparty
 *   risk. For the DeFi coalition, it is a temporary structure facing sunset
 *   as layer-2 protocols and decentralized order books mature. The theater
 *   ratio (0.48) reflects that exchange operations are substantially
 *   functional (real settlement, real price discovery) but include
 *   performative elements (regulatory theater, compliance optics). The
 *   volatility itself is both a fundamental property of crypto assets and an
 *   institutional artifact: decentralized price discovery creates inherent
 *   variance, but exchange fee structures and order type design amplify it by
 *   rewarding high-frequency trading over long-term holding.
 *
 * KEY AGENTS:
 *   - Exchange Operators (Coinbase, Kraken, etc.): Primary beneficiary (institutional/arbitrage) — capture transaction fees, spread revenue, and network effects from concentration of order flow
 *   - Retail Traders: Primary victim (powerless/trapped) — forced to absorb spreads, slippage, and volatility-driven settlement costs; no meaningful exit options
 *   - Market Makers and HFT Firms: Secondary beneficiary (institutional/arbitrage) — profit from volatility signals and liquidity provision incentives; can arbitrage across venues
 *   - Long-Term Holders: Secondary victim (moderate/constrained) — benefit from liquidity and settlement speed but exposed to custody risk and forced participation in volatile market microstructure
 *   - DeFi Coalition: Organized competitor (organized/constrained) — building alternative structures but constrained by technical immaturity and liquidity fragmentation; benefits indirectly from exchange price discovery
 *   - Price Discovery Mechanism: Abstract victim (powerless/trapped) — market signals distorted by order flow manipulation and high-frequency noise; cannot exit distortion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.52).
domain_priors:suppression_score(coinbase_crypto_volatility, 0.58).
domain_priors:theater_ratio(coinbase_crypto_volatility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coinbase_crypto_volatility, extractiveness, 0.52).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coinbase_crypto_volatility, tangled_rope).
narrative_ontology:human_readable(coinbase_crypto_volatility, "Centralized Exchange Market Structure on Volatile Assets").
narrative_ontology:topic_domain(coinbase_crypto_volatility, "economic/technological").

domain_priors:requires_active_enforcement(coinbase_crypto_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, exchange_operators).
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, high_frequency_traders).
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, market_makers).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, retail_traders).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, long_term_holders).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL TRADER (SNARE) — Trapped in the exchange structure with no meaningful exit. Experiences maximum extraction through liquidity spreads, order slippage, forced settlement within high-volatility windows, and exposure to algorithmic front-running. No ability to negotiate terms or access alternative market infrastructure. Cannot exit without abandoning position, which incurs precisely the extraction costs they seek to avoid.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LONG-TERM HOLDER (TANGLED ROPE) — Constrained by network effects (liquidity available only on centralized exchanges) but also benefits from price discovery and settlement speed that the exchange provides. Experiences extraction through custody risk, price impact on large trades, and forced participation in volatile market microstructure during entry/exit. But the coordination function (24/7 market access, transparent orderbook) is genuinely valuable for medium-term position management.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MARKET MAKER (ROPE) — Experiences the constraint as pure coordination: the volatility creates liquidity opportunities, settlement speed enables profit, and the exchange's infrastructure (order matching, risk controls) is their primary enabling mechanism. Net beneficiary. Can arbitrage across venues and withdraw capital with minimal friction. Views the constraint as a coordination solution to the collective action problem of maintaining distributed market liquidity.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXCHANGE OPERATOR (ROPE) — Benefits from network effects and transaction volume that volatility generates. The constraint's core function is coordination: providing a centralized, standardized settlement mechanism that enables traders with heterogeneous risk tolerances to interact. The exchange's extractive capacity (spread capture, fee structure) is a necessary incentive to maintain the coordination platform. High exit options (can rebalance fee structure, redirect to derivative products, or integrate new assets).
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZED FINANCE COALITION (TANGLED ROPE) — Organized agents building alternative market structures (liquidity pools, automated market makers, peer-to-peer settlement) that compete with centralized exchanges. Constrained by smart contract limitations, validator coordination, and liquidity fragmentation. But also benefits from the exchange's discovery of price information and volatility signals. Sees the centralized structure as both oppressive (extraction via custody risk, regulatory risk concentration) and temporarily necessary (until decentralized coordination matures). The sunset implicit in DeFi growth.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRICE DISCOVERY MECHANISM (SNARE) — The abstract function of determining fair asset prices is trapped within a structure that incentivizes volatility over stability. The exchange's fee structure rewards high-frequency trading, which distorts price discovery. Information asymmetries between retail and institutional traders prevent price signals from reflecting true fundamental value. The market mechanism itself becomes a victim of extraction: prices reflect order flow manipulation and liquidity-seeking rather than underlying asset value.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/analytical perspective, some market volatility is inherent to price discovery in decentralized assets. Any mechanism that aggregates heterogeneous price expectations will exhibit volatility proportional to information dispersion. This perspective risks naturalizing what is actually institutional design: the volatility observed on Coinbase reflects not only fundamental asset properties but also fee structures, order types offered, trading hours, and margin requirements — all contingent choices. The engine's false summit detector should flag this as naturalization.
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

:- end_tests(coinbase_crypto_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The exchange captures economic surplus through two mechanisms: (1) transactional extraction via spreads and fees that exceed marginal settlement cost, and (2) volatility capture via fee structures that reward high-frequency trading over long-term holding. The value reflects that genuine coordination value is provided (settlement, liquidity aggregation), but the fee structure extracts beyond coordination cost. Suppression (0.58): Moderate-high. Barriers to exit include network effects (liquidity only available on centralized exchanges), regulatory risk concentration (alternative platforms face compliance uncertainty), technical barriers (retail users cannot efficiently run self-custody or DeFi protocols), and information asymmetries (retail traders lack access to order flow data that institutional traders use). Custody centralization adds risk concentration that suppresses individual exit options. Theater ratio (0.48): Moderate. Exchange operations are substantially functional (real-time settlement, transparent pricing, risk management) but include performative components (regulatory compliance optics, UI signaling of legitimacy, marketing of stability assurances). The ratio is rising because increased regulatory scrutiny has increased compliance theater while core settlement infrastructure remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The retail trader (powerless/trapped) experiences pure extraction (Snare) — they cannot exit and absorb all costs. The market maker (institutional/arbitrage) experiences pure coordination (Rope) — volatility is their profit source and the exchange provides enabling infrastructure. The exchange operator sees Rope from their position — they are solving the coordination problem of distributed liquidity. The DeFi coalition sees a structure with sunset (Tangled Rope degrading toward Scaffold) — the centralized constraint is temporary pending protocol maturation. The long-term holder occupies a middle ground (Tangled Rope) — they benefit from liquidity but are extracted from through forced participation in volatile market microstructure. The abstract price discovery mechanism is a victim (Snare) — information signals are distorted by extraction incentives. The analytical observer risks a false summit (Mountain) by naturalizing volatility as an inherent property of crypto assets when much is institutional choice (fee structures, order types, leverage availability). The gap between retail and institutional perspectives is maximal — they experience fundamentally different constraints from identical market infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural position relative to extraction flow: (1) Beneficiaries with arbitrage options (market makers, exchange operators) experience low d → negative or near-zero effective extraction χ. They see the constraint as coordination. (2) Victims with trapped exit (retail traders) experience high d → high χ. They see maximum extraction. (3) Agents with constrained exit (long-term holders, DeFi coalition) occupy middle ground (d ≈ 0.55-0.65). (4) Abstract victims (price discovery) have no agency and trapped exit → d ≈ 1.0, perceiving pure extraction. The engine's derivation chain computes d from beneficiary/victim declarations and exit options. The powerless retail trader gets high d because they are labeled a victim with trapped exit. The institutional market maker gets low d because they are labeled a beneficiary with arbitrage options. The organized DeFi coalition gets moderate d because they are labeled a victim with constrained (not trapped) exit — they have exit paths through protocol development but face near-term constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (Rope vs Snare confusion) by showing that both are correct from different structural positions. The key discriminator is exit options: institutional agents with arbitrage capacity (market makers, exchange operators) genuinely experience the constraint as coordination (Rope) — they choose to participate and can withdraw. Powerless agents with trapped exits (retail traders) experience it as extraction (Snare) — they cannot exit without prohibitive cost. The tangled rope classification at the moderate/constrained level captures the middle case where coordination value is real but extraction is significant. The mandatrophy resolution reveals that 'Is this a market or a trap?' depends on 'Can you walk away?' For retail traders the answer is no; for institutions it is yes. The exchange is simultaneously solving a real coordination problem and extracting rents by controlling the only solution. No single type is correct — the presheaf of perspectives over the market structure is the complete answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volatility_fundamental_vs_institutional,
    'How much of crypto asset price volatility is inherent to decentralized assets versus produced by centralized exchange market microstructure?',
    'Comparative volatility analysis: on-chain transaction patterns vs exchange orderbook activity; volatility on decentralized vs centralized exchanges for identical assets; historical volatility pre/post-exchange adoption',
    'If institutional (>60% exchange-generated): extractiveness should be higher (0.62+), classification remains Tangled Rope. If fundamental (<40% exchange-generated): extractiveness should be lower (0.38), may degrade to Rope-dominated perspectives. If mixed: validates current ε=0.52.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volatility_fundamental_vs_institutional, empirical, 'Attribution of volatility to fundamental vs institutional factors').

omega_variable(
    retail_exit_feasibility,
    'Do retail traders have genuine exit options to decentralized exchanges, peer-to-peer settlement, or self-custody without prohibitive cost or skill barriers?',
    'User adoption rates for DeFi platforms by retail cohort; transaction cost comparison (centralized vs decentralized); survey of barriers cited by retail traders attempting decentralized settlement',
    'If exit costs are prohibitive (gas fees, UI complexity, smart contract risk): retail perspective confirmed as Snare. If exits become accessible (layer-2 solutions mature, UX improves): retail perspective shifts toward Tangled Rope or Rope, extractiveness declines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_exit_feasibility, empirical, 'Whether retail traders have feasible alternative market structures').

omega_variable(
    fee_extraction_vs_coordination_cost,
    'What fraction of exchange fees represents true operational and infrastructure cost versus rent extraction from volatility-induced trading activity?',
    'Detailed cost accounting by exchange; comparison of fee structure to marginal cost of execution; analysis of fee changes during low-volatility versus high-volatility periods',
    'If fees exceed operational costs by >30%: Snare classification strengthened for victim perspectives. If fees approach marginal cost: classification downgrades toward pure Rope. Determines whether exchange is pricing coordination or extracting rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_extraction_vs_coordination_cost, empirical, 'Ratio of exchange fees to operational infrastructure costs').

omega_variable(
    custody_centralization_necessity,
    'Is centralized custody a necessary technical requirement for efficient settlement, or a chosen institutional arrangement that concentrates extraction risk?',
    'Technical analysis of settlement protocols; adoption rate of non-custodial trading interfaces; comparison to traditional securities markets'' custody models',
    'If necessary: justifies Rope perspectives (coordination requirement). If chosen: strengthens Snare perspectives (unnecessary centralization enables extraction). May reveal architectural alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_centralization_necessity, conceptual, 'Whether centralized custody is technical requirement or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coinbase_crypto_volatility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbvol_tr_t0, coinbase_crypto_volatility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cbvol_tr_t3, coinbase_crypto_volatility, theater_ratio, 3, 0.42).
narrative_ontology:measurement(cbvol_tr_t6, coinbase_crypto_volatility, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(cbvol_be_t0, coinbase_crypto_volatility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cbvol_be_t3, coinbase_crypto_volatility, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(cbvol_be_t6, coinbase_crypto_volatility, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coinbase_crypto_volatility, resource_allocation).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, cryptographic_custody_risk).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, blockchain_scalability_trilemma).
narrative_ontology:affects_constraint(coinbase_crypto_volatility, regulatory_arbitrage_crypto_jurisdictions).

% DUAL FORMULATION NOTE:
% The centralized exchange constraint is downstream of blockchain scalability limits (which make decentralized exchange less feasible) and custody technology immaturity. It also enables regulatory arbitrage by concentrating rule enforcement at exchange layer. Each upstream constraint has its own extractiveness; the exchange constraint represents the institutional response to upstream structural limitations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
