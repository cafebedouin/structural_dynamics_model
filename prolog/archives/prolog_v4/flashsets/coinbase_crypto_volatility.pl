% ============================================================================
% CONSTRAINT STORY: coinbase_crypto_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
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
 *   The market structure of a centralized crypto exchange like Coinbase is
 *   built upon the inherent price volatility of crypto assets. This
 *   volatility creates opportunities for high-frequency traders and the
 *   exchange itself to profit, while often exposing retail traders to
 *   significant risks. The exchange facilitates trading but also extracts
 *   fees from each transaction, and benefits from the volatility that drives
 *   trading volume. The high volatility of the assets and the lack of
 *   regulation expose less sophisticated traders to higher losses.
 *
 * KEY AGENTS:
 *   - Coinbase Exchange: Primary beneficiary (institutional/arbitrage) — benefits from transaction fees and market-making activities.
 *   - High-Frequency Traders: Secondary beneficiary (powerful/arbitrage) — profits from short-term price fluctuations.
 *   - Retail Crypto Traders: Primary victim (powerless/trapped) — susceptible to market volatility and information asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.6).
domain_priors:suppression_score(coinbase_crypto_volatility, 0.4).
domain_priors:theater_ratio(coinbase_crypto_volatility, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coinbase_crypto_volatility, extractiveness, 0.6).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coinbase_crypto_volatility, tangled_rope).
narrative_ontology:human_readable(coinbase_crypto_volatility, "Centralized Exchange Market Structure on Volatile Assets").
narrative_ontology:topic_domain(coinbase_crypto_volatility, "economic/technological").

domain_priors:requires_active_enforcement(coinbase_crypto_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, coinbase_exchange).
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, high_frequency_traders).
narrative_ontology:constraint_victim(coinbase_crypto_volatility, retail_crypto_traders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Retail traders are often trapped within the exchange ecosystem due to lack of knowledge, high transaction fees for exiting, and the allure of potential high returns. They bear the cost of volatility and information asymmetry.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Coinbase benefits from the volatility as it drives trading volume and generates fees. They can arbitrage by setting the price, and thus they act as a rope.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the market structure as a tangled rope, where volatility is both a source of profit and a risk factor that could destabilize the exchange. High-frequency traders benefit, retail traders are exploited. There is a coordination element, but it includes extraction.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% High-frequency traders also benefit from the volatility and can arbitrage the price discrepancy for the immediate profits.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coinbase_crypto_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.6): The exchange extracts fees from each transaction, and benefits from the volatility, suppressing alternatives. The high volatility of the assets and lack of regulation expose less sophisticated traders to higher losses. Suppression (0.4): While retail traders are not physically forced to trade, the lure of high returns, combined with limited financial literacy and a complex market, creates a situation where they are effectively trapped. Theater ratio (0.3): Some efforts are made to provide educational materials and risk warnings, but these are often overshadowed by promotional activities and the inherently speculative nature of the market.
 *
 * PERSPECTIVAL GAP:
 *   Retail traders see the exchange as a potential get-rich-quick scheme, but often end up losing money due to volatility. The exchange profits either way. Regulatory intervention to protect retail traders might be resisted by the exchange and high-frequency traders, since it would cut into their earnings. The exchange and high frequency traders see a coordination mechanism from which they benefit, while retail traders are extracted.
 *
 * DIRECTIONALITY LOGIC:
 *   The exchange benefits from each transaction. Retail traders are trapped due to the lack of knowledge and the transaction fees for exiting the market. The high-frequency traders can operate with arbitrage, since they have more information at their disposal. The analytical observer understands the power dynamics between those three agents.
 *
 * MANDATROPHY ANALYSIS:
 *   While the exchange presents itself as a platform for democratizing finance, the inherent volatility of the market and the lack of regulation create a situation where retail traders are vulnerable to exploitation. It cannot be purely coordination, since the victims do not get any advantage from the process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_intervention,
    'Will increased regulatory scrutiny and intervention mitigate the exploitative aspects of this market structure?',
    'Monitoring regulatory actions and their impact on market volatility, transparency, and consumer protection.',
    'If yes, the constraint could shift towards a more balanced Tangled Rope or even a Rope. If no, the Snare perspective will remain dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention, empirical, 'The potential for regulatory intervention to alter the market structure.').

omega_variable(
    decentralized_alternatives,
    'Will decentralized exchanges (DEXs) and other alternative platforms erode the dominance of centralized exchanges like Coinbase?',
    'Tracking the growth and adoption of DEXs and their impact on the market share and profitability of centralized exchanges.',
    'If yes, the dependence of retail traders on centralized exchanges will decrease, potentially easing the Snare. If no, the dominance and extractive power of centralized exchanges will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_alternatives, empirical, 'The impact of decentralized alternatives on centralized exchange dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coinbase_crypto_volatility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coin_tr_t0, coinbase_crypto_volatility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coin_tr_t5, coinbase_crypto_volatility, theater_ratio, 5, 0.3).
narrative_ontology:measurement(coin_tr_t10, coinbase_crypto_volatility, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(coin_be_t0, coinbase_crypto_volatility, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(coin_be_t5, coinbase_crypto_volatility, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(coin_be_t10, coinbase_crypto_volatility, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coinbase_crypto_volatility, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
