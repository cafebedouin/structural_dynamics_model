% ============================================================================
% CONSTRAINT STORY: coinbase_crypto_volatility
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Coinbase Global, Inc. Form S-1 (February 25, 2021)
% ============================================================================

:- module(coinbase_volatility, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: coinbase_crypto_volatility
 * human_readable: Crypto-Economy Market Cycles
 * domain: economic/technological
 * temporal_scope: 2021 (IPO context) - Ongoing
 * spatial_scope: Global
 * * SUMMARY:
 * The extreme price volatility and cyclical nature of the crypto-economy, 
 * particularly Bitcoin and Ethereum, which directly dictate Coinbase's 
 * revenue, trading volume, and platform activity.
 * * KEY AGENTS:
 * - Coinbase Management: Navigating a business where revenue can drop 50%+ 
 * due to external market shifts.
 * - Retail Investors: The "engine" of trading revenue, highly sensitive to 
 * price swings.
 * - Institutional Investors: A growing segment seeking a "Rope" into 
 * crypto through a regulated entity.
 * * NARRATIVE ARC:
 * Coinbase’s S-1 is a study in "High-Variance Reality". The 
 * document explicitly states that their financial results will fluctuate, 
 * often wildly, based on a market they do not control. This is 
 * the ultimate "Mountain" constraint—an environmental fact of the 
 * industry.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(coinbase_ipo_window, 0, 10).
narrative_ontology:constraint_claim(coinbase_crypto_volatility, mountain).

% Base extractiveness score (0.0 - 1.0)
% Rationale: The market itself isn't "extractive" in a zero-sum sense, but 
% volatility extracts capital from unprepared retail participants.
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.4).

% Suppression score (0.0 - 1.0)
% Rationale: High; short-term price movements are absolute and cannot 
% be managed or "voted" away by management or investors.
domain_priors:suppression_score(coinbase_crypto_volatility, 0.9).

% Enforcement requirements
% Rationale: Emerges naturally from global market dynamics.
domain_priors:emerges_naturally(coinbase_crypto_volatility).

% Metrics
% Beneficiaries and Victims
constraint_beneficiary(coinbase_crypto_volatility, short_term_arbitrageurs).
constraint_victim(coinbase_crypto_volatility, long_term_capital_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Retail Trader - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless.
   WHEN: immediate - Trading on daily or weekly price action.
   WHERE: trapped - Often "locked in" by exchange downtime or price crashes.
   
   WHY THIS CLASSIFICATION:
   For the retail trader during a "crypto winter" or a flash crash, volatility 
   is a "Snare." They are liquidated or unable to exit positions due to 
   network congestion, resulting in an asymmetric loss of capital.
   
   NARRATIVE EVIDENCE:
   "Our net revenue is substantially dependent on... the price of Crypto 
   Assets... price volatility can result in increased costs... and potentially 
   impact our ability to operate".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    coinbase_crypto_volatility,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(coinbase_crypto_volatility, market_makers),
        constraint_victim(coinbase_crypto_volatility, retail_trader),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(coinbase_crypto_volatility, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Coinbase Institutional Team - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional.
   WHEN: historical - Building for the "100-year" crypto-economy.
   WHERE: arbitrage - Capitalizing on volatility through transaction fees.
   
   WHY THIS CLASSIFICATION:
   For Coinbase as an entity, volatility is a "Rope." It is the very engine 
   of their business. Without price movement, there is no trading volume; 
   without volume, there is no revenue. They use this 
   volatility to pull institutional capital into their "regulated" ecosystem.
   
   NARRATIVE EVIDENCE:
   "We have a highly scalable and reliable platform... our business is 
   substantially dependent on the total volume of transactions".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    coinbase_crypto_volatility,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(coinbase_crypto_volatility, coinbase_revenue),
        constraint_victim(coinbase_crypto_volatility, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(coinbase_crypto_volatility, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MARKET ANALYST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the external market drivers.
   WHEN: immediate - Volatility events occur with no warning.
   WHERE: arbitrage - Operates in a global, 24/7 liquid market.
   SCOPE: global - Crypto asset cycles are a worldwide phenomenon.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the exogenous volatility of Bitcoin and Ethereum. 
   As stated in the S-1, transaction volume and revenue are "driven by 
   factors that are not within our control." To the analyst, these cycles 
   are the unyielding physical laws of the crypto terrain. They represent 
   an immutable structural constraint that Coinbase must inhabit, rather 
   than a mechanism they can significantly alter.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    coinbase_crypto_volatility,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(coinbase_crypto_volatility, S),
    S < 0.5.

% Explicit priors reflecting the unyielding nature of market cycles.
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.4).
domain_priors:suppression_score(coinbase_crypto_volatility, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(coinbase_crypto_volatility, extractiveness, 0.4).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, suppression_requirement, 0.9).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(coinbase_volatility_tests).

test(revenue_correlation_logic) :-
    % Verify that institutional revenue is tied to the "arbitrage" of volatility
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope, 
        context(institutional, _, arbitrage, _, _, _)).

test(retail_vulnerability_audit) :-
    % Verify that the powerless/trapped agent sees a Snare
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare, 
        context(powerless, immediate, trapped, _, _, _)).

:- end_tests(coinbase_volatility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Unlike Blackstone (Internal Snare), Coinbase is 
 * defined by an External Mountain (Market Cycles).
 * 2. VOLATILITY AS ENGINE: The paradox of Coinbase is that the 
 * "Risk Factor" (volatility) is also the primary "Growth Factor".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    crypto_winter_duration,
    "Can Coinbase survive a multi-year period of low volatility and low prices (Mountain) without a secondary revenue Rope?",
    resolution_mechanism("Track growth of non-transaction revenue (Staking, Subscriptions) over 24 months"),
    impact("If staking fails: it is a Snare for the stock. If it succeeds: a Rope for stability."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Decentralized Exchanges (DEXs)
 * Viability: Direct competitors mentioned in the S-1.
 * Suppression: Competitive suppression; Coinbase positions itself as 
 * "regulated and safe" to differentiate from the "unregulated" DEXs.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [coinbase_volatility].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

