% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Auction Theory / Behavioral Economics / Thaler (1988)
% ============================================================================

:- module(constraint_winners_curse, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: winners_curse
 * human_readable: The Winner's Curse
 * domain: economic/social
 * temporal_scope: Permanent (Information Asymmetry in Markets)
 * spatial_scope: Global (Competitive Bidding)
 * * SUMMARY:
 * The Winner's Curse is a phenomenon in auction theory where the winning bidder 
 * for an item of uncertain value tends to overpay. This occurs because the winner 
 * is typically the person with the most optimistic estimate of the item's worth, 
 * which is statistically likely to be higher than the actual intrinsic value.
 * * KEY AGENTS:
 * - The Auctioneer (Institutional): Sets the rules of the game to maximize 
 * competitive pressure and extract the highest bid.
 * - The Disciplined Bidder (Individual Moderate): Uses statistical models to 
 * shade their bids downward, treating the constraint as a manageable Rope.
 * - The Aggressive Winner (Individual Powerless): Subject to the "joy of winning" 
 * and incomplete information, finding the victory has become a financial Noose.
 * * NARRATIVE ARC:
 * In a competitive market, the "Curse" functions as a Mountain—a statistical 
 * reality where the mean estimate is closer to the truth than the highest 
 * estimate. For the institutional architect, it is a Rope to pull capital 
 * from participants. For the participant who fails to account for the bias, 
 * it becomes a Noose that strangles their future liquidity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(winners_curse_interval, 0, 10).
narrative_ontology:constraint_claim(winners_curse, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.6 (Moderate-High). The curse extracts "economic surplus" from 
% the winner and transfers it to the seller, often leaving the winner with 
% negative net value.
domain_priors:base_extractiveness(winners_curse, 0.6).

% Suppression score (0.0-1.0)
% Rationale: 0.5 (Moderate). It suppresses "Rational Equilibrium." The 
% emotional and statistical pressure of the auction makes the alternative 
% of "bidding exactly at value" invisible or impossible to achieve.
domain_priors:suppression_score(winners_curse, 0.5).

% Enforcement requirements
% Emerges naturally from the interaction of heterogeneous estimates and 
% competitive drive.
domain_priors:emerges_naturally(winners_curse).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(winners_curse, extractiveness, 0.6).
narrative_ontology:constraint_metric(winners_curse, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(winners_curse, [auctioneers, sellers, asset_holders]).
constraint_victim(winners_curse, [aggressive_bidders, overconfident_investors]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL ECONOMIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of statistical distributions and game theory.
   WHEN: civilizational - Viewing the curse as a permanent feature of human trade.
   WHERE: analytical - Not a participant in the bidding process.
   SCOPE: global - Universal to all common-value auctions.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the Winner's Curse is a Mountain. It is an unchangeable 
   consequence of probability. In any group where people estimate a hidden 
   value, the highest estimate is mathematically likely to be an outlier. 
   It is a fixed peak of reality that can be mapped but not moved.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    winners_curse,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, analytical, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE M&A TEAM - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design bidding strategies and shade offers.
   WHEN: biographical - Managing a series of acquisitions over a career.
   WHERE: arbitrage - Can choose to exit the auction if the price exceeds a model.
   SCOPE: national - Market-wide acquisition strategies.
   
   WHY THIS CLASSIFICATION:
   For the professional bidder, the Curse is a Rope. It is a coordination 
   mechanism for discipline. By assuming they are the "optimistic outlier" 
   if they win, they "tether" their bids to a lower price point. They use 
   the constraint to pull their organization toward profitable deals.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    winners_curse,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(winners_curse, E),
    E < 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EMOTIONAL BIDDER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "frenzy" of the live auction.
   WHEN: immediate - The seconds before the hammer falls.
   WHERE: trapped - Cannot exit the psychological commitment once the bid is made.
   SCOPE: local - The immediate auction room/platform.
   
   WHY THIS CLASSIFICATION:
   For the individual caught in a bidding war, the Curse is a Noose. The 
   desire to win "strangles" their rational judgment. When the hammer 
   falls, the "victory" extracts their financial security. They are 
   trapped with an asset worth less than they paid, with no way to undo 
   the transaction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    winners_curse,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(winners_curse, E),
    E > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(winners_curse_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(winners_curse, mountain, context(analytical, civilizational, analytical, global)),
    constraint_indexing:constraint_classification(winners_curse, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(winners_curse, noose, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_overpayment) :-
    % Powerless individuals feel the total extraction of their capital (Noose).
    % Institutional actors shade their bids to manage the extraction (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(winners_curse, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(winners_curse, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.6): The curse is highly extractive because the 
 * "winner" often loses money immediately upon winning. The benefit 
 * is entirely captured by the seller.
 * 2. PERSPECTIVES: Selected the Analyst (Hardware), the Professional (Tool), 
 * and the Amateur (Victim) to highlight the shift from statistical fact 
 * to strategic tool to personal trap.
 * 3. NOOSE LOGIC: Specifically focuses on the "trap" of overconfidence, 
 * where winning a competition is the mechanism that causes the loss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_accuracy,
    "In a world of perfect information (Mountain), would the Curse vanish, 
    or is the 'Joy of Winning' an irreducible biological Noose?",
    resolution_mechanism("Comparative study of AI-driven auctions vs. 
    human-emotion-driven auctions"),
    impact("If Mountain: Better data unties the Noose. If Noose: The 
    curse is an eternal part of human ego."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Sealed-Bid/Second-Price Auctions (Vickrey)
 * Viability: High. Theoretically encourages bidders to bid their 
 * true value without fear of the curse.
 * Suppression: Moderate. Sellers often suppress these "fairer" 
 * alternatives because they extract less capital than an open, 
 * ascending-price auction.
 * * CONCLUSION:
 * The existence of "Fair" auction models (Rope) that are often bypassed 
 * for "Exciting" auctions confirms that the Winner's Curse is a Noose 
 * designed or leveraged by institutions to extract maximum value.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_winners_curse].
 * 2. Multi-perspective: ?- multi_index_report(winners_curse).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
