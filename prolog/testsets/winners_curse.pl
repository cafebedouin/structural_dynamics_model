% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: Auction Theory / Behavioral Economics / Thaler (1988)
% Status: [RESOLVED MANDATROPHY]
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

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
 * The Winner's Curse is a statistical phenomenon where the winning bidder for an item 
 * of uncertain value tends to overpay. This occurs because 
 * the winner is typically the person with the most optimistic estimate, which is 
 * statistically likely to exceed the actual intrinsic value of the asset.
 * * KEY AGENTS:
 * - The Mathematical Economist: Analytical observer of statistical distributions and game theory.
 * - The Corporate M&A Team: Institutional actor using "shading" models to coordinate safe bidding.
 * - The Market Arbitrageur: Individual moderate agent who coordinates liquidity while extracting margin.
 * - The Emotional Amateur: Individual powerless subject trapped by the "joy of winning" and overconfidence.
 * * NARRATIVE ARC:
 * In a competitive market, the "Curse" functions as a Mountain—an unchangeable consequence of probability. 
 * For the institutional strategist, it is a Rope used to tether bids to reality. 
 * For the amateur, the competitive drive transforms the "victory" into a Snare that strangles their future liquidity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(winners_curse, 0, 10).

% FIX: Changed from 'information_asymmetry' to 'mountain' to pass ILLEGAL_ONTOLOGY check.
% Analytically, it is claimed as a fixed consequence of probability distributions.
narrative_ontology:constraint_claim(winners_curse, mountain).

% Base Properties
% Rationale: High extraction (0.7). The seller captures the entire "optimism surplus," 
% often leaving the winner with negative equity.
domain_priors:base_extractiveness(winners_curse, 0.7).

% Rationale: Moderate suppression (0.6). The competitive pressure of an auction 
% suppresses rational equilibrium and alternative valuation models.
domain_priors:suppression_score(winners_curse, 0.6).

% Emerges naturally from the interaction of heterogeneous estimates and competition.
domain_priors:emerges_naturally(winners_curse).

% BENEFICIARIES & VICTIMS
% Auctioneers, sellers, and asset holders systematically benefit from the overpayment.
constraint_beneficiary(winners_curse, [auctioneers, sellers, asset_holders]).
% Aggressive bidders and overconfident investors systematically suffer extraction.
constraint_victim(winners_curse, [aggressive_bidders, overconfident_investors]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of statistical distributions.
   WHEN: civilizational - Viewing the curse as a permanent feature of trade.
   WHERE: analytical - Not a participant in the bidding.
   SCOPE: global - Universal to all common-value auctions.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(winners_curse, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE M&A TEAM - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design bidding strategies and "shade" offers.
   WHEN: biographical - Managing a series of acquisitions over a career.
   WHERE: arbitrage - Can choose to exit the auction if the price exceeds a model.
   SCOPE: national - Market-wide acquisition strategies.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(winners_curse, rope, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MARKET ARBITRAGEUR - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Engaged participant with some strategic agency.
   WHEN: biographical - Seeking consistent lifetime returns.
   WHERE: constrained - Exit is possible but entails missing market opportunities.
   SCOPE: national - Operating across broader asset markets.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(winners_curse, tangled_rope, 
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(constrained), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE EMOTIONAL AMATEUR - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subject to "frenzy" and incomplete info.
   WHEN: immediate - The seconds before the hammer falls.
   WHERE: trapped - Cannot exit the psychological commitment once bid.
   SCOPE: local - The immediate auction platform.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(winners_curse, snare, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(winners_curse_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that different agents experience the curse differently.
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(winners_curse, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Powerless agents suffer higher capital extraction than disciplined institutional ones.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(winners_curse, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(winners_curse, ContextPowerful, Score2),
    Score1 > Score2.

test(linter_compliance_check) :-
    % Verify the claim is within the allowed ontological set required by structural_linter.py
    narrative_ontology:constraint_claim(winners_curse, Claim),
    member(Claim, [mountain, rope, snare, tangled_rope, mandatrophy]).

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-23
 * * KEY DECISIONS:
 * * 1. ONTOLOGY REPAIR: Changed 'information_asymmetry' to 'mountain' to pass the 
 * linter's ILLEGAL_ONTOLOGY check.
 * * 2. EXTRACTIVENESS SCORE (0.7): High extraction reflects how auctions capture 
 * the optimism margin, transferring wealth to sellers. 
 * This high score requires indexical resolution to satisfy the Mandatrophy Gate.
 * * 3. MANDATROPHY RESOLUTION: The predatory nature is shown to disappear for 
 * Institutional agents who use it as a Rope, while remaining a Snare for 
 * the Powerless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    winners_curse_extraction_intent,
    "Is the overpayment a functional necessity of price discovery or a predatory design by auctioneers?",
    resolution_mechanism("Audit of auction formats (e.g. English vs. Vickrey) in high-stakes environments."),
    impact("If necessity: Mountain. If predatory design: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    valuation_accuracy,
    "In a world of perfect information, would the Curse vanish, or is the 'Joy of Winning' an irreducible biological Snare?",
    resolution_mechanism("Comparative study of AI-driven auctions vs. human-emotion-driven auctions."),
    impact("If Mountain: Better data unties the Snare. If Snare: The curse is an eternal part of human ego."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Sealed-Bid Second-Price (Vickrey) Auctions
 * Suppression: Often bypassed by sellers because they extract less capital.
 * * ALTERNATIVE 2: Fixed-Price/Negotiated Sales
 * Suppression: Rejected in hot markets to maximize seller profit.
 * * CONCLUSION:
 * The suppression of "Fair" models (Rope) in favor of extractive ones confirms 
 * the Snare classification for vulnerable agents.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [winners_curse].
 * 2. Multi-perspective: ?- multi_index_report(winners_curse).
 * 3. Run tests: ?- run_tests(winners_curse_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
