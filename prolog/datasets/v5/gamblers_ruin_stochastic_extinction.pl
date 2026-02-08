% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: gamblers_ruin_stochastic_extinction
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Pascal & Fermat (1650s) / Huygens / Bernoulli
% ============================================================================

:- module(constraint_gamblers_ruin, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
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
 * * constraint_id: gamblers_ruin_stochastic_extinction
 * human_readable: Gambler's Ruin
 * domain: mathematical/economic
 * temporal_scope: 17th Century - Present
 * spatial_scope: Global/Abstract (Finite Capital vs. Infinite Bank)
 * * SUMMARY:
 * Gambler's Ruin is a statistical theorem stating that a gambler with finite 
 * wealth, playing a fair game against an opponent with infinite wealth (the 
 * "Bank"), will eventually go bankrupt with a probability of 1. It represents 
 * the inescapable "suction" of a random walk toward a boundary.
 * * KEY AGENTS:
 * - The Finite Gambler: The powerless agent whose capital is a limited buffer 
 * against the variance of the walk.
 * - The House (Institutional): An agent who uses the Law of Large Numbers as 
 * a "Rope" to coordinate inevitable profits from finite-capital participants.
 * - The Systemic Risk Manager: An observer who sees the ruin as a "Snare" 
 * for leveraged entities in volatile markets.
 * * NARRATIVE ARC:
 * Gambler's Ruin is the "Mountain" of stochastic fate—if the walk is long enough, 
 * the boundary of zero is a destiny. For the Casino, it is a "Rope" that 
 * coordinates stable revenue. However, for the individual player, the 
 * "long run" acts as a "Snare" that extracts their entire net worth 
 * (extraction) through the simple "choking" action of variance.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for Deferential Realism Auditor
narrative_ontology:interval(ruin_era, 1650, 2026).
narrative_ontology:constraint_claim(gamblers_ruin_stochastic_extinction, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. This is a highly extractive constraint. It mathematically 
% guarantees the total transfer of wealth from the powerless agent to the 
% institutional agent given enough time.
domain_priors:base_extractiveness(gamblers_ruin_stochastic_extinction, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.5. It suppresses the visibility of "lucky streaks" as a 
% viable survival strategy, rendering them merely a "Scaffold" that 
% temporarily delays the inevitable Snare.
domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, extractiveness, 0.9).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, suppression_requirement, 0.5).

% Enforcement: Emerges naturally from the axioms of the Random Walk.
domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(gamblers_ruin_stochastic_extinction, casino_corporations).
constraint_beneficiary(gamblers_ruin_stochastic_extinction, deep_pocket_investors). % Use wealth to outlast smaller rivals.
constraint_victim(gamblers_ruin_stochastic_extinction, finite_capital_players).
constraint_victim(gamblers_ruin_stochastic_extinction, leveraged_small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CASINO OWNER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to set house edge and table limits.
   WHEN: biographical - Planning the fiscal health of a multi-decade enterprise.
   WHERE: mobile - Can adjust table rules or diversify risk across games.
   SCOPE: global - Universal standard for gaming establishments.
   
   WHY THIS CLASSIFICATION:
   For the owner, the ruin is a "Rope"—a functional coordination mechanism. 
   It ensures that even in "fair" games (zero edge), the house's deeper 
   reserves act as a safety line. It provides a standard of achievement for 
   long-term profitability by "pulling" the aggregate of finite players 
   toward the house's vault.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    gamblers_ruin_stochastic_extinction,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FINITE STAKEHOLDER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Lacks the capital to change the math of the walk.
   WHEN: immediate - Every bet is a step closer to or further from the cliff.
   WHERE: trapped - Bound within the session rules and finite wallet.
   SCOPE: local - Immediate neighborhood of their current balance.
   
   WHY THIS CLASSIFICATION:
   For the individual player, the ruin is an unyielding Mountain. They 
   cannot "negotiate" with the probability density. If they play forever, 
   the math dictates their destruction. There is zero agency to change the 
   terminal state of the walk.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gamblers_ruin_stochastic_extinction,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MARGIN-TRADED INVESTOR - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the intelligence to trade but is bound by margin limits.
   WHEN: immediate - Facing a "Margin Call" that liquidates their position.
   WHERE: constrained - The "exit" requires capital they do not have.
   SCOPE: regional - Impacting local market segments.
   
   WHY THIS CLASSIFICATION:
   In leveraged trading, Gambler's Ruin is a "Snare." It "strangles" the investor 
   during temporary downward variance. Because they have a finite buffer, 
   the market's "randomness" extracts their entire equity (extraction) 
   before the upward trend can return. The volatility "chokes" the 
   life out of their account.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    gamblers_ruin_stochastic_extinction,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- 
    domain_priors:base_extractiveness(gamblers_ruin_stochastic_extinction, E),
    E >= 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gamblers_ruin_tests).

test(multi_perspective_variance) :-
    % Casino -> Rope
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, Type1, context(institutional, biographical, mobile, global)),
    % Player -> Mountain
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, Type2, context(powerless, immediate, trapped, local)),
    Type1 \= Type2.

test(extraction_intensity) :-
    % Demonstrates that the Snare classification highlights the 0.9 extraction of wealth.
    ContextSnare = context(individual_moderate, immediate, constrained, regional),
    constraint_indexing:extractiveness_for_agent(gamblers_ruin_stochastic_extinction, ContextSnare, Score),
    Score >= 0.8.

test(natural_emergence) :-
    domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction).

:- end_tests(gamblers_ruin_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.9): 
 * I chose 0.9 because the "ruin" is a absolute transfer of wealth. It is 
 * not just a tax; it is a terminal extraction of the entire stake. 
 * * 2. CLASSIFICATION RATIONALE:
 * - Casino Owner (Rope): Using the "Rope" of liquidity to outlast variance.
 * - Player (Mountain): Facing the unyielding law of probability.
 * - Leveraged Trader (Snare): Where the "Mountain" of variance becomes 
 * a "Snare" due to a margin call.
 * * 3. OMEGAS:
 * Formalized the uncertainty of "Infinite Play"—does the Mountain hold 
 * in a universe with finite time and finite house wealth?
 */

% OMEGA IDENTIFICATION
omega_variable(
    finite_bank_symmetry,
    "Is the 'Mountain' stable if the House also has a finite limit (Scaffold)?",
    resolution_mechanism("Calculation of ruin probability with two absorbing boundaries at $0$ and $B_{house}$."),
    impact("If House is finite: The 'Snare' is symmetric; the house can also be ruined."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Stopping Rules (Quit while ahead)
 * Viability: Theoretically possible for an individual to win and stop.
 * Suppression: Actively suppressed by "Compulsive Play" and "Near-Miss" 
 * psychological triggers in casinos (Scaffolding).
 * * ALTERNATIVE 2: Martingale Strategy (Double after loss)
 * Viability: Seems like a "Rope" but is actually a "Snare."
 * Suppression: Suppressed by the "Mountain" of Table Limits and 
 * finite bankrolls.
 * * CONCLUSION:
 * The absence of Alternative 2's viability (due to finite capital) is exactly 
 * what makes the Gambler's Ruin a terminal "Snare" for the individual.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_gamblers_ruin].
 * 2. Analyze: ?- multi_index_report(gamblers_ruin_stochastic_extinction).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
