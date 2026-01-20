% ============================================================================
% CONSTRAINT STORY: st_petersburg_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Daniel Bernoulli (1738) / Expected Utility Theory
% ============================================================================

:- module(constraint_st_petersburg_paradox, []).

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
 * * constraint_id: st_petersburg_paradox
 * human_readable: St. Petersburg Paradox (Divergent Expected Value)
 * domain: mathematical/economic
 * temporal_scope: 1738 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Decision Theory)
 * * SUMMARY:
 * The St. Petersburg Paradox describes a lottery game with an infinite expected 
 * monetary value, yet for which players are only willing to pay a small finite 
 * entrance fee. It exposes a fundamental constraint in classical decision theory: 
 * expected value does not equal human utility.
 * * KEY AGENTS:
 * - The Gambler (Subject): A powerless agent facing a game that promises 
 * "infinite" riches but offers no realistic path to collect them.
 * - The Casino Architect (Institutional): Uses the paradox as a "Rope" to 
 * justify house limits and the transition to Expected Utility Theory.
 * - The Rational Automaton (Victim): A hypothetical agent bound by 
 * pure expected value logic, who would pay everything they own for the ticket.
 * * NARRATIVE ARC:
 * The paradox acts as a "Mountain" of mathematical divergence—the sum 
 * $\sum 2^{n-1} \cdot (1/2^n)$ is undeniably infinite. In modern finance, 
 * Bernoulli’s resolution (Logarithmic Utility) is a "Rope" for coordinating 
 * risk management. However, for the "Rational" agent, the divergent sum is a 
 * "Noose" that extracts all their wealth (extraction) for a 1/2^k chance 
 * of a win they will never live to see.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(st_petersburg_era, 1738, 2026).
narrative_ontology:constraint_claim(st_petersburg_paradox, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.65. Highly extractive for an agent following pure EV logic. 
% The game "extracts" finite certainty (the entrance fee) in exchange for 
% an infinite "promise" that effectively has zero realizable value.
domain_priors:base_extractiveness(st_petersburg_paradox, 0.65).

% Suppression score (0.0-1.0)
% Rationale: 0.4. It suppresses the visibility of "linear" wealth valuation, 
% rendering the idea that "$100 is twice as good as $50" functionally 
% invisible in high-stakes risk modeling.
domain_priors:suppression_score(st_petersburg_paradox, 0.4).

% Enforcement: Emerges naturally from the interaction of powers of two.
domain_priors:emerges_naturally(st_petersburg_paradox).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(st_petersburg_paradox, extractiveness, 0.65).
narrative_ontology:constraint_metric(st_petersburg_paradox, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(st_petersburg_paradox, insurance_industries). % Profit from utility-based risk premiums.
constraint_beneficiary(st_petersburg_paradox, logarithmic_utility_models).
constraint_victim(st_petersburg_paradox, pure_expected_value_logicians).
constraint_victim(st_petersburg_paradox, naive_gamblers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COIN TOSS SEQUENCE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The coin has no agency; the probability $1/2^n$ is fixed.
   WHEN: immediate - True at every infinitesimal step of the game.
   WHERE: trapped - Bound within the geometric progression.
   SCOPE: local - Immediate result of the current flip.
   
   WHY THIS CLASSIFICATION:
   For the mathematical sequence itself, the divergent expected value is an 
   absolute Mountain. There is no "exit" from the arithmetic reality that 
   the sum of probabilities times payoffs has no finite bound.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    st_petersburg_paradox,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MODERN RISK ANALYST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to price insurance and financial derivatives.
   WHEN: biographical - Planning for a fund's long-term risk-adjusted return.
   WHERE: mobile - Can choose different utility functions (Log, Power, Exponential).
   SCOPE: global - Universal application in global finance.
   
   WHY THIS CLASSIFICATION:
   For the analyst, the paradox is a "Rope"—a tool for functional coordination. 
   By recognizing the "Mountain" of infinite EV, they coordinate a standard of 
   achievement (expected utility) that allows for the pricing of risk-aversion, 
   effectively "pulling" the market toward stability.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    st_petersburg_paradox,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CLASSICAL LOGICIAN - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the "Rationality" of Expected Value.
   WHEN: immediate - Forced to make a decision at the ticket booth.
   WHERE: constrained - The "exit" (refusing to play) feels like an irrational choice.
   SCOPE: local - A single, high-stakes decision.
   
   WHY THIS CLASSIFICATION:
   For the agent following pure expected value, the paradox is a "Noose." 
   It "strangles" their wealth by demanding a near-infinite entrance fee 
   (extraction). Because their logic tells them the game is worth everything, 
   they are "choked" into bankrupting themselves for a prize that only 
   exists in a transfinite "long run" they cannot survive.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    st_petersburg_paradox,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(st_petersburg_paradox, E),
    E >= 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INDIVIDUAL PLAYER (GAMBLER) - Noose
   --------------------------------------------------------------------------
   WHO: agent_power(individual_powerless) - The subject with finite wealth.
   WHEN: immediate - The decision to play and the risk of ruin occur now.
   WHERE: trapped - Bound by the physical limit of their bankroll.
   SCOPE: local - The impact is on the player's personal solvency.
   
   WHY THIS CLASSIFICATION:
   The "Noose" is the mismatch between infinite expectation and finite 
   utility. While the math (Mountain) suggests any entry fee is "cheap," 
   the player's finite capital means a high entry fee is an extractive 
   trap. The individual is "trapped" by the Gambler's Ruin: the 
   probability of total loss is 0.99 for any payout higher than the fee.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    st_petersburg_paradox,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(st_petersburg_paradox, E),
    E > 0.4.

% Explicit priors reflecting the extractive nature of infinite-value traps.
domain_priors:base_extractiveness(st_petersburg_paradox, 0.8).
domain_priors:suppression_score(st_petersburg_paradox, 0.2).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(st_petersburg_paradox_tests).

test(multi_perspective_variance) :-
    % Sequence -> Mountain
    constraint_indexing:constraint_classification(st_petersburg_paradox, Type1, context(individual_powerless, immediate, trapped, local)),
    % Analyst -> Rope
    constraint_indexing:constraint_classification(st_petersburg_paradox, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(extraction_penalty_insight) :-
    % Powerless logician feels high extraction (> 0.6)
    Context = context(individual_powerless, immediate, constrained, local),
    constraint_indexing:extractiveness_for_agent(st_petersburg_paradox, Context, Score),
    Score >= 0.6.

test(natural_emergence) :-
    domain_priors:emerges_naturally(st_petersburg_paradox).

:- end_tests(st_petersburg_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.65):
 * Reasoning: The paradox is fundamentally a trap for non-utility-based logic. 
 * It extracts all available finite assets in exchange for a "promise" of 
 * infinite payoff that only occurs at $n \to \infty$. 
 * * 2. PERSPECTIVE SELECTION:
 * Chose Coin (Subject), Analyst (Institution), and Logician (Victim) to 
 * show how the "divergence" is experienced as either a rule, a tool, or a trap.
 * * 3. OMEGA IDENTIFICATION: 
 * Formalized the "Finite Bank" uncertainty—does the Mountain hold if the 
 * house doesn't have infinite money?
 */

% OMEGA IDENTIFICATION:
omega_variable(
    house_resource_finiteness,
    "Is the 'Mountain' of infinite EV stable if the Casino is finite (Scaffold)?",
    resolution_mechanism("Calculation of expected value with a ceiling $2^k$ (e.g., total atoms in the universe)."),
    impact("If finite: The Mountain collapses into a manageable Rope ($EV \approx 10-20$)."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Finite Bank Limits
 * Viability: In reality, no house can pay $2^{100}$ dollars.
 * Suppression: Often ignored in math to keep the "Paradox" pure.
 * Evidence: Common sense physics.
 * * ALTERNATIVE 2: Risk-Aversion (Expected Utility)
 * Viability: The standard "resolution" of the paradox.
 * Suppression: Rejects pure EV logic, turning the "Noose" into a "Rope."
 * * CONCLUSION:
 * The existence of "Finite Bank" limits (Alternative 1) proves that the 
 * infinite "Mountain" is actually a Scaffold built on the assumption of 
 * infinite physical resources.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [st_petersburg_paradox].
 * 2. Analyze: ?- multi_index_report(st_petersburg_paradox).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
