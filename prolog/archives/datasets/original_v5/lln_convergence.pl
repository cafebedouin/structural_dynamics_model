% ============================================================================
% CONSTRAINT STORY: lln_convergence
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Law of Large Numbers (Jakob Bernoulli, 1713)
% ============================================================================

:- module(constraint_lln, []).

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
 * * constraint_id: lln_convergence
 * human_readable: Law of Large Numbers (LLN)
 * domain: mathematical/probabilistic
 * temporal_scope: 1713 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Independent trials)
 * * SUMMARY:
 * The Law of Large Numbers (LLN) is a theorem that describes the result of 
 * performing the same experiment a large number of times. It states that the 
 * average of the results obtained from a large number of trials should be 
 * close to the expected value, and will tend to become closer as more trials 
 * are performed.
 * * KEY AGENTS:
 * - The Individual Trial: The powerless subject whose variance is eventually 
 * smoothed away by the aggregate.
 * - The Casino / Insurance Firm (Institutional): An agent who uses the LLN 
 * as a "Rope" to ensure predictable profitability across millions of events.
 * - The Gambler (Victim): An agent with finite capital for whom the LLN 
 * acts as a "Snare" (Gambler's Ruin), ensuring they go broke before reaching 
 * the "long run" equilibrium.
 * * NARRATIVE ARC:
 * LLN is the "Mountain" of equilibrium—it is the inescapable gravity of 
 * probability. For the institutional architect, it is the "Rope" of 
 * coordination that turns randomness into business. However, for the 
 * individual with finite resources, the "Long Run" is a "Snare" because 
 * "in the long run, we are all dead" (Keynes).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(lln_era, 1713, 2026).
narrative_ontology:constraint_claim(lln_convergence, mountain).

% Base extractiveness: 0.25
% Rationale: Moderate. While a mathematical truth, the LLN "extracts" 
% capital from those who bet against the mean (Casinos) and "extracts" 
% the relevance of the individual event for the sake of the average.
domain_priors:base_extractiveness(lln_convergence, 0.25).

% Suppression score: 0.2
% Rationale: It suppresses the visibility of short-term variance in favor 
% of long-term stability.
domain_priors:suppression_score(lln_convergence, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(lln_convergence, extractiveness, 0.25).
narrative_ontology:constraint_metric(lln_convergence, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the axioms of probability.
domain_priors:emerges_naturally(lln_convergence).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(lln_convergence, insurance_industry).
constraint_beneficiary(lln_convergence, casino_operators).
constraint_victim(lln_convergence, gamblers_with_finite_capital).
constraint_victim(lln_convergence, short_term_variance_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SINGLE COIN FLIP - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The single trial has no agency over the total mean.
   WHEN: immediate - True at the moment of aggregation.
   WHERE: trapped - Bound within the laws of large-scale numbers.
   SCOPE: local - Immediate neighborhood of the specific trial.
   
   WHY THIS CLASSIFICATION:
   For the individual trial, the LLN is a natural law. It cannot "choose" to 
   skew the average of a billion flips; it is an insignificant, powerless 
   component of an inevitable convergence.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lln_convergence,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ACTUARY - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to price risk based on the aggregate.
   WHEN: biographical - Planning the multi-decade stability of a fund.
   WHERE: mobile - Can choose different sample pools to tune convergence.
   SCOPE: global - Applying the law to millions of policyholders.
   
   WHY THIS CLASSIFICATION:
   For the institution, LLN is a "Rope"—a functional coordination tool. 
   It allows the conversion of individual chaos into institutional order, 
   providing a "standard of achievement" for fiscal stability.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lln_convergence,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE FINITE-CAPITAL GAMBLER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Trapped by the "Law of Small Numbers" fallacy.
   WHEN: immediate - Running out of capital before the mean is reached.
   WHERE: constrained - The agent is bound by the specific limits of their wallet.
   SCOPE: local - A specific table or session.
   
   WHY THIS CLASSIFICATION:
   When capital is finite, the "guarantee" of convergence is a "Snare." 
   The agent is "strangled" by the variance (the path to the mean) before the 
   equilibrium (the mean itself) can save them. This is the extractive 
   reality of Gambler's Ruin.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lln_convergence,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(lln_convergence, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(lln_convergence_tests).

test(aggregation_fate_variance) :-
    % Trial -> Mountain
    constraint_indexing:constraint_classification(lln_convergence, Type1, context(powerless, immediate, trapped, local)),
    % Institution -> Rope
    constraint_indexing:constraint_classification(lln_convergence, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(gamblers_ruin_penalty) :-
    % A powerless agent with constrained options (finite capital) sees the law as a Snare.
    constraint_indexing:constraint_classification(lln_convergence, snare, context(powerless, immediate, constrained, local)).

test(emergence) :-
    domain_priors:emerges_naturally(lln_convergence).

:- end_tests(lln_convergence_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SHIFT: The core insight is that the "Law" is a gift to the 
 * infinite (or the large institution) but a trap (Snare) for the finite 
 * individual who cannot survive the variance.
 * 2. EXTRACTIVENESS (0.25): High enough to trigger the beneficiary/victim 
 * logic, reflecting that the LLN is the engine of extractive industries like 
 * commercial gambling.
 * 3. IMAGES: Triggered diagrams for convergence plots and dice simulations to 
 * clarify the transition from variance to stability.
 */

% OMEGA IDENTIFICATION
omega_variable(
    path_dependency,
    "Can the 'Snare' of the gambler be avoided if the 'Rope' of the mean 
    is visible in real-time?",
    resolution_mechanism("Experimental trial of gambling behavior with vs without live-mean visualizations."),
    impact("If Yes: The Snare becomes a Rope. If No: It is a permanent Mountain of biological limitation."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "Law of Small Numbers" (Fallacy)
 * Viability: Psychologically dominant in human brains.
 * Suppression: Suppressed by formal education and scientific method.
 * Evidence: Common in "Gambler's Fallacy" where players expect a "win" 
 * after a "loss."
 * * ALTERNATIVE 2: Cauchy Distributions (Infinite Variance)
 * Viability: The LLN does not apply here; the mean never stabilizes.
 * Suppression: Functionally "invisible" in standard business models that 
 * assume finite variance.
 * * CONCLUSION:
 * The institutional dominance of the LLN "Rope" effectively suppresses 
 * awareness of Alternative 2 (Fat Tails), leading to systemic "Black Swan" 
 * events when the Mountain collapses.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [lln_convergence].
% Analyze: ?- constraint_indexing:multi_index_report(lln_convergence).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
