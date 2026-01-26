% ============================================================================
% CONSTRAINT STORY: monetary_regime_transition
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Indexical Relativity Applied to Gold and Fiat
% ============================================================================

:- module(constraint_monetary_regime, []).

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
 * * constraint_id: monetary_regime_transition
 * human_readable: The Gold-to-Fiat Transition
 * domain: economic/political
 * temporal_scope: 1944 (Bretton Woods) to 1971 (Nixon Shock)
 * spatial_scope: Global (Reserve Currency context)
 * * SUMMARY:
 * The transition from the Gold Standard (commodity-backed) to Fiat (credit-backed) 
 * currency illustrates the Law of Indexical Relativity. What appears as a 
 * "Natural Law" of value to the individual becomes a "Strategic Variable" 
 * for the state.
 * * KEY AGENTS:
 * - Individual Saver: Agent seeking to preserve value across time.
 * - The Central Bank/State: Agent managing systemic liquidity and debt.
 * - Global Financial Markets: Analytical observers playing arbitrage between regimes.
 * * NARRATIVE ARC:
 * Gold is perceived as a Mountain by savers (limited supply, unchangeable). 
 * For the State, this Mountain is a Snare (constrains spending). By shifting 
 * to Fiat, the State transforms the constraint into a Rope (coordination via 
 * interest rates) while the Saver experiences a shift toward a Snare (inflationary 
 * extraction).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(nixon_shock_1971, 0, 10).
narrative_ontology:constraint_claim(monetary_regime_transition, rope).

% Base extractiveness score (0.5: Moderate extraction via inflation)
% Rationale: Fiat allows for "hidden" extraction of purchasing power over time.
domain_priors:base_extractiveness(monetary_regime_transition, 0.5).

% Suppression score (0.4: Alternatives like gold or crypto are visible but regulated)
domain_priors:suppression_score(monetary_regime_transition, 0.4).

% Requires active enforcement (Legal tender laws)
domain_priors:requires_active_enforcement(monetary_regime_transition).

% Metrics for DR-Audit
narrative_ontology:constraint_metric(monetary_regime_transition, extractiveness, 0.5).
narrative_ontology:constraint_metric(monetary_regime_transition, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(monetary_regime_transition, [debt_issuers, sovereign_states, financial_intermediaries]).
constraint_victim(monetary_regime_transition, [fixed_income_savers, powerless_creditors]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Individual Saver (Gold Standard) - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical (Lifetime of saving)
   WHERE: trapped (Subject to national currency)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the individual, the Gold Standard is a Mountain. Its scarcity is 
   perceived as a physical law of nature that protects the "objective" 
   value of their labor.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    monetary_regime_transition,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Perceived as unchangeable commodity scarcity
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Sovereign State (Fiat Regime) - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   WHERE: arbitrage (Can print or tax)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the State, Fiat is a Rope. It is a coordination mechanism that allows 
   for flexible response to crises, infrastructure investment, and 
   monetary policy adjustment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    monetary_regime_transition,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Creditor (Fiat Inflation) - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: constrained (Costly to exit into hard assets)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For those holding long-term debt or cash, Fiat can function as a Snare. 
   The intentional expansion of the money supply acts as a coercive 
   asymmetric extraction of their future purchasing power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    monetary_regime_transition,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(monetary_regime_transition, E),
    E >= 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(monetary_regime_tests).

test(regime_shift_as_index_shift) :-
    % Individual sees Mountain (Gold)
    constraint_indexing:constraint_classification(monetary_regime_transition, mountain, 
        context(individual_powerless, biographical, trapped, local)),
    % State sees Rope (Fiat)
    constraint_indexing:constraint_classification(monetary_regime_transition, rope, 
        context(institutional, generational, arbitrage, national)),
    % Saver in Fiat sees Snare
    constraint_indexing:constraint_classification(monetary_regime_transition, snare, 
        context(individual_moderate, biographical, constrained, national)).

test(extraction_variance) :-
    % Institutional power reduces the experience of extraction compared to savers
    ContextSaver = context(individual_moderate, biographical, constrained, national),
    ContextState = context(institutional, generational, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(monetary_regime_transition, ContextSaver, S1),
    constraint_indexing:extractiveness_for_agent(monetary_regime_transition, ContextState, S2),
    S1 > S2.

:- end_tests(monetary_regime_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVES: I mapped Gold to Mountain (perceived scarcity) and Fiat 
 * to Rope (institutional flexibility) and Snare (unintended/intended inflation).
 * * 2. OMEGAS:
 * omega_variable(fiat_durability,
 * "Can a credit-based Rope function indefinitely without collapsing into a Snare?",
 * resolution_mechanism("Wait-and-see: Analysis of civilizational debt cycles."),
 * impact("If it collapses: Fiat was always a deferred Snare. If it persists: It's a permanent Rope."),
 * confidence_without_resolution(low)
 * ).
 * * 3. AMBIGUITIES: The 'extractiveness' of fiat is highly debated. I set it 
 * at 0.5 to reflect the constant 2% target inflation which, over a 
 * 'biographical' horizon, is a significant extraction of value.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Algorithmic Scarcity (Bitcoin/Crypto)
 * Viability: A digital Mountain that mimics gold's scarcity without the 
 * physical weight.
 * Suppression: Moderate (Regulatory 'Chokepoints').
 * * ALTERNATIVE 2: Free Banking / Competing Currencies
 * Viability: Multiple Ropes competing for users.
 * Suppression: High (Legal tender laws and central bank monopolies).
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE:
 * ?- [constraint_monetary_regime].
 * ?- constraint_indexing:multi_index_report(monetary_regime_transition).
 * ?- run_tests(monetary_regime_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
