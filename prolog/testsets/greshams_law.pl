% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Economics / Monetary Theory / Sir Thomas Gresham (1519–1579)
% ============================================================================

:- module(constraint_greshams_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: greshams_law
 * human_readable: Gresham's Law ("Bad money drives out good")
 * domain: economic/monetary
 * temporal_scope: Permanent (Historical to Modern)
 * spatial_scope: Universal (Exchange Systems)
 * * SUMMARY:
 * Gresham's Law is a monetary principle stating that if two forms of commodity 
 * money in circulation have the same face value but different intrinsic values, 
 * the "bad money" (debased or overvalued) will circulate while the "good money" 
 * (high intrinsic value or undervalued) will be hoarded or exported.
 * * KEY AGENTS:
 * - The Hoarder: An individual who recognizes the "good" money's value and 
 * removes it from circulation for protection.
 * - The Debasing Sovereign: An institution that lowers the metal content of 
 * coins to extract seigniorage.
 * - The Average Merchant: Forced to accept the "bad" money at face value by 
 * legal tender laws.
 * * NARRATIVE ARC:
 * The law functions as a behavioral feedback loop. When a state enforces a 
 * fixed exchange rate between unequal currencies, it creates an arbitrage 
 * opportunity (Rope) for the savvy, while creating an inevitable drain on 
 * quality (Mountain) for the public. It becomes a Noose when the "bad money" 
 * eventually collapses in purchasing power, leaving those trapped with it 
 * in poverty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(gresham_interval, 0, 10).
narrative_ontology:constraint_claim(greshams_law, mountain).

% Base extractiveness: 0.6 (High)
% Rationale: It extracts "real" value (gold/silver) from the public and 
% replaces it with debased currency. The sovereign extracts seigniorage while 
% the hoarder extracts commodity value from the trade network.
domain_priors:base_extractiveness(greshams_law, 0.6).

% Suppression: 0.7 (High)
% Rationale: Legal tender laws actively suppress the "market price" of the 
% good money, forcing it to be treated as equal to the bad money, which 
% makes circulation of good money impossible.
domain_priors:suppression_score(greshams_law, 0.7).

% Enforcement: Emerges naturally from rational self-interest once legal 
% tender laws (active enforcement) create the initial imbalance.
domain_priors:requires_active_enforcement(greshams_law).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.6).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.7).

% Beneficiaries: The Debasing Sovereign (short-term) and Savvy Hoarders.
constraint_beneficiary(greshams_law, debasing_sovereign).
constraint_beneficiary(greshams_law, arbitrage_hoarders).

% Victims: The public, long-term savers, and those unable to exit the local 
% currency.
constraint_victim(greshams_law, unhedged_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MONETARY HISTORIAN - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of centuries).
   WHEN: historical (Viewing the cycle of debasement and collapse).
   WHERE: analytical (Not participating in the market).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To the historian, Gresham's Law is a Mountain. It is an unchangeable 
   consequence of human behavior in the face of price controls. Every 
   attempt in history to overvalue a currency has led to the same result: 
   the better currency vanishes. It is a fundamental law of economic physics.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greshams_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CURRENCY HOARDER - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A merchant with agency).
   WHEN: biographical (Preserving wealth for a lifetime).
   WHERE: arbitrage (Can switch between what they spend and what they keep).
   SCOPE: regional.
   
   WHY THIS CLASSIFICATION:
   For the hoarder, the law is a Rope. It is a coordination mechanism they 
   can use to their advantage. By spending the "bad" money at face value 
   while keeping the "good" money, they pull themselves out of the 
   systemic drain and protect their private capital.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greshams_law,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUBSISTENCE EARNER - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A worker paid in debased coin).
   WHEN: immediate (Survival needs today).
   WHERE: trapped (Cannot access foreign or bullion markets).
   SCOPE: local.
   
   WHY THIS CLASSIFICATION:
   For the person who only earns and holds the "bad money," Gresham's Law 
   is a Noose. They watch as the quality of their wages declines 
   relative to the price of real goods (inflation), but they lack the 
   surplus or access to "hoard" good money. They are trapped in a system 
   that rewards the rich for hiding value and punishes the poor for 
   circulating it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greshams_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(greshams_law, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(extraction_ratio) :-
    % Extraction is high because real commodity value is being drained.
    domain_priors:base_extractiveness(greshams_law, E),
    E >= 0.6.

test(legal_tender_suppression) :-
    % Suppression is high because legal tender laws forbid the market from pricing coins correctly.
    domain_priors:suppression_score(greshams_law, S),
    S >= 0.7.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. PERSPECTIVE SELECTION:
 * I focused on the "Hoarder" as a Rope-user to show that Gresham's Law is 
 * not just a tragedy, but a strategy for the informed individual. 
 * Contrastingly, the "Subsistence Earner" highlights the Noose aspect 
 * of being stuck in a debasing currency.
 * * 2. EXTRACTIVENESS (0.6):
 * Chose a high score because this is a classic "invisible tax." The 
 * Sovereign extracts value by circulating "cheap" money while the public's 
 * "real" purchasing power is drained.
 * * 3. AMBIGUITY:
 * Modern fiat currency presents an ambiguity—if all money is "bad" (no 
 * intrinsic value), does Gresham's Law apply? I treated the law in its 
 * classical form (intrinsic vs face value).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thiers_law_transition,
    "At what point of debasement does Gresham's Law flip into Thiers' Law (where 'good money' drives out bad because the bad is too untrustworthy to circulate)?",
    resolution_mechanism("Monitoring the velocity of a currency during hyperinflation"),
    impact("If Thiers' Law kicks in: The Noose snaps, and the system collapses into a new Mountain of gold/commodity barter."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Free Currency Competition (Hayekian model)
 * Viability: High. If legal tender laws didn't exist, people would 
 * discount debased coins and the "good money" would circulate at a 
 * premium.
 * Suppression: Extreme. Modern states almost universally enforce legal 
 * tender laws to maintain monetary control.
 * * CONCLUSION:
 * The existence of suppressed alternatives (free market pricing of 
 * currency) proves that the "Mountain" of Gresham's Law is actually a 
 * "Noose" created by the state's active suppression of market reality.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_greshams_law].
 * 2. Multi-perspective: ?- multi_index_report(greshams_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
