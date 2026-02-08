% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: greshams_law
 * human_readable: Gresham's Law ("Bad money drives out good")
 * domain: economic/monetary
 * temporal_scope: Permanent (Historical to Modern)
 * spatial_scope: Universal (Exchange Systems)
 * 
 * SUMMARY:
 * Gresham's Law is a monetary principle stating that if two forms of commodity 
 * money in circulation have the same face value but different intrinsic values, 
 * the "bad money" (debased) will circulate while the "good money" 
 * (high intrinsic value) will be hoarded.
 * 
 * KEY AGENTS:
 * - The Debasing Sovereign (Institutional): Lowers the metal content of coins to extract seigniorage.
 * - The Currency Hoarder (Individual Moderate): Recognizes the value difference and hoards the good money.
 * - The Subsistence Earner (Individual Powerless): Forced to accept the debased currency at face value.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(greshams_law, 0, 10).
narrative_ontology:constraint_claim(greshams_law, mountain).

% Base extractiveness: 0.6 (High)
% Rationale: It extracts "real" value (gold/silver) from the public and 
% replaces it with debased currency. The sovereign extracts seigniorage.
domain_priors:base_extractiveness(greshams_law, 0.6).

% Suppression: 0.7 (High)
% Rationale: Legal tender laws actively suppress the "market price" of the 
% good money, forcing it to be treated as equal to the bad money.
domain_priors:suppression_score(greshams_law, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.6).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.7).

% Enforcement: Emerges from self-interest once legal tender laws are enforced.
domain_priors:requires_active_enforcement(greshams_law).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(greshams_law, institutional). % The Debasing Sovereign
constraint_victim(greshams_law, powerless). % The public holding the bad money

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DEBASING SOVEREIGN - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (The state minting the currency)
   WHEN: biographical (Short-term need for funds)
   WHERE: arbitrage (Controls the composition of the currency)
   
   WHY THIS CLASSIFICATION:
   For the sovereign, the law is a 'Tangled Rope'. It's a 'Rope' because debasing
   the currency is a powerful tool to raise funds without direct taxation. It becomes
   'Tangled' because this act ultimately destroys the quality of their own currency,
   leading to long-term economic instability and loss of trust.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greshams_law,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CURRENCY HOARDER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A merchant with agency and savings)
   WHEN: biographical (Preserving wealth for a lifetime)
   WHERE: arbitrage (Can switch between what they spend and what they keep)
   
   WHY THIS CLASSIFICATION:
   For the hoarder, the law is a 'Rope'. It is a coordination mechanism they 
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUBSISTENCE EARNER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (A worker paid in debased coin)
   WHEN: immediate (Survival needs today)
   WHERE: trapped (Cannot access foreign or bullion markets)
   
   WHY THIS CLASSIFICATION:
   For the person who only earns and holds the "bad money," Gresham's Law 
   is a 'Snare'. They watch as the quality of their wages declines 
   relative to the price of real goods (inflation), but they lack the 
   surplus or access to "hoard" good money. They are trapped in a system 
   that strangles their purchasing power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greshams_law,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(greshams_law, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'Debasing Sovereign' as the required
 *    'institutional' agent. Their perspective is a 'Tangled Rope' because the
 *    act of debasement is both a useful tool and self-destructive.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Sovereign (Tangled Rope): A useful but dangerous tool.
 *    - Hoarder (Rope): A strategy for personal enrichment.
 *    - Earner (Snare): A trap that destroys their savings.
 * 
 * 3. EXTRACTIVENESS (0.6): High score because this is a classic "invisible tax."
 *    The Sovereign extracts real value (seigniorage) from the economy by forcing
 *    the circulation of inferior money.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is when the social trust in the "bad" money finally breaks.
 */

omega_variable(
    thiers_law_transition,
    "At what point of debasement does Gresham's Law flip into Thiers' Law (where 'good money' drives out bad because the bad is too untrustworthy to circulate)?",
    resolution_mechanism("Monitoring the velocity and public acceptance of a currency during hyperinflation."),
    impact("If Thiers' Law kicks in: The 'Snare' snaps, and the system collapses into a new 'Mountain' of commodity barter or a new currency."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Free Currency Competition (Hayekian model)
 *    Viability: High. If legal tender laws didn't exist, people would 
 *    discount debased coins and the "good money" would circulate at a premium.
 *    Suppression: Extreme. Modern states almost universally enforce legal 
 *    tender laws to maintain sovereign control over money.
 * 
 * CONCLUSION:
 * The existence of a suppressed alternative (free market pricing of currency) 
 * proves that the negative effects of Gresham's Law are a 'Snare' created by the 
 * state's active suppression of market reality, not an immutable 'Mountain'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/greshams_law].
 * 2. Multi-perspective: ?- multi_index_report(greshams_law).
 * 3. Run tests: ?- run_tests(greshams_law_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */