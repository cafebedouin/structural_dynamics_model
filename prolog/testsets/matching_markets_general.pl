% ============================================================================
% CONSTRAINT STORY: matching_markets_general
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Market Design / Alvin Roth / Lloyd Shapley
% ============================================================================

:- module(constraint_matching_markets_general, []).

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
 * constraint_id: matching_markets_general
 * human_readable: Matching Markets (Non-Commodity Exchange)
 * domain: economic/social/technological
 * temporal_scope: Biographical to Generational
 * spatial_scope: National to Global
 * 
 * SUMMARY:
 * Unlike commodity markets where price clears the market, matching markets involve 
 * "thick" preferences where the choice is mutual. You cannot simply "buy" a job 
 * at Google or a seat at Harvard; the other side must also choose you. This
 * requires a more complex mechanism than simple price-based allocation.
 * 
 * KEY AGENTS:
 * - The Unmatched Applicant (Individual Powerless): Lacks credentials to attract a match.
 * - The Institution / Clearinghouse (Institutional): Seeks to curate a specific cohort of participants.
 * - The Elite Candidate (Individual Powerful): Possesses high credentials and market value.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(matching_markets_general, 0, 10).
narrative_ontology:constraint_claim(matching_markets_general, tangled_rope).

% Base extractiveness: 0.4.
% Markets extract "signal" (effort, status, credentials) from participants 
% to facilitate the match.
domain_priors:base_extractiveness(matching_markets_general, 0.4).

% Suppression: 0.7.
% "Unmatched" alternatives (black markets, side deals) are often strictly 
% prohibited to maintain market "thickness" and stability.
domain_priors:suppression_score(matching_markets_general, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(matching_markets_general, extractiveness, 0.4).
narrative_ontology:constraint_metric(matching_markets_general, suppression_requirement, 0.7).

% Enforcement: Usually requires active enforcement (Clearinghouses/Regulations).
domain_priors:requires_active_enforcement(matching_markets_general).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(matching_markets_general, elite_participants).
constraint_victim(matching_markets_general, low_signal_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UNMATCHED APPLICANT - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Lacks credentials or signaling power)
   WHEN: immediate (During a specific matching cycle)
   WHERE: trapped (Excluded from the market due to no match)
   
   WHY THIS CLASSIFICATION:
   For the agent who fails to secure a match in a "thick" market (like medical 
   residency or kidney exchange), the system is a 'Snare'. The high 
   suppression of alternatives means that failing to match within the 
   system is equivalent to total exclusion from the profession or resource.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    matching_markets_general,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTION / CLEARINGHOUSE - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Seeks to curate a specific cohort or allocate resources)
   WHEN: historical (Over multiple matching cycles)
   WHERE: arbitrage (Balances competing preferences and allocates resources)
   
   WHY THIS CLASSIFICATION:
   For the institution or clearinghouse, the matching market is a 'Rope'. It's a
   tool to efficiently allocate resources and talent based on complex preferences,
   ensuring stable and optimal outcomes for the entire market. It helps them
   coordinate a complex system of choices.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    matching_markets_general,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observes the underlying principles of market design)
   WHEN: civilizational (Studying economic laws across different societies)
   WHERE: analytical (Universal principles of matching theory)
   
   WHY THIS CLASSIFICATION:
   The economist sees the "laws of matching" as a 'Mountain'. The need 
   for thickness, the danger of market congestion, and the inevitability 
   of signaling costs are seen as natural features of information-dense 
   environments, regardless of the specific algorithm used.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    matching_markets_general,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(matching_markets_general_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(matching_markets_general, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets_general, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets_general, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(matching_markets_general_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Institution / Clearinghouse' as the
 *    institutional agent. For them, the matching market is a 'Rope' to
 *    efficiently manage complex allocation problems.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Unmatched Applicant (Snare): Excluded from the market.
 *    - Institution (Rope): A tool for efficient allocation.
 *    - Economist (Mountain): Immutable laws of market design.
 * 
 * 3. CORE INSIGHT: Matching markets are a 'Tangled Rope'. They provide a 'Rope'
 *    for efficient allocation in non-commodity markets, but this same system
 *    creates a 'Snare' for those who fail to match, highlighting the tension
 *    between market efficiency and individual opportunity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty revolves around preference revelation and gaming the system.
 */

omega_variable(
    preference_revelation_cost,
    "Do participants reveal their true preferences, or do they strategize to 'game' the match, potentially leading to suboptimal outcomes and market instability?",
    resolution_mechanism("Comparison of algorithmically derived matches vs. post-match regret surveys; laboratory experiments on strategic behavior in matching markets."),
    impact("If Gamed: The 'Mountain' is actually a 'Scaffold' (unstable). If True: The 'Mountain' is solid, and the market is stable."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Price-Clearing Markets
 *    Viability: Effective for commodity goods, but ethically and practically problematic for things like organ transplants, jobs, and school choice.
 *    Suppression: Actively suppressed in many matching markets to prevent commodification and ensure fairness.
 *
 * CONCLUSION:
 * Matching markets are a sophisticated 'Rope' designed to navigate the ethical
 * and practical 'Snare' of price-based allocation for non-commodity goods.
 * They represent a complex solution to a 'Mountain' of social coordination problems.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/matching_markets_general].
 * 2. Multi-perspective: ?- multi_index_report(matching_markets_general).
 * 3. Run tests: ?- run_tests(matching_markets_general_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(matching_markets_general, 0.31).
narrative_ontology:constraint_metric(matching_markets_general, theater_ratio, 0.31).
