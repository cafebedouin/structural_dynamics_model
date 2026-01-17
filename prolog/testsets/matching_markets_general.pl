% ============================================================================
% CONSTRAINT STORY: matching_markets_general
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Market Design / Alvin Roth / Lloyd Shapley
% ============================================================================

:- module(constraint_matching_markets, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: matching_markets_general
 * human_readable: Matching Markets (Non-Commodity Exchange)
 * domain: economic/social/technological
 * temporal_scope: Biographical to Generational
 * spatial_scope: National to Global
 * * SUMMARY:
 * Unlike commodity markets where price clears the market (whoever pays the 
 * most gets the goods), matching markets involve "thick" preferences where 
 * the choice is mutual. You cannot simply "buy" a job at Google or a seat 
 * at Harvard; the other side must also choose you.
 * * KEY AGENTS:
 * - The Applicant: Seeks to be matched with a high-value institution.
 * - The Institution: Seeks to curate a specific cohort of participants.
 * - The Market Architect: Designs the rules (clearinghouses) that govern 
 * how matches are finalized.
 * * NARRATIVE ARC:
 * The constraint functions as a "Gatekeeper's Maze." Success depends not 
 * just on your own resources, but on the visibility of your preferences 
 * and the specific algorithmic or social rules used to pair "sides."
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(matching_markets_general, 0, 10).
narrative_ontology:constraint_claim(matching_markets_general, structural_coordination).

% Base extractiveness: Moderate (0.4). 
% Markets extract "signal" (effort, status, credentials) from participants 
% to facilitate the match.
domain_priors:base_extractiveness(matching_markets_general, 0.4).

% Suppression: High (0.7).
% "Unmatched" alternatives (black markets, side deals) are often strictly 
% prohibited to maintain market "thickness" and stability.
domain_priors:suppression_score(matching_markets_general, 0.7).

% Enforcement: Usually requires active enforcement (Clearinghouses/Regulations).
domain_priors:requires_active_enforcement(matching_markets_general).

% Beneficiaries: The Highly-Ranked (who get their top choices on both sides).
constraint_beneficiary(matching_markets_general, elite_participants).

% Victims: The "Thin" Participants (who lack the credentials or signaling power 
% to attract a match, leaving them stranded).
constraint_victim(matching_markets_general, low_signal_agents).

% Metrics
narrative_ontology:constraint_metric(matching_markets_general, extractiveness, 0.4).
narrative_ontology:constraint_metric(matching_markets_general, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ELITE CANDIDATE - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: biographical
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For an elite agent (high credentials), the matching market is a "Rope." 
   It provides a structured, predictable ladder to a high-status outcome. 
   The market's rules protect them from the "chaos" of unregulated 
   negotiation and ensure their value is recognized.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    matching_markets_general,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(matching_markets_general, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNMATCHED APPLICANT - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the agent who fails to secure a match in a "thick" market (like medical 
   residency or kidney exchange), the system is a "Noose." The high 
   suppression of alternatives means that failing to match within the 
   system is equivalent to total exclusion from the profession or resource.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    matching_markets_general,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(matching_markets_general, S),
    S > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The economist sees the "laws of matching" as a "Mountain." The need 
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
) :-
    domain_priors:emerges_naturally(matching_markets_general),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(matching_markets_tests).

test(signal_power_variance) :-
    % Powerful agents navigate the rope; powerless agents are caught in the noose.
    constraint_indexing:constraint_classification(matching_markets_general, rope, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets_general, noose, context(agent_power(individual_powerless), _, trapped, _)).

test(market_failure_threshold) :-
    % High suppression (S > 0.6) forces a Noose classification for trapped agents.
    domain_priors:suppression_score(matching_markets_general, S),
    S > 0.6.

:- end_tests(matching_markets_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Matching markets differ from auctions because they are about "who gets what" 
 * AND "who gets whom." I set suppression to 0.7 because centralized matching 
 * markets (like the NRMP for doctors) often penalize or ban side-deals, 
 * making the central match a mandatory bottleneck (Noose/Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    preference_revelation_cost,
    "Do participants hide their true preferences to 'game' the match?",
    resolution_mechanism("Comparison of algorithmically derived matches vs. post-match regret surveys"),
    impact("If Gamed: The 'Mountain' is actually a 'Scaffold' (unstable). If True: The 'Mountain' is solid."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Price-Clearing Markets: (Ineffective for kidneys/jobs due to ethics/signaling).
 * 2. Decentralized Signaling: (Causes market 'congestion' and delayed offers).
 * * CONCLUSION:
 * The "Noose" aspect of matching markets is often a trade-off for "Market 
 * Thickness." By forcing everyone into one room (the Noose), you ensure the 
 * highest probability that a match exists (the Rope).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/matching_markets_general].
 * 2. Run: ?- multi_index_report(matching_markets_general).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
