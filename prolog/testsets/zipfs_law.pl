% ============================================================================
% CONSTRAINT STORY: zipfs_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: George Kingsley Zipf (1949) / Quantitative Linguistics
% ============================================================================

:- module(constraint_zipfs_law, []).

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
 * * constraint_id: zipfs_law
 * human_readable: Zipf's Law (Word Frequency)
 * domain: technological/social
 * temporal_scope: Permanent (Universal Law of Information)
 * spatial_scope: Global (All Natural Languages and Large Data Sets)
 * * SUMMARY:
 * Zipf's Law states that in a given corpus of natural language, the frequency 
 * of any word is inversely proportional to its rank in the frequency table. 
 * The most frequent word will occur twice as often as the second most 
 * frequent, three times as often as the third, and so on. This power-law 
 * distribution governs not just words, but city sizes, income distributions, 
 * and internet traffic.
 * * KEY AGENTS:
 * - The Linguist: Analytical observer mapping the "Long Tail" of human 
 * communication and the Principle of Least Effort.
 * - The Search Engine Architect: Institutional agent using the law to 
 * optimize indexing, caching, and compression for the few "head" terms.
 * - The Minority Voice/Long-Tail Content Creator: Individual powerless 
 * agent whose content is mathematically "suppressed" by the extreme dominance 
 * of the high-rank head terms.
 * * NARRATIVE ARC:
 * Zipf's Law functions as a "Mountain" of statistical reality—a byproduct 
 * of efficiency in complex systems. For the engineer, it is a "Rope" for 
 * coordination (allowing for efficient data compression). However, for the 
 * creator in the "Long Tail," it is a "Noose" that strangles visibility, 
 * as the winner-take-all nature of the distribution extracts attention 
 * and redirects it to the already-dominant head.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification
narrative_ontology:interval(zipf_interval, 0, 10).
narrative_ontology:constraint_claim(zipfs_law, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). Zipf's Law extracts "attention" and "visibility" 
% from the many (the tail) and concentrates it in the few (the head). 
% It is the mathematical foundation of "The Matthew Effect."
domain_priors:base_extractiveness(zipfs_law, 0.5).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). The power-law distribution effectively suppresses 
% the "Equal Distribution" alternative. In a Zipfian world, 50% of 
% information is often invisible to the average observer because it is 
% hidden in the low-frequency tail.
domain_priors:suppression_score(zipfs_law, 0.7).

% Enforcement requirements
% Emerges naturally from the "Principle of Least Effort" in complex systems.
domain_priors:emerges_naturally(zipfs_law).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(zipfs_law, extractiveness, 0.5).
narrative_ontology:constraint_metric(zipfs_law, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(zipfs_law, [search_engines, dominant_languages, head_terms]).
constraint_victim(zipfs_law, [minority_languages, niche_creators, tail_information]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DATA SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the universal laws of information.
   WHEN: civilizational - Viewing the distribution as a permanent feature of data.
   WHERE: trapped - Large datasets inevitably settle into this distribution.
   SCOPE: global - Universal across all known large-scale systems.
   
   WHY THIS CLASSIFICATION:
   To the scientist, Zipf's Law is a Mountain. It is an unchangeable 
   feature of "information hardware." Whether you look at ancient Greek, 
   modern English, or the city populations of Mars, the power-law curve 
   is a fixed peak in the statistical topography of the universe.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    zipfs_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GOOGLE ENGINEER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design algorithms that handle global data.
   WHEN: biographical - Optimizing a search engine over a career.
   WHERE: arbitrage - Can choose how to treat the head vs. the tail.
   SCOPE: global - Managing the world's information.
   
   WHY THIS CLASSIFICATION:
   For the engineer, Zipf's Law is a Rope. It is a coordination mechanism 
   for extreme efficiency. By knowing that a handful of words (the, of, and) 
   make up the vast majority of traffic, they can "tether" their 
   infrastructure to those head terms, using the constraint to pull 
   global search results into existence with minimal latency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    zipfs_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(zipfs_law, E),
    E > 0.3, % Leverage the extraction for performance
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NICHE CONTENT CREATOR - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "algorithm" and attention limits.
   WHEN: immediate - Today's struggle for views and engagement.
   WHERE: constrained - High cost to compete with "head" category dominance.
   SCOPE: local - Immediate professional viability.
   
   WHY THIS CLASSIFICATION:
   For a creator in a niche topic, Zipf's Law is a Noose. The 
   mathematical distribution of human attention ensures that the 
   "head" captures nearly everything. The Noose of obscurity tightens 
   as the power-law extracts their potential audience, strangling 
   their ability to survive without becoming a generic "head" term.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    zipfs_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(zipfs_law, E),
    E > 0.4, % Extraction of visibility and viability
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(zipfs_law_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(zipfs_law, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(zipfs_law, rope, context(institutional, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(zipfs_law, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_attention) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, global),
    constraint_indexing:extractiveness_for_agent(zipfs_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(zipfs_law, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(zipfs_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): Zipf's Law extracts "centrality." It forces 
 * a "rich-get-richer" dynamic where the most common elements are 
 * disproportionately prioritized.
 * 2. SUPPRESSION (0.7): High. It suppresses the idea of an "equal 
 * playing field." In any Zipfian system, a tiny minority of items 
 * will always be orders of magnitude more visible than the majority.
 * 3. NOOSE LOGIC: Specifically focuses on "The Long Tail" as a trap 
 * for those whose survival depends on visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_reversal,
    "Can algorithmic recommendation systems 'untie' the Noose of Zipf's Law 
    by artificially boosting the Long Tail (Rope), or do they inevitably 
    reinforce it via feedback loops (Mountain)?",
    resolution_mechanism("Long-term analysis of platform diversity metrics 
    under different recommendation architectures"),
    impact("If Rope: The distribution of success can be socialized. 
    If Mountain: Inequality is a law of information physics."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Normal Distribution (Bell Curve)
 * Viability: High in physical measurements (height, weight), but 
 * non-existent in communicative and social systems.
 * Suppression: High. The "Long Tail" reality suppresses the bell-curve 
 * fantasy in marketing and search.
 * * CONCLUSION:
 * Since language and social systems refuse to follow a Bell Curve, Zipf's Law 
 * is a Mountain of reality that creates a Noose of obscurity for the tail.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_zipfs_law].
 * 2. Multi-perspective: ?- multi_index_report(zipfs_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
