% ============================================================================
% CONSTRAINT STORY: scurvy_maritime_extraction
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Historical Maritime Records / Medical History
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_scurvy_maritime_extraction, []).

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
 * * constraint_id: scurvy_maritime_extraction
 * human_readable: The Scurvy/Empire Trade-off
 * domain: biological/political/technological
 * temporal_scope: 15th - 18th Century (Age of Sail)
 * spatial_scope: Global Maritime Routes
 * * SUMMARY:
 * Scurvy (Vitamin C deficiency) was the primary constraint on global maritime expansion during the Age of Sail. 
 * While the cause (lack of fresh produce) and cure (citrus) were periodically discovered and lost, 
 * the disease functioned as a lethal filter that extracted the lives of "little people" (sailors) 
 * to fuel the expansion of Empires. It represents a "Machinery of Justice" where 
 * institutional inertia and biological ignorance created a terminal "Snare" for the powerless.
 * * KEY AGENTS:
 * - The Common Sailor (Little Person): Subject to the "Snare" of the ship's diet; unable to 
 * procure fresh food at sea.
 * - The Admiralty/Empire (Institutional): Benefits from the "Rope" of long-distance trade 
 * and colonization, treating sailor mortality as a predictable "cost of business."
 * - James Lind / The Scientist (Analytical): Observer who discovers the "Rope" of 
 * citrus but is often ignored by the institutional "Mountain."
 * * NARRATIVE ARC:
 * Scurvy was an "apocalypse" within the "utopia" of global discovery. It turned the 
 * ship into a "Black Iron Prison" where the "rules of the game" (diet) were fixed by 
 * institutional elites. The resolution came not from "magic" but from the 
 * eventual institutional adoption of citrus—a transition from an unchangeable 
 * "Mountain" of fate to a manageable "Rope" of nutrition.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(scurvy_maritime_interval, 0, 10).
narrative_ontology:constraint_claim([scurvy_maritime_extraction], [biological_economic_trap]).

% Base extractiveness score (0.9 = High)
% Rationale: Scurvy extracted the literal lives of millions of sailors. Between 
% 1500 and 1800, it is estimated that scurvy killed more sailors than all other 
% diseases, naval battles, and shipwrecks combined.
domain_priors:base_extractiveness(scurvy_maritime_extraction, 0.9).

% Suppression score (0.7 = High)
% Rationale: The cure (citrus) was discovered multiple times (e.g., by Lancaster in 1601, 
% Lind in 1747) but was suppressed by institutional inertia, costs, and competing 
% "shitty feedback" (theories about bad air or laziness).
domain_priors:suppression_score(scurvy_maritime_extraction, 0.7).

% Enforcement: Emerges naturally from biological limits but is actively maintained 
% by the institutional "Machinery" of naval diet standards.
domain_priors:requires_active_enforcement(scurvy_maritime_extraction).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(scurvy_maritime_extraction, extractiveness, 0.9).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(scurvy_maritime_extraction, maritime_empires). % Expansion fueled by disposable labor
constraint_victim(scurvy_maritime_extraction, common_sailors).        % Terminal extraction of life

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SAILOR - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "little person" with no control over ship stores)
   WHEN: immediate (Short-term survival during a 6-month voyage)
   WHERE: trapped (Physically located on a ship with no exit options)
   SCOPE: global (The "Empty Quarter" of the ocean)
   
   WHY THIS CLASSIFICATION:
   For the sailor, scurvy is a "Snare." They are "caught up in the churn" of imperial 
   expansion where their death "doesn't really mean anything" to the system. 
   They have "nothing to hide" but their failing health is treated as a 
   lack of "merit" or "grit."
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    scurvy_maritime_extraction,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ADMIRALTY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (The "players" who make the deals)
   WHEN: historical (Managing centuries of maritime dominance)
   WHERE: mobile (Can choose to implement lime juice or ignore it)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the institution, the risk of scurvy is a "Rope"—a manageable coordination 
   problem. They "cream the liquidation" of sailors with the excuse that it is 
   "just business" or "the way of the world," until the cost of replacing 
   trained men exceeds the cost of citrus.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    scurvy_maritime_extraction,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MEDICAL ANALYST (James Lind) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer who discovered the cure)
   WHEN: civilizational (Observing the biological law of Vitamin C)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, the link between citrus and recovery is a "Mountain"—an 
   immutable biological law. It isn't "magic"; it's a zero-degree-of-freedom 
   requirement for human survival that the institutional "Barkers" 
   choose to ignore.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    scurvy_maritime_extraction,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(scurvy_maritime_extraction_tests).

test(multi_perspective_asymmetry) :-
    % Sailor (Powerless) sees Snare
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, snare, context(individual_powerless, _, _, _)),
    % Admiralty (Institutional) sees Rope
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, rope, context(institutional, _, _, _)),
    % Scientist (Analytical) sees Mountain
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, mountain, context(analytical, _, _, _)).

test(extraction_mortality_scaling) :-
    % High extraction (0.9) indicates the "liquidation" of the little people.
    domain_priors:base_extractiveness(scurvy_maritime_extraction, E),
    E > 0.8.

:- end_tests(scurvy_maritime_extraction_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.9):
 * The cost of scurvy was the literal life of the subject. Empires built wealth 
 * on a "churn" of sailors who were essentially "loose ends."
 * * 2. SUPPRESSION SCORE (0.7):
 * Institutional "Barkers" in the Navy suppressed the simple discovery of citrus 
 * for decades in favor of complex, "monosophical" theories that didn't work.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The "Rope" of imperial expansion was 
 * explicitly built upon the "Snare" of sailor mortality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_ignorance_intent,
    "Was the 42-year delay between Lind's discovery and Navy implementation a 
     functional necessity (Mountain) or a predatory choice to save money (Snare)?",
    resolution_mechanism("Audit of British Admiralty cost-benefit logs regarding lime juice vs. sailor replacement costs"),
    impact("If necessity: Institutional Mountain. If choice: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    scurvy_genetic_drift,
    "Could humans have re-evolved Vitamin C synthesis (Rope) if given enough 
     generations of maritime exposure, or is the loss an absolute Mountain?",
    resolution_mechanism("Genetic analysis of human GULO pseudogene mutation rates"),
    impact("If possible: Biological Rope. If impossible: Biological Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: James Lancaster's Lemon Juice (1601)
 * Viability: Successfully prevented scurvy on the first East India Company voyage.
 * Suppression: Ignored by the "Machinery" because it was logistically 
 * cumbersome and the "experts" had other stories.
 * * ALTERNATIVE 2: Sauerkraut (Cook's Method)
 * Viability: James Cook used it to keep his crew scurvy-free.
 * Suppression: Not widely adopted because it required "active enforcement" 
 * of a diet that sailors (the subjects) initially rejected.
 * * CONCLUSION:
 * The existence of Alternative 1 for over 150 years before adoption makes the 
 * maritime diet a definitive "Snare."
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [scurvy_maritime_extraction].
% Multi-perspective: ?- multi_index_report(scurvy_maritime_extraction).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
