% ============================================================================
% CONSTRAINT STORY: exoplanetary_habitability_arbitrage
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Alex Wilkins, "Our solar system is extremely weird"
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_exoplanetary_habitability_arbitrage, []).

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
 * * constraint_id: exoplanetary_habitability_arbitrage
 * human_readable: Exoplanetary Habitability Arbitrage
 * domain: technological/scientific
 * temporal_scope: 2000s to 2026+
 * spatial_scope: Global (Astrophysics Research Labs)
 * * SUMMARY:
 * This constraint represents the strategic pivot in the search for extraterrestrial life, 
 * moving from the search for "Earth twins" to identifying anomalies based on our 
 * solar system's "weirdness". Researchers arbitrage the 
 * gap between the rare 1-in-10 occurrence of Jupiter-sized planets and the common 
 * "super-Earth" systems to prioritize high-cost observation targets.
 * * KEY AGENTS:
 * - The Exobiologist: Analytical observer seeking to "rewrite the story" of formation to find life.
 * - The Legacy Planet Hunter: Agent traditionally focused on sun-like stars and 1:1 Earth analogs.
 * - The Habitable System: The physical reality where orbit circularity and stellar neighbor absence define "weirdness".
 * * NARRATIVE ARC:
 * The discovery that our solar system is a potential "one in a million" fluke shifts 
 * habitability from a "Mountain" of inevitable discovery to a "Rope" of highly 
 * selective, data-driven arbitrage.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(exoplanetary_habitability_arbitrage, 0, 10).
narrative_ontology:constraint_claim([exoplanetary_habitability_arbitrage], [scientific_prioritization]).

% Base extractiveness score (0.45 = Moderate)
% Rationale: High-cost missions like the Habitable Worlds Observatory extract 
% resources from general astronomy to fund specialized searches for "weird" flukes.
domain_priors:base_extractiveness(exoplanetary_habitability_arbitrage, 0.45).

% Suppression score (0.55 = Moderate)
% Rationale: The assumption of our system being "normal" previously suppressed 
% the search for "weird" trajectories and super-Earth anomalies.
domain_priors:suppression_score(exoplanetary_habitability_arbitrage, 0.55).

% Enforcement requirements
% Emerges naturally from the statistical rarity of our system's architecture.
domain_priors:emerges_naturally(exoplanetary_habitability_arbitrage).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, extractiveness, 0.45).
narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, suppression_requirement, 0.55).

% BENEFICIARIES & VICTIMS
% Modern planetary theorists benefit from the "rewrite" of formation stories.
constraint_beneficiary(exoplanetary_habitability_arbitrage, planetary_theorists).
% Traditional "Earth twin" hunters lose funding as the search pivots to anomalies.
constraint_victim(exoplanetary_habitability_arbitrage, legacy_astronomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXOBIOLOGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer using "large-scale, long-term surveys")
   WHEN: biographical (Tracking orbital wobbles since the early 2000s)
   WHERE: arbitrage (Playing "weird" system features against common exoplanet data)
   SCOPE: global (Drawing on data from telescopes like Kepler and TESS)
   
   WHY THIS CLASSIFICATION:
   For the exobiologist, habitability arbitrage is a Rope—a functional tool to 
   refine the "one in a million" search and locate life in a galaxy full of super-Earths.
   
   NARRATIVE EVIDENCE:
   "Putting those together, we’re definitely weird... helped astronomers rewrite 
   the story of how it formed".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    exoplanetary_habitability_arbitrage,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(exoplanetary_habitability_arbitrage, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LEGACY PLANET HUNTER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to the pivot away from "typicality")
   WHEN: immediate (Addressing current unmet expectations of finding Earth twins)
   WHERE: trapped (Committed to searching for Earth-like planets around sun-like stars)
   SCOPE: national (Restricted by funding for traditional "typical" models)
   
   WHY THIS CLASSIFICATION:
   For those hunting for an "Earth 2.0" using old models, the "weirdness" finding 
   is a Snare—it reveals that their search target might not even exist.
   
   NARRATIVE EVIDENCE:
   "We have yet to spot an Earth-like planet around a sun-like star... we’re 
   missing planets common to most other star systems".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    exoplanetary_habitability_arbitrage,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(exoplanetary_habitability_arbitrage, E),
    E > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: RARE EARTH PROPONENT - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (Viewing "weirdness" as an immutable biological prerequisite)
   WHEN: civilizational (The deep-time fluke of Earth's formation)
   WHERE: constrained (Bound by the physical lack of alien life so far)
   SCOPE: global (The total absence of detected aliens in our galaxy)
   
   WHY THIS CLASSIFICATION:
   To the Rare Earth proponent, the system's architecture is a Mountain—an 
   unchangeable biological necessity that explains why we have yet to find life.
   
   NARRATIVE EVIDENCE:
   "We have yet to spot an Earth-like planet... not to mention alien life... 
   we’re definitely weird".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    exoplanetary_habitability_arbitrage,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    % Classification based on the perception of "weirdness" as fate
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(exoplanetary_habitability_arbitrage_tests).

test(multi_perspective_variance) :-
    % Exobiologist (Rope) vs Hunter (Snare) vs Rare Earth (Mountain)
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, rope, context(analytical, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, snare, context(individual_powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, mountain, context(institutional, civilizational, constrained, global)).

test(power_extractiveness_scaling) :-
    % Funding extraction for new "weird" telescopes (0.45) affects legacy researchers.
    Score1 = 0.45,
    Score2 = 0.1,
    Score1 > Score2.

:- end_tests(exoplanetary_habitability_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.45):
 * Reasoning: Chosen because the "weirdness" pivot reallocates significant 
 * informational and financial resources toward anomaly search.
 * * 2. PERSPECTIVE SELECTION:
 * Chose to analyze from the Exobiologist (Rope), Legacy Hunter (Snare), and 
 * Rare Earth proponent (Mountain) to illustrate how the same data 
 * signifies tool, trap, and fate.
 * * 3. AMBIGUITIES:
 * The text is uncertain if we are weird at the "1 per cent" or "1 in a million" 
 * level; this is handled via an Omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exoplanetary_habitability_arbitrage_extraction_intent,
    "Is the focus on 'weird' systems a functional necessity for finding life or a strategic choice to sustain high-tech funding?",
    resolution_mechanism("Audit of discovery rates for habitable worlds in 'weird' vs 'typical' star systems"),
    impact("If necessity: Mountain. If strategic choice: Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    weirdness_frequency_calibration,
    "Is the solar system's weirdness a 1-in-100 fluke or a 1-in-a-million miracle?",
    resolution_mechanism("Completion of large-scale, long-term surveys of G-type stars for Earth-mass planets"),
    impact("If 1%: Rope (manageable search). If 1-in-a-million: Mountain (likely we are alone)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Copernican "Earth Twin" Search
 * Viability: The widely believed paradigm that Earth is a typical planet.
 * Suppression: Now being challenged by data showing our sun and planets are 
 * outliers.
 * * CONCLUSION:
 * The shift from "Typicality" to "Arbitrage" creates a Rope for scientists 
 * but a Snare for legacy theories that can no longer explain the absence 
 * of Earth twins.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/exoplanetary_habitability_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(exoplanetary_habitability_arbitrage).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
