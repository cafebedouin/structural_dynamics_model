% ============================================================================
% CONSTRAINT STORY: technological_provenance_arbitrage
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Michael Marshall, "Revealing the epic story of ancient humans"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_technological_provenance_arbitrage, []).

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
 * * constraint_id: technological_provenance_arbitrage
 * human_readable: Technological Provenance Arbitrage
 * domain: technological/scientific
 * temporal_scope: 21st Century (2000-2026)
 * spatial_scope: Global (Archaeological sites and Advanced Labs)
 * * SUMMARY:
 * This constraint describes the shift from finding "more" fossils to extracting "more" information 
 * using advanced analytical techniques that were unavailable in the 20th century. 
 * It allows researchers to re-examine legacy remains—like Orrorin or Ardipithecus—to arbitrage 
 * and rewrite the timeline of human origins.
 * * KEY AGENTS:
 * - The Analytical Observer (Modern Archaeologist): Uses "more and better techniques" to analyze remains.
 * - The Legacy Fossil: Ancient remains (e.g., Orrorin praegens) subject to new analysis decades after discovery.
 * - The Institutional Lab: Possesses the sequencing and imaging power required to join the "revolution".
 * * NARRATIVE ARC:
 * The field has shifted from a "Mountain" of sparse fossil data to an "Information Blizzard". 
 * This blizzard acts as a "Rope" for those with the technological means to climb into deep time 
 * (back to 7 million years ago), but acts as a "Snare" for traditional methodologies that cannot 
 * process this scale of data.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(technological_provenance_arbitrage, 0, 10).
narrative_ontology:constraint_claim([technological_provenance_arbitrage], [information_extraction]).

% Base extractiveness score (0.78 = High)
% Rationale: High extractiveness is driven by the "huge amount of information" now being 
% pulled from singular fossils like H. naledi or diminutive "hobbits".
domain_priors:base_extractiveness(technological_provenance_arbitrage, 0.78).

% Suppression score (0.5 = Moderate)
% Rationale: Previous linear models (Ardipithecus as the 4.4mya limit) were suppressed 
% by the sudden "swathe of surprising archaeological discoveries".
domain_priors:suppression_score(technological_provenance_arbitrage, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(technological_provenance_arbitrage, extractiveness, 0.78).
narrative_ontology:constraint_metric(technological_provenance_arbitrage, suppression_requirement, 0.5).

% Enforcement requirements: Requires active enforcement (Advanced lab protocols and genetic sequencing standards).
domain_priors:requires_active_enforcement(technological_provenance_arbitrage).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
% Researchers with "more and better techniques" who can describe new species quietly.
constraint_beneficiary(technological_provenance_arbitrage, high_tech_paleoanthropologists).
% Traditional researchers or underfunded institutions "replaced" by the information blizzard.
constraint_victim(technological_provenance_arbitrage, traditional_methodologists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MODERN LAB RESEARCHER - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer using "better techniques" to analyse remains)
   WHEN: biographical (The 25-year period of "more" discoveries)
   WHERE: arbitrage (Playing new analytical data against old fossil records)
   SCOPE: global (Accessing more places and fossils)
   
   WHY THIS CLASSIFICATION:
   For the modern researcher, the "swathe of surprising discoveries" is a Rope—a functional tool 
   to build a richer story of how modern humans emerged from the hominin pack.
   
   NARRATIVE EVIDENCE:
   "Researchers have developed more and better techniques for analysing all these remains".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    technological_provenance_arbitrage,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(technological_provenance_arbitrage, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRADITIONAL EXCAVATOR - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Unable to compete with the "blizzard" of lab data)
   WHEN: immediate (Current pressure to produce high-tech results)
   WHERE: trapped (Relies on physical fossils without digital/genetic arbitrage)
   SCOPE: local (Limited to specific fossil sites)
   
   WHY THIS CLASSIFICATION:
   For those without the latest technology, the information blizzard is a Snare. It extracts 
   authority from physical fossil discovery and places it into the hands of analytical labs.
   
   NARRATIVE EVIDENCE:
   "There is, quite simply, a huge amount of information about our origins... in more places".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    technological_provenance_arbitrage,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(technological_provenance_arbitrage, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANCIENT HUMAN LINEAGE - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (The unchangeable biological fact of Orrorin/Ardipithecus)
   WHEN: civilizational (7 million years of evolutionary time)
   WHERE: constrained (Bound by physical remains in deep caves)
   SCOPE: global (The total history of hominins)
   
   WHY THIS CLASSIFICATION:
   The fossil record is a biological Mountain—it exists whether we have the "more" 
   techniques to see it or not. The information is there, waiting for the "revolution".
   
   NARRATIVE EVIDENCE:
   "The hominin fossil record has been extended much further back in time... Orrorin tugenensis 
   from 6 million years ago and Sahelanthropus tchadensis from 7 million years ago".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    technological_provenance_arbitrage,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(technological_provenance_arbitrage, S),
    S > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(technological_provenance_arbitrage_tests).

test(multi_perspective_variance) :-
    % Lab Researcher (Rope) vs Traditional Excavator (Snare) vs Hominin Lineage (Mountain)
    constraint_indexing:constraint_classification(technological_provenance_arbitrage, rope, context(analytical, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(technological_provenance_arbitrage, snare, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(technological_provenance_arbitrage, mountain, context(institutional, civilizational, constrained, global)).

test(information_density_scaling) :-
    % High tech researchers experience lower extraction of their labor 
    % while physical fossils experience total informational extraction.
    Score1 = 0.78,
    Score2 = 0.20,
    Score1 > Score2.

:- end_tests(technological_provenance_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.78):
 * Reasoning: Chosen because the "revolution" is defined by getting "huge amounts" 
 * of data out of sparse physical remains.
 * * 2. SUPPRESSION SCORE (0.50):
 * Reasoning: Reflects how the 20th-century Ardipithecus limit was a conceptual 
 * boundary that has only recently been "shattered".
 * * 3. PERSPECTIVE SELECTION:
 * Used the "Traditional Excavator" to show the Snare effect when data-driven 
 * "blizzards" replace site-based authority.
 * * 4. AMBIGUITIES:
 * - The "quiet description" of species in 2022 suggests a move toward 
 * specialized expertise arbitrage that excludes the public or generalists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    technological_provenance_arbitrage_extraction_intent,
    "Is the 'huge amount of information' a purely scientific gain (Rope) or a form of technological colonization over fossil heritage (Snare)?",
    resolution_mechanism("Audit of data ownership vs. site-of-origin rights for genetic sequencing results"),
    impact("If scientific: Rope. If exclusionary: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    data_authenticity_blizzard,
    "Will the 'blizzard' of information eventually lead to noise that obscures the 'true' evolutionary path?",
    resolution_mechanism("Long-term replication of automated analytical fossil techniques across independent labs"),
    impact("If noise: Snare (false patterns). If signal: Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Physical Morphological Analysis (The 20th-Century Standard)
 * Viability: The standard way fossils were analyzed for 100 years.
 * Suppression: Rapidly being "replaced" by DNA and high-res CT scans.
 * * CONCLUSION:
 * The 21st-century "Best Idea" in archaeology is the transition from 
 * fossil-as-object (Mountain) to fossil-as-data-source (Rope).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/technological_provenance_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(technological_provenance_arbitrage).
 * 3. Run tests: ?- run_tests(technological_provenance_arbitrage_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
