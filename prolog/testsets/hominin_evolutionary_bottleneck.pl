% ============================================================================
% CONSTRAINT STORY: hominin_evolutionary_bottleneck
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Michael Marshall, "Revealing the epic story of ancient humans" 
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_hominin_evolutionary_bottleneck, []).

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
 * 
 * constraint_id: hominin_evolutionary_bottleneck
 * human_readable: The Hominin Evolutionary Bottleneck
 * domain: scientific/biological
 * temporal_scope: 7 million years ago to 60,000 years ago
 * spatial_scope: Global (via replacement)
 * 
 * SUMMARY:
 * For millennia, the story of human evolution was constrained by a sparse fossil record. 
 * Recent "blizzards" of fossil and genetic data have revealed a complex history where 
 * our direct ancestors expanded out of Africa ~60,000 years ago, replacing all other 
 * hominin species. This event acts as a constraint with vastly different meanings 
 * depending on the perspective.
 * 
 * KEY AGENTS:
 * - Extinct Hominin (Individual Powerless): Species like H. naledi that were replaced.
 * - Modern Scientist (Analytical): Synthesizes genetic and fossil data.
 * - Natural History Museum (Institutional): Curates and presents the scientific narrative to the public.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(hominin_evolutionary_bottleneck, 0, 10).
narrative_ontology:constraint_claim(hominin_evolutionary_bottleneck, snare).

% Base extractiveness score (0.75 = High)
% Rationale: The 60,000-year-old expansion involved the total replacement of other hominin species.
% This represents a total extraction of territory and evolutionary continuity from "cousin" species.
domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, 0.75).

% Suppression score (0.60 = Moderate-High)
% Rationale: The dominant "Out of Africa" theory, backed by strong evidence, has suppressed 
% alternative "multiregional" hypotheses.
domain_priors:suppression_score(hominin_evolutionary_bottleneck, 0.60).

% Enforcement requirements: Emerges naturally through biological competition and environmental pressures.
domain_priors:emerges_naturally(hominin_evolutionary_bottleneck).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hominin_evolutionary_bottleneck, homo_sapiens).
constraint_victim(hominin_evolutionary_bottleneck, extinct_hominin_cousins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: EXTINCT COUSIN (e.g., Homo naledi) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Biological species subject to replacement) 
   WHEN: civilizational (Deep-time evolutionary scale) 
   WHERE: trapped (Isolated in deep caves or on islands) 
   
   WHY THIS CLASSIFICATION:
   For species like H. naledi, the evolutionary expansion of Homo sapiens was a terminal 'Snare'.
   They were confined to specific environments and ultimately driven to extinction by a competing group.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hominin_evolutionary_bottleneck,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN SCIENTIST - Rope
   --------------------------------------------------------------------------
   WHO: analytical (Observer using genetic and fossil data) 
   WHEN: biographical (The 25-year "revolution" in understanding) 
   WHERE: arbitrage (Ability to play fossil data against genetic evidence) 
   
   WHY THIS CLASSIFICATION:
   For the scientist, the bottleneck and expansion is a 'Rope'—a powerful explanatory
   framework that ties together disparate data points into a coherent narrative about human origins.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hominin_evolutionary_bottleneck,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: NATURAL HISTORY MUSEUM (INSTITUTIONAL) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Curator of the public narrative)
   WHEN: generational (The lifespan of a museum exhibit)
   WHERE: constrained (Must present a simplified, clear story)
   
   WHY THIS CLASSIFICATION:
   The museum presents the scientific consensus as a 'Mountain'. The complex, debated 'Rope' 
   of the scientists is simplified into an immutable, factual diorama for public consumption. 
   The nuances are flattened into a linear, authoritative story.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hominin_evolutionary_bottleneck,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hominin_evolutionary_bottleneck_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type2, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(civilizational), exit_options(trapped), spatial_scope(regional)),
    ContextPowerful = context(agent_power(analytical), time_horizon(biographical), exit_options(arbitrage), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(hominin_evolutionary_bottleneck, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(hominin_evolutionary_bottleneck, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(hominin_evolutionary_bottleneck_tests).

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
 * 1. EXTRACTIVENESS SCORE (0.75):
 *    Reasoning: Chosen because the "Replacement" theory describes a total biological extraction event where one group displaces all others, taking their territory and evolutionary future. This is a high-extraction event.
 * 
 * 2. INSTITUTIONAL PERSPECTIVE:
 *    Added the 'Natural History Museum' as the institutional agent. This is a common pattern where a complex scientific reality ('Rope') is simplified into an authoritative public narrative ('Mountain').
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    - Extinct Cousin -> Snare: They were biologically replaced.
 *    - Scientist -> Rope: The theory is a tool for organizing data.
 *    - Museum -> Mountain: The theory is presented as an immutable fact to the public.
 * 
 * 4. CONFIDENCE:
 *    High. The model of scientific consensus hardening into public fact is a very strong fit for the institutional 'Mountain' perspective. The 'Snare' of extinction is self-evident.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    hominin_evolutionary_bottleneck_extraction_intent,
    "Is the 60,000-year expansion a predatory biological displacement (Snare) or a natural adaptive shift (Mountain)?",
    resolution_mechanism("Audit of genetic admixture rates between Sapiens and Cousins to determine if 'replacement' was violent or absorptive"),
    impact("If predatory: Snare/Mandatrophy. If adaptive: Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    undiscovered_cousins,
    "Do other isolated pockets of hominins still exist in the fossil record yet to be found?",
    resolution_mechanism("Systematic exploration of deep-cave systems in previously unsearched regions"),
    impact("If yes: The 'bottleneck' is less restrictive than modeled. If no: The expansion-replacement model is absolute."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Multiregional Evolution
 *    Viability: Prior to 2000, some argued for diverse, parallel origins of modern humans.
 *    Suppression: Largely rejected by strong genetic evidence showing a common African descent ~60,000 years ago. The dominance of the "Out of Africa" model actively suppresses this alternative in mainstream science.
 * 
 * CONCLUSION:
 * The suppression of the multiregionalism alternative is a key reason the "Out of Africa" model acts as a powerful Rope for scientists and a Mountain for the public.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/hominin_evolutionary_bottleneck].
 * 2. Multi-perspective: ?- multi_index_report(hominin_evolutionary_bottleneck).
 * 3. Run tests: ?- run_tests(hominin_evolutionary_bottleneck_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */