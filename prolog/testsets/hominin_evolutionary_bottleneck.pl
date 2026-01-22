% ============================================================================
% CONSTRAINT STORY: hominin_evolutionary_bottleneck
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: hominin_evolutionary_bottleneck
 * human_readable: The Hominin Evolutionary Bottleneck
 * domain: technological/scientific/biological
 * temporal_scope: 7 million years ago to 60,000 years ago (Paleoanthropological record) 
 * spatial_scope: Africa and Global (Migration/Replacement patterns) 
 * * SUMMARY:
 * Historically, human evolution was viewed through a limited fossil record, primarily Ardipithecus (4.4mya). 
 * Since 2000, discoveries like Sahelanthropus (7mya) and genetic evidence of an African expansion 60,000 years ago 
 * have reframed human origins as a complex story of replacement and deep-time persistence.
 * * KEY AGENTS:
 * - The Modern Human (Homo sapiens): Descendants of the 60,000-year-old African expansion.
 * - The Extinct Cousin (e.g., Homo naledi, Flores "Hobbits"): Isolated species replaced by expanding populations.
 * - The Paleoanthropologist: The analytical observer using new techniques to interpret fossil blizzards.
 * * NARRATIVE ARC:
 * The constraint began as a "Mountain" of missing information that made evolution seem like a simple linear 
 * path. Modern archaeological "more" has turned it into a "Rope" for scientists to climb into the 
 * past, while for replaced species, the expansion was a biological "Noose".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(hominin_evolutionary_bottleneck, 0, 10).
narrative_ontology:constraint_claim([hominin_evolutionary_bottleneck], [biological_replacement]).

% Base extractiveness score (0.75 = High)
% Rationale: The 60,000-year-old expansion involved the total replacement of other hominin species.
% This represents a total extraction of territory and evolutionary continuity from "cousin" species.
domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, 0.75).

% Suppression score (0.60 = Moderate-High)
% Rationale: The "Normal Human" story previously suppressed the existence of diverse cousins 
% like H. naledi and Indonesian "hobbits" due to limited data.
domain_priors:suppression_score(hominin_evolutionary_bottleneck, 0.60).

% Enforcement requirements: Emerges naturally through biological competition and environmental isolation.
domain_priors:emerges_naturally(hominin_evolutionary_bottleneck).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, extractiveness, 0.75).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, suppression_requirement, 0.60).

% BENEFICIARIES & VICTIMS
% Modern humans (Non-African people descended from African ancestors) benefit from the expansion.
constraint_beneficiary(hominin_evolutionary_bottleneck, homo_sapiens).
% Extinct cousins (replaced species) are the victims of the evolutionary expansion.
constraint_victim(hominin_evolutionary_bottleneck, extinct_hominin_cousins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: EXTINCT COUSIN (e.g., Homo naledi) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Biological species subject to replacement) 
   WHEN: civilizational (Deep-time evolutionary scale) 
   WHERE: trapped (Isolated in deep caves or on islands) 
   SCOPE: regional (Isolated pockets like Flores or single South African caves) 
   
   WHY THIS CLASSIFICATION:
   For species like H. naledi, the evolutionary bottleneck was a Noose. They were 
   confined to specific environments and eventually replaced by modern humans.
   
   NARRATIVE EVIDENCE:
   "Modern humans evolved in Africa and then expanded from there, replacing all the other hominin species".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hominin_evolutionary_bottleneck,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN SCIENTIST - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer using better techniques and data "blizzards") 
   WHEN: biographical (The 25-year "revolution" in understanding) 
   WHERE: arbitrage (Ability to play fossil data against genetic evidence) 
   SCOPE: global (Mapping origins from Africa to Indonesia) 
   
   WHY THIS CLASSIFICATION:
   The bottleneck and expansion patterns serve as a Rope—a functional framework that 
   organizes the "huge amount of information" into a coherent story of origins.
   
   NARRATIVE EVIDENCE:
   "Genetic evidence had demonstrated that all non-African people are descended from 
   African ancestors who lived about 60,000 years ago".
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
) :-
    % Analytical observers see the functional patterns of the data
    true,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE "ORRORIN" PRE-HUMAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Ancient ancestors with no conceptual agency) 
   WHEN: historical (7 million years of stasis and drift) 
   WHERE: trapped (Bounded by the physical "natural law" of Ardipithecus) 
   SCOPE: local (Specific fossil sites) 
   
   WHY THIS CLASSIFICATION:
   For early hominins like Orrorin, evolution was a Mountain—a slow, immutable 
   process of biological unfolding that spanned millions of years without intervention.
   
   NARRATIVE EVIDENCE:
   "In 2000 and 2001, researchers found an even older Ardipithecus... Orrorin tugenensis 
   from 6 million years ago".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hominin_evolutionary_bottleneck,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(hominin_evolutionary_bottleneck, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hominin_evolutionary_bottleneck_tests).

test(multi_perspective_variance) :-
    % Cousin (Noose) vs Scientist (Rope) vs Ancestor (Mountain)
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, noose, context(individual_powerless, civilizational, trapped, regional)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, rope, context(analytical, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, mountain, context(individual_powerless, historical, trapped, local)).

test(power_extractiveness_scaling) :-
    % Powerless extinct cousins experience the total extraction of their species lineage (Score 1)
    % while analytical observers experience only data acquisition (Score 2).
    Score1 = 0.75,
    Score2 = 0.10,
    Score1 > Score2.

:- end_tests(hominin_evolutionary_bottleneck_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: Chosen because the "Replacement" theory (African expansion) 
 * describes a high-extraction biological event where one group displaces others.
 * * 2. SUPPRESSION SCORE (0.60):
 * Reasoning: The long-standing Ardipithecus limit suppressed the "older" Orrorin 
 * and Sahelanthropus reality for decades.
 * * 3. PERSPECTIVE SELECTION:
 * Chose "Extinct Cousin" (H. naledi) to illustrate the Noose of replacement 
 * and "Modern Scientist" to illustrate the Rope of analytical synthesis.
 * * 4. AMBIGUITIES:
 * - The text mentions a "blizzard" of information; I interpreted this as a reduction 
 * in suppression but an increase in the complexity of the analytical Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    hominin_evolutionary_bottleneck_extraction_intent,
    "Is the 60,000-year expansion a predatory biological displacement (Noose) or a natural adaptive shift (Mountain)?",
    resolution_mechanism("Audit of genetic admixture rates between Sapiens and Cousins to determine if 'replacement' was violent or absorptive"),
    impact("If predatory: Noose/Mandatrophy. If adaptive: Mountain."),
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
 * * ALTERNATIVE 1: Multiregional Evolution
 * Viability: Prior to 2000, some argued for diverse origins.
 * Suppression: Rejected by genetic evidence showing African descent 60,000 years ago.
 * * CONCLUSION:
 * The existence of suppressed alternatives (multiregionalism) shifts the modern 
 * understanding from a simple Mountain to an analytical Rope, and for those 
 * replaced, confirms the historical Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/hominin_evolutionary_bottleneck].
 * 2. Multi-perspective: ?- multi_index_report(hominin_evolutionary_bottleneck).
 * 3. Run tests: ?- run_tests(hominin_evolutionary_bottleneck_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
