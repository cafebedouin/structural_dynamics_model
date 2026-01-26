% ============================================================================
% CONSTRAINT STORY: generational_replacement_inertia
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The Primary Human Problem" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_generational_replacement_inertia, []).

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
 * constraint_id: generational_replacement_inertia
 * human_readable: Generational Cognitive Inertia
 * domain: social/psychological
 * temporal_scope: Generational
 * spatial_scope: Civilizational
 * 
 * SUMMARY:
 * This constraint posits that human thoughts are trapped in "well-worn pathways"
 * that rarely change, leading to a state where societies only evolve as a
 * function of generational replacement. The "Primary Human Problem" is the duality
 * of being "good" but also "self-centered, lazy and stubborn," making the
 * "breaking of chains" a biological rather than intellectual process.
 * 
 * KEY AGENTS:
 * - The Stuck Thinker (Individual Powerless): Caught in static mental routes.
 * - The New Generation (Individual Moderate): The carrier of "new ideas".
 * - The Government/Institution (Institutional): Must plan around demographic and ideological shifts.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(generational_replacement_inertia, 0, 10).
narrative_ontology:constraint_claim(generational_replacement_inertia, mountain).

% Base extractiveness score (0.75): High. The constraint extracts the agency and
% potential for growth of existing individuals, liquidating their capacity for
% change in favor of "generational replacement".
domain_priors:base_extractiveness(generational_replacement_inertia, 0.75).

% Suppression score (0.8): High. "Advice is useless" and "fools won't heed it,"
% actively suppressing the visibility of alternative pathways for the current generation.
domain_priors:suppression_score(generational_replacement_inertia, 0.8).

% Enforcement: Emerges naturally from human psychology ("stubborn") and biology.
domain_priors:emerges_naturally(generational_replacement_inertia).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(generational_replacement_inertia, future_generations).
constraint_victim(generational_replacement_inertia, individual_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL THINKER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Slaves to "self-centered, lazy and stubborn" nature)
   WHEN: immediate (The daily cycle of "well-worn pathways")
   WHERE: trapped (Caught in routes that are "static, unchanging")
   
   WHY THIS CLASSIFICATION:
   For the individual, cognitive inertia is a 'Snare.' Their thoughts are "abbreviated versions" 
   of their being, and they are trapped in a cycle where they seek "corroboration"
   over advice, effectively strangling their own potential for evolution.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    generational_replacement_inertia,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NEW GENERATION - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "create new pathways")
   WHEN: generational (The timescale of social evolution)
   WHERE: mobile (Moving into the currency of new ideas)
   
   WHY THIS CLASSIFICATION:
   For the incoming cohort, the replacement cycle is a 'Rope.' It is the functional 
   mechanism that "breaks the chains" of the old society, allowing for the 
   nourishment of new ideas that the previous generation could not understand.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    generational_replacement_inertia,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GOVERNMENT / INSTITUTION - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (A long-range planning body like a university or government)
   WHEN: civilizational (Planning for social security, infrastructure, etc.)
   WHERE: analytical (Observing demographic and ideological trends as inputs)
   
   WHY THIS CLASSIFICATION:
   For a long-term institution, generational replacement is a 'Mountain'. It is
   an immutable demographic reality that cannot be changed, only planned for.
   It is the fundamental, slow-moving variable that all long-range policy must
   be built upon.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    generational_replacement_inertia,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(generational_replacement_inertia_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(generational_replacement_inertia, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_replacement_inertia, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(generational_replacement_inertia, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(generational_replacement_inertia_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Government/Institution' to represent
 *    the macro-scale view. For such an entity, generational change is not a
 *    social goal but an unchangeable demographic 'Mountain' to be managed.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Individual (Snare): Trapped in their own mind.
 *    - New Generation (Rope): A mechanism for social change.
 *    - Institution (Mountain): An immutable demographic fact.
 * 
 * 3. MANDATROPHY STATUS: High extraction for the individual (lost potential) is
 *    justified from the system's view by the functional 'Rope' of social
 *    evolution that it enables.
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
    generational_replacement_inertia_extraction_intent,
    "Is the inability for individual thoughts to change a biological necessity (Mountain) or an extractive psychological Snare that could be escaped with better tools?",
    resolution_mechanism("Audit of cognitive plasticity in 'nourishing' vs 'poisonous' psychological and social environments."),
    impact("If necessity: Evolutionary Mountain. If a product of environment: A solvable Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Intellectual Persuasion ("Advice")
 *    Viability: The common hope that sharing information changes minds.
 *    Suppression: Explicitly rejected by the source as "useless" because "fools won't heed it" and people prefer corroboration.
 *
 * CONCLUSION:
 * The failure of the "advice" alternative makes "Generational Replacement" the
 * only viable mechanism for large-scale social change presented in the text,
 * justifying its role as a macro-level 'Rope' even as it acts as a personal 'Snare'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/generational_replacement_inertia].
 * 2. Multi-perspective: ?- multi_index_report(generational_replacement_inertia).
 * 3. Run tests: ?- run_tests(generational_replacement_inertia_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */