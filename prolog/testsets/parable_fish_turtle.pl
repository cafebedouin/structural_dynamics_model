% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Ayya Khema, "Being Nobody, Going Nowhere"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_parable_fish_turtle, []).

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
 * constraint_id: parable_fish_turtle
 * human_readable: The Ontological Lake (Fish and Turtle)
 * domain: social/philosophical
 * temporal_scope: Perennial
 * spatial_scope: Global / Cognitive
 * 
 * SUMMARY:
 * This constraint defines the limits of understanding based purely on lived experience. 
 * A fish, trapped in an aquatic ontology, cannot conceptualize "beauty" in a terrestrial 
 * environment because the land lacks the only attributes the fish knows (wetness, 
 * buoyancy, coolness).
 * 
 * KEY AGENTS:
 * - The Fish (Individual Powerless): An agent trapped in a single sensory and experiential domain.
 * - The Turtle (Individual Moderate): A mobile agent who transcends the lake's boundary.
 * - Scientific/Educational Institution (Institutional): Uses parables to teach about cognitive limits.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(parable_fish_turtle, 0, 10).
narrative_ontology:constraint_claim(parable_fish_turtle, tangled_rope).

% Base extractiveness: 0.7.
% The aquatic ontology extracts the totality of terrestrial 
% beauty from the fish's universe. The fish's "standard of thinking" is so 
% rigid it liquidates the land's value.
domain_priors:base_extractiveness(parable_fish_turtle, 0.7).

% Suppression score: 0.85.
% The experience of being "wet" and "buoyant" 
% completely suppresses the possibility of a "dry" or "solid" beauty.
domain_priors:suppression_score(parable_fish_turtle, 0.85).

% Enforcement: Emerges naturally from biological and environmental constraints.
domain_priors:emerges_naturally(parable_fish_turtle).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(parable_fish_turtle, aquatic_stability).
constraint_victim(parable_fish_turtle, terrestrial_truth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE FISH - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Trapped in a single sensory and experiential domain)
   WHEN: immediate (The moment of receiving the turtle's report)
   WHERE: trapped (No conceptual or physical exit from the water)
   
   WHY THIS CLASSIFICATION:
   For the fish, its definitions are a 'Snare.' The requirement that beauty 
   *must* be "wet" and "cool" strangles its ability to understand the turtle's
   discovery, effectively extracting the land from its world. This perceptual
   lock-in prevents it from recognizing other realities.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parable_fish_turtle,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SCIENTIFIC/EDUCATIONAL INSTITUTION - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Uses parables to teach about cognitive limits)
   WHEN: historical (Drawing on perennial philosophical insights)
   WHERE: analytical (Dissecting the mechanisms of perceptual bias)
   
   WHY THIS CLASSIFICATION:
   For a scientific or educational institution, the parable illustrates the
   'Mountain' of inherent biases in observation and education. It emphasizes
   the immutable reality of cognitive limits and the need for diverse
   methodologies to avoid perceptual lock-in, acting as a foundational
   truth in understanding learning.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parable_fish_turtle,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TURTLE - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "visit the land")
   WHEN: biographical (The lifespan required to move between domains)
   WHERE: mobile (Can leave the lake and return)
   
   WHY THIS CLASSIFICATION:
   For the turtle, the boundary of the lake is a 'Rope.' It is a functional 
   coordination mechanism that allows for movement between different realities. 
   The turtle uses both water and land as tools for a "good look around,"
   expanding its ontological understanding.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parable_fish_turtle,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(parable_fish_turtle_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(parable_fish_turtle, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(parable_fish_turtle_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Scientific/Educational Institution' as
 *    the institutional agent. For them, the parable serves as a 'Mountain'
 *    illustrating fundamental cognitive limits in learning and perception.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Fish (Snare): Trapped by its own narrow perception.
 *    - Institution (Mountain): Illustrates inherent cognitive biases.
 *    - Turtle (Rope): A tool for exploring different realities.
 * 
 * 3. CORE INSIGHT: The parable of the fish and turtle is a 'Tangled Rope'
 *    of understanding. What is a suffocating 'Snare' of perceptual lock-in
 *    for the fish, and an immutable 'Mountain' of cognitive limits for
 *    institutions, becomes a liberating 'Rope' for the turtle that
 *    actively seeks out new experiences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the biological and cognitive limits of overcoming ingrained perception.
 */

omega_variable(
    fish_imagination_threshold,
    "Is it biologically possible for the fish to truly conceptualize 'dry beauty' (Rope), or is its cognitive limit a permanent 'Mountain' that prevents such understanding?",
    resolution_mechanism("Audit of fish behavioral responses to non-aquatic stimuli and training; comparative neurobiology of sensory integration across species."),
    impact("If Rope: Perceptual lock-in is temporary. If Mountain: Perceptual lock-in is eternal, a fundamental limit."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Terrestrial Evolution
 *    Viability: The historical path from aquatic to amphibious life, leading to a broader ontological understanding.
 *    Suppression: Suppressed by the immediate time horizon of the fish, which cannot evolve on demand.
 *
 * CONCLUSION:
 * The parable highlights how a lack of viable 'Exit Options' (like terrestrial
 * evolution or conceptual abstraction) makes the fish's perspective a 'Snare'.
 * Without the 'Rope' of mobility and cognitive flexibility, the fish
 * remains a victim of its own definitions.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/parable_fish_turtle].
 * 2. Multi-perspective: ?- multi_index_report(parable_fish_turtle).
 * 3. Run tests: ?- run_tests(parable_fish_turtle_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */