% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: parable_fish_turtle
 * human_readable: The Ontological Lake (Fish and Turtle)
 * domain: social/philosophical
 * temporal_scope: Perennial
 * spatial_scope: Global / Cognitive
 * * SUMMARY:
 * This constraint defines the limits of understanding based purely on lived experience. 
 * A fish, trapped in an aquatic ontology, cannot conceptualize "beauty" in a terrestrial 
 * environment because the land lacks the only attributes the fish knows (wetness, 
 * buoyancy, coolness).
 * * KEY AGENTS:
 * - The Fish: An agent trapped in a single sensory and experiential domain.
 * - The Turtle: A mobile agent who transcends the lake's boundary to experience 
 * a different reality (land).
 * - The Sage (Ayya Khema): An analytical observer using the parable to describe 
 * the "mountain" of cognitive limits.
 * * NARRATIVE ARC:
 * The turtle explores the land and returns to share its beauty, but the fish 
 * filters this information through the "noose" of its aquatic definitions. 
 * Because the land is not wet or rippling, the fish rejects its existence as 
 * beautiful or meaningful, illustrating how a lack of "exit options" in experience 
 * liquidates the ability to perceive truth.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(parable_fish_turtle, 0, 10).
narrative_ontology:constraint_claim([parable_fish_turtle], [perceptual_lock_in]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.7). The aquatic ontology extracts the totality of terrestrial 
% beauty from the fish's universe. The fish's "standard of thinking" is so 
% rigid it liquidates the land's value.
domain_priors:base_extractiveness(parable_fish_turtle, 0.7).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High (0.85). The experience of being "wet" and "buoyant" 
% completely suppresses the possibility of a "dry" or "solid" beauty.
domain_priors:suppression_score(parable_fish_turtle, 0.85).

% Enforcement requirements
% Emerges naturally from biological and environmental constraints.
domain_priors:emerges_naturally(parable_fish_turtle).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, 0.7).
narrative_ontology:constraint_metric(parable_fish_turtle, suppression_requirement, 0.85).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(parable_fish_turtle, aquatic_stability). % The fish stays safe in the known.
constraint_victim(parable_fish_turtle, terrestrial_truth).      % The land's beauty is erased.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE FISH - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "little person" of the lake with no land-access)
   WHEN: immediate (The moment of receiving the turtle's report)
   WHERE: trapped (No conceptual or physical exit from the water)
   SCOPE: local (The boundaries of the lake)
   
   WHY THIS CLASSIFICATION:
   For the fish, its definitions are a "Noose." The requirement that beauty *must* be "wet" and "cool" strangles its ability to understand the turtle's discovery, 
   effectively extracting the land from its world.
   
   NARRATIVE EVIDENCE:
   "When the turtle said it had none of these attributes, the fish said, ‘What 
   can be beautiful about it then?'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    parable_fish_turtle,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(parable_fish_turtle, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TURTLE - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "visit the land")
   WHEN: biographical (The lifespan required to move between domains)
   WHERE: mobile (Can leave the lake and return)
   SCOPE: regional (Lake + surrounding land)
   
   WHY THIS CLASSIFICATION:
   For the turtle, the boundary of the lake is a "Rope." It is a functional 
   coordination mechanism that allows for movement between different realities. 
   The turtle uses both water and land as tools for a "good look around".
   
   NARRATIVE EVIDENCE:
   "One day the turtle decided to visit the land surrounding the lake... came back 
   to tell her friend the fish of the wonders she had seen".
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SAGE - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of "Being Nobody, Going Nowhere")
   WHEN: civilizational (Perennial wisdom across thousands of years)
   WHERE: analytical (Observer stance; "it is all yours")
   SCOPE: global (The human condition of limited perspective)
   
   WHY THIS CLASSIFICATION:
   To the Sage, the fish's limitation is a "Mountain"—an unchangeable natural law of 
   subjective perception. One cannot "force" the fish to see the land; the 
   perceptual lock-in is a zero-degree-of-freedom reality of the "puny cell" 
   of limited experience.
   
   NARRATIVE EVIDENCE:
   The parable serves to illustrate the fundamental difficulty of communicating 
   transcendent beauty to those bound by sensory definitions.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parable_fish_turtle,
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

:- begin_tests(parable_fish_turtle_tests).

test(multi_perspective_ontology) :-
    % Fish sees Noose (Liquidation of Land)
    constraint_indexing:constraint_classification(parable_fish_turtle, noose, context(individual_powerless, immediate, trapped, local)),
    % Turtle sees Rope (Coordination between domains)
    constraint_indexing:constraint_classification(parable_fish_turtle, rope, context(individual_moderate, biographical, mobile, regional)),
    % Sage sees Mountain (Fixed law of perspective)
    constraint_indexing:constraint_classification(parable_fish_turtle, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_fish) :-
    % The powerless fish suffers higher extraction of beauty/truth than the mobile turtle.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(parable_fish_turtle, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(parable_fish_turtle, ContextModerate, E2),
    E1 > E2.

test(time_immutability_lake) :-
    % In the immediate horizon, the lake is a Mountain (fact).
    % Over biographical time (the turtle's journey), it becomes a Rope.
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(parable_fish_turtle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * Reasoning: I chose a high score because the fish's narrow perspective 
 * completely liquidates the value and existence of the land. This is a 
 * "Mandatrophy" risk where the system extracts the truth of a higher 
 * reality for the sake of aquatic comfort.
 * * 2. SUPPRESSION SCORE (0.85):
 * Reasoning: The fish literally cannot imagine the land. The presence of 
 * water suppresses any other concept of beauty.
 * * 3. PERSPECTIVE SELECTION:
 * Contrasted the Fish (Noose) with the Turtle (Rope) to highlight how 
 * "mobile" exit options transform a social trap into a tool for exploration.
 * * 4. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The extraction experienced by the fish 
 * is justified by its "trapped" status. For the "mobile" turtle, the 
 * lake's limits are simply one part of a larger "Rope" of experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fish_imagination_threshold,
    "Is it biologically possible for the fish to conceptualize 'dry beauty' (Rope) or is the cognitive limit a permanent Mountain?",
    resolution_mechanism("Audit of fish behavioral responses to non-aquatic stimuli and training"),
    impact("If Rope: Perspective 1 is temporary. If Mountain: Perspective 1 is eternal."),
    confidence_without_resolution(medium)
).

omega_variable(
    land_beauty_truth,
    "Is the 'beauty' of land an objective Mountain of fact or a subjective Rope created by the turtle's narrative?",
    resolution_mechanism("Cross-species consensus on terrestrial aesthetics - currently impossible"),
    impact("If Mountain: The fish is missing truth. If Rope: The turtle is telling a story."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Terrestrial Evolution
 * Viability: The historical path from fish to amphibian to turtle.
 * Suppression: Suppressed by the "immediate" time horizon of the fish.
 * * ALTERNATIVE 2: Conceptual Abstraction
 * Viability: The fish learning to believe in "dry beauty" without seeing it.
 * Suppression: Shunted by the fish's demand for "transparent, cool, rippling" 
 * evidence.
 * * CONCLUSION:
 * The absence of these alternatives (evolution/abstraction) makes the fish's 
 * view a "Noose." Without the "Rope" of mobility, the fish is a victim 
 * of its own definition.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [parable_fish_turtle].
% Multi-perspective: ?- constraint_indexing:multi_index_report(parable_fish_turtle).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
