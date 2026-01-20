% ============================================================================
% CONSTRAINT STORY: hydra_game
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Kirby, L., & Paris, J. (1982) / Mathematical Logic
% ============================================================================

:- module(constraint_hydra_game, []).

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
 * * constraint_id: hydra_game
 * human_readable: The Hydra Game
 * domain: technological
 * temporal_scope: Permanent (Universal Mathematical Law)
 * spatial_scope: Global (Formal Systems)
 * * SUMMARY:
 * The Hydra Game is a mathematical game played on a rooted tree (the Hydra). 
 * While Hercules (the player) is guaranteed to win by eventually removing all 
 * heads, the length of the game grows so rapidly that its termination is 
 * unprovable in Peano Arithmetic.
 * * KEY AGENTS:
 * - Hercules: The agent attempting to solve the game (The Player).
 * - The Hydra: The recursive structure that grows in response to being attacked.
 * - The Proof Theorist: An analytical observer measuring the strength of 
 * the axioms required to prove termination.
 * * NARRATIVE ARC:
 * The game functions as a "Mountain" of transfinite necessity. For the 
 * logician, it is a "Rope" used to justify the move to stronger axioms (ZFC). 
 * For a computer attempting to simulate the game, it is a "Noose" that 
 * consumes all available time and memory long before the first branch is cleared.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for structural anchor
narrative_ontology:interval(hydra_interval, 0, 10).
narrative_ontology:constraint_claim(hydra_game, mountain).

% Base extractiveness: How much physical possibility is taken?
% Rationale: It extracts "realizability." The game is "computable" but 
% physically impossible to finish in our universe, extracting the utility 
% of the "Hercules always wins" promise.
domain_priors:base_extractiveness(hydra_game, 0.7).

% Suppression: How much are simpler alternatives hidden?
% Rationale: It suppresses "finitistic intuition." Peano Arithmetic 
% literally cannot "see" the end of the game, making the truth of 
% termination invisible to standard arithmetic.
domain_priors:suppression_score(hydra_game, 0.5).

% Enforcement: Emerges naturally from the rules of tree-pruning and recursion.
domain_priors:emerges_naturally(hydra_game).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(hydra_game, extractiveness, 0.7).
narrative_ontology:constraint_metric(hydra_game, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hydra_game, transfinite_logicians).
constraint_victim(hydra_game, [physical_computation, finitist_philosophers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL LOGICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal informational laws.
   WHEN: civilizational - Viewing logic as a permanent substrate.
   WHERE: trapped - Logic cannot bypass the well-ordering of ordinals.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the logician, the Hydra is a Mountain. The termination of the game 
   is a fixed fact of the transfinite landscape. It is not a policy; it 
   is a consequence of the structure of $\epsilon_0$ (epsilon-zero).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hydra_game,
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
   PERSPECTIVE 2: THE AXIOMATIC ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define the meta-theories used for proof.
   WHEN: biographical - Achieving results within a career.
   WHERE: arbitrage - Can move between PA and stronger transfinite systems.
   SCOPE: national - Academic and industrial application.
   
   WHY THIS CLASSIFICATION:
   For the architect of formal systems, the Hydra is a Rope. It is a 
   coordination mechanism for benchmarking theories. By showing that 
   a system can or cannot prove the Hydra terminates, they "pull" 
   the boundaries of mathematical knowledge forward.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hydra_game,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(hydra_game, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PHYSICAL COMPUTER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the limits of entropy and time.
   WHEN: immediate - Today's simulation run.
   WHERE: constrained - Limited by the heat death of the universe.
   SCOPE: local - Immediate hardware environment.
   
   WHY THIS CLASSIFICATION:
   For any physical machine, the game is a Noose. Even though "Hercules 
   wins," the number of steps required is so large that the universe 
   will end before the first major branch is removed. The "computability" 
   of the game strangles the physical resources of the computer.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hydra_game,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(hydra_game, E),
    E > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hydra_game_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(hydra_game, mountain, context(analytical, civilizational, trapped, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(hydra_game, rope, context(institutional, biographical, arbitrage, national)),
    % Powerless sees Noose
    constraint_indexing:constraint_classification(hydra_game, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_realizability) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(hydra_game, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(hydra_game, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_hydra) :-
    % Immediate horizon + trapped = Mountain/Noose (the wall is hit now)
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(hydra_game_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.7): High. The Hydra Game extracts the "meaning" of 
 * computability. It is technically computable, yet physically impossible.
 * 2. NOOSE CLASSIFICATION: Applied to physical computation because the 
 * "guarantee" of winning is a trap—you will die/run out of energy long 
 * before you win.
 * 3. MOUNTAIN LOGIC: Based on the unprovability in PA (Peano Arithmetic), 
 * establishing it as a fixed feature for those limited to finitistic axioms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfinite_intuition_physicality,
    "Can human consciousness 'access' transfinite truth (Mountain) or is it 
    merely a linguistic scaffold (Rope) for a finitist brain?",
    resolution_mechanism("Long-term neuro-computational study of transfinite reasoning"),
    impact("If Mountain: We can eventually 'see' the end of the Hydra. 
    If Rope: The Hydra remains a permanent Noose for us."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Non-standard Models of Arithmetic
 * Viability: Allows for "infinite" integers that might interpret 
 * the Hydra game differently.
 * Suppression: Standard logic suppresses non-standard models to 
 * maintain intuitive consistency.
 * * CONCLUSION:
 * The existence of stronger transfinite systems converts the "Noose" 
 * of the Hydra into a "Rope" for the advanced mathematician.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_hydra_game].
 * 2. Multi-perspective: ?- multi_index_report(hydra_game).
 * 3. Run tests: ?- run_tests(hydra_game_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
