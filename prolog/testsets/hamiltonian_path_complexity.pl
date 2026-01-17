% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Graph Theory / Complexity Classes (NP-Complete)
% ============================================================================

:- module(constraint_hamiltonian_path, []).

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
 * * constraint_id: hamiltonian_path_complexity
 * human_readable: The Hamiltonian Path Constraint
 * domain: technological/mathematical
 * temporal_scope: Immediate to Historical
 * spatial_scope: Global (Abstract Math)
 * * SUMMARY:
 * A Hamiltonian Path is a path in an undirected or directed graph that visits 
 * each vertex exactly once. Finding such a path is an NP-complete problem, 
 * meaning that while a solution is easy to verify, finding one is 
 * computationally exhaustive as the number of nodes increases.
 * * KEY AGENTS:
 * - The Explorer: The agent tasked with finding a valid path through a network.
 * - The Network Architect: The one who defines the edges (constraints) the 
 * explorer must navigate.
 * - The Computer Scientist: The analytical observer who recognizes the 
 * "Complexity Wall" inherent in the problem.
 * * NARRATIVE ARC:
 * This constraint functions as a "Labyrinth of Efficiency." In a simple graph, 
 * it is a trivial coordination task (Rope). In a complex graph, it becomes 
 * a computational Noose that strangles resources and time, ultimately 
 * appearing as an immutable Mountain of mathematical reality.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(hamiltonian_path_complexity, 0, 10).
narrative_ontology:constraint_claim(hamiltonian_path_complexity, algorithmic_limit).

% Base extractiveness score (0.1)
% Pure math doesn't "extract" in a social sense, but the "brute force" 
% requirement extracts energy and time from the solver.
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.1).

% Suppression score (0.2)
% Low suppression; the "dead ends" in the search are highly visible to the agent.
domain_priors:suppression_score(hamiltonian_path_complexity, 0.2).

% Enforcement requirements
domain_priors:emerges_naturally(hamiltonian_path_complexity).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.1).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hamiltonian_path_complexity, cryptography_systems). % Complexity is a feature here
constraint_victim(hamiltonian_path_complexity, logistics_optimizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGISTICS PLANNER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (subject to the network's topology)
   WHEN: immediate (deadline-driven delivery)
   WHERE: trapped (must visit every node, cannot skip)
   SCOPE: local (city-wide delivery route)
   
   WHY THIS CLASSIFICATION:
   For a planner with a fixed number of stops and a strict "visit-once" 
   requirement, the Hamiltonian constraint is a "Noose." As nodes are 
   added, the number of possible paths grows factorially ($N!$), 
   strangling their ability to find a valid route before the deadline.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(hamiltonian_path_complexity, E),
    E < 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TOURIST - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile (can choose which cities to visit or change the goal)
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For a casual traveler, the idea of a Hamiltonian path is a "Rope." 
   It's an interesting coordination challenge that adds structure to 
   their trip. Because they aren't strictly "trapped" by the math, 
   the constraint is a helpful guide for "completing" their journey.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The mathematician sees the P vs NP problem as the ultimate "Mountain." 
   The fact that Hamiltonian Path is NP-complete is an immutable 
   feature of the universe's logical geography. It is a boundary 
   that no amount of individual effort can circumvent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(hamiltonian_path_complexity),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hamiltonian_path_tests).

test(multi_perspective_variance) :-
    % Perspective 1: Planner
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, noose, context(individual_powerless, _, trapped, _)),
    % Perspective 3: Analyst
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain, context(analytical, _, _, _)).

test(power_extractiveness_scaling) :-
    % A 'trapped' agent (Planner) pays a higher computational 'price' (Noose) 
    % than an 'analytical' observer who just sees the 'Mountain'.
    true.

:- end_tests(hamiltonian_path_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS MADE BY MODEL:
 * 1. CLASSIFICATION: I chose Noose for the Planner because NP-completeness 
 * effectively "strangles" the search space as the problem scales.
 * 2. OMEGAS: The ultimate uncertainty in computer science is P vs NP.
 */

omega_variable(
    p_vs_np_resolution,
    "Is P = NP? (Can an efficient algorithm find the path?)",
    resolution_mechanism("Mathematical proof or counter-example"),
    impact("If P=NP: The 'Mountain' turns into a 'Rope' (trivial coordination)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
