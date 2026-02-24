% ============================================================================
% CONSTRAINT STORY: traveling_salesperson_problem
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Combinatorial Optimization / Complexity Theory / Operations Research
% ============================================================================

:- module(constraint_tsp, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: traveling_salesperson_problem
 * human_readable: Traveling Salesperson Problem (TSP)
 * domain: economic/technological/mathematical
 * temporal_scope: Immediate to Historical
 * spatial_scope: Regional to Global
 * * SUMMARY:
 * TSP is an optimization problem: given a list of cities and the distances 
 * between each pair, what is the shortest possible route that visits each 
 * city exactly once and returns to the origin city? It is NP-hard.
 * * KEY AGENTS:
 * - The Courier: The agent facing the physical exhaustion of the route.
 * - The Logistics Algorithm: The "Institutional" force attempting to solve 
 * the path to minimize fuel/time costs.
 * - The Market: The force that extracts profit based on the efficiency 
 * (or failure) of the solution.
 * * NARRATIVE ARC:
 * TSP is the "Efficiency Ceiling." For a small set of nodes, it is a 
 * navigable Rope. However, as the network expands, the search space 
 * explodes, creating a computational Snare that drains resources. It 
 * eventually settles as a Mountain of irreducible cost—the "minimum price" 
 * nature demands for connectivity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(traveling_salesperson_problem, 0, 10).
narrative_ontology:constraint_claim(traveling_salesperson_problem, mountain).

% Base extractiveness score (0.4)
% High because every suboptimal mile "extracts" real-world energy (fuel, labor) 
% that cannot be recovered.
domain_priors:base_extractiveness(traveling_salesperson_problem, 0.4).

% Suppression score (0.2)
% The "costs" of the edges (distances/prices) are usually visible, 
% making the constraint highly transparent but unavoidable.
domain_priors:suppression_score(traveling_salesperson_problem, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(traveling_salesperson_problem, extractiveness, 0.4).
narrative_ontology:constraint_metric(traveling_salesperson_problem, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from physical distance and finite time.
domain_priors:emerges_naturally(traveling_salesperson_problem).

% Metrics
% BENEFICIARIES & VICTIMS
constraint_beneficiary(traveling_salesperson_problem, efficient_monopolies).
constraint_victim(traveling_salesperson_problem, small_scale_logistics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DELIVERY DRIVER - Snare
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate
   WHERE: trapped (must follow the optimized route to meet performance KPIs)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a driver, TSP is a "Snare." The algorithm's search for the absolute 
   minimum path often ignores human factors (fatigue, traffic, bathroom breaks). 
   The mathematical "optimum" becomes a punitive standard they are 
   strangled by during their shift.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    traveling_salesperson_problem,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(traveling_salesperson_problem, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NETWORK ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: institutional/individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   To the engineer, TSP is a "Rope." It is a tool to coordinate resources. 
   By using heuristics (like the "Nearest Neighbor" or "Ant Colony" 
   optimization), they can navigate the complexity to create a functional, 
   profitable system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    traveling_salesperson_problem,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PHYSICIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The physicist sees TSP as a "Mountain" of entropy and energy. 
   The minimum energy state required to traverse a network is a 
   fundamental physical reality. It is a boundary of the universe that 
   dictates the limits of all physical transport.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    traveling_salesperson_problem,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(traveling_salesperson_problem),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(tsp_tests).

test(algorithmic_pressure) :-
    % Testing that trapped agents feel the Snare of optimization.
    constraint_indexing:constraint_classification(traveling_salesperson_problem, snare, context(_, _, trapped, _)).

test(heuristic_utility) :-
    % Testing that institutional agents use TSP as a Rope for coordination.
    constraint_indexing:constraint_classification(traveling_salesperson_problem, rope, context(institutional, _, _, _)).

:- end_tests(tsp_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * TSP is the "Economic version" of the Hamiltonian Path. I increased 
 * extractiveness to 0.4 because in the real world, a bad TSP solution 
 * wastes real money and fuel. It's not just a puzzle; it's a drain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    quantum_optimization_breakthrough,
    "Will quantum annealing (e.g., D-Wave) provide near-instant optimal TSP solutions?",
    resolution_mechanism("Benchmarking quantum annealers against classical heuristics on $N>1000$ nodes"),
    impact("If Yes: The Snare for logistics firms vanishes. The Mountain is bypassed by a 'Tunnel'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
