% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Graph Theory / Complexity Classes (NP-Complete)
% ============================================================================

:- module(constraint_hamiltonian_path_complexity, []).

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
 * 
 * constraint_id: hamiltonian_path_complexity
 * human_readable: The Hamiltonian Path Constraint
 * domain: technological/mathematical
 * temporal_scope: Immediate to Historical
 * spatial_scope: Global (Abstract Math)
 * 
 * SUMMARY:
 * A Hamiltonian Path is a path in an undirected or directed graph that visits 
 * each vertex exactly once. Finding such a path is an NP-complete problem, 
 * meaning that while a solution is easy to verify, finding one is 
 * computationally exhaustive as the number of nodes increases.
 * 
 * KEY AGENTS:
 * - The Logistics Planner (Individual Powerless): Subject to the network's topology.
 * - The Network Architect (Institutional): Defines the edges and constraints of the network.
 * - The Mathematician (Analytical): Observes the problem's inherent complexity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(hamiltonian_path_complexity, 0, 10).
narrative_ontology:constraint_claim(hamiltonian_path_complexity, mountain).

% Base extractiveness: 0.1.
% Pure math doesn't "extract" in a social sense, but the "brute force" 
% requirement extracts energy and time from the solver.
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.1).

% Suppression score: 0.2.
% Low suppression; the "dead ends" in the search are highly visible to the agent.
domain_priors:suppression_score(hamiltonian_path_complexity, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.1).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the mathematical properties of the graph.
domain_priors:emerges_naturally(hamiltonian_path_complexity).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hamiltonian_path_complexity, cryptography_systems).
constraint_victim(hamiltonian_path_complexity, logistics_optimizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGISTICS PLANNER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Subject to the network's topology)
   WHEN: immediate (Deadline-driven delivery)
   WHERE: trapped (Must visit every node, cannot skip)
   
   WHY THIS CLASSIFICATION:
   For a planner with a fixed number of stops and a strict "visit-once" 
   requirement, the Hamiltonian constraint is a 'Snare'. As nodes are 
   added, the number of possible paths grows factorially ($N!$), 
   strangling their ability to find a valid route before the deadline.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NETWORK ARCHITECT - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Defines the edges and constraints of the network)
   WHEN: historical (Designing systems for long-term use)
   WHERE: arbitrage (Can design the network to be more or less complex)
   
   WHY THIS CLASSIFICATION:
   For the network architect, Hamiltonian Path complexity is a 'Rope'. They
   can use their understanding of the problem to design networks that are
   either easily solvable (e.g., linear chains) or computationally hard,
   depending on the desired outcome (e.g., for cryptographic applications).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observes the inherent complexity of the problem)
   WHEN: civilizational (A permanent law of computational complexity)
   WHERE: analytical (Universal law)
   
   WHY THIS CLASSIFICATION:
   The mathematician sees the P vs NP problem as the ultimate 'Mountain'. 
   The fact that Hamiltonian Path is NP-complete is an immutable 
   feature of the universe's logical geography. It is a boundary 
   that no amount of individual effort can circumvent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hamiltonian_path_complexity,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hamiltonian_path_complexity_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(hamiltonian_path_complexity_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Network Architect' as the
 *    institutional agent. For them, Hamiltonian Path complexity is a 'Rope'
 *    to be manipulated in network design.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Logistics Planner (Snare): Overwhelmed by computational complexity.
 *    - Network Architect (Rope): Can design networks to be more or less complex.
 *    - Mathematician (Mountain): An immutable law of computational complexity.
 * 
 * 3. CORE INSIGHT: The Hamiltonian Path problem is a classic 'Mountain' of
 *    computational complexity. While it represents a suffocating 'Snare' for
 *    those trying to solve it in real-time, it can be a useful 'Rope' for
 *    those who design the networks themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the fundamental nature of the P vs NP problem.
 */

omega_variable(
    p_vs_np_resolution,
    "Is P = NP? (Can an efficient algorithm find the Hamiltonian path in all cases?)",
    resolution_mechanism("Mathematical proof or counter-example, a major unsolved problem in computer science."),
    impact("If P=NP: The 'Mountain' turns into a 'Rope' (trivial coordination), with profound implications for cryptography and optimization. If P!=NP: The 'Mountain' remains."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Heuristic Solutions / Approximation Algorithms
 *    Viability: Provides "good enough" solutions for many practical applications, such as logistics, without guaranteeing optimality.
 *    Suppression: Not actively suppressed, but acknowledged as a pragmatic workaround for the 'Mountain' of NP-completeness.
 *
 * CONCLUSION:
 * The existence of heuristic solutions acts as a 'Rope' for practical problem-solvers,
 * allowing them to navigate the 'Mountain' of computational complexity. However,
 * for problems requiring absolute certainty, the 'Mountain' remains an impassable barrier.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/hamiltonian_path_complexity].
 * 2. Multi-perspective: ?- multi_index_report(hamiltonian_path_complexity).
 * 3. Run tests: ?- run_tests(hamiltonian_path_complexity_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */