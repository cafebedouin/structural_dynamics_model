% ============================================================================
% CONSTRAINT STORY: p_vs_np
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Stephen Cook (1971) / Computational Complexity Theory
% ============================================================================

:- module(constraint_p_vs_np, []).

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
 * * constraint_id: p_vs_np
 * human_readable: P vs. NP Problem
 * domain: technological
 * temporal_scope: Permanent (Laws of Computation)
 * spatial_scope: Global (Universal Mathematics)
 * * SUMMARY:
 * P vs. NP is a major unsolved problem in theoretical computer science. It asks 
 * whether every problem whose solution can be quickly verified (NP) can also 
 * be quickly solved (P). This constraint represents the fundamental boundary 
 * between "easy" (polynomial time) and "hard" (exponential time) computation.
 * * KEY AGENTS:
 * - The Complexity Theorist: Investigating the deep structure of the P vs. NP 
 * landscape to find a formal proof.
 * - The Cryptographer: Relies on the assumption that P != NP to ensure 
 * that breaking encryption remains computationally "hard."
 * - The Optimization Engineer: Faces NP-hard problems (like the Traveling 
 * Salesman) and must find heuristics to bypass the computational wall.
 * * NARRATIVE ARC:
 * The P vs. NP gap functions as a "Mountain" of physical reality for 
 * computation—if P != NP, certain tasks are forever hard. For the security 
 * architect, it is a "Rope" (the bedrock of modern trust). For the 
 * programmer stuck with an NP-hard task and a deadline, it is a "Snare" 
 * that strangles their ability to provide an optimal solution.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural extraction
narrative_ontology:interval(p_vs_np_interval, 0, 10).
narrative_ontology:constraint_claim(p_vs_np, mountain).

% Base extractiveness: 0.4 (Moderate)
% Rationale: It extracts "efficiency." The asymmetry of verification vs. 
% calculation creates a rent where those with valid "witnesses" (solutions) 
% hold power over those who must find them.
domain_priors:base_extractiveness(p_vs_np, 0.4).

% Suppression: 0.2 (Low)
% Rationale: Alternatives (like Quantum computing or P = NP) are widely 
% discussed and visible, though currently unproven for breaking the constraint.
domain_priors:suppression_score(p_vs_np, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(p_vs_np, extractiveness, 0.4).
narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the structure of logical state space.
domain_priors:emerges_naturally(p_vs_np).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(p_vs_np, cryptographers).
constraint_victim(p_vs_np, [brute_force_attackers, optimization_solvers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMPLEXITY THEORIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of systemic limits)
   WHEN: civilizational (A permanent feature of mathematical truth)
   WHERE: trapped (Logic cannot bypass the complexity of state space)
   SCOPE: global (Universal computation)
   
   WHY THIS CLASSIFICATION:
   To the theorist, P vs. NP is a Mountain. It represents an objective 
   difficulty in the fabric of the universe. Regardless of technology, 
   the branching of possible solutions creates a fixed physical barrier 
   to efficiency that must be mapped, not bypassed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    p_vs_np,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SECURITY ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (In charge of protecting a system)
   WHEN: biographical (Ensuring safety over a project's life)
   WHERE: arbitrage (Can use different hard problems for different needs)
   SCOPE: national (The reach of their infrastructure)
   
   WHY THIS CLASSIFICATION:
   For the architect, the hardness of NP is a Rope. It is the coordination 
   mechanism that allows for public-key cryptography. Without this 
   computational asymmetry, digital trust would collapse. They use this 
   difficulty as a tether to secure the entire internet.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    p_vs_np,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISPERATE PROGRAMMER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (A subject to the math's complexity)
   WHEN: immediate (A deadline that requires a fast solution)
   WHERE: constrained (Cannot change the nature of the problem)
   SCOPE: local (Immediate workspace)
   
   WHY THIS CLASSIFICATION:
   For the coder tasked with finding an optimal route for 50 cities by 
   tomorrow, the P vs. NP gap is a Snare. They know the solution exists, 
   but the "exponential wall" strangles their ability to find it in 
   time. The harder they push for perfection, the more the complexity 
   tightens around their compute budget.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    p_vs_np,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(p_vs_np_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(p_vs_np, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, T3, context(agent_power(powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_security) :-
    % Institutional actors (Architects) derive security from the gap (lower felt extraction).
    % Powerless actors feel the drain of unfeasible goals (higher felt extraction).
    domain_priors:base_extractiveness(p_vs_np, E),
    E >= 0.3.

test(time_immutability) :-
    % Civilizational horizon views mathematical invariants as Mountains.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(p_vs_np_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): The gap extracts "possibility." It ensures that 
 * brute force is always more expensive than verifying a result, which 
 * is a fundamental asymmetry of energy.
 * 2. SNARE CLASSIFICATION: I used this for the "Disperate Programmer" 
 * because NP-completeness is often a wall that forces people to settle 
 * for "good enough," strangling the ideal of perfection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_is_np_consequence,
    "If P = NP were proven to be true, would the algorithm be 
    computationally efficient enough to be usable, or would the constant 
    factors be too large (creating a new Mountain)?",
    resolution_mechanism("A formal constructive proof of P = NP"),
    impact("If Usable: Cryptography dies (Snare). If Unusable: The P vs. NP 
    Mountain remains effectively intact."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Quantum Computing (BQP)
 * Viability: Can solve *some* NP problems (like factoring) faster, 
 * but likely not all NP-complete problems.
 * Suppression: Low. Massive global investment.
 * * ALTERNATIVE 2: Heuristics and Approximation
 * Viability: High. Used every day to bypass the "Snare" of NP-hardness.
 * * CONCLUSION:
 * The existence of heuristics as a "Rope" allows workers to escape the 
 * "Snare" of NP-completeness, shifting the constraint from an 
 * unmanageable trap to a manageable engineering challenge.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_p_vs_np].
 * 2. Multi-perspective: ?- multi_index_report(p_vs_np).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(p_vs_np, 0.03).
narrative_ontology:constraint_metric(p_vs_np, theater_ratio, 0.03).
