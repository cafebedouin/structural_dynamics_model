% ============================================================================
% CONSTRAINT STORY: church_turing_thesis
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Church (1936), Turing (1936) / Computability Theory
% ============================================================================

:- module(constraint_church_turing_thesis, []).

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
 * * constraint_id: church_turing_thesis
 * human_readable: Church-Turing Thesis (Computability Boundary)
 * domain: technological/mathematical
 * temporal_scope: 1936 - Present
 * spatial_scope: Global/Abstract (Universal computation)
 * * SUMMARY:
 * The Church-Turing Thesis asserts that any function that can be computed by an 
 * algorithm can be computed by a Turing machine. It establishes the absolute 
 * horizon of what is "effectively calculable," separating the solvable from 
 * the logically impossible.
 * * KEY AGENTS:
 * - The Algorithm (Subject): A powerless sequence of instructions bound by the 
 * mechanics of the transition function.
 * - The Hardware Architect (Institutional): The designer of silicon or quantum 
 * substrates who uses the thesis as a "Rope" to guarantee universal utility.
 * - The Hypercomputationalist (Analytical/Victim): An observer seeking to 
 * transcend these bounds, for whom the thesis is a "Snare" of undecidability.
 * * NARRATIVE ARC:
 * The thesis acts as a "Mountain" of natural law in mathematics; it is the 
 * unyielding definition of a limit. For software engineers, it is a "Rope"—the 
 * foundation that allows one language (C++, Python) to do anything another can. 
 * However, to those attempting to solve the Halting Problem, it is a "Snare" 
 * that extracts the possibility of absolute systemic certainty.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(church_turing_era, 1936, 2026).
narrative_ontology:constraint_claim(church_turing_thesis, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.1. As a mathematical truth, it does not "take" in a social sense, 
% though it "extracts" the possibility of total algorithmic omniscience.
domain_priors:base_extractiveness(church_turing_thesis, 0.1).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses "non-effective" computational models from being 
% accepted as valid logic, but generally enables universal tool building.
domain_priors:suppression_score(church_turing_thesis, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(church_turing_thesis, extractiveness, 0.1).
narrative_ontology:constraint_metric(church_turing_thesis, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the axioms of logic and set theory.
domain_priors:emerges_naturally(church_turing_thesis).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(church_turing_thesis, universal_computer_designers). % Predictable bounds.
constraint_victim(church_turing_thesis, halting_problem_solvers). % Absolute logical barrier.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXECUTING CODE - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The code has zero agency to compute the uncomputable.
   WHEN: immediate - The limit is true at every step of execution.
   WHERE: trapped - Bound within the state-space of the machine.
   SCOPE: local - Immediate neighborhood of the current instruction.
   
   WHY THIS CLASSIFICATION:
   For the algorithm in motion, the Church-Turing boundary is an absolute 
   Mountain. It cannot "decide" to solve a problem that is fundamentally 
   non-recursive. The logic is a fixed feature of the universe.
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    church_turing_thesis,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMPILER ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design languages that fulfill the thesis.
   WHEN: biographical - Spanning the life of a language or architecture.
   WHERE: mobile - Can choose any Turing-complete representation.
   SCOPE: global - Universal application across all digital systems.
   
   WHY THIS CLASSIFICATION:
   For the architect, the thesis is a "Rope"—a coordination mechanism. It 
   guarantees that if they build a "Turing Complete" system, it can coordinate 
   any computable task, providing a standard of achievement for all hardware.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    church_turing_thesis,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SECURITY ANALYST - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency to analyze, but is bound by Rice's Theorem.
   WHEN: immediate - Every attempt to verify code safety hits the uncomputability wall.
   WHERE: constrained - No alternative but to accept "heuristic" rather than "absolute" safety.
   SCOPE: national - Protecting infrastructure against malware.
   
   WHY THIS CLASSIFICATION:
   For the analyst, the thesis acts as a "Snare." It "strangles" the hope for 
   perfect malware detection because "any non-trivial property of code is 
   undecidable." It extracts the possibility of absolute security.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    church_turing_thesis,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- 
    domain_priors:base_extractiveness(church_turing_thesis, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(church_turing_thesis_tests).

test(multi_perspective_variance) :-
    % Code -> Mountain
    constraint_indexing:constraint_classification(church_turing_thesis, Type1, context(powerless, immediate, trapped, local)),
    % Architect -> Rope
    constraint_indexing:constraint_classification(church_turing_thesis, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(halting_impossibility_insight) :-
    % Demonstrates that the Snare classification emerges for the analyst due to undecidability
    constraint_indexing:constraint_classification(church_turing_thesis, snare, context(individual_moderate, immediate, constrained, national)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(church_turing_thesis).

:- end_tests(church_turing_thesis_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.1):
 * Reasoning: Low because it is an enabling law of mathematics. However, it 
 * is not zero because it "extracts" the certainty required for perfect 
 * automated verification.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Code (Subject), Architect (Institution), and Analyst (Victim) to 
 * show the transition from "Physical Reality" to "Utility" to "Barrier."
 * * 3. OMEGA IDENTIFICATION:
 * Identified the "Physical Church-Turing Thesis" as the irreducible 
 * uncertainty—whether the actual universe supports hypercomputation.
 */

% OMEGA IDENTIFICATION
omega_variable(
    physical_computability_limit,
    "Does the physical universe support computations beyond the Turing limit?",
    resolution_mechanism("Empirical tests of non-recursive physical processes, e.g., in quantum gravity or GR."),
    impact("If Yes: The 'Mountain' is a 'Scaffold' that falls to a higher Rope. If No: It is a permanent Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hypercomputation (Analog/Quantum gravity computers)
 * Viability: Theoretically proposed models (like BSS machines or Malament-Hogarth spacetimes).
 * Suppression: Rejected by the current "Mountain" of standard physics and logic.
 * * ALTERNATIVE 2: Purely Finitist Logic
 * Viability: Computability only within fixed memory bounds.
 * Suppression: Rejected because the Turing "Rope" of infinite tape is 
 * necessary for a clean mathematical theory.
 * * CONCLUSION:
 * The absence of proven hypercomputation keeps the Church-Turing Thesis as a 
 * "Mountain." If an alternative were found, it would be a "Rope" for 
 * post-Turing civilization.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [church_turing_thesis].
 * 2. Analyze: ?- constraint_indexing:multi_index_report(church_turing_thesis).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
