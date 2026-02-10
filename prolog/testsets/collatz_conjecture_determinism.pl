% ============================================================================
% CONSTRAINT STORY: collatz_conjecture_determinism
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Lothar Collatz (1937) / Number Theory
% ============================================================================

:- module(constraint_collatz_conjecture, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: collatz_conjecture_determinism
 * human_readable: The Collatz Conjecture (3n + 1)
 * domain: mathematical/technological
 * temporal_scope: 1937 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Natural Numbers)
 * * SUMMARY:
 * The Collatz conjecture is an unsolved problem in mathematics involving an 
 * iterative sequence: if n is even, divide by 2; if n is odd, multiply by 3 
 * and add 1. It represents a fundamental constraint where simple arithmetic 
 * rules generate a "Mountain" of unpredictable, pseudo-random behavior 
 * that seemingly always collapses to the 4-2-1 loop.
 * * KEY AGENTS:
 * - The Natural Number (Subject): A powerless agent whose path is 
 * absolutely dictated by the parity of its current value.
 * - The Computational Researcher (Institutional): An agent who uses the 
 * conjecture as a "Rope" to test the limits of distributed computing and 
 * verification.
 * - The Number Theorist (Analytical): The observer attempting to map the 
 * "Mountain" to find a formal proof of convergence.
 * * NARRATIVE ARC:
 * The Collatz sequence functions as a "Mountain" of deterministic fate—for 
 * every number tested, the path is unyielding. In computer science, it is a 
 * "Rope" for benchmarking. However, for the mathematician, the absence of a 
 * proof acts as a "Snare," extracting decades of cognitive labor (extraction) 
 * while "strangling" the progress of arithmetic theory with its deceptive 
 * simplicity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required structural anchor for extraction
narrative_ontology:interval(collatz_era, 1937, 2026).
narrative_ontology:constraint_claim(collatz_conjecture_determinism, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.15. Low. While it "extracts" massive human and computational 
% effort to verify higher bounds, it primarily functions as a mathematical 
% "gift" of structural mystery.
domain_priors:base_extractiveness(collatz_conjecture_determinism, 0.15).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses the visibility of simple "linear" 
% transformations, proving that even basic arithmetic can hide chaos.
domain_priors:suppression_score(collatz_conjecture_determinism, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(collatz_conjecture_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the axioms of the integers.
domain_priors:emerges_naturally(collatz_conjecture_determinism).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(collatz_conjecture_determinism, distributed_computing_projects). % e.g., Collatz@Home.
narrative_ontology:constraint_beneficiary(collatz_conjecture_determinism, heuristic_complexity_theorists).
narrative_ontology:constraint_victim(collatz_conjecture_determinism, classical_analytical_simplicity).
narrative_ontology:constraint_victim(collatz_conjecture_determinism, mathematical_certainty). % The lack of proof is a "tax."

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NUMBER '27' - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The number has no agency; its parity is its fate.
   WHEN: immediate - The rule applies at every step n -> n+1.
   WHERE: trapped - Bound within the set of natural numbers.
   SCOPE: local - Immediate neighborhood of the current value.
   
   WHY THIS CLASSIFICATION:
   For an individual number, the Collatz rules are an absolute Mountain. 
   '27' must climb to 9232 before finally descending to 1. It cannot 
   "choose" a shorter path; the arithmetic is an unyielding law of its 
   existence.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    collatz_conjecture_determinism,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GRID-COMPUTING ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to coordinate millions of CPUs to test ranges.
   WHEN: biographical - Planning a multi-year computational search project.
   WHERE: mobile - Can choose which ranges of N to test or which algorithms to use.
   SCOPE: global - Using world-wide hardware resources.
   
   WHY THIS CLASSIFICATION:
   For the architect, the conjecture is a "Rope"—a tool for functional 
   coordination. It allows them to coordinate a standard of achievement 
   (testing up to $2^{68}$) that pushes the boundaries of hardware 
   reliability and distributed software.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    collatz_conjecture_determinism,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE FIELDS MEDAL ASPIRANT - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the intelligence to analyze but is bound by the problem.
   WHEN: biographical - Spending decades of a career on a single proof.
   WHERE: constrained - The "exit" (a proof) is seemingly unreachable.
   SCOPE: global - Universal mathematical problem.
   
   WHY THIS CLASSIFICATION:
   For the mathematician, the conjecture is a "Snare." It "strangles" their 
   career by being "too simple to be this hard." It extracts massive 
   intellectual capital (extraction) while providing no guarantee of 
   success, "choking" the theorist with the possibility of an undecidable 
   outcome.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    collatz_conjecture_determinism,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(collatz_conjecture_determinism, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(collatz_conjecture_tests).

test(multi_perspective_variance) :-
    % Number -> Mountain
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, Type1, context(powerless, immediate, trapped, local)),
    % Architect -> Rope
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(career_extraction_penalty) :-
    % The theorist feels the "Snare" of the unproven extraction.
    Context = context(individual_moderate, biographical, constrained, global),
    constraint_indexing:extractiveness_for_agent(collatz_conjecture_determinism, Context, Score),
    Score >= 0.1.

test(natural_emergence) :-
    domain_priors:emerges_naturally(collatz_conjecture_determinism).

:- end_tests(collatz_conjecture_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.15):
 * Reasoning: Low, because it's a theoretical problem. However, the 
 * "Extraction of Time" is real for the mathematical community. It takes 
 * genius and returns only empirical data, not structural proof.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Number (Subject), Architect (User), and Theorist (Victim) to 
 * show how a simple "Mountain" of logic is a "Rope" for engineering but 
 * a "Snare" for theory.
 * * 3. OMEGA IDENTIFICATION:
 * Formalized the "Undecidability" uncertainty—is Collatz a Mountain of 
 * truth or an unprovable Scaffold of the integers?
 */

% YOUR OMEGAS HERE:
omega_variable(
    collatz_undecidability,
    "Is the Collatz conjecture unprovable (Mountain) within standard Peano Arithmetic?",
    resolution_mechanism("Investigation into whether the conjecture is independent of ZFC."),
    impact("If Undecidable: The 'Mountain' is a mirage; it's a permanent 'Snare' for logic."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: 3n - 1 Sequence
 * Viability: A similar rule that leads to multiple loops (e.g., 5-14-7-20-10-5).
 * Suppression: Often ignored to maintain the "purity" of the 3n+1 "Mountain."
 * * ALTERNATIVE 2: Generalized Collatz Functions
 * Viability: Functions where the parity rules are more complex.
 * Evidence: Conway (1972) proved that general Collatz-like problems 
 * are undecidable.
 * * CONCLUSION:
 * The existence of Alternative 2 (Conway's proof) transforms the Collatz 
 * "Snare" into a "Mountain" of complexity—proving that our specific Snare 
 * is likely part of a broader undecidable landscape.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_collatz_conjecture].
 * 2. Multi-perspective: ?- multi_index_report(collatz_conjecture_determinism).
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
% Formal truth — substantive with near-zero performative component
domain_priors:theater_ratio(collatz_conjecture_determinism, 0.0).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, theater_ratio, 0.0).
