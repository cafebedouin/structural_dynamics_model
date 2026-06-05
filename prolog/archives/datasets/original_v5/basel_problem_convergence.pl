% ============================================================================
% CONSTRAINT STORY: basel_problem_convergence
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: The Basel Problem (Sum of Reciprocals of Squares)
% ============================================================================

:- module(constraint_basel_problem, []).

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
 * * constraint_id: basel_problem_convergence
 * human_readable: The Basel Problem (Infinite Series Convergence)
 * domain: mathematical/analytical
 * temporal_scope: 1644 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Number Theory)
 * * SUMMARY:
 * The Basel Problem asks for the precise sum of the reciprocals of the squares 
 * of the natural numbers. Posed in 1644 and solved by Leonhard Euler in 1734, 
 * the sum is exactly pi^2 / 6. This constant represents a fundamental limit 
 * of harmonic summation.
 * * KEY AGENTS:
 * - The Iterative Sum (The Subject): The sequence of partial sums, 
 * relentlessly moving toward a fixed limit.
 * - Leonhard Euler: The institutional agent who "unlocked" the series by 
 * connecting it to trigonometric functions (sinc product).
 * - The Bernoulli Brothers: Historical agents who failed to find the exact 
 * sum, experiencing the problem as a "Snare" of professional frustration.
 * * NARRATIVE ARC:
 * The Basel Problem began as a "Snare" for the 17th-century mathematical 
 * community—a simple-looking question that extracted massive cognitive effort 
 * without resolution. Euler converted it into a "Rope" (the Riemann Zeta 
 * Function) which now coordinates modern physics and cryptography. To the 
 * numbers themselves, the limit is a "Mountain"—a fixed, unchangeable 
 * gravitational point of reality.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(basel_era, 1644, 2026).
narrative_ontology:constraint_claim(basel_problem_convergence, mountain).

% Base extractiveness: 0.2
% Rationale: Low. Mathematical truth provides more than it takes. However, 
% partial sums "extract" time because the series converges slowly (O(1/n)).
domain_priors:base_extractiveness(basel_problem_convergence, 0.2).

% Suppression score: 0.1
% Rationale: Low. The exact sum pi^2/6 renders all other "guesses" 
% invisible/invalid, but it is an enabling truth for number theory.
domain_priors:suppression_score(basel_problem_convergence, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(basel_problem_convergence, extractiveness, 0.2).
narrative_ontology:constraint_metric(basel_problem_convergence, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from the structure of analysis.
domain_priors:emerges_naturally(basel_problem_convergence).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(basel_problem_convergence, quantum_mechanics). % Zeta regularisation.
constraint_beneficiary(basel_problem_convergence, analytical_number_theory).
constraint_victim(basel_problem_convergence, computational_brute_force). % Slow convergence.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PARTIAL SUM SEQUENCE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The sequence follows the sum of 1/n^2 blindly.
   WHEN: immediate - True for every term added.
   WHERE: trapped - Bound within the real line, converging on pi^2/6.
   SCOPE: local - Immediate neighborhood of the current value.
   
   WHY THIS CLASSIFICATION:
   For the iterates of the series, the sum is a natural law. They have no 
   agency to "choose" a different limit. The convergence is an inescapable 
   Mountain of mathematical gravity.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    basel_problem_convergence,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LEONHARD EULER / THE UN - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to establish the identity and tools.
   WHEN: civilizational - Long-term impact on the history of math.
   WHERE: mobile - Moving between different areas of math (Analysis/Trigonometry).
   SCOPE: global - Universal application across all mathematics.
   
   WHY THIS CLASSIFICATION:
   For Euler, the Basel solution was a "Rope"—a functional tool for 
   coordination. It linked the discrete world of integers to the continuous 
   world of circles, providing a standard of achievement for all later 
   work in Zeta functions.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    basel_problem_convergence,
    rope,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EARLY BERNOULLIS (FRUSTRATED SEARCH) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Despite their genius, they couldn't solve it.
   WHEN: biographical - Years of their lives spent on a result that eluded them.
   WHERE: constrained - Stuck with 17th-century mathematical tools.
   SCOPE: local - Their specific career and research.
   
   WHY THIS CLASSIFICATION:
   For those who face the problem without the necessary tools, the Basel 
   problem is a "Snare." It extracts massive labor and reputation (frustration) 
   without yielding a result, "strangling" the mathematician with its 
   deceptively simple appearance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    basel_problem_convergence,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(basel_problem_convergence, E),
    E >= 0.15,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(basel_problem_tests).

test(convergence_fate_variance) :-
    % Iterates -> Mountain
    constraint_indexing:constraint_classification(basel_problem_convergence, Type1, context(powerless, immediate, trapped, local)),
    % Euler -> Rope
    constraint_indexing:constraint_classification(basel_problem_convergence, Type2, context(institutional, civilizational, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(computational_extraction_penalty) :-
    % A powerless searcher in a local range sees it as a Snare.
    constraint_indexing:constraint_classification(basel_problem_convergence, snare, context(powerless, biographical, constrained, local)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(basel_problem_convergence).

:- end_tests(basel_problem_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I chose Mountain as the "Actual" type because mathematical 
 * convergence is a natural law.
 * 2. PERSPECTIVE SHIFT: The core insight is that for the searcher (Bernoulli), 
 * the lack of a constructive path makes the constraint an extractive "Snare."
 * 3. EXTRACTIVENESS (0.2): This reflects the "cost" of the slow convergence. 
 * Summing the first 1,000 terms only gets you 2 decimal places of pi^2/6. 
 * This is a high computational extraction for very little truth gain.
 */

% OMEGA IDENTIFICATION
omega_variable(
    precision_threshold,
    "At what point does the 'Mountain' of the limit become a 'Scaffold' of floating-point error?",
    resolution_mechanism("Numerical audit of double-precision sum drift in the first $10^9$ iterates."),
    impact("If noise exceeds the delta: The Mountain is functionally a Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Numerical Approximation
 * Viability: How most people "use" the sum.
 * Suppression: Rejected by the professional "Rope" of pure math, which 
 * demands exactness.
 * * ALTERNATIVE 2: The Harmonic Series (Divergent)
 * Viability: The "Evil Twin" of Basel.
 * Suppression: Explicitly differentiated by P-test; Basel is the "stable" 
 * version of the 1/n summation.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [basel_problem_convergence].
% Analyze: ?- constraint_indexing:multi_index_report(basel_problem_convergence).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
