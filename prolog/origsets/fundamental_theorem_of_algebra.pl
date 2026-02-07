% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_algebra
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Fundamental Theorem of Algebra (Gauss/Argand/d'Alembert)
% ============================================================================

:- module(constraint_fta, []).

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
 * * constraint_id: fundamental_theorem_of_algebra
 * human_readable: Fundamental Theorem of Algebra (FTA)
 * domain: mathematical
 * temporal_scope: 1799 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Complex Plane)
 * * SUMMARY:
 * The FTA states that every non-constant single-variable polynomial with complex 
 * coefficients has at least one complex root. In terms of constraints, it 
 * establishes the "completeness" of the complex number system, ensuring that 
 * algebraic equations of degree n always yield n solutions.
 * * KEY AGENTS:
 * - The Polynomial (Subject): The passive mathematical object whose roots 
 * are predestined by its coefficients and degree.
 * - The Numerical Analyst (Institutional): An agent using the theorem as a 
 * guarantee to build robust solvers (e.g., Jenkins-Traub or Durand-Kerner).
 * - The "Real" Number Purist (Victim): An agent constrained to the real line, 
 * for whom the FTA acts as an invasive force requiring the use of "imaginary" numbers.
 * * NARRATIVE ARC:
 * The FTA begins as a "Mountain" of natural law—an inescapable truth of 
 * mathematical reality. For practitioners, it functions as a "Rope," 
 * providing a coordination mechanism for engineering stability and 
 * signal processing. However, for those trapped in lower-dimensional 
 * number systems (like the Reals), it acts as a "Snare," proving that 
 * their system is "incomplete" and forcing the extraction of cognitive 
 * labor to navigate the complex plane.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(fta_era, 1799, 2026).
narrative_ontology:constraint_claim(fundamental_theorem_of_algebra, mountain).

% Base extractiveness: 0.1
% Rationale: Mathematical truths are generally non-extractive, but the FTA 
% "extracts" the possibility of rootless polynomials to provide a stable system.
domain_priors:base_extractiveness(fundamental_theorem_of_algebra, 0.1).

% Suppression score: 0.2
% Rationale: It suppresses the existence of "unsolvable" polynomials within 
% its domain, rendering alternative "rootless" algebraic universes invalid.
domain_priors:suppression_score(fundamental_theorem_of_algebra, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, 0.1).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the axioms of the complex field.
domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(fundamental_theorem_of_algebra, control_engineers).
constraint_beneficiary(fundamental_theorem_of_algebra, complexity_theorists).
constraint_victim(fundamental_theorem_of_algebra, real_variable_simplicity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POLYNOMIAL ITERATE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The polynomial cannot "choose" to have no roots.
   WHEN: immediate - The root exists at the moment the polynomial is defined.
   WHERE: trapped - Bound within the complex plane C.
   SCOPE: local - Immediate neighborhood of the roots.
   
   WHY THIS CLASSIFICATION:
   For the mathematical object, the FTA is a "Mountain." There are zero degrees 
   of freedom; the existence of the root is a fixed, unchangeable law of its 
   being.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    fundamental_theorem_of_algebra,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CONTROL SYSTEMS ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design filters and feedback loops.
   WHEN: biographical - Spanning the operational life of a machine.
   WHERE: mobile - Can adjust coefficients to move roots (poles) for stability.
   SCOPE: global - Applicable across all linear time-invariant systems.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the FTA is a "Rope." It is a functional tool that 
   guarantees a solution exists, allowing them to coordinate the 
   stability of physical systems (ensuring poles stay in the left-half plane).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fundamental_theorem_of_algebra,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STUDENT OF REAL ANALYSIS - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Subject to the curriculum and the limits of R.
   WHEN: immediate - Facing the failure of x^2 + 1 = 0 on the real line.
   WHERE: constrained - Knowing a solution exists "elsewhere" but unable to see it.
   SCOPE: local - A specific homework problem.
   
   WHY THIS CLASSIFICATION:
   When confined to Real numbers, the FTA is a "Snare." It proves the 
   incompleteness of the student's reality, "strangling" the simplicity of 
   the number line and forcing a transition into a higher-dimensional 
   complexity that they may not be prepared for.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    fundamental_theorem_of_algebra,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(fundamental_theorem_of_algebra, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(fta_tests).

test(completeness_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, Type1, context(powerless, immediate, trapped, local)),
    % Engineer -> Rope
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(extraction_of_possibility) :-
    % Even low extraction is felt when the agent is trapped.
    constraint_indexing:extractiveness_for_agent(fundamental_theorem_of_algebra, context(powerless, immediate, trapped, local), Score),
    Score > 0.05.

test(natural_emergence) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

:- end_tests(fta_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SELECTION: Chose the "Student of Real Analysis" as the 
 * powerless agent to highlight the "Snare" aspect—the realization that 
 * one's current system is lacking.
 * 2. EXTRACTIVENESS: Set at 0.1. While math is a "gift," the FTA effectively 
 * forbids certain mathematical structures (non-closed algebraic fields 
 * over C), which is a form of structural extraction.
 * 3. SUPPRESSION: Set at 0.2 because the FTA renders the "search for 
 * rootless complex polynomials" a futile and invisible path.
 */

% OMEGA IDENTIFICATION
omega_variable(
    constructive_finding_complexity,
    "Does the 'Rope' of existence help find roots, or is it a 'Snare' of unreachable solutions?",
    resolution_mechanism("Comparison of search time for degree-1000 polynomials across different fields."),
    impact("If unsolvable: Snare. If solvable: Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Real-only Algebra
 * Viability: The world before the 16th century.
 * Suppression: Ineffective for describing vibration, rotation, or stability.
 * Evidence: The existence of x^2 + 1 = 0.
 * * ALTERNATIVE 2: Quaternions / Octonions
 * Viability: Higher dimensional algebras.
 * Suppression: The FTA only applies to commutative fields; non-commutative 
 * systems are "suppressed" by the FTA's dominance in standard curriculum.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [fundamental_theorem_of_algebra].
% Run: ?- constraint_indexing:multi_index_report(fundamental_theorem_of_algebra).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(fnudamental_theorem_of_algebra, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(fnudamental_theorem_of_algebra, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(fnudamental_theorem_of_algebra, snare, agent_power(powerless)).
