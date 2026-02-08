% ============================================================================
% CONSTRAINT STORY: galois_theory_symmetry
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Évariste Galois (1832) / Abstract Algebra / Field Theory
% ============================================================================

:- module(constraint_galois_theory, []).

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
 * * constraint_id: galois_theory_symmetry
 * human_readable: Galois Theory (Symmetry of Roots)
 * domain: mathematical/theoretical
 * temporal_scope: 1832 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Algebraic Fields)
 * * SUMMARY:
 * Galois Theory provides a connection between field theory and group theory. 
 * It characterizes the solvability of polynomials by radicals through the 
 * symmetry group of their roots (the Galois group). Most famously, it proves 
 * the insolvability of the general quintic equation.
 * * KEY AGENTS:
 * - The Polynomial (Subject): The mathematical object whose roots are 
 * governed by an inescapable internal symmetry.
 * - Évariste Galois: The historical individual who was "strangled" by the 
 * social/political Snare of 19th-century France while mapping the Mountain 
 * of group symmetry.
 * - The Abstract Algebraist (Institutional): An agent who uses the "Rope" of 
 * Galois correspondence to navigate complex algebraic structures.
 * * NARRATIVE ARC:
 * Galois Theory is the "Mountain" of algebraic limits. It reveals that the 
 * "freedom" to solve equations is restricted by the group structure of their 
 * permutations. Historically, for Galois himself, the theory was a "Rope" of 
 * genius that failed to save him from the political "Snare" of his era.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(galois_era, 1832, 2026).
narrative_ontology:constraint_claim(galois_theory_symmetry, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Low (0.2). It is a structural truth. However, it "extracts" 
% the possibility of "easy" radical solutions for high-degree equations, 
% demanding massive cognitive labor to understand the "hidden" group laws.
domain_priors:base_extractiveness(galois_theory_symmetry, 0.2).

% Suppression score (0.0-1.0)
% Rationale: Low (0.1). It reveals deeper truths rather than hiding them, 
% though it suppresses the "naive" hope for a general formula for the quintic.
domain_priors:suppression_score(galois_theory_symmetry, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(galois_theory_symmetry, extractiveness, 0.2).
narrative_ontology:constraint_metric(galois_theory_symmetry, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from the axioms of fields and groups.
domain_priors:emerges_naturally(galois_theory_symmetry).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(galois_theory_symmetry, theoretical_physicists). % Symmetry groups.
constraint_beneficiary(galois_theory_symmetry, algebraic_number_theorists).
constraint_victim(galois_theory_symmetry, radical_solvability_hope). % The death of the "general formula."

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ROOTS OF THE QUINTIC - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The roots have no agency over their own symmetry.
   WHEN: immediate - The law is true at the moment the coefficients are set.
   WHERE: trapped - Bound within the field extension $E/F$.
   SCOPE: local - Immediate neighborhood of the polynomial $P(x)$.
   
   WHY THIS CLASSIFICATION:
   For the roots of $x^5 - x - 1$, the Galois group $S_5$ is an unchangeable 
   destiny. Because $S_5$ is not solvable, the roots *cannot* be expressed 
   by radicals. This is a topological and algebraic Mountain that no 
   computation can circumvent.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    galois_theory_symmetry,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GRADUATE STUDENT - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has the intelligence to learn, but is bound by rigor.
   WHEN: biographical - Spanning the months of a grueling course.
   WHERE: constrained - Knowing the answer exists but lost in the abstraction.
   SCOPE: local - A specific exam or research hurdle.
   
   WHY THIS CLASSIFICATION:
   To the student, the "Galois Correspondence" often feels like a "Snare." 
   It promises a "simple" link between subfields and subgroups, but 
   the path to verifying that link extracts immense cognitive energy and 
   "strangles" intuitive understanding until the "aha" moment occurs.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    galois_theory_symmetry,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(galois_theory_symmetry, E),
    E >= 0.1,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PROFESSIONAL MATHEMATICIAN - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to use the theory to coordinate new proofs.
   WHEN: civilizational - Long-term development of mathematics.
   WHERE: mobile - Moving between algebraic geometry, physics, and number theory.
   SCOPE: global - Universal application.
   
   WHY THIS CLASSIFICATION:
   For the expert, Galois Theory is a "Rope"—a functional coordination 
   mechanism. It allows them to "climb" from the low-level data of coefficients 
   to the high-level structure of groups, making intractable problems solvable 
   by switching representations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    galois_theory_symmetry,
    rope,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (Insights into Algebraic Power)
   ========================================================================== */

:- begin_tests(galois_theory_tests).

test(symmetry_fate_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(galois_theory_symmetry, Type1, context(powerless, immediate, trapped, local)),
    % Expert -> Rope
    constraint_indexing:constraint_classification(galois_theory_symmetry, Type2, context(institutional, civilizational, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(student_frustration_snare) :-
    % A moderate power agent in a constrained/biographical context sees the abstraction as a Snare.
    constraint_indexing:constraint_classification(galois_theory_symmetry, snare, context(individual_moderate, biographical, constrained, local)).

test(emergence) :-
    domain_priors:emerges_naturally(galois_theory_symmetry).

:- end_tests(galois_theory_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.2): 
 * Galois Theory "extracts" the simple world of formulas ($x = \frac{-b...}$) 
 * and replaces it with the "tax" of abstract group theory.
 * * 2. CLASSIFICATION AS MOUNTAIN: 
 * For the polynomial itself, it is the ultimate Mountain. No amount of 
 * arithmetic can "escape" the group structure of the roots.
 * * 3. PERSPECTIVES:
 * Chose Roots (Subject), Student (Victim of abstraction), and Expert 
 * (User of the tool) to illustrate the transition from "strangulation" 
 * to "coordination."
 */

% OMEGA IDENTIFICATION
omega_variable(
    inverse_galois_problem,
    "Is every finite group a Galois group of some extension of Q?",
    resolution_mechanism("Formal proof or counterexample; current status: unsolved for many groups."),
    impact("If Yes: The Mountain is symmetric and complete. If No: The Mountain has 'cracks' (Ropes) we haven't mapped."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Abel-Ruffini Theorem
 * Viability: Proves the same result (insolvability of quintic) but without 
 * the "Rope" of group correspondence.
 * Status: Replaced by Galois because it provides a more functional tool 
 * for coordination across other domains.
 * * ALTERNATIVE 2: Purely Numerical Methods
 * Viability: Using computers to find roots to high precision.
 * Suppression: Rejected by pure mathematicians because it avoids the 
 * "truth" of the structural Mountain.
 * * CONCLUSION:
 * The dominance of Galois Theory over Abel's work is due to its utility as a 
 * "Rope" that links distinct fields of thought.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [galois_theory_symmetry].
% Analyze: ?- constraint_indexing:multi_index_report(galois_theory_symmetry).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
