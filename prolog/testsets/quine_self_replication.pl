% ============================================================================
% CONSTRAINT STORY: quine_self_replication
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Willard Van Orman Quine (1951) / Kleene's Second Recursion Theorem
% ============================================================================

:- module(constraint_quine_self_replication, []).

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
 * * constraint_id: quine_self_replication
 * human_readable: Quines (Computational Self-Replication)
 * domain: technological/mathematical
 * temporal_scope: 1951 - Present
 * spatial_scope: Global/Abstract (Turing Complete Systems)
 * * SUMMARY:
 * A Quine is a non-empty computer program which takes no input and produces a 
 * copy of its own source code as its only output. This demonstrates a 
 * fundamental constraint of computability: any Turing-complete system possesses 
 * the latent capability for self-description and replication without external 
 * templates.
 * * KEY AGENTS:
 * - The Source Code (Subject): The powerless agent whose structure must 
 * simultaneously encode logic and its own literal representation.
 * - The Compiler/Interpreter (Institutional): The rule-enforcing environment 
 * that translates the quine's "instructions" into its "physical" output.
 * - The Computer Scientist (Analytical): The observer mapping the "Mountain" 
 * of Kleene's Recursion Theorem through the "Rope" of specific code.
 * * NARRATIVE ARC:
 * The Quine functions as a "Mountain" of logical inevitability—self-replication 
 * is a mandatory property of universal computation. In programming pedagogy, 
 * it is a "Rope" for coordinating an understanding of fixed points. However, 
 * in the context of self-replicating malware or autonomous agents, the Quine 
 * structure acts as a "Noose," extracting the possibility of external control 
 * by enabling entities to "strangle" the host system with unmediated growth.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================= */

% Structural Anchor for extraction
narrative_ontology:interval(quine_era, 1951, 2026).
narrative_ontology:constraint_claim(quine_self_replication, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.2. While a logic "gift," a quine "extracts" the distinction 
% between data and instruction, forcing the environment to process a 
% recursive loop that uses its own identity as fuel.
domain_priors:base_extractiveness(quine_self_replication, 0.2).

% Suppression score (0.0-1.0)
% Rationale: 0.1. Quines do not suppress alternatives; they reveal a hidden 
% property that was previously invisible, though they render the concept of 
% "pure non-replicating logic" functionally incomplete.
domain_priors:suppression_score(quine_self_replication, 0.1).

% Enforcement: Emerges naturally from Kleene's Second Recursion Theorem.
domain_priors:emerges_naturally(quine_self_replication).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(quine_self_replication, extractiveness, 0.2).
narrative_ontology:constraint_metric(quine_self_replication, suppression_requirement, 0.1).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(quine_self_replication, autonomous_agent_developers).
constraint_beneficiary(quine_self_replication, cellular_automata_theory).
constraint_victim(quine_self_replication, static_code_analysis). % Self-replication makes behavior harder to bound.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXECUTING QUINE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The code has zero agency to be anything other than its source.
   WHEN: immediate - The replication is true at the moment of execution.
   WHERE: trapped - Bound within the syntax and semantics of the host language.
   SCOPE: local - Immediate neighborhood of the print buffer.
   
   WHY THIS CLASSIFICATION:
   For the quine itself, its self-replication is a natural law of its internal 
   logic. It cannot "choose" to output a random string if its fixed-point 
   mechanisms are engaged. It is a mathematical Mountain; its identity is its 
   destiny.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    quine_self_replication,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PEDAGOGICAL INSTRUCTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design curriculum and coding challenges.
   WHEN: biographical - Spanning the duration of a student's introduction to logic.
   WHERE: mobile - Can choose any Turing-complete language to demonstrate the paradox.
   SCOPE: global - Universal application in CS education.
   
   WHY THIS CLASSIFICATION:
   For the educator, the quine is a "Rope"—a tool for functional coordination. 
   It allows them to "pull" students toward an understanding of recursion and 
   self-reference, providing a standard of achievement for logical mastery.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quine_self_replication,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CYBERSECURITY DEFENDER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools to analyze but is bound by the quine's logic.
   WHEN: immediate - Real-time defense against self-replicating worms.
   WHERE: constrained - The host environment provides the "exit" that the quine exploits.
   SCOPE: national - Protecting state infrastructure from autonomous threats.
   
   WHY THIS CLASSIFICATION:
   For the defender, the ability for code to quine itself is a "Noose." It 
   "strangles" containment strategies because the code carries its own 
   blueprints for revival. It extracts massive compute and human overhead 
   (extraction) to suppress a process that the underlying "Mountain" of logic 
   naturally permits.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    quine_self_replication,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- 
    domain_priors:base_extractiveness(quine_self_replication, E),
    E >= 0.15,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(quine_self_replication_tests).

test(multi_perspective_variance) :-
    % Code -> Mountain
    constraint_indexing:constraint_classification(quine_self_replication, Type1, context(individual_powerless, immediate, trapped, local)),
    % Instructor -> Rope
    constraint_indexing:constraint_classification(quine_self_replication, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(autonomous_noose_insight) :-
    % Demonstrates that for the defender, the self-replication is a Noose
    constraint_indexing:constraint_classification(quine_self_replication, noose, context(individual_moderate, immediate, constrained, national)).

test(emergence) :-
    domain_priors:emerges_naturally(quine_self_replication).

:- end_tests(quine_self_replication_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.2): Chose this because a Quine is essentially 
 * "selfish" logic. It extracts the processor's resources solely to reproduce 
 * its own state, creating an asymmetric benefit for the "code-organism" at 
 * the expense of the host's utility.
 * 2. CLASSIFICATION RATIONALE: Chose Code (Passive), Instructor (Active/Tool), 
 * and Defender (Victim) to highlight how self-replication shifts from 
 * "Curiosity" to "Weapon."
 * 3. OMEGA IDENTIFICATION: Formalized the "Quine Entropy" uncertainty—can 
 * a quine evolve into something non-quine-like without external input?
 */

% OMEGA IDENTIFICATION
omega_variable(
    autonomous_evolutionary_drift,
    "Can a self-replicating quine spontaneously evolve complex new traits (Mountain) or is it a closed loop (Scaffold)?",
    resolution_mechanism("Long-term observation of quine-based digital life simulations with random bit-flip mutations."),
    impact("If Mountain: Quines are the seeds of true digital life. If Scaffold: They are mere parrots."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: External Template Replication (e.g., DNA/Ribosome model)
 * Viability: Most real-world biological systems use an external reader 
 * (Ribosome) to replicate a template (DNA).
 * Suppression: Rejected in pure "computational" quining because the goal 
 * is to prove that the code is its own ribosome.
 * * ALTERNATIVE 2: Hard-coded Self-Copying (Impossible Paradox)
 * Viability: Trying to write a program that simply contains its own source 
 * string as a literal.
 * Suppression: Suppressed by the "Infinite Regress" problem; the string 
 * would have to contain a string that contains a string... ad infinitum.
 * * CONCLUSION:
 * The existence of Alternative 2's failure proves that the Quine's specific 
 * "Mountain" (the Kleene fixed-point) is the only viable path to true 
 * self-description.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_quine_self_replication].
 * 2. Multi-perspective: ?- multi_index_report(quine_self_replication).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
