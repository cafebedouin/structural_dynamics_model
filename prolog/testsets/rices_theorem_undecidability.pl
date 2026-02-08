% ============================================================================
% CONSTRAINT STORY: rices_theorem_undecidability
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Henry Gordon Rice (1953) / Computability Theory
% ============================================================================

:- module(constraint_rices_theorem, []).

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
 * * constraint_id: rices_theorem_undecidability
 * human_readable: Rice's Theorem (Undecidability of Semantic Properties)
 * domain: technological/mathematical
 * temporal_scope: 1953 - Present
 * spatial_scope: Global/Abstract (Universal Computation)
 * * SUMMARY:
 * Rice's Theorem is a fundamental result in computability theory stating that 
 * any non-trivial semantic property of a program (the language recognized by 
 * a Turing machine) is undecidable. This means it is logically impossible to 
 * create a general-purpose tool that can determine behavioral traits of code, 
 * such as "will this program ever crash?" or "is this code malicious?".
 * * KEY AGENTS:
 * - The Turing Machine (Subject): The powerless agent whose behavioral 
 * properties remain fundamentally shrouded from external automated judgment.
 * - The Software Verifier: An institutional agent attempting to coordinate 
 * system safety, for whom the theorem is a functional boundary.
 * - The Malware Analyst: An individual moderate agent who is "strangled" by 
 * the impossibility of a perfect automated detection tool.
 * * NARRATIVE ARC:
 * Rice's Theorem functions as a "Mountain" of logical reality—a wall that 
 * defines the limits of what machines can know about each other. In 
 * professional software engineering, it is a "Rope" for coordination, 
 * allowing developers to accept "heuristic" analysis as the standard of 
 * achievement. However, in cybersecurity, it is a "Snare" that extracts 
 * massive human labor and capital to perform manual audits that the 
 * "Mountain" prevents from being automated.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for Python extraction
narrative_ontology:interval(rices_era, 1953, 2026).
narrative_ontology:constraint_claim(rices_theorem_undecidability, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.15. While a mathematical truth, it "extracts" the possibility 
% of absolute automated correctness, forcing a permanent tax of human oversight.
domain_priors:base_extractiveness(rices_theorem_undecidability, 0.15).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses the hope for "Perfect Verification" tools, 
% rendering them logically impossible and thus functionally invisible.
domain_priors:suppression_score(rices_theorem_undecidability, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rices_theorem_undecidability, extractiveness, 0.15).
narrative_ontology:constraint_metric(rices_theorem_undecidability, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the logic of the Halting Problem.
domain_priors:emerges_naturally(rices_theorem_undecidability).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(rices_theorem_undecidability, complexity_theorists). % Intellectual stability.
constraint_victim(rices_theorem_undecidability, automated_security_vendors). % Cannot deliver "perfect" results.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXECUTING PROGRAM - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The program cannot "show" its property to a decider.
   WHEN: immediate - The limit is true for any single execution or analysis.
   WHERE: trapped - Bound by the laws of recursive enumerability.
   SCOPE: local - Immediate neighborhood of the code logic.
   
   WHY THIS CLASSIFICATION:
   For the code itself, the inability for an external algorithm to classify 
   its non-trivial behavior is a natural law. It is an unyielding Mountain; 
   there are zero degrees of freedom where a "perfect decider" could exist.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    rices_theorem_undecidability,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STATIC ANALYSIS ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define what "Safe" code looks like via Linters.
   WHEN: biographical - Planning the lifecycle of a codebase or language.
   WHERE: mobile - Can choose different subsets of decidable properties to check.
   SCOPE: global - Universal application in software development.
   
   WHY THIS CLASSIFICATION:
   For the architect, Rice's Theorem is a "Rope"—a functional coordination 
   mechanism. It provides the "standard of achievement" that justifies why 
   compilers only give "warnings" or "potential errors." It coordinates the 
   industry's expectations around the fact that perfect proofs are not the goal.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rices_theorem_undecidability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CYBER DEFENSE ENGINEER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools to analyze but is bound by the theorem.
   WHEN: immediate - Every piece of malware is a new instance of the "Snare."
   WHERE: constrained - Forced to use imperfect heuristics (AV/EDR).
   SCOPE: national - Protecting state infrastructure.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the theorem acts as a "Snare." It "strangles" their 
   ability to provide 100% security. It extracts massive cycles of human 
   analysis (extraction) because the "Mountain" of Rice's Theorem means 
   every new behavioral trait must be manually verified or guessed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rices_theorem_undecidability,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- 
    domain_priors:base_extractiveness(rices_theorem_undecidability, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rices_theorem_undecidability_tests).

test(multi_perspective_variance) :-
    % Code -> Mountain
    constraint_indexing:constraint_classification(rices_theorem_undecidability, Type1, context(powerless, immediate, trapped, local)),
    % Architect -> Rope
    constraint_indexing:constraint_classification(rices_theorem_undecidability, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(extraction_scaling) :-
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(rices_theorem_undecidability, ContextPowerless, S1),
    constraint_indexing:extractiveness_for_agent(rices_theorem_undecidability, ContextPowerful, S2),
    S1 > S2.

test(natural_emergence) :-
    domain_priors:emerges_naturally(rices_theorem_undecidability).

:- end_tests(rices_theorem_undecidability_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.15):
 * Reasoning: It is low because it is an abstract law of logic, but not zero 
 * because it creates an "Impossibility Tax" on security.
 * * 2. CLASSIFICATION RATIONALE:
 * Chose Code (Subject), Linter Designer (Utility), and Security Analyst 
 * (Victim) to highlight the gap between "Logic" and "Praxis."
 * * 3. OMEGA IDENTIFICATION:
 * Formalized the uncertainty of "Heuristic Completeness"—can AI 
 * effectively turn this Snare back into a Rope by guessing correctly 
 * 99.9% of the time?
 */

% YOUR OMEGAS HERE:
omega_variable(
    heuristic_asymptotic_completeness,
    "Will Large Language Models or Neural Verifiers provide 99.9% behavioral detection?",
    resolution_mechanism("Audit of false-positive rates in AI-driven static analysis across 10 years."),
    impact("If Yes: The Snare is functionally a Rope. If No: The Snare remains a Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Domain-Specific Languages (DSLs) with Restricted Logic
 * Viability: By using languages that are NOT Turing-complete (e.g., SQL, regex), 
 * Rice's Theorem no longer applies, and properties become decidable.
 * Suppression: Often rejected for general-purpose programming due to lack 
 * of power, but a vital "Rope" for high-security sub-systems.
 * * ALTERNATIVE 2: Proof-Carrying Code
 * Viability: The code comes with a proof of its properties.
 * Suppression: High "extraction" of developer labor makes it rare in 
 * commercial industry.
 * * CONCLUSION:
 * The existence of DSLs (Alternative 1) proves that Rice's Theorem is a 
 * "Mountain" only if we choose the "Rope" of Turing-completeness.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_rices_theorem].
 * 2. Multi-perspective: ?- multi_index_report(rices_theorem_undecidability).
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
domain_priors:theater_ratio(rices_theorem_undecidability, 0.04).
narrative_ontology:constraint_metric(rices_theorem_undecidability, theater_ratio, 0.04).

% --- Analytical perspective classification (missing) ---
% chi = 0.15 * 1.15 (analytical) * 1.2 (global) = 0.207
% Classification: scaffold
constraint_indexing:constraint_classification(rices_theorem_undecidability, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
