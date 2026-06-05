% ============================================================================
% CONSTRAINT STORY: reciprocity_laws_math
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Gauss's Quadratic Reciprocity (1801); Artin Reciprocity (1927)
% ============================================================================

:- module(constraint_reciprocity_laws, []).

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
 * * constraint_id: reciprocity_laws_math
 * human_readable: Mathematical Reciprocity Laws
 * domain: mathematical/technological
 * temporal_scope: 1801 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Number Fields)
 * * SUMMARY:
 * Reciprocity laws, beginning with Gauss's Quadratic Reciprocity, describe a 
 * deep symmetry in the behavior of prime numbers. They state that the 
 * solvability of an equation $x^2 = p \pmod q$ is intrinsically linked to the 
 * solvability of $x^2 = q \pmod p$. This functions as a structural constraint 
 * on how primes interact across different arithmetic dimensions.
 * * KEY AGENTS:
 * - The Prime Couple (p, q): Powerless subjects whose relationship is 
 * predestined by the law.
 * - The Cryptographer: An institutional agent using the law as a coordination 
 * mechanism for efficient primality testing and key generation.
 * - The Langlands Researcher: An analytical observer seeking a "Grand Unified" 
 * reciprocity that links all of mathematics.
 * * NARRATIVE ARC:
 * To the numbers themselves, the law is a "Mountain"—an unchangeable, 
 * unyielding symmetry of the universe. To the cryptographer, it is a "Rope"—a 
 * tool that allows them to "jump" across complex calculations with ease. 
 * However, in the realm of Higher Reciprocity (Cubic/Quartic), the 
 * complexity of the law acts as a "Snare" for practitioners, extracting 
 * massive cognitive labor to resolve seemingly simple arithmetic questions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(reciprocity_laws_math, 1801, 2026).
narrative_ontology:constraint_claim(reciprocity_laws_math, mountain).

% Base extractiveness: 0.1
% Rationale: Pure math is largely non-extractive. However, it "extracts" the 
% possibility of asymmetric primality (e.g., p is a residue mod q but q isn't mod p)
% to enforce structural harmony.
domain_priors:base_extractiveness(reciprocity_laws_math, 0.1).

% Suppression score: 0.1
% Rationale: It does not suppress other theories, though it renders 
% "non-reciprocal" universes logically impossible within standard axioms.
domain_priors:suppression_score(reciprocity_laws_math, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, 0.1).
narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from the structure of number fields.
domain_priors:emerges_naturally(reciprocity_laws_math).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(reciprocity_laws_math, asymmetric_cryptography).
constraint_beneficiary(reciprocity_laws_math, analytical_number_theory).
constraint_victim(reciprocity_laws_math, computational_inefficiency). % In the absence of the law.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRIME NUMBER ($p$) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The prime has no agency over its residue status.
   WHEN: immediate - The law is true for all primes simultaneously.
   WHERE: trapped - Bound within the rigid structure of the integers.
   SCOPE: global - Applies across all number fields.
   
   WHY THIS CLASSIFICATION:
   For the prime $p$, the fact that its "quadratic character" depends on $q$ 
   is a natural law. It is an unchangeable, fixed feature of its mathematical 
   reality with zero degrees of freedom.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    reciprocity_laws_math,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RSA CRYPTOGRAPHER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design standards (NIST, RSA) using the law.
   WHEN: biographical - Planning security for the lifecycle of a system.
   WHERE: mobile - Can choose different primes or reciprocity levels.
   SCOPE: global - Worldwide security coordination.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the law is a "Rope"—a functional tool. It allows for the 
   efficient coordination of prime-finding algorithms (like Miller-Rabin), 
   ensuring that various security sub-systems are pulled toward a standard of 
   "absolute" security.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    reciprocity_laws_math,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: HIGHER RECIPROCITY ANALYST - Snare
   --------------------------------------------------------------------------
   WHO: powerless - A graduate student/researcher bound by complexity.
   WHEN: immediate - Every proof step is a struggle against the abstraction.
   WHERE: constrained - Knowing the law exists but unable to easily compute it.
   SCOPE: local - A specific, intractable problem in class field theory.
   
   WHY THIS CLASSIFICATION:
   In the realm of Cubic or Artin reciprocity, the law becomes a "Snare." 
   It promises a "simple" relationship but extracts immense cognitive energy 
   and years of study (extraction) to yield the result. The practitioner 
   is "strangled" by the very abstraction meant to simplify the field.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    reciprocity_laws_math,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(reciprocity_laws_math, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(reciprocity_laws_math_tests).

test(symmetry_fate_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(reciprocity_laws_math, Type1, context(powerless, immediate, trapped, global)),
    % Cryptographer -> Rope
    constraint_indexing:constraint_classification(reciprocity_laws_math, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(computational_labor_extraction) :-
    % A researcher (powerless) in a local/constrained context sees the higher law as a Snare.
    constraint_indexing:constraint_classification(reciprocity_laws_math, snare, context(powerless, immediate, constrained, local)).

test(emergence) :-
    domain_priors:emerges_naturally(reciprocity_laws_math).

:- end_tests(reciprocity_laws_math_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Mathematical laws are the purest form of "Mountain" 
 * (Natural Law). However, their application in technology (Cryptography) 
 * transforms them into "Ropes" (Tools).
 * 2. SNARE ANALOGY: The frustration of "Higher Reciprocity" is a unique form 
 * of intellectual "Snare." It's a promise of symmetry that is so hard to 
 * prove that it extracts a "tithe" of a researcher's life.
 * 3. EXTRACTIVENESS (0.1): Low, because the universe doesn't charge money for 
 * arithmetic, but the *following* of the law requires an "extraction" of 
 * cognitive focus.
 */

% OMEGA IDENTIFICATION
omega_variable(
    non_abelian_generalization,
    "Will the Langlands Program provide a 'Rope' (functional tool) for the non-abelian case, or is it a permanent 'Mountain'?",
    resolution_mechanism("Verification of the global Langlands correspondence for GL(n)."),
    impact("If Yes: The entire number line becomes a navigable 'Rope'. If No: It remains a distant 'Mountain'."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Brute Force Verification (Euler's method before Gauss)
 * Viability: Checking residues manually for every prime.
 * Suppression: Replaced by Gauss because Reciprocity is a more efficient 
 * "Rope" for coordination.
 * * ALTERNATIVE 2: Pure Probabilistic Heuristics
 * Viability: Assuming residues are random.
 * Suppression: Suppressed by the proof of Reciprocity, which shows the 
 * "chaos" of primes is actually a hidden "Mountain" of order.
 * * CONCLUSION:
 * The existence of the Reciprocity Law as a "Mountain" makes the brute-force 
 * "Scaffolds" of the 18th century look like primitive errors.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [reciprocity_laws_math].
% Analyze: ?- constraint_indexing:multi_index_report(reciprocity_laws_math).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
