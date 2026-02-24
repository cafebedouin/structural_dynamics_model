% ============================================================================
% CONSTRAINT STORY: prime_number_theorem
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Prime Number Theorem (PNT) / Asymptotic Distribution of Primes
% ============================================================================

:- module(constraint_pnt, []).

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
 * * constraint_id: prime_number_theorem
 * human_readable: Prime Number Theorem (Asymptotic Density)
 * domain: mathematical/technological
 * temporal_scope: 1896 - Present (Civilizational)
 * spatial_scope: Global/Abstract (The Number Line)
 * * SUMMARY:
 * The Prime Number Theorem (PNT) describes the asymptotic distribution of prime 
 * numbers among the positive integers. It formalizes the intuitive idea that 
 * primes become less frequent as they become larger, specifically stating that 
 * the number of primes less than x is approximately x/ln(x).
 * * KEY AGENTS:
 * - The Prime Number (Subject): The mathematical entity whose density and 
 * occurrence are dictated by the laws of arithmetic.
 * - The Cryptographer (Institutional): An agent who uses the predictable 
 * distribution of primes to coordinate global security (RSA/ECC).
 * - The Search Algorithm (Individual Powerless): A computational process 
 * searching for large primes, subject to the "thinning" reality of PNT.
 * * NARRATIVE ARC:
 * PNT is the ultimate "Mountain" of number theory—an inescapable density law. 
 * For the cryptographer, it functions as a "Rope," providing a coordination 
 * mechanism that ensures enough "raw material" (primes) exists to secure the 
 * world's data. However, for a high-precision algorithm, the logarithmic 
 * decay is a "Snare," extracting ever-increasing computational energy for 
 * diminishing returns in search results.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(pnt_era, 1896, 2026).
narrative_ontology:constraint_claim(prime_number_theorem, mountain).

% Base extractiveness: 0.15
% Rationale: While mathematical truth is free, the realization of PNT 
% "extracts" computational resources (time/energy) as the logarithmic 
% density decreases at scale.
domain_priors:base_extractiveness(prime_number_theorem, 0.15).

% Suppression score: 0.1
% Rationale: It does not suppress other truths, but it renders alternative 
% prime distributions (e.g., linear density) logically impossible.
domain_priors:suppression_score(prime_number_theorem, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(prime_number_theorem, extractiveness, 0.15).
narrative_ontology:constraint_metric(prime_number_theorem, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from the structure of the integers.
domain_priors:emerges_naturally(prime_number_theorem).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(prime_number_theorem, system_security). % RSA reliability.
constraint_beneficiary(prime_number_theorem, mathematicians). % Proof-of-concept for chaos/order.
constraint_victim(prime_number_theorem, computational_efficiency). % Search time at high x.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRIME NUMBER (SUBJECT) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The prime has no choice in its density or location.
   WHEN: immediate - The law applies across the entire number line simultaneously.
   WHERE: trapped - Bound within the logical structure of the integers.
   SCOPE: global - Applies to all integers to infinity.
   
   WHY THIS CLASSIFICATION:
   For the numbers themselves, the distribution is a "Mountain." There is no 
   volition or agency; the density is an unchangeable feature of the universe.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    prime_number_theorem,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CRYPTOGRAPHER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design standards (NIST/ISO) based on this law.
   WHEN: biographical - Planning security for the lifecycle of modern encryption.
   WHERE: mobile - Can choose different prime sizes or algorithms to adjust.
   SCOPE: global - Managing the security of the global internet.
   
   WHY THIS CLASSIFICATION:
   For the architect of security systems, PNT is a "Rope." It is a functional 
   coordination tool that guarantees a sufficient supply of large primes 
   exists to make key collision statistically impossible.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    prime_number_theorem,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LARGE-PRIME SEARCHER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - A single CPU/GPU bound by the search space.
   WHEN: immediate - Each cycle spent in a "prime desert" is an extraction.
   WHERE: constrained - Knowing a prime exists but unable to jump to it.
   SCOPE: local - A specific range of the number line (e.g., near 2^82,589,933 - 1).
   
   WHY THIS CLASSIFICATION:
   When searching for the next Mersenne prime or high-bit RSA factor, PNT acts 
   as a "Snare." It proves that the "space" of freedom (primes) is getting 
   smaller, extracting massive energy (FLOPs) while tightening the difficulty 
   of the search.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    prime_number_theorem,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(prime_number_theorem, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(pnt_tests).

test(multi_perspective_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(prime_number_theorem, Type1, context(powerless, immediate, trapped, global)),
    % Architect -> Rope
    constraint_indexing:constraint_classification(prime_number_theorem, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(computational_snare_penalty) :-
    % A powerless searcher in a local range sees it as a Snare.
    constraint_indexing:constraint_classification(prime_number_theorem, snare, context(powerless, immediate, constrained, local)).

test(extraction_scaling) :-
    % Extraction should be non-zero for mathematical existence checks.
    domain_priors:base_extractiveness(prime_number_theorem, E),
    E > 0.1.

:- end_tests(pnt_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: PNT is the perfect example of a "Mountain" (Natural Law) 
 * that is used as a "Rope" (Tool) by those in power. 
 * 2. SNARE ANALOGY: The "Snare" classification for the search algorithm is a 
 * critical realization. As x -> infinity, the probability of finding a 
 * prime is 1/ln(x). This logarithmic tightening is a literal resource 
 * extraction (FLOPs/Electricity) for the "individual" search agent.
 * 3. EXTRACTIVENESS: Set at 0.15 to reflect that while the law is free, the 
 * *following* of the law at high magnitudes is computationally expensive.
 */

% OMEGA IDENTIFICATION
omega_variable(
    riemann_hypothesis_link,
    "Does the 'Mountain' remain stable if the Riemann Hypothesis is proven or disproven?",
    resolution_mechanism("Verification of the error term bound Li(x) - pi(x)."),
    impact("If RH is true: The Mountain is smooth and predictable. If false: The Mountain has chaotic spikes."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Chebyshev's Bounds
 * Viability: Provides upper and lower bounds but lacks the asymptotic equality.
 * Suppression: Replaced by PNT for higher-precision coordination (Rope).
 * * ALTERNATIVE 2: Pure Probabilistic Models (Cramér's Model)
 * Viability: Primes as a random walk.
 * Suppression: Useful as a heuristic, but "suppressed" by the definitive proof 
 * of PNT for rigorous structural engineering.
 * * CONCLUSION:
 * The existence of PNT as a "Mountain" makes the probabilistic "Ropes" of 
 * earlier eras look like "Scaffolds"—temporary structures used until the 
 * final truth was reached.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [prime_number_theorem].
% Report: ?- constraint_indexing:multi_index_report(prime_number_theorem).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
