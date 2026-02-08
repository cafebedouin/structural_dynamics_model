% ============================================================================
% CONSTRAINT STORY: birthday_paradox_collision
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Combinatorial Probability / Cryptography
% ============================================================================

:- module(constraint_birthday_paradox, []).

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
 * * constraint_id: birthday_paradox_collision
 * human_readable: The Birthday Paradox (Collision Probability)
 * domain: mathematical/technological
 * temporal_scope: 1939 (Von Mises) - Present
 * spatial_scope: Global/Abstract (Discrete probability spaces)
 * * SUMMARY:
 * The Birthday Paradox (or problem) demonstrates that in a set of n randomly 
 * chosen people, the probability that a pair of them will have the same 
 * birthday reaches 50% with only 23 people. This functions as a fundamental 
 * constraint on entropy, hashing, and digital uniqueness.
 * * KEY AGENTS:
 * - The Hash Function: A powerless agent whose output space is "compressed" 
 * by the inevitable logic of collisions.
 * - The Security Architect: An institutional agent who uses the paradox as a 
 * "Rope" to determine necessary bit-lengths for digital signatures.
 * - The Attacker (Birthday Attack): An agent who treats the paradox as a 
 * "Rope" (tool) to break cryptosystems.
 * - The Unsuspecting User: An agent for whom the paradox is a "Snare," 
 * strangling their expectation of uniqueness in small data sets.
 * * NARRATIVE ARC:
 * The paradox begins as a counter-intuitive "Mountain" of natural law. In 
 * engineering, it is a "Rope" for coordination (choosing ID lengths). However, 
 * in cryptography, it is a "Snare"—it extracts security from short hashes 
 * by guaranteeing collisions far sooner than intuition suggests.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(birthday_era, 1939, 2026).
narrative_ontology:constraint_claim(birthday_paradox_collision, mountain).

% Base extractiveness: 0.4
% Rationale: It "extracts" security and uniqueness. In a 64-bit hash space, 
% you don't get 2^64 unique items; you get roughly 2^32 before a collision 
% is likely. This "halving" of effective entropy is a structural extraction.
domain_priors:base_extractiveness(birthday_paradox_collision, 0.4).

% Suppression score: 0.25
% Rationale: It suppresses human intuition (which expects a linear 
% relationship) in favor of the exponential reality of combinations.
domain_priors:suppression_score(birthday_paradox_collision, 0.25).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(birthday_paradox_collision, extractiveness, 0.4).
narrative_ontology:constraint_metric(birthday_paradox_collision, suppression_requirement, 0.25).

% Enforcement: Emerges naturally from the Pigeonhole Principle and combinatorics.
domain_priors:emerges_naturally(birthday_paradox_collision).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(birthday_paradox_collision, cryptographic_attackers).
constraint_beneficiary(birthday_paradox_collision, hardware_manufacturers). % Need more storage/bits.
constraint_victim(birthday_paradox_collision, legacy_hash_functions).
constraint_victim(birthday_paradox_collision, intuitive_expectations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HASH VALUE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The bits have no choice in their collision probability.
   WHEN: immediate - True at the moment of the 23rd insertion.
   WHERE: trapped - Bound within the set of 365 (or 2^n) possible states.
   SCOPE: local - Immediate neighborhood of a duplicate value.
   
   WHY THIS CLASSIFICATION:
   For the data point, the paradox is a "Mountain." No amount of "willpower" 
   by the algorithm can change the fact that the probability landscape is 
   shaped by factorials and exponents.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    birthday_paradox_collision,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CRYPTOGRAPHER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to set the bit-length of the hash (e.g., SHA-256).
   WHEN: biographical - Planning the safety of a system for 20+ years.
   WHERE: mobile - Can "exit" the Snare of collisions by doubling the bit-length.
   SCOPE: global - Worldwide standard for digital signatures.
   
   WHY THIS CLASSIFICATION:
   For the architect, the paradox is a "Rope"—a functional coordination tool. 
   Knowing the "Birthday Bound" allows them to coordinate a standard of 
   achievement (security) by simply using twice as many bits as the intended 
   security level.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    birthday_paradox_collision,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BIRTHDAY ATTACKER - Snare
   --------------------------------------------------------------------------
   WHO: powerful - Using the paradox to coerce a system.
   WHEN: immediate - Cracking a signature in real-time.
   WHERE: arbitrage - Exploiting the gap between the system's bits and its 
        actual security.
   SCOPE: global - Targeting global financial or identity systems.
   
   WHY THIS CLASSIFICATION:
   For the attacker, the paradox is a "Snare" they tighten around a weak hash 
   function. It allows them to "extract" a collision (and thus a forgery) 
   asymmetrically, with far less effort than a brute-force search.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    birthday_paradox_collision,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(birthday_paradox_collision, E),
    E >= 0.3,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(birthday_paradox_tests).

test(collision_fate_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(birthday_paradox_collision, Type1, context(powerless, immediate, trapped, local)),
    % Architect -> Rope
    constraint_indexing:constraint_classification(birthday_paradox_collision, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(extraction_of_security) :-
    % Attackers see it as a Snare/Arbitrage opportunity.
    constraint_indexing:constraint_classification(birthday_paradox_collision, snare, context(powerful, immediate, arbitrage, global)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(birthday_paradox_collision).

:- end_tests(birthday_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): I assigned this because the paradox creates 
 * a "hidden tax" on all digital systems. You pay for 128 bits of storage 
 * but only get 64 bits of collision resistance.
 * 2. PERSPECTIVE SHIFT: The core insight is that "knowledge" of the constraint 
 * transforms it from a Snare (for the victim) into a Rope (for the architect).
 * 3. SUPPRESSION: Set at 0.25. The paradox effectively "hides" the feasibility 
 * of collisions from non-experts, which is a form of informational suppression.
 */

% OMEGA IDENTIFICATION
omega_variable(
    non_uniform_birth_distribution,
    "How does the 'Mountain' shift if births are seasonal rather than uniform?",
    resolution_mechanism("Compare collision rates in datasets with actual demographic spikes (e.g., September births)."),
    impact("If Spiked: The probability curve steepens, making the 'Snare' tighter."),
    confidence_without_resolution(high)
).

omega_variable(
    quantum_collision_acceleration,
    "Does Grover's Algorithm turn the Birthday 'Rope' into a 'Snare' for modern hashes?",
    resolution_mechanism("Audit of cube-root vs square-root collision speeds in quantum simulations."),
    impact("If Cube-root: Current bit-length standards are a failing Scaffold."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Brute Force Search (1/N)
 * Viability: Requires checking every single possible value.
 * Suppression: Suppressed by the sheer efficiency of the Birthday Attack 
 * (sqrt(N)), which makes the 1/N alternative functionally invisible to 
 * professional attackers.
 * * ALTERNATIVE 2: Collision-Free Perfect Hashing
 * Viability: Possible for fixed, known datasets.
 * Suppression: Rejected for dynamic systems where inputs are unknown or adversarial.
 * * CONCLUSION:
 * The existence of the sqrt(N) "Mountain" makes any security model based 
 * on 1/N a "Scaffold" that will inevitably collapse under load.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [birthday_paradox_collision].
% Analyze: ?- constraint_indexing:multi_index_report(birthday_paradox_collision).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(birthday_paradox_collison, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(birthday_paradox_collison, snare, agent_power(powerless)).

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
domain_priors:theater_ratio(birthday_paradox_collision, 0.0).
narrative_ontology:constraint_metric(birthday_paradox_collision, theater_ratio, 0.0).
