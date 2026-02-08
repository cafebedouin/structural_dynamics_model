% ============================================================================
% CONSTRAINT STORY: van_der_waerden_theorem
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Bartel Leendert van der Waerden (1927) / Ramsey Theory
% ============================================================================

:- module(constraint_van_der_waerden_theorem, []).

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
 * * constraint_id: van_der_waerden_theorem
 * human_readable: Van der Waerden's Theorem
 * domain: technological
 * temporal_scope: Permanent (Universal Mathematical Law)
 * spatial_scope: Global (Formal Systems)
 * * SUMMARY:
 * Van der Waerden's theorem states that for any given positive integers k and r, 
 * there exists a number N such that if the integers {1, 2, ..., N} are colored 
 * with r colors, then there is at least one monochromatic arithmetic progression 
 * of length k. It is a fundamental result in Ramsey theory, proving that 
 * complete disorder is impossible.
 * * KEY AGENTS:
 * - The Ramsey Theorist: Uses the theorem to find structured patterns in 
 * seemingly random partitions.
 * - The Finitist: Confronted with the astronomical size of the Van der 
 * Waerden numbers W(k, r), which are often beyond the reach of standard 
 * computation.
 * - The Formal System (Arithmetic): The substrate that guarantees order 
 * arises from scale.
 * * NARRATIVE ARC:
 * The theorem functions as a "Mountain" of inevitable structure; it is a 
 * law of the universe that large enough systems must contain patterns. 
 * For the mathematician, it is a "Rope" for proving lower bounds in 
 * combinatorics. However, for the computer scientist seeking the exact 
 * value of N, it becomes a "Snare," as the search space grows at an 
 * ackermannian rate, strangling computational resources.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(van_der_waerden_interval, 0, 10).
narrative_ontology:constraint_claim(van_der_waerden_theorem, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "randomness" from the system. It forces structure 
% upon any large set, preventing the existence of "perfect" chaos.
domain_priors:base_extractiveness(van_der_waerden_theorem, 0.2).

% Suppression score (0.0-1.0)
% Rationale: It suppresses the alternative of "disorder." In a system 
% governed by this theorem, the possibility of a purely random partition 
% vanishes once the threshold N is reached.
domain_priors:suppression_score(van_der_waerden_theorem, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(van_der_waerden_theorem, extractiveness, 0.2).
narrative_ontology:constraint_metric(van_der_waerden_theorem, suppression_requirement, 0.5).

% Enforcement requirements
domain_priors:emerges_naturally(van_der_waerden_theorem).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(van_der_waerden_theorem, ramsey_theorists).
constraint_victim(van_der_waerden_theorem, [chaos_theorists, computational_resources]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL LOGICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the laws of information.
   WHEN: civilizational - Viewing logic as a permanent substrate.
   WHERE: trapped - Logic cannot bypass the pigeonhole principle or induction.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the logician, the theorem is a Mountain. It is an unchangeable 
   mathematical fact. Complete disorder is logically impossible in a 
   sufficiently large set of integers; this is a fixed peak in the 
   landscape of Ramsey Theory.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    van_der_waerden_theorem,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMBINATORIAL STRATEGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define and utilize formal rules for proofs.
   WHEN: biographical - Achieving results within a career.
   WHERE: arbitrage - Can use the theorem to bound other unknown systems.
   SCOPE: national - Specific to the academic or professional domain.
   
   WHY THIS CLASSIFICATION:
   For the strategist, the theorem is a Rope. It is a coordination mechanism 
   that provides a guarantee of structure. By knowing that a monochromatic 
   progression must exist, they can "pull" other proofs forward by 
   leveraging this guaranteed order.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    van_der_waerden_theorem,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COMPUTATIONAL ALGORITHMIST - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to the limits of compute and time.
   WHEN: immediate - The current task of finding a specific W(k, r) value.
   WHERE: trapped - Limited by the extreme growth of the Van der Waerden numbers.
   SCOPE: local - Immediate workstation.
   
   WHY THIS CLASSIFICATION:
   For the computer scientist trying to calculate W(5, 2), the theorem is 
   a Snare. The upper bounds are so large (Gowers' bounds) that they 
   effectively strangle computational possibility. The theorem promises 
   the number exists, but the complexity of finding it is a trap that 
   tightens as k or r increases.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    van_der_waerden_theorem,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(van_der_waerden_theorem_tests).

test(multi_perspective_variance) :-
    % Perspective 1: Analyst sees Mountain
    constraint_indexing:constraint_classification(van_der_waerden_theorem, mountain, context(agent_power(analytical), _, _, _)),
    % Perspective 2: Institutional sees Rope
    constraint_indexing:constraint_classification(van_der_waerden_theorem, rope, context(agent_power(institutional), _, _, _)),
    % Perspective 3: Powerless sees Snare
    constraint_indexing:constraint_classification(van_der_waerden_theorem, snare, context(agent_power(powerless), _, _, _)).

test(power_extractiveness_complexity) :-
    % Powerless algorists feel the total extraction of compute time (Snare).
    % Institutional mathematicians use it to bound other theories (Rope).
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    domain_priors:base_extractiveness(van_der_waerden_theorem, Score),
    Score > 0.1.

test(time_immutability_scale) :-
    % Long-term civilizational logic = Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(van_der_waerden_theorem_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.2):
 * Reasoning: Low. The theorem extracts "entropy" or "disorder" from the 
 * mathematical landscape. It is not coercive in a social sense, but 
 * it forces structure where one might expect none.
 * * 2. SUPPRESSION SCORE (0.5):
 * Reasoning: The theorem suppresses the possibility of long-term random 
 * coloring. Once the scale is large enough, disorder is forbidden.
 * * 3. PERSPECTIVE SELECTION:
 * The Analyst (Mountain) sees the Law. The Strategist (Rope) sees the 
 * Guarantee. The Algorist (Snare) sees the Complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vd_waerden_exact_growth,
    "What is the exact growth rate of the Van der Waerden numbers W(k, r)?",
    resolution_mechanism("Mathematical proof narrowing the gap between Gowers' upper bounds and current lower bounds"),
    impact("If Gowers-like: It's a permanent Snare for computation. If significantly lower: It becomes a Rope for computer science."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hales-Jewett Theorem
 * Viability: High. A much more general version of Van der Waerden's theorem 
 * applying to n-dimensional cubes.
 * Suppression: None, they are mutually reinforcing parts of Ramsey theory.
 * * CONCLUSION:
 * Since there are no "chaos-permitting" alternatives for large N, this 
 * constraint remains a Mountain for the universe, but the specific 
 * difficulty of computing N creates a Snare for the individual actor.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_van_der_waerden_theorem].
 * 2. Multi-perspective: ?- multi_index_report(van_der_waerden_theorem).
 * 3. Run tests: ?- run_tests(van_der_waerden_theorem_tests).
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
domain_priors:theater_ratio(van_der_waerden_theorem, 0.03).
narrative_ontology:constraint_metric(van_der_waerden_theorem, theater_ratio, 0.03).
