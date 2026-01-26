% ============================================================================
% CONSTRAINT STORY: four_color_theorem_topological_bound
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Appel & Haken (1976) / Graph Theory / Topology
% ============================================================================

:- module(constraint_four_color_theorem, []).

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
 * * constraint_id: four_color_theorem_topological_bound
 * human_readable: The Four Color Theorem
 * domain: mathematical/technological
 * temporal_scope: 1852 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Planar Graphs)
 * * SUMMARY:
 * The Four Color Theorem states that no more than four colors are required to 
 * color the regions of any map so that no two adjacent regions have the same 
 * color. It is a fundamental topological constraint on how planar space can 
 * be partitioned and connected.
 * * KEY AGENTS:
 * - The Map Region (Subject): A powerless agent whose "identity" (color) is 
 * constrained by the neighbors it touches.
 * - The Cartographer/Network Architect (Institutional): An agent who uses 
 * the theorem as a "Rope" to optimize resource allocation (frequencies, colors).
 * - The Mathematical Purist (Victim): An agent for whom the computer-assisted 
 * proof is a "Snare," extracting the "elegant human-readable proof" and 
 * replacing it with exhaustive algorithmic verification.
 * * NARRATIVE ARC:
 * The theorem functions as a "Mountain" of planar reality—a hard limit of 
 * geometry that cannot be bypassed. In wireless networking, it is a "Rope" 
 * for coordinating frequency reuse. However, for the history of mathematics, 
 * its proof acted as a "Snare," extracting the traditional "aesthetic of 
 * understanding" (extraction) and "strangling" the era of human-only 
 * verification with the first major computer-dependent proof.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================= */

% Structural Anchor for the Deferential Realism Auditor
narrative_ontology:interval(four_color_era, 1852, 2026).
narrative_ontology:constraint_claim(four_color_theorem_topological_bound, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.25. The theorem "extracts" the possibility of higher-color 
% variety to enforce a lower-bound efficiency. It also "extracts" human 
% intuition in favor of computational brute force.
domain_priors:base_extractiveness(four_color_theorem_topological_bound, 0.25).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses the visibility of 5-color or 6-color 
% "redundant" systems by proving they are mathematically unnecessary for 
% planar partitions.
domain_priors:suppression_score(four_color_theorem_topological_bound, 0.2).

% Enforcement: Emerges naturally from the topology of the plane.
domain_priors:emerges_naturally(four_color_theorem_topological_bound).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, extractiveness, 0.25).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(four_color_theorem_topological_bound, frequency_allocation_engineers).
constraint_beneficiary(four_color_theorem_topological_bound, automated_theorem_provers).
constraint_victim(four_color_theorem_topological_bound, classical_human_intuition).
constraint_victim(four_color_theorem_topological_bound, map_complexity_enthusiasts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL MAP REGION - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The region has no agency over its adjacency.
   WHEN: immediate - The color constraint is true the moment the map is drawn.
   WHERE: trapped - Bound within the 2D plane.
   SCOPE: local - Can only "see" its immediate neighbors.
   
   WHY THIS CLASSIFICATION:
   For a specific region on a map, the limit of 4 colors is an absolute 
   Mountain. It cannot "choose" to be a 5th color that is distinct from its 
   neighbors if the global topology has already constrained the available 
   choices. The arithmetic of adjacency is a natural law.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    four_color_theorem_topological_bound,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SPECTRUM MANAGER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to allocate frequencies or design cellular grids.
   WHEN: biographical - Planning a regional or national wireless network.
   WHERE: mobile - Can adjust tower placement or boundary lines.
   SCOPE: national - Managing a country's spectrum.
   
   WHY THIS CLASSIFICATION:
   For the institutional actor, the theorem is a "Rope"—a functional 
   coordination mechanism. It provides a "standard of achievement" that 
   guarantees they never need more than 4 distinct frequency sets to 
   cover a territory without overlap, "pulling" the system toward 
   maximum efficiency.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    four_color_theorem_topological_bound,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ELEGANCE-SEEKING MATHEMATICIAN - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools to prove but is bound by the theorem's nature.
   WHEN: historical - Spanning the 124 years between conjecture and proof.
   WHERE: constrained - No alternative but to accept a "computer-only" proof.
   SCOPE: global - Affecting the philosophy of all mathematical proof.
   
   WHY THIS CLASSIFICATION:
   For the classical theorist, the Four Color Theorem is a "Snare." It 
   "strangles" the ideal of a beautiful, human-comprehensible proof. 
   By requiring thousands of case-checkings via computer, it extracts the 
   "human element" (extraction) from the proof, "choking" the 
   tradition of Euclidean simplicity.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    four_color_theorem_topological_bound,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(four_color_theorem_topological_bound, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(four_color_tests).

test(multi_perspective_variance) :-
    % Region -> Mountain
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, Type1, context(individual_powerless, immediate, trapped, local)),
    % Manager -> Rope
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, Type2, context(institutional, biographical, mobile, national)),
    Type1 \= Type2.

test(intuition_extraction_penalty) :-
    % The mathematician feels the extraction of human-readability.
    Context = context(individual_moderate, historical, constrained, global),
    constraint_indexing:extractiveness_for_agent(four_color_theorem_topological_bound, Context, Score),
    Score >= 0.2.

test(natural_emergence) :-
    domain_priors:emerges_naturally(four_color_theorem_topological_bound).

:- end_tests(four_color_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.25): 
 * Chose this because the Four Color Theorem represents the first time 
 * mathematics "extracted" the necessity of computer-aid, a major shift 
 * in the "tax" of verification.
 * 2. CLASSIFICATION: 
 * Captured the transition from a "Mountain" of geometry to a "Snare" 
 * for mathematical philosophy due to the brute-force nature of the proof.
 */

% OMEGA IDENTIFICATION
omega_variable(
    human_readable_proof_existence,
    "Does a 'Mountain' of a short, elegant human-readable proof exist for the Four Color Theorem (Scaffold)?",
    resolution_mechanism("Verification of new proof candidates against the Appel-Haken-Robertson exhaustion."),
    impact("If Yes: The current 'Snare' of computer-verification is just a Scaffold for human limits."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Map coloring on different surfaces (e.g., Torus)
 * Viability: Requires 7 colors. 
 * Suppression: The 4-color limit is only a "Mountain" for planar graphs. 
 * On a torus, it's a "Rope" that fails.
 * * ALTERNATIVE 2: 5-Color Theorem
 * Viability: Much simpler human proof exists. 
 * Suppression: Functionally "suppressed" by the more powerful (but harder) 
 * 4-color result.
 * * CONCLUSION:
 * The existence of Alternative 1 (Torus coloring) proves that the "Mountain" 
 * of 4 colors is a feature of the Plane's topology, not an absolute 
 * universal law of all space.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [four_color_theorem_topological_bound].
 * 2. Multi-perspective: ?- multi_index_report(four_color_theorem_topological_bound).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
