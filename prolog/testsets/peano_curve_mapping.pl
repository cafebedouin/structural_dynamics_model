% ============================================================================
% CONSTRAINT STORY: peano_curve_mapping
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Giuseppe Peano (1890) / Space-Filling Curves
% ============================================================================

:- module(constraint_peano_curve, []).

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
 * * constraint_id: peano_curve_mapping
 * human_readable: Peano Space-Filling Curve
 * domain: mathematical/technological
 * temporal_scope: 1890 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Unit Interval to Unit Square)
 * * SUMMARY:
 * The Peano Curve is the first discovered space-filling curve, a continuous 
 * mapping from a one-dimensional interval onto a two-dimensional square. It 
 * represents a fundamental constraint on the relationship between dimension 
 * and cardinality, proving that a 1D line can "occupy" all points in a 2D space.
 * * KEY AGENTS:
 * - The Mapping Point (Subject): A powerless agent whose position in 2D space 
 * is strictly determined by its 1D "time" parameter.
 * - The Database Architect (Institutional): An agent using space-filling 
 * logic (e.g., Z-order or Hilbert curves) as a "Rope" for data locality.
 * - Geometric Intuition (Victim): The observer whose common-sense 
 * distinction between 1D and 2D is "strangled" by the mapping.
 * * NARRATIVE ARC:
 * Originally a "pathological" counter-example to intuition, the Peano Curve 
 * is a "Mountain" of topological reality. In modern computing, it is a 
 * "Rope" for functional coordination in spatial indexing. However, for 
 * the 1D parameter, the mapping is a "Noose"—it extracts its 1D simplicity 
 * and forces it to assume the burden of a 2D existence, creating 
 * locality-breaking "jumps" in higher dimensions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(peano_era, 1890, 2026).
narrative_ontology:constraint_claim(peano_curve_mapping, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.25. The curve "extracts" 1D locality (points that are close 
% in 1D may be far in 2D) to satisfy the space-filling requirement.
domain_priors:base_extractiveness(peano_curve_mapping, 0.25).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses the intuitive topological distinction 
% between dimensions, rendering the "1D is not 2D" argument invalid.
domain_priors:suppression_score(peano_curve_mapping, 0.2).

% Enforcement: Emerges naturally from the axioms of continuity and set theory.
domain_priors:emerges_naturally(peano_curve_mapping).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(peano_curve_mapping, extractiveness, 0.25).
narrative_ontology:constraint_metric(peano_curve_mapping, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(peano_curve_mapping, spatial_database_indexing). % Efficient 2D range queries.
constraint_beneficiary(peano_curve_mapping, image_compression_algorithms). 
constraint_victim(peano_curve_mapping, classical_dimension_intuition). % Strangled by the surjective mapping.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MAPPING POINT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - The point on the line has no agency over the map.
   WHEN: immediate - True at every infinitesimal step of the parameter t.
   WHERE: trapped - Bound within the surjective mapping f: [0,1] -> [0,1]^2.
   SCOPE: local - Immediate coordinate mapping.
   
   WHY THIS CLASSIFICATION:
   For the point moving along the curve, the mapping is an unyielding law. 
   If it is at time 't', it *must* be at coordinate (x,y). There are zero 
   degrees of freedom to avoid the 2D "destiny" dictated by the fractal limit.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    peano_curve_mapping,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GIS ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to choose the mapping (Peano, Hilbert, or Morton).
   WHEN: biographical - Planning the architecture of a geographic system.
   WHERE: mobile - Can adjust the depth of recursion or the type of curve.
   SCOPE: global - Universal application in spatial data.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the space-filling curve is a "Rope"—a tool for functional 
   coordination. It allows them to "pull" 2D spatial data into a 1D 
   linear order (the database index) without losing too much locality, 
   facilitating efficient range searches.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    peano_curve_mapping,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LOCALITY PRESERVATION - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - The sequence of points bound by the 1D line.
   WHEN: immediate - Encountering a "jump" where 2D neighbors are 1D strangers.
   WHERE: constrained - The topological limit of f is continuous but not 
        homeomorphic (no 1D-2D isomorphism is a homeomorphism).
   SCOPE: local - A specific discontinuous "jump" in the mapping.
   
   WHY THIS CLASSIFICATION:
   The Peano mapping acts as a "Noose" for locality. Because a 1D line 
   cannot be continuously mapped to a 2D square in a way that preserves 
   all neighborhood relations (Netto's Theorem), the curve "strangles" 
   locality. It extracts the 1D proximity of points, forcing distant 1D 
   neighbors to be 2D neighbors and vice versa.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peano_curve_mapping,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(peano_curve_mapping, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS (Insights into Space-Filling Logic)
   ========================================================================== */

:- begin_tests(peano_curve_tests).

test(perspective_variance) :-
    % Point -> Mountain
    constraint_indexing:constraint_classification(peano_curve_mapping, Type1, context(individual_powerless, immediate, trapped, local)),
    % Engineer -> Rope
    constraint_indexing:constraint_classification(peano_curve_mapping, Type2, context(institutional, biographical, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(locality_noose_penalty) :-
    % A powerless data sequence (constrained) sees the loss of locality as a Noose.
    constraint_indexing:constraint_classification(peano_curve_mapping, noose, context(individual_powerless, immediate, constrained, local)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(peano_curve_mapping).

:- end_tests(peano_curve_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I classified the Peano mapping as a Mountain for the 
 * individual point because topologicalSurjectivity is non-negotiable. 
 * 2. TYPE SHIFT: I highlighted how the "Rope" of spatial indexing depends on 
 * the "Noose" of locality loss. You cannot fill space (Rope) without 
 * breaking the 1D-2D neighborhood equivalence (Noose).
 * 3. EXTRACTIVENESS (0.25): Assigned due to the "Tax" on locality. Any 
 * space-filling curve must "extract" some level of linear order to 
 * satisfy the higher-dimensional density.
 */

% OMEGA IDENTIFICATION
omega_variable(
    locality_preservation_limit,
    "Is there a 'Rope' (a different curve) that preserves locality better than Peano?",
    resolution_mechanism("Comparison of Peano vs Hilbert vs Morton curves using the 'Clustering Factor' metric."),
    impact("If Hilbert is superior: Peano is a 'Noose' compared to the Hilbert 'Rope'."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hilbert Curve
 * Viability: Better locality preservation than Peano's original construction.
 * Suppression: Often preferred in engineering, making Peano's original 
 * construction a "Noose" by comparison for practical use.
 * * ALTERNATIVE 2: Morton Order (Z-order)
 * Viability: Simpler to compute (bit-interleaving).
 * Status: A "Rope" used in many low-level CPU operations.
 * * CONCLUSION:
 * The existence of the Hilbert Curve (Alternative 1) transforms the 
 * original Peano Curve into a "Noose" for performance-seeking engineers, 
 * as it extracts more locality for the same space-filling result.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [peano_curve_mapping].
% Analyze: ?- constraint_indexing:multi_index_report(peano_curve_mapping).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
