% ============================================================================
% CONSTRAINT STORY: pythagorean_geometric_constancy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Pythagoras of Samos (c. 570–495 BC) / Euclidean Geometry
% ============================================================================

:- module(constraint_pythagorean_theorem, []).

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
 * * constraint_id: pythagorean_geometric_constancy
 * human_readable: The Pythagorean Theorem
 * domain: technological/mathematical
 * temporal_scope: c. 500 BC - Present (Civilizational)
 * spatial_scope: Global/Abstract (Euclidean Space)
 * * SUMMARY:
 * The Pythagorean Theorem states that in a right-angled triangle, the square of 
 * the hypotenuse is equal to the sum of the squares of the other two sides ($a^2 + b^2 = c^2$). 
 * It represents an immutable geometric constraint on the relationship between 
 * linear distance and orthogonal orientation in flat space.
 * * KEY AGENTS:
 * - The Right Triangle (Subject): A powerless agent whose dimensions are 
 * strictly bound by the laws of Euclidean geometry.
 * - The Architect/Engineer (Institutional): An agent who uses the theorem 
 * as a "Rope" to coordinate stable structures, navigation, and land surveys.
 * - The Non-Euclidean Navigator (Analytical): An observer for whom the 
 * theorem acts as a "Noose" or limit when attempting to map curved spaces 
 * (like the Earth's surface) using flat-space logic.
 * * NARRATIVE ARC:
 * The Pythagorean Theorem is the quintessential "Mountain"—a natural law 
 * that remains unyielding across millennia. In construction and navigation, 
 * it is a "Rope" for functional coordination, providing a "standard of 
 * achievement" for structural integrity. However, for those transitioning 
 * to global scales or general relativity, the theorem becomes a "Noose," 
 * extracting precision (extraction) and "strangling" calculations that 
 * fail to account for the "Mountain" of spacetime curvature.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for the DR-Audit Suite
narrative_ontology:interval(pythagorean_era, -500, 2026).
narrative_ontology:constraint_claim(pythagorean_geometric_constancy, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.1. Extremely low. The theorem is primarily a "gift" of 
% predictability. It only "extracts" the possibility of "impossible" triangles, 
% which is a trivial tax on creative freedom.
domain_priors:base_extractiveness(pythagorean_geometric_constancy, 0.1).

% Suppression score (0.0-1.0)
% Rationale: 0.3. It suppresses the visibility of non-Euclidean alternatives 
% in everyday human perception, rendering the idea of "curved space" 
% counter-intuitive for the average agent.
domain_priors:suppression_score(pythagorean_geometric_constancy, 0.3).

% Enforcement: Emerges naturally from the axioms of flat space.
domain_priors:emerges_naturally(pythagorean_geometric_constancy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, extractiveness, 0.1).
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(pythagorean_geometric_constancy, civil_engineers).
constraint_beneficiary(pythagorean_geometric_constancy, gps_system_designers).
constraint_victim(pythagorean_geometric_constancy, spherical_geometry_intuition).
constraint_victim(pythagorean_geometric_constancy, architectural_anarchists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CARPENTER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools to measure and the agency to build.
   WHEN: biographical - Spanning the career of a builder.
   WHERE: mobile - Can apply the 3-4-5 rule to any corner on a job site.
   SCOPE: local - Immediate physical environment of the build.
   
   WHY THIS CLASSIFICATION:
   For the carpenter, the theorem is a "Rope"—a functional coordination tool. 
   By using a 3-4-5 triangle, they coordinate a "standard of achievement" 
   (a perfect right angle), pulling the structure toward stability and 
   aesthetic alignment. It is a reliable tool for agency.
   
   NARRATIVE EVIDENCE:
   The "3-4-5 rule" has been the standard for ensuring square corners since 
   ancient Egypt and remains the primary "Rope" for modern masonry and framing.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    pythagorean_geometric_constancy,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HYPOTENUSE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The line segment has no agency to deviate.
   WHEN: immediate - The length is fixed the moment the legs are defined.
   WHERE: trapped - Bound within the Euclidean plane.
   SCOPE: local - Immediate geometric relationship.
   
   WHY THIS CLASSIFICATION:
   For the hypotenuse itself, the theorem is an absolute Mountain. If the 
   legs are 3 and 4, the hypotenuse *must* be 5. It cannot "negotiate" or 
   "exit" this relationship. The arithmetic of its length is a natural law 
   of the space it inhabits.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    pythagorean_geometric_constancy,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GEODETIC SURVEYOR - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to map whole continents but bound by Earth's shape.
   WHEN: historical - Spanning the effort to map the globe accurately.
   WHERE: constrained - The "exit" (switching to spherical trig) is required.
   SCOPE: continental - Mapping distances across the horizon.
   
   WHY THIS CLASSIFICATION:
   For the surveyor mapping large distances, the theorem acts as a "Noose." 
   If they rely on flat-earth Pythagorean math, the curvature of the Earth 
   "strangles" their accuracy. It extracts precision (extraction), forcing 
   the surveyor to abandon the simple "Rope" of $a^2+b^2=c^2$ for the 
   more complex "Mountain" of spherical trigonometry.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pythagorean_geometric_constancy,
    noose,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(continental)
    )
) :- 
    domain_priors:base_extractiveness(pythagorean_geometric_constancy, E),
    E >= 0.1,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(pythagorean_theorem_tests).

test(multi_perspective_variance) :-
    % Carpenter -> Rope
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, Type1, context(individual_moderate, biographical, mobile, local)),
    % Hypotenuse -> Mountain
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, Type2, context(individual_powerless, immediate, trapped, local)),
    Type1 \= Type2.

test(scale_based_extraction) :-
    % Demonstrates that for the surveyor (institutional), the "Noose" effect is felt.
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, noose, context(institutional, historical, constrained, continental)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(pythagorean_geometric_constancy).

:- end_tests(pythagorean_theorem_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.1): 
 * Chose 0.1 because math is generally a "gift." The only "extraction" 
 * is the loss of fidelity when the model is applied to the wrong 
 * domain (curvature).
 * 2. CLASSIFICATION: 
 * Showed how a "Rope" for a carpenter becomes a "Noose" for a 
 * surveyor, highlighting that a tool's utility is scale-dependent.
 */

% OMEGA IDENTIFICATION
omega_variable(
    euclidean_primacy_bias,
    "Is the 'Mountain' of Euclidean geometry a fundamental property of the universe (Mountain) or just a 'Scaffold' of human scale (Scaffold)?",
    resolution_mechanism("Investigation of quantum-scale geometry to see if Pythagorean laws hold at the Planck length."),
    impact("If Scaffold: The theorem is a 'Noose' for high-energy physics. If Mountain: It is a universal invariant."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Spherical Trigonometry (The Haversine Formula)
 * Viability: Essential for long-distance navigation.
 * Suppression: Often suppressed in basic education to simplify the 
 * "Mountain" of flat-space logic.
 * * ALTERNATIVE 2: Taxicab Geometry (Manhattan Distance)
 * Viability: Calculating distance on a grid ($|x_1-x_2| + |y_1-y_2|$).
 * Suppression: Useful in urban planning but suppressed as "not real math" 
 * in classical Euclidean contexts.
 * * CONCLUSION:
 * The existence of Alternative 1 (Spherical Trig) is what turns the 
 * Pythagorean "Mountain" into a "Noose" for those navigating the 
 * actual surface of the Earth.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [pythagorean_geometric_constancy].
 * 2. Multi-perspective: ?- multi_index_report(pythagorean_geometric_constancy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, noose, agent_power(individual_powerless)).
