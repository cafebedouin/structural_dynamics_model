% ============================================================================
% CONSTRAINT STORY: relativity_physical_invariance
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Einstein, A. (1920). Relativity: The Special and General Theory.
% ============================================================================

:- module(constraint_relativity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: relativity_physical_invariance
 * human_readable: General Relativity (The Spacetime Constraint)
 * domain: technological/scientific
 * temporal_scope: 1905-Present
 * spatial_scope: Universal
 * * SUMMARY:
 * Relativity describes the fundamental constraints of the universe: the 
 * constancy of the speed of light (Special Relativity) and the curvature 
 * of spacetime by matter/energy (General Relativity). It removes the 
 * "Noose" of absolute space and time, replacing it with a "Rope" of 
 * mathematical transformation.
 * * KEY AGENTS:
 * - The Observer: An agent in a specific reference body (e.g., a moving train).
 * - The Light Signal: The universal constant that limits information transfer.
 * - The Mass/Energy: The "institutional" force that dictates the geometry 
 * of the landscape.
 * * NARRATIVE ARC:
 * For centuries, humanity was trapped in the "Noose" of Newtonian Absolute 
 * Time. Einstein provided the "Rope" of the Lorentz Transformation and 
 * General Covariance, allowing us to coordinate observations across 
 * different frames. Ultimately, the curvature of spacetime remains the 
 * ultimate "Mountain" through which all matter must flow.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(relativity_physical_invariance, 0, 10).
narrative_ontology:constraint_claim(relativity_physical_invariance, mountain).

% Base extractiveness: Low (0.0). 
% Physics does not "extract" value in a social sense, but it limits 
% the "efficiency" of travel/communication.
domain_priors:base_extractiveness(relativity_physical_invariance, 0.0).

% Suppression: Low (0.1). 
% The laws are transparent and universal, though their effects are 
% "suppressed" from common sense by our low-velocity evolution.
domain_priors:suppression_score(relativity_physical_invariance, 0.1).

% Enforcement: Emerges naturally from the structure of the universe.
domain_priors:emerges_naturally(relativity_physical_invariance).

% Beneficiaries: GPS Satellites, Cosmologists.
constraint_beneficiary(relativity_physical_invariance, high_precision_technology).

% Victims: The Newtonian "Common Sense" Intuition.
constraint_victim(relativity_physical_invariance, intuitive_classical_physics).

% Metrics
narrative_ontology:constraint_metric(relativity_physical_invariance, extractiveness, 0.0).
narrative_ontology:constraint_metric(relativity_physical_invariance, suppression_requirement, 0.1).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TRAVELLING OBSERVER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (subject to $c$)
   WHEN: immediate (relativistic speeds)
   WHERE: trapped (within the light cone)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For an agent attempting to exceed the speed of light, Relativity is a 
   "Noose." No amount of energy can pull the agent out of their local 
   light cone. Causality strangles the possibility of FTL communication.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_physical_invariance,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COORDINATING SCIENTIST - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile (can transform between frames)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To the scientist, the Principle of Relativity is a "Rope." It provides 
   the coordination equations (Lorentz transformations) that allow 
   agreement between two observers moving at different velocities. It 
   turns chaos into a synchronized "Match."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_physical_invariance,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COSMOS - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical (civilizational)
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   General Relativity describes the "Mountain" of spacetime curvature. 
   Matter tells space how to curve; space tells matter how to move. This 
   is the immutable geometry of existence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_physical_invariance,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(relativity_tests).

test(equivalence_principle_check) :-
    % Testing that gravity and acceleration are indistinguishable (Equivalence).
    % In this system, that means they share the same classification.
    constraint_indexing:constraint_classification(relativity_physical_invariance, mountain, _).

test(simultaneity_gap) :-
    % Testing that different observers (Powerless/Trapped) cannot agree on 
    % a single 'Noose' of time.
    true.

:- end_tests(relativity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * I classified the speed of light as the primary "Noose" for FTL travel, 
 * while treating the Principle of Relativity itself as a "Rope" for 
 * intellectual coordination. The "Mountain" is the field equations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    quantum_gravity_unification,
    "Will a 'Theory of Everything' reveal Relativity as a mere Rope, or is it a permanent Mountain?",
    resolution_mechanism("Experimental detection of gravitons or discovery of String Theory/Loop Quantum Gravity proof"),
    impact("If Unified: Relativity becomes a 'Local Rope' within a larger Quantum Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
