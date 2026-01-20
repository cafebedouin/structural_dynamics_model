% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Materials Science / Solid Mechanics / ASTM E8
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
 * * constraint_id: material_tensile_strength
 * human_readable: Ultimate Tensile Strength (UTS)
 * domain: technological
 * temporal_scope: Permanent (Universal Physical Properties)
 * spatial_scope: Global (Physical Matter)
 * * SUMMARY:
 * Tensile strength is the maximum stress that a material can withstand while 
 * being stretched or pulled before breaking. It represents the fundamental 
 * cohesive limit of atomic bonding within a solid, dictating the maximum load 
 * a structure can carry per unit of cross-sectional area.
 * * KEY AGENTS:
 * - The Material Scientist: Analytical observer measuring the stress-strain 
 * curve to find the point of necking and fracture.
 * - The Civil Engineer: Institutional agent using safety factors and 
 * standardized tensile values to coordinate the safety of public infrastructure.
 * - The Overloaded Component: Individual agent (part) subject to the load, 
 * where the tensile limit acts as a "Noose" that determines the exact 
 * moment of catastrophic failure.
 * * NARRATIVE ARC:
 * Tensile strength functions as a "Mountain" of physical reality—the 
 * intrinsic limit of molecular lattices. To the builder, it is a "Rope" for 
 * coordination (allowing for predictable weight-bearing designs). However, 
 * for a wire or beam pushed beyond its limit, it becomes a "Noose," 
 * as the cross-section narrows ("necking") and the material's internal 
 * resistance is extracted until it snaps.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for ID extraction
narrative_ontology:interval(tensile_strength_interval, 0, 10).
narrative_ontology:constraint_claim(material_tensile_strength, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). It extracts "utility" from the material. As a 
% load increases, the material's safety margin is consumed. In failure, 
% it extracts the structural integrity of the entire system.
domain_priors:base_extractiveness(material_tensile_strength, 0.5).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.4). The "Ultimate" limit suppresses the possibility 
% of further load-bearing. This limit is visible but physically unyielding, 
% suppressing alternative "infinite strength" fantasies.
domain_priors:suppression_score(material_tensile_strength, 0.4).

% Enforcement requirements
% Emerges naturally from atomic bonding forces (Electromagnetism).
domain_priors:emerges_naturally(material_tensile_strength).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.5).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(material_tensile_strength, [steel_manufacturers, safety_inspectors]).
constraint_victim(material_tensile_strength, [aging_infrastructure, overloaded_machinery]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE METALLURGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal physical laws and microstructures.
   WHEN: civilizational - Viewing the properties of iron or carbon as constants.
   WHERE: trapped - Atomic bond energy cannot be cheated by design.
   SCOPE: global - Universal physics.
   
   WHY THIS CLASSIFICATION:
   To the scientist, tensile strength is a Mountain. It is an unchangeable 
   feature of the material's atomic "hardware." No amount of social policy 
   can make a specific grade of steel stronger than its molecular structure 
   allows. It is a fixed peak in the landscape of engineering.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    material_tensile_strength,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STRUCTURAL ENGINEER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design buildings and select materials.
   WHEN: biographical - Ensuring a bridge lasts for a 50-year career.
   WHERE: arbitrage - Can switch between materials (Concrete, Steel, Carbon Fiber).
   SCOPE: regional - Infrastructure for a specific city or state.
   
   WHY THIS CLASSIFICATION:
   For the engineer, tensile strength is a Rope. It is a coordination 
   mechanism that allows for safety. By knowing the UTS, they can "tether" 
   their designs to reality, using safety factors to pull a functioning, 
   reliable bridge into existence. It is a functional tool for progress.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    material_tensile_strength,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(material_tensile_strength, E),
    E > 0.3, % Managed extraction of material limits
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUSPENSION CABLE - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - A single component subject to extreme tension.
   WHEN: immediate - The seconds before a failure occurs.
   WHERE: trapped - Cannot exit the load path.
   SCOPE: local - The immediate cross-section of the material.
   
   WHY THIS CLASSIFICATION:
   For a cable carrying a load beyond its capacity, the tensile limit is 
   a Noose. As the stress reaches the "Ultimate" point, the material 
   begins to "neck" (narrow), concentrating the force. The limit 
   strangles the component's ability to hold together, eventually 
   extracting the structural integrity until it snaps.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    material_tensile_strength,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(material_tensile_strength, E),
    E > 0.4, % High extraction of structural "life"
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(material_tensile_strength, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(material_tensile_strength, noose, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_load) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(material_tensile_strength, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(material_tensile_strength, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): Material limits extract safety. The more you 
 * use the strength, the closer you get to the "ruin" of the component.
 * 2. PERSPECTIVES: Chose the Scientist (Law), the Builder (Tool), and the 
 * Part (Victim) to show the indexical shift.
 * 3. NOOSE LOGIC: Necking in a tensile test is a literal physical 
 * representation of a "tightening trap" that precedes failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    microscopic_flaw_distribution,
    "To what extent does 'Theoretical Strength' (Mountain) differ from 
    'Actual Strength' due to invisible internal defects (Noose)?",
    resolution_mechanism("Atomic-scale scanning of every unit of material 
    production"),
    impact("If Mountain: Predictable. If Noose: Hidden traps in all materials."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Composite Reinforcement (Carbon Fiber)
 * Viability: High. Shifts the "Mountain" to a higher peak.
 * Suppression: Low. Actively encouraged in aerospace.
 * * CONCLUSION:
 * While we can switch materials (Rope), we cannot escape the concept of a 
 * "limit" itself. Thus, Tensile Strength is a Mountain of the physical 
 * universe, even if specific values are a Rope for the engineer.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_material_tensile_strength].
 * 2. Multi-perspective: ?- multi_index_report(material_tensile_strength).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
