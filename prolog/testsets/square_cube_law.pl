% ============================================================================
% CONSTRAINT STORY: square_cube_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Galileo Galilei (1638) / Biomechanics / Scaling Laws
% ============================================================================

:- module(constraint_square_cube_law, []).

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
 * * constraint_id: square_cube_law
 * human_readable: The Square-Cube Law
 * domain: technological/biological
 * temporal_scope: Permanent (Physical Law of Geometry)
 * spatial_scope: Global (Universal)
 * * SUMMARY:
 * The Square-Cube Law states that as an object grows in size, its surface area 
 * increases by the square of the multiplier, while its volume (and mass) 
 * increases by the cube. This represents a fundamental scaling limit where 
 * structural strength or heat dissipation (surface-dependent) fails to keep 
 * pace with weight or heat generation (volume-dependent).
 * * KEY AGENTS:
 * - The Evolutionary Biologist: Analytical observer mapping the "Mountain" 
 * of physical limits on animal size.
 * - The Aircraft Designer: Institutional agent using scaling ratios as a 
 * "Rope" to coordinate the transition from prototypes to full-scale jets.
 * - The Mega-Fauna / Giant: Individual powerless subject whose physical 
 * integrity is "strangled" by the law, making gargantuan size a lethal trap.
 * * NARRATIVE ARC:
 * The law functions as a "Mountain" of geometric reality—the ultimate 
 * physical boundary of the material world. For the engineer, it is a 
 * "Rope" for coordination (predicting when a bridge or wing will fail). 
 * However, for an organism or machine pushed beyond its scaling threshold, 
 * it becomes a "Noose," as its own mass extracts the strength of its 
 * supports until collapse occurs.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(square_cube_interval, 0, 10).
narrative_ontology:constraint_claim(square_cube_law, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). The law extracts "viability" from large-scale 
% entities. It imposes a "mass tax" where doubling size requires more than 
% doubling the support material, extracting efficiency as scale increases.
domain_priors:base_extractiveness(square_cube_law, 0.5).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). It suppresses the "Linear Scaling" fantasy. 
% While "Giant" narratives are visible in fiction, the physical reality 
% of the universe actively suppresses their existence through gravity and 
% thermodynamics.
domain_priors:suppression_score(square_cube_law, 0.7).

% Enforcement requirements
% Emerges naturally from the Euclidean geometry of three-dimensional space.
domain_priors:emerges_naturally(square_cube_law).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(square_cube_law, extractiveness, 0.5).
narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(square_cube_law, [material_science_consultants, small_organisms]).
constraint_victim(square_cube_law, [giant_engineering_projects, mega_fauna]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE BIOMECHANIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal physical laws.
   WHEN: civilizational - Viewing evolution as a permanent substrate.
   WHERE: trapped - Biology cannot bypass the math of surface-to-volume ratios.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the law is a Mountain. It is an unchangeable feature 
   of the universe's geometric "hardware." No amount of biological 
   adaptation can make an ant the size of an elephant without its legs 
   shattering; it is a fixed peak in the topography of reality.
   
   NARRATIVE EVIDENCE:
   Galileo's "Two New Sciences" demonstrates that the bones of a giant 
   would have to be disproportionately thick, eventually filling the 
   entire body, proving a hard upper limit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    square_cube_law,
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
   PERSPECTIVE 2: THE AEROSPACE ENGINEER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design and scale complex machines.
   WHEN: biographical - Managing the lifecycle of a jet design.
   WHERE: arbitrage - Can shift materials (e.g., Carbon Fiber) to alter ratios.
   SCOPE: national - Large-scale industrial production.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the law is a Rope. It is a coordination mechanism. 
   By understanding scaling coefficients, they can "tether" a small-scale 
   wind-tunnel model to a full-sized airliner, using the constraint to 
   pull a safe, functional design into reality.
   
   NARRATIVE EVIDENCE:
   The use of Reynolds numbers and Froude numbers in fluid dynamics 
   allows engineers to arbitrage the law for predictable scaling.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    square_cube_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(square_cube_law, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SPECULATIVE GIANT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the physics of their own mass.
   WHEN: immediate - Today's struggle to move or breathe.
   WHERE: constrained - Cannot leave the gravity well or change geometry.
   SCOPE: local - Immediate internal physiology.
   
   WHY THIS CLASSIFICATION:
   For a hypothetical giant, the law is a Noose. Their own volume-to-surface 
   ratio extracts their ability to cool down or support their own mass. 
   The geometry of their existence is a trap; the larger they grow, the 
   tighter the "gravity tax" on their bones strangles their viability.
   
   NARRATIVE EVIDENCE:
   J.B.S. Haldane's "On Being the Right Size" notes that a giant would 
   break its thighs with every step, the weight becoming a lethal trap.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    square_cube_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(square_cube_law, E),
    E > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(square_cube_law_tests).

test(multi_perspective_scaling) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(analytical, civilizational, trapped, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(square_cube_law, rope, context(institutional, biographical, arbitrage, national)),
    % Powerless sees Noose
    constraint_indexing:constraint_classification(square_cube_law, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_geometry) :-
    % Powerless subjects (the giant) feel the total extraction of their health (Noose).
    % Institutional actors (engineers) use the limit for coordination (Rope).
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(square_cube_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(square_cube_law, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_physics) :-
    % Civilizational scale = Mountain
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(square_cube_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.5):
 * Reasoning: The law extracts structural and metabolic "surplus." 
 * As things get bigger, they must pay more for their existence. 
 * Evidence: Found in engineering "scaling penalties" and biological mass limits.
 * * 2. SUPPRESSION SCORE (0.7):
 * Reasoning: It suppresses "linear growth" as a viable alternative. 
 * Evidence: The absence of gigantic insects or building-sized land animals 
 * in the fossil record (outside specific aquatic environments).
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Biologist (Law), Engineer (Utility), and Giant (Victim) 
 * to show the indexical shift from "Math" to "Policy" to "Trap."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_strength_evolution,
    "Can nanotechnology 'untie' the scaling Noose by increasing material 
    strength-to-weight ratios beyond current biological/metallic limits (Rope)?",
    resolution_mechanism("Development of macroscopic carbon-nanotube structures"),
    impact("If Rope: Giants can be built. If Mountain: Geometry still 
    imposes heat-dissipation limits that act as a new Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Fractal Internal Structures
 * Viability: High. Many organisms increase internal surface area 
 * (lungs, intestines) to bypass the "volume extraction" of nutrients.
 * Suppression: None. This is the biological "Rope" used to survive.
 * * ALTERNATIVE 2: Buoyancy (Aquatic Environment)
 * Viability: High. Allows whales to reach sizes impossible on land.
 * Suppression: Low. Gravity is the "Noose" that is bypassed here.
 * * CONCLUSION:
 * Moving to a different environment (Water) or changing topology (Fractals) 
 * turns the "Noose" back into a "Rope," allowing the agent to exit the 
 * immediate trap of the land-based scaling mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * ?- [constraint_square_cube_law].
 * ?- multi_index_report(square_cube_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
