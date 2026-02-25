% ============================================================================
% CONSTRAINT STORY: mars_rover_positioning_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mars_rover_positioning_system, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mars_rover_positioning_system
 *   human_readable: NavCam Frame Tie Rover Positioning System on Mars
 *   domain: space_exploration/robotics
 *
 * SUMMARY:
 *   NASA's Perseverance rover on Mars has been equipped with a new navigation
 *   system, 'NavCam Frame Tie', which functions as a planetary GPS. By
 *   correlating images taken by the rover's cameras with high-resolution
 *   orbital maps from the Mars Reconnaissance Orbiter, the system can
 *   determine the rover's absolute position on the planet to within a few
 *   centimeters. This solves a long-standing problem of cumulative
 *   positioning errors that afflicted previous missions, which only knew
 *   their location relative to their landing site (whose absolute position
 *   was uncertain by several kilometers). The constraint is the imposition of
 *   a shared, high-precision coordinate grid on Martian exploration.
 *
 * KEY AGENTS:
 *   - NASA/JPL Mission Planners: Primary beneficiary (institutional/arbitrage) — Gains operational efficiency, scientific accuracy, and enhanced mapping capabilities.
 *   - Future Mars Explorers: Secondary beneficiary (powerless/trapped) — Will rely on the established grid for navigation and safety.
 *   - Planetary Scientists: Beneficiary (analytical/mobile) — Use the precise data to create accurate geological maps of Mars.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mars_rover_positioning_system, 0.05).
domain_priors:suppression_score(mars_rover_positioning_system, 0.1).
domain_priors:theater_ratio(mars_rover_positioning_system, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mars_rover_positioning_system, extractiveness, 0.05).
narrative_ontology:constraint_metric(mars_rover_positioning_system, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(mars_rover_positioning_system, theater_ratio, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mars_rover_positioning_system, rope).
narrative_ontology:human_readable(mars_rover_positioning_system, "NavCam Frame Tie Rover Positioning System on Mars").
narrative_ontology:topic_domain(mars_rover_positioning_system, "space_exploration/robotics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mars_rover_positioning_system, nasa_jpl_mission_planners).
narrative_ontology:constraint_beneficiary(mars_rover_positioning_system, future_mars_explorers).
narrative_ontology:constraint_beneficiary(mars_rover_positioning_system, planetary_scientists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISSION PLANNER (ROPE) — For NASA/JPL, this system is a pure coordination good. It solves the critical problem of locational uncertainty, increasing navigational precision and scientific return. The cost is in development, not ongoing extraction. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.007. The negative extraction indicates a net subsidy (gain in efficiency).
constraint_indexing:constraint_classification(mars_rover_positioning_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD SCIENTIST (ROPE) — For a future human or robotic agent on Mars dependent on this grid, it functions as a vital utility. Despite being 'trapped' with no alternative, the system enables their work and survival rather than extracting from it. It's a foundational piece of infrastructure, not a coercive constraint.
constraint_indexing:constraint_classification(mars_rover_positioning_system, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — This system is a canonical example of a coordination Rope. It creates a shared, high-fidelity information standard (a coordinate system) that reduces entropy and enables more complex, coordinated actions. Its low base extractiveness and suppression scores confirm its classification as a non-extractive public good.
constraint_indexing:constraint_classification(mars_rover_positioning_system, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_rover_positioning_system_tests).
:- end_tests(mars_rover_positioning_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.05) is minimal, representing the computational and energy cost to the rover, not a social or economic extraction from a victim. Suppression (0.10) is low because the system supersedes older methods through superior performance, not coercion. Theater Ratio (0.05) is extremely low, as this is a purely functional engineering solution to a concrete problem. The constraint is a classic technological public good.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. This is a uniform-type constraint where all relevant viewpoints classify it as a Rope. From the institutional planner to the hypothetical dependent user, the system is perceived as an enabling utility. This uniformity is characteristic of pure coordination mechanisms and technological infrastructure that lack an extractive dimension.
 *
 * DIRECTIONALITY LOGIC:
 *   The system has clearly defined beneficiaries (mission planners, future explorers) and no victims. The structural relationship for all agents is that of a user benefiting from a utility. This results in a consistently low directionality value ('d') across all perspectives, ensuring the effective extraction (χ) remains low or negative, which is the signature of a Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case poses no risk of mandatrophy. The system's function as a coordination mechanism is transparent and unambiguous. There is no plausible way to frame this as a Snare (no victims), a Mountain (it's engineered, not natural), or a Piton (it's brand new and highly functional). It serves as a clear baseline example of a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mars_rover_positioning_system, 2022, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mars_rover_positioning_system, information_standard).
narrative_ontology:affects_constraint(mars_rover_positioning_system, mars_sample_return_logistics).
narrative_ontology:affects_constraint(mars_rover_positioning_system, future_manned_mars_mission_planning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
