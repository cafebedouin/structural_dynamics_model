% ============================================================================
% CONSTRAINT STORY: terrain_inaccessibility_wheeled_vehicles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terrain_inaccessibility_wheeled_vehicles, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: terrain_inaccessibility_wheeled_vehicles
 *   human_readable: Inaccessibility of Terrain to Wheeled Vehicles
 *   domain: technological/physical
 *
 * SUMMARY:
 *   The inaccessibility of unstructured terrain to wheeled vehicles is a
 *   foundational constraint in transportation physics and engineering. A
 *   wheeled vehicle's ability to traverse terrain is determined by immutable
 *   geometric and mechanical principles: wheel diameter, suspension travel,
 *   axle design, weight distribution, and tire properties interact with
 *   terrain slope, obstacle size, surface deformability, and friction
 *   coefficient to produce deterministic success or failure outcomes. This
 *   constraint exhibits uniform mountain classification across all
 *   observational perspectives because it is rooted in physical law, not
 *   institutional arrangement or policy. Unlike constraints with
 *   beneficiaries and victims (where different agents experience different
 *   effective extraction based on their structural position), the terrain
 *   inaccessibility constraint is invariant: all agents — wheeled vehicles,
 *   engineers, planners, travelers — face the same immutable limit. The
 *   constraint cannot be suppressed by resources, cannot be negotiated around
 *   for wheeled vehicles specifically, and has no hidden theater or
 *   performative content. It is what it claims to be: a natural law boundary.
 *
 * KEY AGENTS:
 *   - Wheeled Vehicles: Physical objects subject to constraint (no power) — cannot traverse steep slopes, large obstacles, or soft/deformable terrain regardless of design optimization
 *   - Transportation Engineers: Design specialists (powerful/analytical) — understand limits and work within them; cannot defeat constraint but can optimize vehicle parameters to extend accessible envelope
 *   - Terrain: Physical environment (no agency) — obstacle distribution, slope, bearing capacity, and friction are given conditions, not subject to negotiation
 *   - Mission Planners / Explorers: Decision-makers (moderate to powerful) — face constraint as a real operational boundary; can choose alternative vehicle types but cannot make wheels traverse impossible terrain
 *   - Physics / Mechanics: Underlying natural law (no agent) — fundamental principle constraining all designs equally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terrain_inaccessibility_wheeled_vehicles, 0.12).
domain_priors:suppression_score(terrain_inaccessibility_wheeled_vehicles, 0.03).
domain_priors:theater_ratio(terrain_inaccessibility_wheeled_vehicles, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, extractiveness, 0.12).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terrain_inaccessibility_wheeled_vehicles, mountain).
narrative_ontology:human_readable(terrain_inaccessibility_wheeled_vehicles, "Inaccessibility of Terrain to Wheeled Vehicles").
narrative_ontology:topic_domain(terrain_inaccessibility_wheeled_vehicles, "technological/physical").

domain_priors:emerges_naturally(terrain_inaccessibility_wheeled_vehicles).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WHEELED VEHICLE (MOUNTAIN) — A wheeled vehicle with fixed axle and rigid wheel geometry cannot surmount obstacles larger than wheel diameter or steeper than maximum gradeability angle. This is a mathematical and physical limit, not a policy choice. ε≈0.10, suppression≈0.02, accessibility_collapse=0.92 (contact dynamics and geometry fully determine failure modes), resistance=0.08 (wheel diameter and spring travel define narrow operational envelope). No beneficiary/victim — this is immutable law of physics.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From first principles of tribology, contact mechanics, and dynamics: wheel-terrain interaction requires normal force perpendicular to ground and insufficient tangential force to overcome rolling resistance + gravitational load component on slope. Physics and geometry determine outcome. No degrees of freedom for policy or institutional design. ε≈0.08, suppression≈0.01, accessibility_collapse=0.95, resistance=0.05. This perspective confirms the natural law view.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: TRANSPORTATION ENGINEER (MOUNTAIN) — Even with technological optimization (low-profile tires, hydropneumatic suspension, all-terrain geometry, weight distribution), wheeled vehicles face hard limits: gradeability ceiling ~45-50°, obstacle clearance = wheel radius + suspension compression, sinkage depth function of ground bearing capacity. These limits are not suppressible by policy or funding — they are laws of mechanics. ε≈0.15, suppression≈0.04, accessibility_collapse=0.88, resistance=0.10. The constraint persists despite engineering effort because it is rooted in fundamental geometry and dynamics.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: POWERFUL AGENT WITH RESOURCES (MOUNTAIN) — Even unlimited resources cannot overcome the constraint through wheeled vehicles alone. Alternative solutions (tracked vehicles, helicopters, legged robots, foot traffic) exist, but wheeled vehicles as a class have immutable access limits. The powerful agent can choose alternative transport; they cannot make wheels work on bedrock or boulder fields. This perspective demonstrates that the constraint is truly immutable — resources and power do not defeat it. ε≈0.10, suppression≈0.02, accessibility_collapse=0.94, resistance=0.07.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terrain_inaccessibility_wheeled_vehicles_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, ExtMetricName, E),
    domain_priors:suppression_score(terrain_inaccessibility_wheeled_vehicles, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(terrain_inaccessibility_wheeled_vehicles),
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(terrain_inaccessibility_wheeled_vehicles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract value from any agent; it is a neutral fact about physical interaction. The 0.12 (rather than 0.0) reflects minor measurement uncertainty and the fact that engineering effort can approach (but not exceed) physical limits through optimization. Suppression (0.03): Minimal. There is no coercive mechanism or lack of alternatives—alternative vehicle types exist (tracked, legged, helicopters, foot traffic). Suppression is low because the constraint is not imposed by an actor with gatekeeping power; it emerges from physics. Theater (0.15): Minimal. The constraint is functional, not performative. Vehicle designers do not engage in ritualistic behavior to honor the limit; they simply hit it and adapt design or switch vehicle type. The small theater value reflects minor overhead in testing and validation to confirm physical limits, not any substantial performative content. Accessibility collapse (0.92): High. The constraint fully determines which terrain is accessible. Once you specify vehicle geometry, suspension, and tire properties, the set of traversable terrain is nearly fully determined by contact mechanics and dynamics. Resistance (0.08): Low. The constraint cannot be resisted or overcome through force, negotiation, or alternative institutional arrangements. Alternative vehicle types bypass it (they are not wheeled), but wheeled vehicles cannot exceed it.
 *
 * PERSPECTIVAL GAP:
 *   There is NO perspectival gap. All four perspectives (vehicle, analyst, engineer, powerful agent) classify the constraint identically as Mountain. This uniformity is not a weakness but the signature of a true natural law constraint. The lack of beneficiary/victim structure, the absence of suppression mechanisms, and the invariance across all observational positions confirm that this is an immutable physical limit, not a contingent institutional arrangement. Even a powerful agent with unlimited resources cannot make wheeled vehicles traverse bedrock or boulder fields—they must switch to alternative vehicle types. This is the core definition of a mountain constraint: zero degrees of freedom for all indices.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. Mountain constraints have no beneficiaries or victims; they have no directionality computation. All agents face the same immutable limit. The constraint's existence is not contingent on any agent's structural position or power. This is true invariance across the index space.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wheel_geometry_optimality,
    'Is there a theoretical optimal wheel geometry that could exceed current gradeability and obstacle limits while remaining a wheeled vehicle?',
    'Formal analysis of contact mechanics and dynamics; exploration of non-rigid wheel designs, segmented wheels, or morphing tire concepts; empirical testing of prototype high-performance designs',
    'If no geometry exceeds ~50° grade and wheel-radius obstacle clearance: confirms mountain classification. If optimization pathway extends limits to 65°+ and 2x wheel radius: constraint remains mountain but accessible envelope expands (ε unchanged, but effective penetration of terrain increases). Constraint identity unchanged because the limits are set by physics, not by current engineering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wheel_geometry_optimality, empirical, 'Whether optimized wheel geometry can exceed physical gradeability and obstacle limits').

omega_variable(
    terrain_complexity_definition,
    'What constitutes ''unstructured, rugged terrain'' for classification purposes? Where does ''rough surface'' transition to ''inaccessible terrain''?',
    'Terrain taxonomy by obstacle frequency, size distribution, slope, and deformability; empirical testing of wheeled vehicle performance across terrain classes; statistical mapping of vehicle failure rates to terrain parameters',
    'If boundary is sharp (discrete transition): constraint has well-defined operational envelope. If boundary is gradual (continuous degradation): constraint exhibits smooth failure, not hard limit. Classification remains mountain either way, but the accessibility_collapse metric might shift by ±0.05 depending on continuity of failure modes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terrain_complexity_definition, conceptual, 'Where the boundary lies between traversable and inaccessible terrain for wheeled vehicles').

omega_variable(
    alternative_vehicle_classes,
    'Should alternative vehicle classes (tracked, legged, flying) be counted as ''wheeled vehicle solutions'' or as constraint escapes that define the boundary of the constraint''s domain?',
    'Definitional analysis: is the constraint about ''wheeled vehicles'' (narrow) or ''ground vehicles'' (broad)? Review of engineering taxonomy; assessment of whether alternative designs represent optimization within constraint space or exit from constraint space',
    'If narrow definition (wheeled only): constraint is mountain over wheeled vehicle domain. If broad definition (all ground transport): constraint dissolves into ''terrain types and vehicle-terrain fit'' (many constraints, not one). The narrow reading is correct for this story because ''wheeled vehicle'' is the structural commitment that creates the limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_vehicle_classes, conceptual, 'Whether constraint applies to wheeled vehicles specifically or all ground vehicles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terrain_inaccessibility_wheeled_vehicles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terrain_tr_t0, terrain_inaccessibility_wheeled_vehicles, theater_ratio, 0, 0.1).
narrative_ontology:measurement(terrain_tr_t5, terrain_inaccessibility_wheeled_vehicles, theater_ratio, 5, 0.12).
narrative_ontology:measurement(terrain_tr_t10, terrain_inaccessibility_wheeled_vehicles, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(terrain_be_t0, terrain_inaccessibility_wheeled_vehicles, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(terrain_be_t5, terrain_inaccessibility_wheeled_vehicles, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(terrain_be_t10, terrain_inaccessibility_wheeled_vehicles, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terrain_inaccessibility_wheeled_vehicles, global_infrastructure).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, terrain_accessibility_tracked_vehicles).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, terrain_accessibility_legged_vehicles).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, modal_choice_ground_transport).

% DUAL FORMULATION NOTE:
% This constraint defines the boundary of wheeled vehicle capability. Parallel stories for tracked vehicles and legged vehicles have different ε values and different accessible terrain envelopes, but all three are rooted in the same underlying physical principles. The network links show how the choice of vehicle modal class intersects with terrain accessibility—each modality has its own constraint story reflecting different geometric and dynamic limits. This is not decomposition due to measurement ambiguity, but rather a constraint family where each member addresses a distinct vehicle class operating over the same terrain complexity space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
