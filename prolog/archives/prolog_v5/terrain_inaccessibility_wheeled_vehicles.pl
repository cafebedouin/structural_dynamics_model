% ============================================================================
% CONSTRAINT STORY: terrain_inaccessibility_wheeled_vehicles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The inaccessibility of unstructured terrain to wheeled vehicles is a pure
 *   constraint of physical law. Wheeled locomotion depends on continuous
 *   surface contact and tire-ground friction to transmit propulsive force.
 *   When terrain topology exceeds the vehicle's ground clearance, or when
 *   surface contact is interrupted (rocks, holes, vegetation entanglement),
 *   the vehicle cannot proceed. This is not a policy issue, a market failure,
 *   or an institutional problem — it is a consequence of Newtonian mechanics,
 *   geometry, and material properties. The constraint applies universally to
 *   all wheeled vehicles regardless of design, fuel source, or control
 *   system. It has never been violated and cannot be violated without
 *   changing the physical mechanism of wheel-based locomotion itself.
 *
 * KEY AGENTS:
 *   - Wheeled Vehicle Operators: Users constrained by the physical limit (powerless/trapped) — experience the barrier as an absolute boundary
 *   - Logistics and Transportation Planners: Institutional actors (institutional/arbitrage) — recognize the constraint as an input to route planning and modal selection
 *   - Off-Road Engineering Community: Organized specialists (organized/constrained) — work within the constraint through alternative designs and modal choices
 *   - Analytical Observer: Physicist (analytical/analytical) — derives the constraint from first principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terrain_inaccessibility_wheeled_vehicles, 0.12).
domain_priors:suppression_score(terrain_inaccessibility_wheeled_vehicles, 0.02).
domain_priors:theater_ratio(terrain_inaccessibility_wheeled_vehicles, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, extractiveness, 0.12).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, suppression_requirement, 0.02).
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

% PERSPECTIVE 1: WHEELED VEHICLE OPERATOR (MOUNTAIN) — Any agent operating a wheeled vehicle faces an invariant physical constraint: wheels require relatively smooth, continuous surface contact. Rocky slopes, dense vegetation, deep mud, and boulder fields create insurmountable barriers. No technology or policy can change the fundamental physics of wheel-terrain interaction. The constraint is experienced as a natural law of physics.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: LOGISTICS ADMINISTRATOR (MOUNTAIN) — Transportation planners universally acknowledge this as an irreducible physical fact. No amount of investment, policy, or market pressure can make a standard wheeled vehicle traverse impassable terrain. The constraint is not a bottleneck to optimize but a law to respect. All logistics strategies must route around it, not through it.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From first principles, the constraint emerges from the geometry of wheel contact patches, gravity, friction, and terrain topology. The physics is deterministic and invariant across all observable conditions. No observer position, measurement basis, or technological context can change the classification: wheels have degrees of freedom in x,y motion and z rotation; unstructured terrain denies continuous surface contact in the z dimension. This is a mathematical and physical necessity.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: OFF-ROAD ENGINEERING COMMUNITY (MOUNTAIN) — Even specialized communities (all-terrain vehicle designers, military logistics engineers, planetary rovers) treat this constraint as a fixed boundary condition. Their entire field is organized around working within this limit: using low-pressure tires, increasing ground clearance, reducing vehicle mass, or selecting terrain-appropriate locomotion modes (legs, tracks, crawlers). They do not try to eliminate the constraint — they engineer around it.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

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
 *   Extractiveness (0.12): Very low. The constraint does not extract value from anyone — it is purely restrictive. Some might argue that it creates demand for alternative transportation modes (helicopters, pack animals, tracked vehicles) and thus benefits those industries, but this is indirect economic effect, not extraction by the constraint itself. The constraint imposes cost uniformly on all wheeled-vehicle users equally; it does not funnel benefits to a specific actor. Suppression (0.02): Minimal. There are no alternatives to suppress — the constraint is not competing with other transportation modes, it is a physical law that determines which modes are feasible in which terrain. Theater ratio (0.15): Very low. There is no performative element. The constraint is directly observable — try to drive a car across a boulder field and it fails obviously. No theatrical maintenance is required.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify identically as Mountain. This is the expected outcome for a pure constraint of physical law. The wheeled vehicle operator, the logistics planner, the engineer, and the analytical observer all agree: this is an immutable limit. The lack of perspectival gap is not a weakness of the framework but evidence of successful identification of a natural law. The constraint is invariant across all structural positions, time horizons, and measurement contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain-class constraints do not require beneficiary/victim declarations because they are not extraction mechanisms — they do not transfer value from one agent to another. All agents experience the same constraint equally. Directionality (d) is undefined for mountains because there is no 'direction' of extraction. The constraint simply does not permit certain actions, and all agents are equally excluded from those actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terrain_inaccessibility_wheeled_vehicles, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terrain_inaccessibility_wheeled_vehicles, global_infrastructure).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, rural_logistics_access).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, emergency_response_reach).
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, off_road_mineral_extraction).

% DUAL FORMULATION NOTE:
% This constraint is fundamental and upstream to multiple domain-specific accessibility constraints. Rural logistics access, emergency response reach, and off-road resource extraction are all constrained by this physical law. The network links show dependency relationships, not decomposition — terrain inaccessibility is a single mountain that downstream constraints must accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
