% ============================================================================
% CONSTRAINT STORY: terrain_inaccessibility_wheeled_vehicles
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_terrain_inaccessibility_wheeled_vehicles, []).

:- use_module(library(plunit)).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- use_module(config).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   This constraint describes the fundamental physical limitation that
 *   conventional wheeled vehicles cannot traverse unstructured, rugged, or
 *   debris-filled terrain. This reality severely limits access for emergency
 *   services in disaster zones (e.g., after earthquakes, tsunamis), logistics
 *   in undeveloped areas, and general civilian transport, necessitating
 *   alternative, often more expensive or slower, solutions. The development
 *   of "walking cars" like the Hyundai Elevate is a direct response to this
 *   constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Disaster Victims: Primary target of the *consequences* of the constraint (powerless/trapped).
 *   - First Responders: Limited by the constraint in their ability to perform their duties (organized/mobile).
 *   - Vehicle & Robotics Developers (e.g., Hyundai): Actors seeking to overcome the constraint (institutional/arbitrage).
 *   - Analytical Observer: Urban planners, disaster response strategists, physicists.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terrain_inaccessibility_wheeled_vehicles, 0.05).
domain_priors:suppression_score(terrain_inaccessibility_wheeled_vehicles, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(terrain_inaccessibility_wheeled_vehicles, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, extractiveness, 0.05).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(terrain_inaccessibility_wheeled_vehicles, mountain).
narrative_ontology:human_readable(terrain_inaccessibility_wheeled_vehicles, "Inaccessibility of Terrain to Wheeled Vehicles").
narrative_ontology:topic_domain(terrain_inaccessibility_wheeled_vehicles, "technological/physical").

% --- Binary flags ---
% No flags required for a Mountain constraint.

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges from physics and geology without human design.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(terrain_inaccessibility_wheeled_vehicles).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a neutral physical law (Mountain), this constraint has no inherent
% beneficiaries or victims in its design. The consequences affect different
% groups, but the constraint itself doesn't extract or coordinate by design.
% Therefore, these declarations are intentionally omitted.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it is a fundamental physical limit.
% Low ε and low suppression ensure it cannot be classified as anything else.

% PERSPECTIVE 1: THE TRAPPED INDIVIDUAL
% A person in a disaster zone experiences this as an absolute, unchangeable barrier.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE FIRST RESPONDER
% A paramedic or firefighter sees this as a logistical obstacle dictated by physics.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE TECHNOLOGY DEVELOPER
% An engineer at Hyundai or Boston Dynamics views this as a fixed physical problem
% to be solved, representing a market opportunity. The problem itself is a Mountain.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% A physicist or disaster strategist analyzes this as a universal law of mechanics.
constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terrain_inaccessibility_wheeled_vehicles_tests).

test(perspectival_invariance, [nondet]) :-
    % Verify perspectival invariance for a Mountain constraint.
    constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terrain_inaccessibility_wheeled_vehicles, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain.

test(threshold_validation_mountain, [nondet]) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, ExtMetricName, E),
    E =< 0.25,
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, suppression_requirement, S),
    S =< 0.05.

test(natural_law_profile_compliance, [nondet]) :-
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, accessibility_collapse, AC),
    config:param(natural_law_collapse_min, MinAC),
    AC >= MinAC,
    narrative_ontology:constraint_metric(terrain_inaccessibility_wheeled_vehicles, resistance, R),
    config:param(natural_law_resistance_max, MaxR),
    R =< MaxR,
    domain_priors:emerges_naturally(terrain_inaccessibility_wheeled_vehicles).


:- end_tests(terrain_inaccessibility_wheeled_vehicles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε = 0.05): The constraint is a physical law, not an
 *     economic mechanism. It doesn't "extract" value; it simply exists and
 *     imposes costs on overcoming it. The low value reflects this neutrality.
 *   - Suppression (0.05): The constraint "suppresses" the use of wheeled
 *     vehicles in certain areas, but not through active enforcement. It's an
 *     inherent property of the interaction between wheels and terrain.
 *   - NL Profile (AC=0.95, R=0.05, emerges_naturally): For a conventional
 *     wheeled vehicle, the alternative of driving over a collapsed building is
 *     completely foreclosed (high accessibility collapse). Resistance is futile
 *     and incoherent (low resistance). The constraint arises from physics, not
 *     human design. This profile is essential for the Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key feature of Mountain constraints
 *   representing physical laws. The person trapped by rubble, the paramedic unable
 *   to reach them, and the engineer designing a solution all agree on the
 *   fundamental nature of the physical barrier. Its classification as a Mountain
 *   is invariant across all indices (P,T,E,S).
 *
 * DIRECTIONALITY LOGIC:
 *   As a neutral physical law, the constraint has no designed beneficiaries or
 *   victims. The engine's directionality derivation chain is not activated, as
 *   no `constraint_beneficiary` or `constraint_victim` facts are declared.
 *   This correctly models the constraint's neutrality. The *consequences*
 *   create victims of circumstance, but this is distinct from a constraint
 *   *designed* to extract from a specific group.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a physical limit as a Mountain.
 *   This prevents the mislabeling of a natural barrier as a socially-constructed
 *   Snare. For example, one could argue "the lack of roads into this area is a
 *   Snare to keep residents isolated," which would be a different constraint story
 *   with a higher ε. This story focuses purely on the physical impossibility,
 *   which is correctly a Mountain, thus providing a clear baseline against
 *   which social or political constraints (like funding for roads) can be measured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_terrain_inaccessibility_wheeled_vehicles,
    'Is the universal reliance on wheeled vehicles a pure optimization against physical laws (Mountain), or is it a path-dependent equilibrium maintained by industrial inertia and suppressed alternatives (Piton)?',
    'A full techno-historical analysis of non-wheeled ground transport solutions and their historical suppression or failure.',
    'If it is a Piton, then "impassable terrain" is a theatrical excuse, not a real limit, and solutions have been ignored. If it remains a Mountain, then novel solutions like walking cars are genuine breakthroughs.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(terrain_inaccessibility_wheeled_vehicles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for low-extraction constraints (ε < 0.46).
% As a physical law, the metrics for this Mountain constraint are stable
% across civilizational time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: Not applicable. A Mountain is a barrier, not a coordination system.

% Network relationships (structural influence edges)
% This Mountain constraint necessitates the creation of coordination solutions (Ropes).
% A story about the "Elevate" vehicle system would be a Rope, influenced by this Mountain.
narrative_ontology:affects_constraint(terrain_inaccessibility_wheeled_vehicles, elevate_vehicle_system_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The absence of beneficiary/victim declarations
% correctly models the neutrality of this physical constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */