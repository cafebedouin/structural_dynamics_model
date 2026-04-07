% ============================================================================
% CONSTRAINT STORY: vehicle_routing_problem_distance_symmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vehicle_routing_problem_distance_symmetry, []).

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
 *   constraint_id: vehicle_routing_problem_distance_symmetry
 *   human_readable: Vehicle Routing Problem Distance Symmetry Constraint
 *   domain: operations_research/combinatorial_optimization
 *
 * SUMMARY:
 *   The Vehicle Routing Problem (VRP) distance symmetry constraint asserts
 *   that the distance from location A to location B equals the distance from
 *   location B to location A: d(A,B) = d(B,A). This constraint is a
 *   foundational assumption in classical VRP formulations and emerges
 *   naturally from Euclidean metric space axioms. It is not a policy choice,
 *   institutional arrangement, or contingent design decision — it is a
 *   property of the mathematical structure that underlies the problem domain.
 *   The constraint reduces the problem's complexity by eliminating half the
 *   distance matrix (since symmetric matrices contain redundant information).
 *   All perspectives classify this as a Mountain: the constraint is
 *   unchangeable within its domain, exhibits zero degrees of freedom, and
 *   applies universally to any routing system that operates in Euclidean
 *   space or metric spaces where the triangle inequality and symmetry axioms
 *   hold.
 *
 * KEY AGENTS:
 *   - Route Planning Agent: Structurally trapped (powerless/trapped) — any algorithm must work within the symmetry constraint or redefine the problem entirely
 *   - Problem Modeler: Analytical capacity (moderate/analytical) — recognizes the constraint as mathematical, not contingent; can choose domains but cannot change the mathematical property
 *   - Optimization Algorithm: Institutional actor (institutional/analytical) — encounters symmetry as a fixed feature; algorithms exploit it for efficiency but do not violate it
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the constraint as an emergent property of metric space axioms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vehicle_routing_problem_distance_symmetry, 0.12).
domain_priors:suppression_score(vehicle_routing_problem_distance_symmetry, 0.02).
domain_priors:theater_ratio(vehicle_routing_problem_distance_symmetry, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, extractiveness, 0.12).
narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vehicle_routing_problem_distance_symmetry, mountain).
narrative_ontology:human_readable(vehicle_routing_problem_distance_symmetry, "Vehicle Routing Problem Distance Symmetry Constraint").
narrative_ontology:topic_domain(vehicle_routing_problem_distance_symmetry, "operations_research/combinatorial_optimization").

domain_priors:emerges_naturally(vehicle_routing_problem_distance_symmetry).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROUTE PLANNING AGENT (MOUNTAIN) — Cannot escape the constraint. Any algorithm that produces a route must satisfy the symmetry assumption or explicitly rewrite the problem formulation. The agent cannot choose to violate the constraint; the mathematical structure enforces it universally.
constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROBLEM MODELER (MOUNTAIN) — Recognizes the constraint as a mathematical property, not a contingent design choice. The modeler can choose to work in the symmetric or asymmetric domain, but cannot eliminate the structural distinction. Both formulations exhibit irreducible mathematical properties.
constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: OPTIMIZATION ALGORITHM (MOUNTAIN) — Encounters the symmetry as a fixed feature of the problem space. Algorithms that exploit symmetry produce computationally efficient solutions; algorithms that ignore it do not benefit from the reduction in search space. The constraint is intrinsic to the mathematical structure.
constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal perspective, the distance symmetry constraint is a fixed property of Euclidean geometry and metric space mathematics. The constraint emerges naturally from the axioms of metric spaces and cannot be violated by any agent or system that operates within that mathematical framework.
constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vehicle_routing_problem_distance_symmetry_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vehicle_routing_problem_distance_symmetry, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, ExtMetricName, E),
    domain_priors:suppression_score(vehicle_routing_problem_distance_symmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vehicle_routing_problem_distance_symmetry),
    narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vehicle_routing_problem_distance_symmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vehicle_routing_problem_distance_symmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint imposes no coercion, extraction, or asymmetric burden. It is a structural property of the mathematical domain, not an institution that extracts value. The value reflects only that any simplifying assumption reduces available problem space — a negligible cost. Suppression (0.02): Minimal. There are no alternative frameworks being suppressed; the symmetry axiom is an axiom, not a suppressed option. Agents who need asymmetric distance functions must switch problem domains (directed graphs, manifolds) rather than negotiate within the symmetric domain. Theater ratio (0.05): Negligible. The constraint has no performative component. Either a metric space is symmetric, or it is not. Accessibility collapse (0.92): Very high. Once one accepts the metric space formulation, the symmetry constraint is entirely inaccessible from alternative frameworks — agents cannot 'work around' it within the domain. Resistance (0.08): Very low. No agent resists the constraint; it is simply the structure of the space.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification. There is no perspectival gap. This uniformity is the diagnostic signature of a genuine natural law: different observers with different structural positions all experience the constraint as unchangeable, inaccessible, and applicable universally. The constraint does not exhibit the perspectival variance that characterizes institutional arrangements (Rope, Snare, Tangled Rope) or degraded mechanisms (Piton, Scaffold).
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation applies to this Mountain constraint. Mountain constraints are beneficiary/victim-agnostic because they impose no extraction. The symmetry constraint has no beneficiaries (no agent gains asymmetric advantage) and no victims (no agent bears asymmetric cost). The constraint is indifferent across all observers — it applies equally to all agents regardless of power, time horizon, exit options, or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy. It is a pure Mountain — there is no tension between coordination and extraction, no hidden institutional arrangement, no degraded function. The constraint is what it claims to be: a mathematical property of metric spaces. The mandatrophy is resolved by recognizing that some constraints are genuinely, intrinsically mountain-type across all perspectives. The absence of mandatrophy is itself diagnostic — it confirms that the constraint is not masking an extractive institution under the appearance of natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetric_embedding_question,
    'Does the constraint apply universally or only within Euclidean metric spaces?',
    'Formal analysis of non-Euclidean routing domains (manifolds with non-symmetric metrics, directed graph environments, asymmetric cost functions from real-world factors like wind, elevation, or traffic flow directionality)',
    'If constraint is universal: Mountain classification confirmed for all routing contexts. If constraint is domain-specific: Classification may degrade to Rope (coordination of distance assumptions) in asymmetric domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_embedding_question, conceptual, 'Universal scope of distance symmetry across all routing domains').

omega_variable(
    computational_accessibility,
    'Is the constraint accessible in practice for large routing instances, or does it become computationally intractable?',
    'Empirical analysis of solver performance on symmetric vs asymmetric VRP instances of varying size; measurement of whether symmetry exploitation reduces computational barrier in practice',
    'If symmetry remains accessible: Mountain persists. If accessibility collapses for large instances: Constraint may degrade to Piton (performative rather than functional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_accessibility, empirical, 'Computational accessibility of distance symmetry constraint at scale').

omega_variable(
    real_world_violation_prevalence,
    'How frequently do real-world routing problems violate distance symmetry due to one-way streets, traffic flow directionality, or asymmetric terrain?',
    'Survey of real-world routing datasets; analysis of how often d(A,B) ≠ d(B,A) in practice',
    'If violations are rare: Constraint remains applicable as a simplifying assumption (Mountain). If violations are frequent: Constraint becomes a modeling approximation (Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_violation_prevalence, empirical, 'Prevalence of distance asymmetry in real-world routing problems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vehicle_routing_problem_distance_symmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrp_dist_sym_tr_t0, vehicle_routing_problem_distance_symmetry, theater_ratio, 0, 0.04).
narrative_ontology:measurement(vrp_dist_sym_tr_t5, vehicle_routing_problem_distance_symmetry, theater_ratio, 5, 0.05).
narrative_ontology:measurement(vrp_dist_sym_tr_t10, vehicle_routing_problem_distance_symmetry, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(vrp_dist_sym_be_t0, vehicle_routing_problem_distance_symmetry, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(vrp_dist_sym_be_t5, vehicle_routing_problem_distance_symmetry, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(vrp_dist_sym_be_t10, vehicle_routing_problem_distance_symmetry, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vehicle_routing_problem_distance_symmetry, information_standard).
narrative_ontology:affects_constraint(vehicle_routing_problem_distance_symmetry, traveling_salesman_problem_tour_optimality).
narrative_ontology:affects_constraint(vehicle_routing_problem_distance_symmetry, graph_metric_space_axioms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
