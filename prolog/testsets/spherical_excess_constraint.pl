% ============================================================================
% CONSTRAINT STORY: spherical_excess_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spherical_excess_constraint, []).

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
 *   constraint_id: spherical_excess_constraint
 *   human_readable: Spherical Excess Constraint in Non-Euclidean Geometry
 *   domain: mathematics/differential_geometry
 *
 * SUMMARY:
 *   Spherical excess is a mathematical constraint arising from the
 *   Gauss-Bonnet theorem in differential geometry. On any sphere or
 *   spherical-like closed surface with positive Gaussian curvature, the sum
 *   of interior angles in any closed region (a 'spherical polygon' or
 *   triangle) exceeds the corresponding Euclidean angle sum by an amount
 *   equal to the integral of Gaussian curvature over that region. For a
 *   sphere of radius R, the excess of any spherical triangle is precisely
 *   equal to its area divided by R². This constraint is logically and
 *   physically inevitable — no observer, agent, or institutional arrangement
 *   can modify, eliminate, or negotiate with it. The constraint has no
 *   beneficiaries or victims; it simply describes an intrinsic property of
 *   non-Euclidean geometry. There are no degrees of freedom, no exit options,
 *   and no alternative frameworks that escape the relationship. This makes
 *   spherical excess a canonical example of a Mountain constraint: a
 *   mathematical law that is unchangeable, emerges naturally from the axioms
 *   of geometry, and admits no suppression (no one can hide from it) and no
 *   theater (the constraint operates in pure mathematical form with no
 *   performative component).
 *
 * KEY AGENTS:
 *   - Mathematical Observer: Universal perspective (analytical/analytical) — perceives the constraint as intrinsic to the structure of non-Euclidean geometry itself
 *   - Differential Geometry Discipline: Collective institutional view (powerful/mobile) — recognizes the constraint as both discovered and immutable across all alternative frameworks
 *   - Applied Geometers (Cartographers, Surveyors): Practical users (moderate/constrained) — must account for spherical excess in real-world applications but cannot eliminate it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spherical_excess_constraint, 0.12).
domain_priors:suppression_score(spherical_excess_constraint, 0.02).
domain_priors:theater_ratio(spherical_excess_constraint, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spherical_excess_constraint, extractiveness, 0.12).
narrative_ontology:constraint_metric(spherical_excess_constraint, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(spherical_excess_constraint, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(spherical_excess_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(spherical_excess_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spherical_excess_constraint, mountain).
narrative_ontology:human_readable(spherical_excess_constraint, "Spherical Excess Constraint in Non-Euclidean Geometry").
narrative_ontology:topic_domain(spherical_excess_constraint, "mathematics/differential_geometry").

domain_priors:emerges_naturally(spherical_excess_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Spherical excess is an intrinsic geometric property of closed surfaces with positive curvature. The constraint emerges from the Gauss-Bonnet theorem: for any spherical triangle, the sum of interior angles must exceed the sum for an equivalent Euclidean triangle by exactly the integral of Gaussian curvature over the enclosed region. This relationship is a mathematical necessity, not a contingent institutional fact. The constraint is invariant across all observational frameworks and measurement methodologies.
constraint_indexing:constraint_classification(spherical_excess_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of differential geometry as a mathematical discipline, spherical excess represents an immutable structural feature of curved spaces. Geometers cannot eliminate, reduce, or work around this constraint through any method or framework innovation. Attempts to flatten spherical geometry always generate contradictions. The constraint is both discovered and irreplaceable.
constraint_indexing:constraint_classification(spherical_excess_constraint, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% A cartographer must account for spherical excess when representing planetary surfaces on flat maps. The excess is an unchangeable feature of attempting to project a curved surface onto a plane. No map projection eliminates distortion — the constraint ensures that all projections involve trade-offs. Yet the cartographer experiences this as a hard limit on what is possible, not as a negotiable constraint.
constraint_indexing:constraint_classification(spherical_excess_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spherical_excess_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(spherical_excess_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spherical_excess_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(spherical_excess_constraint, ExtMetricName, E),
    domain_priors:suppression_score(spherical_excess_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(spherical_excess_constraint),
    narrative_ontology:constraint_metric(spherical_excess_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(spherical_excess_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(spherical_excess_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint represents pure mathematical structure with no extraction component. The value exceeds zero only because the Gauss-Bonnet relationship does impose a real limitation on what geometric relationships are possible — one could imagine simpler geometries without this constraint, so its existence represents a structural 'cost' to reality. However, this cost is not extractive in the sense of one agent gaining at another's expense. Suppression (0.02): Negligible. There is no mechanism suppressing knowledge of or access to the constraint. The constraint is publicly discoverable and transparently represented in mathematical literature. Resistance (0.08): Low. No countervailing force opposes the constraint's operation. Theater ratio (0.08): Minimal. The constraint has no performative element. Its verification is direct mathematical proof, not institutional or social performance. The measurement values remain essentially flat over the interval — the constraint has not changed in character or severity over the history of differential geometry.
 *
 * PERSPECTIVAL GAP:
 *   Unlike extractive constraints that show perspectival gaps between beneficiaries and victims, spherical excess shows complete perspectival agreement: all observers (powerful, moderate, analytical) classify it identically as Mountain. This uniformity is diagnostic of a true mathematical constraint rather than a social or institutional one. The cartographer's perspective differs only in context (they must work with the constraint in practice) but not in classification. This is the canonical pattern for NL constraints — invariant classification across all observables and all observer positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Spherical excess resolves the mandatrophy by eliminating it entirely. There is no ambiguity between 'is this coordination or extraction?' because the constraint is neither. It is a brute mathematical fact that permits neither beneficiaries nor victims, neither negotiation nor alternative formulations. The Gauss-Bonnet theorem exhausts the relationship between curvature and angle sum in all Riemannian geometries. The constraint cannot be reframed as hidden coordination or mislabeled as extraction. It simply is what it is: a mathematical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gauss_bonnet_exhaustiveness,
    'Does the Gauss-Bonnet theorem completely capture the relationship between curvature and angle sum, or might alternative geometric frameworks reveal additional constraints?',
    'Systematic review of non-Riemannian geometries and generalizations of Gauss-Bonnet; assessment of whether all known geometric systems instantiate the same relationship or whether alternative systems exist.',
    'If exhaustive: mountain classification is definitive across all geometric frameworks. If alternative frameworks exist: the constraint may be framework-relative rather than universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gauss_bonnet_exhaustiveness, conceptual, 'Whether Gauss-Bonnet completely captures curvature-angle relationships').

omega_variable(
    measurement_independence,
    'Is spherical excess measurable independently of the specific coordinate system or metric choice on the surface?',
    'Demonstration that angle excess calculations yield identical results across all equivalent geometric representations; proof that the constraint is invariant under all diffeomorphisms preserving the surface topology.',
    'If coordinate-independent: confirms the mountain classification by showing the constraint is intrinsic. If dependent on representation: the constraint may be epistemic rather than ontological.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_independence, empirical, 'Coordinate-independence of spherical excess measurement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spherical_excess_constraint, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spherex_theater_t0, spherical_excess_constraint, theater_ratio, 0, 0.05).
narrative_ontology:measurement(spherex_theater_t500, spherical_excess_constraint, theater_ratio, 500, 0.08).
narrative_ontology:measurement(spherex_theater_t1000, spherical_excess_constraint, theater_ratio, 1000, 0.08).

% Extraction over time
narrative_ontology:measurement(spherex_extract_t0, spherical_excess_constraint, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(spherex_extract_t500, spherical_excess_constraint, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(spherex_extract_t1000, spherical_excess_constraint, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spherical_excess_constraint, information_standard).
narrative_ontology:affects_constraint(spherical_excess_constraint, geodesic_deviation_constraint).
narrative_ontology:affects_constraint(spherical_excess_constraint, riemannian_metric_constraint).

% DUAL FORMULATION NOTE:
% Spherical excess is upstream of applied geometric constraints. Any constraint involving curved surfaces (cartography, survey measurement, geodetic systems) depends on the structural reality that spherical excess is non-negotiable. This constraint is not decomposable — it is a single, unified geometric fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
