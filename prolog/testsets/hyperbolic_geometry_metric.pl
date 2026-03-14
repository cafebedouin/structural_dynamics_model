% ============================================================================
% CONSTRAINT STORY: hyperbolic_geometry_metric
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hyperbolic_geometry_metric, []).

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
 *   constraint_id: hyperbolic_geometry_metric
 *   human_readable: Hyperbolic Geometry Metric Constraint
 *   domain: mathematics/differential_geometry
 *
 * SUMMARY:
 *   The hyperbolic geometry metric is a mathematical constraint emerging from
 *   the logical structure of non-Euclidean geometry. Unlike institutional or
 *   social constraints that might be negotiated, renegotiated, or abandoned,
 *   the hyperbolic metric is an invariant property of any geometric system
 *   satisfying the negation of the Euclidean parallel postulate. The
 *   constraint has no beneficiaries or victims in the institutional sense —
 *   it imposes uniform requirements on all agents engaging with hyperbolic
 *   space, regardless of their position or interests. The extractiveness is
 *   minimal (0.08) because the 'extraction' consists only of the irreducible
 *   mathematical demands of consistency: if you work with negative curvature,
 *   you cannot simultaneously maintain Euclidean distance relations. This is
 *   not coercion but logical necessity. The suppression is near-zero (0.02)
 *   because there are always alternatives — one can choose to work in
 *   Euclidean, spherical, or flat spaces instead. The exit cost is epistemic
 *   (the loss of hyperbolic geometry's explanatory power for certain
 *   phenomena), not coercive.
 *
 * KEY AGENTS:
 *   - Mathematicians and Differential Geometers: Researchers working with Riemannian manifolds accept the metric as a structural requirement, not an extraction
 *   - Physics Researchers: Those applying hyperbolic geometry to general relativity or AdS/CFT encounter the metric as an immutable feature of their domain
 *   - Mathematical Structures (abstract): The hyperbolic metric itself is the 'agent' in the sense that it determines relationships and constraints for anything instantiating non-Euclidean geometry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hyperbolic_geometry_metric, 0.08).
domain_priors:suppression_score(hyperbolic_geometry_metric, 0.02).
domain_priors:theater_ratio(hyperbolic_geometry_metric, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hyperbolic_geometry_metric, extractiveness, 0.08).
narrative_ontology:constraint_metric(hyperbolic_geometry_metric, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hyperbolic_geometry_metric, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hyperbolic_geometry_metric, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hyperbolic_geometry_metric, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hyperbolic_geometry_metric, mountain).
narrative_ontology:human_readable(hyperbolic_geometry_metric, "Hyperbolic Geometry Metric Constraint").
narrative_ontology:topic_domain(hyperbolic_geometry_metric, "mathematics/differential_geometry").

domain_priors:emerges_naturally(hyperbolic_geometry_metric).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — From the mathematical standpoint, hyperbolic geometry's metric structure is an immutable consequence of non-Euclidean axioms. The metric tensor with negative constant curvature (K < 0) is logically entailed by the negation of the parallel postulate. This is a necessity of mathematical structure, not a contingent institutional arrangement. Zero degrees of freedom.
constraint_indexing:constraint_classification(hyperbolic_geometry_metric, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: DIFFERENTIAL GEOMETER (MOUNTAIN) — Mathematicians working with Riemannian manifolds cannot choose to escape the metric constraint. The curvature tensor is determined by the metric. Working in hyperbolic space means accepting that distances, angles, and geodesics follow from the negative curvature. The constraint is perceived as an unchangeable law of the mathematical system itself.
constraint_indexing:constraint_classification(hyperbolic_geometry_metric, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: PHYSICS RESEARCHER (APPLIED SETTING) (MOUNTAIN) — When modeling spacetime near massive objects or AdS/CFT correspondence, researchers cannot choose to ignore hyperbolic geometry. The metric structure emerges from the physics itself. Whether measuring geodesics in gravitational wells or examining holographic duality, the hyperbolic metric is an immutable structural feature of the phenomenon being studied.
constraint_indexing:constraint_classification(hyperbolic_geometry_metric, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hyperbolic_geometry_metric_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hyperbolic_geometry_metric, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hyperbolic_geometry_metric, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hyperbolic_geometry_metric, ExtMetricName, E),
    domain_priors:suppression_score(hyperbolic_geometry_metric, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hyperbolic_geometry_metric),
    narrative_ontology:constraint_metric(hyperbolic_geometry_metric, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hyperbolic_geometry_metric, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hyperbolic_geometry_metric_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The hyperbolic metric imposes formal requirements (curvature tensor structure, geodesic equations, distance measures) but these are not extractive in the institutional sense. They are mathematical necessities. The small non-zero value reflects the slight 'tax' of having to maintain internal consistency within the system — if you choose hyperbolic geometry, you accept all its consequences. This is not an agent choosing to extract from another; it is a system's internal logical structure. Suppression (0.02): Near-zero. There is no suppression because anyone can exit by simply working in a different geometric system. The constraint is universal within its domain but not coercive across domains. Resistance to the constraint is negligible — mathematicians do not struggle against the hyperbolic metric; they either adopt it (finding it useful for certain problems) or abandon it (choosing other geometries). Theater ratio (0.15): Very low. The constraint's operation is entirely transparent. The mathematical relationships that follow from negative curvature are directly verifiable through proof and computation. There is no performative layer or institutional theater. The formalism is what it is.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the hyperbolic metric constraint is uniform-type (mountain across all perspectives). All observers — whether analytical, practical, or embedded — perceive the same immutable mathematical structure. The constraint does not generate the conflict typical of institutional arrangements because no agent benefits from violating it while others comply. Rather, all agents either accept the constraint as part of their framework or step outside the framework entirely. This uniformity is precisely what characterizes a true mountain: the constraint is the same from every valid observation point within its domain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not meaningfully apply to this constraint because there is no extraction flow. The hyperbolic metric imposes symmetric requirements on all agents within its domain. No agent experiences d > 0.5 (full target) or d < 0.5 (full beneficiary). The constraint simply structures the space in which mathematical and physical work occurs. For agents choosing to work with hyperbolic geometry, the metric is a resource (they benefit from its explanatory power); for agents choosing other geometries, the metric is absent (no cost or benefit). The choice to engage is mobile — agents can exit by switching frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the clean resolution of mandatrophy for mathematical/logical constraints. There is no ambiguity between 'pure extraction' and 'pure coordination' because the constraint is neither. It is a structural necessity that all agents working within hyperbolic geometry must satisfy. The mandatrophy resolves to: 'This is a mountain because it is logically entailed, universally uniform, immutably structured, and present across all valid observation points within its epistemic domain. It is not institutional extraction because there is no extraction flow, no beneficiary advantage, and no suppression mechanism. It is not coordination because there is nothing to coordinate — the metric structure simply is what it is.' The classification stands on all standard gates without exception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_independence_axioms,
    'Is the hyperbolic metric uniquely determined by the non-Euclidean axioms alone, or does it depend on additional geometric or physical assumptions?',
    'Foundational analysis of Riemannian geometry axioms; examination of whether alternative non-Euclidean metrics (elliptic, parabolic of variable curvature) satisfy the same axiom set',
    'If uniquely determined: mountain classification is fully justified. If dependent on additional assumptions: the constraint may be contingent on those assumptions and partially decomposable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_independence_axioms, conceptual, 'Whether hyperbolic metric is uniquely determined by non-Euclidean axioms').

omega_variable(
    physical_vs_mathematical_necessity,
    'In applications (general relativity, AdS/CFT), is the hyperbolic metric a necessity imposed by physical reality, or a choice of mathematical model?',
    'Examination of alternative metrics that fit the same empirical data; analysis of whether hyperbolic geometry is enforced by physics or merely convenient',
    'If physical necessity: mountain across all contexts. If modeling choice: the applied constraint may be rope or tangled_rope (the mathematical structure remains mountain, but the application constraint is hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_mathematical_necessity, empirical, 'Whether hyperbolic metric is physically or merely mathematically necessary in applications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hyperbolic_geometry_metric, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hyp_tr_t0, hyperbolic_geometry_metric, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hyp_tr_t5, hyperbolic_geometry_metric, theater_ratio, 5, 0.15).

% Extraction over time
narrative_ontology:measurement(hyp_be_t0, hyperbolic_geometry_metric, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hyp_be_t5, hyperbolic_geometry_metric, base_extractiveness, 5, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hyperbolic_geometry_metric, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
