% ============================================================================
% CONSTRAINT STORY: riemannian_metric_curvature
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_riemannian_metric_curvature, []).

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
 *   constraint_id: riemannian_metric_curvature
 *   human_readable: Riemannian Metric Curvature as Geometric Constraint
 *   domain: differential_geometry/mathematical_physics
 *
 * SUMMARY:
 *   Riemannian metric curvature is a geometric property that emerges
 *   necessarily from the definition of a smooth manifold with a Riemannian
 *   metric. Given a metric, the curvature tensor is determined uniquely by
 *   the Levi-Civita connection via the Riemann curvature tensor. This is not
 *   a negotiable constraint imposed by external authority — it is a
 *   mathematical structure that cannot be avoided or reframed without
 *   abandoning the geometric framework itself. No agent, regardless of power
 *   or position, can negotiate away the curvature of a given metric. The
 *   constraint applies identically to all observers: mathematicians,
 *   physicists, engineers, institutions. There is no beneficiary class and no
 *   victim class — the constraint binds all equally. This is the defining
 *   signature of a mountain constraint in the Deferential Realism framework.
 *
 * KEY AGENTS:
 *   - Differential Geometers: Analytical agents (analytical/analytical) — work with curvature as a fundamental tool; cannot negotiate the constraint itself, only apply it
 *   - General Relativists: Powerful agents (analytical/analytical) — use curvature to model spacetime; the constraint determines their equations of motion uniquely
 *   - Physicists: Institutional agents (analytical/analytical) — apply Riemannian geometry to physical systems; curvature determines observable predictions
 *   - Mathematical Logicians: Analytical agents (analytical/analytical) — examine the formal structure of geometry; confirm the necessity of the constraint within the axiomatic framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(riemannian_metric_curvature, 0.08).
domain_priors:suppression_score(riemannian_metric_curvature, 0.02).
domain_priors:theater_ratio(riemannian_metric_curvature, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(riemannian_metric_curvature, extractiveness, 0.08).
narrative_ontology:constraint_metric(riemannian_metric_curvature, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(riemannian_metric_curvature, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(riemannian_metric_curvature, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(riemannian_metric_curvature, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(riemannian_metric_curvature, mountain).
narrative_ontology:human_readable(riemannian_metric_curvature, "Riemannian Metric Curvature as Geometric Constraint").
narrative_ontology:topic_domain(riemannian_metric_curvature, "differential_geometry/mathematical_physics").

domain_priors:emerges_naturally(riemannian_metric_curvature).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of differential geometry, Riemannian curvature is an invariant geometric property. The constraint is mathematical: given a smooth manifold with a Riemannian metric, the curvature tensor is determined uniquely. No observer can exit, negotiate, or reframe this constraint. It is not enforced — it is constitutive of the structure itself.
constraint_indexing:constraint_classification(riemannian_metric_curvature, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From general relativity, spacetime curvature determines geodesics and hence the motion of massive bodies. The constraint binds all agents equally: mass-energy distributions curve spacetime, and all particles follow geodesic paths in that curved geometry. The constraint is enforced by the structure of spacetime itself, not by external coercion. Zero degrees of freedom for all observers.
constraint_indexing:constraint_classification(riemannian_metric_curvature, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even for agents with institutional power and technical mobility, the mathematical necessity of curvature does not diminish. A physicist or mathematician cannot ignore or circumvent the Ricci tensor or scalar curvature in any manifold they work with. The constraint applies uniformly — no hierarchical variation in how it binds.
constraint_indexing:constraint_classification(riemannian_metric_curvature, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riemannian_metric_curvature_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(riemannian_metric_curvature, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riemannian_metric_curvature, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(riemannian_metric_curvature, ExtMetricName, E),
    domain_priors:suppression_score(riemannian_metric_curvature, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(riemannian_metric_curvature),
    narrative_ontology:constraint_metric(riemannian_metric_curvature, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(riemannian_metric_curvature, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(riemannian_metric_curvature_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract resources from any agent. No agent bears a cost that another agent avoids. The curvature property is intrinsic to the metric — it either exists or does not. All agents experience it identically. Suppression (0.02): Negligible. There are no alternatives to suppress. Given a metric, curvature follows necessarily. No agent is prevented from computing, using, or understanding the constraint. The suppression score reflects only the boundary condition at framework limits (e.g., what happens at singularities is undefined, not suppressed). Theater ratio (0.05): Minimal. The constraint is not performative. The mathematics is transparent and reproducible. Verification is computational, not ritual-based. Accessibility collapse (0.92): Very high. To work with Riemannian metrics at all, one must accept the curvature constraint. The framework becomes inaccessible if one rejects this invariant property. Resistance (0.03): Minimal. No organized agents resist the constraint. Mathematical and physical communities accept it as foundational.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists. All three perspectives classify as mountain because the constraint is truly invariant across all observational positions. The geometer, physicist, and mathematical physicist all encounter the same mathematical necessity. The constraint does not depend on power level, time horizon, or exit options — it applies identically from all (P,T,E,S) tuples. This invariance is the diagnostic signature that confirms the mountain classification and demonstrates why some constraints are truly natural laws rather than institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no beneficiary-victim asymmetry. The curvature property does not flow from one agent to another. All agents relate to the constraint identically: they must accept it as a structural feature of any Riemannian manifold they work with. The derived d value would be identical (approximately 0.5, symmetric) for all agents, because no agent benefits at another's expense. This absence of asymmetric extraction is what defines the constraint as a natural law rather than an extractive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable to this constraint. Mandatrophy arises when a constraint claims to be both coordinating (beneficiary-victim asymmetry justified) and extracting (pure rent-seeking). A mountain constraint has zero mandatrophy because it is neither extracting nor coordinating — it is simply a structural fact. The constraint cannot be misclassified as extraction because there is no extraction mechanism. It cannot be misclassified as coordination because there is no collective action problem it solves (curvature is not a solution to disagreement; it is the geometric reality that all agents must work within).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    curvature_observability_signature,
    'Is Riemannian curvature an intrinsic geometric property or dependent on embedding in a higher-dimensional space?',
    'Gauss''s Theorema Egregium establishes that Gaussian curvature is intrinsic to the metric itself, invariant under isometric embeddings. This is already established mathematically.',
    'Resolution confirms: curvature is structure-dependent, not observer-dependent. No measurement basis ambiguity exists. The constraint is invariant across all possible computational or observational schemes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(curvature_observability_signature, empirical, 'Intrinsic vs extrinsic curvature determination').

omega_variable(
    metric_determinism_universality,
    'Does the uniqueness of the Levi-Civita connection guarantee that curvature is the only possible geometric invariant, or could alternative geometric structures coexist?',
    'The Fundamental Theorem of Riemannian Geometry proves that given a metric, the Levi-Civita connection is unique. This determines the curvature tensor completely. No alternative frameworks are compatible with the same metric.',
    'Resolution confirms: given a Riemannian metric, curvature is uniquely determined. No degrees of freedom exist for negotiation or alternative interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_determinism_universality, empirical, 'Uniqueness of geometric invariants from a given metric').

omega_variable(
    physical_realization_scope,
    'Does the mathematical constraint on Riemannian curvature apply to all physical spacetimes, or only to idealized smooth manifolds?',
    'Real spacetimes (near singularities, at quantum scales) may violate smoothness assumptions. However, where smooth Riemannian geometry applies, the curvature constraint is absolute. Breakdown occurs at scale where the framework becomes inapplicable, not where it becomes negotiable.',
    'If framework applies: mountain classification holds. If framework breaks: the constraint transitions from mountain (in smooth regime) to undefined (in quantum/singular regime). No mixed regime where negotiation is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realization_scope, empirical, 'Scope of applicability for smooth Riemannian geometry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riemannian_metric_curvature, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(riemann_tr_t0, riemannian_metric_curvature, theater_ratio, 0, 0.05).
narrative_ontology:measurement(riemann_tr_t5, riemannian_metric_curvature, theater_ratio, 5, 0.05).
narrative_ontology:measurement(riemann_tr_t10, riemannian_metric_curvature, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(riemann_be_t0, riemannian_metric_curvature, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(riemann_be_t5, riemannian_metric_curvature, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(riemann_be_t10, riemannian_metric_curvature, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(riemannian_metric_curvature, information_standard).
narrative_ontology:affects_constraint(riemannian_metric_curvature, geodesic_completeness).
narrative_ontology:affects_constraint(riemannian_metric_curvature, sectional_curvature_bounds).
narrative_ontology:affects_constraint(riemannian_metric_curvature, ricci_flow_dynamics).

% DUAL FORMULATION NOTE:
% Riemannian curvature is a foundational constraint upstream of more complex geometric and physical constraints. Geodesic completeness, sectional curvature bounds, and Ricci flow dynamics all depend on curvature properties. The network reflects this mathematical dependency: curvature determines which downstream constraints can be satisfied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
