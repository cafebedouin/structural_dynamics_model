% ============================================================================
% CONSTRAINT STORY: differentiability_degree_freedom
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_differentiability_degree_freedom, []).

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
 *   constraint_id: differentiability_degree_freedom
 *   human_readable: Differentiability and Degree of Freedom in Constraint Systems
 *   domain: mathematical_logic/systems_theory
 *
 * SUMMARY:
 *   Differentiability degree of freedom is a constraint intrinsic to the
 *   mathematical structure of systems with constraints. In any system with N
 *   degrees of freedom and M independent differentiable constraints, the
 *   accessible configuration space has dimension N - M (generically). This
 *   relationship is not a policy, institution, or social arrangement — it is
 *   a structural feature of manifolds and constraint geometry. The constraint
 *   is universal across all applications: mechanical systems, field theories,
 *   optimization problems, information geometry, and dynamical systems all
 *   exhibit the same reduction. The differentiability requirement (rather
 *   than mere constraint existence) ensures that the implicit function
 *   theorem applies and that the dimension formula holds without exception.
 *   This is a natural law constraint: it has zero institutional content, zero
 *   coercion, and zero alternatives.
 *
 * KEY AGENTS:
 *   - Mathematical Structure: The constraint itself — no agent, no beneficiary, no victim
 *   - Analytical Observer (Universal): Views the constraint as a necessary consequence of differential geometry
 *   - Applied Engineer (Powerful/Mobile): Must respect the constraint but has full freedom to choose how many constraints to impose
 *   - Pure Mathematician (Analytical): Recognizes the constraint as a theorem derived from axioms
 *   - Dynamical Systems Analyst (Analytical): Uses the constraint as a foundation for stability and attractor analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(differentiability_degree_freedom, 0.12).
domain_priors:suppression_score(differentiability_degree_freedom, 0.03).
domain_priors:theater_ratio(differentiability_degree_freedom, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(differentiability_degree_freedom, extractiveness, 0.12).
narrative_ontology:constraint_metric(differentiability_degree_freedom, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(differentiability_degree_freedom, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(differentiability_degree_freedom, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(differentiability_degree_freedom, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(differentiability_degree_freedom, mountain).
narrative_ontology:human_readable(differentiability_degree_freedom, "Differentiability and Degree of Freedom in Constraint Systems").
narrative_ontology:topic_domain(differentiability_degree_freedom, "mathematical_logic/systems_theory").

domain_priors:emerges_naturally(differentiability_degree_freedom).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL NECESSITY (MOUNTAIN) — Differentiability constraints are intrinsic to mathematical structure. Any system with M constraints reduces the degrees of freedom by at least M dimensions (generically). This is not an institutional choice or a policy decision — it follows from the definition of constraint and dimension. The relationship ΔdoF = N - M emerges necessarily from the geometry of configuration spaces.
constraint_indexing:constraint_classification(differentiability_degree_freedom, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED ENGINEER (MOUNTAIN) — Regardless of what the engineer wants or values, adding a constraint removes degrees of freedom. This is invariant across all engineering domains: structural mechanics, thermodynamics, robotics, electrical networks. The engineer can choose to add more constraints, fewer constraints, or constraints of different type, but cannot add a constraint and have degrees of freedom increase. The constraint is mathematical fact, not design choice.
constraint_indexing:constraint_classification(differentiability_degree_freedom, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: PURE MATHEMATICIAN (MOUNTAIN) — Differentiability is a property of manifolds and differentiable maps. The dimension of the solution space of a system of equations is determined by the rank of the constraint Jacobian. Smoothness (differentiability) is necessary for rank calculations to be well-defined and for the implicit function theorem to apply. This is not negotiable or observer-dependent — it follows from differential geometry.
constraint_indexing:constraint_classification(differentiability_degree_freedom, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: DYNAMICAL SYSTEMS (MOUNTAIN) — A differentiable constraint on a phase space reduces the attractor dimension and solution space dimension. This holds for dissipative systems, Hamiltonian systems, and stochastic systems (where it applies to stationary measure support). The reduction is not approximate or contingent — it is exact for generic constraint configurations. Differentiability is required for Lyapunov exponents and stability analysis to be well-defined.
constraint_indexing:constraint_classification(differentiability_degree_freedom, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(differentiability_degree_freedom_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(differentiability_degree_freedom, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(differentiability_degree_freedom, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(differentiability_degree_freedom, ExtMetricName, E),
    domain_priors:suppression_score(differentiability_degree_freedom, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(differentiability_degree_freedom),
    narrative_ontology:constraint_metric(differentiability_degree_freedom, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(differentiability_degree_freedom, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(differentiability_degree_freedom_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint imposes no asymmetric extraction — it applies universally and equally to all agents and systems. The small residual value (0.12, above theoretical zero) accounts for the minimal cost of understanding and applying differentiability requirements (documentation, formal education in differential geometry). This is not extraction but necessity cost. Suppression (0.03): Minimal. There are no alternatives to differentiability in classical differential geometry — you cannot construct a valid smooth manifold while violating this constraint. The suppression is not coercive suppression but logical necessity. Suppression value reflects that the constraint cannot be avoided or negotiated; it can only be accepted or worked outside the differentiable framework. Theater ratio (0.08): Minimal. There is no performative aspect to differentiability constraints — their function is entirely structural. No institutional ritual or symbolic display is needed to maintain the constraint; the mathematics itself enforces it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap. All observers — powerless, moderate, powerful, organized, institutional, analytical — classify it identically as Mountain across all time horizons and exit options. The uniformity is diagnostic of a genuine natural law constraint. There is no social or power dimension to the constraint; it applies with equal force to a student solving a homework problem and to a civilization-scale optimization problem. This uniformity is the gold-standard signature of a mountain constraint in the DR system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to mountain constraints. There are no beneficiaries or victims — the constraint is impersonal. The directional function f(d) is not computed because the constraint's extractiveness value (0.12) is determined entirely by intrinsic mathematical structure, not by power asymmetry or exit options. The low extractiveness value is invariant across all possible d values. No agent can negotiate or escape the constraint through power, organizational capacity, time horizon manipulation, or spatial scope changes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    holonomic_vs_nonholonomic_generalization,
    'Does the differentiability-degree-freedom relationship generalize identically to non-holonomic constraints (constraints on velocities rather than positions)?',
    'Comparison of dimension reduction formulas: holonomic constraints reduce dimension M for rank-M constraint Jacobian; non-holonomic constraints reduce dimension but not in simple rank formula. Analysis of Pfaffian constraint systems.',
    'If generalization fails: the mountain classification may be local to holonomic systems only. If it generalizes: the mountain extends to broader constraint classes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(holonomic_vs_nonholonomic_generalization, empirical, 'Whether non-holonomic constraints exhibit identical differentiability-dimension relationship').

omega_variable(
    singular_point_behavior,
    'At points where the constraint Jacobian is singular (rank < M), does the degree-of-freedom reduction fail, remain constant, or exhibit unpredictable behavior?',
    'Study of constraint singularities (e.g., gimbal lock in rotational systems, configuration space singularities); analysis of solution set topology near singular points.',
    'If unpredictable: the mountain classification is challenged at singular configurations. If the reduction persists via generalization: the mountain extends to singular regions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(singular_point_behavior, empirical, 'Behavior of degree-of-freedom reduction at constraint singularities').

omega_variable(
    approximation_vs_exact_constraint,
    'Is there a regime in which approximate or weak constraints behave qualitatively differently from exact constraints in their degree-of-freedom reduction effect?',
    'Perturbation analysis: study how small constraint violations affect dimension reduction; examine transition from weakly-enforced to strongly-enforced constraints.',
    'If yes: the mountain classification applies only to exact constraints; approximate constraints may decompose into distinct constraint stories. If no: the mountain is robust to enforcement regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_vs_exact_constraint, empirical, 'Whether approximate constraints exhibit different dimension-reduction dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(differentiability_degree_freedom, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddf_tr_t0, differentiability_degree_freedom, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ddf_tr_t50, differentiability_degree_freedom, theater_ratio, 50, 0.08).
narrative_ontology:measurement(ddf_tr_t100, differentiability_degree_freedom, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(ddf_be_t0, differentiability_degree_freedom, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ddf_be_t50, differentiability_degree_freedom, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(ddf_be_t100, differentiability_degree_freedom, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(differentiability_degree_freedom, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
