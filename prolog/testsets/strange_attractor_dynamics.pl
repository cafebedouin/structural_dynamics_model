% ============================================================================
% CONSTRAINT STORY: strange_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strange_attractor_dynamics, []).

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
 *   constraint_id: strange_attractor_dynamics
 *   human_readable: Strange Attractor Dynamics (Sensitive Dependence on Initial Conditions)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   Strange attractor dynamics — specifically the sensitive dependence on
 *   initial conditions characteristic of chaotic systems — represents a
 *   fundamental constraint on predictability in nonlinear systems. The
 *   constraint emerges from the mathematical structure of the attractor
 *   itself: systems with positive Lyapunov exponents exhibit exponential
 *   divergence of nearby trajectories, collapsing long-term prediction to
 *   chance. Unlike institutional constraints or policy mechanisms, this
 *   constraint is indifferent to the observer's resources, preferences, or
 *   institutional position. A weather forecaster with unlimited computing
 *   power faces the same fundamental ceiling as one with minimal resources. A
 *   central bank cannot negotiate with the mathematics of market dynamics.
 *   The constraint is not coercive (suppression = 0.02) because there is no
 *   alternative pathway being suppressed — the structure permits no
 *   workaround. It is not theatrical (theater_ratio = 0.08) because the
 *   constraint manifests directly in failed predictions, not through
 *   institutional performance metrics. The constraint exhibits perfect
 *   invariance across observational contexts: whether measured through
 *   weather prediction skill, financial forecasting, or laboratory turbulence
 *   experiments, the structural signature remains identical. This invariance
 *   is the hallmark of a natural law constraint (Mountain).
 *
 * KEY AGENTS:
 *   - Predictable Observer: Any agent attempting finite-horizon prediction from finite-precision measurements (trapped/immediate) — experiences the constraint as an immutable boundary
 *   - Applied Forecaster: Weather services, financial institutions, engineering design teams (moderate/biographical) — have resources and agency but face the same mathematical ceiling
 *   - Analytical Observer: Dynamical systems theorists and mathematical physicists (analytical/civilizational) — perceive the constraint as a structural invariant of the mathematical system itself
 *   - The Attractor Itself: The mathematical object (not an agent, but the locus of the constraint) — imposes the constraint through its topological and ergodic properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractor_dynamics, 0.12).
domain_priors:suppression_score(strange_attractor_dynamics, 0.02).
domain_priors:theater_ratio(strange_attractor_dynamics, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, extractiveness, 0.12).
narrative_ontology:constraint_metric(strange_attractor_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(strange_attractor_dynamics, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(strange_attractor_dynamics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strange_attractor_dynamics, mountain).
narrative_ontology:human_readable(strange_attractor_dynamics, "Strange Attractor Dynamics (Sensitive Dependence on Initial Conditions)").
narrative_ontology:topic_domain(strange_attractor_dynamics, "mathematical/physical").

domain_priors:emerges_naturally(strange_attractor_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREDICTABLE OBSERVER — An observer with finite measurement precision is trapped within the constraint. No exit exists: no amount of additional resources or care can overcome the exponential divergence of nearby trajectories. The constraint is experienced as an immutable boundary on predictability itself. Suppression is minimal because there is no alternative pathway — the mathematical structure permits no workaround.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — From the standpoint of dynamical systems theory, sensitive dependence is a structural invariant of chaotic systems. It emerges from the topology of the attractor (positive Lyapunov exponents) and is independent of any observer's preferences, institutional arrangements, or policy choices. The constraint classifies identically across all contexts because its root cause is mathematical, not social or economic.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: APPLIED FORECASTER — A practitioner attempting to forecast chaotic systems (weather, financial markets, turbulence) experiences the constraint as an absolute limit on prediction horizon. They may have agency to improve measurement networks, computing power, and algorithmic sophistication, but all improvements reach a hard ceiling determined by the attractor's geometry. The constraint is immovable even for resourced, organized actors.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractor_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(strange_attractor_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strange_attractor_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(strange_attractor_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(strange_attractor_dynamics),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(strange_attractor_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. There is no extraction mechanism because there is no alternative being suppressed. The constraint operates through mathematical necessity, not through restriction of options. Unlike institutional constraints that extract value by blocking access to alternatives, this constraint simply makes certain predictions impossible — it does not create asymmetric costs between winners and losers. The small non-zero value (0.12 vs. 0.00) reflects the fact that organizations can sometimes exploit sensitive dependence strategically (weather uncertainty, market volatility), but this is parasitic on the constraint rather than constitutive of it. Suppression (0.02): Minimal. There are no alternatives being suppressed because the mathematics permits none. No policy, no institution, no technological breakthrough can eliminate sensitive dependence from systems with positive Lyapunov exponents. Theater ratio (0.08): Minimal. The constraint manifests directly in failed predictions and divergent trajectories. There is no performative element — the dynamics reveal themselves empirically. The small non-zero value reflects that some forecasting institutions use prediction failures to justify budgets and attention ('we must invest in better models'), but this is institutional use of the constraint, not the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is unusual: all perspectives converge on Mountain. The Lyapunov exponent is not observer-dependent; it does not vary with the observer's power level, institutional position, time horizon, or spatial scope. A powerless individual and an institutional forecasting agency both face the same prediction ceiling. The immediate horizon and the civilizational horizon both encounter the same mathematical boundary. A local weather forecast and a global climate model both confront sensitive dependence. This convergence is diagnostic of a true natural law constraint — the mathematical structure is indifferent to the observer's context. There is no perspectival gap in classification type, though there may be differences in how various agents experience or exploit the constraint in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY RISK. This constraint is a pure Mountain, with zero risk of misclassification as extractive or as pure coordination. The absence of beneficiaries and victims is appropriate and expected: there is no asymmetric benefit structure, no coercion mechanism, and no alternative pathway. The constraint does not create winners and losers through institutional design — it creates predictability ceilings that apply universally. Any attempt to classify this as Rope (pure coordination for prediction) or Snare (extraction through forecasting uncertainty) would be a category error. The constraint is structurally indifferent to institutional framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attractor_reconstruction_fidelity,
    'Is sensitive dependence a property of the true underlying attractor or an artifact of incomplete state-space reconstruction from time-series measurements?',
    'Compare Lyapunov exponent estimates across different embedding dimensions and time delays; validate with synthetic data from known chaotic systems; assess convergence of computed exponents as measurement resolution improves',
    'If true property: Mountain classification is confirmed across all domains. If artifact: The constraint might be Tangled Rope (measurement methodology + physical sensitivity) or even Rope (coordination problem in sensor networks). This distinction is critical for forecasting applications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attractor_reconstruction_fidelity, empirical, 'Whether sensitive dependence is intrinsic or measurement-dependent').

omega_variable(
    universality_class_stability,
    'Do sensitivity exponents (Lyapunov exponents) remain structurally invariant under small perturbations to system parameters, or does the exponent distribution change discontinuously across parameter ranges?',
    'Bifurcation analysis; computation of exponent sensitivity to parameter variations; identification of phase transitions where exponent signs flip or structure changes qualitatively',
    'If universally stable: Mountain classification confirmed (parameter-independent). If exponents flip at critical parameters: constraint becomes parameter-dependent (Scaffold or Tangled Rope in application domains where parameters can be controlled).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_class_stability, empirical, 'Stability of Lyapunov exponents under parameter perturbation').

omega_variable(
    observer_dependent_timescale_collapse,
    'Can enhanced observational capacity (faster sampling, finer resolution) push the prediction horizon past the Lyapunov timescale, or is the timescale itself a structural invariant independent of measurement quality?',
    'Test forecasting skill as a function of measurement resolution, sampling frequency, and model fidelity; identify whether the prediction ceiling is resolution-independent or resolution-dependent',
    'If resolution-independent: Mountain is confirmed (pure mathematical boundary). If resolution-dependent: constraint is Rope or Scaffold (coordination in measurement infrastructure can partially mitigate the problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_dependent_timescale_collapse, empirical, 'Whether prediction horizon depends on measurement resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strange_attractor_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sad_tr_t0, strange_attractor_dynamics, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sad_tr_t5, strange_attractor_dynamics, theater_ratio, 5, 0.08).
narrative_ontology:measurement(sad_tr_t10, strange_attractor_dynamics, theater_ratio, 10, 0.08).

% Extraction over time
narrative_ontology:measurement(sad_be_t0, strange_attractor_dynamics, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sad_be_t5, strange_attractor_dynamics, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(sad_be_t10, strange_attractor_dynamics, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strange_attractor_dynamics, information_standard).
narrative_ontology:affects_constraint(strange_attractor_dynamics, lyapunov_exponent_computation).
narrative_ontology:affects_constraint(strange_attractor_dynamics, fractal_dimension_measure).
narrative_ontology:affects_constraint(strange_attractor_dynamics, ergodic_decomposition).

% DUAL FORMULATION NOTE:
% Strange attractor dynamics is a foundational constraint that underlies more specific constraints in forecasting and control theory. The sensitive dependence property is empirically measured through Lyapunov exponents, fractal dimensionality, and ergodic properties — these are separate stories with their own extractiveness values, but they all depend on the mathematical structure of sensitive dependence itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
