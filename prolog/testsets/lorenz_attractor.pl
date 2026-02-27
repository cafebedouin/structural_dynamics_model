% ============================================================================
% CONSTRAINT STORY: lorenz_attractor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lorenz_attractor, []).

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
 *   constraint_id: lorenz_attractor
 *   human_readable: Sensitivity to Initial Conditions (Lorenz Attractor)
 *   domain: technological/dynamical_systems
 *
 * SUMMARY:
 *   The Lorenz Attractor exemplifies a mountain-class constraint: a
 *   fundamental property of deterministic nonlinear dynamical systems that
 *   cannot be eliminated, negotiated, or circumvented through any practical
 *   intervention. Edward Lorenz's 1963 discovery of sensitivity to initial
 *   conditions in a simplified model of atmospheric convection revealed that
 *   tiny variations in initial state (a change from 0.506127 to 0.506)
 *   produce wildly divergent trajectories after a brief time horizon. This
 *   sensitivity is not a function of measurement error, computational
 *   precision, or modeling fidelity — it is an intrinsic property of the
 *   equations themselves. The Lorenz system is deterministic (fully specified
 *   by its three ODEs and initial conditions) yet practically unpredictable
 *   beyond a finite time horizon. This appears paradoxical only from a
 *   pre-chaos perspective. The constraint is: any agent attempting long-term
 *   deterministic prediction of Lorenz dynamics must accept exponential
 *   amplification of uncertainty in initial conditions. No alternative
 *   formulation, no additional data, no algorithmic advance can eliminate
 *   this fundamental limit. The system is governed by the three-dimensional
 *   strange attractor with a positive Lyapunov exponent — a mathematical
 *   invariant.
 *
 * KEY AGENTS:
 *   - Weather Prediction System: Primary constrained agent (moderate/constrained) — must predict atmospheric dynamics that obey Lorenz-like sensitivities; cannot overcome the fundamental barrier
 *   - Atmospheric Scientist: Analytical observer (institutional/analytical) — understands the constraint mathematically; uses ensemble and probabilistic methods to work within it
 *   - Mathematical Theory: The constraint enforcer (analytical/analytical) — the structure of nonlinear ODEs produces the attractor; no observer-relative modification changes this
 *   - Computational Infrastructure: Secondary agent (institutional/arbitrage) — benefits from developing methods (ensemble forecasting, chaos theory) that respect rather than fight the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor, 0.08).
domain_priors:suppression_score(lorenz_attractor, 0.02).
domain_priors:theater_ratio(lorenz_attractor, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor, extractiveness, 0.08).
narrative_ontology:constraint_metric(lorenz_attractor, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lorenz_attractor, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lorenz_attractor, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lorenz_attractor, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lorenz_attractor, mountain).
narrative_ontology:human_readable(lorenz_attractor, "Sensitivity to Initial Conditions (Lorenz Attractor)").
narrative_ontology:topic_domain(lorenz_attractor, "technological/dynamical_systems").

domain_priors:emerges_naturally(lorenz_attractor).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER PREDICTOR (MOUNTAIN) — No agent can escape sensitivity to initial conditions in Lorenz dynamics. Arbitrarily small errors in atmospheric measurement amplify exponentially, rendering forecasts beyond ~2 weeks impossible regardless of computational power or observation density. This is not a choice or a policy constraint; it is a structural property of the equations themselves. Zero degrees of freedom.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ATMOSPHERIC SCIENTIST (MOUNTAIN) — Despite decades of improved instrumentation, satellite networks, and ensemble forecasting methods, the fundamental sensitivity barrier remains. The scientist can manage the constraint (ensemble methods, probabilistic forecasts) but cannot eliminate it. The constraint is invariant across all measurement technologies and all algorithmic approaches.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of dynamical systems theory, sensitivity to initial conditions is a mathematical invariant of the Lorenz equations. The three coupled nonlinear ODEs produce a strange attractor where nearby trajectories diverge exponentially in time. This property holds across all initial conditions within the attractor's basin, across all parameter regimes where chaos emerges, and across all observation contexts. It is a feature of the mathematical structure itself, not a feature of how we measure or model it.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: LYAPUNOV EXPONENT (MOUNTAIN) — The positive Lyapunov exponent λ ≈ 0.906 is a mathematical invariant of the standard Lorenz system (σ=10, ρ=28, β=8/3). Small perturbations grow as e^(λt), quantifying the rate of divergence. This is not an empirical measurement subject to context-dependence; it is a computed property of the deterministic system. All observables access the same underlying dynamical structure.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lorenz_attractor, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lorenz_attractor, ExtMetricName, E),
    domain_priors:suppression_score(lorenz_attractor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lorenz_attractor),
    narrative_ontology:constraint_metric(lorenz_attractor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lorenz_attractor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lorenz_attractor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The Lorenz constraint does not extract value from any agent or redirect it to a beneficiary. It is a barrier to a goal (long-term deterministic prediction) but creates no redistribution of resources. No agent benefits structurally from the existence of sensitivity to initial conditions. Suppression (0.02): Near-zero. The constraint does not suppress alternatives through coercion or institutional design. It suppresses certain mathematical possibilities (deterministic prediction beyond ~2 weeks) through logical necessity, not through human enforcement. Theater ratio (0.15): Very low. The Lorenz equations are not performative. They compute the same dynamics regardless of whether anyone observes them. Ensemble forecasting methods developed to manage the constraint have some communication overhead (explaining probabilistic forecasts to stakeholders), but the underlying mathematical structure has zero theater.
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival gap. All four perspectives converge on mountain classification because the constraint's essential property — positive Lyapunov exponent and exponential trajectory divergence — is invariant across observational contexts and agent positions. The weather predictor experiences the constraint as an immutable barrier. The scientist understands it as a mathematical invariant. The analytical observer sees the strange attractor structure. The Lyapunov exponent perspective quantifies the rate of divergence. All arrive at the same conclusion: this is a natural law, not a contingent institutional arrangement. The minimal gap (all four perspectives produce Mountain) is the defining feature of mountain-only constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY RISK. The Lorenz constraint exhibits pure mountain structure with no hybrid or extractive properties. The accessibility_collapse (0.92) and resistance (0.08) confirm that agents cannot feasibly work around the sensitivity barrier through alternative measurement, alternative models, or alternative observation strategies. The suppression (0.02) reflects logical necessity, not coercive institutional design. The theater_ratio (0.15) indicates minimal performative content — ensemble forecasting methods have some communication overhead but the underlying dynamics are not staged. The constraint does not benefit any agent, harm any particular group (it harms the goal of long-term deterministic prediction, not any person or institution), or exist through institutional maintenance. It is pure mathematics with zero degrees of freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_apparatus_precision,
    'Does finite measurement precision constitute an intrinsic constraint on Lorenz systems, or is it a separate practical limitation?',
    'Theoretical analysis of quantum measurement limits on atmospheric data; distinction between algorithmic chaos (sensitivity in computation) and physical chaos (sensitivity in nature)',
    'If intrinsic: the Lorenz mountain has a quantum substrate, and sensitivity is deeper than classical mechanics. If separate: classical sensitivity is pure mathematics, independent of measurement capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_apparatus_precision, conceptual, 'Whether measurement precision is intrinsic to Lorenz dynamics or a practical limitation').

omega_variable(
    parameter_dependence_universality,
    'Does sensitivity to initial conditions hold universally across all parameter regimes, or only in the chaotic region?',
    'Bifurcation analysis sweeping (σ, ρ, β) parameter space; identification of periodic and fixed-point regimes where sensitivity disappears',
    'If universal: sensitivity is truly invariant and mountain-classified. If regime-dependent: constraint is conditional on parameter values, weakening mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parameter_dependence_universality, empirical, 'Whether sensitivity to initial conditions is universal across Lorenz parameter space').

omega_variable(
    strange_attractor_stability,
    'Is the existence and structure of the Lorenz strange attractor itself stable under small perturbations to the equations, or does it dissolve under sufficiently small structural changes?',
    'Structural stability analysis; study of unfoldings of the Lorenz system under parameter perturbation; numerical continuation of attractor structure',
    'If structurally stable: the Lorenz mountain is robust to model variation. If structurally unstable: the mountain is more fragile than it appears, contingent on precise equation form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strange_attractor_stability, empirical, 'Structural stability of the Lorenz strange attractor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lorenz_tr_t0, lorenz_attractor, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lorenz_tr_t50, lorenz_attractor, theater_ratio, 50, 0.15).
narrative_ontology:measurement(lorenz_tr_t100, lorenz_attractor, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(lorenz_be_t0, lorenz_attractor, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(lorenz_be_t50, lorenz_attractor, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(lorenz_be_t100, lorenz_attractor, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lorenz_attractor, information_standard).
narrative_ontology:affects_constraint(lorenz_attractor, weather_predictability_horizon).
narrative_ontology:affects_constraint(lorenz_attractor, climate_model_initialization).
narrative_ontology:affects_constraint(lorenz_attractor, chaos_theory_foundation).

% DUAL FORMULATION NOTE:
% Sensitivity to initial conditions in the Lorenz system is a single, indivisible mountain constraint. No decomposition is needed because epsilon is invariant across all measurement methodologies. The constraint's mathematical definition (positive Lyapunov exponent in the strange attractor) does not change when examined from different contexts. Related constraints (weather predictability horizon, climate initialization) are downstream consequences that inherit the same fundamental barrier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
