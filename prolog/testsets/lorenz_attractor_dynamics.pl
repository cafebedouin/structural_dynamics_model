% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lorenz_attractor_dynamics, []).

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
 *   constraint_id: lorenz_attractor_dynamics
 *   human_readable: Lorenz Attractor (Deterministic Chaos)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   The Lorenz attractor is a canonical example of deterministic chaos — a
 *   system with no stochastic elements that nonetheless exhibits long-term
 *   unpredictability. The constraint it imposes is not institutional, not
 *   extractive, not suppressible by will or resources: trajectories with
 *   slightly different initial conditions diverge exponentially. This
 *   constraint classifies as Mountain from every observer perspective because
 *   it reflects an intrinsic mathematical property of the nonlinear
 *   differential equations, not a contingent social arrangement or policy
 *   choice. The system was derived by Edward Lorenz in 1963 as a simplified
 *   model of atmospheric convection (Rayleigh-Bénard instability), making it
 *   both a pure mathematical object and a physical phenomenon. The
 *   extractiveness (0.08) is near zero because there is no asymmetric
 *   extraction from any agent — the constraint simply bounds what is knowable
 *   about the system's future state. The suppression (0.02) is minimal
 *   because there is no coercion or silencing involved. The theater ratio
 *   (0.05) is trivial because the attractor is not performative — it is pure
 *   function with no surplus ritualistic activity.
 *
 * KEY AGENTS:
 *   - Weather Forecasters: Analytical agents (analytical/trapped) — cannot extend forecast horizon beyond 2 weeks despite unlimited resources because of the attractor's exponential divergence
 *   - Meteorological Services: Institutional observer (institutional/analytical) — manages forecasting operations within the constraint's structural limits; cannot change the limits themselves
 *   - Pure Mathematicians: Analytical researchers (analytical/analytical) — study the attractor as an abstract object independent of physical instantiation; see the constraint as a necessity of the formal system
 *   - Computational Researchers: Powerful agents (powerful/analytical) — can simulate the system with high precision but cannot bypass the fundamental divergence rate
 *   - Physicists: Interdisciplinary observers (analytical/analytical) — interpret the attractor as both a mathematical object and a model of physical reality; responsible for assessing fidelity to actual fluid dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor_dynamics, 0.08).
domain_priors:suppression_score(lorenz_attractor_dynamics, 0.02).
domain_priors:theater_ratio(lorenz_attractor_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, 0.08).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lorenz_attractor_dynamics, mountain).
narrative_ontology:human_readable(lorenz_attractor_dynamics, "Lorenz Attractor (Deterministic Chaos)").
narrative_ontology:topic_domain(lorenz_attractor_dynamics, "mathematical/physical").

domain_priors:emerges_naturally(lorenz_attractor_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER FORECASTER (MOUNTAIN) — Cannot escape the dynamical constraint that small perturbations in atmospheric state diverge exponentially. This is not a policy choice or institutional arrangement; it is an intrinsic property of the nonlinear equations governing fluid dynamics. The forecaster's inability to predict beyond ~2 weeks is not extractive or suppressible — it is a structural limit of the physical system itself.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: METEOROLOGICAL SERVICE (MOUNTAIN) — Even with institutional resources and advanced computing, the attractor's sensitivity to initial conditions imposes an absolute ceiling on forecast horizon. This is not a resource allocation problem, not an enforcement mechanism, not extractive — it is a mathematical property of the Lorenz system. Increased funding, computing power, or better measurement cannot overcome the fundamental divergence rate encoded in the dynamical equations.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PURE MATHEMATICIAN (MOUNTAIN) — The Lorenz attractor is a rigorous mathematical object with invariant properties: fractal dimension, Lyapunov exponent, sensitivity to initial conditions. These properties hold regardless of interpretation or measurement basis. The attractor's geometry constrains all trajectories in its basin; no observer can escape this constraint through re-framing or alternative methodology. This is a natural law in the strongest sense.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: COMPUTATIONAL RESEARCHER (MOUNTAIN) — Even powerful agents with access to exascale computing face the constraint that the Lorenz system's divergence is exponential in initial condition precision. Doubling computational power extends prediction horizon by only a few time units. The constraint is not bypassed by resources or intelligence — it is intrinsic to the differential equations.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(lorenz_attractor_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lorenz_attractor_dynamics),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lorenz_attractor_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimum. The Lorenz attractor does not extract from any agent — it constrains knowledge rather than extracting resources. No one is enriched by the system's unpredictability; everyone is equally subject to the prediction limit. Suppression (0.02): Negligible. There is no coercion, no alternative pathways being blocked, no resistance mechanism being overcome. The attractor simply is. Theater ratio (0.05): Negligible. The mathematical and physical descriptions of the Lorenz system contain no performative surplus. Every statement about the attractor's properties (Lyapunov exponent, fractal dimension, sensitivity to initial conditions) directly describes functional behavior rather than masking degraded function.
 *
 * PERSPECTIVAL GAP:
 *   Surprisingly, there is no perspectival gap. All observer positions (powerless forecaster, institutional service, analytical mathematician, powerful researcher) classify the Lorenz attractor as Mountain. This uniformity is a signature of true natural law: the constraint is invariant across all observables and contexts. A forecaster's inability to predict beyond 2 weeks is not a different phenomenon from a mathematician's proof that the Lyapunov exponent is positive — they are the same structural property seen from different scales and purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply. Mountain constraints have zero beneficiaries and zero victims because they are not extraction mechanisms. There is no agent who benefits from the attractor's unpredictability, and there is no agent who bears uniquely high costs. The constraint is universal and symmetrical.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY. The Lorenz attractor exemplifies the pure mountain case: all six perspectives collapse into a single classification. There is no hidden coordination function misidentified as extraction, no asymmetric burden masked as universal law. The mountain classification is not a false positive — the constraint genuinely emerges naturally, has accessibility collapse ≥0.85, resistance ≤0.15, and extractiveness ≤0.25. This is the gold standard for natural law classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attractor_robustness_perturbation,
    'Does the Lorenz attractor''s topological structure persist under small perturbations to the coefficient parameters (σ, ρ, β)?',
    'Bifurcation analysis; parameter space exploration; detection of topological transitions in attractor geometry as parameters vary smoothly',
    'If robust: the attractor is a genuine structural invariant of the system. If sensitive: the attractor may be an artifact of the specific parameter choice, weakening mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attractor_robustness_perturbation, empirical, 'Structural persistence of the attractor under parameter perturbation').

omega_variable(
    physical_lorenz_correspondence,
    'Does the idealized Lorenz system accurately model the actual nonlinear dynamics of atmospheric convection, or is it a highly simplified toy model that obscures real-world complexity?',
    'Comparison of Lorenz system trajectories with experimental fluid dynamics data from Rayleigh-Bénard convection; quantification of parameter fidelity; analysis of neglected nonlinear terms in the original derivation',
    'If highly faithful: the attractor''s constraints directly constrain weather prediction. If primarily pedagogical: the mountain classification applies to the mathematics but not necessarily to meteorology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_lorenz_correspondence, empirical, 'Fidelity of the Lorenz model to actual atmospheric convection').

omega_variable(
    lyapunov_exponent_measurability,
    'Can the Lyapunov exponent be measured or computed with sufficient precision to confirm the exponential divergence claim in finite time?',
    'Numerical integration with high-precision arithmetic; limit-cycle analysis; comparison of divergence rates across different integration methods and step sizes',
    'If measurable with high confidence: the exponential divergence is well-established. If subject to significant numerical artifacts: the mountain claim depends on perfect mathematical formalism, not empirical evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lyapunov_exponent_measurability, empirical, 'Measurability of the Lyapunov exponent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor_dynamics, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lorenz_tr_t0, lorenz_attractor_dynamics, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lorenz_tr_t25, lorenz_attractor_dynamics, theater_ratio, 25, 0.05).
narrative_ontology:measurement(lorenz_tr_t50, lorenz_attractor_dynamics, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(lorenz_be_t0, lorenz_attractor_dynamics, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(lorenz_be_t25, lorenz_attractor_dynamics, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(lorenz_be_t50, lorenz_attractor_dynamics, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lorenz_attractor_dynamics, information_standard).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, butterfly_effect_sensitivity).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, weather_predictability_limit).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, phase_space_dimension_constraint).

% DUAL FORMULATION NOTE:
% The Lorenz attractor is foundational to deterministic chaos theory. Other constraints in the chaos family (butterfly effect sensitivity, weather predictability limits) inherit the attractor's mathematical properties but frame them differently: the attractor is the underlying structure; the sensitivity and limit constraints are observable consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
