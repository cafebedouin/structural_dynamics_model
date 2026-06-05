% ============================================================================
% CONSTRAINT STORY: strange_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Strange Attractor Dynamics (Sensitive Dependence)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   Strange attractor dynamics represent a mathematical/physical constraint
 *   arising from the topological structure of nonlinear dynamical systems.
 *   Sensitive dependence on initial conditions — the core property of chaos —
 *   emerges necessarily from the attractor's geometry: nearby trajectories
 *   diverge exponentially at a rate governed by the positive Lyapunov
 *   exponent. This constraint is universal across all dynamical systems
 *   exhibiting chaos (the Lorenz system, logistic map, double pendulum, fluid
 *   turbulence). It is not imposed by an external agent, negotiated through
 *   coordination, or maintained through enforcement. Rather, it emerges from
 *   the mathematical structure of the system itself. The constraint defines
 *   what is achievable: prediction horizons in chaotic systems are
 *   fundamentally bounded by the rate of trajectory divergence, independent
 *   of computational power or measurement precision. This is not a Rope
 *   (coordination mechanism), Snare (extraction), Scaffold (temporary), or
 *   Piton (degraded). It is a Mountain — a structural limit inherent to the
 *   system's mathematical nature.
 *
 * KEY AGENTS:
 *   - Embedded Observer: Any agent measuring or predicting within the chaotic system (powerless/trapped) — confronts exponential divergence as an absolute barrier to long-term prediction
 *   - External Analyst: Mathematical or computational observer (analytical/analytical) — recognizes sensitive dependence as a topological property of the attractor, not an imposed constraint
 *   - Systems Modeler: Engineer or climate scientist working with chaotic systems (institutional/analytical) — must acknowledge the Lyapunov barrier and design ensemble or probabilistic approaches rather than attempting to overcome it
 *   - Dynamical System Itself: The mathematical structure generating the attractor (universal scope) — is the source of the constraint, not a participant in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractor_dynamics, 0.12).
domain_priors:suppression_score(strange_attractor_dynamics, 0.02).
domain_priors:theater_ratio(strange_attractor_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, extractiveness, 0.12).
narrative_ontology:constraint_metric(strange_attractor_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(strange_attractor_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(strange_attractor_dynamics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strange_attractor_dynamics, mountain).
narrative_ontology:human_readable(strange_attractor_dynamics, "Strange Attractor Dynamics (Sensitive Dependence)").
narrative_ontology:topic_domain(strange_attractor_dynamics, "mathematical/physical").

domain_priors:emerges_naturally(strange_attractor_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Any observer embedded in the dynamical system with finite measurement precision confronts sensitive dependence as an absolute barrier: exponential divergence of nearby trajectories cannot be overcome by instrument design or computational power within the system's phase space. d≈1.00, f(d)≈1.42. The constraint is inescapable from within.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of mathematical analysis, sensitive dependence on initial conditions is a topological property of strange attractors: it follows necessarily from the nonlinear feedback structure and the absence of dissipative coupling that would synchronize nearby trajectories. This is not a limitation imposed by an external agent but a feature of the dynamical geometry itself. d≈0.50, f(d)≈0.65. Classified as mountain because the constraint emerges from the mathematical structure, not from extraction or coordination mechanics.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The systems engineer or climate modeler encounters sensitive dependence as a structural inevitability: no amount of computational investment or methodological refinement eliminates the constraint. Ensemble forecasting and probabilistic approaches acknowledge rather than circumvent the barrier. d≈0.60, f(d)≈0.75. Mountain because the constraint is inherent to the system's mathematical structure, not constructed by policy or institutional choice.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.12): Very low. No agent extracts from another; the constraint is a mathematical property, not a redistribution mechanism. The low value reflects that this is a structural limit (accessibility collapse ≥ 0.85 → low ε), not a social or institutional artifact. Suppression (0.02): Negligible. The constraint does not suppress alternatives or coerce compliance — it operates at the level of mathematical possibility. Alternatives to prediction within the prediction horizon are not suppressed by the constraint; they simply don't exist within the physics of the system. Theater ratio (0.05): Minimal. The constraint exhibits no performative content. Claims of sensitivity to initial conditions can be verified directly through numerical computation; the Lyapunov exponent is a measurable, reproducible quantity. There is no gap between functional and theatrical activity.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives converge on the Mountain classification. The embedded observer, external analyst, and systems modeler all recognize sensitive dependence as an inherent structural property. There is no perspectival gap because the constraint does not arise from institutional choice, beneficiary-victim dynamics, or coordination failures. This is characteristic of a true natural law constraint: it appears the same from all observationally accessible positions. The stability of the classification across perspectives confirms the validity of the mountain claim — if perspectives diverged significantly, it would indicate that one perspective was observing a different constraint (e.g., model error, measurement artifact) rather than the true sensitive dependence property.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint exhibits no directionality in the usual sense of beneficiary/victim dynamics. Directionality derivation is not applicable because there is no agent extraction occurring. The embedded observer's high d (≈1.00) reflects not victimhood but epistemic limitation — the constraint limits what the observer can know, not what they can control or extract. The analytical observer's symmetric d (≈0.50) reflects true symmetry: the mathematical structure is neither extracting from nor subsidizing the observer; they occupy a symmetric epistemic relationship to the constraint. No directionality overrides are needed because the structural data (ε=0.12, suppression=0.02) unambiguously establishes that this is not an extraction mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lyapunov_exponent_measurement,
    'Is the positive Lyapunov exponent a fundamental property of the attractor or a measurement artifact dependent on the choice of metric and time scale?',
    'Computation of Lyapunov spectra across different coordinate systems and time-delay embeddings; analysis of whether exponent values depend on observation method or reflect intrinsic dynamical property',
    'If intrinsic: sensitive dependence is a true mathematical/physical constraint (Mountain confirmed). If measurement-dependent: the constraint may degrade to a weaker observational limitation (Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lyapunov_exponent_measurement, empirical, 'Whether positive Lyapunov exponents reflect intrinsic dynamics or measurement choice').

omega_variable(
    attractor_basin_structure,
    'Does the strange attractor basin of attraction occupy a bounded region of phase space, or does sensitive dependence imply unbounded growth of trajectory divergence that would escape any bounded domain?',
    'Analysis of the basin of attraction boundaries; computation of maximum divergence rates and comparison to phase space geometry; determination of whether initial condition sensitivity is confined to the attractor or propagates to unbounded regimes',
    'If confined: constraint is a local property of the attractor''s topology (Mountain with high accessibility_collapse). If unbounded: sensitive dependence extends to global dynamics and becomes a system-wide property affecting all initial conditions (affects classification of parent constraints like weather_predictability_limit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attractor_basin_structure, empirical, 'Whether sensitive dependence is confined to the attractor basin or extends globally').

omega_variable(
    noise_amplification_threshold,
    'Below what noise amplitude does the system recover deterministic predictability? Is there a true noise floor below which trajectories synchronize despite initial condition mismatch?',
    'Stochastic perturbation analysis; measurement of synchronization thresholds in systems with sub-attractor-scale noise; determination of whether noise-induced averaging can suppress sensitive dependence',
    'If noise always amplifies divergence: constraint is absolute (Mountain). If noise below a threshold allows recovery of order: constraint is conditional on the noise regime (degrades to Tangled Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noise_amplification_threshold, empirical, 'Whether noise amplification of sensitive dependence can be suppressed below a threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strange_attractor_dynamics, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sad_tr_t0, strange_attractor_dynamics, theater_ratio, 0, 0.03).
narrative_ontology:measurement(sad_tr_t50, strange_attractor_dynamics, theater_ratio, 50, 0.04).
narrative_ontology:measurement(sad_tr_t100, strange_attractor_dynamics, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(sad_be_t0, strange_attractor_dynamics, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sad_be_t50, strange_attractor_dynamics, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(sad_be_t100, strange_attractor_dynamics, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strange_attractor_dynamics, information_standard).
narrative_ontology:affects_constraint(strange_attractor_dynamics, weather_predictability_limit).
narrative_ontology:affects_constraint(strange_attractor_dynamics, butterfly_effect_practical_boundary).
narrative_ontology:affects_constraint(strange_attractor_dynamics, ergodic_sampling_convergence).

% DUAL FORMULATION NOTE:
% Strange attractor dynamics is the foundational constraint from which several downstream constraints (weather predictability, practical butterfly effect) derive their structure. Weather predictability is constrained by sensitive dependence; the practical butterfly effect is the manifestation of sensitive dependence in real-world systems; ergodic sampling convergence involves trade-offs with trajectory divergence. All three downstream constraints are high-extraction or mixed precisely because they layer human institutional and policy dimensions onto this underlying mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
