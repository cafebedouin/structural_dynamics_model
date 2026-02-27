% ============================================================================
% CONSTRAINT STORY: lorenz_attractor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: mathematical_physics/deterministic_chaos
 *
 * SUMMARY:
 *   The Lorenz attractor represents a foundational constraint in
 *   deterministic chaos: small differences in initial conditions grow
 *   exponentially, rendering long-term prediction impossible despite the
 *   system being fully deterministic. This is not a technological limitation,
 *   measurement gap, or institutional failure — it is a structural property
 *   of nonlinear dynamics. The constraint operates universally across all
 *   observers, power levels, and time horizons. Weather prediction systems
 *   confront this limit daily: initial condition uncertainties of ~0.1%
 *   amplify exponentially with a Lyapunov exponent of ~0.9 bits per day,
 *   producing a predictability horizon of approximately 14 days regardless of
 *   computational investment. The Lorenz system exemplifies a pure Mountain
 *   constraint: high accessibility collapse (0.92), low resistance (0.08),
 *   minimal theater (0.15), and zero suppression (0.02). There are no
 *   beneficiaries or victims because the constraint applies uniformly. All
 *   perspectives converge on the same classification, making this a canonical
 *   example of a uniform-type constraint where the perspectival minimum is
 *   satisfied with fewer distinct types.
 *
 * KEY AGENTS:
 *   - Weather Prediction Systems: Powerless/trapped — structurally bound by the 14-day predictability horizon; cannot negotiate with Lyapunov exponents
 *   - Chaos Researchers: Moderate/analytical — understand the constraint as a fundamental property; recognize it as not negotiable but explainable
 *   - Meteorological Institutions: Institutional/arbitrage — have invested in observation networks; benefit from understanding chaos theory; cannot overcome the limit
 *   - Computational Infrastructure: Powerful/mobile — resources grow exponentially; yet cannot extend predictability horizon beyond ~14 days; demonstrates the problem is not computational
 *   - Analytical Observer: Analytical/analytical — sees the constraint as inevitable consequence of the Lorenz system's geometry; civilization-scale perspective
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
narrative_ontology:topic_domain(lorenz_attractor, "mathematical_physics/deterministic_chaos").

domain_priors:emerges_naturally(lorenz_attractor).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER PREDICTION (MOUNTAIN) — Weather forecasters are structurally trapped by the Lorenz attractor's exponential sensitivity. Initial condition uncertainties (measurement noise ~0.1%) grow exponentially, rendering forecasts beyond ~14 days fundamentally impossible, regardless of computational resources or algorithmic sophistication. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.11. The constraint is immutable: no exit option exists within classical atmospheric physics.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CHAOS RESEARCHER (MOUNTAIN) — Scientists studying deterministic chaos recognize sensitivity to initial conditions as a fundamental property of nonlinear dynamics, not an institutional constraint to be negotiated. The Lyapunov exponent λ≈0.9 bits/day is a physical property of the atmosphere, not a policy choice. d≈0.73, f(d)≈1.15, σ=1.0 → χ≈0.09. Even organized research communities cannot negotiate with mathematics.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, sensitivity to initial conditions in the Lorenz system is a consequence of deterministic nonlinearity itself. The system's basin of attraction contracts to a zero-volume attractor, while any two nearby trajectories separate exponentially. This is not a limitation of our technology or knowledge — it is a structural inevitability of the dynamical system. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL SCIENCE (MOUNTAIN) — Meteorological institutions and space agencies have invested heavily in initial condition measurement (weather stations, satellites, radar). These investments do not change the fundamental constraint — they only narrow the uncertainty cone from ~1% to ~0.1% of the attractor scale. The predictability horizon remains ~14 days. Institutions must accept this limit and structure forecasting accordingly (shorter-range prediction, ensemble uncertainty quantification). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Net beneficiary of basic research into chaos theory (improves understanding), but cannot negotiate with the constraint itself.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: TECHNOLOGICAL CAPACITY (MOUNTAIN) — Computational power and sensor networks have grown exponentially, yet weather forecasting beyond 14 days remains impossible. This is not because we lack resources — it is because the Lorenz attractor's geometry makes the problem fundamentally intractable. No amount of processing speed or measurement precision can overcome exponential divergence. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.06. Powerful actors experience the constraint as inevitable, not extractive.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

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
 *   Extractiveness (0.08): Extremely low. The Lorenz attractor does not extract value from any agent — it is a constraint on all agents equally. The small value (0.08 rather than 0.00) reflects that the constraint does involve measurement and modeling effort; the effort itself is not extraction but is necessary to understand the limit. No asymmetry exists: forecasters pay the cost; no one receives asymmetric benefit. Suppression (0.02): Minimal. The constraint does not suppress alternatives — rather, it makes all long-range deterministic forecasting alternatives equally impossible. There are no viable workarounds to suppress. Theater ratio (0.15): Low. The science of Lyapunov exponents and attractor geometry is well-established and empirically grounded. Weather forecasts honestly represent their uncertainty horizons (though operational practices vary in transparency). There is no meaningful performative component — the scientific understanding is robust and testable.
 *
 * PERSPECTIVAL GAP:
 *   Notably absent. All five perspectives converge on the Mountain classification. The weather prediction system sees it as an immutable limit on their forecasting ability. Researchers see it as a fundamental property of the Lorenz system. Institutional science accepts it as an inescapable boundary for forecasting investment. Technological infrastructure recognizes that computational power cannot overcome it. The analytical observer identifies it as a structural inevitability of deterministic nonlinearity. No agent perceives this as extraction, coordination, theater, or extractability. This uniform convergence is the hallmark of a true Mountain constraint — it appears as natural law from every vantage point.
 *
 * DIRECTIONALITY LOGIC:
 *   Direction not applicable. The Lorenz attractor has no beneficiaries or victims — it is a universal constraint that affects all agents symmetrically. No agent receives privileged benefit or suffers asymmetric extraction. The constraint is purely structural, not relational.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    global_vs_local_chaos,
    'Is the observed chaos in Earth''s atmosphere chaotic at global scales or only locally sensitive, with large-scale atmospheric circulations remaining somewhat predictable?',
    'Empirical analysis of multimodel ensemble forecasts; comparison of large-scale (500 hPa geopotential height) vs small-scale (surface station temperature) predictability horizons; investigation of whether some atmospheric modes (NAO, MJO, ENSO) exhibit longer predictability than the Lorenz scaling would suggest',
    'If only locally chaotic: predictability horizon could extend to 20-30 days for large-scale patterns. If globally chaotic: 14-day limit is fundamental even for global indices. Mountain classification survives either finding — the constraint remains immutable, but its scope varies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_vs_local_chaos, empirical, 'Whether atmospheric chaos is global or localized').

omega_variable(
    stochastic_vs_deterministic,
    'Is the observed unpredictability in weather a consequence of the Lorenz attractor''s deterministic sensitivity, or is it primarily due to unresolved stochastic subgrid processes (turbulence, convection) that genuinely introduce randomness?',
    'Comparison of deterministic chaos predictions with predictions that include parameterized stochasticity; analysis of whether skill improvement saturates with resolution increases (indicating deterministic chaos) or continues indefinitely (indicating stochastic sources)',
    'If deterministic: the Lorenz mountain applies as stated. If significantly stochastic: the constraint is partially a Mountain (quantum-mechanical/stochastic limit) but also partially a policy choice (how to parameterize subgrid processes). Classification remains Mountain, but the source shifts from nonlinearity to quantum mechanics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stochastic_vs_deterministic, empirical, 'Whether weather unpredictability is deterministic chaos or stochastic noise').

omega_variable(
    quantum_measurement_foundational,
    'Does the Lorenz attractor itself rest on a deeper quantum-mechanical indeterminacy, or is it purely a property of classical dynamics?',
    'Foundational physics analysis; investigation of whether quantum measurement uncertainty (Heisenberg limit) provides a hard floor on initial condition precision that makes the Lorenz constraint downstream of a more fundamental quantum mountain',
    'Either way, the constraint is a Mountain. If Lorenz is purely classical, the mountain emerges from deterministic nonlinearity. If Lorenz is downstream of quantum measurement, the mountain emerges from quantum mechanics. The classification does not change — the causal explanation does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_measurement_foundational, conceptual, 'Whether Lorenz rests on quantum-mechanical foundations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lorenz_tr_t0, lorenz_attractor, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lorenz_tr_t100, lorenz_attractor, theater_ratio, 100, 0.15).
narrative_ontology:measurement(lorenz_tr_t200, lorenz_attractor, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(lorenz_be_t0, lorenz_attractor, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(lorenz_be_t100, lorenz_attractor, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(lorenz_be_t200, lorenz_attractor, base_extractiveness, 200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lorenz_attractor, information_standard).
narrative_ontology:affects_constraint(lorenz_attractor, atmospheric_turbulence_cascade).
narrative_ontology:affects_constraint(lorenz_attractor, weather_model_resolution_ceiling).

% DUAL FORMULATION NOTE:
% The Lorenz attractor is the upstream constraint in a family of weather-related constraints. Atmospheric turbulence cascade and weather model resolution ceiling are downstream manifestations or related constraints operating within the bounds set by Lorenz sensitivity. The Lorenz constraint is invariant — the downstream constraints negotiate how to operate within its limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
