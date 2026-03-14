% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_chaos
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lorenz_attractor_chaos, []).

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
 *   constraint_id: lorenz_attractor_chaos
 *   human_readable: Lorenz Attractor Chaos and Sensitive Dependence on Initial Conditions
 *   domain: dynamical_systems/mathematical_physics
 *
 * SUMMARY:
 *   The Lorenz attractor represents a fundamental constraint on
 *   predictability in dynamical systems: sensitive dependence on initial
 *   conditions. The constraint emerges from the deterministic structure of
 *   three coupled differential equations and manifests as exponential
 *   divergence of nearby trajectories. No amount of computational resources,
 *   measurement precision improvement, or institutional coordination can
 *   overcome this mathematical barrier. The Lorenz constraint is invariant
 *   across all observational frames, measurement methodologies, and time
 *   horizons—it is a property of the dynamics itself, not of our instruments
 *   or institutions. This makes it a canonical example of a mountain-type
 *   constraint: immutable, universal, and without beneficiaries or victims,
 *   because it describes a structural feature of reality rather than an
 *   asymmetric extraction mechanism.
 *
 * KEY AGENTS:
 *   - Weather Prediction System: Powerless agent (trapped, civilizational scope) — cannot escape the fundamental limit; bears the structural barrier fully
 *   - Meteorological Institution: Institutional actor (arbitrage, generational scope) — cannot negotiate with mathematics; resources accumulate but do not overcome the barrier
 *   - Forecast Community: Organized agents (constrained, biographical scope) — coordinate on ensemble methods and data assimilation, but these are adaptations to the constraint, not escapes from it
 *   - Analytical Observer: Universal perspective (analytical, civilizational scope) — witnesses the constraint as a mathematical property independent of observer or measurement choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor_chaos, 0.12).
domain_priors:suppression_score(lorenz_attractor_chaos, 0.03).
domain_priors:theater_ratio(lorenz_attractor_chaos, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor_chaos, extractiveness, 0.12).
narrative_ontology:constraint_metric(lorenz_attractor_chaos, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(lorenz_attractor_chaos, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lorenz_attractor_chaos, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lorenz_attractor_chaos, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lorenz_attractor_chaos, mountain).
narrative_ontology:human_readable(lorenz_attractor_chaos, "Lorenz Attractor Chaos and Sensitive Dependence on Initial Conditions").
narrative_ontology:topic_domain(lorenz_attractor_chaos, "dynamical_systems/mathematical_physics").

domain_priors:emerges_naturally(lorenz_attractor_chaos).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER PREDICTION SYSTEM (MOUNTAIN) — Cannot escape the fundamental limit of exponential divergence in chaotic systems. Sensitive dependence on initial conditions is an immutable property of the Lorenz equations themselves. All predictors at all scales confront the same structural barrier. Maximum perceived immutability across all time horizons.
constraint_indexing:constraint_classification(lorenz_attractor_chaos, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The Lorenz attractor's chaotic dynamics emerge from the deterministic structure of the three-equation system itself. The constraint is mathematical, not institutional. No reformulation of measurement, prediction methodology, or computational approach can eliminate the exponential growth of perturbations. The barrier is universal and invariant.
constraint_indexing:constraint_classification(lorenz_attractor_chaos, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: METEOROLOGICAL INSTITUTION (MOUNTAIN) — Even with institutional resources, computational power, and observational networks, the fundamental limit persists. Sensitivity to initial conditions cannot be negotiated with. The constraint remains invariant across institutional timescales and resources. This is not a problem that scales down with better funding.
constraint_indexing:constraint_classification(lorenz_attractor_chaos, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: FORECAST COMMUNITY (MOUNTAIN) — Organized meteorologists confront the same mathematics. Cooperation among forecast centers, data sharing, ensemble methods—all are responses to the constraint, not escapes from it. The constraint defines the problem space within which coordination operates. Immutable across the biographical timescale relevant to career planning.
constraint_indexing:constraint_classification(lorenz_attractor_chaos, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_chaos_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lorenz_attractor_chaos, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor_chaos, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lorenz_attractor_chaos, ExtMetricName, E),
    domain_priors:suppression_score(lorenz_attractor_chaos, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lorenz_attractor_chaos),
    narrative_ontology:constraint_metric(lorenz_attractor_chaos, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lorenz_attractor_chaos, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lorenz_attractor_chaos_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Extremely low. The Lorenz constraint does not extract from any agent or benefit any other agent—it is a neutral property of the dynamical system. The 0.12 value reflects the minimal overhead in detecting and characterizing the constraint itself (computational cost to verify the existence of the attractor), not any asymmetric extraction flow. Suppression (0.03): Minimal. There are no alternatives to suppress; the constraint is not enforced against resistance but simply exists as a mathematical fact. The 0.03 reflects only the trivial cost of acknowledging the constraint. Theater ratio (0.15): Very low. The Lorenz attractor's chaotic dynamics are straightforwardly described by the mathematics; there is no performative layer. The 0.15 reflects only the unavoidable gap between the abstract mathematical description and specific empirical instantiations (numerical simulations have finite precision; real atmospheric dynamics include additional terms not in the Lorenz model). All metrics remain constant across the measurement interval because the underlying mathematical property does not change over time—this is the hallmark of a true mountain.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is NOT a disagreement about classification—all perspectives see a mountain because the constraint is truly universal. Instead, the gap reflects different understandings of what the constraint means and what actions are appropriate in response. The weather prediction system perceives the barrier as an unresolvable limit on forecast horizon. The meteorological institution perceives it as a boundary condition that determines institutional strategy (ensemble forecasting, probabilistic prediction, focus on shorter timescales with more confidence). The forecast community perceives it as a problem to work around through methodology (assimilation, ensemble aggregation, statistical post-processing). The analytical observer perceives it as a mathematical necessity. All four perspectives agree on the classification (mountain) but differ in their pragmatic responses.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain constraints without asymmetric extraction. The Lorenz attractor has no beneficiaries and no victims. The constraint does not favor one agent over another—it applies equally to all agents attempting to predict chaotic systems. The 'powerless' agent (weather prediction) and the 'institutional' agent (meteorological institution) experience the same mathematical barrier. This is the defining characteristic of a mountain: universality without asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lorenz attractor resolves the mandatrophy trivially: all perspectives classify it as a mountain, which is correct. There is no risk of mislabeling coordination as extraction or vice versa because the constraint has neither coordination nor extraction structure—it is a property of the state space itself. The mandatrophy framework does not apply to constraints without beneficiaries or victims. The Lorenz attractor serves as a control case: a constraint where the indexical classification is truly invariant across all (P, T, E, S) tuples because the constraint is universal and acausal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chaos_vs_insufficient_observation,
    'Is the loss of predictability due to inherent chaos in the Lorenz system or to practical insufficiency of initial condition measurement?',
    'Theoretical analysis of Lyapunov exponents and the phase-space geometry of the attractor; empirical validation that perturbations grow exponentially regardless of measurement precision improvements',
    'If chaos is inherent: mountain classification confirmed universally. If insufficiency dominates: the constraint is practical (resource-based) rather than mathematical, potentially reclassifying to scaffold under resource improvements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaos_vs_insufficient_observation, empirical, 'Whether unpredictability is intrinsic chaos or measurement insufficiency').

omega_variable(
    attractor_stability_under_perturbation,
    'Does the Lorenz attractor''s structure remain stable when the system parameters (ρ, σ, β) are slightly perturbed, or does the attractor disappear under small parameter variation?',
    'Bifurcation analysis of the Lorenz system; examination of how the chaotic regime persists or dissolves as parameters vary; identification of critical boundaries between chaos and periodic behavior',
    'If structure is robust: the constraint is truly universal and parameter-independent. If sensitive to parameter choice: the mountain classification depends on the specific regime, narrowing the universality claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attractor_stability_under_perturbation, empirical, 'Robustness of Lorenz chaos to parameter perturbation').

omega_variable(
    quantum_transition_regime,
    'In quantum systems that transition from classical Lorenz-like dynamics to quantum behavior, does quantum mechanics escape or modify the classical constraint?',
    'Analysis of quantum kicked rotor or quantum kicked top; measurement of decoherence rates and localization vs spreading in quantum phase space; comparison of classical and quantum Lyapunov exponents',
    'If quantum mechanics truly escapes: the constraint is a feature of classical dynamics only, narrowing its universality to classical domains. If quantum exhibits analogous constraint: the mountain extends across the quantum-classical boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_transition_regime, empirical, 'Whether quantum mechanics escapes Lorenz-type constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor_chaos, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lorenz_tr_t0, lorenz_attractor_chaos, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lorenz_tr_t5, lorenz_attractor_chaos, theater_ratio, 5, 0.15).
narrative_ontology:measurement(lorenz_tr_t10, lorenz_attractor_chaos, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(lorenz_be_t0, lorenz_attractor_chaos, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(lorenz_be_t5, lorenz_attractor_chaos, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(lorenz_be_t10, lorenz_attractor_chaos, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lorenz_attractor_chaos, information_standard).
narrative_ontology:affects_constraint(lorenz_attractor_chaos, butterfly_effect_epistemic_limit).
narrative_ontology:affects_constraint(lorenz_attractor_chaos, weather_predictability_horizon).

% DUAL FORMULATION NOTE:
% The Lorenz attractor is a foundational mathematical object that influences downstream constraints in predictive science. Weather predictability horizon is directly constrained by Lorenz dynamics; butterfly effect is a narrative interpretation of sensitive dependence on initial conditions. Both downstream constraints inherit the mountain property from the Lorenz system itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
