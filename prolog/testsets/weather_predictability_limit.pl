% ============================================================================
% CONSTRAINT STORY: weather_predictability_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weather_predictability_limit, []).

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
 *   constraint_id: weather_predictability_limit
 *   human_readable: Weather Predictability Limit (Chaos and Lyapunov Exponents)
 *   domain: atmospheric_physics/dynamical_systems
 *
 * SUMMARY:
 *   The weather predictability limit represents a fundamental constraint on
 *   atmospheric forecasting arising from nonlinear dynamics and initial
 *   condition sensitivity (Lyapunov chaos). The practical prediction horizon
 *   for mesoscale atmospheric features is approximately 2 weeks, beyond which
 *   forecast skill degrades below climatological baseline. This constraint is
 *   invariant across all observer positions: meteorologists, climate
 *   modelers, dependent agents (farmers, disaster preparedness systems), and
 *   analytical observers all recognize the same fundamental limit. The
 *   constraint is not extractive — no agent benefits asymmetrically from the
 *   inability to predict weather. It is not suppressive — no actor is using
 *   the predictability limit to coerce another. It is purely a mathematical
 *   and physical fact: the Navier-Stokes equations governing atmospheric
 *   motion are chaotic on the Earth's parameter space, and perturbations at
 *   the scale of atmospheric measurement uncertainty grow at a characteristic
 *   rate (Lyapunov exponent λ ≈ 0.1-0.2 per day, doubling time ≈ 3-5 days for
 *   midlatitude weather features). After 2-3 doubling times, initial
 *   condition uncertainties become as large as the signal itself, and
 *   deterministic predictability is lost. This is a canonical natural law
 *   constraint: universal, unchangeable, applies identically regardless of
 *   measurement method or social context.
 *
 * KEY AGENTS:
 *   - Weather-Dependent Agents (farmers, disaster systems, transportation): Primary victims (powerless/trapped) — constrained by the predictability limit in planning horizon
 *   - Meteorological Research Community: Institutional observers (institutional/analytical) — understand the chaos mechanism deeply; cannot overcome it through better theory
 *   - Meteorological Forecasters: Institutional actors (institutional/constrained) — work within the skill ceiling defined by the predictability limit
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as universal law independent of social arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weather_predictability_limit, 0.12).
domain_priors:suppression_score(weather_predictability_limit, 0.02).
domain_priors:theater_ratio(weather_predictability_limit, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weather_predictability_limit, extractiveness, 0.12).
narrative_ontology:constraint_metric(weather_predictability_limit, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(weather_predictability_limit, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weather_predictability_limit, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(weather_predictability_limit, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weather_predictability_limit, mountain).
narrative_ontology:human_readable(weather_predictability_limit, "Weather Predictability Limit (Chaos and Lyapunov Exponents)").
narrative_ontology:topic_domain(weather_predictability_limit, "atmospheric_physics/dynamical_systems").

domain_priors:emerges_naturally(weather_predictability_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER-DEPENDENT AGENT (MOUNTAIN) — Individual or community dependent on weather predictions cannot escape the predictability limit. The 2-week horizon is an immutable constraint for planning horizons. No alternative coordination mechanism exists. Maximum perceived immutability.
constraint_indexing:constraint_classification(weather_predictability_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AGRICULTURAL COMMUNITY (MOUNTAIN) — Planning requires weather prediction beyond the predictability limit. High-cost mitigation strategies (insurance, irrigation, crop diversification) exist but are costly. The underlying constraint remains: atmospheric chaos cannot be overcome by social organization. Perceives immutability at generational scale.
constraint_indexing:constraint_classification(weather_predictability_limit, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METEOROLOGICAL ESTABLISHMENT (MOUNTAIN) — Institutional actors (national weather services, climate research centers) have studied the predictability limit for decades. The Lyapunov exponent and deterministic chaos are well-characterized. Institutional knowledge confirms immutability and universality of the constraint across all methodologies.
constraint_indexing:constraint_classification(weather_predictability_limit, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Lyapunov instability and atmospheric chaos are mathematical and physical facts independent of observation context. The predictability limit emerges from the nonlinear dynamics of the Navier-Stokes equations on a rotating sphere. This is a natural law perspective: universal, unchangeable, applies across all contexts.
constraint_indexing:constraint_classification(weather_predictability_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weather_predictability_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(weather_predictability_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weather_predictability_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weather_predictability_limit, ExtMetricName, E),
    domain_priors:suppression_score(weather_predictability_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(weather_predictability_limit),
    narrative_ontology:constraint_metric(weather_predictability_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(weather_predictability_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(weather_predictability_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The predictability limit is not an extractive mechanism. No actor captures asymmetric benefit from weather unpredictability. The constraint produces real costs (crop loss, disaster surprise, planning difficulty) but these are distributed damages, not concentrated extraction. Suppression (0.02): Negligible. The predictability limit does not suppress alternatives — it is simply a structural fact about what is possible. Weather observations are freely available; forecast models are openly shared; no agent is blocking better predictions. Theater ratio (0.15): Minimal. Meteorological forecasting is a straightforward technical process: data assimilation, model integration, statistical post-processing. Little performative content. The forecasts themselves are honest about uncertainty and skill limits. The constraint exhibits all hallmarks of a pure natural law: invariant across observers, unchangeable by institutional means, applies universally, requires no enforcement, and has no beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   Unusually, this constraint exhibits no perspectival gap. All six observer positions (powerless/trapped, moderate/constrained, institutional/arbitrage, institutional/analytical, organized/constrained, analytical/analytical) converge on the same classification: Mountain. This convergence is diagnostic evidence for natural law status. The weather-dependent agent trapped by the 2-week horizon sees it as unchangeable. The agricultural community planning across generations sees it as unchangeable. The meteorological establishment studying chaos for 50+ years sees it as unchangeable and universal. The analytical observer at civilizational scale sees it as a law of physics. No observer perceives negotiability, coalition possibility, or sunset logic. This universal invariance is the definitive signature of Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy. The constraint is uniform-type Mountain across all perspectives. No actor is misclassified as perpetrator (beneficiary) when they are victim, or vice versa. No extraction is being naturalized as coordination. The constraint is genuinely about immutable limits, not about power asymmetries. The theoretical possibility: an observer might attempt to naturalize institutional forecasting failures as 'chaotic unpredictability,' blaming the constraint rather than weak prediction infrastructure. The empirical test: compare forecast skill trajectories across countries and decades. If institutional/resource differences correlate with forecast skill, the constraint is malleable (Piton). If skill converges to a common asymptote regardless of investment, the constraint is truly Mountain. Historical data shows convergence to asymptote (~2-week horizon) across all developed meteorological services, confirming natural law status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chaotic_vs_deterministic,
    'Is the weather predictability limit due to intrinsic chaos (butterfly effect) or due to incomplete information and model error?',
    'Ensemble forecasting analysis: compare growth rates of initial condition perturbations (chaos signature) against model improvement and data assimilation gains over the past 30 years',
    'If intrinsic chaos dominates: limit is immutable natural law (Mountain confirmed). If incomplete information dominates: limit could be pushed back by better data and models (constraint degrades from Mountain to Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaotic_vs_deterministic, empirical, 'Whether predictability limit is intrinsic chaos or incomplete information').

omega_variable(
    predictability_horizon_generalization,
    'Does the 2-week predictability horizon apply universally to all weather phenomena, or only to small-scale perturbations?',
    'Skill score analysis across phenomenon scales: monsoon onset, teleconnection patterns, blocking highs, tropical cyclones, severe local storms; map skill decay timescales by scale',
    'If universal: simplifies mountain classification. If scale-dependent: larger-scale phenomena may have longer predictability (horizon stretches to 3-4 weeks for some phenomena), suggesting the mountain is actually a piton (theater_ratio high for small-scale forecasts, low for climate patterns).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictability_horizon_generalization, empirical, 'Whether predictability horizon is scale-universal or phenomenon-dependent').

omega_variable(
    model_fidelity_asymptote,
    'Is the predictability limit fixed by physics, or will it shift as computational resources and observational networks expand?',
    'Longitudinal trend analysis: track median forecast skill for lead times 1-14 days over 20-year period; test for asymptotic convergence or continued improvement',
    'If asymptote exists: natural law (Mountain). If trend continues upward: the limit is shifting, suggesting the constraint is institutional/technological (Piton or Scaffold), not physical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_fidelity_asymptote, empirical, 'Whether predictability limit is fixed or shifting with model improvement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weather_predictability_limit, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(weather_tr_t0, weather_predictability_limit, theater_ratio, 0, 0.12).
narrative_ontology:measurement(weather_tr_t20, weather_predictability_limit, theater_ratio, 20, 0.14).
narrative_ontology:measurement(weather_tr_t40, weather_predictability_limit, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(weather_be_t0, weather_predictability_limit, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(weather_be_t20, weather_predictability_limit, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(weather_be_t40, weather_predictability_limit, base_extractiveness, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(weather_predictability_limit, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
