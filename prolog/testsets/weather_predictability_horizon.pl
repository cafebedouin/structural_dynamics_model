% ============================================================================
% CONSTRAINT STORY: weather_predictability_horizon
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weather_predictability_horizon, []).

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
 *   constraint_id: weather_predictability_horizon
 *   human_readable: Weather Predictability Horizon
 *   domain: atmospheric_physics/meteorology
 *
 * SUMMARY:
 *   Weather predictability horizon represents a fundamental constraint on
 *   atmospheric forecasting arising from the chaotic dynamics of the
 *   atmosphere. The Lyapunov exponent of ~0.2 day^-1 implies that small
 *   perturbations (observation errors, model biases) grow exponentially,
 *   saturating prediction skill at approximately 10-14 days. This constraint
 *   is uniform across all institutional contexts, technological levels, and
 *   resource expenditures. The weather service with access to global
 *   satellite networks and petaflop supercomputers cannot significantly
 *   extend predictability beyond the same 10-14 day horizon as a service with
 *   regional radar and workstation computing. This uniformity across
 *   technological contexts is the diagnostic signature of a Mountain: the
 *   constraint emerges from the intrinsic properties of the system (chaos)
 *   rather than from institutional, economic, or coordinative arrangements.
 *   Theater ratio is low (0.15) because weather forecasting is functionally
 *   effective within the predictable window and its failure modes are obvious
 *   to users — there is little performative content. The constraint exhibits
 *   zero degrees of freedom: no agent can negotiate with the Lyapunov
 *   exponent.
 *
 * KEY AGENTS:
 *   - Agricultural Planners: Immediate users (powerless/trapped) — dependent on forecasts beyond the predictable window for decisions that cannot be delayed
 *   - National Meteorological Services: Professional forecasters (institutional/arbitrage) — benefit from access to global observation networks; cannot overcome the structural limit even with this advantage
 *   - Global Meteorological Infrastructure: Institutional collective (institutional/arbitrage) — all global observing systems and supercomputers face the same barrier
 *   - Analytical Observer: Theoretical physicist/mathematician (analytical/analytical) — recognizes the constraint as a consequence of Navier-Stokes chaos
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weather_predictability_horizon, 0.12).
domain_priors:suppression_score(weather_predictability_horizon, 0.03).
domain_priors:theater_ratio(weather_predictability_horizon, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weather_predictability_horizon, extractiveness, 0.12).
narrative_ontology:constraint_metric(weather_predictability_horizon, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(weather_predictability_horizon, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weather_predictability_horizon, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(weather_predictability_horizon, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weather_predictability_horizon, mountain).
narrative_ontology:human_readable(weather_predictability_horizon, "Weather Predictability Horizon").
narrative_ontology:topic_domain(weather_predictability_horizon, "atmospheric_physics/meteorology").

domain_priors:emerges_naturally(weather_predictability_horizon).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGRICULTURAL AGENT (MOUNTAIN) — A farmer planning crop irrigation faces absolute unpredictability beyond 10-14 days. No amount of capital, political power, or technological investment can overcome the Lyapunov exponent. The constraint is experienced as natural law: the atmosphere has finite predictability horizon, period.
constraint_indexing:constraint_classification(weather_predictability_horizon, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: METEOROLOGIST (MOUNTAIN) — Professional meteorologists with access to global satellite data, supercomputers, and decades of model development confirm daily: predictability decays exponentially. A meteorologist can improve forecasts through better models and observations, but cannot push the horizon meaningfully beyond 10-14 days. This is a constraint on the domain itself, not on access or resources.
constraint_indexing:constraint_classification(weather_predictability_horizon, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: GLOBAL METEOROLOGICAL COMMUNITY (MOUNTAIN) — Across 193 countries, national weather services, academic research groups, and private forecast companies have invested trillions in observation networks (satellites, radiosondes, surface stations, radar), computational capacity, and model development. Despite all this infrastructure and decades of Moore's Law improvements, the 10-14 day predictability barrier has not moved. This is not a resource or coordination problem — it is structural.
constraint_indexing:constraint_classification(weather_predictability_horizon, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From first principles: atmospheric dynamics are governed by the Navier-Stokes equations with chaotic behavior characterized by positive Lyapunov exponents (λ ≈ 0.2 day^-1). This means prediction error doubles every ~3-5 days; after 10-14 days, uncertainty saturates at climatological noise floor. This is not a technological horizon but a mathematical one. No finite observation precision or computational power can overcome deterministic chaos.
constraint_indexing:constraint_classification(weather_predictability_horizon, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weather_predictability_horizon_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(weather_predictability_horizon, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weather_predictability_horizon, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weather_predictability_horizon, ExtMetricName, E),
    domain_priors:suppression_score(weather_predictability_horizon, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(weather_predictability_horizon),
    narrative_ontology:constraint_metric(weather_predictability_horizon, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(weather_predictability_horizon, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(weather_predictability_horizon_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. This constraint imposes no extraction — it is not differential across agents. All agents (powerless farmers, powerful meteorological agencies, wealthy insurance companies) face the identical 10-14 day horizon. There is no asymmetric benefit or cost; the constraint is uniformly binding. The 0.12 value reflects the minimal descriptive extractiveness of any immutable physical law — measurement of the constraint itself requires some empirical work, but the work is not extraction. Suppression (0.03): Negligible. Agents do not experience suppression because there is no alternative being suppressed. A farmer cannot be 'suppressed' in choosing whether to attempt 30-day forecasts; the horizon is simply not available to anyone. Resistance (0.08): Very low. Attempts to extend predictability (higher resolution models, better observations, longer integration times) fail not from institutional resistance but from fundamental physical law. Theater ratio (0.15): Low. Weather forecasting institutions do engage in some performance — confidence framing, model blend selection, communication styling — but the core function (predicting weather accurately within 10-14 days) is transparently effective or ineffective. Failure is obvious. This low theater distinguishes weather forecasting from many institutions that maintain high theater despite degraded function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap — all perspectives converge on Mountain classification. The farmer experiences immutable unpredictability; the meteorologist confirms it professionally; the global meteorological community encounters it universally; the analytical observer derives it from first principles. This uniformity is itself a Mountain diagnostic: the constraint transcends observer position. If perspectives diverged (some seeing Rope, others seeing Mountain), the constraint would not be a true Mountain but a contingent institutional arrangement masquerading as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply — this is a Mountain constraint with no beneficiaries or victims in the extractive sense. The constraint affects all agents identically. There is no d value to compute because there is no asymmetry in how the constraint operates. This is the defining feature of Mountains: zero degrees of freedom for all indices means zero variation across agent positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_precision_limit,
    'Could improved observational density (e.g., high-density drone networks, quantum sensors) reduce the Lyapunov exponent and extend predictability beyond 14 days?',
    'Ensemble sensitivity studies isolating observation error growth; comparison of skill gain vs observation density; theoretical analysis of attractor dimension vs sensor resolution',
    'If yes: predictability horizon is contingent on technology (Rope, not Mountain). If no: chaos-driven unpredictability is truly structural (Mountain confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observation_precision_limit, empirical, 'Whether observation precision could extend predictability horizon').

omega_variable(
    model_parameterization_sufficiency,
    'Is the 10-14 day barrier due to inadequate model physics (parameterization of clouds, convection, land-surface interaction) or irreducible chaos?',
    'Systematic improvement in sub-grid-scale physics (cloud microphysics, mesoscale convection) with skill tracking; comparison of highest-resolution deterministic forecasts vs ensemble spread; theoretical predictability studies isolating physical vs chaotic components',
    'If inadequate physics: barrier is technological (Rope). If chaos-dominated: barrier is Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_parameterization_sufficiency, empirical, 'Whether barrier is due to model physics or deterministic chaos').

omega_variable(
    predictability_regime_transition,
    'Is there a qualitative transition in predictability at the 10-14 day mark, or is it a gradual decay?',
    'Skill metric analysis (RMSE, correlation, anomaly correlation) across all timescales; identification of critical timescale where ensemble spread exceeds signal; phase-space analysis of attractor saturation',
    'If sharp transition: suggests a physical threshold (Mountain). If gradual: suggests technological ceiling (Rope/Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictability_regime_transition, empirical, 'Whether predictability decline is sharp or gradual').

omega_variable(
    regional_variation_structure,
    'Do different atmospheric regimes (tropical vs midlatitude, stable vs convective) have genuinely different predictability horizons, or is the 10-14 day limit universal?',
    'Skill analysis stratified by atmospheric pattern, latitude, season; identification of regimes with extended predictability; comparison of dynamical vs empirical predictability limits',
    'If universal: supports Mountain (immutable law). If regime-dependent: suggests structural variation (Multiple Mountains with different limits, or Rope with context-dependent horizon).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_structure, empirical, 'Whether predictability horizon varies by atmospheric regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weather_predictability_horizon, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(weather_tr_t0, weather_predictability_horizon, theater_ratio, 0, 0.15).
narrative_ontology:measurement(weather_tr_t25, weather_predictability_horizon, theater_ratio, 25, 0.15).
narrative_ontology:measurement(weather_tr_t50, weather_predictability_horizon, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(weather_be_t0, weather_predictability_horizon, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(weather_be_t25, weather_predictability_horizon, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(weather_be_t50, weather_predictability_horizon, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(weather_predictability_horizon, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
