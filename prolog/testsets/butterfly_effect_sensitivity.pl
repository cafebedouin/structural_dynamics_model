% ============================================================================
% CONSTRAINT STORY: butterfly_effect_sensitivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_butterfly_effect_sensitivity, []).

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
 *   constraint_id: butterfly_effect_sensitivity
 *   human_readable: Butterfly Effect Sensitivity in Chaotic Systems
 *   domain: mathematics/physics/dynamical_systems
 *
 * SUMMARY:
 *   The butterfly effect — the property that arbitrarily small changes in
 *   initial conditions lead to exponential divergence of trajectories — is a
 *   fundamental structural feature of chaotic dynamical systems. Discovered
 *   by Edward Lorenz in 1963 and formalized through Lyapunov exponent theory,
 *   this constraint represents an irreducible limitation on predictability
 *   and control. Unlike institutional, social, or economic constraints that
 *   could theoretically be negotiated or redesigned, the butterfly effect
 *   appears to be a law of physics and mathematics that no agent can escape,
 *   transcend, or coordinate around. It emerges naturally from the nonlinear
 *   dynamics of systems ranging from weather to quantum chaotic systems. All
 *   observers, from meteorologists to analytical mathematicians, encounter
 *   the same constraint: arbitrarily close initial states diverge
 *   exponentially in time, making long-term deterministic prediction
 *   impossible in principle. The constraint exhibits zero theater (no
 *   performative component), minimal suppression (no enforcement mechanism —
 *   the limit is structural), and low base extractiveness (it does not favor
 *   one agent over another; it is indifferent to all).
 *
 * KEY AGENTS:
 *   - Weather Prediction Systems: Powerless agent (trapped/biographical) — cannot escape exponential divergence no matter how much computational power is applied
 *   - Meteorological Institutions: Moderate institutional power (constrained/regional) — face predictive skill ceiling from butterfly sensitivity despite operational resources
 *   - Computational Climate Scientists: Analytical observers (analytical/civilizational) — recognize sensitivity as an intrinsic property of the system, not a remediable technical problem
 *   - Physics/Mathematics Community: Analytical community (analytical/universal) — studies the constraint itself through dynamical systems theory and chaos theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(butterfly_effect_sensitivity, 0.12).
domain_priors:suppression_score(butterfly_effect_sensitivity, 0.03).
domain_priors:theater_ratio(butterfly_effect_sensitivity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(butterfly_effect_sensitivity, extractiveness, 0.12).
narrative_ontology:constraint_metric(butterfly_effect_sensitivity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(butterfly_effect_sensitivity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(butterfly_effect_sensitivity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(butterfly_effect_sensitivity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(butterfly_effect_sensitivity, mountain).
narrative_ontology:human_readable(butterfly_effect_sensitivity, "Butterfly Effect Sensitivity in Chaotic Systems").
narrative_ontology:topic_domain(butterfly_effect_sensitivity, "mathematics/physics/dynamical_systems").

domain_priors:emerges_naturally(butterfly_effect_sensitivity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEATHER SYSTEM OBSERVER (MOUNTAIN) — The sensitivity to initial conditions is an immutable property of atmospheric dynamics. No observer can escape this constraint regardless of their temporal horizon or exit capacity. Prediction failures are not avoidable through coordination or effort — they follow from the topology of phase space itself.
constraint_indexing:constraint_classification(butterfly_effect_sensitivity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: OPERATIONAL METEOROLOGIST (MOUNTAIN) — Despite having institutional resources and career incentives to improve forecasts, the meteorologist cannot overcome the exponential divergence of nearby trajectories. The constraint is structural, not remediable by better methods or more computing power. The butterfly effect is an unavoidable ceiling on forecast skill.
constraint_indexing:constraint_classification(butterfly_effect_sensitivity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the standpoint of dynamical systems theory, sensitivity to initial conditions is a universal property of chaotic systems. It emerges necessarily from nonlinearity and cannot be bargained with, regulated, or designed around. The constraint is invariant across all measurement methodologies and observables.
constraint_indexing:constraint_classification(butterfly_effect_sensitivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(butterfly_effect_sensitivity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(butterfly_effect_sensitivity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(butterfly_effect_sensitivity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(butterfly_effect_sensitivity, ExtMetricName, E),
    domain_priors:suppression_score(butterfly_effect_sensitivity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(butterfly_effect_sensitivity),
    narrative_ontology:constraint_metric(butterfly_effect_sensitivity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(butterfly_effect_sensitivity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(butterfly_effect_sensitivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The butterfly effect does not extract value from any agent for the benefit of another. It is an indifferent structural limit. The nonzero value reflects that systems with this property are harder to control, which imposes costs on all actors trying to use those systems for prediction or control — a diffuse cost distributed across all agents equally, not an asymmetric extraction. Suppression (0.03): Near-zero. There is no enforcement mechanism or deliberate suppression — the constraint is a mathematical-physical property. The minimal nonzero value reflects that the universe does not 'allow' trajectories that would violate sensitivity; the property is enforced by the laws of physics themselves rather than by any actor. Theater ratio (0.15): Very low. The constraint has no performative dimension — sensitivity is either present or absent. There is no ritual, theater, or appearance management involved. Measurement of Lyapunov exponents directly tests for the constraint with minimal observational overhead. Accessibility collapse (0.88): Very high. There is no path to escape the constraint. No coordination mechanism, technological advancement, or institutional innovation can eliminate butterfly sensitivity in chaotic systems. The collapse is total. Resistance (0.08): Very low. There is essentially no active resistance to this constraint — it exists regardless of whether any actor wishes to resist it. It is not maintained by suppression or opposition; it is simply a law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives converge on Mountain classification. The powerless meteorologist, the moderate operational forecaster, and the analytical observer all encounter the same immutable constraint. The difference is in their temporal horizons and acceptance: the operational forecaster may see the limitation as merely biographical (solvable with better methods within a career lifetime), while the analytical observer at civilizational scale recognizes it as permanent. However, both classify the constraint identically as Mountain because the underlying structural property is unchanged.
 *
 * DIRECTIONALITY LOGIC:
 *   The butterfly effect exhibits zero directionality gradient. There is no beneficiary who captures value and no victim who bears cost. The constraint is not a relationship between agents but a property of the system itself. In the f(d) function, all agents would have d ≈ 0.5 (symmetric cost/benefit) because no agent structurally benefits from the inability to predict chaotic systems. The chi formula produces χ = ε × f(d) × σ(S) ≈ 0.12 × 0.65 × 1.0 ≈ 0.08 across all perspectives, a value so low that it appears as a natural law constraint to all observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a gold-standard Mountain with zero mandatrophy. All six types resolve to Mountain. There is no tension between coordination and extraction — the constraint is neither. There is no question of whether it is a natural law or a contingent institutional arrangement — the structural properties (emerges_naturally=true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, extractiveness ≤ 0.25) certify it as a Mountain across all measurement methodologies. The constraint is invariant and perspectival equilibrium is achieved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_vs_physical_limits,
    'Is butterfly effect sensitivity a property of the physical system itself or an artifact of finite computational precision and measurement error?',
    'Establish whether exponential divergence occurs in the mathematical limit of infinite precision or only in practical finite-precision arithmetic. Examine renormalization-group flow and ultraviolet cutoffs in continuous systems.',
    'If physical: Mountain classification is correct. If computational artifact: Constraint might be Scaffold (solvable by moving beyond classical determinism). If both: Constraint is still Mountain but the mechanism is clarified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_vs_physical_limits, empirical, 'Physical vs computational nature of sensitivity').

omega_variable(
    quantum_vs_classical_divergence,
    'Does quantum mechanics eliminate or preserve butterfly effect sensitivity through decoherence and measurement collapse?',
    'Compare Lyapunov exponents in quantum systems vs classical limits. Examine whether quantum trajectories show equivalent sensitivity or whether unitarity provides an exit from classical chaos.',
    'If quantum removes sensitivity: Constraint is domain-specific (classical mechanics), not universal. If preserved: Mountain classification extends to quantum mechanics. If ambiguous: Identifies whether quantum interpretations create perspectival mountains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_vs_classical_divergence, empirical, 'Preservation of sensitivity in quantum mechanics').

omega_variable(
    observational_resolution_hierarchy,
    'At what observational scale does butterfly effect sensitivity become irrelevant? Can coarse-graining, averaging, or macro-scale observables escape the constraint?',
    'Analyze information loss in coarse-grained dynamics. Compare sensitivity of microscale vs macroscale observables. Examine whether some collective coordinates are insensitive even when microscopic ones are.',
    'If escape exists at macro scale: Constraint is conditional (mountain at fine scale, rope or absent at coarse scale). If no escape: Confirms universality. Informs whether practical applications can sidestep the limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_resolution_hierarchy, empirical, 'Scale-dependence of butterfly effect sensitivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(butterfly_effect_sensitivity, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(butterfly_tr_t0, butterfly_effect_sensitivity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(butterfly_tr_t100, butterfly_effect_sensitivity, theater_ratio, 100, 0.15).
narrative_ontology:measurement(butterfly_tr_t200, butterfly_effect_sensitivity, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(butterfly_be_t0, butterfly_effect_sensitivity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(butterfly_be_t100, butterfly_effect_sensitivity, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(butterfly_be_t200, butterfly_effect_sensitivity, base_extractiveness, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(butterfly_effect_sensitivity, information_standard).
narrative_ontology:affects_constraint(butterfly_effect_sensitivity, deterministic_prediction_horizon).
narrative_ontology:affects_constraint(butterfly_effect_sensitivity, weather_forecast_skill_ceiling).
narrative_ontology:affects_constraint(butterfly_effect_sensitivity, quantum_chaos_decoherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
