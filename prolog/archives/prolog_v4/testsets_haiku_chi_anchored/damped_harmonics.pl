% ============================================================================
% CONSTRAINT STORY: damped_harmonics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_damped_harmonics, []).

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
 *   constraint_id: damped_harmonics
 *   human_readable: Damped Harmonic Motion
 *   domain: technological/physics
 *
 * SUMMARY:
 *   Damped harmonic motion is a canonical example of a natural law constraint
 *   in technological domains. The phenomenon — the exponential decay of
 *   oscillatory amplitude due to dissipative forces — emerges directly from
 *   the mathematical structure of second-order linear differential equations
 *   with velocity-dependent damping terms. No agent extracts value from the
 *   damping itself; no group suppresses alternatives to it. The constraint is
 *   irreducible across all technological scales, from macroscopic mechanical
 *   systems (bridge suspensions, seismic isolation) to microscopic quantum
 *   systems (atomic decay, decoherence). The base extractiveness (0.12)
 *   reflects residual uncertainty about whether quantum decoherence is a
 *   strict natural law or emerges from effective descriptions; the
 *   suppression (0.03) is near-zero because there is no coercive mechanism.
 *   The theater ratio (0.15) is low because the mechanism is transparent —
 *   engineers can directly measure damping coefficients, calculate decay
 *   envelopes, and verify predictions with high precision. This constraint
 *   serves as a diagnostic exemplar for how natural laws appear across all
 *   observer perspectives: a mathematical physicist sees the differential
 *   equation structure; an engineer sees material damping coefficients; a
 *   device operator sees energy loss; an end user experiences vibration
 *   decay. All perspectives converge on the same classification because the
 *   constraint is universally binding and non-extractive.
 *
 * KEY AGENTS:
 *   - Physicist/Mathematician: Observer (analytical/analytical) — derives constraint from fundamental ODE structure
 *   - Engineer/Designer: Powerful actor (powerful/mobile) — designs systems accounting for damping but cannot eliminate it
 *   - Technician/Operator: Moderate actor (moderate/constrained) — manages device behavior under damping constraint
 *   - End User: Constrained actor (powerless/trapped) — experiences vibration decay as unavoidable physical fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(damped_harmonics, 0.12).
domain_priors:suppression_score(damped_harmonics, 0.03).
domain_priors:theater_ratio(damped_harmonics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(damped_harmonics, extractiveness, 0.12).
narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(damped_harmonics, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(damped_harmonics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(damped_harmonics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(damped_harmonics, mountain).
narrative_ontology:human_readable(damped_harmonics, "Damped Harmonic Motion").
narrative_ontology:topic_domain(damped_harmonics, "technological/physics").

domain_priors:emerges_naturally(damped_harmonics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational/universal scope, damped harmonic motion is a fundamental mathematical consequence of linear differential equations with dissipative terms. The amplitude decay follows exponential envelope functions (exp(-γt)) that emerge directly from the second-order ODE structure. This is not a contingent institutional arrangement but a structural feature of how energy dissipation manifests in oscillatory systems. ε=0.12, suppression=0.03 → Accessibility collapse=0.92, resistance=0.08 → Mountain certification confirmed.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEERING PRACTITIONER (MOUNTAIN) — Engineers designing suspension systems, seismic dampers, or vibration isolation confront damping as an irreducible physical property. No engineering choice eliminates it; only the dissipative mechanism (friction coefficient, air density, material viscosity) can be controlled within tight bounds. The practitioner's exit options are mobile (select materials, adjust design) but the underlying constraint persists. Even idealized systems with zero friction must account for quantum mechanical damping in realistic scales. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. Mountain holds across power levels.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVICE OPERATOR (MOUNTAIN) — A technician running a mechanical oscillator (pendulum clock, vibrating screen, suspension bridge) encounters damping as a constraint that cannot be negotiated away. The device will lose energy; the oscillations will decay. Operators can adjust the system (re-wind springs, re-tension cables, add energy input) but cannot exit the underlying physics. d≈0.60, f(d)≈0.87, σ=1.0 → χ≈0.10. Mountain persists even for constrained exit options.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: END USER / DEVICE ISOLATION (MOUNTAIN) — A user experiencing vibration in a device (laptop fan vibration, car suspension noise, industrial equipment) confronts damping as a completely inescapable physical law. No exit option exists within the immediate local scope. The user cannot manufacture a device without damping; cannot escape the fact that oscillations decay. This is the most constrained perspective, but classification remains Mountain because the constraint is not extractive (no agent benefits from the damping itself) nor suppressive (the mechanism is transparent). d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.17. Mountain persists even at maximum constraint.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(damped_harmonics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(damped_harmonics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(damped_harmonics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(damped_harmonics, ExtMetricName, E),
    domain_priors:suppression_score(damped_harmonics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(damped_harmonics),
    narrative_ontology:constraint_metric(damped_harmonics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(damped_harmonics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(damped_harmonics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint exhibits near-zero extraction because no agent systematically benefits at another's expense. Energy dissipation is a universal property affecting all oscillatory systems symmetrically (though to different degrees depending on materials and geometry). The residual 0.12 reflects uncertainty about whether quantum decoherence constitutes a 'softer' form of the constraint at microscopic scales, potentially leaving room for exotic quantum systems with suppressed decoherence. Suppression (0.03): Near-zero. There is no active suppression of alternatives to damping. The constraint emerges from fundamental physics (second law of thermodynamics) rather than institutional enforcement. Resistance to the constraint is extremely low (0.08) because the mathematical and physical mechanisms are well-understood and universally applicable. Theater ratio (0.15): Very low. The mechanism is transparent and verifiable. Engineers can measure damping coefficients directly; decay envelopes can be predicted and verified experimentally with high precision. No performative content obscures the underlying mechanism. Accessibility collapse (0.92): Very high. The constraint is accessible from first principles of physics; no hidden assumptions or observational barriers prevent verification. The mathematical structure is taught at introductory physics levels and applies identically across scales.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for damped harmonic motion — all observers, regardless of power level, time horizon, exit options, or spatial scope, converge on the Mountain classification. This invariance is the defining feature of a natural law constraint. The physicist sees the differential equation; the engineer sees material viscosity; the operator sees energy decay; the user sees oscillation cessation. All perspectives acknowledge the same irreducible mechanism. This invariance across (P,T,E,S) tuples is the strongest evidence for mountain status. Where perspectival gaps arise in other constraints (beneficiary vs victim seeing different types, organized actors seeing ropes where powerless see snares), damped harmonics exhibits unified perception because no agent captures asymmetric value from the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality override is needed because damping is non-extractive. The constraint does not pit beneficiaries against victims; does not enable coercive power; does not create arbitrage opportunities. All agents experience the same irreducible physics. The directional values computed from the (P,T,E,S) tuples (d ranging from 0.50 to 0.95 across perspectives) feed into χ calculations that yield 0.08 to 0.17 across all observers — all well below snare or tangled rope thresholds, confirming mountain classification from purely mathematical grounds. No structural relationship data (beneficiaries, victims) is needed because the constraint has no beneficiary/victim structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_vs_classical_boundary,
    'At what scale does classical damping transition to quantum decoherence, and is this transition itself a mountain or does the transition point represent a choice of observational framework?',
    'Detailed analysis of quantum master equations and their classical limits; experimental measurements of damping timescales across scale regimes; derivation of decoherence rates from Lindblad operators',
    'If quantum decoherence is fundamental: damping is mountain at all scales (stronger certification). If transition is observational: damping remains mountain but its ''explanation'' depends on framework. Either way, the constraint persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_vs_classical_boundary, empirical, 'Whether classical damping and quantum decoherence represent the same mountain').

omega_variable(
    non_linear_damping_regime,
    'In high-amplitude regimes where damping becomes nonlinear (turbulent drag, stick-slip friction), does the mathematical structure change enough to alter the mountain classification, or is nonlinear damping still a natural law?',
    'Rigorous analysis of nonlinear ODE solutions; classification of whether nonlinear dissipation exhibits universality classes; determination of whether power-law or exponential decay still holds asymptotically',
    'If nonlinear damping preserves mountain structure: ε and suppression remain low, mountain holds universally. If nonlinear regime exhibits bifurcations or strange attractors: ε may rise toward 0.25 boundary; classification may degrade to Rope or Tangled Rope in specific regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_linear_damping_regime, empirical, 'Whether nonlinear damping regimes preserve mountain status').

omega_variable(
    active_compensation_feasibility,
    'Can active feedback systems (energy input proportional to velocity) fully compensate for damping and restore perpetual oscillation, making damping a coordinate problem rather than a natural law?',
    'Stability analysis of active compensation; measurement of compensation bandwidth limits and phase lag; determination of whether compensation can exceed physical limits (causality, entropy production)',
    'If perfect compensation is possible: damping is not a natural law but a Rope coordination problem (system design choice). If compensation has limits: damping remains a mountain but the mountain is narrower (applies only to uncompensated systems). Current physics suggests compensation has hard limits due to causality and entropy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_compensation_feasibility, empirical, 'Whether active compensation can fully eliminate damping effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(damped_harmonics, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(damph_tr_t0, damped_harmonics, theater_ratio, 0, 0.12).
narrative_ontology:measurement(damph_tr_t50, damped_harmonics, theater_ratio, 50, 0.14).
narrative_ontology:measurement(damph_tr_t100, damped_harmonics, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(damph_be_t0, damped_harmonics, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(damph_be_t50, damped_harmonics, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(damph_be_t100, damped_harmonics, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(damped_harmonics, global_infrastructure).
narrative_ontology:affects_constraint(damped_harmonics, entropy_production).
narrative_ontology:affects_constraint(damped_harmonics, oscillatory_stability).
narrative_ontology:affects_constraint(damped_harmonics, energy_dissipation_limits).

% DUAL FORMULATION NOTE:
% Damped harmonic motion is upstream to several energy dissipation constraints: entropy production is the thermodynamic foundation; oscillatory stability in coupled systems depends on damping rates; energy dissipation limits in quantum systems emerge from decoherence models based on classical damping analogs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
