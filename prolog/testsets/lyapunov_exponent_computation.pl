% ============================================================================
% CONSTRAINT STORY: lyapunov_exponent_computation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lyapunov_exponent_computation, []).

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
 *   constraint_id: lyapunov_exponent_computation
 *   human_readable: Lyapunov Exponent Computation and Trajectory Divergence
 *   domain: dynamical_systems/chaos_theory
 *
 * SUMMARY:
 *   Lyapunov exponent computation represents a fundamental constraint on
 *   predictability in deterministic chaotic systems. It is a natural law
 *   constraint arising from mathematics rather than from institutional
 *   arrangement, policy, or asymmetric power. Any deterministic system with a
 *   positive maximum Lyapunov exponent exhibits exponential divergence of
 *   initially close trajectories. This constraint is invariant across all
 *   implementation contexts: computational methods, observational strategies,
 *   institutional resources, and time horizons. The constraint emerges
 *   directly from the definition of chaotic dynamics and is not subject to
 *   negotiation, regulatory bypass, or institutional reform. It classifies as
 *   Mountain from every perspective examined — powerless agents,
 *   institutional actors, and analytical observers all encounter the same
 *   mathematical boundary. The constraint's extractiveness (0.18) reflects
 *   only the computational cost of calculating Lyapunov exponents themselves,
 *   not any asymmetric extraction — the cost is symmetric across all agents.
 *   Suppression (0.03) is minimal because the constraint imposes no
 *   alternative-suppression: agents who understand Lyapunov dynamics can
 *   still use statistical methods, ensemble forecasting, or control
 *   strategies within the constraint's bounds.
 *
 * KEY AGENTS:
 *   - Trajectory Predictor: Individual or system attempting long-term forecasting (powerless/trapped) — encounters the constraint directly when prediction horizon shrinks to Lyapunov timescale
 *   - Institutional Forecaster: Weather service, climate model, economic forecaster (institutional/arbitrage) — has resources but no exemption from Lyapunov bounds
 *   - Mathematician/Analyst: Formal observer (analytical/analytical) — recognizes the constraint as a structural feature of the dynamics itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lyapunov_exponent_computation, 0.18).
domain_priors:suppression_score(lyapunov_exponent_computation, 0.03).
domain_priors:theater_ratio(lyapunov_exponent_computation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lyapunov_exponent_computation, extractiveness, 0.18).
narrative_ontology:constraint_metric(lyapunov_exponent_computation, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(lyapunov_exponent_computation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lyapunov_exponent_computation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lyapunov_exponent_computation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lyapunov_exponent_computation, mountain).
narrative_ontology:human_readable(lyapunov_exponent_computation, "Lyapunov Exponent Computation and Trajectory Divergence").
narrative_ontology:topic_domain(lyapunov_exponent_computation, "dynamical_systems/chaos_theory").

domain_priors:emerges_naturally(lyapunov_exponent_computation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAJECTORY SEEKER (MOUNTAIN) — Any agent attempting to predict long-term behavior in chaotic systems faces the same constraint: finite precision in initial conditions produces exponentially diverging trajectories. The constraint is unchangeable from this position — it follows directly from the mathematics of sensitivity to initial conditions. No amount of computational power or refinement can overcome the exponential amplification timescale.
constraint_indexing:constraint_classification(lyapunov_exponent_computation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: WEATHER SERVICE (MOUNTAIN) — Even institutional actors with resources and arbitrage options cannot escape the Lyapunov constraint. Improved models, higher resolution, ensemble methods all work within the same mathematical boundary. The constraint is invariant across implementation strategies — it emerges from the structure of chaotic dynamics itself, not from institutional factors.
constraint_indexing:constraint_classification(lyapunov_exponent_computation, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational mathematical perspective, the Lyapunov exponent constraint is a structural necessity: systems with positive Lyapunov exponents exhibit chaotic behavior by definition. The constraint is that trajectory divergence follows exponential growth with rate λ (the maximum Lyapunov exponent). This is not a law of nature imposed externally — it is intrinsic to the mathematical definition of chaos.
constraint_indexing:constraint_classification(lyapunov_exponent_computation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lyapunov_exponent_computation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lyapunov_exponent_computation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lyapunov_exponent_computation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lyapunov_exponent_computation, ExtMetricName, E),
    domain_priors:suppression_score(lyapunov_exponent_computation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lyapunov_exponent_computation),
    narrative_ontology:constraint_metric(lyapunov_exponent_computation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lyapunov_exponent_computation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lyapunov_exponent_computation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint carries minimal extractive content. It does not transfer resources from one agent to another. Rather, it represents a symmetric limitation on all agents' capacity for long-term trajectory prediction. The small non-zero value reflects only the real computational cost of calculating Lyapunov exponents in the first place — a pure coordination cost, not extraction. Suppression (0.03): Minimal. The constraint does not suppress alternatives — it simply defines the boundary of single-trajectory predictability. Agents remain free to employ ensemble forecasting, statistical models, or active control strategies within this boundary. There is no mechanism of coercion or suppression inherent to the Lyapunov constraint itself. Theater ratio (0.12): Very low. The constraint has minimal performative content. Lyapunov exponent calculation is straightforward and measurable — there is no ritual or theater required to apply it. The small non-zero value reflects only the inherent computational overhead of numerical integration and sensitivity analysis.
 *
 * PERSPECTIVAL GAP:
 *   There is NO perspectival gap. All three perspectives converge on Mountain classification. This is the defining property of a true natural law constraint: it classifies identically from every observation site. The powerless trajectory seeker, the institutional weather service, and the analytical mathematician all face the same mathematical boundary. This invariance across all contexts is the diagnostic signature of a Mountain constraint — accessibility collapse (0.92) indicates that no observer position can find a workaround, and resistance (0.08) indicates that the constraint is nearly impossible to resist or overcome through institutional means.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to Mountain constraints in the way it applies to extractive or coordinative constraints. There is no beneficiary or victim — the constraint affects all agents symmetrically. The Lyapunov exponent computation constraint does not extract from any agent to benefit another. It is not a coordination mechanism serving mutual benefit. It is a structural limit on predictability that all agents face equally. The canonical d value for a Mountain constraint is 0.5 (symmetric), reflecting that costs and benefits are distributed uniformly rather than asymmetrically. No directionality override is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy present. This is a pure Mountain constraint with zero ambiguity about classification. All perspectives produce Mountain. The constraint emerges naturally from mathematical structure. Extractiveness and suppression both lie well below the thresholds required for alternative classifications (Rope, Snare, etc.). The Lyapunov exponent constraint is one of the clearest examples of a true natural law: it is invariant across all observables, time horizons, institutional contexts, and spatial scales. The resolution of mandatrophy (if any ambiguity had existed) would come from recognizing that the constraint on individual trajectory divergence does not extend to ensemble predictions, measure-theoretic properties, or controlled trajectories — but these are distinct scopes, not the same constraint viewed from different perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observational_precision_floor,
    'Is the effective prediction horizon constrained by Lyapunov sensitivity or by measurement precision of the system itself?',
    'Comparison of theoretical Lyapunov prediction horizon with empirical measurement noise floors across different physical systems (atmospheric, biological, mechanical)',
    'If measurement precision is the limiting factor: constraint is primarily a technical/experimental problem (Rope/Scaffold), not a fundamental mountain. If Lyapunov dominates: mountain classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observational_precision_floor, empirical, 'Whether Lyapunov sensitivity or measurement noise is the true prediction limit').

omega_variable(
    attractor_geometry_escape,
    'Do strange attractors with finite Lyapunov dimension permit statistical long-term prediction even though individual trajectories diverge?',
    'Theoretical: prove or disprove whether measure-theoretic properties (invariant measure, attractor dimension) allow ensemble predictions beyond Lyapunov horizon. Empirical: test ensemble forecasting skill on systems with known attractors.',
    'If yes: ensemble statistics escape the individual-trajectory divergence constraint — mountain is only a constraint on single-trajectory prediction (narrower scope). If no: attractor structure provides no escape — mountain classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attractor_geometry_escape, conceptual, 'Whether attractor geometry permits ensemble-level prediction beyond Lyapunov horizon').

omega_variable(
    control_and_synchronization_bypass,
    'Do control methods (chaos control, synchronization) that exploit Lyapunov stability allow functional long-term prediction or only constraint management?',
    'Analysis of control literature: what class of predictions or system behaviors become possible under active control vs passive observation? Does control change the underlying Lyapunov structure or only the stability properties of constrained trajectories?',
    'If control enables genuine prediction: the constraint is on unpowered observation only (mountain scope narrower). If control only manages trajectories without predicting future states: constraint stands universally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_and_synchronization_bypass, conceptual, 'Whether active control bypasses or only manages Lyapunov constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lyapunov_exponent_computation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lyap_tr_t0, lyapunov_exponent_computation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lyap_tr_t20, lyapunov_exponent_computation, theater_ratio, 20, 0.12).
narrative_ontology:measurement(lyap_tr_t40, lyapunov_exponent_computation, theater_ratio, 40, 0.13).

% Extraction over time
narrative_ontology:measurement(lyap_be_t0, lyapunov_exponent_computation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lyap_be_t20, lyapunov_exponent_computation, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(lyap_be_t40, lyapunov_exponent_computation, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lyapunov_exponent_computation, information_standard).
narrative_ontology:affects_constraint(lyapunov_exponent_computation, weather_prediction_horizon).
narrative_ontology:affects_constraint(lyapunov_exponent_computation, long_term_climate_modeling).
narrative_ontology:affects_constraint(lyapunov_exponent_computation, turbulence_simulation_accuracy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
