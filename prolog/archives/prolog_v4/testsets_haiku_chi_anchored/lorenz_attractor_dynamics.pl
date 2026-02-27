% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Lorenz attractor is the canonical mathematical object demonstrating
 *   deterministic chaos: a system governed by perfectly deterministic
 *   differential equations that produces seemingly random, unpredictable
 *   behavior. Edward Lorenz's 1963 discovery that tiny changes in initial
 *   conditions lead to completely different trajectories revolutionized
 *   understanding of predictability in dynamical systems. The constraint is
 *   not imposed by external agents or policy — it emerges necessarily from
 *   the mathematics of nonlinear dynamics. No actor chooses this constraint,
 *   no coalition could change it, and no alternative formulation eliminates
 *   it. The Lorenz system satisfies all criteria for a Mountain: base
 *   extractiveness ε=0.12 (minimal), suppression 0.02 (virtually none),
 *   accessibility_collapse 0.92 (extreme), resistance 0.08 (negligible),
 *   emerges_naturally true. From all perspectives — meteorologist, physicist,
 *   mathematician, computational scientist — the constraint appears identical
 *   and invariant. This is the exemplar case where DR's mountain
 *   classification converges across all observation sites, revealing a
 *   genuine natural law rather than an institutional arrangement or
 *   perspectival artifact.
 *
 * KEY AGENTS:
 *   - Weather Predictor: Powerless agent (powerless/trapped) — confronts absolute limit on predictability beyond 14 days
 *   - Meteorologist: Moderate practitioner (moderate/constrained) — works within constraint using ensemble methods and probabilistic frameworks
 *   - Climate Science Community: Organized institutional agent (organized/mobile) — leverages attractor structure to predict climate statistics while abandoning point predictions
 *   - Atmospheric Physics Discipline: Institutional mathematical knowledge community (institutional/arbitrage) — studies Lorenz as universal dynamical system applicable across physics and biology
 *   - Analytical Observer: Mathematical perspective (analytical/analytical) — views Lorenz as pure topological/dynamical invariant, context-independent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor_dynamics, 0.12).
domain_priors:suppression_score(lorenz_attractor_dynamics, 0.02).
domain_priors:theater_ratio(lorenz_attractor_dynamics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, 0.12).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, theater_ratio, 0.15).

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

% PERSPECTIVE 1: WEATHER PREDICTION (MOUNTAIN) — Any observer attempting to predict weather beyond the Lyapunov timescale confronts an absolute limit: sensitivity to initial conditions is a mathematical necessity, not a policy failure. No escape from this constraint exists. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: METEOROLOGIST (MOUNTAIN) — Working within the constraint (ensemble forecasting, probabilistic models) rather than against it. Cannot predict beyond ~14 days. This is not suppression of alternatives but irreducible mathematical structure. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.12.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CLIMATE SCIENCE (MOUNTAIN) — Aggregated statistics of the attractor (climate averages, bifurcation structure) remain predictable even though individual trajectories diverge. The constraint enables a different kind of knowledge: structural properties rather than point predictions. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.09.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ATMOSPHERIC PHYSICS (MOUNTAIN) — The Lorenz system is a universal object: it appears across fluid dynamics, laser physics, electrical circuits, and biological systems. Its properties (butterfly effect, strange attractor, bifurcations) are invariant mathematical facts. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the pure mathematics standpoint, the Lorenz attractor is a fixed topological/dynamical object. Its properties (existence of the attractor, sensitivity to initial conditions, strange set structure) follow necessarily from the differential equation coefficients. No context-dependence, no escape. This is the golden standard: all perspectives converge on Mountain.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(analytical),
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
 *   Extractiveness (0.12): Minimal. The Lorenz system does not extract value or resources from any agent. Its structure is a mathematical fact with no coercive force. The value 0.12 reflects only that knowledge of the constraint (understanding chaos) requires cognitive effort and time investment — the bare minimum needed to register any observability. Suppression (0.02): Negligible. There are no suppressed alternatives or coercive mechanisms. Agents are free to attempt long-term weather prediction, quantum simulations, or alternative models — the Lorenz constraint simply makes those attempts fail for the same universal reason. Theater ratio (0.15): Very low. The Lorenz system requires no performative activity to sustain itself. Its dynamics are self-evident from equations and numerical simulation. Any theatrical elements (conferences, peer review of chaos papers) are orthogonal to the constraint's structure, not constitutive of it.
 *
 * PERSPECTIVAL GAP:
 *   Unusually for a constraint story, there is no perspectival gap. All five perspectives classify as Mountain. The weather predictor sees immutable limits; the meteorologist sees them; the climate community sees them (while transcending them at different scales); the physics discipline sees universal structure; the pure mathematician sees logical necessity. This convergence is diagnostic: when all perspectives produce identical classification despite differing agent power, time horizon, and exit options, the constraint is genuinely a natural law, not a social or institutional arrangement. The absence of gap confirms that extractiveness and suppression are not being hidden by asymmetric power or information access.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation is inapplicable to a mountain constraint. There are no beneficiaries or victims because there is no extraction. All agents occupy the same structural position relative to the Lorenz system: they are subjects to a mathematical fact, not participants in a bargain. The d values (ranging from 0.08 to 0.95 across perspectives) do not drive classification differences — all perspectives produce Mountain regardless. This is by design: the index tuple (P,T,E,S) and the base metrics (ε, suppression) fully determine the classification; variations in directionality have no impact on the outcome because extractiveness is so low that effective extraction χ remains negligible even when scaled by f(d).
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY. The Lorenz attractor resolves as a pure Mountain with zero mandatrophy risk. The constraint exhibits no hybrid extraction-and-coordination structure that could be mislabeled. There is no beneficiary lobby claiming the constraint is just helpful coordination (Rope), and no victim coalition claiming it is pure extraction (Snare). The mathematical clarity of the system eliminates the source of mandatrophy entirely: there is nothing extractive to hide, nothing coordinative to fake. This constraint is a negative exemplar for mandatrophy — it is the case where mandatrophy is genuinely absent because the constraint's structure is transparent and invariant across all institutional framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_determinism_challenge,
    'Does the Lorenz attractor remain deterministic at quantum scales, or does quantum uncertainty fundamentally alter the classical sensitivity structure?',
    'Quantum trajectory analysis of atmosphere-sized systems; measurement of decoherence times vs Lyapunov timescale; experimental quantum chaos studies',
    'If quantum effects preserve determinism: mountain classification holds across scales. If quantum indeterminacy dominates at some scale: the classical Lorenz constraint is a high-level approximation with fundamental limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_determinism_challenge, empirical, 'Whether quantum mechanics preserves Lorenz determinism').

omega_variable(
    lyapunov_timescale_measurement,
    'Is the atmospheric Lyapunov timescale (~14 days) a mathematical property of the Lorenz system or an empirical artifact of Earth''s specific parameters?',
    'Variation of Lorenz parameters across exoplanet atmospheres; analysis of timescale scaling with system size, forcing, and dissipation',
    'If mathematical: the constraint is universal. If empirical artifact: sensitivity structure persists but timescales are parameter-dependent, not fundamental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lyapunov_timescale_measurement, empirical, 'Whether Lyapunov timescale is mathematical or empirical').

omega_variable(
    attractor_robustness_across_formulations,
    'Do all equivalent mathematical formulations of atmospheric convection (Lorenz 1963, Lorenz 1969, Saltzman 1962) produce structurally identical attractors and sensitivity properties?',
    'Comparative analysis of phase space topology, bifurcation diagrams, and Lyapunov spectra across formulations; identification of universal vs formulation-specific features',
    'If identical: the mountain property is robust. If formulation-dependent: the strange attractor is a modeling choice, not a discovered structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attractor_robustness_across_formulations, empirical, 'Whether attractor structure is formulation-invariant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor_dynamics, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lorenz_tr_t0, lorenz_attractor_dynamics, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lorenz_tr_t30, lorenz_attractor_dynamics, theater_ratio, 30, 0.12).
narrative_ontology:measurement(lorenz_tr_t60, lorenz_attractor_dynamics, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(lorenz_be_t0, lorenz_attractor_dynamics, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lorenz_be_t30, lorenz_attractor_dynamics, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(lorenz_be_t60, lorenz_attractor_dynamics, base_extractiveness, 60, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lorenz_attractor_dynamics, information_standard).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, weather_predictability_limits).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, climate_stability_bifurcation).
narrative_ontology:affects_constraint(lorenz_attractor_dynamics, strange_attractor_universality).

% DUAL FORMULATION NOTE:
% The Lorenz attractor is a constraint family spanning multiple formulations and contexts. The 1963 original (atmospheric convection) is the canonical instantiation, but equivalent dynamical structures appear in laser physics (Haken 1975), electrical circuits (Chua 1986), and fluid dynamics more broadly. All formulations exhibit the same topological and dynamical properties (three-dimensional strange attractor, butterfly effect, bifurcation sequence). The constraint's universality across formulations is itself a structural property worthy of study. Each domain-specific instantiation has its own measurement parameters but shares the mathematical substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
