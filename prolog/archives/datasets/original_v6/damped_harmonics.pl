% ============================================================================
% CONSTRAINT STORY: damped_harmonics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Damped harmonic motion is a natural law constraint arising from the
 *   irreversibility of dissipative processes. An oscillatory system with
 *   energy-dissipating forces (friction, air resistance, electromagnetic
 *   damping, internal viscosity) must experience amplitude decay over time.
 *   This is not a socially constructed constraint, nor an institutional
 *   arrangement, nor a technological bottleneck. It is a structural property
 *   of systems governed by second-order linear differential equations with
 *   positive damping coefficients. The constraint emerges from thermodynamic
 *   principles and is invariant across all technological contexts: mechanical
 *   pendulums, LC electrical circuits, atomic vibrations, cosmological
 *   perturbations, and quantum oscillators all obey the same mathematical
 *   structure.
 *
 * KEY AGENTS:
 *   - Dissipative Systems: The constrained entity (all oscillatory systems with energy loss mechanisms) — must experience amplitude decay
 *   - Engineers: Design agents (moderate power, biographical horizon) — cannot prevent damping without external energy input or design alternatives
 *   - Research Communities: Organized agents (organized power, generational horizon) — have validated the universality of damping across 400+ years of experimental science
 *   - Analytical Observer: The universal perspective (analytical power, civilizational horizon) — sees damping as a necessary consequence of thermodynamic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(damped_harmonics, 0.12).
domain_priors:suppression_score(damped_harmonics, 0.02).
domain_priors:theater_ratio(damped_harmonics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(damped_harmonics, extractiveness, 0.12).
narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, 0.02).
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

% PERSPECTIVE 1: DISSIPATIVE SYSTEM (MOUNTAIN) — No exit from energy loss. All oscillatory systems with dissipative forces experience amplitude decay according to the governing differential equations. This is not a choice or institutional arrangement but a structural feature of thermodynamic reality. Zero degrees of freedom.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER (MOUNTAIN) — From the design perspective, damped harmonic motion is an immutable constraint on oscillatory system behavior. Engineers cannot prevent amplitude decay in mechanical or electrical oscillators without external energy input. The mathematical description (second-order linear ODE with positive damping coefficient) admits no alternative. The constraint is as reliable at the engineering timescale as at the civilizational timescale.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Damped harmonic motion emerges from first principles: energy conservation plus irreversible dissipation. The constraint is a necessary consequence of thermodynamic law (entropy production in dissipative processes) and cannot be circumvented by observational technique, technological innovation, or institutional design. The mountain classification is invariant across all measurement contexts and is not a false summit.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (MOUNTAIN) — Centuries of experimental and theoretical research confirm the universality of damped harmonic motion. No organized effort has ever identified an exception or found a workaround that preserves oscillation while removing dissipation. The constraint is invariant across technological generations and across different physical substrates (mechanical, electrical, acoustic, quantum).
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.12): Very low. The constraint does not 'extract' in the sense of directing resources or imposing asymmetric costs. Damping is a dissipative loss of energy to heat and other irreversible forms. The 0.12 value reflects that some fraction of the system's motion is 'lost' to dissipation, but this is not extraction by an agent — it is energy physics. Suppression (0.02): Minimal. The constraint does not suppress alternatives through coercion or institutional barriers. Suppression would apply if actors were prevented from seeking ways around the constraint; but no such prevention is needed — the constraint is mathematical. Theater ratio (0.15): Very low. There is no performative layer to damped harmonic motion. The phenomenon is precisely measurable, theoretically predictable, and experimentally verifiable. No institutional ritual or theatrical compliance is required.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify damped harmonic motion as Mountain, and all reach the same classification independently. There is no perspectival gap. The constraint is invariant across power levels, time horizons, exit options, and spatial scopes because it arises from mathematical and thermodynamic necessity rather than from structural relationships between agents. The powerless dissipative system, the moderate engineer, the organized research community, and the analytical observer all agree: the constraint is immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to natural law constraints. The constraint does not differentiate between beneficiaries and victims because it is not extractive. No agent benefits from damping; all agents subject to it experience it equally according to the governing physics. The constraint is universal — it applies to pendulums, springs, resonators, and all oscillatory systems regardless of who is building them, operating them, or measuring them.
 *
 * MANDATROPHY ANALYSIS:
 *   Damped harmonic motion resolves mandatrophy through invariance: it classifies as Mountain from all perspectives because the constraint is mathematically necessary and thermodynamically irreversible. There is no risk of misclassifying coordination as extraction or vice versa — the constraint is neither. It is a boundary condition on what physical systems can do. The omegas address genuine uncertainties (quantum zero-point exceptions, active feedback workarounds) but do not change the baseline classification. Even if one or more omegas resolve to reveal partial exceptions, the core mountain classification persists: damping is a fundamental structural feature of dissipative systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_zero_point_persistence,
    'In quantum systems, does zero-point energy persist when classical damping would predict complete amplitude decay?',
    'Quantum ground state analysis for damped oscillators; comparison of quantum mechanical predictions with classical damping envelope for low-temperature systems',
    'If zero-point energy blocks complete decay: quantum systems have a structural exemption from classical damping (changes nothing about mountain classification, but reveals quantum/classical boundary). If zero-point is merely a lower bound on classical decay: damping remains universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_zero_point_persistence, empirical, 'Whether quantum zero-point energy exempts systems from complete damping').

omega_variable(
    underdamped_critical_threshold,
    'Is the transition between underdamped and overdamped regimes a structural boundary in the damping constraint or merely a parametric variation?',
    'Mathematical analysis of the characteristic equation roots; experimental measurement of systems tuned across the underdamped/critically damped/overdamped transitions',
    'If structural boundary: two distinct constraints (underdamped oscillation persists; overdamped decay is absolute). If parametric: single constraint with continuous variation. Current classification assumes parametric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(underdamped_critical_threshold, conceptual, 'Whether underdamped/overdamped transition represents distinct constraints').

omega_variable(
    negative_damping_engineering,
    'Can active feedback systems create effective negative damping, genuinely defeating the dissipation constraint or merely pumping energy back in?',
    'Experimental test of active damping cancellation isolating feedback energy input from intrinsic system dynamics; analysis of whether negative damping requires external energy source',
    'If external energy compensates dissipation: damping constraint persists (energy conservation boundary). If genuine negative damping emerges: mountain classification is false. Current understanding: external energy is required, constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_damping_engineering, empirical, 'Whether active feedback can achieve genuine negative damping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(damped_harmonics, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(damp_tr_t0, damped_harmonics, theater_ratio, 0, 0.15).
narrative_ontology:measurement(damp_tr_t500, damped_harmonics, theater_ratio, 500, 0.15).
narrative_ontology:measurement(damp_tr_t1000, damped_harmonics, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(damp_be_t0, damped_harmonics, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(damp_be_t500, damped_harmonics, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(damp_be_t1000, damped_harmonics, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(damped_harmonics, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
