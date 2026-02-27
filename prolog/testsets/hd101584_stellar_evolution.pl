% ============================================================================
% CONSTRAINT STORY: hd101584_stellar_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hd101584_stellar_evolution, []).

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
 *   constraint_id: hd101584_stellar_evolution
 *   human_readable: The Gravitational Dynamics of the HD101584 Stellar System
 *   domain: physical/stellar_dynamics
 *
 * SUMMARY:
 *   The HD101584 system consists of a dying red giant primary (currently ~0.6
 *   solar masses in its core, ~10-15 solar radii) and a smaller companion
 *   star (~0.4-0.5 solar masses) in a close binary orbit. This system is in a
 *   late evolutionary stage, likely entering or already in the
 *   common-envelope or post-common-envelope phase. The gravitational dynamics
 *   governing the orbital evolution, tidal interactions, and mass transfer
 *   processes are determined entirely by classical and general relativistic
 *   gravitation — immutable laws of physics that apply universally. The
 *   system admits no alternatives, no negotiation, and no degrees of freedom
 *   in its dynamical structure. The constraint classifies as a pure Mountain
 *   from all perspectives: the gravitational geometry of the system is an
 *   irreducible physical fact.
 *
 * KEY AGENTS:
 *   - Observational Astronomers: Powerful/mobile agents who can choose observation strategies and instruments, yet cannot alter the gravitational dynamics
 *   - Theoretical Physicists: Analytical agents who can refine models of the system but cannot change the underlying laws
 *   - Space Mission Planners: Organized/constrained agents whose design choices must defer to the immutable gravitational structure
 *   - Gravitational Field: The constraint itself — a natural emergence of spacetime geometry and mass distribution
 *   - Computational Models: Tools that approximate but do not modify the real system dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hd101584_stellar_evolution, 0.08).
domain_priors:suppression_score(hd101584_stellar_evolution, 0.03).
domain_priors:theater_ratio(hd101584_stellar_evolution, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, extractiveness, 0.08).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hd101584_stellar_evolution, mountain).
narrative_ontology:human_readable(hd101584_stellar_evolution, "The Gravitational Dynamics of the HD101584 Stellar System").
narrative_ontology:topic_domain(hd101584_stellar_evolution, "physical/stellar_dynamics").

domain_priors:emerges_naturally(hd101584_stellar_evolution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASTROPHYSICAL OBSERVER (MOUNTAIN) — From the universal/civilizational scale, the gravitational dynamics of HD101584 emerge directly from general relativity and Newtonian gravitation. The constraint is immutable: the geometry of spacetime and the distribution of mass determine orbital trajectories with zero degrees of freedom for modification. The system's behavior cannot be negotiated, circumvented, or reframed. It is a pure manifestation of natural law.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: OBSERVATIONAL ASTRONOMER (MOUNTAIN) — Even with advanced telescopes, spectroscopy, and computational power, the observer cannot alter or escape the gravitational dynamics of HD101584. The constraint presents as an irreducible physical fact. Measurements may improve, but the underlying dynamics remain fixed. The astronomer has mobility and power in choosing observation strategies, yet the system itself admits no alternatives.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THEORETICAL PHYSICIST (MOUNTAIN) — Working within a human biographical timescale and global observational reach, the theoretical physicist encounters the HD101584 dynamics as a deterministic system governed by gravitational laws. While computational models approximate the system, they do not modify it. The constraint is immutable across all accessible theoretical frameworks within known physics.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SPACE AGENCY MISSION PLANNER (MOUNTAIN) — Institutional actors planning space missions must accept the gravitational dynamics as fixed constraints that determine fuel requirements, trajectory calculations, and mission feasibility. No institutional reorganization, funding allocation, or political decision can alter the gravitational geometry of HD101584. The constraint is external and binding.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hd101584_stellar_evolution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, ExtMetricName, E),
    domain_priors:suppression_score(hd101584_stellar_evolution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hd101584_stellar_evolution),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hd101584_stellar_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The gravitational constraint imposes no extraction in the economic or institutional sense. It is a pure natural law with no beneficiary class and no victim class. The low value reflects the mountain criterion: ε ≤ 0.25 and no extractive mechanism. Suppression (0.03): Minimal. There is no coercion of alternatives — the system has zero degrees of freedom, not because alternatives are suppressed, but because only one trajectory is possible. Theater ratio (0.15): Low. Observational astronomy involves some representational interpretation (spectroscopic data must be calibrated, numerical integrations approximate real orbits), but the underlying dynamics are transparent to physical law. No performative activity masks the constraint; it is what it is. Accessibility collapse (0.92): Very high. The constraint is completely accessible to physical measurement and theoretical understanding within the frameworks of gravitation. There is no hidden layer. Resistance (0.08): Very low. The constraint cannot be resisted. It operates identically for all observers regardless of power, intent, or institutional position.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint. All observers — regardless of power level, time horizon, exit options, or spatial scope — encounter the same immutable gravitational dynamics. The astrophysicist with lifetime expertise, the amateur astronomer, the space agency planner, and the theoretical physicist all face the identical constraint: the system evolves according to general relativity and gravitation, period. This uniformity across all perspectives is the defining characteristic of a true Mountain. Unlike the verification_bottleneck example (which showed six different types from different perspectives), HD101584 shows six Mountain classifications. The invariance itself is the diagnostic result.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy present. The constraint classifies as Mountain from all perspectives with consistent base properties. There is no risk of miscategorizing coordination as extraction or vice versa — there is no coordination function and no extraction mechanism. The system is pure natural law. The uniform Mountain classification is not a failure of the indexical system; it is the correct recognition that some constraints are genuinely invariant across all observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tertiary_companion_detection,
    'Does a previously undetected tertiary companion or planetary mass object orbit within the HD101584 system, potentially affecting the dynamical structure?',
    'High-resolution spectroscopy, astrometric precision measurements from Gaia or future missions, radial velocity monitoring over extended periods',
    'If tertiary body exists: adds complexity to dynamics but does not change fundamental classification — still a mountain of gravitation. If absent: confirms binary-only model; dynamics remain immutable natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tertiary_companion_detection, empirical, 'Presence of undetected tertiary mass in HD101584 system').

omega_variable(
    relativistic_frame_dragging_measurability,
    'Are general relativistic frame-dragging effects measurable in the HD101584 system given the stellar masses and orbital configuration, or is Newtonian gravity fully sufficient?',
    'Precise long-term orbital monitoring; comparison of predicted Newtonian trajectories against observations; future gravitational wave detection if applicable to the system dynamics',
    'If GR effects measurable: demonstrates the mountain extends into relativistic regime. If Newtonian sufficient: mountain is rooted in classical gravitation but extends into GR framework identically. Either way: immutable natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relativistic_frame_dragging_measurability, empirical, 'Measurability of general relativistic effects in HD101584 dynamics').

omega_variable(
    stellar_oscillation_coupling,
    'Do stellar oscillations or internal circulation patterns in the red giant affect orbital dynamics through tidal coupling, or are rotational/pulsation effects negligible compared to orbital gravity?',
    'Asteroseismic analysis of the red giant; tidal heating calculations; comparison of predicted orbital precession with observations over decades',
    'If coupling significant: adds detail to the dynamical model but does not remove the mountain classification — the coupled system remains immutable. If negligible: simplifies the model; mountain classification unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stellar_oscillation_coupling, empirical, 'Extent of tidal coupling between stellar structure and orbital dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hd101584_stellar_evolution, 0, 5000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hd101584_tr_t0, hd101584_stellar_evolution, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hd101584_tr_t2500, hd101584_stellar_evolution, theater_ratio, 2500, 0.15).
narrative_ontology:measurement(hd101584_tr_t5000, hd101584_stellar_evolution, theater_ratio, 5000, 0.15).

% Extraction over time
narrative_ontology:measurement(hd101584_be_t0, hd101584_stellar_evolution, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hd101584_be_t2500, hd101584_stellar_evolution, base_extractiveness, 2500, 0.08).
narrative_ontology:measurement(hd101584_be_t5000, hd101584_stellar_evolution, base_extractiveness, 5000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hd101584_stellar_evolution, global_infrastructure).
narrative_ontology:affects_constraint(hd101584_stellar_evolution, common_envelope_dynamics).
narrative_ontology:affects_constraint(hd101584_stellar_evolution, stellar_mass_transfer_physics).

% DUAL FORMULATION NOTE:
% HD101584 is a specific astronomical system whose gravitational dynamics are downstream of more general constraints: the laws of gravitation (mountain), stellar evolution physics (mountain), and orbital mechanics (mountain). This story treats the specific system instance as a mountain to validate that the indexical classification correctly identifies natural law constraints when all structural data point to immutability and zero institutional degrees of freedom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
