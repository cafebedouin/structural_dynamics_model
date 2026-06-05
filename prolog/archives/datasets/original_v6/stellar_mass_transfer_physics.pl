% ============================================================================
% CONSTRAINT STORY: stellar_mass_transfer_physics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stellar_mass_transfer_physics, []).

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
 *   constraint_id: stellar_mass_transfer_physics
 *   human_readable: Stellar Mass Transfer Physics
 *   domain: astrophysics/stellar_dynamics
 *
 * SUMMARY:
 *   Stellar mass transfer physics describes the gravitational and dynamical
 *   processes by which matter flows from one star to a companion in a binary
 *   system. The constraint operates universally: the Roche limit defines the
 *   geometric boundary beyond which tidal forces strip material from the
 *   surface of the donor star. Orbital angular momentum conservation,
 *   hydrostatic equilibrium, and the properties of accretion disks emerge
 *   from first principles of physics and mathematics. The constraint exhibits
 *   zero degrees of freedom across all observational and theoretical
 *   perspectives. There are no beneficiaries or victims — the physics is not
 *   an institutional arrangement or a coordination mechanism but a
 *   fundamental feature of how stellar systems behave. The extractiveness
 *   value (0.12) reflects minimal theater: astrophysicists do not perform
 *   elaborate rituals to maintain mass transfer physics. The observations and
 *   models are direct expressions of the underlying phenomena.
 *
 * KEY AGENTS:
 *   - The Physics Itself: No structural relationship (analytical/universal) — the constraint is a feature of physical law, not a social or institutional arrangement
 *   - The Astrophysical Community: Observer (institutional/analytical) — researchers work within and study the constraint; no extraction or coordination function
 *   - Individual Researchers: Constrained participants (moderate/constrained) — face the unchangeable physics as a boundary on what questions can be answered
 *   - Observational Facilities: Resource holders (institutional/arbitrage) — deploy tools to measure the constraint but cannot alter its fundamental nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stellar_mass_transfer_physics, 0.12).
domain_priors:suppression_score(stellar_mass_transfer_physics, 0.03).
domain_priors:theater_ratio(stellar_mass_transfer_physics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stellar_mass_transfer_physics, extractiveness, 0.12).
narrative_ontology:constraint_metric(stellar_mass_transfer_physics, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(stellar_mass_transfer_physics, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stellar_mass_transfer_physics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(stellar_mass_transfer_physics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stellar_mass_transfer_physics, mountain).
narrative_ontology:human_readable(stellar_mass_transfer_physics, "Stellar Mass Transfer Physics").
narrative_ontology:topic_domain(stellar_mass_transfer_physics, "astrophysics/stellar_dynamics").

domain_priors:emerges_naturally(stellar_mass_transfer_physics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Mass transfer in binary star systems is governed by fundamental principles of orbital mechanics, fluid dynamics, and gravitational physics. The Roche limit (geometric constraint), orbital angular momentum conservation, and hydrostatic equilibrium are universal structural properties of stellar systems. No agent benefits from or is harmed by these constraints — they are features of physical law.
constraint_indexing:constraint_classification(stellar_mass_transfer_physics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ASTROPHYSICAL COMMUNITY (MOUNTAIN) — Regardless of observational technique, theoretical framework, or computational method, the underlying physics of mass transfer obeys invariant laws. Stellar evolution models, accretion disk dynamics, and orbital evolution follow mathematical constraints that emerge from first principles. The field has no choice but to work within these boundaries.
constraint_indexing:constraint_classification(stellar_mass_transfer_physics, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDIVIDUAL RESEARCHER (MOUNTAIN) — A researcher studying mass transfer systems cannot change the fundamental physics. They experience the Roche limit, mass loss rates, and accretion heating as unchangeable constraints on what observations are possible and what questions can be answered. The constraint is immutable even at biographical timescale.
constraint_indexing:constraint_classification(stellar_mass_transfer_physics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: OBSERVATIONAL FACILITY (MOUNTAIN) — Even with unlimited computational resources and observational access, facilities discover rather than change stellar mass transfer physics. The constraint remains immutable across all time horizons and power levels.
constraint_indexing:constraint_classification(stellar_mass_transfer_physics, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stellar_mass_transfer_physics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(stellar_mass_transfer_physics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stellar_mass_transfer_physics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(stellar_mass_transfer_physics, ExtMetricName, E),
    domain_priors:suppression_score(stellar_mass_transfer_physics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(stellar_mass_transfer_physics),
    narrative_ontology:constraint_metric(stellar_mass_transfer_physics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(stellar_mass_transfer_physics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(stellar_mass_transfer_physics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Stellar mass transfer physics exhibits no extractive relationship between agents. The coefficient represents the minimal epistemic cost of measuring and verifying the phenomenon — the 'extraction' is purely the inherent difficulty of collecting astrophysical data, not the existence of asymmetric power relationships. Suppression (0.03): Near-zero. There is no suppression mechanism preventing agents from studying mass transfer physics. No barriers to understanding or verification are imposed by one agent on another — barriers are purely technical (equipment, computational resources) and progressively reducible. Theater ratio (0.15): Minimal. The empirical and theoretical study of mass transfer physics is substantially direct and non-performative. Observations are compared against predictions; models are tested against data. No significant portion of the activity consists of maintaining the appearance of validity rather than generating actual knowledge. Accessibility collapse (0.92): Very high. Mass transfer physics is deeply integrated into stellar evolution, supernova formation, gravitational wave production, and fundamental understanding of stellar systems. The processes are universally present wherever binary stars exist; the principles are applied across diverse astrophysical contexts. Resistance (0.08): Very low. The constraint faces minimal effective resistance. Scientific consensus on the basic principles is strong; theoretical framework is mature; empirical confirmation is robust.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint. All perspectives classify identically as mountain. This uniformity is itself diagnostic: when all observers across all power levels, time horizons, and scopes perceive a constraint the same way, the constraint exhibits the invariance signature of natural law. The absence of perspectival disagreement is evidence for the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints have no directionality (no beneficiary/victim relationship). The constraint is not extractive because there is no extraction flow — no agent is gaining at another's expense. All agents (individual researchers, institutions, the astrophysical community) occupy identical structural positions relative to the physics: constrained by unchangeable natural law. This is the defining property that distinguishes mountains from all other types.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exemplifying a constraint that is genuinely a mountain — not a false summit. The natural law certification chain is satisfied: (1) emerges_naturally = true (mass transfer is a feature of how gravity works); (2) accessibility_collapse = 0.92 (the physics is universally present and deeply integrated into stellar astronomy); (3) resistance = 0.08 (the field exhibits strong consensus and robust empirical confirmation); (4) extractiveness ≤ 0.25 and suppression ≤ 0.05 (no asymmetric power relationships or coercive mechanisms). The omega variables address potential decomposition: whether the barriers to measurement are intrinsic or contingent (empirical), whether principles are truly universal or break in specific regimes (empirical), and whether the theory is mathematically complete (conceptual). Resolution of these omegas may reveal sub-constraints within specific regimes (white dwarf accretion vs black hole accretion) that should be decomposed into separate stories, but the fundamental constraint of mass transfer physics itself remains a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_accessibility_empirical,
    'Do the accessibility barriers to verifying stellar mass transfer physics arise from the physics itself or from observational/computational limits?',
    'Historical analysis of how observational and computational capabilities have evolved relative to theoretical predictions; tracking of resolved vs unresolved questions as technology improved',
    'If purely observational limitation: accessibility_collapse remains high but is contingent on technology. If fundamental to the physics: accessibility_collapse reflects intrinsic unknowability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_accessibility_empirical, empirical, 'Whether measurement barriers are intrinsic or technological').

omega_variable(
    universality_across_scales,
    'Do mass transfer principles derived from binary star systems universally apply to white dwarf mergers, neutron star accretion, and black hole systems?',
    'Cross-system empirical verification; identification of systems where predicted mass transfer behavior deviates from observation; theoretical analysis of parameter regimes where assumptions break',
    'If universal: mountain classification holds. If deviations exist in specific regimes: some aspects may degrade to rope or snare depending on whether deviations are predictable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_across_scales, empirical, 'Whether mass transfer principles are universal across stellar object types').

omega_variable(
    theoretical_completeness,
    'Is the current theoretical understanding of stellar mass transfer mathematically complete, or are there fundamental gaps?',
    'Assessment of consistency of theoretical models across different approximation regimes; identification of unresolved singular behaviors or non-converging solutions; testing against X-ray binary and common envelope observations',
    'If complete: mountain classification confirmed. If gaps exist: those aspects degrade to rope or tangled_rope depending on whether the gaps reflect unknown physics or unknown parameters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theoretical_completeness, conceptual, 'Mathematical completeness of stellar mass transfer theory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stellar_mass_transfer_physics, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smt_tr_t0, stellar_mass_transfer_physics, theater_ratio, 0, 0.12).
narrative_ontology:measurement(smt_tr_t2, stellar_mass_transfer_physics, theater_ratio, 2, 0.13).
narrative_ontology:measurement(smt_tr_t4, stellar_mass_transfer_physics, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(smt_be_t0, stellar_mass_transfer_physics, base_extractiveness, 0, 0.11).
narrative_ontology:measurement(smt_be_t2, stellar_mass_transfer_physics, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(smt_be_t4, stellar_mass_transfer_physics, base_extractiveness, 4, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stellar_mass_transfer_physics, information_standard).
narrative_ontology:affects_constraint(stellar_mass_transfer_physics, common_envelope_evolution).
narrative_ontology:affects_constraint(stellar_mass_transfer_physics, x_ray_binary_accretion).
narrative_ontology:affects_constraint(stellar_mass_transfer_physics, gravitational_wave_merger_dynamics).

% DUAL FORMULATION NOTE:
% Stellar mass transfer physics is a foundational constraint upstream of multiple astrophysical phenomena. Specific application domains (X-ray binaries, common envelope evolution, gravitational wave sources) each have their own constraint stories with higher extractiveness values reflecting observational accessibility and theoretical uncertainty. This story models the fundamental physics; downstream stories model domain-specific instantiations and measurement challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
