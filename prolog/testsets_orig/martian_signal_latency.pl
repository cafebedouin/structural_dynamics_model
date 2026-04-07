% ============================================================================
% CONSTRAINT STORY: martian_signal_latency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_martian_signal_latency, []).

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
 *   constraint_id: martian_signal_latency
 *   human_readable: Martian Signal Latency (One-Way Light Time)
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   Martian signal latency is the one-way communication delay between Earth
 *   and Mars caused by the finite speed of light. Current distances range
 *   from approximately 3 to 22 light-minutes, depending on orbital
 *   configuration. This is a pure natural law constraint: it emerges
 *   necessarily from special relativity and has zero degrees of freedom for
 *   any agent. Unlike institutional constraints that redistribute costs or
 *   benefits among actors, this constraint is binding identically on all
 *   parties — mission planners, rovers, NASA, ESA, and private operators all
 *   face the same absolute physics ceiling. The constraint exhibits zero
 *   suppression (cannot be hidden or worked around) and minimal theater (the
 *   latency is what it is; no performative component). Extractiveness is
 *   classified as low (0.08) because the constraint does not extract
 *   asymmetric value from any agent — it imposes symmetric costs on all
 *   operations requiring real-time feedback. The constraint is invariant
 *   across all observables: measuring signal transit time via radio waves,
 *   optical signals, or theoretical computation yields the same latency. No
 *   measurement methodology changes the underlying physics.
 *
 * KEY AGENTS:
 *   - Mars Rovers and Landers: Primary operators (powerless/trapped) — must operate within absolute latency ceiling; no exit option
 *   - Earth-Based Mission Control: Supporting operator (institutional/arbitrage) — can redesign missions but cannot overcome light-speed limit
 *   - Autonomous Systems: Emerging substitution mechanism (analytical/analytical) — onboard AI reducing decision dependency on Earth-Mars feedback loops
 *   - Analytical Physics Community: Validator (analytical/analytical) — confirms mountain classification through relativistic theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(martian_signal_latency, 0.08).
domain_priors:suppression_score(martian_signal_latency, 0.02).
domain_priors:theater_ratio(martian_signal_latency, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(martian_signal_latency, extractiveness, 0.08).
narrative_ontology:constraint_metric(martian_signal_latency, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(martian_signal_latency, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(martian_signal_latency, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(martian_signal_latency, mountain).
narrative_ontology:human_readable(martian_signal_latency, "Martian Signal Latency (One-Way Light Time)").
narrative_ontology:topic_domain(martian_signal_latency, "technological/scientific").

domain_priors:emerges_naturally(martian_signal_latency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARS MISSION OPERATIONS (MOUNTAIN) — Field operations on Mars cannot exit or negotiate with light-speed constraints. Rovers, landers, and rovers operate under absolute causality limit. No workaround exists. The constraint is immutable physics, not policy or design choice. Mission planners accept this as unalterable natural law and design within it.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of fundamental physics, one-way light time is an immediate consequence of special relativity and the constancy of the speed of light in all inertial frames. It emerges necessarily from Maxwell's equations and cannot be suppressed, worked around, or negotiated. The constraint is invariant across all measurement methodologies and observation contexts. This is natural law.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SPACE AGENCY INSTITUTIONAL ACTOR (MOUNTAIN) — Even at the institutional level, with arbitrage options in mission design and budget allocation, the signal latency constraint remains structurally immutable. NASA, ESA, and CNSA all operate within this ceiling. No institutional actor can negotiate the speed of light or purchase faster communication. The constraint is equally binding on all agents regardless of power or resources.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(martian_signal_latency_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(martian_signal_latency, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(martian_signal_latency, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(martian_signal_latency, ExtMetricName, E),
    domain_priors:suppression_score(martian_signal_latency, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(martian_signal_latency),
    narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(martian_signal_latency, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(martian_signal_latency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract value from one agent to benefit another — it imposes costs uniformly on all operations requiring real-time interaction. The 0.08 value reflects the negligible residual complexity in computing latency (relativistic Doppler shifts, orbital eccentricity effects). Suppression (0.02): Minimal. There is no suppression mechanism because there is no alternative pathway to suppress. Agents cannot hide from or negotiate with light-speed constraints. Theater ratio (0.15): Low. Communicating the latency is straightforward physics; there is minimal performative element. Mission planners present latency as a constraint to work within, not as a social construct requiring maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap — all agents classify it identically as Mountain from all observation points. The powerless rover operator sees an immutable ceiling; the institutional space agency sees the same ceiling; the analytical observer sees the same physics. This invariance across all perspectives is the signature of natural law: the structure does not depend on who is measuring or what their interests are. The constraint is not socially constructed, not negotiable, and not subject to coalition formation or institutional modification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality logic does not apply to this constraint because it is a mountain-class natural law with no asymmetric extraction. All agents experience the same constraint and no agent benefits relative to others from the constraint's existence. The latency affects all Mars operations equally — rovers, landers, orbiters, and Earth-based operations all operate within the same physics ceiling.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warp_drive_feasibility,
    'Could exotic physics (warp drives, wormholes, or faster-than-light mechanisms) violate the one-way light time ceiling?',
    'Theoretical breakthrough in general relativity; empirical detection of mechanism violating light-speed constraint; demonstration that Alcubierre or Morris-Thorne metric is physically realizable',
    'If feasible: constraint becomes rope or snare (depending on who controls access). If infeasible: mountain status confirmed indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(warp_drive_feasibility, empirical, 'Whether exotic physics could enable faster-than-light communication').

omega_variable(
    computational_autonomy_substitution,
    'Can autonomous AI systems on Mars reduce the operational impact of latency by substituting Earth-based decision-making with onboard autonomy?',
    'Historical tracking of rover autonomy levels vs mission success metrics; comparison of delay-dependent failures in human-operated vs autonomous systems; quantification of latency-induced cost reduction as autonomy increases',
    'If highly substitutable: latency ceases to be an extraction or suppression mechanism (becomes rope or scaffold for decision-making coordination). If low substitutability: constraint remains mountain for mission-critical operations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_autonomy_substitution, empirical, 'Whether autonomous systems can substitute for light-speed communication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(martian_signal_latency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marslat_tr_t0, martian_signal_latency, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marslat_tr_t5, martian_signal_latency, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marslat_tr_t10, martian_signal_latency, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(marslat_be_t0, martian_signal_latency, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(marslat_be_t5, martian_signal_latency, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(marslat_be_t10, martian_signal_latency, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(martian_signal_latency, global_infrastructure).
narrative_ontology:affects_constraint(martian_signal_latency, mars_mission_autonomy_ceiling).
narrative_ontology:affects_constraint(martian_signal_latency, earth_mars_coordination_overhead).

% DUAL FORMULATION NOTE:
% Martian signal latency is a foundational constraint upstream of multiple mission-design constraints. Mission autonomy ceilings and coordination overhead constraints are downstream effects of this latency baseline. They represent how agents adapt to the latency floor, not alternatives to it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
