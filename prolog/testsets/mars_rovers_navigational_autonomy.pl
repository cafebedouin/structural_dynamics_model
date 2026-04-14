% ============================================================================
% CONSTRAINT STORY: mars_rovers_navigational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mars_rovers_navigational_autonomy, []).

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
 *   constraint_id: mars_rovers_navigational_autonomy
 *   human_readable: Mars Surface Navigational Autonomy (AutoNav)
 *   domain: technological/space_exploration
 *
 * SUMMARY:
 *   Mars rover navigational autonomy is constrained by a physical law:
 *   electromagnetic signal propagation across interplanetary distances at the
 *   speed of light. Earth-Mars signal latency ranges from 3 to 22 minutes
 *   (round-trip), depending on planetary positions. This delay makes
 *   real-time teleoperated control impossible — operators cannot send a drive
 *   command and receive navigation feedback within a control loop cycle. The
 *   constraint forces all Mars rovers to operate autonomously: pre-planned
 *   drive sequences loaded into onboard memory, with the rover executing
 *   hazard detection (via stereo cameras and LIDAR) and obstacle avoidance
 *   algorithms during the drive. The constraint is physically irreducible and
 *   structurally immutable. It does not extract value, suppress alternatives,
 *   or benefit specific agents — it is an environmental parameter that
 *   determines architecture. The constraint exhibits zero extractiveness from
 *   all perspectives, making it a canonical mountain.
 *
 * KEY AGENTS:
 *   - Physical Law / Speed of Light: The irreducible constraint. No agent; no beneficiary or victim.
 *   - JPL Mission Planners: Powerful agents (institutional/arbitrage) — adapt to latency by designing autonomous navigation architecture. No extraction experienced.
 *   - Rover Hardware: Equipment bearing irrecoverable risk from navigation errors (powerless/trapped). Not a victim of the latency constraint itself but exposed to risk because latency forces autonomy.
 *   - Autonomous Navigation Software: The technical solution to latency. Not an agent but the mechanism enabling rovers to operate despite latency.
 *   - Planetary Orbital Mechanics: The upstream physical determinant. Makes latency irreducible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mars_rovers_navigational_autonomy, 0.18).
domain_priors:suppression_score(mars_rovers_navigational_autonomy, 0.03).
domain_priors:theater_ratio(mars_rovers_navigational_autonomy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, extractiveness, 0.18).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mars_rovers_navigational_autonomy, mountain).
narrative_ontology:human_readable(mars_rovers_navigational_autonomy, "Mars Surface Navigational Autonomy (AutoNav)").
narrative_ontology:topic_domain(mars_rovers_navigational_autonomy, "technological/space_exploration").

domain_priors:emerges_naturally(mars_rovers_navigational_autonomy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL LAW (MOUNTAIN) — Signal latency is an irreducible constraint imposed by the speed of light. Earth-Mars distance ranges 54.6 to 401 million km, producing round-trip light delays of 3 to 22 minutes. No technology can overcome this limit. The constraint emerges naturally from relativistic physics and cannot be suppressed by engineering effort or resource investment. All observers at all power levels face identical effective latency.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MISSION PLANNING (MOUNTAIN) — Rover operators (JPL engineers) experience signal latency as an irreducible design constraint. They cannot reduce latency but can engineer around it: pre-planned drive sequences, onboard hazard detection, autonomous obstacle avoidance algorithms. The constraint forces a specific architectural choice (autonomous navigation rather than teleoperative control) but does not extract value or suppress alternatives — it is purely architectural. No effective extraction occurs; suppression is minimal because the engineering solution is well-understood.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: HARDWARE RISK (MOUNTAIN) — Rover hardware (Spirit, Opportunity, Curiosity, Perseverance) operates in an environment of extreme risk: dust storms, temperature extremes (-140°C to +40°C), radiation exposure, mechanical failure. Once a rover exceeds safe mobility limits due to navigation error, recovery is impossible — the hardware is lost. Operators face an irreducible tradeoff between autonomy (required by latency) and safety (demanded by hardware fragility). This tradeoff is not extractive; it is structural. The constraint appears as an immutable physical reality from this perspective as well.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MISSION ARCHITECTURE (MOUNTAIN) — NASA's rover program treats signal latency as a fixed parameter that determines architecture (autonomous vs teleoperated control). The institutional response is standardized: mission planners pre-load daily drive sequences, rover autonomy software executes hazard detection onboard, and operators monitor via imagery. The constraint does not benefit or harm NASA institutionally — it is simply an environmental parameter that requires specific design responses. No extraction; no suppression.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPARATIVE ANALYSIS (MOUNTAIN) — Across all planetary exploration systems, signal latency scales with distance and emerges from the speed of light invariant. Mars rovers experience 3-22 minute latency; Moon rovers experience 1-3 second latency; Earth drones experience <0.1 second latency. The relationship is deterministic and inescapable. This perspective confirms the mountain classification universally: the constraint is not a feature of Mars exploration specifically but a consequence of orbital mechanics and relativistic physics.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_rovers_navigational_autonomy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, ExtMetricName, E),
    domain_priors:suppression_score(mars_rovers_navigational_autonomy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mars_rovers_navigational_autonomy),
    narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mars_rovers_navigational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint does not extract resources, wealth, or labor from any agent. It imposes an architectural requirement (autonomous rather than teleoperated control) but does not direct value flow toward any beneficiary. The slight non-zero value (0.18 rather than 0.00) reflects the measurement error inherent in quantifying physical constants — signal latency is measured with certainty, but characterizing it as a constraint on human agency introduces minimal indexical uncertainty. Suppression (0.03): Minimal. The constraint does not suppress alternatives through coercion or resource denial. It eliminates certain options (real-time teleoperative control is impossible), but this is a physical elimination, not suppression. No agent withholds information or alternatives; the law of physics does. Theater ratio (0.15): Very low. Rover navigation is not performative. The autonomous navigation systems are functional: they move the rover safely across terrain. The slight theater reflects the pre-planned nature of drives (operators stage rehearsal before each sol's execution) but this is good engineering practice, not theatrical substitution of appearance for function.
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival gap. All observers — from the powerless rover hardware to the powerful mission planners to the analytical observer — classify the constraint identically as a mountain. This is the expected outcome for a pure physical law. The constraint exhibits invariance across all (P,T,E,S) tuples, confirming that it operates at the level of natural law rather than contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint. Mountains do not have beneficiaries or victims because they do not extract or coordinate. The constraint is not indexed to any agent's structural position — it appears identically to all observers. The latency affects all Mars rovers equally. The hardware risk affects all missions. The architectural requirement applies to all future explorers. No agent experiences directionality advantage or disadvantage relative to the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE MOUNTAIN — NO MANDATROPHY RISK. This constraint exhibits zero degrees of freedom for all indices. The classification is invariant across all perspectives. Signal latency is not a hidden coordination mechanism (like a Rope in disguise) or a sneaky extraction mechanism (like a Snare hiding as a Mountain). It is genuinely immutable. The mandatrophy that occurs in other constraint types — the risk of misidentifying extraction as coordination, or theatrical performance as function — does not apply here. The constraint stands as a baseline natural law: the kind of irreducible physical limit that justifies the mountain category's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_sufficiency_threshold,
    'What level of onboard autonomy is sufficient to ensure safe navigation given the signal latency constraint?',
    'Historical analysis of rover navigation errors (e.g., Spirit''s wheels, Opportunity''s dust storm descent); correlation between autonomy algorithm sophistication and safe drive distance per sol',
    'If threshold is low: current rover autonomy is overdesigned. If threshold is high: rovers operate at the edge of safe autonomy, and degradation of onboard sensors or software increases risk of loss.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_sufficiency_threshold, empirical, 'Threshold autonomy capability for safe Mars rover navigation').

omega_variable(
    hardware_fragility_irreducibility,
    'Is the hardware fragility (that makes navigation errors irrecoverable) a constraint imposed by current engineering or a fundamental tradeoff of planetary exploration?',
    'Engineering roadmap analysis: can redundancy, repair mechanisms, or backup systems reduce the irreversibility of navigation failures? Comparison to terrestrial robotics with repair/recovery options.',
    'If engineering can reduce fragility: the constraint becomes hybrid (Tangled Rope) — latency + manageable risk. If fragility is irreducible: mountain classification remains valid across all timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_fragility_irreducibility, empirical, 'Whether rover hardware fragility is engineerable or fundamental').

omega_variable(
    signal_latency_irreducibility,
    'Is signal latency truly immutable, or could future quantum communication or relativistic physics breakthroughs change this?',
    'Physics review: current theoretical limits on signal propagation; assessment of quantum teleportation relevance to classical navigation; long-term research horizon for faster-than-light communication.',
    'If latency is truly immutable: mountain stands forever. If theoretical path exists: classification might shift on civilizational timescales, but current classification remains valid for biographical and generational horizons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(signal_latency_irreducibility, empirical, 'Whether signal latency constraint is truly immutable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mars_rovers_navigational_autonomy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autnav_tr_t0, mars_rovers_navigational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(autnav_tr_t15, mars_rovers_navigational_autonomy, theater_ratio, 15, 0.15).
narrative_ontology:measurement(autnav_tr_t30, mars_rovers_navigational_autonomy, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(autnav_be_t0, mars_rovers_navigational_autonomy, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(autnav_be_t15, mars_rovers_navigational_autonomy, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(autnav_be_t30, mars_rovers_navigational_autonomy, base_extractiveness, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mars_rovers_navigational_autonomy, information_standard).
narrative_ontology:affects_constraint(mars_rovers_navigational_autonomy, mars_dust_storm_communication_blackout).
narrative_ontology:affects_constraint(mars_rovers_navigational_autonomy, rover_onboard_autonomy_complexity).

% DUAL FORMULATION NOTE:
% Signal latency is a single, physically irreducible constraint. No decomposition needed. The network links identify downstream constraints that depend on this mountain: dust storms exacerbate communication by blocking signals (separate constraint); onboard autonomy complexity is a necessary response to latency (not a separate constraint but a design consequence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
