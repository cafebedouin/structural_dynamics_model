% ============================================================================
% CONSTRAINT STORY: mars_rovers_navigational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Mars Surface Navigational Autonomy (AutoNav) Constraint
 *   domain: technological/robotics/planetary_exploration
 *
 * SUMMARY:
 *   Mars rovers operate under an inescapable physical constraint: the finite
 *   speed of light. Signal latency to Mars ranges from 3 minutes (closest
 *   approach) to 22 minutes (maximum separation), creating a one-way
 *   communication delay of 6 to 44 minutes round-trip. This constraint is not
 *   institutional, economic, or political — it is inscribed in spacetime
 *   itself. No rover can receive real-time commands from Earth. All
 *   navigation autonomy must be pre-loaded into onboard computer systems. The
 *   constraint exhibits the key signature of a Mountain: it cannot be
 *   negotiated, suppressed, or overcome through organizational effort. Rovers
 *   (Spirit, Opportunity, Perseverance) are designed with sophisticated
 *   onboard autonomy precisely because the constraint is immutable. This is
 *   not a limitation begrudgingly accepted; it is a law of physics that
 *   engineers work *with*, not against. The minimal theater ratio (0.15)
 *   reflects that the onboard autonomy is genuine function, not performance:
 *   hazard detection uses real sensor data, pathfinding uses actual terrain
 *   classification, and the rover's actions have direct consequences. There
 *   is no pretense or ritual—only physics and engineering.
 *
 * KEY AGENTS:
 *   - Signal propagation physics: Immutable constraint (light-speed limit) — defines the problem space
 *   - Mars rover hardware: Constrained actor (powerless/trapped) — must execute autonomy because latency forbids real-time control
 *   - Rover engineering team: Problem-solver (organized/mobile) — designs onboard autonomy as the solution; no exit from the constraint itself
 *   - Mission commander/NASA JPL: Institutional beneficiary (institutional/arbitrage) — benefits from latency constraint's forcing function (disciplined planning, lower single-point-of-failure risk)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — recognizes the constraint as a fundamental law of physics
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
narrative_ontology:human_readable(mars_rovers_navigational_autonomy, "Mars Surface Navigational Autonomy (AutoNav) Constraint").
narrative_ontology:topic_domain(mars_rovers_navigational_autonomy, "technological/robotics/planetary_exploration").

domain_priors:emerges_naturally(mars_rovers_navigational_autonomy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / RELATIVISTIC CONSTRAINT (MOUNTAIN) — Signal propagation to Mars operates at light speed; the one-way latency ranges 3-22 minutes depending on orbital geometry. This is an irreducible physical law. No technological advance can exceed the speed of light. ε=0.18, suppression=0.03 (minimal coercive overhead; it's simply physics). accessibility_collapse≥0.85 (confirmed). emergence is natural (confirmed). Resistance≤0.15 (confirmed). This perspective is Mountain from all index positions.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MISSION COMMANDER / OPERATIONAL CONSTRAINT (MOUNTAIN) — Even possessing unlimited budget and engineering capability, the commander cannot transmit a command and receive feedback within a single Sol. The constraint is not institutional or political — it is inscribed in spacetime. All rovers must operate with pre-loaded waypoint sequences and onboard autonomous obstacle avoidance. This is not a choice; it is imposed by physics. d≈0.50 (symmetric: constraint affects commander equally with engineers), f(d)≈0.65, σ=1.0 → χ≈0.12. Still firmly mountain: even at maximum structural power, the constraint is immutable.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ROVER ENGINEERING TEAM / TECHNICAL DESIGN (MOUNTAIN) — The constraint forces a specific architectural solution: rovers must carry sufficient onboard computation (vision systems, hazard avoidance, terrain classification) to navigate autonomously for 8-12 hours between ground-command upload windows. This is not optional; it emerges from the signal latency. The team's exit option is 'mobile' only in the sense that they could theoretically use radio relays, orbital satellites, or pre-computed routes — but these do not escape the latency constraint, they only distribute the problem differently. The constraint is invariant across architectural choices. ε=0.18, suppression=0.03 (no suppression of alternatives because the constraint is not enforced coercively). Mountain classification holds.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SPACE AGENCY / INSTITUTIONAL VIEW (ROPE) — From the agency perspective, the latency constraint is a solved coordination problem. NASA/JPL have developed proven techniques: pre-loaded drive commands, hazard detection vision, dead-reckoning localization. The constraint is reframed as a coordination mechanism: it forces disciplined operational planning (staging commands, testing in testbeds, validating sensor outputs) rather than reactive teleoperation. The agency sees this as coordination, not extraction. Beneficiary: mission success rate (higher autonomy → lower single-point-of-failure risk). Victim: none (the agency coordinated around the constraint and benefits from its forcing function). d≈0.15 (beneficiary + arbitrage), f(d)≈-0.01, σ=1.2 → χ≈-0.002. Negative effective extraction = coordination only. Classification: Rope (or arguably natural-law-aware rope, but schema allows pure rope).
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ROVER HARDWARE / PHYSICS PERSPECTIVE (MOUNTAIN) — The rover itself experiences the constraint as absolute. It cannot receive real-time commands. It must decide when to stop, when to avoid rocks, when to retreat from steep terrain. The hardware design (onboard CPU, vision processing, propulsion controllers) is entirely structured around this inescapable constraint. From the rover's perspective (however we anthropomorphize it), there is no exit, no negotiation, no coercion — only an immutable physical fact. ε=0.18, suppression≈0 (no suppression needed; the constraint enforces itself through physics). This is the clearest mountain perspective: a pure physical limit.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_rovers_navigational_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

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
 *   Extractiveness (0.18): Low, as required for Mountain classification. This value reflects the extractiveness of the constraint qua constraint — not the cost of solving it (which is high), but the structural extraction of autonomy capacity from the rover. The rover cannot choose whether to be autonomous; latency forces it. However, this 'extraction' is purely structural, not coercive. No agent is suppressing alternatives. The extractiveness is the irreducible portion of the constraint's logical structure. Suppression (0.03): Minimal, as required for Mountain classification. There is no coercive enforcement because the constraint enforces itself via physics. No organization maintains it; no suppression of alternatives is needed. Engineers are free to propose any alternative (quantum entanglement, relay networks, etc.), and the constraint remains indifferent to proposals. Theater ratio (0.15): Very low. Onboard autonomy is genuine function, not performance. The rover's hazard avoidance processes actual sensor data; its pathfinding uses real-time terrain classification. There is no ritual, no theatrical display — only authentic problem-solving under a real constraint. The slight rise over time (0.12 → 0.15) reflects minor increases in mission complexity and sensor fusion processing, not degradation into theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is subtle because the constraint is truly immutable across all observables. However, the gap exists in *meaning*, not structure. The physicist sees a law of nature (relativity). The mission commander sees a design requirement (onboard autonomy). The engineering team sees a technical challenge (vision systems, hazard detection). The space agency sees a forcing function for disciplined operations (coordination benefit). The rover sees a brute physical fact (no real-time control possible). All perspectives converge on Mountain classification because the constraint is genuinely invariant. The perspectival gap is not about disagreement but about different framings of the same immutable reality. This is the hallmark of a true Mountain: all perspectives, when honestly rendered, arrive at the same structural conclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Mars rovers (powerless/trapped): d≈0.95. The rover has no exit from the constraint and no choice in autonomy. However, this is not 'victimization' — the rover is a tool designed to operate under these conditions. Mission commander (powerful/constrained): d≈0.50. Even unlimited power cannot overcome light-speed limits. The constraint is symmetric from the commander's perspective: it applies universally, not asymmetrically. Rover engineering team (organized/mobile): d≈0.40. The team can propose alternatives (relay networks, quantum entanglement), but the constraint remains immutable regardless. These alternatives would not escape the constraint; they would redistribute it. Space agency (institutional/arbitrage): d≈0.15. The agency benefits from the constraint's forcing function (disciplined operations), so it is a beneficiary. The derived d values are consistent with Mountain classification across all perspectives — none produce high chi because suppression is minimal and the constraint is not extractive in the coercive sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_entanglement_communication,
    'Could quantum entanglement be leveraged to transmit control information to Mars rovers instantaneously or at FTL speeds, circumventing light-speed latency?',
    'Theoretical physics research on no-communication theorem and whether it definitively rules out FTL information transfer via entanglement; any positive breakthrough would require re-evaluation of the constraint''s mountain status',
    'If entanglement communication becomes viable: constraint degrades from Mountain to Rope or Scaffold (technology-dependent). If no-communication theorem is provably airtight: constraint remains Mountain with certainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_entanglement_communication, empirical, 'Quantum entanglement communication feasibility').

omega_variable(
    relay_satellite_network_completeness,
    'Does a Mars orbital relay satellite network reduce autonomous navigation requirements sufficiently to become a functional alternative to onboard autonomy, or does latency remain the bottleneck?',
    'Simulation and deployment testing of Mars Relay Network concepts; measurement of latency reduction and control bandwidth improvement; comparison with terrestrial UAV autonomy architectures that use relay networks',
    'If relay network effectively reduces latency to sub-3-minute range: constraint shifts from Mountain to Rope (coordination of multi-rover systems via relay). If latency reduction is marginal: constraint remains Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relay_satellite_network_completeness, empirical, 'Relay satellite network latency reduction effectiveness').

omega_variable(
    human_cognitive_latency_equivalence,
    'Is the human operator''s ability to make real-time decisions equivalent to or better than pre-programmed rover autonomy when latency prevents real-time teleoperation?',
    'Comparative mission data: success rates of autonomously navigated segments vs. segments using pre-computed commands; failure analysis of collisions, immobilization, and navigation errors',
    'If pre-loaded autonomy is superior: constraint reinforces mountain classification (no human option available). If human planning is superior: constraint shifts toward coordination (humans and rovers coordinate via staging discipline rather than real-time control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_cognitive_latency_equivalence, empirical, 'Autonomy effectiveness vs human-planned command sequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mars_rovers_navigational_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marsnav_tr_t0, mars_rovers_navigational_autonomy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marsnav_tr_t5, mars_rovers_navigational_autonomy, theater_ratio, 5, 0.14).
narrative_ontology:measurement(marsnav_tr_t10, mars_rovers_navigational_autonomy, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(marsnav_be_t0, mars_rovers_navigational_autonomy, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(marsnav_be_t5, mars_rovers_navigational_autonomy, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(marsnav_be_t10, mars_rovers_navigational_autonomy, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mars_rovers_navigational_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(mars_rovers_navigational_autonomy, mars_dust_storm_visibility).
narrative_ontology:affects_constraint(mars_rovers_navigational_autonomy, power_generation_solar_mars).

% DUAL FORMULATION NOTE:
% The navigational autonomy constraint is downstream of more fundamental physical constraints (signal latency, power limitations, sensor noise). The latency constraint (signal propagation) is the primary structural limit; autonomy design is the human response to that limit. The power and dust constraints create secondary pressures on autonomy (energy budget for computation, visibility for hazard detection) but do not replace the latency constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
