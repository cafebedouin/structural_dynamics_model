% ============================================================================
% CONSTRAINT STORY: martian_signal_latency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: technological/scientific/space_exploration
 *
 * SUMMARY:
 *   Martian signal latency is the absolute, irreducible delay in
 *   electromagnetic communication between Earth and Mars caused by the finite
 *   speed of light (c ≈ 299,792 km/s) and the vast interplanetary distance
 *   (54.6 to 401 million km depending on orbital configuration). One-way
 *   light time ranges from approximately 3 minutes at closest approach
 *   (conjunction) to 22 minutes at maximum distance (opposition). This
 *   creates a round-trip delay of 6 to 44 minutes, making real-time
 *   teleoperation of rovers impossible and forcing Mars missions to operate
 *   with high levels of autonomous decision-making or accept significant
 *   mission complexity and risk from delayed Earth-based command loops.
 *   Unlike extractive constraints (Snare, Tangled Rope) or contingent
 *   institutional arrangements (Scaffold, Piton, Rope), Martian signal
 *   latency is a direct consequence of fundamental physics and cannot be
 *   negotiated, circumvented, or restructured through institutional
 *   innovation. It is a constraint of the natural law class: immutable,
 *   universal, and invariant across all observables. However, the operational
 *   impact of this constraint depends heavily on mission design choices
 *   (autonomous vs remote-controlled operations), raising a subtle
 *   distinction between the physical constraint itself (undeniably a
 *   Mountain) and the institutional framing of how Mars missions are designed
 *   around it.
 *
 * KEY AGENTS:
 *   - Mars Rover/Lander: Primary actor (powerless/trapped) — subject to latency; must operate autonomously or accept delayed command loops
 *   - Earth-Based Mission Control: Primary operator (institutional/arbitrage) — designs operations around the latency constraint; experiences it as a planning requirement, not as extraction
 *   - Space Agency (NASA/ESA/CNSA): Institutional beneficiary (institutional/arbitrage) — frames mission architecture to accommodate latency; benefits from long operational lifespans of autonomous rovers
 *   - Physics Observer: Analytical perspective (analytical/analytical) — sees the constraint as axiomatic to spacetime structure; latency is a logical consequence of c and distance, not a negotiable term
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(martian_signal_latency, 0.12).
domain_priors:suppression_score(martian_signal_latency, 0.03).
domain_priors:theater_ratio(martian_signal_latency, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(martian_signal_latency, extractiveness, 0.12).
narrative_ontology:constraint_metric(martian_signal_latency, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(martian_signal_latency, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(martian_signal_latency, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(martian_signal_latency, mountain).
narrative_ontology:human_readable(martian_signal_latency, "Martian Signal Latency (One-Way Light Time)").
narrative_ontology:topic_domain(martian_signal_latency, "technological/scientific/space_exploration").

domain_priors:emerges_naturally(martian_signal_latency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROVER OPERATOR (MOUNTAIN) — Trapped in real-time response loops that span 3-22 minutes one-way. Cannot negotiate around the speed of light. Must operate in autonomous mode or accept command delays that make reactive control impossible. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.17. The constraint is a pure physical limit, not a coordination problem or extraction mechanism.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SPACE AGENCY PLANNER (MOUNTAIN) — Despite powerful institutional position and significant resources, cannot change the speed of light. The constraint operates at the same level for all actors. Mobility (via relay satellites, communication networks) does not reduce the fundamental latency. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. Mountain classification is invariant across power levels.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of physics, signal latency is a direct consequence of the speed of light constant (c ≈ 299,792 km/s) and the orbital geometry of Earth-Mars distance (54.6 to 401 million km depending on orbital position). The constraint is axiomatic to spacetime structure. No amount of engineering, coordination, or institutional arrangement can overcome it. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. Mountain classification certified by Natural Law profile metrics.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
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
 *   Extractiveness (0.12): Minimal. The constraint does not extract value from any agent — it is not redistributive or coercive in the classical sense. Instead, it imposes uniform structural requirements on all Mars operations. One-way light time affects mission design equally regardless of the operator's resources or position; it is not a mechanism by which one actor gains at another's expense. The low extractiveness reflects that this is a constraint of nature, not a social mechanism. Suppression (0.03): Minimal. There are no alternative modalities being suppressed. Agents cannot choose to ignore signal latency or use workarounds; the constraint is total in its domain (electromagnetic communication across interplanetary space). However, suppression is near-zero because agents can fully perceive and plan around the constraint — it is not hidden or obscured. Theater ratio (0.15): Very low. The constraint requires minimal performative overhead. Mission planners explicitly account for latency in operational procedures; there is no illusion that real-time control is possible. Autonomy protocols are transparent. The slight theater (0.15, not 0.0) reflects minor ceremonial aspects of mission briefings and public communication about 'commanding' rovers, which implies real-time control even though operations are entirely pre-planned and autonomous.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify as Mountain, reflecting that this constraint is genuinely natural law — invariant across observables. However, the perspectives differ in their relationship to the latency. The rover operator (powerless/trapped) experiences the constraint as binding on every action — they cannot operate in real-time modes. The mission planner (powerful/mobile) experiences the constraint as a design parameter — it shapes architecture but does not prevent capability. The analytical observer (analytical/analytical) sees the constraint as axiomatic — it is a logical fact about spacetime, not a problem to be solved. No perspectival gap exists in classification (all Mountain), but the structural experience of the constraint differs by agent position. This is the expected pattern for true natural law constraints: identical classification, but different operational contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraint: Beneficiaries and victims are not applicable. The constraint imposes uniform structural requirements on all agents. Directionality does not apply to natural law — all agents are equally subject to c and distance. The three perspectives derive directionality as follows: (1) Rover operator: d≈0.95 (trapped agent experiencing the constraint as an absolute limitation), f(d)≈1.42. (2) Mission planner: d≈0.50 (balanced — powerful position can accommodate the constraint through design, but cannot eliminate it), f(d)≈0.65. (3) Analytical observer: d≈0.72 (observing the constraint as a structural property of physics), f(d)≈1.15. All three derive from the power and exit options of the observer, not from an asymmetric extraction relationship. The constraint's structure is identical for all agents; only their relationship to it differs.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY RISK. This constraint is classified as Mountain from all perspectives, and Mountain is the correct classification. The extractiveness (0.12) is well below the threshold for mandatrophy detection (ε > 0.46). The constraint does not risk being mislabeled as coordination (Rope) because there is no coordination function — signal latency creates operational requirements, not collective action problems. The constraint does not risk being mislabeled as extraction (Snare) because there is no redistribution or asymmetric coercion — the latency affects all agents uniformly. The constraint does not risk being labeled as a temporary problem (Scaffold) because the speed of light is not a temporary institutional arrangement with a sunset clause. The natural law profile metrics (accessibility_collapse=0.92, resistance=0.08, emerges_naturally=true) all confirm the Mountain classification. This constraint is a gold standard exemplar of a true natural law: immutable, universal, and invariant across all measurement bases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relay_network_sufficiency,
    'Can relay satellites in Martian orbit or at Lagrange points reduce the effective operational latency below the one-way light time threshold?',
    'Deployment and operational testing of communication relay networks (e.g., Mars Relay Network, proposed L4/L5 stations); measurement of effective decision loop closure time with relay-based autonomous command sequences',
    'If effective: operational latency can be reduced through autonomous protocols, but the underlying physical constraint (one-way light time) remains unchanged. If ineffective: one-way light time remains the binding constraint on all Mars operations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relay_network_sufficiency, empirical, 'Whether relay networks can reduce effective operational latency').

omega_variable(
    autonomous_agency_scalability,
    'Can Mars rovers be given sufficient autonomous decision-making authority to operate productively without real-time Earth oversight, effectively neutralizing the latency constraint as a practical limitation?',
    'Historical data on rover success rates with increasing autonomy levels; mission outcomes under high-autonomy protocols (e.g., Curiosity/Perseverance autonomous navigation) vs constrained protocols; analysis of tasks that require Earth-Mars loop closure',
    'If yes: latency becomes a constraint on human oversight, not on rover function — the constraint''s structural impact diminishes. If no: latency remains a binding constraint on mission complexity and scientific capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_agency_scalability, empirical, 'Whether autonomous agency can neutralize latency as a practical constraint').

omega_variable(
    physical_law_versus_institutional_contingency,
    'Is the constraint properly classified as a Mountain (immutable natural law) or as an institutionally contingent limitation (Snare or Tangled Rope) on mission design choices that could be restructured with different operational paradigms?',
    'Decomposition analysis: the speed of light is a Mountain; but the specific operational demand for real-time control is a contingent institutional choice. If missions are redesigned to operate purely autonomously (Mars base with local decision authority), the latency constraint becomes irrelevant to mission function.',
    'If purely natural law: Mountain classification holds for all observables. If institutionally contingent: separate constraint stories needed for ''speed of light'' (Mountain) vs ''real-time control requirement'' (Tangled Rope/Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_law_versus_institutional_contingency, conceptual, 'Whether this is a natural law or institutionally contingent operational constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(martian_signal_latency, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msl_tr_t0, martian_signal_latency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(msl_tr_t25, martian_signal_latency, theater_ratio, 25, 0.15).
narrative_ontology:measurement(msl_tr_t50, martian_signal_latency, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(msl_be_t0, martian_signal_latency, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(msl_be_t25, martian_signal_latency, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(msl_be_t50, martian_signal_latency, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(martian_signal_latency, information_standard).
narrative_ontology:affects_constraint(martian_signal_latency, mars_mission_autonomy_requirement).
narrative_ontology:affects_constraint(martian_signal_latency, interplanetary_communication_bandwidth).

% DUAL FORMULATION NOTE:
% Martian signal latency is a pure physical constraint (this story, ε=0.12, Mountain). It should not be confused with the institutional constraint of 'real-time control requirement' (separate story if needed, ε potentially higher, Tangled Rope or Scaffold). The speed of light is immutable; the demand for real-time oversight is contingent. These are two different constraints related by causality but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
