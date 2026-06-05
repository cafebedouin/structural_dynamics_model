% ============================================================================
% CONSTRAINT STORY: multi_planetary_latency_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multi_planetary_latency_lock, []).

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
 *   constraint_id: multi_planetary_latency_lock
 *   human_readable: Earth-Centric Interplanetary Clearing
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The speed of light creates an irreducible communication latency between
 *   Earth and Mars of 3-22 minutes depending on orbital phase. This
 *   constraint is a mountain — a structural property of spacetime itself, not
 *   an institutional arrangement that could be reformed or escaped. Unlike
 *   other technological constraints that emerge from human choices
 *   (regulatory frameworks, infrastructure decisions, economic incentives),
 *   this constraint is a law of physics. It applies uniformly to all agents
 *   regardless of power, wealth, or institutional authority. As human
 *   civilization expands multiplanetarily, the latency becomes a structural
 *   feature of all Earth-Mars coordination: financial transactions, mission
 *   control, emergency response, real-time communication with loved ones, and
 *   inter-organizational governance. The constraint is not extracted by any
 *   agent — no one benefits from the latency itself. It is not suppressed in
 *   the sense of institutional coercion — the latency exists regardless of
 *   anyone's preferences. It is simply an immutable property of the universe.
 *
 * KEY AGENTS:
 *   - Physics itself: The law of light-speed invariance in special relativity — not an agent, but the source of the constraint
 *   - Mars Settlement Authority: Institutional authority managing Mars operations (powerful/mobile) — must design all systems to accommodate latency
 *   - Real-Time Operations Teams: Earth-based mission control personnel (institutional/trapped) — cannot operate rovers in real time; must use autonomous pre-programming
 *   - Mars-Based Workers: Human and robotic inhabitants of Mars (moderate/constrained) — experience communication latency as a fact of life, not institutional suppression
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as an immutable property of spacetime, invariant across all observables
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multi_planetary_latency_lock, 0.08).
domain_priors:suppression_score(multi_planetary_latency_lock, 0.02).
domain_priors:theater_ratio(multi_planetary_latency_lock, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multi_planetary_latency_lock, extractiveness, 0.08).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(multi_planetary_latency_lock, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multi_planetary_latency_lock, mountain).
narrative_ontology:human_readable(multi_planetary_latency_lock, "Earth-Centric Interplanetary Clearing").
narrative_ontology:topic_domain(multi_planetary_latency_lock, "technological/economic").

domain_priors:emerges_naturally(multi_planetary_latency_lock).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICS OBSERVER (MOUNTAIN) — The speed of light c ≈ 300,000 km/s is a fundamental constant of physics. Earth-Mars distance (54-401 million km) creates light-travel time of 3-22 minutes depending on orbital phase. This is a physical law, not a policy choice. No agent can negotiate with the speed of light. ε=0.08, emerges_naturally=true, accessibility_collapse=0.92, resistance=0.08.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MARS SETTLEMENT AUTHORITY (MOUNTAIN) — Even the most powerful interplanetary authority cannot negotiate light-speed delays. Mars rovers and autonomous systems must operate with 3-22 minute decision latency. This is not suppression or extraction — it is a structural property of spacetime. No coercion is involved; no alternatives exist. Classification remains mountain across all power atoms because the constraint is invariant to agent capability.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: REAL-TIME OPERATIONS TEAM (MOUNTAIN) — Mission control operators cannot control Mars robots in real time. A command sent from Earth takes 3-22 minutes to arrive; the response takes another 3-22 minutes to return. Robots must be autonomous or pre-programmed. This is not a constraint they suffer — it is a law they work within. The latency is physical, not institutional. No suppression (institutional barriers do not exist), no extraction (no agent benefits from the delay itself).
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MARS-BASED WORKER (MOUNTAIN) — A person working on Mars experiences 6-44 minute round-trip latency for Earth communication. They cannot hold a real-time conversation with Earth-based friends or family. This is not because anyone is suppressing them — it is because light travels at a fixed speed. The constraint is physical. No agent benefits from the delay (suppression=0). No alternative pathway exists (resistance=0.08, near-zero).
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multi_planetary_latency_lock_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, ExtMetricName, E),
    domain_priors:suppression_score(multi_planetary_latency_lock, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(multi_planetary_latency_lock),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(multi_planetary_latency_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The light-speed latency does not extract value from one agent to benefit another. No agent gains rent or advantage from the delay itself. The latency is symmetric — it affects all Earth-Mars communication equally. Some economic analysis might argue that Earth-based institutions benefit from control leverage (Mars operators must wait for Earth approval), but this is secondary institutional arrangement layered on top of the physical constraint, not intrinsic to it. The 0.08 value reflects residual institutional overhead required to manage the constraint, not extraction. Suppression (0.02): Negligible. There are no institutional barriers, no coercion, no lack of alternatives — there are simply no alternatives. Suppression measures institutional resistance or coercion; physics imposes no such friction. The 0.02 reflects measurement noise and minimal administrative overhead. Theater ratio (0.05): Negligible. The latency is entirely functional — communication delay is not performative; it is material. There is no theatrical replacement of real function. The 0.05 reflects only minimal meta-communication overhead (confirmations, retransmissions). Accessibility collapse (0.92): Very high. The constraint is utterly inaccessible to human manipulation — no amount of engineering, wealth, or political will can overcome light-speed limits. The only possible collapse would be a discovery of physics beyond Einstein (FTL, wormholes), which has zero historical precedent and contradicts 130 years of empirical validation. Resistance (0.08): Negligible. There is zero institutional resistance to the constraint — everyone acknowledges and works within it. The 0.08 reflects only measurement noise and rare edge cases where someone temporarily denies the constraint.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, this one produces ZERO perspectival gap. All agents, regardless of power, exit options, or time horizon, classify it identically as mountain. This is the signature of a true natural law: the classification is invariant across all indexical tuples. The Mars Settlement Authority sees mountain. The powerless Mars worker sees mountain. The analytical observer sees mountain. There is no disagreement about the constraint's nature because its nature is physics, not sociology. The perspectival invariance is the strongest possible evidence for mountain classification and the cleanest validation of the NL profile gates (accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no asymmetric extraction and no beneficiary/victim structure. The latency affects all agents identically. The derived d values (if computed via the canonical fallback) are immaterial to the classification — the constraint remains mountain regardless of agent power because the base extraction (0.08) and suppression (0.02) satisfy the mountain thresholds independent of f(d) and σ(S). This is a key feature of mountains: they are invariant under directionality transformation. You cannot negotiate with physics by changing who holds power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wormhole_or_ftl_discovery,
    'Could a fundamental physics discovery (wormholes, warp drives, quantum teleportation) bypass the light-speed limit and collapse this constraint?',
    'Empirical discovery in fundamental physics; theoretical proof or disproof of causality-preserving FTL mechanisms',
    'If FTL is possible: constraint shifts from mountain to rope (cleared by new technology). If FTL is impossible: constraint remains mountain forever (civilizational timescale).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wormhole_or_ftl_discovery, empirical, 'Whether FTL communication is theoretically possible').

omega_variable(
    mars_independence_trajectory,
    'As Mars develops self-sufficient settlements, will the latency constraint shift from immutable law to coordination problem (rope)?',
    'Historical analysis of settlement autonomy growth; determination of whether Mars economic/political independence reduces Earth-centric clearing requirements',
    'If Mars becomes fully independent: latency becomes a structural feature of two-body problem (rope, not mountain). If Earth maintains financial/political control: latency remains extractive bottleneck (mountain from unified perspective). Classification may decompose into two stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mars_independence_trajectory, conceptual, 'Whether Mars independence reframes latency from law to coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multi_planetary_latency_lock, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mpl_tr_t0, multi_planetary_latency_lock, theater_ratio, 0, 0.04).
narrative_ontology:measurement(mpl_tr_t50, multi_planetary_latency_lock, theater_ratio, 50, 0.05).
narrative_ontology:measurement(mpl_tr_t100, multi_planetary_latency_lock, theater_ratio, 100, 0.06).

% Extraction over time
narrative_ontology:measurement(mpl_be_t0, multi_planetary_latency_lock, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mpl_be_t50, multi_planetary_latency_lock, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(mpl_be_t100, multi_planetary_latency_lock, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multi_planetary_latency_lock, global_infrastructure).
narrative_ontology:affects_constraint(multi_planetary_latency_lock, mars_financial_settlement_delay).
narrative_ontology:affects_constraint(multi_planetary_latency_lock, mars_autonomous_governance_emergence).
narrative_ontology:affects_constraint(multi_planetary_latency_lock, interplanetary_supply_chain_buffering).

% DUAL FORMULATION NOTE:
% The speed-of-light latency is a single, unified constraint across all Earth-Mars interactions. It does not decompose into multiple constraints with different ε values — the physics is invariant. However, the *institutional responses* to the constraint (financial clearing mechanisms, governance structures, supply chain buffering) do generate separate constraint stories that are downstream of and affected by this physical limit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
