% ============================================================================
% CONSTRAINT STORY: environmental_instability_as_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_environmental_instability_as_constraint, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: environmental_instability_as_constraint
 *   human_readable: Environmental Instability as Perceived Constraint
 *   domain: cognitive_psychology/decision_theory/environmental_dynamics
 *
 * SUMMARY:
 *   Environmental instability — unpredictable changes in physical, social,
 *   economic, or informational conditions — creates a perception of
 *   unchangeable constraint by compressing time horizons and preventing
 *   recognition of exit options. This constraint is unusual in the DR corpus
 *   because it appears as a mountain from ALL perspectives, including the
 *   powerless agent who typically experiences maximum extraction. The key
 *   insight: environmental volatility itself is a natural law (emergent from
 *   complex system dynamics), but the RESPONSES to volatility are not. This
 *   story models the volatility as the constraint, not the institutional or
 *   cognitive responses. Those responses — policy instability, manufactured
 *   crisis, cognitive biases amplified by stress — are separate constraints
 *   that should be modeled as distinct stories and linked via
 *   network.affects_constraints. The structural delta is that instability
 *   prevents the cognitive prerequisites for recognizing alternatives:
 *   long-term planning requires stable enough conditions to model
 *   counterfactuals, and exit option recognition requires cognitive bandwidth
 *   not consumed by immediate survival demands. The constraint's very low
 *   extractiveness (0.18) reflects that volatility itself does not extract —
 *   it is a background condition that other constraints exploit. The minimal
 *   theater ratio (0.08) reflects that environmental instability has no
 *   performative component — it is genuinely functional (or dysfunctional)
 *   with no ritual overlay.
 *
 * KEY AGENTS:
 *   - Crisis-Locked Agent: Powerless/trapped/immediate — experiences instability as total constraint; cannot perceive alternatives
 *   - Reactive Planner: Moderate/constrained/biographical — adapts to instability but cannot eliminate it; sees it as structural limit
 *   - Buffered Strategist: Powerful/mobile/generational — navigates instability through resources and planning; still cannot eliminate volatility itself
 *   - Systems Architect: Institutional/arbitrage/civilizational — builds resilience mechanisms but accepts instability as permanent feature
 *   - Analytical Observer: Analytical/analytical/universal — confirms instability as emergent property of complex systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(environmental_instability_as_constraint, 0.18).
domain_priors:suppression_score(environmental_instability_as_constraint, 0.03).
domain_priors:theater_ratio(environmental_instability_as_constraint, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(environmental_instability_as_constraint, extractiveness, 0.18).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(environmental_instability_as_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(environmental_instability_as_constraint, mountain).
narrative_ontology:human_readable(environmental_instability_as_constraint, "Environmental Instability as Perceived Constraint").
narrative_ontology:topic_domain(environmental_instability_as_constraint, "cognitive_psychology/decision_theory/environmental_dynamics").

domain_priors:emerges_naturally(environmental_instability_as_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRISIS-LOCKED AGENT (MOUNTAIN) — Immediate time horizon collapses all planning into survival mode. Environmental volatility appears as unchangeable natural law: 'things are always chaotic, nothing can be planned.' Cannot perceive exit options because instability prevents the cognitive space needed to recognize alternatives. Experiences the constraint as immutable.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REACTIVE PLANNER (MOUNTAIN) — Biographical time horizon allows some pattern recognition, but chronic instability trains reactive rather than proactive cognition. Sees environmental volatility as a structural limit on agency: 'I can adapt but not control.' Exit options exist in principle but instability makes their costs unpredictable, functionally constraining choice. Still experiences the constraint as largely immutable.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUFFERED STRATEGIST (MOUNTAIN) — Generational time horizon and resource buffers allow strategic planning despite volatility. Recognizes that environmental instability is a structural feature requiring adaptation, not a constraint that can be eliminated. Mobile exit options mean this agent can navigate instability, but the instability itself remains an unchangeable background condition. Experiences the constraint as immutable but manageable.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEMS ARCHITECT (MOUNTAIN) — Civilizational time horizon reveals that environmental instability is a permanent feature of complex adaptive systems. Institutions can build resilience mechanisms (redundancy, modularity, adaptive capacity) but cannot eliminate volatility itself. Arbitrage options allow navigation between stable and unstable domains, but the existence of instability as a category is unchangeable. Experiences the constraint as a fundamental property of reality.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Universal scope reveals environmental instability as an emergent property of complex systems with multiple interacting agents and stochastic processes. Thermodynamic constraints, information limits, and chaotic dynamics guarantee that some degree of environmental unpredictability is irreducible. This is not a false summit — the analytical perspective confirms what the powerless agent experiences: volatility itself is a natural law, though responses to it are not.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(environmental_instability_as_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(environmental_instability_as_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(environmental_instability_as_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, ExtMetricName, E),
    domain_priors:suppression_score(environmental_instability_as_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(environmental_instability_as_constraint),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(environmental_instability_as_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. Environmental instability itself does not extract — it is a background condition that creates vulnerability to extraction by other agents or constraints. The measured extractiveness represents the cognitive and planning costs imposed by volatility: compressed time horizons, reduced decision quality, opportunity costs of reactive rather than proactive strategies. This is inherent cost, not asymmetric extraction. Suppression (0.03): Minimal. Environmental instability does not suppress alternatives through coercion — it suppresses recognition of alternatives through cognitive load and time horizon compression. The suppression is a side effect of the constraint's nature, not an enforcement mechanism. Theater ratio (0.08): Minimal. Environmental volatility has no performative component. It is a genuine structural feature with real functional consequences. The small non-zero value reflects measurement noise and the fact that some agents may engage in theatrical responses to instability (crisis narratives, performative resilience), but the instability itself is not theatrical. Accessibility collapse (0.92): Very high. Once environmental instability is recognized as a structural feature rather than a temporary disruption, the constraint becomes highly accessible — it is a straightforward consequence of complex system dynamics. Resistance (0.08): Very low. Attempts to eliminate environmental volatility entirely face minimal ideological resistance because the constraint is recognized as a natural law. Resistance exists only at the margins (debates about how much volatility is reducible vs irreducible).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits ZERO perspectival gap in classification type — all five perspectives return mountain. This is the diagnostic signature of a genuine natural law constraint. The perspectival variation is in EXPERIENCED SEVERITY (chi values differ by power and exit options) and in RESPONSE CAPACITY (powerful agents can buffer, powerless agents cannot), but all agents agree that the constraint itself is unchangeable. The uniformity across perspectives is what distinguishes this constraint from false summits like 'market efficiency' or 'bureaucratic necessity,' where the analytical perspective reveals contingent institutional arrangements naturalized as laws. Here, the analytical perspective CONFIRMS the powerless agent's experience: environmental volatility is a structural feature of reality, not a policy choice or extractive mechanism. The constraint's mountain classification is robust to observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the traditional sense because environmental instability is not an agent-driven extraction mechanism. All agents experience instability as a cost, though the magnitude varies by power level and exit options. The powerless agent with immediate time horizon experiences maximum cost (d ≈ 0.95) because instability consumes all cognitive bandwidth. The moderate agent with biographical horizon experiences high cost (d ≈ 0.65) because instability disrupts planning. The powerful agent with generational horizon experiences moderate cost (d ≈ 0.48) because resources provide buffering. The institutional agent with civilizational horizon experiences low cost (d ≈ 0.15) because long time horizons average out volatility. The analytical observer (d ≈ 0.73) experiences the constraint as an object of study rather than a direct cost. All perspectives classify as mountain because the constraint is genuinely immutable — volatility is an emergent property of complex adaptive systems and cannot be eliminated, only navigated.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONFIRMATION: This constraint resolves the mandatrophy by demonstrating that not all perceived immutability is false consciousness. Environmental instability is a genuine natural law — an emergent property of complex systems with multiple interacting agents, stochastic processes, and chaotic dynamics. The analytical perspective confirms rather than contradicts the powerless agent's experience. The key to avoiding false summit misclassification is decomposition: the VOLATILITY ITSELF is a mountain (this story), but the INSTITUTIONAL RESPONSES to volatility (policy instability, manufactured crisis, extractive exploitation of vulnerability) are separate constraints that may be snares or tangled ropes. The mountain classification applies only to the irreducible stochastic component, not to the amplification mechanisms. The omega variables identify the decomposition boundaries: if measured instability is primarily policy-driven rather than thermodynamic, the constraint splits into a mountain (natural volatility floor) and a snare (artificial amplification). The current story models the floor, not the amplification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instability_source_decomposition,
    'Is the experienced instability genuinely environmental (stochastic, thermodynamic, chaotic) or artificially induced (policy volatility, institutional churn, manufactured crisis)?',
    'Decompose variance sources: natural stochastic processes vs policy-driven volatility vs extractive destabilization. Compare stability metrics across domains with similar natural volatility but different institutional arrangements.',
    'If genuinely environmental: mountain classification confirmed across all perspectives. If artificially induced: constraint decomposes into separate stories — the natural volatility (mountain) and the institutional amplification mechanism (snare or tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instability_source_decomposition, empirical, 'Whether instability is natural or artificially induced').

omega_variable(
    time_horizon_compression_mechanism,
    'Does environmental instability mechanically compress time horizons (cognitive load, survival prioritization) or does it reveal pre-existing short-term bias?',
    'Longitudinal studies tracking time horizon changes as environmental stability varies. Control for baseline time preference. Measure cognitive load and planning capacity under stable vs volatile conditions.',
    'If mechanical compression: instability is a direct constraint on cognition (mountain). If revealing pre-existing bias: instability is a stressor that exposes but does not create the constraint (the short-term bias itself may be a separate constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(time_horizon_compression_mechanism, empirical, 'Whether instability compresses time horizons or reveals existing bias').

omega_variable(
    exit_option_recognition_threshold,
    'At what level of environmental stability do agents begin to recognize exit options that were structurally available but cognitively inaccessible during instability?',
    'Experimental manipulation of perceived stability (framing, information provision, resource buffering). Measure exit option recognition rates and decision quality as stability perception varies.',
    'If threshold is low: instability is a weak constraint (agents quickly recognize options once volatility decreases). If threshold is high: instability creates persistent cognitive lock-in even after conditions stabilize (hysteresis effect suggests a separate identity_locked constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_recognition_threshold, empirical, 'Stability threshold for exit option recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(environmental_instability_as_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(env_instab_tr_t0, environmental_instability_as_constraint, theater_ratio, 0, 0.08).
narrative_ontology:measurement(env_instab_tr_t5, environmental_instability_as_constraint, theater_ratio, 5, 0.08).
narrative_ontology:measurement(env_instab_tr_t10, environmental_instability_as_constraint, theater_ratio, 10, 0.08).

% Extraction over time
narrative_ontology:measurement(env_instab_be_t0, environmental_instability_as_constraint, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(env_instab_be_t5, environmental_instability_as_constraint, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(env_instab_be_t10, environmental_instability_as_constraint, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(environmental_instability_as_constraint, information_standard).
narrative_ontology:boltzmann_floor_override(environmental_instability_as_constraint, 0.02).

% DUAL FORMULATION NOTE:
% This constraint is the natural volatility floor that other constraints exploit. Separate stories should model: (1) policy instability as extractive amplification of natural volatility (snare or tangled_rope), (2) manufactured crisis as deliberate destabilization (snare), (3) cognitive biases amplified by stress (identity_locked or constrained), (4) institutional responses that buffer or amplify volatility (rope, scaffold, or tangled_rope depending on structure). This story models ONLY the irreducible stochastic component — the thermodynamic and chaotic dynamics that guarantee some degree of environmental unpredictability regardless of institutional arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
