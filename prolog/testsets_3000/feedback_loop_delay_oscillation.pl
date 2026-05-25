% ============================================================================
% CONSTRAINT STORY: feedback_loop_delay_oscillation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feedback_loop_delay_oscillation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feedback_loop_delay_oscillation
 *   human_readable: Feedback Loop Delay Oscillation
 *   domain: systems_theory/control_dynamics
 *
 * SUMMARY:
 *   Feedback loop delay oscillation is a fundamental constraint arising from
 *   the temporal gap between sensing system state, computing corrective
 *   action, and applying that action back to the system. This constraint is
 *   ubiquitous across engineered systems (process control, power grids,
 *   network congestion, thermostat cycles, biological homeostasis) and
 *   exhibits the full spectrum of Deferential Realism types depending on
 *   observer position. The constraint creates a characteristic oscillatory
 *   behavior: the system overshoots setpoint, feedback corrects,
 *   overcorrects, oscillates around equilibrium. End users experience this as
 *   instability and wasted resources (heating-cooling cycles, network latency
 *   swings, supply chain bullwhip effects). System designers experience it as
 *   a coordination mechanism — the delay and oscillation actually prevent
 *   catastrophic overshoot in many contexts. Advanced control researchers see
 *   it as a solvable technical problem with emerging alternatives (predictive
 *   control, real-time optimization, distributed sensing). Legacy systems see
 *   it as an unchangeable feature, maintaining expensive compensatory
 *   mechanisms rather than redesigning the feedback architecture.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — experience oscillating system behavior with no capacity to modify underlying feedback structure; bear extraction cost of repeated corrective cycles
 *   - System Operators: Secondary actors (moderate/constrained) — constrained by technical barriers and standard operating procedures; also benefit from feedback's eventual corrective action
 *   - Control System Designers: Primary beneficiaries (institutional/arbitrage) — design feedback loops that balance multiple objectives; delay-induced oscillation is a feature, not a bug, from their perspective
 *   - Predictive Control Research Community: Organized agents (organized/constrained) — developing alternative feedback architectures (model predictive control, machine learning optimization) that reduce delay through anticipation rather than reaction
 *   - Legacy System Maintainers: Institutional actors (institutional/arbitrage) — preserve existing feedback-based control through maintenance budgets and operating procedures; benefit from stability through inertia
 *   - Analytical Observer: Theoretical perspective (analytical/analytical) — risks naturalizing a contingent technological constraint as an immutable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feedback_loop_delay_oscillation, 0.38).
domain_priors:suppression_score(feedback_loop_delay_oscillation, 0.42).
domain_priors:theater_ratio(feedback_loop_delay_oscillation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feedback_loop_delay_oscillation, extractiveness, 0.38).
narrative_ontology:constraint_metric(feedback_loop_delay_oscillation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(feedback_loop_delay_oscillation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feedback_loop_delay_oscillation, tangled_rope).
narrative_ontology:human_readable(feedback_loop_delay_oscillation, "Feedback Loop Delay Oscillation").
narrative_ontology:topic_domain(feedback_loop_delay_oscillation, "systems_theory/control_dynamics").

domain_priors:requires_active_enforcement(feedback_loop_delay_oscillation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feedback_loop_delay_oscillation, system_controller).
narrative_ontology:constraint_beneficiary(feedback_loop_delay_oscillation, feedback_delay_maintainers).
narrative_ontology:constraint_victim(feedback_loop_delay_oscillation, system_stability).
narrative_ontology:constraint_victim(feedback_loop_delay_oscillation, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USERS (SNARE) — Trapped in oscillating system behavior with no capacity to exit. Experiences extraction through repeated corrective cycles, wasted effort, resource drain. Cannot change the feedback loop structure that governs their experience. Maximum extraction, full suppression — all coping mechanisms remain within the oscillation.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SYSTEM OPERATOR (TANGLED ROPE) — Constrained by technical barriers and institutional procedures but also benefits from the feedback structure's coordination function: it does provide eventual corrective action, even if delayed. Can manage within bounds but at cost. Mixed experience — genuine coordination need meets asymmetric extraction burden.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTROL SYSTEM DESIGNER (ROPE) — Sees the feedback loop as a coordination mechanism solving the engineering problem of maintaining system stability over distance/time. Benefits from the delay as a buffer against over-correction; experiences the oscillation as a manageable trade-off. Can arbitrage between competing design goals.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVANCED CONTROL RESEARCH COMMUNITY (SCAFFOLD) — Organized agents (model predictive control, machine learning-based optimization) are developing alternative feedback mechanisms that anticipate system state rather than react to it, reducing inherent delay. See oscillation as a temporary coordination problem with a sunset: as predictive systems mature, reactive delay-dependent oscillation becomes obsolete.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY FEEDBACK ARCHITECTURE (PITON) — Traditional time-delayed feedback (sensor → controller → actuator → sensor) persists through institutional inertia: maintenance budgets preserve it, personnel are trained in it, replacement costs are high. The structure continues despite recognition that it creates oscillation; theater ratio reflects that much effort goes into 'managing' oscillation through fine-tuning rather than resolving the root architectural constraint.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational view, feedback delay and resulting oscillation appear as an immutable physical/informational law: causality requires time for signals to propagate, sensing latency is inherent to measurement, control action takes time to propagate through physical systems. These constraints seem unchangeable. However, the structural data contradicts full mountain classification — delay can be compensated through predictive models and real-time optimization, revealing the naturalization of a contingent technological architecture as inherent physical limit.
constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feedback_loop_delay_oscillation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feedback_loop_delay_oscillation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(feedback_loop_delay_oscillation, TR),
    TR >= 0.70.

:- end_tests(feedback_loop_delay_oscillation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The feedback delay creates genuine extraction for end users (wasted energy in heating cycles, increased network latency, supply chain amplification), but the extraction is not maximal because the feedback mechanism also provides coordination benefit — it does eventually stabilize the system. Over the 6-unit interval, extractiveness increases from 0.18 to 0.38 as more compensatory mechanisms are added to manage oscillation (fine-tuning gains, damping, buffering), each adding overhead. Suppression (0.42): Moderate. End users and operators face barriers to exit: the feedback loop is structurally embedded in the system. However, suppression is not total — higher-cost alternatives (predictive control, redesign) exist but require organizational investment. Theater ratio (0.55): Moderate-high. Much effort in legacy systems goes into managing oscillation through gain adjustment, filter tuning, buffer allocation, safety margins — activities that are performative (appear to solve the problem) rather than addressing root cause (the delay itself). As systems mature, theater increases as fine-tuning returns diminish. Claimed type: Tangled Rope. The constraint provides genuine coordination (feedback mechanism stabilizes systems) alongside asymmetric extraction (end users bear oscillation costs while controllers gain from delay as an overshoot buffer). Requires active enforcement: true — the controller must continuously adjust parameters to maintain stability within acceptable oscillation bounds.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon produces divergent classifications. The end user sees a snare — pure extraction with no escape. The designer sees a rope — coordination mechanism solving a genuine problem. The researcher sees a scaffold — temporary problem being solved by predictive control with sunset in 5-10 years. The legacy system sees a piton — performative management of oscillation through endless fine-tuning. The civilizational analyst risks seeing a mountain — delay and oscillation appear as immutable laws of physics and information. The gap reveals that 'immutable' is perspectival: from the designer's vantage, the delay is a feature; from the end user's vantage, it's an extraction mechanism. Predictive control bridges this gap by removing the need for reactive feedback delay altogether.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural relationship to the extraction flow. End users are trapped victims with no arbitrage options — they experience maximum d (0.95+), producing high f(d) and visible chi. System designers are institutional beneficiaries with arbitrage options — they experience low d (0.15-0.20), producing low or negative f(d). Operators are moderately constrained — their d is mid-range (0.55-0.65), reflecting both victim status (they must manage oscillation) and partial benefit status (they gain from coordination). The organized control research community has exit options (they can deploy new control paradigms) and constrained options within legacy systems, producing moderate d. The piton perspective reflects institutional inertia: the legacy feedback architecture maintains itself through organizational entropy and replacement cost, not because it produces low chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that feedback delay oscillation is structurally a Tangled Rope (coordination + asymmetric extraction) seen from the system level, but appears as pure extraction (Snare) to powerless end users and pure coordination (Rope) to institutional designers. The mandatrophy is resolved by recognizing these are not contradictory — they are different perspectives on the same constraint. The analytical observer's mountain (delay is immutable law) is revealed as a false summit: predictive control shows that anticipation can replace reaction, demonstrating that the delay and oscillation are contingent on reactive architecture, not inherent to feedback itself. The scaffold perspective provides the exit path: as predictive systems mature and deployment costs drop, legacy reactive-delay systems can be replaced by proactive-anticipation systems with lower oscillation and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delay_source_ambiguity,
    'Is the feedback delay primarily physical/causal (signal propagation, sensing latency, actuation lag) or institutional/technical (measurement processing, decision procedures, communication protocols)?',
    'Temporal decomposition: measure each component of the delay chain separately; distinguish hardware latency from software processing from human decision time',
    'If primarily physical: oscillation is near-immutable (mountain closer to true). If primarily institutional: delay is architecturally contingent (tangled_rope or scaffold better fit); can be reduced through system redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delay_source_ambiguity, empirical, 'Physical vs institutional sources of feedback delay').

omega_variable(
    oscillation_function_ambiguity,
    'Does the oscillation serve any stabilizing or regulatory function (e.g., prevents overshoot, distributes load, dampens transients), or is it purely parasitic extraction with no coordination benefit?',
    'Comparative analysis: simulate removal of delay component while preserving other feedback mechanisms; measure stability, overshoot, and resource utilization with and without oscillatory behavior',
    'If functional: tangled_rope confirmed (genuine coordination + extraction). If purely parasitic: classification shifts toward snare (pure extraction, no coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_function_ambiguity, empirical, 'Whether oscillation provides stabilizing function or is purely parasitic').

omega_variable(
    predictive_compensation_feasibility,
    'Can model-based predictive control reliably compensate for feedback delay, or does prediction error and system nonlinearity prevent closure of the loop?',
    'Testing of predictive control implementations across system classes; measurement of residual oscillation and failure modes when prediction diverges from reality',
    'If feasible: scaffold perspective confirmed, sunset is real, organizational replacement pathways exist. If infeasible: predictive approach fails at complexity/uncertainty scales, oscillation persists as constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictive_compensation_feasibility, empirical, 'Feasibility of predictive compensation for feedback delay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feedback_loop_delay_oscillation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbld_tr_t0, feedback_loop_delay_oscillation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fbld_tr_t2, feedback_loop_delay_oscillation, theater_ratio, 2, 0.44).
narrative_ontology:measurement(fbld_tr_t4, feedback_loop_delay_oscillation, theater_ratio, 4, 0.52).
narrative_ontology:measurement(fbld_tr_t6, feedback_loop_delay_oscillation, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(fbld_be_t0, feedback_loop_delay_oscillation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fbld_be_t2, feedback_loop_delay_oscillation, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(fbld_be_t4, feedback_loop_delay_oscillation, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(fbld_be_t6, feedback_loop_delay_oscillation, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feedback_loop_delay_oscillation, enforcement_mechanism).
narrative_ontology:affects_constraint(feedback_loop_delay_oscillation, supply_chain_bullwhip).
narrative_ontology:affects_constraint(feedback_loop_delay_oscillation, thermostat_overcorrection).
narrative_ontology:affects_constraint(feedback_loop_delay_oscillation, network_congestion_cycling).

% DUAL FORMULATION NOTE:
% Feedback loop delay oscillation is a general structural constraint that manifests domain-specifically in supply chains (bullwhip effect), thermal systems (heating/cooling cycles), network systems (congestion control), and biological homeostasis. Each domain has its own constraint story with domain-specific ε values, but all share the core mechanism of delay-induced overshoot and oscillation. This story provides the general framework; domain-specific stories decompose the concrete extraction mechanisms (inventory cascades, energy waste, packet loss, metabolic cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feedback_loop_delay_oscillation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
