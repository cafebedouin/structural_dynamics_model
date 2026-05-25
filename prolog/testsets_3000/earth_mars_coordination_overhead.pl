% ============================================================================
% CONSTRAINT STORY: earth_mars_coordination_overhead
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_earth_mars_coordination_overhead, []).

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
 *   constraint_id: earth_mars_coordination_overhead
 *   human_readable: Earth-Mars Coordination Overhead in Interplanetary Operations
 *   domain: space_exploration/interplanetary_logistics
 *
 * SUMMARY:
 *   Earth-Mars coordination overhead creates a structural tension between the
 *   immutable constraint of signal propagation (6-24 minute round-trip delay
 *   depending on orbital configuration) and the institutional response to
 *   that constraint (centralized earth-based mission control that
 *   consolidates decision authority and delays autonomous response). The
 *   constraint exemplifies how a physical limit becomes entangled with
 *   organizational extraction: the communication delay itself is immutable,
 *   but the institutional arrangements that treat the delay as justifying
 *   centralized control are contingent choices. This story decomposes into
 *   multiple structurally distinct constraints related to Mars operations:
 *   the communication delay (this constraint), the cognitive load of
 *   distributed decision-making (separate), and the cargo transport logistics
 *   (separate). This story focuses on the overhead imposed by the earth-mars
 *   synchronous coordination requirement and how it enables organizational
 *   extraction of autonomy from mars-based agents. The extractiveness value
 *   (0.38) reflects moderate extraction: genuine coordination benefits exist
 *   (earth-based computational resources, trajectory optimization expertise)
 *   alongside asymmetric decision authority (earth retains final say despite
 *   mars bearing consequences). Theater ratio (0.65) reflects that mission
 *   control processes include significant performative elements: redundant
 *   safety checks, command approval chains, and risk reviews that provide
 *   organizational legitimacy rather than direct functional improvement. The
 *   measurement trajectory shows increasing theater as more oversight
 *   processes are layered onto the core coordination problem, and
 *   extractiveness growing as earth-based mission control consolidates
 *   broader decision authority during the interval.
 *
 * KEY AGENTS:
 *   - Mars Surface Operations: Primary victim (powerless/trapped) — robots and habitats entirely dependent on earth-based decisions with 6-24 minute delay; cannot exit or negotiate
 *   - Mars Field Robotics Teams: Secondary victim (moderate/constrained) — field scientists and engineers operate rovers under earth control; can advocate for autonomy but ultimate authority remains at earth
 *   - Earth Mission Control: Primary beneficiary (institutional/arbitrage) — consolidates decision authority, operates optimized resource allocation, directs mission priorities; can reallocate resources and personnel
 *   - Distributed Autonomy Programs: Organized challengers (organized/constrained) — researchers developing autonomous systems and local decision-making; see earth-control overhead as temporary institutional constraint with sunset path
 *   - Traditional Mission Architecture: Institutional incumbent (institutional/arbitrage) — earth-centric control model persists through organizational inertia; recognizes degradation but maintains ritual structures
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks conflating immutable physics (light speed) with contingent institutional arrangements (earth-based authority structure)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(earth_mars_coordination_overhead, 0.38).
domain_priors:suppression_score(earth_mars_coordination_overhead, 0.52).
domain_priors:theater_ratio(earth_mars_coordination_overhead, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(earth_mars_coordination_overhead, extractiveness, 0.38).
narrative_ontology:constraint_metric(earth_mars_coordination_overhead, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(earth_mars_coordination_overhead, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(earth_mars_coordination_overhead, tangled_rope).
narrative_ontology:human_readable(earth_mars_coordination_overhead, "Earth-Mars Coordination Overhead in Interplanetary Operations").
narrative_ontology:topic_domain(earth_mars_coordination_overhead, "space_exploration/interplanetary_logistics").

domain_priors:requires_active_enforcement(earth_mars_coordination_overhead).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(earth_mars_coordination_overhead, earth_mission_planners).
narrative_ontology:constraint_beneficiary(earth_mars_coordination_overhead, terrestrial_infrastructure_providers).
narrative_ontology:constraint_victim(earth_mars_coordination_overhead, mars_mission_autonomy).
narrative_ontology:constraint_victim(earth_mars_coordination_overhead, operational_responsiveness).
narrative_ontology:constraint_victim(earth_mars_coordination_overhead, mars_surface_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARS SURFACE OPERATIONS (SNARE) — Trapped by 6-24 minute signal delay. Cannot execute real-time responses to emergencies or unexpected conditions. Bears full extraction cost: delayed decisions, constrained autonomy, dependency on earth-based risk assessment. Cannot exit: the constraint is structural to the orbital geometry. Maximum experienced extraction.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARS FIELD ROBOTICS TEAMS (TANGLED ROPE) — Constrained by communication lag but also benefit from earth-based computational resources, mission planning expertise, and trajectory correction capabilities. Genuine coordination function (rovers + earth planners solve problems jointly) exists alongside asymmetric extraction (earth retains decision authority). Moderately experienced extraction — significant agency but not full autonomy.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARTH MISSION CONTROL (ROPE) — Experiences coordination overhead as the infrastructure they operate and control. Can redirect resources, adjust mission parameters, reallocate personnel. Net beneficiary — the overhead consolidates decision authority at their location, enabling centralized optimization. Extraction flows toward this agent.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISTRIBUTED AUTONOMY ADVOCATES (SCAFFOLD) — Organized researchers and engineers developing on-Mars decision systems (autonomous science triage, local hazard response) see the communication lag as a temporary institutional constraint with a clear sunset. Constraints on local autonomy are expected to decline as AI capabilities mature and autonomous systems prove reliable. Theater is declining as proof-of-concept demonstrations replace theoretical debates about autonomy safety.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EARTH-CENTRIC ARCHITECTURE (PITON) — The centralized earth-control model persists through institutional inertia despite technological alternatives (autonomous rovers, distributed decision systems) becoming feasible. Mission planning rituals, safety verification processes, and organizational hierarchies are optimized around earth-based authority. The architecture sees itself as degraded — recognized as suboptimal but maintained because replacing it entails organizational restructuring and risk acceptance. High theater ratio reflects performative aspects of redundant verification and command approval chains.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a universal/civilizational perspective, the communication delay is a straightforward consequence of relativistic physics: electromagnetic signals travel at c, and Mars orbital distances produce deterministic delay. This perspective sees the constraint as immutable — a law of nature, not an institutional arrangement. However, the structural data reveal this as a false summit: the constraint's extractiveness, suppression, and theatrical elements are not properties of physics but of organizational choices about how to respond to the delay.
constraint_indexing:constraint_classification(earth_mars_coordination_overhead, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(earth_mars_coordination_overhead_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(earth_mars_coordination_overhead, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(earth_mars_coordination_overhead, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(earth_mars_coordination_overhead, TR),
    TR >= 0.70.

:- end_tests(earth_mars_coordination_overhead_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint imposes moderate extraction. Mars surface agents lose autonomy and bear delayed-response costs. Earth mission control gains decision authority and resource optimization capability. The extraction is not maximal because genuine coordination benefits exist — earth-based computation and planning expertise genuinely improve mission outcomes. However, the coordination benefits are asymmetrically distributed: earth captures planning advantage; mars bears response delay costs. Suppression (0.52): Moderate-high. Mars surface agents face structural barriers to autonomous decision-making: organizational policies enforcing earth approval, safety standards designed around earth-control, institutional risk acceptance that penalizes autonomous failure but accepts earth-control delay costs. However, suppression is not total — increasing autonomous capabilities and growing technical evidence of autonomy safety are creating exit pathways. Theater ratio (0.65): High and growing. Mission control includes performative elements: safety approval chains that add delay without reducing risk, redundant verification processes that mirror earth-based standards regardless of mars conditions, organizational rituals around earth-based authority. The theater ratio has increased from 0.40 to 0.65 over the interval as more oversight processes have been layered onto the core coordination problem, not because the coordination problem has grown more complex.
 *
 * PERSPECTIVAL GAP:
 *   The mountain perspective (speed of light as immutable law) is a false summit. The constraint's extractiveness, suppression, and theater are not properties of physics but of institutional response to physics. The signal delay is immutable; the organizational arrangements are contingent. The analytical observer who sees the constraint as natural law is naturalizing institutional choices. The real perspectival gap exists between earth-control beneficiaries (who see rope-style coordination) and mars-surface victims (who see snare-style extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Mars surface operations (trapped, victim) derive high d (~0.95), experiencing maximum extraction through f(d). Earth mission control (arbitrage, beneficiary) derives low d (~0.05), experiencing negative extraction (extraction flows toward them). Field robotics teams (constrained, mixed) derive moderate d (~0.55), experiencing moderate extraction as they have some agency but significant structural barriers. The scaffold perspective uses organized/constrained, deriving d ~0.40, reflecting their organized agency against a constraint they perceive as temporary. The piton perspective uses institutional/arbitrage, deriving low d and showing why the architecture persists despite low functional extraction — the architecture benefits earth-based institutional actors through consolidation of authority, not through dramatic extraction values.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves mandatrophy through perspectival decomposition. The mountain perspective identifies a false summit — the constraint appears immutable because the analytical observer conflates physics (delay) with institutions (earth-based control). The scaffold perspective identifies a real institutional sunset — autonomous systems are maturing, and organizational barriers to their deployment are weakening. The tangled rope classification correctly identifies the mixed coordination-extraction: earth-based mission control does provide genuine coordination benefits (computational expertise, trajectory optimization) while also consolidating decision authority that mars-based agents cannot escape. The piton perspective reveals that the traditional earth-control architecture is maintained through organizational inertia despite recognized suboptimality. The snare perspective from mars surface operations is the key diagnostic: if autonomous systems can achieve comparable or superior outcomes, the snare classification reveals extraction where institutional framing treats it as inevitable coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomous_reliability_threshold,
    'At what demonstrated reliability level does autonomous on-mars decision-making become institutionally acceptable as a substitute for earth-based control?',
    'Longitudinal comparison of autonomous rover performance vs. earth-controlled rover performance across mission-critical tasks; institutional safety standards review; incident analysis comparing autonomous vs. controlled outcomes',
    'If threshold is low (85-90% success rate): scaffold sunset becomes near-term (5-10 years), extraction overhead converts to coordination benefit. If threshold is high (99%+): scaffold timeline extends indefinitely, earth-control extraction persists. If threshold is asymmetrically applied (stricter for autonomous than earth-control): reveals institutional bias toward centralized authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomous_reliability_threshold, empirical, 'Reliability threshold for autonomous mars decision-making acceptance').

omega_variable(
    communication_redundancy_function,
    'Does multi-channel communication redundancy (relay satellites, backup pathways) represent genuine coordination function or primarily performative safety theater?',
    'Analysis of failure rates with vs. without redundancy; examination of mission success probability improvements from redundancy; comparison of redundancy costs vs. actual reliability gains',
    'If genuine coordination: suppression value should be lower (alternatives exist). If primarily theater: suppression is structural despite redundancy presence, and theater_ratio increases. Determines whether communication lag is pure constraint or mixed coordination-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communication_redundancy_function, empirical, 'Whether redundancy represents functional coordination or safety theater').

omega_variable(
    orbital_geometry_immutability,
    'Is the 6-24 minute delay truly immutable, or do alternative mission architectures (permanent relay networks, in-transit communication nodes) effectively compress the operational delay below critical thresholds?',
    'Technical feasibility studies of relay constellation concepts; cost-benefit analysis of compression approaches; timeline estimates for deployment of alternative architectures',
    'If delay is immutable: mountain classification contains validity (though the institutional response remains contingent). If delay can be compressed technologically: mountain classification is false summit; the constraint is institutional choice about investment in alternatives, not physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orbital_geometry_immutability, empirical, 'Whether communication delay is immutable or can be compressed via alternative architectures').

omega_variable(
    earth_control_necessity,
    'Does earth-based mission control solve problems that autonomous systems demonstrably cannot solve, or does it primarily enforce centralized decision authority for organizational/political reasons?',
    'Catalog of earth-control interventions; classification by necessity (actual problem-solving) vs. authority (policy enforcement); comparison with autonomous system capabilities in analogous domains (underwater robotics, deep-space probes with communication lag)',
    'If primarily problem-solving: benign coordination overhead, extraction value justified by genuine benefit. If primarily authority enforcement: reveals extraction as institutional choice, not technical necessity. Clarifies whether tangled_rope classification accurately captures mixed coordination-extraction or over-privileges coordination narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(earth_control_necessity, conceptual, 'Whether earth control provides essential problem-solving or enforces organizational authority').

omega_variable(
    risk_acceptance_asymmetry,
    'Why do earth-based mission architectures accept risk from communication delay (e.g., scientific opportunity loss, slow hazard response) while simultaneously rejecting risk from autonomous systems (despite autonomous systems operating elsewhere with acceptable safety profiles)?',
    'Comparative risk analysis: quantified failure modes and consequences for earth-control vs. autonomous approaches; institutional risk acceptance standards review; analysis of decision-making processes that approve earth-control delays but reject autonomous systems at equivalent risk levels',
    'If risk standards are genuinely comparable: institutional choices are rational and extraction overhead is justified cost. If risk standards asymmetrically penalize autonomy: reveals institutional bias protecting centralized control, indicating extraction mechanism. Directs suppression interpretation: is it structural necessity or institutional preference?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_acceptance_asymmetry, conceptual, 'Asymmetry in risk acceptance between earth-control and autonomous approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(earth_mars_coordination_overhead, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emco_tr_t0, earth_mars_coordination_overhead, theater_ratio, 0, 0.4).
narrative_ontology:measurement(emco_tr_t5, earth_mars_coordination_overhead, theater_ratio, 5, 0.58).
narrative_ontology:measurement(emco_tr_t10, earth_mars_coordination_overhead, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(emco_be_t0, earth_mars_coordination_overhead, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(emco_be_t5, earth_mars_coordination_overhead, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(emco_be_t10, earth_mars_coordination_overhead, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(earth_mars_coordination_overhead, enforcement_mechanism).
narrative_ontology:affects_constraint(earth_mars_coordination_overhead, mars_autonomous_decision_authority).
narrative_ontology:affects_constraint(earth_mars_coordination_overhead, interplanetary_mission_logistics).
narrative_ontology:affects_constraint(earth_mars_coordination_overhead, earth_mars_resource_allocation).

% DUAL FORMULATION NOTE:
% The earth-mars coordination overhead decomposes into three structurally distinct constraints: (1) this story focuses on the decision authority overhead and institutional extraction from communication delay, (2) a separate story addresses autonomous decision-making capability and the institutional barriers to deployment, (3) a third addresses logistics and resource transport constraints. The three constraints are linked by network edges representing organizational coupling and technical dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(earth_mars_coordination_overhead, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
