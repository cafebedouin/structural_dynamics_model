% ============================================================================
% CONSTRAINT STORY: rover_onboard_autonomy_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rover_onboard_autonomy_complexity, []).

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
 *   constraint_id: rover_onboard_autonomy_complexity
 *   human_readable: Rover Onboard Autonomy Complexity Constraint
 *   domain: robotics/space_exploration/autonomous_systems
 *
 * SUMMARY:
 *   Rover onboard autonomy complexity presents a structural constraint where
 *   genuine coordination challenges (managing rover behavior across 20+
 *   minute communication delays, integrating diverse sensor modalities,
 *   operating under severe thermal and power budgets) are layered with
 *   institutional extraction mechanisms (centralized mission control
 *   authority, risk-averse doctrine, vendor dependency). The constraint
 *   appears as immutable technical law (communication latency, computational
 *   complexity) from the analytical perspective, but manifests as
 *   organizational choice and career incentive structure from institutional
 *   and operational perspectives. The constraint exhibits tangled rope
 *   characteristics: coordination is genuine (rovers require command
 *   architecture), but extraction is real (autonomy is suppressed below
 *   technical feasibility, mission control maintains a power position beyond
 *   what physics requires). The theater ratio (0.55) reflects increasing
 *   ceremonial character of mission control procedures as onboard
 *   computational capability grows — decision protocols are rehearsed rather
 *   than adaptive. The extractiveness trajectory (0.22 → 0.38) shows the
 *   constraint tightening over time as rover sophistication increases
 *   relative to autonomy budget, forcing more elaborate workarounds rather
 *   than enabling direct autonomy gains.
 *
 * KEY AGENTS:
 *   - Rover Operational Autonomy: Primary victim (powerless/trapped) — system capabilities constrained below technical feasibility by architectural limits; cannot exercise onboard decision capacity.
 *   - Mission Control Infrastructure: Primary beneficiary (institutional/arbitrage) — maintains centralized authority and organizational continuity through command architecture; can exit/adapt protocols.
 *   - Field Exploration Science Community: Secondary victim (moderate/constrained) — benefits from rover capability but harmed by exploration constraints imposed by autonomy ceiling; constrained exit via alternative platforms.
 *   - Hardware Vendors: Secondary beneficiary (institutional/arbitrage) — sustained demand for incremental upgrades to processor/communication systems within complexity envelope.
 *   - Autonomy Research Community: Organized agents (organized/constrained) — developing edge computing and hierarchical planning solutions; building sunset pathway through research maturation.
 *   - Pre-Autonomy Operational Doctrine: Institutional inertia (institutional/arbitrage) — command-centric mission doctrine persists through path dependence; maintains itself through training, procedures, organizational structure.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent organizational arrangements as immutable physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rover_onboard_autonomy_complexity, 0.38).
domain_priors:suppression_score(rover_onboard_autonomy_complexity, 0.48).
domain_priors:theater_ratio(rover_onboard_autonomy_complexity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rover_onboard_autonomy_complexity, extractiveness, 0.38).
narrative_ontology:constraint_metric(rover_onboard_autonomy_complexity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rover_onboard_autonomy_complexity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rover_onboard_autonomy_complexity, tangled_rope).
narrative_ontology:human_readable(rover_onboard_autonomy_complexity, "Rover Onboard Autonomy Complexity Constraint").
narrative_ontology:topic_domain(rover_onboard_autonomy_complexity, "robotics/space_exploration/autonomous_systems").

domain_priors:requires_active_enforcement(rover_onboard_autonomy_complexity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rover_onboard_autonomy_complexity, mission_control_infrastructure).
narrative_ontology:constraint_beneficiary(rover_onboard_autonomy_complexity, hardware_vendors).
narrative_ontology:constraint_victim(rover_onboard_autonomy_complexity, field_exploration_capability).
narrative_ontology:constraint_victim(rover_onboard_autonomy_complexity, rover_operational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROVER OPERATIONAL CAPABILITY (SNARE) — The rover system cannot exit the complexity bottleneck. Real-time autonomous decision-making is suppressed by architectural constraints: computational limits, communication latency, thermal/power budgets. The rover experiences full extraction in the form of crippled autonomy — it must wait for Earth commands despite possessing sensors and processing capacity for local decisions. Maximum suppression; minimal coordination benefit.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EXPLORATION SCIENCE (TANGLED ROPE) — Scientists experience genuine coordination (rovers enable missions that would be impossible otherwise) alongside extraction (mission timelines are constrained by autonomy limits, exploration sites are pre-selected rather than dynamically chosen in response to discoveries). Constrained exit: moving to alternative platforms (different rovers, terrestrial analogs) is costly but possible. Mixed experience — benefit from rover existence, harm from its autonomy ceiling.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MISSION CONTROL (ROPE) — Benefits from centralized control architecture. Experiences the constraint as coordination mechanism: managing rover behavior through Earth commands solves the synchronization problem (all rovers operate under unified protocols). Net beneficiary through reduced coordination overhead and risk concentration. High arbitrage options: can shift protocols, reassign personnel, scale infrastructure up/down.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HARDWARE VENDORS (ROPE) — Benefit from continuing demand for upgrades to processor/communication systems to marginally increase onboard autonomy without breaching the complexity threshold. Experience the constraint as pure coordination — managing component integration across mission architectures. Sustained demand for solutions within the complexity envelope. High arbitrage: can shift between vendors, adjust component specifications, develop for adjacent markets.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTONOMY RESEARCH (SCAFFOLD) — Organized agents (AI/robotics labs, rover development teams) see the onboard complexity bottleneck as a temporary coordination failure with a real sunset. Edge computing architectures, hierarchical planning algorithms, and neuromorphic computing are creating pathways to higher autonomy within strict resource budgets. Suppression declines as research matures and proves in operational settings. Constrained exit because adoption requires mission redesign, but exit path is visible and being walked.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MISSION DOCTRINE (PITON) — The remote-command paradigm persists despite technical feasibility of greater onboard autonomy. Operational doctrine (procedures, training, command structure) and risk management frameworks treat the Earth-rover link as functionally irreplaceable. This doctrine maintains itself through institutional inertia and path dependence in mission design, not because the technical constraint is immutable. Theater ratio elevated by rehearsal of command protocols and contingency planning that could be partially automated. Extracted value: organizational stability and career continuity for mission control staff.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, communication latency and computational complexity are immutable constraints of physics and mathematics. Mars rovers operate under ~20-minute one-way light delay; onboard computing is limited by thermal/power physics; complex algorithms (real-time SLAM, pathfinding, hazard detection) grow exponentially in state space. These limits appear as natural law. However, this perspective risks naturalizing what is actually contingent technological and organizational choice. The analytical engine will flag this as a false summit: the 'laws' are real but the constraint's extraction arises from architectural decisions layered on top of them, not from the physical laws themselves.
constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rover_onboard_autonomy_complexity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rover_onboard_autonomy_complexity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rover_onboard_autonomy_complexity, TR),
    TR >= 0.70.

:- end_tests(rover_onboard_autonomy_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint creates real asymmetry — mission control benefits from centralized authority, hardware vendors benefit from sustained upgrade demand, while field autonomy is suppressed. However, the extraction is not severe (not a snare) because rovers do function and autonomy has increased marginally over time. The suppression (0.48) reflects real barriers (communication latency is physics, onboard computation is limited), but these barriers do not fully explain the autonomy ceiling — organizational doctrine and risk models contribute significantly to suppression. Theater ratio (0.55): Moderate. Mission control procedures have increasingly ceremonial character as rover sophistication grows — decision trees are rehearsed at Earth when rovers possess sufficient onboard compute to execute more of them locally. The theater increased over the interval as the technical feasibility/autonomy practice gap widened. Claimed type (tangled_rope): The constraint exhibits both coordination function (command architecture solves genuine synchronization problem) and asymmetric extraction (autonomy is suppressed below what physics/technology would permit). Requires active enforcement (doctrine, training, risk protocols) to maintain the suppression; cannot arise passively from physics alone.
 *
 * PERSPECTIVAL GAP:
 *   The gap between mission control (sees Rope, pure coordination) and rover autonomy (sees Snare, pure extraction) is the core perspectival divergence. Mission control genuinely solves a coordination problem — rovers in a swarm must operate under unified protocols. Autonomy system genuinely experiences suppression — its capabilities are capped below technical feasibility. Both perspectives are accurate to their structural position. The analytical observer risks collapsing this gap by naturalizing the constraint as immutable physical law (Mountain), which would erase the agency dimension of organizational choice. The scaffold perspective (research pathway to higher autonomy) and piton perspective (mission doctrine maintaining suppression through inertia) together show that the constraint is contingent — it could change if organizational risk appetite shifted or if research validated higher autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position. Mission control occupies a beneficiary position (arbitrage exit) — centralized authority provides career stability, procedural continuity, and organizational power. Their experienced extraction (d ≈ 0.15) is low or slightly negative because the constraint distributes benefits toward them. Rover autonomy occupies a victim position (trapped exit) — it cannot exercise capability without violating command protocols; has no alternative; experiences maximum suppression relative to technical feasibility. Its experienced extraction (d ≈ 0.95) is maximal because it bears all constraint costs with no exit. Field exploration occupies an intermediate position (constrained exit) — constrained by autonomy limits but also enabled by rover existence; can exit through alternative platforms at significant cost. Their experienced extraction (d ≈ 0.60) reflects mixed exposure. Hardware vendors occupy a beneficiary position (arbitrage exit) — sustained upgrade demand within the complexity envelope. Their directionality (d ≈ 0.20) is favorable because the constraint creates recurring revenue.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that 'coordination' (mission control's experience) and 'extraction' (autonomy system's experience) coexist in the same structure. The tangled rope classification captures this duality: the command architecture genuinely coordinates multi-rover behavior, AND it genuinely suppresses onboard autonomy below technical feasibility. Neither reading is false. The risk is that mission control's beneficiary perspective (Rope) drowns out the victim perspective (Snare) in policy discussions, naturalizing the constraint as inevitable. The analytical observer's mountain perspective threatens the same error at civilizational scale — 'communication latency is a law of physics' becomes 'therefore rovers will always be remote-controlled' in sloppy thinking. The true mandatrophy resolution is the scaffold perspective: the constraint is real (coordination is needed, latency is physics), but the specific architectural choice (Earth-centric command) is contingent. Higher onboard autonomy with coordinated command is technically feasible and is being incrementally deployed. The sunset is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_sufficiency_threshold,
    'What level of onboard autonomous capability would constitute ''exit'' from the constraint? Is the threshold technical or organizational?',
    'Operational deployment data: comparison of mission science yield between Earth-commanded rovers and hypothetical higher-autonomy rovers using simulation/analog studies. Measurement of decision latency impact on exploration effectiveness.',
    'If threshold is technical (e.g., 95% autonomous decisions): constraint remains fundamental. If threshold is organizational (e.g., risk acceptance culture): constraint is contingent on institutional appetite for innovation risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_sufficiency_threshold, empirical, 'Definition of sufficient autonomy for constraint exit').

omega_variable(
    edge_computing_feasibility,
    'Can hierarchical/edge computing architectures achieve significant autonomy gains within the current power/thermal budget, or are the gains marginal/illusory?',
    'Technical performance data from experimental rovers and testbeds. Quantification of autonomy improvement vs computational overhead and reliability costs. Real mission performance data if available.',
    'If gains are significant: scaffold sunset is real, research pathway is viable. If gains are marginal: scaffold is aspirational, autonomy constraint remains fundamental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(edge_computing_feasibility, empirical, 'Whether edge computing enables substantial autonomy gains').

omega_variable(
    mission_control_risk_model_accuracy,
    'Are mission control''s risk models justified by actual failure data, or do they represent organizational conservatism/liability aversion?',
    'Comparative failure analysis: rovers that operated with higher autonomy (test rovers, commercial units) vs NASA/ESA Mars rovers. Frequency of autonomous-decision failures vs Earth-command failures (including communication latency failures).',
    'If models are justified: constraints reflect genuine risk. If conservative: constraints are organizational choice maintainable via doctrine rather than physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_control_risk_model_accuracy, preference, 'Whether risk models justify autonomy restrictions').

omega_variable(
    career_structure_lock_in,
    'Would adoption of higher onboard autonomy require restructuring mission control career paths and organizational roles?',
    'Organizational analysis of skill requirements, staffing patterns, training pipelines. Interviews with mission control personnel. Comparison with organizations (commercial space, robotics labs) operating higher-autonomy systems.',
    'If yes: piton perspective is accurate, constraint persists through institutional identity lock despite technical feasibility. If no: organizational barriers are weaker than technical barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_structure_lock_in, conceptual, 'Whether organizational restructuring blocks autonomy adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rover_onboard_autonomy_complexity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rover_tr_t0, rover_onboard_autonomy_complexity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rover_tr_t10, rover_onboard_autonomy_complexity, theater_ratio, 10, 0.5).
narrative_ontology:measurement(rover_tr_t20, rover_onboard_autonomy_complexity, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(rover_be_t0, rover_onboard_autonomy_complexity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rover_be_t10, rover_onboard_autonomy_complexity, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(rover_be_t20, rover_onboard_autonomy_complexity, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rover_onboard_autonomy_complexity, enforcement_mechanism).
narrative_ontology:affects_constraint(rover_onboard_autonomy_complexity, mars_communication_latency).
narrative_ontology:affects_constraint(rover_onboard_autonomy_complexity, rover_thermal_power_budget).

% DUAL FORMULATION NOTE:
% Rover autonomy complexity is downstream of communication latency (fundamental physics) and power/thermal budgets (engineering constraints). The autonomy constraint itself is not a physical limit but an architectural choice that manages complexity within those underlying constraints. All three should be modeled separately: latency and power are upstream mountains; autonomy complexity is a tangled rope layered on top. This story focuses on autonomy complexity as an organizational/architectural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rover_onboard_autonomy_complexity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
