% ============================================================================
% CONSTRAINT STORY: lunar_rover_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lunar_rover_autonomy, []).

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
 *   constraint_id: lunar_rover_autonomy
 *   human_readable: Lunar Rover Autonomy Constraints
 *   domain: aerospace_engineering/space_operations
 *
 * SUMMARY:
 *   Lunar rover autonomy constraints emerge from the intersection of physics
 *   (8-second round-trip communication delay), institutional governance
 *   (mission control authority), and technical capability (onboard autonomous
 *   systems). The constraint exhibits the classical pattern of a hybrid
 *   mechanism: genuine coordination value (rovers must navigate safely in
 *   unknown terrain; Earth-based operators cannot react in real time to
 *   hazards) layered with asymmetric extraction (institutional actors
 *   maintain control and decision authority far beyond what communication
 *   physics requires). The theater ratio is relatively low (0.35) because
 *   rover teleoperation maintains genuine functional content — command
 *   sequences do prevent collisions and mission failures. However,
 *   extractiveness has risen steadily over a decade as institutional autonomy
 *   budgets have remained conservative despite rapid advancement in machine
 *   vision and terrain classification algorithms. The scaffold perspective is
 *   now active: multiple space agencies and private operators are developing
 *   rovers with genuine onboard autonomous capabilities that could reduce
 *   Earth-dependency within a 10-15 year horizon. The constraint is
 *   transitional.
 *
 * KEY AGENTS:
 *   - The Rover (Operational Flexibility): Primary victim (powerless/trapped) — constrained by physics and design choices; cannot negotiate autonomy bounds in real time
 *   - Field Science Teams: Secondary victim (moderate/constrained) — face 24-48 hour planning cycles and rigid command sequences; limited real-time adaptation capacity
 *   - Mission Control Operators: Primary beneficiary (institutional/arbitrage) — maintain decision authority and oversight control; can upgrade autonomy parameters or adopt new rover designs
 *   - Space Agencies: Institutional beneficiary (institutional/arbitrage) — govern autonomy policies, control mission resources, capture scientific and political benefits of rover exploration
 *   - Autonomous Capability Development: Organized coalition (organized/mobile) — machine learning engineers, roboticists, autonomous systems researchers building alternative technical pathways
 *   - Legacy Teleoperation Protocols: Institutional system (institutional/arbitrage) — command upload rituals, daily supervision schedules persist through organizational inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lunar_rover_autonomy, 0.52).
domain_priors:suppression_score(lunar_rover_autonomy, 0.48).
domain_priors:theater_ratio(lunar_rover_autonomy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lunar_rover_autonomy, extractiveness, 0.52).
narrative_ontology:constraint_metric(lunar_rover_autonomy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(lunar_rover_autonomy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lunar_rover_autonomy, tangled_rope).
narrative_ontology:human_readable(lunar_rover_autonomy, "Lunar Rover Autonomy Constraints").
narrative_ontology:topic_domain(lunar_rover_autonomy, "aerospace_engineering/space_operations").

domain_priors:requires_active_enforcement(lunar_rover_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lunar_rover_autonomy, mission_operators).
narrative_ontology:constraint_beneficiary(lunar_rover_autonomy, space_agencies).
narrative_ontology:constraint_beneficiary(lunar_rover_autonomy, lunar_base_planners).
narrative_ontology:constraint_victim(lunar_rover_autonomy, rover_operational_flexibility).
narrative_ontology:constraint_victim(lunar_rover_autonomy, mission_timeline_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The rover itself (as operational system) cannot exit the autonomy constraints imposed by Earth-Moon communication delay and pre-programmed command sets. Trapped by fundamental physics (8-second round-trip latency) and design decisions that limit real-time responsiveness. The rover bears the full cost of conservative autonomy bounds.
constraint_indexing:constraint_classification(lunar_rover_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Science teams are constrained by pre-uploaded command sequences and limited autonomous decision capacity, yet genuinely benefit from the rover's existence and coordination with Earth-based planning. They have some agency (can plan 24-48 hour sequences) but face high friction and lost efficiency from communication delays and rigid autonomy bounds.
constraint_indexing:constraint_classification(lunar_rover_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Mission operators experience the autonomy constraint as a legitimate coordination mechanism. Command protocols ensure safety, reduce collision risk, and maintain accountability. They benefit from controlled operations and can arbitrage by updating autonomy parameters or adopting new rover designs. Extraction runs toward this institutional actor.
constraint_indexing:constraint_classification(lunar_rover_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized efforts (machine vision, terrain classification algorithms, obstacle avoidance systems) see autonomy constraints as a temporary problem with a sunset clause. Advanced rovers with onboard AI are being designed to replace Earth-dependent command sequences. The scaffold has a clear exit path: as autonomous systems mature over the next 10-15 years, real-time human control becomes optional rather than mandatory.
constraint_indexing:constraint_classification(lunar_rover_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Legacy teleoperation command protocols persist through institutional inertia despite advanced autonomous systems being technically feasible. The ritual of daily command uploads and real-time supervision is maintained because alternatives haven't fully displaced it, not because it's optimal. Theater ratio reflects the performative nature of human oversight when humans cannot actually react in real-time to rover conditions.
constraint_indexing:constraint_classification(lunar_rover_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal perspective, the 8-second communication delay is an immutable physical constraint (speed of light over 380,000 km). Some degree of autonomous rover operation is theoretically inevitable. However, the structural data (high suppression, moderate extractiveness, institutional beneficiary control) contradicts a pure mountain classification. This perspective risks naturalizing social choices (limited autonomy budgets, conservative command protocols) as if they were physics. The engine should flag this as a false summit.
constraint_indexing:constraint_classification(lunar_rover_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lunar_rover_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lunar_rover_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lunar_rover_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lunar_rover_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lunar_rover_autonomy, TR),
    TR >= 0.70.

:- end_tests(lunar_rover_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising trend. The base value reflects institutional actors' ability to maintain control over rover operations far beyond what communication delay would require. Physics requires *some* autonomy (rovers must avoid obvious hazards in real time), but institutional governance constrains autonomy to minimal safe baseline rather than expanding it toward rovers' technical capability. The 0.28→0.52 trajectory shows accumulating control restrictions: each new operational constraint (thermal management, power budget optimization, scientific prioritization) is implemented as a centralized Earth-based decision rather than delegated to onboard systems. Suppression (0.48): Moderate. Barriers to expanded autonomy include: regulatory liability frameworks (space agencies assume full accountability for rover actions), institutional risk aversion (failure modes at scale 380,000 km create political costs), technical conservatism (proven teleoperation protocols outweigh untested autonomous systems in institutional decision-making). Suppression is not total because technical alternatives are feasible and some operators (private firms, younger agencies) are willing to experiment. Theater ratio (0.35, rising slowly): Relatively low because the coordination function (preventing collisions, maintaining power/thermal budgets, ensuring scientific data quality) is genuinely performed by Earth-based oversight. However, the theater increases over time as human oversight becomes increasingly asymmetric — operators supervising rovers they cannot actually intervene in real time to correct, creating illusion of control without reality.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between mission control's experience (rope — coordination mechanism preventing costly failures) and the rover's experience (snare — rigid operational constraints with no adaptation pathway). A secondary gap exists between institutional beneficiaries' immediate experience (rope, coordination logic) and the scaffold perspective's generational view (temporary problem, sunset approaching). The mountain perspective risks collapsing these gaps by attributing all constraints to physics, naturalizing what are actually institutional choices. The analytical observer must distinguish between: (1) immutable physical constraints (light-speed delay exists), (2) necessary operational constraints (some Earth-based oversight prevents catastrophic failures), and (3) institutional extraction mechanisms (autonomous budgets artificially restricted, control authority maintained beyond necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   Mission control operators are institutional actors with arbitrage exit options — they can change rover designs, update autonomy parameters, or shift to different operational protocols without exiting the space exploration domain. They derive low-to-negative directionality (beneficiaries experiencing minimal extraction). The rover's operational flexibility is powerless and trapped — it cannot negotiate its constraints in real time and cannot exit to an alternative operational model without external intervention. It derives high directionality (victim experiencing maximum extraction). Field science teams are moderate-power agents with constrained exit options — they can design alternative mission profiles or work with multiple rovers, but face career risk and resource constraints in doing so. The institutional beneficiary's arbitrage options (updating autonomy policies, adopting new rovers, shifting to private operators with different governance) mean they can exit extraction mechanisms without leaving the functional domain. This asymmetry drives the tangled_rope classification: genuine coordination layered with extraction mechanisms that concentrate decision authority upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that extractiveness (0.52) lies in the tangled_rope range precisely because genuine coordination (rover safety, mission success) is coupled with asymmetric extraction (institutional control authority maintained beyond communication-delay necessity). The constraint is not misclassified as pure coordination (rope) because suppression (0.48) and the institutional beneficiary structure prevent this. It is not a pure snare because the coordination function is genuine — rovers genuinely benefit from Earth-based supervision and pre-programmed safety bounds reduce mission risk. The scaffold perspective is crucial: autonomous systems development is creating an alternative coordination mechanism that could eventually replace teleoperation, which would lower extractiveness by removing the institutional asymmetry. The mandatrophy analysis shows: (1) the coordination problem is real, (2) the extraction mechanism is institutional (control authority beyond necessity), (3) technical alternatives exist (autonomous systems), and (4) the sunset clause is plausible (10-15 year timeline for mature autonomous rovers). This prevents mislabeling the constraint as either pure coordination (missing the extraction) or pure control (missing the genuine coordination value).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communication_delay_primacy,
    'Is the autonomy constraint primarily driven by the 8-second light-speed delay, or by institutional risk aversion and control preferences?',
    'Comparative analysis: measure autonomy grants in rovers with identical communication delays but different institutional governance structures (NASA, ESA, China, private operators). If delay is primary, autonomy levels should correlate only with delay. If institutional preferences dominate, similar delays with different operators should show variance.',
    'If delay-primary: constraint is mountain-adjacent (physics-driven). If institutional-primary: constraint is tangled_rope (extractive control mechanisms using physics as justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communication_delay_primacy, empirical, 'Whether autonomy constraints are physics-driven or governance-driven').

omega_variable(
    autonomous_capability_readiness,
    'Are onboard autonomous systems (vision, terrain classification, obstacle avoidance) technically ready to replace Earth-dependent command protocols, or do fundamental sensor/compute limitations persist?',
    'Field testing of autonomous navigation in Earth terrain analog (lava tubes, regolith simulant) with metrics: time-to-goal, obstacle detection accuracy, terrain classification success rate, without Earth communication.',
    'If ready: scaffold sunset is real — expect extraction to decrease as autonomous systems are adopted. If not ready: institutional control persists by necessity, not just preference — extraction floor rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomous_capability_readiness, empirical, 'Technical readiness of autonomous systems to replace teleoperation').

omega_variable(
    liability_and_institutional_risk,
    'To what extent do liability structures and institutional risk frameworks drive conservative autonomy bounds, independent of technical capability?',
    'Legal/policy analysis: compare rover autonomy levels with mission insurance costs, liability frameworks, and agency oversight requirements across different space agencies and private operators. Analyze whether institutional reorganization (clearer liability assignment) correlates with autonomy expansion.',
    'If liability is high driver: removing institutional barriers could expand autonomy without technical change. If low driver: technical limitations are the real constraint. Affects whether mandate is institutional reform (lowering suppression) or technology development (raising capability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_and_institutional_risk, empirical, 'Institutional risk aversion as driver of autonomy constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lunar_rover_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lunar_rover_tr_t0, lunar_rover_autonomy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lunar_rover_tr_t5, lunar_rover_autonomy, theater_ratio, 5, 0.26).
narrative_ontology:measurement(lunar_rover_tr_t10, lunar_rover_autonomy, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(lunar_rover_be_t0, lunar_rover_autonomy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lunar_rover_be_t5, lunar_rover_autonomy, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lunar_rover_be_t10, lunar_rover_autonomy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lunar_rover_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(lunar_rover_autonomy, lunar_mission_planning_timescale).
narrative_ontology:affects_constraint(lunar_rover_autonomy, autonomous_system_development_pace).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lunar_rover_autonomy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
