% ============================================================================
% CONSTRAINT STORY: perverse_incentive_propagation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perverse_incentive_propagation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perverse_incentive_propagation
 *   human_readable: Perverse Incentive Propagation Through Hierarchical Systems
 *   domain: organizational/systemic/economic
 *
 * SUMMARY:
 *   Perverse incentive propagation occurs when an organization defines
 *   measurable success metrics that diverge from true objectives, creating
 *   structural incentives for agents to optimize the metric rather than the
 *   objective. The constraint operates hierarchically: senior leadership
 *   defines metrics based on what is measurable rather than what matters;
 *   middle management must report against those metrics; front-line operators
 *   face explicit instructions to hit metrics and implicit pressure to
 *   maintain performance, creating an impossible bind when metrics conflict
 *   with actual mission. The system exhibits tangled rope characteristics: it
 *   genuinely coordinates information flow and aggregates activity across
 *   distributed agents (coordination function) while simultaneously
 *   extracting effort from front-line operators who bear the cost of
 *   metric-objective misalignment (asymmetric extraction). The theater ratio
 *   increases over time as gaming becomes more sophisticated and measurement
 *   infrastructure expands while actual predictive power of metrics remains
 *   flat or declines (Goodhart drift). This constraint appears in virtually
 *   all large organizations and many small ones, scaled across economic,
 *   governmental, educational, and social domains.
 *
 * KEY AGENTS:
 *   - Front-Line Operators: Victims (powerless/trapped) — face direct instruction to optimize metrics even when metrics conflict with stated objectives; cannot exit without career loss; bear full cost of system misalignment
 *   - Middle Managers: Constrained moderates (moderate/constrained) — must coordinate information flow (coordination) while managing contradictions between metrics and reality; face career risk if they challenge metric system
 *   - Metric System Designers: Beneficiaries (institutional/arbitrage) — control definition of metrics; benefit from concentration of accountability; can exit or redesign if pressured
 *   - Organizational Culture: Institutional actor (institutional/arbitrage) — maintains metric-focused management through inertia despite known dysfunctions; controls the frame
 *   - Metrics Reform Movement: Organized agents (organized/constrained) — theorists and practitioners building alternative evaluation frameworks; constrained by institutional opposition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perverse_incentive_propagation, 0.58).
domain_priors:suppression_score(perverse_incentive_propagation, 0.65).
domain_priors:theater_ratio(perverse_incentive_propagation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perverse_incentive_propagation, extractiveness, 0.58).
narrative_ontology:constraint_metric(perverse_incentive_propagation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(perverse_incentive_propagation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perverse_incentive_propagation, tangled_rope).
narrative_ontology:human_readable(perverse_incentive_propagation, "Perverse Incentive Propagation Through Hierarchical Systems").
narrative_ontology:topic_domain(perverse_incentive_propagation, "organizational/systemic/economic").

domain_priors:requires_active_enforcement(perverse_incentive_propagation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perverse_incentive_propagation, metric_optimizers).
narrative_ontology:constraint_beneficiary(perverse_incentive_propagation, hierarchy_maintainers).
narrative_ontology:constraint_victim(perverse_incentive_propagation, actual_objective_achievement).
narrative_ontology:constraint_victim(perverse_incentive_propagation, agent_wellbeing).
narrative_ontology:constraint_victim(perverse_incentive_propagation, system_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONT-LINE OPERATOR (SNARE) — Trapped in a system where measured metrics become the operative goal, regardless of true objective. Cannot exit without career loss; cannot change the metric system from below. Bears full extraction as gaming behavior is cascaded downward while benefits of metric optimization flow upward. Maximum suppression: explicit instructions vs implicit actual goals create impossible bind.
constraint_indexing:constraint_classification(perverse_incentive_propagation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Experiences both coordination and extraction. Genuinely needs to aggregate information from front-line operators (coordination) while also facing pressure to optimize metrics that may contradict true objectives (extraction). Constrained exit: switching organizations incurs career penalty and market-wide coordination on metrics is uniform. Moderate suppression: understands the system's contradictions but constrained by institutional reporting structure.
constraint_indexing:constraint_classification(perverse_incentive_propagation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METRIC SYSTEM DESIGNER (ROPE) — Experiences the constraint as a coordination mechanism: designing metrics that enable aggregate measurement of distributed activity. Arbitrage exit: can redesign metrics or switch organizations. Benefits from the structure as it concentrates reporting burden. Sees the perverse incentive as a feature (motivational alignment) rather than a bug. Low suppression: controls the system.
constraint_indexing:constraint_classification(perverse_incentive_propagation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL CULTURE (PITON) — Metric-focused performance management persists through institutional inertia despite widespread knowledge that it produces gaming and misalignment. The ritual of 'data-driven management' maintains legitimacy while actual decision-making often ignores metrics that contradict leadership intuition. Theater ratio high: measurement activity and reporting theater have grown while actual predictive power of metrics has stagnated or declined. Once-functional coordination mechanism now substantially performative.
constraint_indexing:constraint_classification(perverse_incentive_propagation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: METRICS REFORM MOVEMENT (SCAFFOLD) — Organized agents (behavioral economics researchers, organizational theorists, progressive companies) see perverse incentives as a solvable coordination problem with an explicit sunset. Proposals include: outcome-based evaluation replacing metric proxies, participatory metric design, temporal cycles of metric renewal. Constrained exit: institutional opposition from metric-dependent bureaucracies and consulting firms. But sees a path forward — the constraint is temporary if reformed metrics architecture gains institutional adoption.
constraint_indexing:constraint_classification(perverse_incentive_propagation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, perverse incentives are an inevitable feature of any monitoring and evaluation system: any metric will be gamed if sufficiently disconnected from true objectives. The constraint appears as an immutable law of bureaucracy (Goodhart's Law, Campbell's Law). However, this naturalizes what may be contingent institutional design choices. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(perverse_incentive_propagation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perverse_incentive_propagation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perverse_incentive_propagation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perverse_incentive_propagation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perverse_incentive_propagation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perverse_incentive_propagation, TR),
    TR >= 0.70.

:- end_tests(perverse_incentive_propagation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction flow is effort expended on metric optimization that does not advance true objectives, diverted from front-line operators upward to leadership that benefits from apparent performance. The value reflects that some metric-based coordination is genuine (not all extractive) but the mismeasurement dominates, creating persistent value leakage. The upward trajectory from 0.32 to 0.58 reflects gaming acceleration: as operators learn metric system, gaming becomes more sophisticated and more extraction accrues. Suppression (0.65): High. Multiple suppression mechanisms: (1) hierarchy prevents challenge from below without career loss; (2) metric system is formally legitimate (defended by management theory and consulting industry); (3) alternatives are not institutionally available (reform is organized but not yet mainstream); (4) feedback loops are slow — operators see metric targets immediately but consequences of metric-objective misalignment appear over years; (5) explicit instruction to hit metrics conflicts with implicit actual objective, creating psychological bind. Theater ratio (0.68): High. Metric reporting activity has become largely performative: data is collected, dashboards are created, reports are written, but actual decision-making often ignores metrics or uses them selectively to confirm prior intuition. The ritual maintains organizational legitimacy ('we are data-driven') while bypassing genuine uncertainty reduction. Theater increases over the interval as measurement infrastructure expands while accuracy and predictive power stagnate.
 *
 * PERSPECTIVAL GAP:
 *   The front-line operator experiences snare (pure extraction, no escape) while the metric designer experiences rope (coordination mechanism, low cost). The organizational culture sees piton (degraded ritual, maintained through inertia) while the reform movement sees scaffold (temporary problem with a sunset as alternative metrics gain adoption). The middle manager sees tangled rope (genuine coordination plus extraction) while the analytical observer risks seeing mountain (perverse incentives as inevitable law of any monitoring system). The perspectival gap reveals that the constraint is not monolithic — it is generated by institutional choices about what to measure, how to report, and what to optimize. Different institutional designs would produce different perspectival readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Front-line operators (d = 0.95, trapped exit) experience near-maximal extraction because they have no exit capacity and are explicitly instructed to pursue metrics regardless of actual objectives. Middle managers (d = 0.60, constrained exit) occupy the mixed position — they benefit modestly from the coordination infrastructure while bearing extraction costs from impossible reporting demands. Metric designers and organizational culture actors (d = 0.05-0.15, arbitrage exit) are beneficiaries — they control the system and can modify it or leave. The organized reform movement (d = 0.55, constrained exit) faces high barriers to replacing the incumbent system but has some agency through alternative institutional building. The analytical observer (d = 0.72, analytical position) sees the full structure but risks naturalizing it as immutable rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by showing that perverse incentive propagation is neither pure coordination (Rope) nor pure extraction (Snare) but a genuine hybrid: hierarchical systems require some information aggregation and performance measurement (coordination function), but metric-objective misalignment creates systematic extraction (asymmetric costs borne by front-line operators). The classification is stable as tangled_rope: the coordination function is real (organizations genuinely need aggregated information), the extraction is real (operators genuinely lose effort to metric gaming), and enforcement is active (leadership actively pushes metric optimization). The false summit (mountain) is revealed by the fact that gaming severity correlates with institutional design choices: organizations with outcome-based evaluation instead of metric-based management show lower perverse incentive costs, proving the constraint is not inevitable. Goodhart's Law (any metric becomes invalid when used as a target) is a statement about metric properties, not an immutable law of organizations. The organizational design choices are contingent, making the constraint a tangled rope rather than a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_observability_limit,
    'Is gaming inevitable because true objectives are fundamentally unobservable, or because metric designers have chosen metrics with convenient measurement rather than true predictive power?',
    'Comparative analysis of organizations with outcome-based evaluation vs metric-based evaluation; longitudinal tracking of gaming prevalence as measurement technology improves',
    'If unobservable: Goodhart''s Law is a mountain (immutable). If designed choice: current metrics represent a contingent institutional arrangement that could be replaced. Changes classification from mountain to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_observability_limit, empirical, 'Whether perverse incentives are inevitable or contingent on metric design').

omega_variable(
    gaming_detection_feasibility,
    'Can organizations detect and penalize gaming behavior with sufficient speed and accuracy to counteract the incentive to game?',
    'Case studies of organizations with active gaming detection; measurement of detection lag vs gaming feedback loop speed; effectiveness of auditing mechanisms',
    'If detectable: gaming can be constrained through enforcement (suppression increases but extraction decreases). If undetectable: gaming is rational behavior and extraction dominates. Affects suppression vs extractiveness trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_detection_feasibility, empirical, 'Whether gaming behavior can be effectively detected and penalized').

omega_variable(
    alternative_coordination_mechanisms,
    'Do outcome-based evaluation, participatory metric design, and soft accountability mechanisms actually reduce perverse incentives, or do they merely relocate them to less observable dimensions?',
    'Longitudinal case studies of organizations transitioning away from metric-based management; measurement of objective achievement and staff wellbeing before/after transition; identification of gaming in alternative evaluation systems',
    'If truly reduces perverse incentives: scaffold perspective is structural and sunset is real. If relocates gaming: all approaches produce tangled_rope regardless of mechanism (extraction is invariant). Affects viability of reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether alternative evaluation mechanisms reduce or relocate perverse incentives').

omega_variable(
    scale_invariance_of_gaming,
    'Does perverse incentive severity correlate with organizational scale, metric complexity, or power distance in the hierarchy? Is gaming equally prevalent in small flat organizations as in large hierarchies?',
    'Cross-organizational comparison of gaming behavior by scale, structure, and power distance; qualitative analysis of when gaming emerges and when it remains below detection threshold',
    'If scale-invariant: perverse incentives are universal (mountain). If scale-dependent: they are contingent on hierarchical structure and could be reduced through organizational redesign. Affects whether mountain classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_invariance_of_gaming, empirical, 'Whether perverse incentives are scale-invariant or contingent on organizational structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perverse_incentive_propagation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perverse_tr_t0, perverse_incentive_propagation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(perverse_tr_t5, perverse_incentive_propagation, theater_ratio, 5, 0.55).
narrative_ontology:measurement(perverse_tr_t10, perverse_incentive_propagation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(perverse_be_t0, perverse_incentive_propagation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(perverse_be_t5, perverse_incentive_propagation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(perverse_be_t10, perverse_incentive_propagation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perverse_incentive_propagation, resource_allocation).
narrative_ontology:affects_constraint(perverse_incentive_propagation, goodharts_law_instantiation).
narrative_ontology:affects_constraint(perverse_incentive_propagation, metric_gaming_in_education).
narrative_ontology:affects_constraint(perverse_incentive_propagation, metric_gaming_in_healthcare).
narrative_ontology:affects_constraint(perverse_incentive_propagation, bureaucratic_goal_displacement).

% DUAL FORMULATION NOTE:
% Perverse incentive propagation is a general structural phenomenon that instantiates differently across domains. This story captures the organizational-level constraint; domain-specific versions (education metrics, healthcare metrics, etc.) have their own stories with potentially different ε values depending on domain-specific features like feedback loop speed and objective measurability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
