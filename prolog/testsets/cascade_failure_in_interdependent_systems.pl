% ============================================================================
% CONSTRAINT STORY: cascade_failure_in_interdependent_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascade_failure_in_interdependent_systems, []).

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
 *   constraint_id: cascade_failure_in_interdependent_systems
 *   human_readable: Cascade Failure in Interdependent Systems
 *   domain: infrastructure/systems_dynamics/risk_management
 *
 * SUMMARY:
 *   Cascade failure in interdependent systems emerges from the structural
 *   tension between efficiency-driven tight coupling (which maximizes
 *   resource utilization and reduces redundancy costs) and resilience-driven
 *   loose coupling (which isolates failures and prevents contagion). Modern
 *   infrastructure — electrical grids, telecommunications networks, financial
 *   systems, supply chains, internet topology — operate under relentless
 *   pressure to eliminate redundancy and maximize throughput. This constraint
 *   exhibits the full typology of DR classifications. The same structural
 *   phenomenon — the propagation of localized failures through interconnected
 *   systems — appears as a coordination mechanism enabling real-time
 *   optimization (Rope from operator perspective), a temporary architecture
 *   being replaced by decoupled systems (Scaffold from engineering coalition
 *   perspective), a natural inevitability of coupling (Mountain from
 *   complexity theory view), pure extraction from the resilience perspective,
 *   mixed coordination-extraction from regulatory perspective, and degraded
 *   risk theater from institutional perspective. The constraint's
 *   theater_ratio (0.58) reflects that cascade failure risk management
 *   consists largely of simulation exercises and post-hoc reviews that do not
 *   prevent actual cascades. The extractiveness trajectory (0.22 → 0.38 over
 *   the interval) shows increasing extraction as coupling has tightened in
 *   pursuit of efficiency gains.
 *
 * KEY AGENTS:
 *   - Centralized System Operators: Primary beneficiary (institutional/arbitrage) — captures efficiency gains and cost reduction through tight coupling; benefits from reduced redundancy expenses
 *   - Distributed Resilience: Primary victim (powerless/trapped) — abstract collective good that cannot exit interdependency structure; bears full cost of cascade events
 *   - Infrastructure Regulators: Secondary actor (moderate/constrained) — tasked with both efficiency and stability; constrained by political economy; bear asymmetric risk
 *   - Resilience Engineering Coalition: Organized agents (organized/mobile) — engineers, redundancy advocates, decentralization advocates building alternative architectures with decoupling sunset
 *   - Risk Management Institutions: Institutional actor (institutional/arbitrage) — maintain performative cascade contingency planning with low functional capacity to prevent cascades
 *   - Analytical Observer: Complexity science view (analytical/analytical) — risks naturalizing contingent coupling choices as inevitable phase transitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascade_failure_in_interdependent_systems, 0.38).
domain_priors:suppression_score(cascade_failure_in_interdependent_systems, 0.62).
domain_priors:theater_ratio(cascade_failure_in_interdependent_systems, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascade_failure_in_interdependent_systems, extractiveness, 0.38).
narrative_ontology:constraint_metric(cascade_failure_in_interdependent_systems, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cascade_failure_in_interdependent_systems, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cascade_failure_in_interdependent_systems, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(cascade_failure_in_interdependent_systems, resistance, 1.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascade_failure_in_interdependent_systems, tangled_rope).
narrative_ontology:human_readable(cascade_failure_in_interdependent_systems, "Cascade Failure in Interdependent Systems").
narrative_ontology:topic_domain(cascade_failure_in_interdependent_systems, "infrastructure/systems_dynamics/risk_management").

domain_priors:requires_active_enforcement(cascade_failure_in_interdependent_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cascade_failure_in_interdependent_systems, centralized_operators).
narrative_ontology:constraint_beneficiary(cascade_failure_in_interdependent_systems, efficiency_maximizers).
narrative_ontology:constraint_victim(cascade_failure_in_interdependent_systems, distributed_resilience).
narrative_ontology:constraint_victim(cascade_failure_in_interdependent_systems, system_redundancy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED RESILIENCE (SNARE) — Cannot exit the interdependency structure; bears full cost of cascade failure when it occurs. Abstract collective good (system-wide redundancy and localized failure containment) has no advocate and cannot organize. Maximum extraction realized during cascade events, while prevention costs are diffused and invisible.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CENTRALIZED OPERATORS (ROPE) — Benefit from efficiency gains and cost reduction through tight interdependency coupling. Experience the constraint as coordination: integrating distributed nodes enables real-time optimization and resource utilization. Extraction flows toward this actor through saved redundancy costs. From their immediate perspective, tight coupling is rational and beneficial.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INFRASTRUCTURE REGULATORS (TANGLED ROPE) — Constrained by political economy of efficiency demands and cost pressures, but also tasked with systemic stability. Experience the constraint as mixed: genuine coordination of interconnected infrastructure with asymmetric extraction (operators capture efficiency gains while regulators bear systemic risk). Career costs to stricter redundancy mandates; career benefits to continued operation. Moderate agency.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: RESILIENCE ENGINEERING COALITION (SCAFFOLD) — Organized agents (engineers, redundancy advocates, grid-modernization initiatives) see cascade failure as a solvable coordination problem with a sunset: distributed control systems, circuit breaker protocols, and decentralized monitoring are building alternative architectures that preserve efficiency while isolating failures. Low effective extraction because this coalition has agency and sees a genuine path to architecturally-decoupled systems.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RISK MANAGEMENT THEATER (PITON) — Cascade failure contingency planning, stress testing, and disaster drills persist largely as performative risk management. Simulations cannot capture true systemic behavior under failure; plans become obsolete as infrastructure evolves; post-failure reviews blame operator error rather than structural coupling. The institutional machinery of risk management maintains itself through inertia despite low functional capacity to prevent cascades.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHASE TRANSITION VIEW (MOUNTAIN) — From a complexity science perspective, cascade failure is an unavoidable phase transition in tightly coupled systems: beyond a critical coupling density, the system enters a regime where failure propagation becomes inevitable regardless of design. This perspective sees the bottleneck as an immutable property of coupled systems themselves. However, the structural data contradicts the mountain classification — the empirical record shows that decoupled architectures do not exhibit cascades, suggesting the coupling is contingent, not a law of nature.
constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascade_failure_in_interdependent_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascade_failure_in_interdependent_systems, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cascade_failure_in_interdependent_systems, TR),
    TR >= 0.70.

:- end_tests(cascade_failure_in_interdependent_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint captures significant extraction through operator cost reductions (eliminated redundancy) that accrue during normal operation, but the full extraction cost is realized only during rare cascade events when the entire system fails simultaneously. The probabilistic nature of cascade failure means the extraction is real but temporally concentrated in catastrophic events. Between cascade events, the constraint appears to be purely beneficial (efficient operation), which suppresses perception of extraction. Suppression (0.62): Moderate-high. Significant barriers to moving away from tight coupling include sunk infrastructure costs, network effects that reward staying connected, political barriers to requiring redundancy (framed as inefficiency), and path dependency in system design. Operators have financial incentive not to exit; regulators face political cost to strict decoupling mandates. Theater ratio (0.58): Moderate-high. Cascade risk management consists substantially of simulation exercises, stress tests, and contingency plans that have low correlation with actual cascade prevention. Post-cascade inquiries consistently find that known vulnerabilities were not addressed; plans become obsolete as systems evolve. The theater has increased over the interval as cascades have occurred (2003 Northeast Blackout, 2008 financial contagion, 2011 Japan supply chain cascade, COVID-19 interdependency cascades) without corresponding prevention, instead increasing the performed risk management activity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same infrastructure design choice — tight coupling for efficiency — appears radically differently depending on structural position. Operators see Rope (rational coordination solving the efficiency problem). Engineers see Scaffold (temporary suboptimal architecture being replaced). Risk managers see Piton (degraded theater). Regulators see Tangled Rope (mixed benefits and risks). Resilience sees Snare (pure extraction, zero agency). The analytical observer risks seeing Mountain (phase transition inevitability) but the empirical data — decoupled systems do not cascade — contradicts this. The perspectival gap reveals that the 'inevitability' framing naturalizes what is actually a choice by dominant actors to extract efficiency benefits while distributing cascade risk to the system as a whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position relative to coupling and cascade risk. Centralized operators experience low d (beneficiary with arbitrage exit — they can opt into decoupling if incentivized, though path dependency constrains this). Resilience experiences high d (victim with no exit — the abstract property of system stability cannot leave the coupled system). Regulators experience moderate d (constrained between efficiency pressure and stability mandate). Engineers have mobile d (they can shift to decoupling projects). Risk management institutions have institutional d (they benefit from continued tight coupling through expanded contingency budgets). The analytical observer has analytical d (no material stake in the structure). The asymmetry between operator d and resilience d drives the snare/tangled_rope classification from victim and moderate perspectives respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: Cascade failure in interdependent systems should decompose into at least two structurally distinct constraints: (1) tight_coupling_efficiency_extraction (ε ≈ 0.35, Tangled Rope) — the coordination of real-time optimization with asymmetric extraction of efficiency gains; (2) cascade_failure_structural_risk (ε ≈ 0.72, Snare) — the pure extraction during cascade events when all coupled nodes fail simultaneously. These are not the same constraint viewed differently — they have different temporal profiles (continuous vs catastrophic), different beneficiary/victim structures (operators/regulators vs distributed infrastructure), and different failure modes. The unified story conflates them. For this generation, we treat them as a single story to show the full typology, but a proper analysis would separate them and link via network.affects_constraints. The mandatrophy is resolved by recognizing that the 'inevitability' framing (mountain) serves incumbent operators by naturalizing what is actually a choice to prioritize efficiency extraction over systemic resilience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coupling_density_threshold,
    'Is there a coupling density threshold above which cascade failure becomes inevitable, or is prevention always possible through architectural choice?',
    'Comparative analysis of high-coupling vs decoupled systems; historical review of cascade prevention in systems that reduced coupling vs those that maintained tight coupling despite redundancy investment',
    'If threshold exists: cascade failure approaches mountain classification (structural unavoidability). If prevention always possible: constraint is purely institutional (Snare or Tangled Rope), suggesting decoupling is politically/economically resisted rather than physically impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coupling_density_threshold, empirical, 'Whether coupling density makes cascade failure inevitable or preventable').

omega_variable(
    efficiency_extraction_measurement,
    'How much of the efficiency gain from tight coupling accrues to centralized operators vs distributed benefits that justify the cascade risk?',
    'Cost-benefit accounting: measure operator savings from reduced redundancy against system-wide costs of rare catastrophic failure; compare benefit distribution in coupled systems vs decoupled systems with equivalent throughput',
    'If centralized, asymmetric: extraction mechanism is confirmed (Snare for resilience). If distributed, symmetric: constraint reverts toward Rope or Scaffold (coordination vs temporary coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_extraction_measurement, empirical, 'Distribution of efficiency gains between operators and system').

omega_variable(
    decoupling_cost_inflation,
    'Are decoupled architectures genuinely more expensive, or does cost inflation reflect incumbent operator resistance to transition?',
    'Longitudinal cost analysis of decoupled system deployments; correlation between decoupling adoption and incumbent market share loss; historical precedent analysis (electric grid transitions, telecom network decentralization)',
    'If genuinely expensive: scaffold perspective is aspirational (sunset may not arrive). If inflation is incumbent resistance: scaffold perspective is confirmed (architectural exit is feasible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_cost_inflation, empirical, 'Whether decoupling cost inflation reflects technology or market structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascade_failure_in_interdependent_systems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cascade_tr_t0, cascade_failure_in_interdependent_systems, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cascade_tr_t5, cascade_failure_in_interdependent_systems, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cascade_tr_t10, cascade_failure_in_interdependent_systems, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cascade_be_t0, cascade_failure_in_interdependent_systems, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cascade_be_t5, cascade_failure_in_interdependent_systems, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cascade_be_t10, cascade_failure_in_interdependent_systems, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cascade_failure_in_interdependent_systems, global_infrastructure).
narrative_ontology:affects_constraint(cascade_failure_in_interdependent_systems, financial_system_interconnection).
narrative_ontology:affects_constraint(cascade_failure_in_interdependent_systems, supply_chain_concentration).
narrative_ontology:affects_constraint(cascade_failure_in_interdependent_systems, power_grid_synchronization).

% DUAL FORMULATION NOTE:
% Cascade failure represents a constraint family with multiple structurally distinct mechanisms: tight coupling as efficiency extraction (Tangled Rope) upstream of catastrophic cascade events themselves (Snare at cascade time). The unified story demonstrates full DR typology; proper decomposition would create separate constraint stories for efficiency-extraction vs cascade-event-extraction, linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
