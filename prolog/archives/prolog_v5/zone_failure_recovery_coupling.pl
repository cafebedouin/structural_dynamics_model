% ============================================================================
% CONSTRAINT STORY: zone_failure_recovery_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zone_failure_recovery_coupling, []).

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
 *   constraint_id: zone_failure_recovery_coupling
 *   human_readable: Zone Failure Recovery Coupling
 *   domain: infrastructure/systems_reliability
 *
 * SUMMARY:
 *   Zone failure recovery coupling describes the structural interdependence
 *   between geographically separated power zones where the failure of one
 *   zone's generation or distribution infrastructure cascades to adjacent
 *   zones through power flow physics and automated recovery protocols. This
 *   constraint operates across multiple scales: the physics of electrical
 *   coupling (Kirchhoff's laws create mandatory interdependence), the
 *   engineering of protective relays (load-shedding algorithms that isolate
 *   failures), the economics of grid operation (operators balance local
 *   reliability against inter-zone coordination costs), and the regulation of
 *   energy markets (zoning boundaries often follow utility monopoly
 *   territories rather than electrical topology). The constraint exhibits
 *   tangled rope structure: genuine coordination benefits exist (shared
 *   redundancy, emergency protocols, pooled reserves) alongside asymmetric
 *   extraction (operators in failure zones control who bears cascading
 *   costs). Theater ratio has increased over the measurement interval as
 *   automated recovery protocols have become more complex and opaque, while
 *   extractiveness has risen as operators accumulate more discretion in
 *   recovery sequence decisions. The constraint faces structural pressure
 *   from decentralization (microgrids, distributed storage, local renewable
 *   generation) which would reduce inter-zone dependency, and from regulatory
 *   capture (operators may resist decentralization to preserve central
 *   coordination control).
 *
 * KEY AGENTS:
 *   - Zone Operators: Primary beneficiaries (institutional/arbitrage) — control recovery protocols, can shift failure costs to adjacent zones through restoration sequencing decisions
 *   - End Users in Cascading Zones: Primary victims (powerless/trapped) — no alternative infrastructure; geographic dependency with no exit option
 *   - Adjacent Zone Operators: Secondary victims (moderate/constrained) — bear costs of upstream failures; also benefit from shared redundancy and can organize through sectoral coordination
 *   - System Resilience Authority: Institutional coordinator (institutional/arbitrage) — sets standard protocols; benefits from perceived control over distributed risk
 *   - Grid Decentralization Coalition: Organized agents (organized/constrained) — renewable developers, microgrids, storage operators with exit option through technical alternatives
 *   - Legacy Centralized Grid: Institutional beneficiary (institutional/arbitrage) — perpetuates zone-based operations; sees decentralization as threat to utility business model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zone_failure_recovery_coupling, 0.58).
domain_priors:suppression_score(zone_failure_recovery_coupling, 0.62).
domain_priors:theater_ratio(zone_failure_recovery_coupling, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zone_failure_recovery_coupling, tangled_rope).
narrative_ontology:human_readable(zone_failure_recovery_coupling, "Zone Failure Recovery Coupling").
narrative_ontology:topic_domain(zone_failure_recovery_coupling, "infrastructure/systems_reliability").

domain_priors:requires_active_enforcement(zone_failure_recovery_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zone_failure_recovery_coupling, zone_operators).
narrative_ontology:constraint_beneficiary(zone_failure_recovery_coupling, system_resilience_infrastructure).
narrative_ontology:constraint_victim(zone_failure_recovery_coupling, adjacent_zone_operators).
narrative_ontology:constraint_victim(zone_failure_recovery_coupling, end_users_in_cascading_zones).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER IN CASCADING FAILURE (SNARE) — Cannot exit the coupled failure chain; bears full cost of zone failures in adjacent sectors. No alternative infrastructure; geographic and economic dependency. Maximum extraction with minimal coordination benefit. The user's only 'choice' is to absorb the outage.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ADJACENT ZONE OPERATOR (TANGLED ROPE) — Constrained by technical coupling and regulatory coordination requirements, but also benefits from shared redundancy infrastructure and cross-zone emergency protocols. Must coordinate recovery sequences (coordination gain) while bearing cost of dependent zones' failures (extraction). Medium agency through sectoral coordination bodies.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM RESILIENCE AUTHORITY (ROPE) — Benefits from coordination of distributed recovery protocols. Views zone coupling as a solvable coordination problem with technical solutions (redundancy standards, communication protocols, priority restoration rules). Experiences the constraint as coordination infrastructure itself — the mechanisms they deploy ARE the solution.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GRID DECENTRALIZATION COALITION (SCAFFOLD) — Organized agents (renewable microgrids, distributed storage, local resilience networks) see zone coupling as a temporary failure mode being solved by decentralization. Constraint has sunset: as local generation and storage mature, inter-zone dependency weakens. Current high suppression will decline as agents gain technical exit options.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CENTRALIZED GRID (PITON) — The centralized grid architecture persists through institutional inertia despite known inefficiency at managing zone coupling. Regulatory frameworks, utility monopolies, and sunk infrastructure investments maintain the topology. Recovery protocols (automated load-shedding, demand response) are largely performative theater — they manage symptoms rather than address architectural coupling. Zone coupling is a feature of the degraded system, not a bug that gets fixed.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, power flow coupling between zones is a fundamental constraint of grid physics: electrical current follows least-resistance paths regardless of zoning boundaries. Kirchhoff's laws create inherent interdependence. This view naturalizes zone coupling as a law of physics rather than a contingent institutional choice. The engine's false summit detector will reveal whether this is genuine physical law or naturalization of architectural choices.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zone_failure_recovery_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zone_failure_recovery_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zone_failure_recovery_coupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zone_failure_recovery_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(zone_failure_recovery_coupling, TR),
    TR >= 0.70.

:- end_tests(zone_failure_recovery_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Zone operators capture substantial value through discretion in restoration sequencing — they can deprioritize restoration in adjacent zones to preserve their own load. The extraction is not total (genuine coordination protocols constrain this; regulators impose restoration priorities) but substantial. The measurement trajectory shows rising extractiveness as automation increases operator discretion through more complex algorithms. Suppression (0.62): High. Substantial barriers exist to exit: geographic dependency (users cannot relocate), regulatory barriers (grid codes require interconnection; decentralization faces utility opposition), technical barriers (intermittency requires coordination even in microgrids), and information barriers (cascade prediction is imperfect). Theater ratio (0.55): Moderate-high. Recovery protocols involve genuinely functional elements (automated load-shedding prevents cascade spread) but increasingly theatrical elements (complex optimization algorithms serve regulatory compliance more than improved outcomes; demand response theater creates illusion of user participation without actual control). The rising trajectory reflects that as automation advances, protocols become more opaque and performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals how the same physical phenomenon is experienced as coordination (rope for system authority), mixed coordination/extraction (tangled rope for adjacent operators), pure extraction (snare for trapped users), temporary failure (scaffold for decentralizers), degradation (piton for centralized grid), and natural law (mountain for analytical observer). The gap is structurally meaningful: zone operators who control restoration protocols experience the constraint as coordinating distributed resources (rope); users at the end of restoration queues experience it as pure extraction (snare). The piton perspective identifies that recovery protocols are increasingly theatrical — they satisfy regulatory requirements and create appearance of control without preventing the underlying problem (zone coupling). The scaffold perspective is real but conditional — decentralization reduces coupling only if intermittency does not recreate it through demand-side coordination. The mountain perspective risks naturalizing architecture as physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their structural relationship to the failure-cost flow. Zone operators who control restoration protocols have low d (they are beneficiaries — costs flow away from them through protocol design). End users with no alternatives have high d (costs flow entirely toward them). Adjacent zone operators have medium-high d (they face costs from upstream failures but can organize and have some protocol participation). System resilience authority has low d (they benefit from the coordination infrastructure they maintain). The scaffold coalition has low-to-medium d (they face constraints but have technical exit paths and organization). The measured extractiveness scales with f(d) — the snare perspective (trapped user, high d) produces high chi; the rope perspective (system authority, low d) produces low chi. Beneficiary/victim declarations directly drive this computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Zone failure recovery coupling resolves the mandatrophy by decomposing the constraint family into physics-level coupling (mountain — Kirchhoff's laws are immutable) and institutional-level extraction (tangled rope/snare — recovery protocol design is contingent). The false summit detector in the mountain perspective reveals this decomposition: 'zone coupling as a law of physics' is accurate for power flow (true mountain), but 'inability to prevent cascading failures' is institutional (contingent design choice, not law). The constraint family should decompose: (1) power_flow_kirchhoff_coupling (ε=0.05, Mountain) — fundamental physics; (2) zone_failure_recovery_protocols (ε=0.58, Tangled Rope) — institutional choice of who bears cascading costs. The measurement trajectory (rising theater ratio and extractiveness) reflects that institutional extraction has increased as automation has concentrated operator discretion. The scaffold perspective's sunset is conditional on whether decentralization actually decouples zones or merely shifts the coupling to demand-side coordination. Mandatrophy resolution requires keeping physics (mountain) and institutions (tangled rope) in separate stories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    architecture_vs_physics_coupling,
    'Is zone failure coupling primarily a constraint of electromagnetic physics or a consequence of centralized grid architecture design?',
    'Comparative analysis of grid architectures (centralized vs distributed microgrids) and their susceptibility to cascading failures; measurement of coupling strength in islanded vs interconnected networks',
    'If physics-driven: mountain classification valid. If architecture-driven: the constraint is contingent and should decompose into separate stories for circuit topology, regulatory boundaries, and economic incentives. Foundational to whether the ''law of nature'' framing is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(architecture_vs_physics_coupling, empirical, 'Whether zone coupling is physical law or architectural choice').

omega_variable(
    cascading_failure_predictability,
    'Are cascading failures between zones predictable and preventable through system design, or are they fundamentally stochastic phenomena that exceed operator control?',
    'Historical analysis of major cascading events; correlation between prevention protocol deployment and actual cascade prevention; machine learning analysis of failure prediction accuracy at different lead times',
    'If predictable: coordination mechanisms (rope/tangled rope) are viable and suppression is policy-contingent. If stochastic: suppression is inherent and snare classification is permanent. Determines whether the scaffold sunset is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascading_failure_predictability, empirical, 'Predictability and preventability of cascading zone failures').

omega_variable(
    decentralization_technical_viability,
    'Can distributed renewable generation and local storage actually decouple zone dependencies at scale, or does the intermittency problem recreate coupling through demand-side coordination requirements?',
    'Modeling of large-scale microgrid scenarios; empirical data from deployed microgrids on inter-zone coordination frequency; analysis of whether intermittency management simply shifts coupling from supply-side (generation) to demand-side (storage coordination)',
    'If decentralization viable: scaffold sunset is structural. If intermittency recreates coupling: the constraint evolves rather than resolves — same extraction mechanism under different labels. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_technical_viability, empirical, 'Technical viability of decentralization to escape zone coupling').

omega_variable(
    regulatory_capture_in_resilience,
    'Do grid operators use zone-coupling recovery protocols as regulatory theater to avoid structural grid upgrades and distributed generation that would reduce their operational control?',
    'Analysis of investment allocation (spending on redundancy/protocols vs grid topology modernization); correlation between regions with strong decentralization policies and regions with aging zone-coupled grids; rate of protocol obsolescence replacement vs actual failure prevention',
    'If regulatory capture: piton classification confirmed and the institutional identity_locked dynamics explain why the constraint persists despite technical alternatives. Suppression is partly institutional choice rather than technical necessity. Affects beneficiary identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_resilience, empirical, 'Whether resilience protocols serve or obstruct decentralization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zone_failure_recovery_coupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zfrc_tr_t0, zone_failure_recovery_coupling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(zfrc_tr_t10, zone_failure_recovery_coupling, theater_ratio, 10, 0.45).
narrative_ontology:measurement(zfrc_tr_t20, zone_failure_recovery_coupling, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(zfrc_be_t0, zone_failure_recovery_coupling, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(zfrc_be_t10, zone_failure_recovery_coupling, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(zfrc_be_t20, zone_failure_recovery_coupling, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zone_failure_recovery_coupling, resource_allocation).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, power_flow_kirchhoff_coupling).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, utility_monopoly_regulatory_capture).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, renewable_intermittency_coordination).

% DUAL FORMULATION NOTE:
% Zone failure recovery coupling decomposes into two structurally distinct constraints: power flow physics (immutable, mountain) and recovery protocol design (contingent, tangled rope). The physics constraint is downstream of neither. The institutional constraint is downstream of utility monopoly structure and upstream of grid decentralization. See constraint family documentation for decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zone_failure_recovery_coupling, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
