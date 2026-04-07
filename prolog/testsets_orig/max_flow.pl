% ============================================================================
% CONSTRAINT STORY: max_flow
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_max_flow, []).

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
 *   constraint_id: max_flow
 *   human_readable: Socio-Technical Bottlenecks Governed by Max-Flow Principles
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Max-Flow Min-Cut theorem, proven in graph theory by Ford and
 *   Fulkerson, establishes that the maximum flow through a network equals the
 *   minimum capacity cut needed to sever all paths from source to sink. In
 *   socio-technical systems, this mathematical structure creates a
 *   rent-extraction mechanism: whoever controls the minimum cut (the
 *   bottleneck) controls the flow and can extract rents from dependent
 *   agents. The constraint is not the theorem itself but the institutional
 *   apparatus that uses the theorem's inevitability to justify monopoly
 *   control. This creates a tension between legitimate coordination (managing
 *   scarce capacity) and extractive rent-seeking (charging rents that exceed
 *   the cost of coordination). Different agents experience this constraint
 *   differently depending on their structural position relative to the
 *   bottleneck: those trapped on one side of the cut see pure extraction;
 *   those managing the cut see coordination; those building alternatives see
 *   a temporary problem with a sunset. The theater ratio has increased over
 *   the interval as bottleneck controllers have become more sophisticated in
 *   justifying their rents through appeals to network efficiency and
 *   mathematical necessity.
 *
 * KEY AGENTS:
 *   - Flow-Dependent Producer: Primary victim (powerless/trapped) — completely reliant on bottleneck access, cannot negotiate capacity, bears extraction costs
 *   - Competing Flow Agent: Secondary victim (moderate/constrained) — shares bottleneck, faces extraction but has limited routing alternatives
 *   - Bottleneck Controller: Primary beneficiary (institutional/arbitrage) — captures rents from scarce capacity; sees role as essential coordination
 *   - Infrastructure Coalition: Organized actors (organized/constrained) — regulators, alternative network builders, open standards consortia developing parallel capacity
 *   - Legacy Routing System: Institutional actor (institutional/arbitrage) — maintains hub-and-spoke or centralized architectures through inertia; sees own processes as degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — at risk of naturalizing contingent institutional bottlenecks as mathematical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(max_flow, 0.52).
domain_priors:suppression_score(max_flow, 0.58).
domain_priors:theater_ratio(max_flow, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(max_flow, extractiveness, 0.52).
narrative_ontology:constraint_metric(max_flow, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(max_flow, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(max_flow, tangled_rope).
narrative_ontology:human_readable(max_flow, "Socio-Technical Bottlenecks Governed by Max-Flow Principles").
narrative_ontology:topic_domain(max_flow, "technological/economic").

domain_priors:requires_active_enforcement(max_flow).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(max_flow, bottleneck_controller).
narrative_ontology:constraint_beneficiary(max_flow, capacity_arbitrageur).
narrative_ontology:constraint_victim(max_flow, flow_dependent_agents).
narrative_ontology:constraint_victim(max_flow, systemic_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOW-DEPENDENT PRODUCER (SNARE) — Completely dependent on bottleneck access. No alternative routing, cannot negotiate capacity, bears full cost of congestion. Maximum extraction: pays rent for access that is structurally necessary. Example: semiconductor fabrication dependent on rare earth supply chains, or content creators dependent on platform distribution.
constraint_indexing:constraint_classification(max_flow, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING FLOW AGENT (TANGLED ROPE) — Shares bottleneck capacity with others. Experiences mixed coordination (capacity sharing solves the joint resource problem) and extraction (bottleneck controller allocates capacity asymmetrically). Can route around some constraints but faces switching costs. Benefits from the coordination mechanism while bearing extraction costs.
constraint_indexing:constraint_classification(max_flow, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BOTTLENECK CONTROLLER (ROPE) — Perceives the constraint as pure coordination: managing the narrow path solves a collective action problem. Benefits from rent extraction during high demand and from the necessity of their role. Low experienced extraction because they control the constraint.
constraint_indexing:constraint_classification(max_flow, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFRASTRUCTURE COALITION (SCAFFOLD) — Organized actors (regulators, alternative infrastructure builders, open standards consortia) recognize the bottleneck as a temporary inefficiency with a sunset clause. Investments in parallel capacity, mesh networks, or decentralized alternatives represent structural exit paths. The constraint's extractiveness declines as these pathways mature.
constraint_indexing:constraint_classification(max_flow, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ROUTING SYSTEM (PITON) — Traditional flow management (hub-and-spoke networks, centralized packet routing, master-detail supply contracts) persists through institutional inertia. The actual functional verification of these routing protocols is mostly theatrical — institutions maintain them because alternatives haven't fully supplanted them, not because they optimize flow. Theater ratio is high; extractiveness is low but structurally maintained.
constraint_indexing:constraint_classification(max_flow, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATHEMATICAL THEOREM VIEW (MOUNTAIN) — From a universal/analytical perspective, the Max-Flow Min-Cut theorem is a mathematical necessity: any flow network has a bottleneck, and that bottleneck capacity equals the maximum flow. This is logically immutable. However, the constraint story is not about the theorem itself but about socio-technical systems where the mathematical structure becomes a rent-extraction mechanism. The false summit detection reveals that 'inevitability of bottlenecks' is used to naturalize the distribution of rent from those bottlenecks.
constraint_indexing:constraint_classification(max_flow, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(max_flow_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(max_flow, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(max_flow, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(max_flow, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(max_flow, TR),
    TR >= 0.70.

:- end_tests(max_flow_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The bottleneck controller extracts rents from dependent agents, but the extraction is not total because some flow-dependent agents have constrained (not fully trapped) exit options. The measurement trajectory from 0.28 to 0.52 reflects increasing sophistication in rent capture and legitimation as digital and supply chain networks have deepened dependency. Suppression (0.58): Moderate-high. Significant barriers to exit include switching costs, network effects, geographic lock-in (Suez Canal, Panama Canal), and regulatory capture of alternative routes. However, suppression is not total because organized actors are building alternatives and standards bodies are creating competing protocols. Theater ratio (0.64): High. Bottleneck controllers use appeals to mathematical necessity, network efficiency, and impossibility of alternatives to justify rents. The actual operational complexity of managing the bottleneck is modest; much of the rationale is theatrical legitimation. The trajectory from 0.48 to 0.64 reflects increasing use of technical language and mathematical arguments to obscure the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the bottleneck controller (who sees coordination and necessity) and the flow-dependent producer (who sees extraction and trap). The controller views the constraint as solving a collective action problem: limited capacity must be allocated somehow, and centralized management is efficient. The producer views the same constraint as an extraction mechanism: they are forced to pay rents for access to a scarce resource they cannot avoid. The competing flow agent occupies the middle ground, experiencing both coordination (shared capacity management) and extraction (asymmetric rent allocation). The infrastructure coalition explicitly rejects the premise of inevitability, viewing the bottleneck as temporary — their scaffolding perspective sees declining extractiveness as alternatives mature. The legacy routing system sees its own process as degraded (piton), maintained not because it works well but because institutions have invested in it. The analytical observer risks naturalizing the bottleneck as a mathematical necessity, falsely transferring the inevitability of the theorem to the contingency of the institutional arrangement that exploits it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the bottleneck. Flow-dependent producers have d ≈ 0.95 (trapped, no arbitrage, full victim status) — maximum experienced extraction. Competing flow agents have d ≈ 0.60 (constrained exit, partial victim, some organizing capacity) — moderate extraction. Bottleneck controllers have d ≈ 0.05 (arbitrage exit, full beneficiary, control position) — negative or minimal extraction. Infrastructure coalitions have d ≈ 0.50 (constrained but organized exit, building alternatives, asymmetric but not complete victim status) — moderate extraction but declining over time as alternatives mature. Legacy routing systems have d ≈ 0.20 (institutional beneficiary, arbitrage exit in some contexts, theatrical maintenance) — low extraction despite high theater. The analytical observer has d ≈ 0.72 (analytical exit, seeing the full structure but risking naturalization) — high analytical extraction but structured differently than economic extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Max-Flow Min-Cut theorem establishes the existence and location of bottlenecks but does NOT establish the distribution of rents from those bottlenecks. The mathematical fact (there is a minimum cut with capacity equal to maximum flow) is invariant and generates the mountain perspective at the analytical level. But the socio-technical system — who controls the cut, who extracts rents, who bears costs — is entirely contingent and institutional. The bottleneck is inevitable; the monopoly control of it is not. The constraint story correctly separates the theorem (universal, immutable) from the institutional exploitation of the theorem (contingent, contestable). The false summit detector identifies the mountain classification as naturalization: the analytical observer who claims 'bottlenecks are inevitable, therefore monopoly rent is inevitable' has confused a necessary property (the existence of a cut) with a contingent one (who controls that cut). The mandatrophy is resolved by clarifying that the constraint is the socio-technical exploitation of the mathematical structure, not the mathematical structure itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_surplus_threshold,
    'At what ratio of actual flow to theoretical max capacity does the bottleneck transition from coordination to pure extraction?',
    'Empirical measurement of flow utilization rates across diverse networks (transport, communication, supply chain); correlation with extraction price premiums charged during high utilization periods',
    'If threshold is low (< 60% utilization): most bottlenecks are predominantly extractive. If threshold is high (> 85% utilization): bottlenecks serve primary coordination function during peak demand, with extraction only during surge pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_surplus_threshold, empirical, 'Capacity utilization threshold at which bottleneck transitions from coordination to extraction').

omega_variable(
    alternative_capacity_build_feasibility,
    'Can alternative routing paths or parallel capacity reliably overcome the bottleneck within economically viable timeframes, or is the bottleneck structurally permanent?',
    'Historical case studies of bottleneck relief: Suez Canal alternatives (Suez Canal Authority control, New Suez Canal expansion), internet routing (BGP divergence, alternate path investment), semiconductor capacity (Taiwan TSMC capacity vs other foundries). Measurement of lead times and capital costs for parallel capacity.',
    'If alternatives are feasible: scaffold sunset is real and extractiveness should be lower. If alternatives are structurally blocked (geography, network effects, capital barriers): bottleneck is closer to mountain (inevitable constraint on the system).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_capacity_build_feasibility, empirical, 'Whether alternative capacity can be built to overcome the bottleneck').

omega_variable(
    rent_extraction_vs_maintenance_cost,
    'Do the rents extracted from bottleneck control proportionally cover the cost of maintaining and upgrading capacity, or do they exceed operational necessity?',
    'Comparative financial analysis: operating costs of bottleneck infrastructure vs rents charged; industry benchmarking of margin rates for monopoly vs competitive capacity providers; long-term capital expenditure patterns.',
    'If extraction equals maintenance cost: pure coordination frame is justified. If extraction exceeds maintenance cost: bottleneck controller captures monopoly rent beyond coordination necessity, shifting classification toward snare for flow-dependent agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_extraction_vs_maintenance_cost, empirical, 'Whether bottleneck rents exceed maintenance costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(max_flow, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maxflow_tr_t0, max_flow, theater_ratio, 0, 0.48).
narrative_ontology:measurement(maxflow_tr_t5, max_flow, theater_ratio, 5, 0.58).
narrative_ontology:measurement(maxflow_tr_t10, max_flow, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(maxflow_be_t0, max_flow, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(maxflow_be_t5, max_flow, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(maxflow_be_t10, max_flow, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(max_flow, resource_allocation).
narrative_ontology:affects_constraint(max_flow, supply_chain_concentration).
narrative_ontology:affects_constraint(max_flow, platform_gatekeeper_power).
narrative_ontology:affects_constraint(max_flow, spectrum_allocation_monopoly).
narrative_ontology:affects_constraint(max_flow, shipping_lane_control).

% DUAL FORMULATION NOTE:
% The max-flow-min-cut theorem itself (constraint_id: max_flow_theorem) is a mathematical mountain with ε ≈ 0.08. This story addresses the socio-technical application of the theorem to justify bottleneck rent extraction. The two are structurally distinct: the theorem is immutable; the institutional control of bottlenecks is contingent. Network edges link to specific bottleneck systems where this constraint manifests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(max_flow, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
