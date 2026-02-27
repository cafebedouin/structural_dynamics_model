% ============================================================================
% CONSTRAINT STORY: max_flow
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The max-flow min-cut theorem is a mathematical invariant: in any flow
 *   network, the maximum flow equals the minimum cut capacity separating
 *   source from sink. This structural property of graphs is inevitable.
 *   However, the socio-technical application of max-flow principles creates a
 *   distinct constraint: institutions, infrastructure providers, and
 *   gatekeepers use the min-cut as a control point to extract economic value
 *   through artificial capacity restrictions, regulatory capture, and
 *   strategic bottleneck maintenance. This constraint exhibits the full
 *   perspectival range because it conflates a topological mathematical
 *   invariant with governance choices that are contingent and restructurable.
 *   The bottleneck controller benefits from enforcing a min-cut and capturing
 *   the differential rents between what flows and what could flow.
 *   Flow-dependent actors are trapped behind the bottleneck with no
 *   alternative routes. Infrastructure investors see this as a temporary
 *   coordination problem with a clear sunset: distributed routing, mesh
 *   networks, and regulatory reform can eliminate the artificial scarcity.
 *   The legacy bottleneck institution (toll roads, international ports,
 *   telecom hubs) maintains its position through theater — pricing and
 *   regulatory behavior that appears to reflect genuine capacity constraints
 *   but increasingly reflects institutional inertia. The analytical observer
 *   risks naturalizing what is actually a governance choice as a topological
 *   necessity.
 *
 * KEY AGENTS:
 *   - Bottleneck Controller: Primary beneficiary (institutional/arbitrage) — captures rents through capacity control and strategic allocation decisions
 *   - Flow-Dependent Actors: Primary victims (powerless/trapped) — bear full cost of restricted capacity with no routing alternatives
 *   - Secondary Flow Networks: Secondary victims (moderate/constrained) — limited alternate routes but some coordination benefit
 *   - Network Coordinator: Institutional beneficiary (institutional/arbitrage) — experiences constraint as coordination problem; genuine allocation function
 *   - Infrastructure Investor Coalition: Organized actors (organized/mobile) — seeing bottleneck as temporary; investing in alternatives
 *   - Legacy Bottleneck Institution: Institutional gatekeeper (institutional/arbitrage) — maintaining position through performative regulation and switching costs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks mistaking governance choice for mathematical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(max_flow, 0.52).
domain_priors:suppression_score(max_flow, 0.58).
domain_priors:theater_ratio(max_flow, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(max_flow, extractiveness, 0.52).
narrative_ontology:constraint_metric(max_flow, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(max_flow, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(max_flow, tangled_rope).
narrative_ontology:human_readable(max_flow, "Socio-Technical Bottlenecks Governed by Max-Flow Principles").
narrative_ontology:topic_domain(max_flow, "technological/economic").

domain_priors:requires_active_enforcement(max_flow).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(max_flow, bottleneck_controller).
narrative_ontology:constraint_beneficiary(max_flow, capacity_restrictor).
narrative_ontology:constraint_victim(max_flow, flow_dependent_actors).
narrative_ontology:constraint_victim(max_flow, system_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOW-DEPENDENT ACTOR (SNARE) — Trapped by the min-cut bottleneck with no alternative routing. Bears full cost of capacity restrictions. Cannot exit without catastrophic loss. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.68. Pure extraction.
constraint_indexing:constraint_classification(max_flow, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY FLOW NETWORK (TANGLED ROPE) — Constrained by limited alternate routes and network topology, but also benefits from capacity coordination that enables some flow. Mixed: genuine coordination problem (network routing) plus asymmetric extraction (those controlling min-cut capture value). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(max_flow, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NETWORK COORDINATOR (ROPE) — Institutional beneficiary with arbitrage options. Controls capacity allocation and experiences the constraint as pure coordination: routing flows efficiently solves a genuine collective action problem. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(max_flow, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFRASTRUCTURE INVESTOR COALITION (SCAFFOLD) — Organized actors (regulators, tech consortia, venture capital) treating capacity bottlenecks as temporary coordination failures with a sunset: network expansion, redundancy protocols, and decentralized routing (mesh networks, blockchain-verified routing) are building alternative pathways. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.21. Low effective extraction due to coalition agency and visible exit path.
constraint_indexing:constraint_classification(max_flow, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BOTTLENECK INSTITUTION (PITON) — Traditional chokepoint gatekeepers (toll roads, international ports, fiber-optic hubs) whose bottleneck function is now largely performative. Theater ratio 0.68 reflects that much regulatory and pricing behavior around the bottleneck persists through institutional inertia, not genuine coordination need. Modern alternatives exist (bypass routes, multimodal transport, mesh networks) but the legacy institution maintains its position through regulatory capture and switching costs.
constraint_indexing:constraint_classification(max_flow, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TOPOLOGICAL VIEW (MOUNTAIN) — From a civilizational/universal perspective, some min-cut constraint is inherent to any finite network topology: graph structure alone guarantees that flow is bounded by the minimum cut separating source from sink. This appears as a natural law. However, the structural data (ε=0.52, suppression=0.58, theater=0.68) reveals this as a false summit: the topological min-cut (a mathematical invariant) is being conflated with socio-technical bottleneck governance (a contingent institutional arrangement). The theorem constrains flows; socio-technical actors extract value through controlling the cut.
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
 *   Extractiveness (0.52): Moderate-high. The bottleneck controller captures significant economic value through capacity restrictions, pricing power, and strategic allocation — but the extraction is not absolute because some routing coordination is genuine (network paths must be selected, capacity must be allocated). The value increased over the interval as gatekeepers recognized the bottleneck's strategic importance and extracted more aggressively. Suppression (0.58): Moderate-high. Significant barriers to bypassing the bottleneck include physical/topological constraints, regulatory capture, switching costs, and incumbency advantages. But suppression is not total — alternative technologies (mesh networks, decentralized routing, multimodal transport) are technologically feasible, creating a suppression floor at ~0.50 even in the long term. Theater ratio (0.68): Elevated. The bottleneck institution's behavior increasingly appears performative: pricing and regulatory decisions that claim to reflect physical capacity constraints actually reflect institutional convenience and rent-seeking. The theater has grown because legacy institutions have realized that purely extractive postures generate political resistance; theatrical justification (network efficiency, capacity management, safety compliance) provides cover for gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The bottleneck controller sees a coordination solution (Rope) — genuine network allocation. The flow-dependent actor sees pure extraction (Snare) — no alternatives, trapped, full cost-bearing. The infrastructure coalition sees a temporary problem (Scaffold) — alternatives are being built, sunset is real. The analytical observer sees topological necessity (Mountain) — the min-cut is a mathematical fact. The legacy institution sees its own degrading ritual (Piton) — the gatekeeping role persists but its justification is now mostly theater. The secondary network sees mixed extraction-coordination (Tangled Rope) — some routing benefit, some cost from restricted capacity. The perspectives diverge because they embody different structural positions relative to the min-cut: controller, victim, reformer, theorist, incumbent, secondary user. No single type resolves all positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Bottleneck controller: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; low effective extraction because institutional position enables optimal outcomes. Flow-dependent actor: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction — no exit options. Secondary flow network: Victim + constrained → d≈0.68, f(d)≈1.02. High extraction but not absolute; some benefits from routing coordination. Infrastructure coalition: Organized + mobile → d≈0.35, f(d)≈0.33. Low effective extraction due to coalition agency and visible alternatives. Legacy institution: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification comes from theater gate (0.68 ≥ 0.70 is close; 0.68 reflects that performative behavior is substantial but not yet dominant). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is a false summit — the topological min-cut constrains graphs; it does not constrain whether institutions exploit that topology for rent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by distinguishing the mathematical theorem (topological min-cut = mountain) from the socio-technical governance system (bottleneck control = tangled_rope). The false summit (mountain perspective) conflates these two distinct concepts. The true structure is: (1) the topological min-cut is invariant and immutable (mathematical fact), but (2) the exploitation of that min-cut through institutional gatekeeping is contingent and restructurable (governance choice). The theorem's inevitability does not make the governance system's extractive behavior inevitable. This is a canonical case where naturalizing language ('bottlenecks are inherent to networks') hides a governance choice. The mandatrophy is resolved by decomposing: one story (topological_min_cut_theorem) would be a true mountain; this story (max_flow_min_cut_governance) is tangled_rope because it models the institutional exploitation, not the mathematical invariant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    topological_necessity_boundary,
    'Is the observed bottleneck a consequence of network topology (mathematical min-cut) or of governance/allocation decisions that artificially restrict what topology could support?',
    'Network redesign studies; analysis of whether capacity restrictions are physical limits or policy choices; investigation of alternative topologies (mesh, redundant, distributed) that could bypass the min-cut.',
    'If topological: bottleneck is a mountain (natural limit). If governance: bottleneck is tangled_rope (artificial extraction). Determines whether the constraint is immutable or restructurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(topological_necessity_boundary, empirical, 'Whether bottleneck is topological necessity or governance choice').

omega_variable(
    value_extraction_vs_coordination_service,
    'How much of the bottleneck controller''s economic return comes from providing genuine routing coordination vs. rent extraction through artificial scarcity?',
    'Cost analysis of bottleneck operation; comparison of pricing to marginal cost of capacity; measurement of deadweight loss from artificial capacity restrictions; analysis of controller behavior when bypass alternatives emerge.',
    'If mostly coordination: rope from controller perspective is accurate. If mostly extraction: snare/tangled_rope is accurate. Determines whether beneficiary classification is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_extraction_vs_coordination_service, empirical, 'Ratio of coordination service to rent extraction').

omega_variable(
    alternative_pathway_viability,
    'Are alternative pathways (decentralized routing, mesh networks, regulatory bypass, multimodal transport) structurally viable or only theoretically possible?',
    'Cost comparison of alternative routes vs bottleneck; deployment timelines for alternatives; regulatory barriers to alternatives; historical precedent of technology/regulation shifts that eliminated bottlenecks.',
    'If viable: scaffold perspective is accurate, sunset is real. If not viable: trapped perspective prevails, snare classification persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Viability of alternative pathways to bypass bottleneck').

omega_variable(
    institutional_control_durability,
    'How stable is the institutional control of the min-cut? Can bottleneck controllers maintain their gatekeeping role as technology and regulation evolve?',
    'Historical analysis of technology-driven obsolescence of bottlenecks (toll roads vs. highways, centralized routing vs. distributed protocols); strategic behavior of gatekeepers when facing alternatives.',
    'If stable: bottleneck persists as piton or snare. If unstable: scaffold sunset timeline shortens, transforming piton to institutional degradation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_control_durability, empirical, 'Durability of institutional control over min-cut').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(max_flow, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfmc_tr_t0, max_flow, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mfmc_tr_t5, max_flow, theater_ratio, 5, 0.58).
narrative_ontology:measurement(mfmc_tr_t10, max_flow, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(mfmc_be_t0, max_flow, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mfmc_be_t5, max_flow, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mfmc_be_t10, max_flow, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(max_flow, resource_allocation).
narrative_ontology:affects_constraint(max_flow, infrastructure_gatekeeping).
narrative_ontology:affects_constraint(max_flow, supply_chain_chokepoints).
narrative_ontology:affects_constraint(max_flow, regulatory_capture_logistics).

% DUAL FORMULATION NOTE:
% This story models the socio-technical application of max-flow governance, not the mathematical theorem itself. A separate constraint story (topological_min_cut_theorem) would model the pure mathematical invariant as a mountain. The network decomposition reflects the distinction between theorem (ε ≈ 0.05, mountain) and governance (ε ≈ 0.52, tangled_rope). The theorem is downstream of pure mathematics; the governance system is downstream of institutional choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(max_flow, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
