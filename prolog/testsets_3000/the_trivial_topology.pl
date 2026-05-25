% ============================================================================
% CONSTRAINT STORY: the_trivial_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_trivial_topology, []).

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
 *   constraint_id: the_trivial_topology
 *   human_readable: Trivial Topology Information Asymmetry
 *   domain: technological/network_architecture
 *
 * SUMMARY:
 *   The trivial topology information asymmetry is a structural constraint in
 *   fully connected networks where theoretical connectivity masks practical
 *   information access inequality. While every node has a theoretical path to
 *   every other node, the cost, latency, and cognitive load of accessing
 *   information vary dramatically across the network. Central nodes with
 *   aggregation capacity, caching, and processing power can access
 *   information efficiently; peripheral nodes face routing delays, indirect
 *   paths, and dependence on intermediary processors. This constraint
 *   exhibits all six DR types from different perspectives, making it
 *   diagnostic for how network architecture embeds extraction mechanisms. The
 *   same structural phenomenon — full connectivity with asymmetric access
 *   cost — appears as an immutable law of network physics (mountain), a
 *   coordination mechanism enabling global reach (rope), a temporary problem
 *   solved by decentralized alternatives (scaffold), a performative standard
 *   (piton), a mixed coordination-extraction hybrid (tangled rope), or pure
 *   extraction (snare), depending on the observer's position in the topology.
 *   The theater_ratio (0.61) reflects that modern routing protocols (BGP, DNS
 *   hierarchies, CDN infrastructure) are substantially theatrical: they
 *   perform the ritual of network universality (all nodes connected) while
 *   compensating for the real asymmetry through complex hierarchical
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Peripheral Nodes: Primary victims (powerless/trapped) — theoretically connected but practically dependent on central processors; bear full cost of routing inefficiency and information gatekeeping
 *   - Central Information Processors: Primary beneficiaries (institutional/arbitrage) — positioned to aggregate, cache, and prioritize information; capture efficiency gains and role asymmetry
 *   - Application Developers: Secondary victims (moderate/constrained) — must design around asymmetry through replication, caching, and compensatory logic; can migrate to alternative architectures at switching cost
 *   - Decentralization Movement: Organized actors (organized/constrained) — IPFS, DHT, peer-to-peer communities building alternative architectures with different asymmetry properties
 *   - Legacy Protocol Standards: Institutional actors (institutional/arbitrage) — TCP/IP, DNS, BGP maintain theatrical hierarchies that hide the trivial topology's asymmetry
 *   - Analytical Observer: Universal/civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices as inherent network laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_trivial_topology, 0.52).
domain_priors:suppression_score(the_trivial_topology, 0.58).
domain_priors:theater_ratio(the_trivial_topology, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_trivial_topology, extractiveness, 0.52).
narrative_ontology:constraint_metric(the_trivial_topology, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(the_trivial_topology, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_trivial_topology, tangled_rope).
narrative_ontology:human_readable(the_trivial_topology, "Trivial Topology Information Asymmetry").
narrative_ontology:topic_domain(the_trivial_topology, "technological/network_architecture").

domain_priors:requires_active_enforcement(the_trivial_topology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_trivial_topology, central_information_processors).
narrative_ontology:constraint_beneficiary(the_trivial_topology, network_topology_designers).
narrative_ontology:constraint_victim(the_trivial_topology, peripheral_nodes).
narrative_ontology:constraint_victim(the_trivial_topology, latency_sensitive_applications).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL NODE (SNARE) — A leaf node in a fully connected topology theoretically has a path to all other nodes, but experiences severe latency, throughput, and processing asymmetry. Cannot exit the network; trapped by dependence on central processors for information aggregation and routing decisions. Bears full cost of architectural extraction via forced indirect routing, buffering delays, and information gatekeeping.
constraint_indexing:constraint_classification(the_trivial_topology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: APPLICATION DEVELOPER (TANGLED ROPE) — Must design around the trivial topology's latency and asymmetry constraints. Experiences both coordination (the network enables global reach) and extraction (forced to compensate for inefficient routing, cache management, and replication strategies). Can migrate to alternative architectures (mesh, hierarchical) but at significant switching cost and loss of interoperability.
constraint_indexing:constraint_classification(the_trivial_topology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL INFORMATION PROCESSOR (ROPE) — Benefits from the trivial topology as a pure coordination mechanism: direct connectivity to all nodes enables information aggregation, caching, and request prioritization. Experiences the constraint as enabling rather than extractive. Can arbitrage between network roles (hub, arbiter, relay) and exit into higher-value service layers.
constraint_indexing:constraint_classification(the_trivial_topology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Organized advocates (blockchain communities, peer-to-peer networks, distributed systems researchers) perceive the trivial topology's information asymmetry as a temporary architectural problem with a sunset: content-addressable networks (IPFS), sharding, and local-first protocols are building alternative verification and discovery pathways that reduce dependence on central processors. The extraction mechanism loses force as redundant caching and edge computing mature.
constraint_indexing:constraint_classification(the_trivial_topology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PROTOCOL STANDARD (PITON) — Standards like TCP/IP assume trivial connectivity (all nodes theoretically reachable) but rely on performative hierarchical routing to hide the asymmetry. The protocol standard persists through institutional inertia: IPv6 and modern DNS hierarchy are substantially theatrical compensations for the trivial topology's inefficiency, maintained because migration costs are prohibitive, not because they optimally solve the problem.
constraint_indexing:constraint_classification(the_trivial_topology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION-THEORETIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, some information asymmetry is inherent to any finite network: physical latency, bandwidth constraints, and entropy production make perfect information symmetry impossible. This perspective sees the trivial topology's asymmetry as a natural consequence of thermodynamic limits on communication. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to networks' framing naturalizes what is actually a contingent architectural choice.
constraint_indexing:constraint_classification(the_trivial_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_trivial_topology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_trivial_topology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_trivial_topology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_trivial_topology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_trivial_topology, TR),
    TR >= 0.70.

:- end_tests(the_trivial_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Central processors capture efficiency and control benefits during the network's growth phase (interval 0-10), but the extraction is not maximal — much of the asymmetry is justified by legitimate resource constraints and physics-driven latency. The rising trajectory (0.28 → 0.52) reflects that as the network scales and dependence on central aggregation increases, the extraction mechanism becomes more severe. Suppression (0.58): Moderate-high. Significant barriers to exit include switching costs to alternative architectures, standardization lock-in (TCP/IP ubiquity), and the coordination benefit of global connectivity. However, suppression is not absolute — decentralized alternatives exist and are improving. Theater ratio (0.61): Moderate-high. Modern routing and caching infrastructure (BGP, DNS, CDN) performs substantial theater: they maintain the fiction of network universality and peer equivalence while hiding real hierarchical asymmetry through technical complexity. The rising trajectory (0.38 → 0.61) reflects increasing gap between formal protocol simplicity and actual implementation complexity as networks scale.
 *
 * PERSPECTIVAL GAP:
 *   Peripheral nodes perceive the trivial topology as pure extraction (Snare): they have theoretical access but practical dependence. Central processors perceive it as pure coordination (Rope): global reach enables their legitimate function. Application developers perceive mixed experience (Tangled Rope): the architecture enables reach but forces compensatory design patterns. Decentralization advocates perceive a temporary problem with a sunset (Scaffold): alternatives are maturing. Legacy standards perceive a degraded ritual (Piton): TCP/IP and DNS hierarchies are largely performative solutions to problems that decentralized protocols might solve more elegantly. The analytical observer risks perceiving an immutable law (Mountain) — network asymmetry is inevitable given finite bandwidth and latency. The engine's false summit detector identifies this as naturalization of a contingent architectural choice: the asymmetry is not inherent to networks but to the specific design decision to optimize for central aggregation rather than edge-first decentralization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent position. Central processors with arbitrage options experience low or negative effective extraction (beneficiary position). Peripheral nodes with no exit options experience high effective extraction (trapped victim position). Application developers with constrained options experience moderate extraction (moderate power, constrained exit). Decentralization advocates with organized capacity and partial exit paths experience lower extraction (organized power, constrained exit but with visible alternatives). Legacy protocol standards, as institutional beneficiaries, experience arbitrage-level directionality. The analytical observer at the civilizational level has maximum analytical distance. Each perspective's chi value is derived from these structural positions through the sigmoid f(d) function and scope modifier σ(S). The perspectival gap is large: beneficiaries see rope or scaffold, victims see snare or tangled_rope, observers risk false mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that all six types are perspectival readings of the same architectural structure. The question 'Is trivial topology information asymmetry Rope or Snare?' has no single answer — it depends on the observer's structural position. The beneficiary (central processor) genuinely experiences coordination (Rope). The victim (peripheral node) genuinely experiences extraction (Snare). The scaffold perspective (decentralization advocates) identifies a real structural feature: alternatives are emerging and the extraction mechanism has a sunset date (10-20 years as IPFS, DHT, and edge computing mature). The piton perspective identifies another real feature: legacy protocols maintain the extraction through inertial institutional arrangements. The mountain perspective is false — the engine correctly identifies this as naturalization. The true analytical resolution is that trivial topology information asymmetry is a Tangled Rope from the system's aggregate perspective: it provides genuine coordination (global reach) while embedding extractive asymmetry (dependence on central processors). The perspectival presheaf over all observables is more accurate than any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    central_processing_bottleneck_empirical,
    'What fraction of global information requests pass through a central processor versus peer-to-peer routing in practice?',
    'Network traffic analysis; packet-level measurement of routing paths; comparison of direct peer routes versus centralized aggregation for common query types',
    'If < 20% centralized: information asymmetry is largely a theoretical concern (Mountain). If > 60% centralized: extraction mechanism is real and structural (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(central_processing_bottleneck_empirical, empirical, 'Empirical measurement of central processing bottleneck prevalence').

omega_variable(
    decentralized_alternative_maturity,
    'Do decentralized protocols (IPFS, DHT-based systems, edge computing) actually provide comparable information access latency and reliability as centralized architectures?',
    'Longitudinal performance benchmarks; comparison of p99 latency, availability, and update propagation times between centralized and decentralized systems for canonical workloads',
    'If decentralized ≥ 90% of centralized performance: scaffold sunset is real and imminent (Scaffold from organized perspective). If decentralized < 60% of centralized performance: architectural constraint persists (extraction mechanism remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_alternative_maturity, empirical, 'Whether decentralized alternatives match centralized performance').

omega_variable(
    information_asymmetry_necessity,
    'Is the information asymmetry in trivial topologies a consequence of network physics (latency, bandwidth) or a consequence of architectural choice (hierarchical routing, caching strategies)?',
    'Controlled network experiments: measure asymmetry in flat routing protocols (flooding, gossip) versus hierarchical protocols; analysis of theoretical bounds on information symmetry under constraint of finite resources',
    'If physics-driven: constraint is a Mountain (natural law of networks). If architecture-driven: constraint is extractive design choice (Snare/Tangled Rope). This determines whether decentralization solutions are addressing a real limit or a contingent design artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_necessity, conceptual, 'Whether information asymmetry is physics-driven or architecture-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_trivial_topology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trivtop_tr_t0, the_trivial_topology, theater_ratio, 0, 0.38).
narrative_ontology:measurement(trivtop_tr_t5, the_trivial_topology, theater_ratio, 5, 0.5).
narrative_ontology:measurement(trivtop_tr_t10, the_trivial_topology, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(trivtop_be_t0, the_trivial_topology, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trivtop_be_t5, the_trivial_topology, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(trivtop_be_t10, the_trivial_topology, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_trivial_topology, global_infrastructure).
narrative_ontology:affects_constraint(the_trivial_topology, routing_protocol_gatekeeping).
narrative_ontology:affects_constraint(the_trivial_topology, dns_hierarchy_lock_in).
narrative_ontology:affects_constraint(the_trivial_topology, content_delivery_network_monopsony).

% DUAL FORMULATION NOTE:
% Trivial topology information asymmetry decomposes into three subordinate constraints: (1) routing_protocol_gatekeeping (ε≈0.45) addresses the specific extraction through routing decisions; (2) dns_hierarchy_lock_in (ε≈0.38) addresses institutional lock-in; (3) content_delivery_network_monopsony (ε≈0.58) addresses extraction through edge computing consolidation. The parent constraint represents the abstract architectural feature; subordinates represent specific institutional manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
