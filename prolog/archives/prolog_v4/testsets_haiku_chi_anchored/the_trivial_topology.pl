% ============================================================================
% CONSTRAINT STORY: the_trivial_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   The trivial topology information asymmetry presents a structural
 *   constraint in fully connected networks where nominal connectivity masks
 *   severe information access disparities. While graph theory treats a fully
 *   connected (complete) network as topologically trivial — all nodes
 *   equidistant — the operational reality involves asymmetric access costs:
 *   some nodes serve as information brokers, others as peripheral consumers.
 *   This constraint exhibits multiple classification perspectives depending
 *   on whether one views the asymmetry as: (a) an immutable consequence of
 *   distributed systems physics (Mountain), (b) a legitimate coordination
 *   mechanism (Rope), (c) a technological choice that enables but also
 *   extracts value (Tangled Rope), (d) a pure extraction mechanism masked by
 *   formal connectivity (Snare), or (e) a degraded institutional artifact now
 *   supplanted by peer-to-peer alternatives (Piton). The constraint's
 *   extractiveness has increased from 0.28 to 0.52 over three decades as
 *   network scale has grown and brokerage power has concentrated. Theater
 *   ratio has risen from 0.42 to 0.58, reflecting that protocol governance
 *   (RFCs, standards committees, infrastructure management) has become
 *   increasingly ceremonial relative to actual technical constraints. The
 *   emergence of peer-to-peer and mesh protocols demonstrates that the
 *   asymmetry is primarily institutional choice rather than physical law.
 *
 * KEY AGENTS:
 *   - Peripheral Nodes: Primary victim (powerless/trapped) — depend on central brokers for information access; no alternative routing options; suffer latency amplification and filtering
 *   - Central Information Brokers: Primary beneficiary (institutional/arbitrage) — capture routing traffic, metadata access, and information filtering capability; low-cost coordination advantage
 *   - Distributed Agent Networks: Secondary victim (moderate/constrained) — benefit from network coordination but constrained by processing dependencies on central nodes
 *   - Protocol Design Authority: Organized actor (organized/constrained) — standards bodies and infrastructure consortia; constrained by legacy systems and installed base but capable of reshaping topology
 *   - Legacy Protocol Stack: Institutional artifact (institutional/arbitrage) — OSI model, BGP, TCP/IP; persists through institutional inertia despite viability of alternatives (Piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent technological arrangements as laws of physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_trivial_topology, 0.52).
domain_priors:suppression_score(the_trivial_topology, 0.68).
domain_priors:theater_ratio(the_trivial_topology, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_trivial_topology, extractiveness, 0.52).
narrative_ontology:constraint_metric(the_trivial_topology, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(the_trivial_topology, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_trivial_topology, tangled_rope).
narrative_ontology:human_readable(the_trivial_topology, "Trivial Topology Information Asymmetry").
narrative_ontology:topic_domain(the_trivial_topology, "technological/network_architecture").

domain_priors:requires_active_enforcement(the_trivial_topology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_trivial_topology, central_information_brokers).
narrative_ontology:constraint_beneficiary(the_trivial_topology, network_topology_designers).
narrative_ontology:constraint_victim(the_trivial_topology, peripheral_nodes).
narrative_ontology:constraint_victim(the_trivial_topology, distributed_agents).
narrative_ontology:constraint_victim(the_trivial_topology, network_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL NODE (SNARE) — Nominally connected but information access requires passing through central brokers with no alternative routing. High suppression from processing bottlenecks and latency amplification. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.78. True connectivity masks information extraction.
constraint_indexing:constraint_classification(the_trivial_topology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISTRIBUTED AGENT NETWORK (TANGLED ROPE) — Benefits from network coordination (enables any-to-any communication in principle) but constrained by processing dependencies on central nodes. Suppression from latency costs and information filtering. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50. Mixed coordination and extraction.
constraint_indexing:constraint_classification(the_trivial_topology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL INFORMATION BROKER (ROPE) — Coordination beneficiary. Experiences the constraint as a legitimate coordination function: all nodes benefit from being reachable, and centralized routing is efficient. Information brokerage is a genuine service. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary with positive coordination externality.
constraint_indexing:constraint_classification(the_trivial_topology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DESIGN AUTHORITY (TANGLED ROPE) — Organized agents (standards bodies, infrastructure consortia) see the asymmetry as a solvable architectural problem. Benefit from designing efficient routing; constrained by legacy systems and installed base. Suppression from path dependencies in protocol deployment. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.28. Low-to-moderate extraction because the authority has agency to reshape topology.
constraint_indexing:constraint_classification(the_trivial_topology, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PROTOCOL STACK (PITON) — OSI model, BGP, TCP/IP were designed for different eras and threat models. Modern peer-to-peer and mesh protocols demonstrate that trivial topology (full connectivity) can be achieved with more symmetric information access. The legacy stack persists through institutional inertia: billions of deployed devices, routing tables, firewall rules. theater_ratio=0.58 reflects that much protocol governance is now ceremonial (RFCs, standards committees) rather than functionally constraining innovation. Alternative topologies are technically viable but institutionally suppressed.
constraint_indexing:constraint_classification(the_trivial_topology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From a universal/analytical perspective, some information latency is inherent to distributed systems: the speed of light, processing capacity, and the computational complexity of optimal routing are immutable constraints. This perspective risks naturalizing a contingent technological arrangement as a law of physics. However, base properties (ε=0.52, suppression=0.68) contradict a mountain classification — the constraint is substantially human-engineered, not naturally emergent.
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
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value through information brokerage — peripheral nodes must route requests through central brokers, creating a surcharge on information access. The 0.28→0.52 trajectory reflects increasing consolidation of network infrastructure and growing information asymmetry as networks have scaled. Base extraction would be lower (≈0.35) if alternative routing were freely available, but protocol defaults enforce centralized paths. Suppression (0.68): High. Peripheral nodes face multiple barriers: routing protocol defaults that favor central brokers, caching/latency amplification at edges, information filtering policies enforced at broker nodes, processing capacity bottlenecks, and path dependency from deployed infrastructure. Escape is nominally possible (P2P protocols exist) but suppressed by institutional inertia. Theater ratio (0.58): Moderate-high. Protocol governance (IETF standards, RFC processes, infrastructure management) occupies significant effort but increasingly decoupled from technical constraints. The rise of alternative topologies (Kademlia, DHTs, gossip algorithms) shows that the constraint is institutional, not physical. The ceremonial aspect has grown as protocol designers have proven that more symmetric topologies are viable — yet centralized brokers persist because they create extractive value for those who control them.
 *
 * PERSPECTIVAL GAP:
 *   Central brokers see coordination (Rope) — they are solving the legitimate problem of routing information at scale. Peripheral nodes see pure extraction (Snare) — they are trapped paying an access surcharge with no alternatives. The design authority sees a solvable problem (Tangled Rope or Scaffold) — P2P protocols prove that symmetric access is achievable with engineering effort. The legacy protocol stack sees degradation (Piton) — TCP/IP was designed for smaller, more trusted networks; modern infrastructure requirements exceed its symmetric coordination capacity, but it persists through installed base inertia. The analytical observer risks naturalizing the constraint as inevitable (Mountain) — information latency and routing complexity ARE real challenges — but the base metrics reveal that the specific asymmetry observed is substantially engineered, not emergent from physics. The perspectival gap is widest between the beneficiary (coordinator) and the victim (trapped peripheral), revealing the core extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Central information broker: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with positive coordination value. Peripheral nodes: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — no exit options. Distributed agents: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but with some escape capacity (P2P protocols reduce but don't eliminate reliance). Protocol design authority: Organized + constrained → d≈0.45, f(d)≈0.45. Low-to-moderate effective extraction; authority has agency to reshape topology through standards. Legacy protocol stack: Institutional + arbitrage → d≈0.05, f(d)≈-0.10. Piton classification emerges from theater ratio gate (0.58 ≥ 0.70 threshold not reached, but still elevated), not from directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification risks false naturalization — the constraint is more institutional than physical.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing between two separate constraints that were conflated in naive topology analysis: (1) INHERENT ROUTING COMPLEXITY: routing in distributed networks involves genuine tradeoffs between path optimality, computational cost, and scalability. This is a coordination problem (Rope) or a natural limit (Mountain edge case). (2) INSTITUTIONAL INFORMATION BROKERAGE: the specific asymmetry observed in centralized networks (peripheral nodes depending on central brokers) is a technological choice, enforced by protocol defaults and infrastructure control. This is a Tangled Rope (mixed coordination and extraction). The mandatrophy resolves by clarifying that not all information latency is extraction — some is genuine coordination cost — but the specific ASYMMETRY observed in real networks substantially exceeds what the routing complexity alone would require. P2P protocols demonstrate that symmetric access is achievable at comparable cost, revealing that the observed asymmetry is institutional suppression, not natural law. The constraint is primarily Tangled Rope with Snare elements (for trapped peripheral nodes) and Piton elements (legacy protocols degraded by institutional inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    broker_necessity_threshold,
    'What fraction of network transactions genuinely require central brokering versus what fraction is routed through brokers solely by protocol default rather than functional necessity?',
    'Protocol-level measurement: analyze actual transaction paths vs minimal-hop paths; identify cases where alternative routes exist but are not explored due to routing table design',
    'If <30% genuine necessity: asymmetry is primarily extractive (higher χ, snare classification strengthened). If >70% genuine necessity: asymmetry is primarily coordination (lower χ, rope classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(broker_necessity_threshold, empirical, 'Fraction of network traffic requiring central brokering versus protocol default routing').

omega_variable(
    peer_to_peer_viability,
    'Can peer-to-peer topologies (Kademlia, DHTs, gossip protocols) achieve information access parity with centralized brokers at comparable latency and energy cost?',
    'Comparative benchmarks: measure information discovery time, message overhead, and processing costs in P2P vs centralized systems under identical load conditions',
    'If P2P achieves parity: trivial topology asymmetry is choice/extraction (institutional perspective controls suppression). If P2P incurs >2x overhead: asymmetry reflects genuine scalability tradeoffs (coordination perspective stronger).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_to_peer_viability, empirical, 'Whether P2P protocols can match centralized broker performance').

omega_variable(
    information_filtering_intentionality,
    'To what extent is the information access asymmetry caused by intentional filtering/gatekeeping versus unintended emergent effects of protocol design?',
    'Protocol-level analysis: examine whether routing tables and caching strategies could be reconfigured to distribute access more symmetrically without changing the underlying topology; interview protocol designers regarding design intentions',
    'If intentional: snare classification strengthened (suppression is active policy). If emergent: tangled_rope classification strengthened (asymmetry is design artifact, not extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_filtering_intentionality, conceptual, 'Whether information asymmetry is intentional policy or emergent protocol effect').

omega_variable(
    computational_parity_cost,
    'What is the minimum computational overhead required to achieve symmetric information access in a fully connected network?',
    'Information-theoretic lower bounds on routing complexity; empirical measurement of processing costs for symmetric-access protocols (gossip algorithms, epidemic protocols) vs centralized brokers',
    'If symmetric access costs >3x as much as asymmetric: topology asymmetry reflects fundamental efficiency tradeoffs (mountain perspective gains credibility). If cost differential is <1.5x: asymmetry is institutional choice (snare perspective strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_parity_cost, empirical, 'Computational cost differential between symmetric and asymmetric information access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_trivial_topology, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trivtop_tr_t0, the_trivial_topology, theater_ratio, 0, 0.42).
narrative_ontology:measurement(trivtop_tr_t15, the_trivial_topology, theater_ratio, 15, 0.5).
narrative_ontology:measurement(trivtop_tr_t30, the_trivial_topology, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(trivtop_be_t0, the_trivial_topology, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trivtop_be_t15, the_trivial_topology, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(trivtop_be_t30, the_trivial_topology, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_trivial_topology, information_standard).
narrative_ontology:affects_constraint(the_trivial_topology, routing_concentration).
narrative_ontology:affects_constraint(the_trivial_topology, metadata_visibility_asymmetry).
narrative_ontology:affects_constraint(the_trivial_topology, latency_stratification).

% DUAL FORMULATION NOTE:
% Trivial topology information asymmetry is downstream of network architecture choices (IPv4/IPv6 addressing, BGP routing) and upstream of specific information brokerage mechanisms (DNS centralization, content delivery networks, ISP peering). The constraint represents the intersection of: (a) routing protocol design (technical), (b) infrastructure deployment economics (economic), and (c) information control incentives (political). Each formulation has different ε values. This story focuses on the technological/architectural dimension (ε=0.52). Sibling constraints in the family address economic incentives (higher ε for rent extraction) and political dimensions (control mechanisms, suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_trivial_topology, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
