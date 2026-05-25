% ============================================================================
% CONSTRAINT STORY: quorum_consensus_protocols
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quorum_consensus_protocols, []).

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
 *   constraint_id: quorum_consensus_protocols
 *   human_readable: Quorum Consensus Protocols in Distributed Systems
 *   domain: computer_science/distributed_systems
 *
 * SUMMARY:
 *   Quorum consensus protocols are coordination mechanisms that enable
 *   distributed systems to reach agreement despite faults or malicious
 *   participants. A quorum is a threshold number of participants whose
 *   agreement is necessary and sufficient for consensus — typically more than
 *   half (majority) or more than two-thirds (supermajority) in Byzantine
 *   fault tolerant systems. The constraint exhibits Tangled Rope structure:
 *   protocols solve a genuine coordination problem (Byzantine agreement in
 *   distributed systems) while simultaneously creating asymmetric extraction:
 *   minority partitions below the quorum threshold lose all voice,
 *   latency-constrained nodes are systematically disadvantaged, and protocol
 *   designers accumulate gatekeeping authority over consensus rules. The
 *   extractiveness value (0.38) reflects moderate asymmetry — the
 *   coordination function is real and necessary, but the extractive overhead
 *   has grown as systems have decentralized and as economic incentives have
 *   made formal BFT guarantees partially redundant. Theater ratio (0.35) is
 *   relatively low because the coordination logic is transparent and
 *   functional, but it is increasing as systems maintain Byzantine fault
 *   tolerance theorems as institutional inertia even when economic mechanisms
 *   could provide equivalent security at lower quorum cost.
 *
 * KEY AGENTS:
 *   - Minority Partition Nodes: Primary victims (powerless/trapped) — cannot participate below quorum threshold; bear full cost of exclusion with no exit option
 *   - Latency-Constrained Participants: Secondary victims (moderate/constrained) — experience slower confirmation times and quorum inclusion risk; face significant infrastructure migration costs
 *   - System Operators: Primary beneficiaries (institutional/arbitrage) — can select protocol variants; benefit from clear consensus rules and fault tolerance guarantees
 *   - Protocol Designers: Secondary beneficiaries (institutional/arbitrage) — accumulate authority over quorum threshold rules; can adjust extraction through parameter changes
 *   - Blockchain Community: Organized agents (organized/constrained) — see quorum requirements as temporary mechanism with sunset clause as economic incentives mature
 *   - Formal BFT Tradition: Institutional inertia (institutional/arbitrage) — maintains Byzantine fault tolerance dogma as performative ritual despite economic incentives providing equivalent security
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent quorum choices as inherent limits of distributed consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quorum_consensus_protocols, 0.38).
domain_priors:suppression_score(quorum_consensus_protocols, 0.42).
domain_priors:theater_ratio(quorum_consensus_protocols, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quorum_consensus_protocols, extractiveness, 0.38).
narrative_ontology:constraint_metric(quorum_consensus_protocols, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quorum_consensus_protocols, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quorum_consensus_protocols, tangled_rope).
narrative_ontology:human_readable(quorum_consensus_protocols, "Quorum Consensus Protocols in Distributed Systems").
narrative_ontology:topic_domain(quorum_consensus_protocols, "computer_science/distributed_systems").

domain_priors:requires_active_enforcement(quorum_consensus_protocols).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quorum_consensus_protocols, protocol_designers).
narrative_ontology:constraint_beneficiary(quorum_consensus_protocols, system_operators).
narrative_ontology:constraint_victim(quorum_consensus_protocols, minority_participants).
narrative_ontology:constraint_victim(quorum_consensus_protocols, latency_constrained_nodes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY PARTITION NODE (SNARE) — A node in a network partition below quorum threshold cannot participate in consensus, cannot propose transactions, and cannot update state. It is structurally trapped: exit requires rejoining the network topology, which is not under the node's control. The node bears full cost of unavailability while the majority partition extracts consensus authority. No coordination benefit accrues to this agent.
constraint_indexing:constraint_classification(quorum_consensus_protocols, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATENCY-CONSTRAINED PARTICIPANT (TANGLED ROPE) — Nodes in high-latency regions must accept message delays and potential exclusion from fast quorum rounds. They experience genuine coordination benefit (agreement is reached, state is consistent) alongside asymmetric extraction: their voting power is discounted by round-trip latency, and they bear the cost of slower confirmation times. Constrained because they can migrate infrastructure but face significant capital and operational costs.
constraint_indexing:constraint_classification(quorum_consensus_protocols, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM OPERATOR (ROPE) — Benefits from quorum protocols: they solve the genuine Byzantine fault tolerance coordination problem, enable distributed state consistency, and provide clear governance rules. The operator can arbitrage across protocol variants (BFT, Raft, Paxos) based on application requirements. Experiences the protocol as pure coordination with minimal extraction overhead.
constraint_indexing:constraint_classification(quorum_consensus_protocols, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BLOCKCHAIN COMMUNITY (SCAFFOLD) — Organized agents in decentralized systems (consensus protocol researchers, node operators, governance DAOs) see quorum requirements as a temporary coordination mechanism with a sunset clause: Byzantine fault tolerance is necessary during the transition to economically-incentivized consensus (Proof-of-Stake) and distributed validator sets. As economic incentives mature and validator pools decentralize, the extractive overhead of quorum-based gatekeeping declines. Sunset: 5-10 years as staking mechanisms mature and threshold signatures enable lower-quorum consensus.
constraint_indexing:constraint_classification(quorum_consensus_protocols, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BYZANTINE FAULT TOLERANCE DOGMA (PITON) — The theoretical requirement for (n/3)+1 quorum thresholds to guarantee Byzantine fault tolerance under synchrony assumptions has become partially performative in modern systems. Many real deployments operate with weaker quorum assumptions (supermajority rather than strict BFT threshold) and rely on reputation/collateral rather than protocol-level Byzantine guarantees. The formal BFT requirement persists as institutional inertia despite being relaxed in practice — maintained for theoretical purity rather than empirical necessity. Theater ratio is moderate (0.35) because the coordination function is still real, but the gap between BFT guarantee and actual operational security is growing.
constraint_indexing:constraint_classification(quorum_consensus_protocols, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE TOKEN HOLDER (TANGLED ROPE) — In delegated quorum systems (staking-based consensus), large token holders or delegated validators have voting power concentrated in proportion to stake. They experience coordination benefit (protocol security, finality guarantees) alongside asymmetric extraction: the quorum requirement creates a threshold effect where validators below ~30% stake have minimal influence regardless of participation. Constrained because exit requires unstaking (capital lockup, opportunity cost) but exit is possible at a price.
constraint_indexing:constraint_classification(quorum_consensus_protocols, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some quorum requirement is inherent to Byzantine fault tolerance: you cannot reach consensus with fewer than 2f+1 participants in a system with f faulty nodes (FLP impossibility result). This perspective sees quorum thresholds as immutable laws of distributed computing. However, the structural data contradicts the mountain classification — modern systems achieve lower effective quorum through economic incentives and reputation — revealing that the 'inherent to Byzantine fault tolerance' framing naturalizes what is actually a contingent choice about which fault model to defend against.
constraint_indexing:constraint_classification(quorum_consensus_protocols, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quorum_consensus_protocols_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quorum_consensus_protocols, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quorum_consensus_protocols, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quorum_consensus_protocols, TR),
    TR >= 0.70.

:- end_tests(quorum_consensus_protocols_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The quorum requirement creates genuine asymmetry: nodes below the threshold have zero influence, while nodes at or above the threshold have decision authority. However, extractiveness is not maximal because operators can adjust quorum parameters based on application needs, and the coordination benefit is substantial. The value reflects that the extraction is real but bounded by the genuine coordination function. Suppression (0.42): Moderate. Suppression mechanisms include: protocol-enforced exclusion (high), latency-based discrimination (moderate), capital lockup in stake-based systems (moderate), and network partition failures (high). But suppression is not total — nodes can upgrade infrastructure, delegate stake, or migrate to lower-quorum systems. Theater ratio (0.35): Low-moderate. The coordination logic is transparent: consensus algorithms are well-understood, proofs are formal, and implementations are auditable. But theater is increasing as systems maintain Byzantine fault tolerance theorems despite economic mechanisms relaxing their necessity. The theater growth reflects Goodhart drift: the formal guarantee ('Byzantine fault tolerance') is becoming a goal in itself rather than a means to system safety.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural coordination mechanism appears as pure extraction (Snare) to minority nodes, mixed coordination-extraction (Tangled Rope) to latency-constrained participants, pure coordination (Rope) to benefiting operators, and as a temporary problem with a sunset (Scaffold) to organized blockchain communities. The analytical observer risks classifying the quorum requirement as immutable (Mountain) based on FLP impossibility results, but this naturalizes the choice of which fault model to defend against — choosing to defend against Byzantine faults rather than accepting a weaker consistency model. The Piton perspective reveals that formal BFT requirements are increasingly performative: many deployed systems achieve equivalent security through economic incentives at lower quorum cost, but maintain the Byzantine fault tolerance dogma as institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) differ across agents based on their power level, exit options, and beneficiary/victim status. Minority partition nodes are full victims (d ≈ 0.95) with no exit — maximum experienced extraction. Latency-constrained participants are partial victims (d ≈ 0.65) with exit possible at significant cost. System operators are beneficiaries (d ≈ 0.15) with full arbitrage across protocol variants. Protocol designers are beneficiaries (d ≈ 0.10) with lowest experienced extraction due to institutional power to adjust rules. Blockchain community members are organized victims (d ≈ 0.45) with exit paths through protocol upgrades and stake delegation. The analytical observer derives d from their structural position relative to the constraint (d ≈ 0.72) — they can document the structure but cannot directly influence protocol parameters.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that 'quorum consensus' labels a family of structurally distinct mechanisms: (1) protocol-enforced Byzantine fault tolerance (Mountain/Piton elements), (2) coordination for distributed state consistency (Rope elements), (3) extraction through latency-based discrimination and minority exclusion (Snare elements), and (4) temporary governance mechanism during transition to economic incentives (Scaffold elements). No single type captures all perspectives. The mandatrophy resolution shows that the extractiveness value itself is the key finding: extractiveness (0.38) confirms that moderate asymmetric extraction exists within a genuine coordination function. The canonical classifier's Tangled Rope gate requires beneficiaries + victims + active enforcement — all three are present. The extraction is real but bounded by the coordination necessity, making this a textbook Tangled Rope rather than a mislabeled Snare or false Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synchrony_assumption_validity,
    'Do modern network conditions satisfy the synchrony assumptions required for formal BFT quorum correctness, or has network heterogeneity invalidated these assumptions?',
    'Empirical measurement of message delivery times across geographic regions; statistical analysis of latency distributions; comparison against synchrony bounds in BFT proofs',
    'If assumptions hold: quorum thresholds are necessary (mountain elements valid). If violated: quorum thresholds are contingent governance choices (extraction elements increase, classification shifts toward Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synchrony_assumption_validity, empirical, 'Whether synchrony assumptions in BFT proofs hold in practice').

omega_variable(
    economic_incentive_sufficiency,
    'Do economic incentives (slashing, staking rewards) in modern consensus systems provide equivalent Byzantine fault tolerance to protocol-enforced quorum thresholds?',
    'Historical analysis of attacks on systems with economic incentives vs protocol-enforced quorum; measurement of actual slashing frequency and deterrence effect; modeling of rational adversary incentives',
    'If sufficient: quorum thresholds can be relaxed without losing security (Scaffold sunset accelerates). If insufficient: formal quorum remains necessary (Piton persistence confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_incentive_sufficiency, empirical, 'Whether economic incentives replace protocol-enforced quorum').

omega_variable(
    minority_partition_necessity,
    'Does the exclusion of minority partitions below quorum threshold serve a necessary safety function, or is it extractive gatekeeping justified only by BFT formalism?',
    'Comparison of safety violations in systems that enforce strict quorum vs systems with relaxed thresholds; analysis of actual partition scenarios in deployed networks; cost-benefit analysis of minority exclusion vs data inconsistency risk',
    'If necessary: minority exclusion is coordination cost (mountain/rope elements valid). If extractive: minority nodes are victims of protocol design choice (Snare elements confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_partition_necessity, empirical, 'Whether minority partition exclusion serves necessary safety function').

omega_variable(
    latency_discrimination_mechanism,
    'Does latency-based quorum slowness represent inherent physics/network properties or extractive protocol design that privileges low-latency nodes?',
    'Analysis of latency impact on quorum reachability; comparison across protocol designs (synchronous vs asynchronous consensus); measurement of quorum size vs confirmation time trade-offs in deployed systems',
    'If inherent: latency constraints are mountain elements (physics-imposed). If protocol design: constraints are Tangled Rope elements (extractive structure disclosed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latency_discrimination_mechanism, empirical, 'Whether latency discrimination is inherent or protocol-design extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quorum_consensus_protocols, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quorum_tr_t0, quorum_consensus_protocols, theater_ratio, 0, 0.2).
narrative_ontology:measurement(quorum_tr_t3, quorum_consensus_protocols, theater_ratio, 3, 0.28).
narrative_ontology:measurement(quorum_tr_t6, quorum_consensus_protocols, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(quorum_be_t0, quorum_consensus_protocols, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(quorum_be_t3, quorum_consensus_protocols, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(quorum_be_t6, quorum_consensus_protocols, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quorum_consensus_protocols, enforcement_mechanism).
narrative_ontology:affects_constraint(quorum_consensus_protocols, byzantine_fault_tolerance_limits).
narrative_ontology:affects_constraint(quorum_consensus_protocols, network_partition_tolerance).
narrative_ontology:affects_constraint(quorum_consensus_protocols, latency_sensitive_consensus).

% DUAL FORMULATION NOTE:
% Quorum consensus protocols decompose into three structurally distinct constraints: (1) Byzantine Fault Tolerance Lower Bounds (ε=0.08, Mountain) — FLP impossibility and protocol correctness proofs; (2) Quorum-Based Consensus Coordination (ε=0.28, Rope) — the genuine distributed agreement function; (3) Minority Exclusion Extraction (ε=0.62, Snare) — the asymmetric gatekeeping. This story focuses on the hybrid (Tangled Rope) view. Upstream constraints are the mathematical limits; downstream constraints are the extraction mechanisms that quorum gatekeeping enables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quorum_consensus_protocols, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
