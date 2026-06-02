% ============================================================================
% CONSTRAINT STORY: ergo_utxo_state_bloat_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_utxo_state_bloat_accumulation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_utxo_state_bloat_accumulation
 *   human_readable: Ergo UTXO State Bloat Accumulation
 *   domain: blockchain/systems_scalability
 *
 * SUMMARY:
 *   The Ergo UTXO state bloat accumulation constraint arises from a
 *   fundamental design choice: the protocol guarantees that all historical
 *   outputs remain valid and accessible indefinitely, with no pruning or
 *   state rent mechanism to retire inactive outputs. As transaction volume
 *   accumulates, the UTXO set grows monotonically, imposing increasing
 *   storage and verification costs on full node operators. This constraint
 *   exhibits the characteristic dual nature of coordination-extraction
 *   hybrids: the immutability guarantee genuinely coordinates around
 *   confidence in output permanence (enabling long-term holding and reducing
 *   counterparty risk), but it simultaneously extracts from node operators by
 *   concentrating storage and latency burdens without incentive compensation.
 *   The accumulation is neither a natural law nor a simple coordination
 *   problem—it is a contingent protocol design that trades node operator
 *   burden for early-holder confidence. The constraint's theater_ratio (0.48)
 *   reflects that some aspects are performative (the increasing need for
 *   distributed node validation becomes theater as users migrate to light
 *   clients) but core coordination remains functional. The extractiveness
 *   trajectory (0.35 → 0.58) shows accumulation acceleration as transaction
 *   density increases.
 *
 * KEY AGENTS:
 *   - Full Node Operators: Primary victims (powerless/trapped) — bear monotonic increase in storage and verification costs with no exit mechanism or incentive compensation
 *   - Early UTXO Holders: Primary beneficiaries (institutional/arbitrage) — benefit from immutable permanence guarantee that protects holdings indefinitely and creates confidence premium
 *   - Blockchain Scalability Infrastructure: Secondary victim (moderate/constrained) — system performance degrades as UTXO verification latency increases; can partially exit via light client delegation at cost
 *   - State Rent Protocol Designers: Organized technical communities (organized/constrained) — developing sunset mechanisms (state rent, UTxO consolidation incentives) with medium-term deployment horizon
 *   - Light Client Developers: Secondary beneficiary (institutional/arbitrage) — benefit as full node bloat drives users toward light client solutions they develop and maintain
 *   - Ergo Protocol Governance: Institutional actor (institutional/arbitrage) — maintains immutability guarantee through inertia; governs decision to implement or reject state rent solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_utxo_state_bloat_accumulation, 0.58).
domain_priors:suppression_score(ergo_utxo_state_bloat_accumulation, 0.65).
domain_priors:theater_ratio(ergo_utxo_state_bloat_accumulation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_utxo_state_bloat_accumulation, tangled_rope).
narrative_ontology:human_readable(ergo_utxo_state_bloat_accumulation, "Ergo UTXO State Bloat Accumulation").
narrative_ontology:topic_domain(ergo_utxo_state_bloat_accumulation, "blockchain/systems_scalability").

domain_priors:requires_active_enforcement(ergo_utxo_state_bloat_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_utxo_state_bloat_accumulation, early_utxo_holders).
narrative_ontology:constraint_beneficiary(ergo_utxo_state_bloat_accumulation, light_client_developers).
narrative_ontology:constraint_victim(ergo_utxo_state_bloat_accumulation, full_node_operators).
narrative_ontology:constraint_victim(ergo_utxo_state_bloat_accumulation, blockchain_scalability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FULL NODE OPERATOR (SNARE) — Trapped in the accumulation dynamic. Running a full node requires storing the entire UTXO set, which grows monotonically as new outputs accumulate. Storage costs rise without relief; operators cannot exit without abandoning node participation. Suppression is structural: no incentive mechanism rewards UTXO set pruning, and the protocol enforces that all historical outputs remain valid. Maximum extraction — the constraint transfers storage and compute costs to node operators while benefiting early-stage protocol participants.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BLOCKCHAIN SCALABILITY INFRASTRUCTURE (TANGLED ROPE) — Experiences both coordination and extraction. The UTXO accumulation enables genuine coordination: outputs represent valid value transfers and provide proof of stake attestation. But extraction is embedded: as UTXO set grows, verification latency increases, transaction throughput decreases, and light client synchronization becomes costlier. The constraint solves a real problem (maintaining outputs for verification) while degrading system capacity. Exit is costly: redesigning output retention would require protocol hard fork.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY UTXO HOLDERS AND PROTOCOL DESIGNERS (ROPE) — Experience the constraint as pure coordination. The guarantee that all outputs remain accessible indefinitely creates strong incentives for early participation and long-term holding. Protocol designers benefit from lower complexity requirements (no pruning logic, no state rent mechanisms). This perspective sees the accumulation as a feature enabling confidence in output permanence. Extraction runs toward these agents — they benefit from the asymmetric storage burden placed on operators.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE RENT AND PRUNING PROTOCOL DESIGNERS (SCAFFOLD) — Organized technical communities working on solutions (state rent mechanisms, UTxO consolidation incentives, tiered storage) view the bloat accumulation as a temporary coordination failure with an exit path. Light client protocols, compression techniques, and economic incentives for output consolidation represent the sunset clause. However, implementation requires either soft fork coordination (constrained exit) or hard fork (high cost). The scaffold perspective assumes these solutions mature within a generational timeline — optimistic but structural.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY FULL NODE INFRASTRUCTURE (PITON) — The maintained but degrading institutional role. Full node operation is conceptually required for network security (decentralization, censorship resistance) but increasingly performative in practice as most users rely on light clients and hosted nodes. The infrastructure persists through institutional commitment to decentralization values, but the actual verification role has atrophied. Theater ratio is moderate here — the infrastructure remains functional but not optimal. Piton classification derives from degraded primary function masked by ideological maintenance.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICS-OF-ACCUMULATION VIEW (MOUNTAIN) — From a universal/civilizational perspective, monotonic accumulation of immutable historical records is a fundamental constraint of any append-only ledger. The UTXO set accumulation appears as an irreducible law: you cannot simultaneously guarantee output permanence and avoid state growth. However, the structural data contradicts this naturalization — the constraint is contingent on protocol design choices (immutability guarantee, full node archival requirement, lack of state rent mechanisms). The mountain classification is a false summit revealing how technical contingencies are naturalized as laws of decentralized systems.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_utxo_state_bloat_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_utxo_state_bloat_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_utxo_state_bloat_accumulation, TR),
    TR >= 0.70.

:- end_tests(ergo_utxo_state_bloat_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The UTXO accumulation transfers real resource costs (storage, bandwidth, CPU for verification) to node operators without compensation mechanism or exit option. This is structural extraction. However, it is not maximal (0.72+) because: (1) early UTXO holders' benefit is legitimate coordination incentive (not pure rent), (2) node operators retain the technical capability to exit via light client delegation (constrained but possible), and (3) no single entity controls the accumulation—it is distributed across all protocol participants. Suppression (0.65): Moderate-high. Barriers to escaping the accumulation include: technical complexity of running light clients, network effects favoring full node security arguments, lack of alternative accumulation-control mechanisms, and protocol immutability making state rent retroactively difficult. However, suppression is not total because light client alternatives exist and state rent solutions are technically feasible. Theater ratio (0.48): Moderate. The increasing need for distributed full node validation becomes partially performative as users migrate to light clients and hosted nodes, yet core output verification remains functional. The gap between 'we need many full nodes for decentralization' and 'most users don't run nodes' is where theater accumulates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single accumulation mechanism produces fundamentally different classifications across perspectives. The full node operator sees pure extraction (Snare): their costs rise without relief and they cannot exit without abandoning participation. The early UTXO holder sees coordination (Rope): the immutability guarantee solves their problem (protecting holdings indefinitely) and the operator burden is not their responsibility. The blockchain infrastructure sees hybrid coordination-extraction (Tangled Rope): the mechanism both enables verification and degrades capacity. The state rent designers see a temporary problem with a sunset (Scaffold): state rent mechanisms can emerge within a generational timeframe. The full node infrastructure sees its own degraded role (Piton): the validation function persists through ideological commitment to decentralization even as users migrate to light clients. The civilizational analytical observer risks seeing an immutable law (Mountain) — 'you cannot guarantee permanence without accumulation'—but this is a false summit naturalizing contingent design choices. The perspectival gap reveals that the constraint is not 'the physics of accumulation' but 'the institutional choice to prioritize early-holder confidence over operator burden distribution.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: Full node operators occupy the target position (d ≈ 0.90) — they bear costs and face trapped exit, producing high f(d) ≈ 1.38 and high experienced extraction. Early UTXO holders occupy the beneficiary position (d ≈ 0.05) — they receive extraction benefits and have arbitrage exit, producing low f(d) ≈ -0.12 and negative experienced extraction (the constraint subsidizes them). Blockchain scalability infrastructure occupies a mixed position (d ≈ 0.55) — it both enables coordination and experiences extraction, producing moderate f(d) ≈ 0.75 and moderate chi. The scaffold perspective (organized/constrained) has derived d ≈ 0.40, reflecting that organized agents have some agency through protocol change proposals but remain constrained by coordination requirements. No directionality overrides are required—the structural relationships are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the six-type spectrum is not a classification problem but a governance problem. The mountain perspective (universal/civilizational) naturalizes what is actually a protocol design choice. The rope perspective (beneficiary) captures the genuine coordination function. The snare perspective (operator) captures the genuine extraction burden. The tangled rope perspective (infrastructure) is the structural reality—the constraint BOTH coordinates output verification AND extracts from operators. The scaffold perspective (technical designers) is the forward-looking solution. The piton perspective (legacy institution) is the institutional reality of degradation masked by ideological maintenance. No single type is 'correct'—the constraint's classification is a presheaf over observational positions. The mandatrophy is resolved by accepting that different institutional actors experience legitimately different classifications, and that the 'true' constraint is the tangled rope (hybrid coordination-extraction), with a genuine sunset path via state rent mechanisms that could transition it toward pure coordination (rope) if implemented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_rent_feasibility,
    'Can state rent mechanisms (charging fees to keep outputs in active state) be implemented without creating extractive barriers to asset ownership?',
    'Analysis of proposed state rent designs; simulation of rent costs relative to typical output value; survey of user experience in systems with rent (e.g., Ethereum 2.0 EIP-4488 proposals); measurement of adoption rates if implemented',
    'If feasible with low friction: constraint converts from snare to tangled_rope with sunset path. If rent becomes extractive: replaces one extraction mechanism with another, shifting rather than resolving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_rent_feasibility, empirical, 'Whether state rent can resolve bloat without creating secondary extraction').

omega_variable(
    light_client_sufficiency,
    'Do light client protocols (SPV, SNARKs, rollup-based verification) provide equivalent security guarantees to full node verification for preventing double-spending and consensus attacks?',
    'Comparative security analysis of light vs full node verification; historical analysis of attacks prevented by full node validation that light clients would have missed; game-theoretic analysis of incentives for honest light client operators',
    'If light clients are sufficient: full node archival is not a consensus requirement, and the UTXO set bloat becomes a coordination choice rather than immutable constraint. Transitions mountain to rope/tangled_rope. If light clients have fundamental gaps: full node bloat is unavoidable, supporting mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(light_client_sufficiency, empirical, 'Whether light clients provide equivalent security to full node verification').

omega_variable(
    accumulation_rate_sustainability,
    'At current transaction volume and density, what is the timeline before UTXO set storage exceeds affordable hardware for individual node operators?',
    'Projection of UTXO set growth based on historical transaction rates; comparison to consumer-grade storage hardware cost curves; measurement of minimum viable full node operator equipment costs over time',
    'If timeline > 50 years: constraint is manageable within generational horizon, supporting scaffold/piton perspectives. If timeline < 10 years: critical inflection point approaching, supporting snare/tangled_rope severity assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accumulation_rate_sustainability, empirical, 'Timeline to UTXO set storage exceeding affordable hardware').

omega_variable(
    protocol_coordination_capacity,
    'Can the Ergo protocol community achieve consensus on hard fork governance and execute it without contentious chain split?',
    'Historical analysis of prior Ergo governance decisions; assessment of developer community alignment on scaling roadmap; monitoring of hashpower distribution and miner signaling on protocol changes',
    'If high coordination capacity: scaffold sunset path is real (state rent mechanisms can be deployed). If low capacity: coordination failure locks in the constraint, supporting piton/snare perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_coordination_capacity, empirical, 'Whether Ergo protocol community can coordinate on hard fork solutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_utxo_state_bloat_accumulation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergobloa_tr_t0, ergo_utxo_state_bloat_accumulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ergobloa_tr_t3, ergo_utxo_state_bloat_accumulation, theater_ratio, 3, 0.41).
narrative_ontology:measurement(ergobloa_tr_t6, ergo_utxo_state_bloat_accumulation, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(ergobloa_be_t0, ergo_utxo_state_bloat_accumulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ergobloa_be_t3, ergo_utxo_state_bloat_accumulation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ergobloa_be_t6, ergo_utxo_state_bloat_accumulation, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_utxo_state_bloat_accumulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ergo_utxo_state_bloat_accumulation, 0.18).
narrative_ontology:affects_constraint(ergo_utxo_state_bloat_accumulation, ergo_light_client_security_assumptions).
narrative_ontology:affects_constraint(ergo_utxo_state_bloat_accumulation, ergo_network_decentralization_requirement).

% DUAL FORMULATION NOTE:
% The UTXO state bloat accumulation decomposes into two structurally distinct constraints: (1) output_permanence_coordination (ε ≈ 0.15, Rope) — the genuine coordination mechanism guaranteeing immutability, upstream and foundational; (2) ergo_utxo_state_bloat_accumulation (ε ≈ 0.58, Tangled Rope) — the extraction of storage/verification costs from node operators downstream of the permanence guarantee. The story focuses on the accumulated bloat constraint; the permanence coordination is the upstream constraint in the family. Both stories required for full decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
