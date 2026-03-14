% ============================================================================
% CONSTRAINT STORY: full_node_economic_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_full_node_economic_sustainability, []).

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
 *   constraint_id: full_node_economic_sustainability
 *   human_readable: Full Node Economic Sustainability in Blockchain Networks
 *   domain: blockchain/network_infrastructure/economic_incentives
 *
 * SUMMARY:
 *   The full node economic sustainability constraint describes a structural
 *   asymmetry in blockchain networks where the infrastructure required to
 *   maintain network security and decentralization (running a full validating
 *   node) has become economically unviable for independent operators. As
 *   networks mature and transaction volume increases, hardware requirements
 *   and bandwidth costs grow, while the economic rewards for node operation
 *   remain zero or negligible. This creates a Tangled Rope constraint
 *   combining genuine coordination necessity (network security requires
 *   distributed validation) with asymmetric extraction (hardware and
 *   bandwidth costs are concentrated on powerless independent operators,
 *   while economic benefits concentrate in mining pools, exchanges, and
 *   protocol developers). The constraint's extractiveness has increased from
 *   0.35 to 0.58 over the interval as network complexity has grown and mining
 *   has consolidated. The theater ratio (0.48) reflects that governance
 *   discussions and community deliberation around node incentives occupy
 *   substantial discourse time, yet structural economic mechanisms remain
 *   unchanged. The constraint demonstrates why a 'decentralized' protocol can
 *   still contain extractive structures when the economic incentives
 *   systematically disadvantage small participants.
 *
 * KEY AGENTS:
 *   - Independent Node Operators: Primary victim (powerless/trapped) — bear hardware and bandwidth costs; zero economic compensation; no exit option without abandoning network participation
 *   - Small Mining Operations: Secondary victim (moderate/constrained) — face economies-of-scale barriers forcing consolidation into mining pools; receive share of rewards but sacrificed autonomy
 *   - Mining Pool Operators: Primary beneficiary (institutional/arbitrage) — aggregate hashpower from many small miners; capture transaction fees and consolidate rewards; can exit by switching protocols
 *   - Exchange Operators: Secondary beneficiary (institutional/arbitrage) — control large validator stakes or significant hashpower; benefit from network activity; highly mobile
 *   - Protocol Developers/Foundations: Tertiary beneficiary (organized/constrained) — maintain protocol legitimacy; capture grants and foundation funding; constrained by need to preserve network security narrative
 *   - Network Decentralization Advocates: Organized agent (organized/constrained) — see structural problem; push for alternative incentive mechanisms; constrained by technical and game-theoretic barriers to implementation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as contingent design problem, not natural law; identifies extraction mechanism as solvable through architecture change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(full_node_economic_sustainability, 0.58).
domain_priors:suppression_score(full_node_economic_sustainability, 0.65).
domain_priors:theater_ratio(full_node_economic_sustainability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(full_node_economic_sustainability, extractiveness, 0.58).
narrative_ontology:constraint_metric(full_node_economic_sustainability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(full_node_economic_sustainability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(full_node_economic_sustainability, tangled_rope).
narrative_ontology:human_readable(full_node_economic_sustainability, "Full Node Economic Sustainability in Blockchain Networks").
narrative_ontology:topic_domain(full_node_economic_sustainability, "blockchain/network_infrastructure/economic_incentives").

domain_priors:requires_active_enforcement(full_node_economic_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(full_node_economic_sustainability, mining_pools).
narrative_ontology:constraint_beneficiary(full_node_economic_sustainability, exchange_operators).
narrative_ontology:constraint_beneficiary(full_node_economic_sustainability, protocol_developers).
narrative_ontology:constraint_victim(full_node_economic_sustainability, independent_node_operators).
narrative_ontology:constraint_victim(full_node_economic_sustainability, network_decentralization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT NODE OPERATOR (SNARE) — Trapped by hardware/bandwidth costs with no viable exit. Bears full extraction cost. Hardware depreciation, bandwidth expenses, and zero economic reward create an untenable position. Suppression is structural: no alternative coordination mechanisms exist that provide the same validation function at lower cost. The operator cannot exit without abandoning their commitment to decentralization.
constraint_indexing:constraint_classification(full_node_economic_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MINING OPERATION (TANGLED ROPE) — Constrained by economies of scale in hardware investment and pool-based coordination requirements. Faces mixed extraction and genuine coordination benefit (access to collective hash rate). High suppression due to difficulty adjustment and competitive barriers, but some coordination function exists through mining pools.
constraint_indexing:constraint_classification(full_node_economic_sustainability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MINING POOL OPERATORS (ROPE) — Institutional arbitrage position. Pool operators benefit from transaction fees and block rewards concentrated through their infrastructure. Experience the constraint as pure coordination: aggregating hashrate for smaller miners enables mutual benefit. Low effective extraction because the pool operator can arbitrage to other blockchain ecosystems.
constraint_indexing:constraint_classification(full_node_economic_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NETWORK DECENTRALIZATION ADVOCATES (TANGLED ROPE) — Organized agents (client developers, community foundations) see both genuine coordination (network security requires distributed validation) and asymmetric extraction (economic incentives concentrate power toward pools and exchanges). Constrained by the need to maintain protocol legitimacy while facing resistance to structural changes. The constraint's existence creates pressure to innovate alternative coordination mechanisms.
constraint_indexing:constraint_classification(full_node_economic_sustainability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL GOVERNANCE RITUAL (PITON) — From a civilizational timescale, the protocol governance process around node incentives is substantially performative. Community deliberation produces minimal actual changes to economic incentives; the core extraction structure persists through institutional inertia. Theater ratio reflects that governance discussions (forums, conferences, improvement proposals) occupy significant energy while the underlying economic mechanism remains unchanged. Governance theatre masks the structural constraints.
constraint_indexing:constraint_classification(full_node_economic_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal position, the constraint exhibits both genuine coordination necessity (distributed validation provides security guarantees) and irreducible asymmetric extraction (economic incentive structures concentrate power). The analytical perspective sees this as a structural design problem, not a law of nature. The constraint is contingent on current incentive mechanisms and could be restructured through protocol-level changes.
constraint_indexing:constraint_classification(full_node_economic_sustainability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(full_node_economic_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(full_node_economic_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(full_node_economic_sustainability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(full_node_economic_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(full_node_economic_sustainability, TR),
    TR >= 0.70.

:- end_tests(full_node_economic_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The economic extraction is not total (independent nodes still exist, some subsidized or activist-maintained) but is severe enough to prevent sustainable participation. The trajectory shows acceleration — extractiveness has risen from 0.35 to 0.58 as network complexity outpaced hardware cost decline. Suppression (0.65): High. Multiple suppression mechanisms exist: (1) technical barriers — specialized knowledge required to run a node, (2) economic barriers — hardware/bandwidth costs increasing faster than alternative solutions, (3) coordination barriers — no organized subsidy mechanisms exist at network level, (4) narrative barriers — network legitimacy claims depend on decentralization narrative despite observed centralization. Theater ratio (0.48): Moderate. Governance discussions (improvement proposals, foundation grant programs, community forums) create appearance of addressing the problem without changing fundamental economic structure. The theater has been stable or declining because some protocols (Ethereum's Lido, for example) have adopted explicit solutions rather than pure discourse.
 *
 * PERSPECTIVAL GAP:
 *   The independent node operator experiences a Snare (no exit, full cost, zero benefit). The mining pool operator experiences a Rope (coordination benefit, arbitrage mobility). The network decentralization advocate experiences a Tangled Rope (sees both genuine coordination necessity and asymmetric extraction). The analytical observer at civilizational scale also sees Tangled Rope, but frames it as a solvable design problem rather than a natural constraint. The perspectival gap reveals that the network legitimacy narrative (Rope: 'everyone can run a node') contradicts the economic reality (Snare: 'independent nodes are economically unviable'). This gap is the signature of false advertising — the protocol claims one structure (decentralized Rope) but delivers another (centralized Snare for independents).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position in the extraction flow and their exit capacity. Independent node operators have d ≈ 0.95 (trapped victims): they bear costs and receive zero economic benefit; their structural exit options are minimal (they can only abandon the network). Mining pools have d ≈ 0.15 (beneficiaries with arbitrage): they concentrate rewards and can exit by shifting to other protocols. Protocol developers have d ≈ 0.25-0.35 (beneficiaries with constrained mobility): they benefit from network legitimacy but depend on maintaining the network's viability. The decentralization advocates have d ≈ 0.65 (mixed victims and advocates): they bear reputation costs when centralization increases but have organizational mobility. The analytical perspective has d ≈ 0.72 (observer position): high because the constraint blocks systemic analysis — the ability to see and fix the problem is itself constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy (false natural law detection) by recognizing that the extraction is structural but not immutable. The Tangled Rope classification acknowledges genuine coordination necessity (distributed validation is required for network security) without naturalizing the economic asymmetry as inevitable. Multiple alternative architectures exist (PoS with different staking models, proof-of-authority variants, directed subsidies) that could provide coordination without extraction. The constraint's extractiveness is high (0.58) because the current mechanism forces a choice between network participation (costly) and economic viability (impossible for independents). However, this is a contingent design outcome, not a natural law. The moonality is resolvable through protocol-level changes: introducing block reward redistribution to node operators, implementing validator incentive mechanisms, or migrating to consensus systems with lower hardware requirements. The false mountain detector correctly identifies that treating this as an immutable constraint misses the design flexibility that exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hardware_cost_trajectory,
    'Will declining hardware costs and increased bandwidth availability eventually make independent node operation economically viable without subsidies or new revenue mechanisms?',
    'Long-term tracking of hardware price trends, bandwidth commodity costs, and node operation expense baseline; comparison with network security requirements trajectory',
    'If costs decline sufficiently: constraint could shift from Snare to Tangled Rope or Scaffold (temporary problem). If costs remain high or grow with security requirements: structural extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_cost_trajectory, empirical, 'Whether hardware/bandwidth cost trends will enable economic sustainability').

omega_variable(
    proof_of_stake_extraction_mechanism,
    'Do proof-of-stake consensus mechanisms genuinely eliminate the economic extraction of full node operators, or do they substitute one form of extraction (hardware capital) for another (stake concentration)?',
    'Empirical analysis of PoS network validator distributions, capital concentration metrics, and economic barriers to independent staking; comparison of extractiveness values across consensus mechanisms',
    'If PoS solves the problem: extracted extraction mechanism disappears in upgraded networks. If PoS substitutes extraction: constraint persists with different structural form (stake concentration vs mining pool concentration).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_of_stake_extraction_mechanism, empirical, 'Whether PoS mechanisms solve or transform the extraction structure').

omega_variable(
    public_goods_subsidy_feasibility,
    'Can network-level or protocol-level subsidies for independent nodes (through block rewards, fee allocation, or directed incentives) overcome the economic sustainability gap without creating new extraction vectors or reducing network security?',
    'Comparative analysis of subsidy mechanisms (miner-fund models, foundation-supported node programs, fee redistribution schemes); measurement of adoption rates and network decentralization outcomes; assessment of incentive compatibility',
    'If feasible: constraint shifts to Scaffold with sunset logic (temporary problem with policy solution). If infeasible: extraction remains structural, only solution paths involve architectural redesign or accepting centralization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_goods_subsidy_feasibility, empirical, 'Whether public goods subsidies can provide sustainable economic models').

omega_variable(
    decentralization_coordination_problem,
    'Is the observed centralization trend (toward mining pools, exchanges, validator operators) a necessary consequence of economic incentive structures, or a coordination failure that could be reversed through alternative governance mechanisms?',
    'Historical analysis of network structure changes; comparison across multiple blockchain networks with different incentive designs; counterfactual modeling of alternative incentive structures',
    'If necessary consequence: extraction is structural and fundamental to blockchain economics. If coordination failure: problem is solvable through protocol redesign without sacrificing security or decentralization guarantees.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_coordination_problem, conceptual, 'Whether centralization is economically necessary or a solvable coordination problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(full_node_economic_sustainability, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fullnode_tr_t0, full_node_economic_sustainability, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fullnode_tr_t4, full_node_economic_sustainability, theater_ratio, 4, 0.4).
narrative_ontology:measurement(fullnode_tr_t8, full_node_economic_sustainability, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(fullnode_be_t0, full_node_economic_sustainability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fullnode_be_t4, full_node_economic_sustainability, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(fullnode_be_t8, full_node_economic_sustainability, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(full_node_economic_sustainability, resource_allocation).
narrative_ontology:affects_constraint(full_node_economic_sustainability, blockchain_51_percent_attack_vulnerability).
narrative_ontology:affects_constraint(full_node_economic_sustainability, mining_pool_consolidation).
narrative_ontology:affects_constraint(full_node_economic_sustainability, validator_stake_concentration).

% DUAL FORMULATION NOTE:
% Full node economic sustainability decomposition: (1) hardware_cost_constraint (ε≈0.25, Rope) — technical requirement for validation; genuinely coordination function with low extractiveness; (2) incentive_asymmetry (ε≈0.58, Tangled Rope) — economic structure concentrating rewards; genuine coordination function (need for nodes) plus asymmetric extraction (zero rewards for independents); (3) mining_pool_dominance (ε≈0.72, Snare) — structural consolidation where independent mining becomes unviable; mostly extraction, minimal coordination. These are linked: the hardware requirement creates economies of scale (constraint 1 → 3), which makes independent operation uneconomical (constraint 2 → 3). Each story has different ε values reflecting different observables. The upstream constraint (hardware requirement) is nearly inevitable; the downstream constraints (economic incentive distribution) are contingent design choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(full_node_economic_sustainability, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
