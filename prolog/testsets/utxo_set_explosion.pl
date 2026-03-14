% ============================================================================
% CONSTRAINT STORY: utxo_set_explosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utxo_set_explosion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: utxo_set_explosion
 *   human_readable: UTXO Set Explosion in Bitcoin and Blockchain Systems
 *   domain: cryptocurrency/blockchain_scalability
 *
 * SUMMARY:
 *   The UTXO (Unspent Transaction Output) set explosion in Bitcoin and
 *   related blockchain systems creates a structural tension between
 *   decentralization (all nodes maintaining full state) and scalability
 *   (managing ever-growing transaction history). As transaction volume
 *   increases and users create many small outputs, the UTXO set — the set of
 *   all unspent outputs — grows without bound, increasing memory, I/O, and
 *   computational requirements for full node operation. This constraint
 *   exhibits classic Tangled Rope characteristics: genuine coordination
 *   function (decentralized consensus verification) combined with asymmetric
 *   extraction (those who can afford full nodes capture value from those who
 *   cannot). The constraint is neither pure extraction (snare) nor pure
 *   coordination (rope) — it is a hybrid mechanism where coordination
 *   requirements are leveraged to enforce an extractive barrier to entry.
 *
 * KEY AGENTS:
 *   - Solo Miners: Powerless/trapped (institutional/arbitrage, but trapped exit at blockchain level) — cannot economically maintain UTXO index; forced into pool membership or custodial exchanges
 *   - Light Client Users: Moderate/constrained (moderate/constrained) — can participate but must trust external infrastructure; face bandwidth and synchronization delays
 *   - Exchange Operators: Institutional/arbitrage (institutional/arbitrage) — benefit from full-node requirements; operate efficient infrastructure; control custody
 *   - Mining Pool Operators: Organized/constrained (organized/constrained) — provide coordination (variance pooling) while extracting fees; exacerbate UTXO growth through output fragmentation
 *   - Layer-2 Protocol Developers: Organized/constrained (organized/constrained) — build alternative pathways (Lightning, rollups); perceive constraint as temporary with sunset logic
 *   - Consensus Rule Enforcement: Institutional/arbitrage (institutional/arbitrage) — maintains 'full node must carry all UTXOs' norm through institutional inertia despite technical feasibility of pruning
 *   - Analytical Observer: Analytical/analytical (analytical/analytical) — risks naturalizing contingent design (UTXO model) as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utxo_set_explosion, 0.58).
domain_priors:suppression_score(utxo_set_explosion, 0.72).
domain_priors:theater_ratio(utxo_set_explosion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utxo_set_explosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(utxo_set_explosion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(utxo_set_explosion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utxo_set_explosion, tangled_rope).
narrative_ontology:human_readable(utxo_set_explosion, "UTXO Set Explosion in Bitcoin and Blockchain Systems").
narrative_ontology:topic_domain(utxo_set_explosion, "cryptocurrency/blockchain_scalability").

domain_priors:requires_active_enforcement(utxo_set_explosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utxo_set_explosion, exchange_operators).
narrative_ontology:constraint_beneficiary(utxo_set_explosion, mining_pools).
narrative_ontology:constraint_beneficiary(utxo_set_explosion, full_node_operators_with_resources).
narrative_ontology:constraint_victim(utxo_set_explosion, solo_miners).
narrative_ontology:constraint_victim(utxo_set_explosion, light_client_users).
narrative_ontology:constraint_victim(utxo_set_explosion, micropayment_services).
narrative_ontology:constraint_victim(utxo_set_explosion, blockchain_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOLO MINER (SNARE) — Cannot economically maintain a full UTXO index or efficiently mine without it. Extraction is maximal: forced to either (a) consolidate funds into exchange wallets (surrendering custody), (b) use mining pools (surrendering block discovery rewards), or (c) operate at severe disadvantage. No viable exit path within Bitcoin protocol constraints.
constraint_indexing:constraint_classification(utxo_set_explosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LIGHT CLIENT USER (TANGLED ROPE) — Constrained by bandwidth and storage limits; must trust external servers for transaction verification. The constraint provides genuine coordination (SPV proofs, Bloom filters enable scalable participation) but with embedded extraction: reliance on third-party infrastructure, vulnerability to eclipse attacks, delayed confirmations. Moderate extraction with some benefit.
constraint_indexing:constraint_classification(utxo_set_explosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXCHANGE OPERATOR (ROPE) — Benefits from UTXO set explosion through barriers to entry: only institutions with capital to maintain full nodes can operate exchanges efficiently. Experiences the constraint as pure coordination: managing user withdrawals, building batching logic, optimizing fee structures. Net beneficiary — extraction runs toward this agent through network consolidation.
constraint_indexing:constraint_classification(utxo_set_explosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING POOL OPERATOR (TANGLED ROPE) — Provides coordination (pooled variance reduction, payment distribution logic) while extracting fees (1-4% of rewards). Constrained by UTXO set burden: must maintain efficient indexing to serve miners, but pool operation itself creates many small UTXOs, exacerbating the problem. Active enforcement through stratum protocol standardization.
constraint_indexing:constraint_classification(utxo_set_explosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LAYER-2 PROTOCOL DEVELOPERS (SCAFFOLD) — Lightning Network, sidechains, rollups create alternative verification pathways that reduce reliance on the base-layer UTXO set. The constraint is temporary: as Layer-2 transactions mature and establish custodial norms, settlement to Layer-1 can batch millions of transactions into single UTXOs. Sunset clause is genuine: if 90% of payments move to Layer-2, base-layer UTXO burden becomes manageable for ecosystem nodes.
constraint_indexing:constraint_classification(utxo_set_explosion, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENSUS RULE ENFORCEMENT (PITON) — The 'no pruning without hard fork' norm is substantially performative. Full nodes could prune UTXOs without consensus validity (only archival nodes would break), but institutional inertia preserves the fiction that every node must carry the entire UTXO set. Theater arises from the design principle's nobility (decentralization through full verifiability) persisting despite its practical abandonment (most nodes now use SPV or delegated verification). Theater ratio high despite low actual functional demand.
constraint_indexing:constraint_classification(utxo_set_explosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION-THEORETIC VIEW (MOUNTAIN) — From a universalizable perspective, UTXO set growth is inherent to any immutable transaction ledger: if users create many outputs and spend them slowly, the UTXO set grows polynomially. This perspective risks naturalizing contingent design choices (Bitcoin's UTXO model, no built-in pruning) as immutable laws of distributed systems. However, the structural data contradicts this — the constraint is a *design tradeoff* (Byzantine-robust verification vs scalability), not a natural law. Engine will flag this as a false summit.
constraint_indexing:constraint_classification(utxo_set_explosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utxo_set_explosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(utxo_set_explosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utxo_set_explosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(utxo_set_explosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(utxo_set_explosion, TR),
    TR >= 0.70.

:- end_tests(utxo_set_explosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The UTXO set burden creates barriers to independent full node operation. Entry cost (hardware, bandwidth, disk I/O) excludes solo miners and forces retail participation through custodial or light-client models. However, extractiveness is not 0.75+ (snare threshold) because: (1) Layer-2 alternatives genuinely reduce reliance on base-layer UTXO set, (2) hardware improvements tangibly lower absolute costs over time, (3) the mechanism incentivizes batching and consolidation, which partially self-regulate UTXO growth. The constraint is extractive but not purely so. Suppression (0.72): High. Suppression arises from protocol-level irreversibility (UTXO set growth is structurally inherent to immutable ledgers) combined with institutional lock-in (hard fork to add pruning faces coordination failure risk). Exit costs are very high: solo miners cannot cheaply switch to alternative blockchains (they have sunk mining capital in Bitcoin-specific hardware). Theater ratio (0.35): Low-moderate. The constraint is substantially functional — UTXO set indexing is genuinely necessary for transaction validation. Theater increases slightly over time as the 'decentralization through full nodes' ideal persists despite empirical concentration of full nodes among institutions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The solo miner sees a snare (pure extraction, no escape). The light client sees tangled rope (some coordination benefit through SPV, but constrained by trust and latency). The exchange operator sees rope (genuine coordination, net benefit from ecosystem). The Layer-2 developer sees scaffold (temporary constraint being actively solved by protocol alternatives). The consensus rule enforcer sees piton (the 'full node' ideal persists through norm despite practical abandonment). The analytical observer risks seeing mountain (UTXO growth as inherent to immutable ledgers) but the structural data reveals this as a false summit — the constraint is a *design choice* (UTXO model vs account model, no built-in pruning) that could be altered by protocol amendment. The perspectival gap reveals that 'UTXO set explosion' is not a single constraint but a bundle of design decisions that different stakeholders experience with radically different extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agent structural position relative to UTXO burden. Solo miners (powerless/trapped) experience maximum d ≈ 0.95, yielding f(d) ≈ 1.42 — they bear full extractive force and cannot exit. Exchange operators (institutional/arbitrage) experience minimal d ≈ 0.05, yielding f(d) ≈ -0.12 — they benefit from the barrier and can arbitrage alternatives (custodial, sidechains, competitors). Light client users (moderate/constrained) experience moderate d ≈ 0.65, yielding f(d) ≈ 1.00 — they bear cost but have exit options (hardware upgrade, Layer-2 services). Layer-2 developers (organized/constrained) experience lower d ≈ 0.40 due to their ability to bypass the constraint entirely through protocol design. The beneficiary-victim split is clear: those who control infrastructure (institutional power + arbitrage exit) benefit; those without capital or technical resources (powerless + trapped exit) bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false labeling by acknowledging that genuine coordination (Byzantine-robust verification through full nodes) is real AND that extraction (barriers to entry from hardware requirements) is real. The tangled_rope classification captures this hybrid: coordination function (validating transactions requires distributed state) exists alongside asymmetric extraction (only well-resourced actors can participate fully). The false mountain in the analytical perspective demonstrates precisely why cross-position analysis matters — a single observer might conclude UTXO growth is an immutable property of distributed systems, missing that the Bitcoin design could have used account models (like Ethereum), pruning windows (like some alternative chains), or dynamic state commitment schemes. The scaffold perspective from Layer-2 developers is crucial for mandatrophy resolution: it shows that the constraint is *temporary* rather than fundamental. If Layer-2 systems successfully absorb transaction velocity while batching settlements, the UTXO set can stabilize despite continued user growth. This temporal distinction separates snare (permanent extraction) from tangled_rope (extractive coordination that can be sunset through alternative design).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utxo_growth_linearity,
    'Is UTXO set growth fundamentally linear in the number of users, or does it scale with transaction velocity?',
    'Historical analysis of UTXO set growth correlated with active user growth, transaction frequency, and output consolidation patterns. Projection models comparing stable-state UTXO size under different transaction velocity assumptions.',
    'If linear in users: the constraint is inherent and immutable for any peer-to-peer currency. If sublinear under consolidation: extraction mechanism depends on user behavior incentives (batching, UTXOs per user), enabling Layer-2 or protocol-level mitigations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(utxo_growth_linearity, empirical, 'Whether UTXO growth is inherent to user count or contingent on transaction patterns').

omega_variable(
    hardware_catch_up,
    'Will storage and I/O hardware improvements outpace UTXO set growth, rendering the bottleneck obsolete?',
    'Moore''s Law analysis for SSD capacity and random-access speed; projection of UTXO set size in 2035-2040 vs typical full node hardware; cost-per-byte trend extrapolation.',
    'If hardware catches up: constraint becomes piton (maintained by norm, not by structural necessity). If UTXO grows faster: extraction mechanism persists and may strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_catch_up, empirical, 'Whether hardware improvements will make UTXO set manageable').

omega_variable(
    layer2_adoption_threshold,
    'What Layer-2 adoption percentage makes base-layer UTXO consolidation sufficient for solo mining viability?',
    'Economic analysis: break-even point where mining reward covers UTXO-set maintenance cost. Transaction velocity models under different Layer-2 adoption scenarios (10%, 50%, 90%). Measurement of actual full node operating costs in dollar terms per transaction verified.',
    'If threshold < 50%: scaffold sunset is realistic, constraint becomes temporary. If threshold > 80%: Layer-2 adoption alone cannot solve the constraint; protocol-level changes required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer2_adoption_threshold, empirical, 'Layer-2 adoption threshold for UTXO consolidation viability').

omega_variable(
    dust_attack_persistence,
    'Are dust outputs (very small UTXOs) structural attacks on the full node ecosystem, or market artifacts of fee miscalibration?',
    'Analysis of dust UTXO creation rates before and after fee-bumping wallets; correlation with transaction volume; measurement of deliberate vs accidental dust creation.',
    'If structural attacks: suppression mechanism is adversarial (requires active defense). If fee artifacts: suppression mechanism is temporary (resolves with better fee markets). Changes classification from snare to more temporary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dust_attack_persistence, empirical, 'Whether dust outputs are attacks or market artifacts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utxo_set_explosion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utxo_tr_t0, utxo_set_explosion, theater_ratio, 0, 0.28).
narrative_ontology:measurement(utxo_tr_t5, utxo_set_explosion, theater_ratio, 5, 0.31).
narrative_ontology:measurement(utxo_tr_t10, utxo_set_explosion, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(utxo_be_t0, utxo_set_explosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(utxo_be_t5, utxo_set_explosion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(utxo_be_t10, utxo_set_explosion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utxo_set_explosion, resource_allocation).
narrative_ontology:boltzmann_floor_override(utxo_set_explosion, 0.18).
narrative_ontology:affects_constraint(utxo_set_explosion, blockchain_full_node_centralization).
narrative_ontology:affects_constraint(utxo_set_explosion, mining_pool_concentration).
narrative_ontology:affects_constraint(utxo_set_explosion, layer2_custody_consolidation).

% DUAL FORMULATION NOTE:
% UTXO set explosion is downstream of Bitcoin's core design choice (UTXO model + immutable ledger). It is upstream of full node centralization (UTXO burden forces nodes to consolidate in data centers) and mining pool concentration (solo mining becomes uneconomical). The upstream constraints (design choices) have their own extractiveness reflecting how immutable those choices are; downstream constraints (centralization outcomes) have their own extractiveness reflecting how reversible consolidation is through market or protocol changes. This story focuses on the UTXO set constraint itself as the causal link between design and centralization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(utxo_set_explosion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
