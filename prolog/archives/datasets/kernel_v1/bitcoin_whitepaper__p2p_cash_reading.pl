% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__p2p_cash_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin as Censorship-Resistant P2P Cash
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper promised 'a purely peer-to-peer version of
 *   electronic cash' enabling 'direct transactions without going through a
 *   financial institution.' This p2p cash reading interprets that promise as
 *   the primary constraint: Bitcoin should function as a low-cost medium of
 *   exchange for direct electronic transactions. The reading is contested by
 *   the digital gold interpretation (Bitcoin as censorship-resistant store of
 *   value rather than transaction medium) and the protocol ossification
 *   reading (Bitcoin as immutable reference ledger regardless of cash use
 *   case). This story generates Bitcoin as a tangled_rope constraint where
 *   censorship resistance and transaction accessibility form the coordination
 *   function, but architectural choices (10-minute blocks, 1MB block size
 *   limit post-2017 soft fork debate) suppress high-frequency p2p
 *   transactions and extract from users via fee markets. The victim set is
 *   powerless and organized transactors denied medium-of-exchange
 *   functionality; the beneficiary set is miners, exchanges, and wealth
 *   holders who benefit from scarcity and network effect value capture. The
 *   constraint demonstrates false summit risk: the fee market and transaction
 *   throughput limits are often rationalized as cryptographic necessities
 *   when they are actually contingent protocol design choices.
 *
 * KEY AGENTS:
 *   - Excluded Small-Value Transactors: Primary victim (powerless/trapped) — face prohibitive fee ratios during congestion; denied p2p cash functionality by economic constraint not technical barrier
 *   - Unbanked/Underbanked Populations: Primary victim (organized/constrained) — receive genuine censorship-resistant access but high fees and volatility block sustainable daily-use adoption
 *   - Miners: Secondary victim (organized/constrained) — face fee pressure from block size limits; revenue model extracts via difficulty adjustment and hash rate competition
 *   - Full Node Operators: Secondary victim (moderate/constrained) — maintain network consensus; bear bandwidth and storage costs while experiencing fee-market asymmetry
 *   - Exchanges and Custodians: Primary beneficiary (institutional/arbitrage) — capture transaction routing value and fee differentiation arbitrage across platforms and time periods
 *   - High-Value Transactors: Secondary beneficiary (powerful/mobile) — can absorb fee costs; benefit from network effect and censorship resistance without transaction volume constraint
 *   - Bitcoin Core Developers: Institutional actor (institutional/constrained) — stewards protocol choices that instantiate the p2p vs. digital gold tradeoff; face governance constraints from community consensus and technical debt
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing architectural choices (block size, fee market) as immutable cryptographic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.48).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Censorship-Resistant P2P Cash").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'uuid-bitcoin-p2p-cash-reading-001').
narrative_ontology:cs_kernel_codification('uuid-bitcoin-p2p-cash-reading-001', fixed_text).
narrative_ontology:cs_authority_grounding('uuid-bitcoin-p2p-cash-reading-001', distributed).
narrative_ontology:cs_reading_relation('uuid-bitcoin-p2p-cash-reading-001', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('uuid-bitcoin-p2p-cash-reading-001', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('uuid-bitcoin-p2p-cash-reading-001', foundational, transaction_throughput_priority).
narrative_ontology:cs_axiom_status(transaction_throughput_priority, holdable).
narrative_ontology:cs_axiom_grounding('uuid-bitcoin-p2p-cash-reading-001', transaction_throughput_priority, deontological).
narrative_ontology:cs_axiom('uuid-bitcoin-p2p-cash-reading-001', secondary, network_effect_incumbent_responsibility).
narrative_ontology:cs_axiom_status(network_effect_incumbent_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('uuid-bitcoin-p2p-cash-reading-001', network_effect_incumbent_responsibility, instrumental).
narrative_ontology:cs_reference_frame('uuid-bitcoin-p2p-cash-reading-001', satoshi_p2p_cash_vision).
narrative_ontology:cs_drift_state('uuid-bitcoin-p2p-cash-reading-001', contemporary_bitcoin_protocol_design, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('uuid-bitcoin-p2p-cash-reading-001', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, high_frequency_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, censorship_resistant_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, miners_restricted_by_fee_pressure).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, transaction_throughput_constrained_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED TRANSACTOR (SNARE) — Users with small transaction values face prohibitive fee ratios (fees exceed transaction value during congestion). Trapped by network effects: Bitcoin is the most censorship-resistant option, but its architecture extracts via fee markets. No alternative with equivalent properties. Maximum experienced extraction.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNBANKED POPULATION (TANGLED ROPE) — Access to permissionless transaction is real benefit (coordination function); but high fees and volatility create barriers to sustained use as daily medium of exchange. Organized through merchant networks and informal payment systems; constrained by technical knowledge, device access, and fee volatility. Mixed extraction and coordination.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EXCHANGE/CUSTODIAN (ROPE) — Benefits from the network effect and transaction volume. Settlement via Bitcoin provides routing and value transfer coordination with minimal coercive overhead. Can arbitrage fee differences across ledgers and time periods. Net beneficiary with transaction facilitation; experiences constraint as pure coordination.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FULL NODE OPERATOR (TANGLED ROPE) — Enforces network consensus rules (coordination function); requires significant computational resources and bandwidth (extraction cost). Censorship resistance is real public good they help maintain. Constrained by hardware costs and network growth. Benefits from network security increase but bears operational burden asymmetrically.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MINER NETWORK (SNARE) — Fee market architecture extracts from miners through difficulty adjustment and hash rate competition. Revenue constraint (block reward + fees) forces optimization of transaction selection; low-fee transactions excluded. Organized but constrained by hardware capital requirements and energy costs. Experiencing extraction disguised as incentive alignment.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: P2P CASH NARRATIVE (PITON) — The original vision (Satoshi whitepaper) emphasized low-cost direct transactions. Contemporary Bitcoin practice has substantially departed from this narrative: average transaction fees exceed institutional payment rails for small values; settlement times incompatible with retail commerce. The narrative persists in community rhetoric but the functional reality has shifted toward digital gold. Piton classification: performative commitment to p2p cash vision maintained through narrative, not architecture.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a cryptographic/scaling perspective, any decentralized ledger faces immutable tradeoffs: bandwidth limits, state growth, and consensus overhead create inherent constraints on transaction throughput. Byzantine fault tolerance requires redundancy that precludes high-frequency settlement. This perspective sees the fee market not as contingent architecture but as necessary response to natural limits on what decentralized consensus can achieve. However, false summit risk: alternative architectures (rollups, sidechains, layer-2 payment channels) are contingent institutional solutions, not natural laws.
constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper__p2p_cash_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The p2p cash reading frames Bitcoin's fee market as extraction from transaction volume. Early Bitcoin (t=0, ε=0.15) had negligible fees and functioned as p2p cash. As adoption increased and block space became scarce (t=5-10), fees rose exponentially while suppression of transaction throughput increased (block size frozen at 1MB post-2017; average transaction size ~230 bytes = ~4 transactions per second capacity). By t=10, ε=0.48 reflects that the constraint now extracts via fee market: users denied transaction access unless they pay extraction cost proportional to network congestion. The extractiveness is not total (like a snare) because alternatives exist (layer-2 solutions, competitor chains) and users have some agency; hence tangled_rope rather than snare at the moderate power level. Suppression (0.62): Moderate-high. Multiple mechanisms suppress p2p transaction use: (1) technical (1MB block limit, 10-minute block time reduce transaction capacity), (2) economic (fee market prices out small transactions), (3) social (narrative shift toward digital gold reduces incentive to expand capacity). The suppression is enforced by miners (economically) and developers (technically), creating a maintenance cost. Theater ratio (0.58): Moderate. The p2p cash narrative persists in Bitcoin community rhetoric and remains in the whitepaper, but actual protocol development and community consensus prioritize immutability and security over transaction throughput. The gap between narrative promise and architectural reality is substantial but not total — layer-2 solutions (Lightning Network) provide technical fulfillment of p2p cash vision outside the base layer, maintaining plausible deniability that the p2p promise is being kept.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival divergence reveals the kernel contest. From a p2p cash reading, Bitcoin's fee market is extractive—a contingent architectural choice that suppresses the primary use case (low-cost transactions). From the digital gold reading (sibling), the same fee market is a necessary feature that preserves scarcity and security. From the protocol ossification reading (sibling), neither transaction throughput nor gold-store properties matter—the constraint is the immutability of the ledger itself. The disagreement is not about facts (everyone agrees fees exist and throughput is limited) but about which narrative reading of the whitepaper kernel should determine architectural priorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from each agent's structural relationship to this specific constraint (p2p cash reading). Beneficiaries (exchanges, high-value transactors) have low d (0.10-0.25) because fee markets advantage them — extraction runs toward them, not away. Victims (excluded transactors, miners) have high d (0.75-0.95) because they experience costs without proportional benefits. The piton perspective derives from performance gap between narrative and architecture (theater gate, not chi gate). The mountain perspective risks false summit by naturalizing architectural choices — the engine's false summit detector should flag this perspective given the contingent protocol choices evidenced by the historical record.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The mandatrophy for the p2p cash reading resolves by recognizing that the constraint is fundamentally one reading of an ambiguous kernel. Satoshi's whitepaper contains elements supporting multiple readings: the abstract emphasizes 'peer-to-peer' and 'without a trusted third party'; the protocol design emphasizes immutability and scarcity. The community coalesced around the digital gold reading (circa 2013-2015), which deprioritizes throughput. The p2p cash reading persists but is architecturally marginalized by the base layer design. No single type is correct—the readings reflect different projections of an under-determined kernel onto different agent interests and technical priorities. The tangled_rope classification for this reading is stable: it genuinely coordinates (censorship resistance, peer settlement) while genuinely extracting (via fee markets, capacity limits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_fee_tradeoff_necessity,
    'Is the fee market extraction inherent to decentralized consensus, or a contingent architectural choice for Bitcoin specifically?',
    'Comparative analysis: (a) Bitcoin''s block size limit and 10-minute block time are protocol choices, not cryptographic necessities; (b) Ethereum''s higher block capacity shows different tradeoff curve; (c) Layer-2 solutions (Lightning, Rollups) achieve different throughput-decentralization-settlement-time profiles. If alternatives with lower fees and maintained decentralization are feasible, extraction is architectural not natural.',
    'If natural law: mountain classification holds; fee markets are necessary. If contingent: tangled_rope confirmed; the p2p cash reading loses architectural ground against digital gold reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_fee_tradeoff_necessity, empirical, 'Whether fee market extraction is necessary or architectural').

omega_variable(
    network_effect_lock_in_degree,
    'How much is the continued use of Bitcoin as p2p cash constrained by network effects vs. active choice?',
    'Measure user migration patterns when competing platforms (Litecoin, Dogecoin, Lightning Network) offer lower fees and comparable censorship resistance. Track transaction volume shifts during fee spikes. Distinguish exit caused by technical barriers (no merchant acceptance alternative) from exit caused by costs (user prefers other platform when fees are high).',
    'If lock-in is high (users trapped by network effects): snare classification confirmed for excluded transactors. If lock-in is low (users switch when fees are prohibitive): snare weakens; users have mobile exit despite fees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_in_degree, empirical, 'Degree of network-effect lock-in vs. user choice in transaction platform selection').

omega_variable(
    reading_kernel_contest,
    'Which reading of the Bitcoin whitepaper is ''correct'': p2p cash (this reading) or digital gold (sibling reading)?',
    'Not empirically resolvable. The kernel (Satoshi''s whitepaper) is ambiguous: abstract 1 emphasizes p2p electronic cash; protocol design emphasizes scarcity and immutable ledger. The readings coexist in the Bitcoin community as competing interpretations. Resolution would require authority (developer consensus, market practice, or formal amendment) to establish canonical reading. Currently distributed authority with no foreclosure.',
    'If p2p cash reading dominates: network upgrades prioritize throughput (larger blocks, faster settlement); victim set expands to include high-frequency transactors denied capacity. If digital gold reading dominates: network prioritizes immutability and decentralization; victim set includes those denied transaction access due to fee markets (current state).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which reading of Bitcoin whitepaper kernel is canonical: p2p cash or digital gold').

omega_variable(
    suppression_mechanism_structural_vs_policy,
    'Is the suppression of high-frequency p2p transactions (via fee markets and block limits) a structural necessity or a policy choice encoded in the Bitcoin protocol?',
    'Historical analysis: (a) Satoshi''s writings emphasized p2p as primary use case; (b) Core developer consensus shifted toward digital gold (2013-2015); (c) Block size debate (2015-2017) made the choice explicit. Measure: What portions of suppression are cryptographic limits vs. protocol configuration choices (block size, fee minimum, block interval)?',
    'If structural: mountain or rope classification appropriate. If policy: snare confirmed for excluded transactors; policy change could rebalance victims/beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_policy, empirical, 'Whether transaction suppression via fees is structural or policy-driven').

omega_variable(
    false_summit_risk_bitcoin_limits,
    'Is the p2p cash bottleneck being rationalized as a natural law of decentralized systems when it is actually a contingent institutional arrangement (protocol configuration)?',
    'Compare Bitcoin''s architecture choices against (a) theoretical cryptographic limits (Byzantine fault tolerance, consensus time-space tradeoffs), and (b) implemented alternatives (Ethereum, Lightning Network, Solana). Identify which constraints are forced by math vs. chosen by protocol design.',
    'If false summit: the mountain perspective naturalizes contingent policy; restructured as tangled_rope with potential for architectural reform. If genuine summit: mountain perspective is valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk_bitcoin_limits, empirical, 'False summit detection: naturalizing architectural choices as cryptographic necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btcp2p_theater_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(btcp2p_theater_t5, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(btcp2p_theater_t10, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(btcp2p_ext_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(btcp2p_ext_t5, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(btcp2p_ext_t10, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(btcp2p_supp_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(btcp2p_supp_t5, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(btcp2p_supp_t10, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_lightning_network_channel_state).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, cryptocurrency_fee_market_extraction).

% DUAL FORMULATION NOTE:
% The p2p cash reading (this constraint) and the digital gold reading (sibling) decompose the single Bitcoin whitepaper into two structurally distinct constraints with different victim sets, different architectural implications, and different ε values. The p2p reading emphasizes transaction throughput and fee suppression (ε=0.48, tangled_rope). The digital gold reading emphasizes immutability and scarcity (ε varies, likely rope or mountain from digital gold perspective). Both are valid readings of the same kernel; they are not measurement-dependent observations of one constraint but genuinely different constraints instantiated from an ambiguous kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
