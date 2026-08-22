% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__electronic_cash_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Electronic Cash Telos: Expanded On-Chain Capacity Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper title 'A Peer-to-Peer Electronic Cash System'
 *   instantiates a specific coordination mandate: the protocol must support
 *   everyday transactional use with low fees and high throughput. This
 *   constraint reading takes that mandate as binding and examines the
 *   enforcement costs. Under this reading, system design prioritizes expanded
 *   on-chain capacity (larger blocks, higher transaction-per-second
 *   throughput) to keep fees low enough for retail adoption. The structural
 *   asymmetry: payment processors and low-value transactors benefit from high
 *   throughput and low per-transaction costs; full-node operators
 *   (particularly those in bandwidth-constrained environments) bear the cost
 *   of storing and validating larger blocks. The reading's core premise—that
 *   the whitepaper's cash telos is architecturally determinative—coexists
 *   with an alternative reading (store_of_value_reading) that subordinates
 *   the cash use case to the deeper commitment to decentralization and
 *   full-node accessibility.
 *
 * KEY AGENTS:
 *   - Payment processors: benefit from high on-chain throughput; operate layer-2 solutions that reduce costs for their users
 *   - Low-value transactors: benefit from low per-transaction fees; economically dependent on on-chain capacity for small payments
 *   - Full-node operators: pay the cost of expanded capacity via storage and bandwidth; structurally deprioritized under this reading
 *   - Miners/block producers: agenda-setters; control capacity expansion via consensus; benefit from higher transaction throughput (more fees per block)
 *   - Store-of-value advocates: constrained by this reading's prioritization; argue the whitepaper's true mandate is decentralization, not cash use
 *   - Regulatory authorities: excluded by design; would object to an effective cash-replacement mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.72).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Electronic Cash Telos: Expanded On-Chain Capacity Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, 'd4204595-eac7-4f59-9380-baf5231c47ab').
narrative_ontology:cs_kernel_codification('d4204595-eac7-4f59-9380-baf5231c47ab', fixed_text).
narrative_ontology:cs_authority_grounding('d4204595-eac7-4f59-9380-baf5231c47ab', distributed).
narrative_ontology:cs_reading_relation('d4204595-eac7-4f59-9380-baf5231c47ab', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4204595-eac7-4f59-9380-baf5231c47ab', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('d4204595-eac7-4f59-9380-baf5231c47ab', foundational, whitepaper_cash_telos_is_architecturally_binding).
narrative_ontology:cs_axiom_status(whitepaper_cash_telos_is_architecturally_binding, holdable).
narrative_ontology:cs_axiom_grounding('d4204595-eac7-4f59-9380-baf5231c47ab', whitepaper_cash_telos_is_architecturally_binding, conventional).
narrative_ontology:cs_axiom('d4204595-eac7-4f59-9380-baf5231c47ab', foundational, expanded_capacity_necessary_for_retail_adoption).
narrative_ontology:cs_axiom_status(expanded_capacity_necessary_for_retail_adoption, holdable).
narrative_ontology:cs_axiom_grounding('d4204595-eac7-4f59-9380-baf5231c47ab', expanded_capacity_necessary_for_retail_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('d4204595-eac7-4f59-9380-baf5231c47ab', satoshi_peer_to_peer_cash_vision).
narrative_ontology:cs_drift_state('d4204595-eac7-4f59-9380-baf5231c47ab', contemporary_2024_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4204595-eac7-4f59-9380-baf5231c47ab', '2026-06-12T14:22:33Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, miners_and_block_producers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, exchange_trading_participants).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate Layer 2 solutions, payment channels, and custodial services that reduce on-chain transaction costs for merchant settlement. The electronic-cash reading drives demand for higher on-chain capacity and lower baseline fees, which incentivizes their business model expansion. They benefit from a system designed to accommodate retail payment volume.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Users making small everyday payments (coffee, groceries, remittances) who depend on low per-transaction fees to make on-chain use economical. Under the electronic-cash reading, system design prioritizes their transaction capacity at the expense of other node-operational constraints. They benefit from the commitment to expanded capacity but remain dependent on fee markets.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, payer).

% Run full validating nodes to verify the blockchain directly. Expanded on-chain capacity (8MB+ blocks, higher transaction throughput) increases storage and bandwidth costs per node. The electronic-cash reading treats node-operator burden as a secondary constraint subordinate to transactional throughput, placing them structurally as payers. Their exit (ceasing validation, reducing node participation) erodes the decentralization the protocol claims to offer, creating identity lock: they continue despite cost as a matter of principle.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators, payer,
    moderate, biographical, identity_locked, global).

% Set block size limits and transaction inclusion rules via consensus upgrades. Under the electronic-cash reading, they enforce expanded capacity to accommodate retail transaction volume. They benefit from increased transaction throughput (more fees per block), though they also bear marginal costs from larger blocks. They control the enforcement mechanism—whether to adopt larger blocks or maintain smaller limits.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, miners_and_block_producers, agenda_setter,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, miners_and_block_producers, beneficiary).

% Traders and arbitrage operators using Bitcoin for high-frequency value transfer and exchange settlement. Lower fees and higher transaction throughput enable more efficient capital movement. They are incidental beneficiaries of expanded capacity but not the primary constituency the electronic-cash reading addresses.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, exchange_trading_participants, beneficiary,
    organized, biographical, arbitrage, global).

% Believe Bitcoin's primary value proposition is long-term asset store, not everyday cash. They argue expanded on-chain capacity degrades decentralization by raising node-operator burden and that the whitepaper's 'cash' framing is secondary to the deeper commitment to verifiable decentralization. Under the electronic-cash reading's enforcement, they are structurally deprioritized—their constraint preferences (smaller blocks, full-node accessibility) are overridden by the capacity-expansion agenda.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_advocates, payer,
    organized, generational, constrained, global).

% The whitepaper and early writings constitute the canonical kernel text. Neither a beneficiary nor a victim (not an agent), but an analytical reference point for interpreting the founding constraint. This reading instantiates one interpretation of that text.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, satoshi_nakamoto_corpus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_whitepaper_purpose__electronic_cash_reading, satoshi_nakamoto_corpus).

% Governments and financial regulators seeking to limit cash-replacement mechanisms and impose transaction tracking. An effective electronic-cash system would disintermediate their surveillance. They are structurally excluded from the Bitcoin ecosystem's governance but would object loudly to the reading's success; their exclusion is precisely what the decentralization mechanism is built to maintain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, regulatory_authorities, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized payment rail that eliminates intermediaries for retail transactions. The coordination problem: how to enable trusted peer-to-peer value transfer without a central authority. The electronic-cash reading solves this by expanding on-chain capacity so the coordination function (direct settlement on the canonical ledger) remains economically viable for low-value everyday transactions.
% TRANSFER_FUNCTION: Transfers the cost of node operation (storage, bandwidth, compute) from the protocol's supporters (full-node operators who maintain the decentralized verification layer) to beneficiaries (payment processors and low-value transactors who gain reduced fees and higher throughput). Block space—previously scarce and allocated by fee market—becomes more abundant, but the cost of maintaining that abundance is distributed asymmetrically.
% ABSENT_VOICES: Store-of-value advocates and full-node operators uncomfortable with the cost structure; regulatory authorities excluded by design; users in bandwidth-limited jurisdictions who cannot run nodes under expanded capacity; alternative layer-1 designs (competing payment rails) that are excluded by Bitcoin's governance structure and network-effect dominance.
% DISAPPEARANCE_RATIONALE: If the electronic-cash telos were abandoned and on-chain capacity constrained permanently, merchants would migrate to layer-2 systems (Lightning, sidechains) or alternative coins designed for payment; low-value transactors would follow; the Bitcoin ecosystem would consolidate around asset-store use cases. The constraint's enforcement—pushing expanded on-chain capacity—shapes which use cases are economically viable on the canonical ledger.
% FOUNDING_PROBLEM: Peer-to-peer electronic cash requires enabling everyday retail transactions without a trusted third party, at fees low enough to be economical for small purchases. The whitepaper's title and abstract commit to this use case as the core motivation.
% FOUNDING_PROBLEM_CORROBORATION: Satoshi's whitepaper and early communications (2008-2010 emails, forum posts) explicitly prioritize the 'cash' framing and retail adoption. Independent economic analysis and payment-system researchers (Rogoff 2014, Rogoff & Zettelmeyer 2016, Kroll et al 2013) confirm that low-fee everyday transactions were the original stated goal. However, cryptocurrency researchers and decentralization advocates (Antonopoulos 2014, Andreas M. Antonopoulos writings; Luke Dashjr consensus research) argue that on-chain capacity constraints are now intentional, reflecting a shift to store-of-value primacy. The corroboration is split: payment researchers and merchants testify to the founding problem remaining live; protocol developers and node-operator communities testify to a deliberate shift in priorities. No single authoritative source resolves the disagreement because Satoshi disappeared in 2011.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint structurally extracts costs from node operators and imposes them asymmetrically—beneficiaries do not bear proportional cost for the capacity they use. Suppression (0.72) is substantial because the electronic-cash reading's enforcement requires that alternative framings (store-of-value primacy) be overridden in protocol governance; dissent from full-node operators is structured out via consensus mechanisms that weight miner/processor interests. Theater (0.41) is moderate: the constraint's rhetoric emphasizes Satoshi's original vision and retail adoption, but much of the on-chain transaction volume is actually exchange trading and value transfer, not everyday cash use. The measurement series show extractiveness and suppression both rising over the interval (0–12), plateauing around time 12 as the protocol reached practical capacity limits under current consensus rules. The theater ratio rises more gradually as the gap widens between the stated coordination purpose (everyday cash) and actual on-chain use (exchange settlement, store-of-value transfers). At time 16, metrics plateau—projecting forward shows no further change absent new capacity upgrades.
 *
 * PERSPECTIVAL GAP:
 *   From the payment-processor seat, this is a genuine coordination mechanism that enables retail transactions and reduces intermediation costs—they compute it as rope-like (coordination benefit, low enforcement cost from their perspective). From the full-node-operator seat, it is extractive and suppressive: they are forced to bear storage/bandwidth costs without corresponding benefit, and their alternative preference (constrained capacity, lower node burden) is suppressed via mining-pool consensus. From the miner seat, it is beneficial coordination: they set the rules, collect transaction fees from higher throughput, and maintain the enforcement mechanism. The engine computes per-seat classification from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors and low-value transactors are structural beneficiaries: they experience reduced costs and expanded opportunity under capacity expansion (d near 0.0–0.3). Full-node operators are structural targets: they bear asymmetric costs without corresponding benefit and are suppressed when they advocate for reduced capacity (d near 0.8–1.0). Miners are both agenda-setters and beneficiaries: they control the enforcement mechanism and collect more fees, so their directionality is mixed but closer to beneficiary (d near 0.3–0.5). Their identity_locked exit on the node-operator side indicates that many small node operators continue despite cost because they see node-running as a matter of principle (maintaining decentralization), not as economic choice. This makes the suppression particularly effective: the cost extraction works because targets cannot credibly exit without violating their own commitment to decentralization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling everyday peer-to-peer electronic cash) is contested: payment researchers say it is live and economically significant; protocol developers and node operators say it has been superseded by store-of-value primacy and decentralization. The disappearance_verdict (world_rearranges) indicates the constraint is not natural law. However, the theater_ratio rising from 0.25 to 0.41 over the interval suggests the constraint is increasingly maintained through narrative and governance enforcement rather than genuine coordination function—actual on-chain retail transaction volume is dwarfed by exchange settlement and store-of-value transfers. This is a candidate for mandatrophy: the founding problem (retail cash adoption) is substantially unmet; the constraint persists through consensus mining weight and narrative commitment; the cost is borne asymmetrically by node operators whose resistance to further capacity expansion is suppressed. If the theater_ratio continues rising or exchange volume further dominates retail, the constraint would show classic piton dynamics (performance without function). Current classification remains tangled_rope because coordination function (payment settlement without intermediaries) is real, even if the beneficiary distribution is asymmetric and the stated use case (everyday cash) is not the primary actual use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Is peer-to-peer electronic cash (retail payments without intermediaries) still a live, economically significant problem that Bitcoin system design should prioritize, or has it been superseded by store-of-value and settlement-layer use cases?',
    'Measure on-chain transaction composition over time: what fraction of transactions are retail payments (small value, frequent, low-value-density) vs. exchange settlement, store-of-value transfers, and high-value transactions? Compare actual on-chain retail volume to projected retail volume under the electronic-cash reading. Compare fee levels for retail use to projections made in the whitepaper. Survey merchant adoption rates and payment-rail economics.',
    'If on-chain retail transactions remain a small minority of volume and fees remain high relative to retail-transaction values, the founding problem is empirically dead—the constraint persists through narrative and mining incentives despite unmet purpose (piton classification). If retail volume and fee levels track whitepaper projections, the founding problem is live and the constraint is functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether retail cash payments remain the constraint''s operative function or have been displaced by store-of-value and settlement use.').

omega_variable(
    capacity_expansion_decentralization_trade_off,
    'Is the trade-off between on-chain capacity and full-node accessibility genuinely necessary (structural), or can capacity expand without materially increasing node-operation costs?',
    'Monitor node-operation costs (bandwidth, storage, CPU) as a function of block size and transaction throughput. Test whether bandwidth and storage scaling remain hardware-linear or whether Moore''s Law, storage compression, and pruning strategies allow capacity to expand without proportional cost increase. Measure full-node operator participation and geographic distribution under different capacity regimes.',
    'If the trade-off is genuine (costs scale with capacity), the extraction from node operators is structurally unavoidable under this reading—the question becomes whether the beneficiary distribution is justified by the coordination function. If the trade-off is not structurally necessary (costs grow slower than capacity), the extraction is a policy choice masquerading as structural necessity—the constraint would show stronger suppression (hiding the choice) and the theater ratio would rise (performing necessity when policy choice is operational).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_expansion_decentralization_trade_off, empirical, 'Whether on-chain capacity and decentralized node operation are structurally coupled or whether capacity can expand without proportional cost to operators.').

omega_variable(
    kernel_interpretive_authority,
    'Who has the authority to resolve the whitepaper''s ambiguous trade-off between retail-cash adoption and full-node accessibility? Satoshi''s disappearance eliminated an oracle; does the Bitcoin community have a designated interpreter, or is the whitepaper now a contested substrate without authoritative reading?',
    'Examine governance mechanisms: do consensus-rule changes require a designated authority, or do they emerge from mining-pool coordination and developer consensus? Assess whether any reading has achieved supermajority adoption and whether that adoption is based on explicit endorsement of the electronic-cash reading or on contingent mining-pool economic incentives. Survey whether the two readings generate different specific protocol proposals that could be directly compared.',
    'If no designated authority exists and the readings coexist via mining-pool equilibrium, the constraint''s persistence depends on contingent economic incentives (miner fee collection from higher throughput), not on resolved interpretation of the whitepaper''s mandate. This would indicate the kernel has become fully substrate-like (the text provides no tiebreaker) and the reading''s persistence depends on enforcement (suppression of alternatives) rather than on grounded interpretation. The constraint would shift toward Piton classification (performance without functional mandate resolution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretive_authority, conceptual, 'Whether the electronic-cash reading''s mandate is grounded in an authoritative interpretation of the whitepaper or emerges from contingent economic incentives in the absence of interpretive authority.').

omega_variable(
    suppression_mechanism_structural_vs_contingent,
    'Is the suppression of store-of-value advocates and node-operator resistance structural (built into the protocol''s consensus rules) or contingent (emerging from mining-pool economic coordination)?',
    'Analyze consensus governance: do the protocol''s rules structurally favor capacity expansion (e.g., through explicit block-size limits or fees), or does capacity expansion emerge from coordinated miner choices within a more neutral rule set? Test whether a minority of nodes/miners can credibly fork to enforce an alternative reading (smaller blocks, full-node prioritization) without losing majority-hash support. Survey whether suppression of alternatives is achieved through economic incentive (forks lose value) or governance coercion (rules prevent alternatives).',
    'If suppression is structural (rules built-in), the constraint is actively enforced and the suppression metric accurately reflects governance design. If suppression is contingent (emerges from miner coordination), the constraint is more fragile—a change in mining-pool incentives or the emergence of alternative coins could shift the equilibrium, and the high suppression metric reflects temporary coordination rather than durable enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_contingent, empirical, 'Whether suppression of the store-of-value reading is enforced through protocol rules or sustained through mining-pool economic coordination.').

omega_variable(
    reading_coexistence_stability,
    'Can the electronic-cash and store-of-value readings stably coexist as two valid interpretations of the Bitcoin protocol, or does the protocol''s design force a choice between them?',
    'Monitor whether both readings remain live in governance decisions. Assess whether layer-2 solutions (Lightning Network, sidechains) that implement the electronic-cash reading without on-chain capacity expansion represent a stable compromise (both readings operationalized in different layers) or a temporary détente that will collapse into a explicit choice. Survey whether mining-pool operators explicitly endorse one reading over the other or remain agnostic.',
    'If the readings can coexist (layer-2 implements cash, on-chain remains conservative), suppression is lower than measured and the constraint is less extractive—node operators get the decentralization they prioritize and payment processors get payment capability. If only one reading can be operationalized at scale, suppression is accurate and the constraint is extractive—one reading''s priority is enforced at the cost of the other''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'Whether the electronic-cash and store-of-value readings can both be operationalized or whether one must dominate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 16, 0.41).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 16, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel admits two competing readings with substantially different ε values and beneficiary structures. The electronic_cash_reading (this file) authors ε=0.68 (high extraction from node operators for the benefit of payment processors and low-value transactors). The sibling store_of_value_reading authors a lower ε, reflecting that under the store-of-value mandate, capacity constraints are viewed as necessary costs of decentralization rather than as extractive overhead. Both readings share the referent (the Bitcoin protocol under its whitepaper commitment) but instantiate different constraints from that referent because they define the operative function differently. This is NOT a measurement-basis difference (both adopt the same ε-assessment frame); it is a structural decomposition per DP-001 (ε-invariance): the readings commit to different mandates, which generate different beneficiary/victim structures, which yield different extractiveness values. The two readings are linked via network.affects_constraints to enable contamination analysis and reading-relations inference (coexists_with, per cs_structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
