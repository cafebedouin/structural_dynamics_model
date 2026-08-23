% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin as P2P Electronic Cash (Whitepaper Reading)
 *   domain: technological/monetary/governance
 *
 * SUMMARY:
 *   This constraint story models the Bitcoin protocol as experienced by the
 *   p2p_cash_reading of the bitcoin_whitepaper kernel. The reading holds that
 *   Bitcoin's founding commitment is censorship-resistant electronic cash
 *   with low fees, and that the current fee market (emergent from the 1MB
 *   block size limit) constitutes a structural betrayal of that commitment.
 *   The constraint is the standing arrangement — Bitcoin's consensus rules as
 *   they operate today with a competitive fee market — assessed by this
 *   reading's lights. The reading sees genuine coordination (censorship
 *   resistance, settlement finality) coexisting with asymmetric extraction
 *   (fee market pricing out the whitepaper's intended beneficiaries). The
 *   sibling readings (digital_gold, protocol_ossification) are separate
 *   constraints in the kernel family, linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.72).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.78).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as P2P Electronic Cash (Whitepaper Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "technological/monetary/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '0a548ea2-5885-4ec5-b5b0-615aea21d98c').
narrative_ontology:cs_kernel_codification('0a548ea2-5885-4ec5-b5b0-615aea21d98c', fixed_text).
narrative_ontology:cs_authority_grounding('0a548ea2-5885-4ec5-b5b0-615aea21d98c', lineage).
narrative_ontology:cs_interpretation_layer_present('0a548ea2-5885-4ec5-b5b0-615aea21d98c').
narrative_ontology:cs_reading_relation('0a548ea2-5885-4ec5-b5b0-615aea21d98c', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a548ea2-5885-4ec5-b5b0-615aea21d98c', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('0a548ea2-5885-4ec5-b5b0-615aea21d98c', foundational, low_fees_essential_for_cash_function).
narrative_ontology:cs_axiom_status(low_fees_essential_for_cash_function, holdable).
narrative_ontology:cs_axiom_grounding('0a548ea2-5885-4ec5-b5b0-615aea21d98c', low_fees_essential_for_cash_function, empirically_contingent).
narrative_ontology:cs_axiom('0a548ea2-5885-4ec5-b5b0-615aea21d98c', foundational, block_size_expansion_legitimate_per_whitepaper).
narrative_ontology:cs_axiom_status(block_size_expansion_legitimate_per_whitepaper, holdable).
narrative_ontology:cs_axiom_grounding('0a548ea2-5885-4ec5-b5b0-615aea21d98c', block_size_expansion_legitimate_per_whitepaper, conventional).
narrative_ontology:cs_axiom('0a548ea2-5885-4ec5-b5b0-615aea21d98c', secondary, fee_excluded_users_are_victims).
narrative_ontology:cs_axiom_status(fee_excluded_users_are_victims, holdable).
narrative_ontology:cs_axiom_grounding('0a548ea2-5885-4ec5-b5b0-615aea21d98c', fee_excluded_users_are_victims, deontological).
narrative_ontology:cs_reference_frame('0a548ea2-5885-4ec5-b5b0-615aea21d98c', satoshi_whitepaper_vision).
narrative_ontology:cs_drift_state('0a548ea2-5885-4ec5-b5b0-615aea21d98c', post_2017_blocksize_war, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a548ea2-5885-4ec5-b5b0-615aea21d98c', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, developing_economy_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, merchants_needing_low_fees).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, lightning_developers).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, censorship_resistance_requires_low_fees).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, whitepaper_describes_transactional_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Secure the network through proof-of-work and collect block rewards plus transaction fees. Their revenue increasingly depends on fee market dynamics. They signal support for protocol rules (including block size limits) through mining software choices. Can switch to mining other SHA-256 chains but have sunk capital in Bitcoin-specific hardware.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners, beneficiary).

% Maintain the reference implementation (Bitcoin Core) that defines the consensus rules. Resist block size increases citing decentralization and node operation costs. Control the merge process for protocol changes. Their authority derives from technical expertise and historical continuity with the project. Professional reputation and funding tied to Core stewardship.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Run full nodes that independently validate all transactions and blocks. Enforce consensus rules by rejecting invalid blocks. Cost of node operation (bandwidth, storage, compute) is cited as reason to limit block size. Can choose alternative implementations but overwhelmingly run Core. No direct financial reward; motivated by ideological commitment or business dependency.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, node_operators, agenda_setter,
    moderate, biographical, mobile, global).

% Need to send low-value transactions (remittances, daily purchases, micropayments). Priced out when fees exceed transaction value. Forced to use custodial solutions (exchanges, Lightning custodians) or abandon Bitcoin for alternatives. No voice in protocol governance; exit means losing Bitcoin's censorship resistance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_transactors, payer,
    powerless, immediate, constrained, global).

% Rely on Bitcoin for savings preservation and cross-border payments in high-inflation or capital-controlled economies. Fee spikes make on-chain transactions unaffordable relative to local income. Lightning Network access requires technical knowledge and capital they often lack. No realistic alternative with comparable censorship resistance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, developing_economy_users, payer,
    powerless, biographical, trapped, global).

% Accept Bitcoin for commerce but require low, predictable fees for viability. High and volatile fees make BTC impractical for retail. Many migrated to stablecoins or fiat rails. Those remaining use custodial payment processors, reintroducing trusted intermediaries the whitepaper sought to eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_needing_low_fees, payer,
    moderate, biographical, mobile, global).

% The whitepaper's envisioned beneficiaries: people without access to traditional finance who need censorship-resistant electronic cash. Structurally excluded from on-chain use by fee markets. Would benefit most from low-fee p2p transactions but have zero representation in protocol governance. Their needs are invoked rhetorically but not structurally served.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations, excluded,
    powerless, generational, trapped, global).

% Hold significant BTC as store of value. Benefit from scarcity narrative and fee market that reinforces 'digital gold' positioning. Rarely transact on-chain; when they do, fees are negligible relative to holdings. Fund development and advocacy aligned with low-throughput, high-fee design. Can diversify across assets; not dependent on Bitcoin for transactional utility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Allocate to Bitcoin as portfolio hedge / digital gold. Require regulatory clarity and custodial infrastructure, not transactional throughput. Benefit from narrative that Bitcoin's primary value is appreciation, not payments. Lobby for regulatory frameworks that treat Bitcoin as asset, not currency. No operational need for low fees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, institutional_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Build Layer 2 payment channels to route around on-chain fee market. Their project's viability depends on high on-chain fees creating demand for off-chain scaling. Simultaneously advocate for base-layer changes that help Lightning (e.g., taproot, anchor outputs). Professional success tied to Lightning adoption; not neutral on fee market question.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, lightning_developers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, lightning_developers, agenda_setter).

% Study Bitcoin's evolution from p2p cash to settlement layer. Document the fee market emergence, block size wars, and shifting narratives. No financial stake in outcome. Provide evidence for all readings but do not participate in governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, competition_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-spending problem for electronic cash without a trusted central party, enabling censorship-resistant peer-to-peer value transfer globally at minimal cost.
% TRANSFER_FUNCTION: Moves transaction fees from transactors (payers) to miners (beneficiaries) as the price of block space scarcity. Moves value directly between parties without intermediaries — but only for those who can afford the fee.
% ABSENT_VOICES: The unbanked and underbanked populations in developing economies who the whitepaper implicitly positions as primary beneficiaries. Small merchants and micropayment use cases priced out by fee markets. Users in authoritarian regimes who need low-fee censorship resistance but cannot pay premium fees. These voices are structurally excluded from protocol governance (no hash power, no code commit access, no funding influence).
% DISAPPEARANCE_RATIONALE: If the block size limit and resulting fee market vanished overnight, Bitcoin would revert to low-fee transactional use per the whitepaper vision. Miners would lose fee revenue (relying solely on block subsidy, which diminishes over time). The digital gold / store-of-value narrative would lose its primary structural support (fee market as proof of demand for block space). Merchant adoption would likely resurge. Competing chains offering low fees would lose their primary differentiator. The entire ecosystem would reorganize around transactional throughput.
% FOUNDING_PROBLEM: The need for a purely peer-to-peer version of electronic cash that allows online payments to be sent directly from one party to another without going through a financial institution, solving the double-spending problem without a trusted third party.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself (abstract and Section 1) explicitly frames the problem as electronic cash for direct payments. Early Bitcoin communications (Satoshi's forum posts, 2008-2010) consistently describe low fees and transactional use as core. Merchant adoption 2011-2015 demonstrates the founding problem was initially solved. The digital_gold_reading proponents (including some early developers) corroborate the shift by arguing the founding problem was 'solved enough' and the system should pivot to store of value. No independent corroboration exists that the founding problem is dead — only assertions from parties benefiting from the pivot.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the fee market's regressive impact: small transactors pay proportionally more or are excluded entirely, while large holders and institutions pay negligible fees relative to holdings. Suppression (0.78) captures the active enforcement of the block size limit through consensus rules — node operators reject larger blocks, developers resist scaling proposals, miners signal opposition. Theater ratio (0.42) reflects the gap between 'decentralization' rhetoric (node cost arguments) and the reality that fee revenue accrues to miners while the coordination function (low-fee cash) degrades. Accessibility collapse (0.58) because alternatives exist (Lightning, other chains, custodial) but each reintroduces trust or complexity the whitepaper sought to eliminate. Resistance (0.73) from the block size wars (2015-2017), ongoing scaling debates, and persistent merchant/user demand for lower fees.
 *
 * PERSPECTIVAL GAP:
 *   From the miner/large_holder/institutional seats, the fee market is a feature — it secures the network post-subsidy and signals demand. From the small_transactor/developing_economy_user seats, the same structure is a snare — it extracts from the most vulnerable and suppresses the very use case the system was built for. The agenda_setter seats (Core, node operators) genuinely believe they are protecting decentralization; the payer seats experience this as coordination cover for extraction. The engine computes this divergence from the declared power/exit/role structure — the claimed_type (tangled_rope) captures the author's structural judgment that both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners, large holders, and institutional investors sit at the beneficiary end (d ~ 0.1-0.2): they collect fees or benefit from scarcity narrative, with arbitrage-grade exit. Core developers and node operators are agenda_setters with constrained exit (professional/investment lock-in) — their d derives from structural authority over the rules. Small transactors, developing economy users, and merchants are payers with constrained or trapped exit (d ~ 0.8-0.95) — they bear fees or lose access, and cannot exit without losing Bitcoin's unique censorship resistance. Unbanked populations are excluded entirely (trapped, no voice). Lightning developers are dual-positioned: beneficiaries of the fee market (creates demand for L2) but also agenda_setters for base-layer changes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (electronic cash without trusted third parties) remains live for the victim seats but is declared 'solved' or 'superseded' by beneficiary seats. The constraint persists not because the coordination function is fulfilled, but because the beneficiaries of the fee market (miners, holders) have veto power over changes that would restore low fees. The mandatrophy is unresolved: the arrangement's original mandate (p2p cash) is actively undermined by its current operation, yet the governance structure prevents correction. This is not a piton (no theatrical maintenance — the fee market is functionally real) but a tangled_rope where the coordination function has been captured by its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_market_necessity_for_security,
    'Is a competitive fee market structurally necessary for Bitcoin''s long-term security (post-subsidy), or is it a contingent outcome of the 1MB limit that could be replaced by alternative security models?',
    'Empirical observation of chains with different fee market dynamics (BCH, BSV, Litecoin); economic modeling of miner revenue under varying block size and fee regimes; game-theoretic analysis of security without fee pressure.',
    'If fee market is necessary for security, the extraction is a coordination cost (rope-like); if contingent, the fee market is extractive overhead (snare/tangled_rope). Determines whether the p2p_cash_reading''s demand for low fees is structurally compatible with Bitcoin''s survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_necessity_for_security, empirical, 'Whether fee extraction is a necessary coordination cost or avoidable rent.').

omega_variable(
    decentralization_vs_throughput_tradeoff,
    'Do block size increases genuinely compromise decentralization (node operation cost) to a degree that undermines censorship resistance, or is the tradeoff exaggerated to protect fee revenue?',
    'Measurement of node operation costs over time (bandwidth, storage, compute) relative to household/institutional capacity; analysis of node count and geographic distribution on chains with larger blocks; assessment of whether censorship resistance correlates with small blocks.',
    'If tradeoff is real and severe, the suppression is coordination-preserving (tangled_rope); if exaggerated, suppression is extractive (snare). Core to the legitimating narrative of the agenda_setter seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_vs_throughput_tradeoff, empirical, 'Whether the decentralization argument for small blocks is structurally valid or a cover story.').

omega_variable(
    kernel_reading_frame_p2p_cash,
    'This constraint is the p2p_cash_reading of the bitcoin_whitepaper kernel. How does this reading''s structural framing differ from sibling readings, and where is the disagreement located?',
    'Compare the three readings'' beneficiary/victim sets, claimed coordination functions, and founding problem status. The disagreement is located in: (1) whether low fees are essential to the whitepaper''s vision (p2p_cash: yes; digital_gold: no; ossification: irrelevant), (2) whether block size expansion is a legitimate restoration or a dangerous change (p2p_cash: legitimate; digital_gold: unnecessary; ossification: illegitimate), (3) who the victims are (p2p_cash: fee-excluded users; digital_gold: none/holders of debased fiat; ossification: those harmed by unstable protocol).',
    'Each reading instantiates a different constraint with different ε, different victims, different type. The kernel family''s diagnostic value depends on clean separation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_p2p_cash, conceptual, 'Commitment-system framing: this reading''s structural position within the bitcoin_whitepaper kernel family.').

omega_variable(
    lightning_as_solution_vs_entrenchment,
    'Does Lightning Network genuinely solve the p2p_cash_reading''s extraction problem (enabling low-fee transactions) or does it entrench the fee market by creating a vested interest in high on-chain fees?',
    'Track Lightning adoption metrics, fee revenue split between base layer and L2, developer funding sources, and whether Lightning advocates support or oppose base-layer scaling. Observe if Lightning usage grows independently of fee spikes or only during them.',
    'If Lightning is a genuine solution, the extraction is temporary (scaffold-like); if it entrenches fee markets, the extraction is structural (tangled_rope/snare). Affects theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_as_solution_vs_entrenchment, empirical, 'Whether Layer 2 resolves or reinforces the base-layer extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_p2p_cash_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t0, observed).
narrative_ontology:measurement(btc_p2p_cash_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t3, observed).
narrative_ontology:measurement(btc_p2p_cash_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t6, observed).
narrative_ontology:measurement(btc_p2p_cash_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t9, observed).
narrative_ontology:measurement(btc_p2p_cash_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t12, observed).
narrative_ontology:measurement(btc_p2p_cash_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(btc_p2p_cash_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(btc_p2p_cash_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t0, observed).
narrative_ontology:measurement(btc_p2p_cash_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t3, observed).
narrative_ontology:measurement(btc_p2p_cash_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t6, observed).
narrative_ontology:measurement(btc_p2p_cash_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t9, observed).
narrative_ontology:measurement(btc_p2p_cash_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t12, observed).
narrative_ontology:measurement(btc_p2p_cash_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(btc_p2p_cash_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(btc_p2p_cash_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t0, observed).
narrative_ontology:measurement(btc_p2p_cash_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t3, observed).
narrative_ontology:measurement(btc_p2p_cash_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t6, observed).
narrative_ontology:measurement(btc_p2p_cash_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t9, observed).
narrative_ontology:measurement(btc_p2p_cash_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t12, observed).
narrative_ontology:measurement(btc_p2p_cash_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(btc_p2p_cash_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This story and its siblings form the bitcoin_whitepaper kernel family. The p2p_cash_reading sees the whitepaper as committing to low-fee electronic cash (ε=0.72 on current protocol). The digital_gold_reading sees the same protocol as low-extraction store-of-value (ε≈0.15). The protocol_ossification_reading sees protocol stability as the coordination function (ε≈0.05 on stability, but high on flexibility). They share the referent (Bitcoin protocol) but author different ε because they assess different standing arrangements: p2p_cash assesses the fee market; digital_gold assesses the scarcity guarantee; ossification assesses the change governance. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, organized, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, institutional, 0.1).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
