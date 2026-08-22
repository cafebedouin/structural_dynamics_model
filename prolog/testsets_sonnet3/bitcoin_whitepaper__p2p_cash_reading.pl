% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (Small-Block, Low-Fee Reading)
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the peer-to-peer electronic cash reading of the
 *   Bitcoin whitepaper kernel: Bitcoin as a censorship-resistant medium of
 *   exchange for direct transactions, prioritizing low fees and high
 *   transactional throughput, including via block size increases. Assessed by
 *   this reading's own lights, the standing arrangement — a small-block,
 *   fee-market architecture maintained since the 2017 block size wars —
 *   represents a departure from the founding design intent that concentrates
 *   payment utility among holders and layer-two operators while denying the
 *   low-value transactors and unbanked populations the whitepaper named as
 *   its purpose. This is NOT an assessment of the digital-gold reading's
 *   endorsed store-of-value arrangement (which would score near-zero
 *   extraction under its own lights) nor of the ossification reading's
 *   stability arrangement — those are separate constraints, authored
 *   separately, linked here via network edges.
 *
 * KEY AGENTS:
 *   - core_developer_coalition: agenda_setter (institutional/arbitrage) — sets block size and fee policy via commit access
 *   - large_holders_favoring_store_of_value: beneficiary (organized/arbitrage) — benefits from scarcity narrative reinforced by low throughput
 *   - layer_two_infrastructure_operators: beneficiary (organized/mobile) — business model depends on base-layer congestion
 *   - low_value_transaction_users: payer (powerless/trapped) — priced out of on-chain use by fee markets
 *   - remittance_senders_in_low_income_regions: payer (powerless/trapped) — the whitepaper's named beneficiaries, now underserved
 *   - unbanked_populations_targeted_by_original_whitepaper: excluded — no seat in governance despite being the design's stated purpose
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.38).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (Small-Block, Low-Fee Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '82a317ab-0aa1-4ed5-a084-c28ac5b243d7').
narrative_ontology:cs_kernel_codification('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', fixed_text).
narrative_ontology:cs_authority_grounding('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', practice).
narrative_ontology:cs_interpretation_layer_present('82a317ab-0aa1-4ed5-a084-c28ac5b243d7').
narrative_ontology:cs_reading_relation('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', foundational, transactional_utility_is_primary_purpose).
narrative_ontology:cs_axiom_status(transactional_utility_is_primary_purpose, holdable).
narrative_ontology:cs_axiom_grounding('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', transactional_utility_is_primary_purpose, conventional).
narrative_ontology:cs_axiom('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', secondary, block_capacity_should_scale_with_transaction_demand).
narrative_ontology:cs_axiom_status(block_capacity_should_scale_with_transaction_demand, holdable).
narrative_ontology:cs_axiom_grounding('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', block_capacity_should_scale_with_transaction_demand, instrumental).
narrative_ontology:cs_reference_frame('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', whitepaper_cash_payment_intent).
narrative_ontology:cs_drift_state('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', post_2017_block_size_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('82a317ab-0aa1-4ed5-a084-c28ac5b243d7', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, core_developer_coalition).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_holders_favoring_store_of_value).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, layer_two_infrastructure_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_in_low_income_regions).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, merchants_seeking_direct_onchain_payment).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations_targeted_by_original_whitepaper).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, small_block_node_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, peer_to_peer_electronic_cash_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, censorship_resistant_payments_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the reference client's merged-pull-request process and de facto sets the block size and fee-market parameters that determine whether the network functions as cash or as settlement layer. Frames small blocks and rising fees as necessary for decentralization and node accessibility, a framing that also protects the value of their accumulated holdings and consulting/foundation revenue tied to the current architecture. Can propose changes but has resisted block size increases since 2015, making this seat both the arrangement's author and its chief beneficiary of stasis.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developer_coalition, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold large Bitcoin positions and benefit from a narrative that treats the asset as digital gold rather than as a spendable currency; low transaction throughput and rising fees reinforce scarcity-based valuation and discourage on-chain spending that could be read as sell pressure. Faces no cost from fee-market congestion since they transact rarely and hold custody directly.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_holders_favoring_store_of_value, beneficiary,
    organized, generational, arbitrage, global).

% Operate Lightning Network nodes, custodial wallets, and exchange-based off-chain settlement that exists precisely because on-chain small-value transactions became uneconomical. Their business model is downstream of base-layer congestion; a base-layer fee reduction achieved via block size increase would reduce demand for their services.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, layer_two_infrastructure_operators, beneficiary,
    organized, biographical, mobile, global).

% Attempt to use Bitcoin for everyday purchases or small transfers as the whitepaper described, but face transaction fees that can exceed the value being sent during periods of network congestion. Effectively priced out of on-chain use, forced either to abandon Bitcoin as a payment medium or to route through custodial intermediaries that reintroduce the counterparty and censorship risk Bitcoin was built to eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users, payer,
    powerless, immediate, trapped, global).

% Represent the population the original whitepaper's cash framing most directly targeted: people without reliable banking access sending small sums across borders. Fee volatility makes Bitcoin an unreliable remittance rail compared to alternatives, despite the original design intent, because the block size ceiling was held fixed while adoption grew demand for block space.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_in_low_income_regions, payer,
    powerless, immediate, trapped, global).

% Wanted to accept Bitcoin directly as payment for goods and services, consistent with the whitepaper's 'electronic cash' framing, but unpredictable confirmation times and fees force a choice between passing volatile costs to customers, adopting custodial payment processors, or abandoning on-chain acceptance entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_seeking_direct_onchain_payment, payer,
    moderate, biographical, constrained, global).

% Named as the intended beneficiaries of a censorship-resistant peer-to-peer payment system in the original design rationale, but have no seat in the governance process that sets block size and fee policy. Their interest in low-fee, high-throughput transacting is represented, if at all, secondhand by advocacy voices outside the core development process.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations_targeted_by_original_whitepaper, excluded,
    powerless, generational, trapped, global).

% Run full nodes on consumer-grade hardware and argue that larger blocks would raise the bandwidth and storage cost of validation, concentrating node operation among fewer, wealthier operators and undermining the censorship-resistance the payment-system reading itself depends on. Their position genuinely constrains the feasible design space even though it also preserves their own low-cost participation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_block_node_operators, observer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, small_block_node_operators, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared, censorship-resistant ledger that lets any two parties transact directly without a trusted third party, verified by a decentralized network of nodes that ordinary participants can run without specialized infrastructure.
% TRANSFER_FUNCTION: Moves transaction-processing capacity (block space) from being allocated by direct use-value (payments, remittances, commerce) toward being allocated by a fee auction that favors high-value transactions and off-chain settlement operators, transferring practical payment utility away from small transactors toward holders and layer-two intermediaries.
% ABSENT_VOICES: The unbanked and remittance-dependent populations the whitepaper's abstract explicitly named as beneficiaries have no representation in the technical governance process (BIP review, mailing list consensus, mining pool signaling) that determines block size and fee policy; their interest in cheap, reliable small transactions is asserted by advocates but not held by anyone with commit access.
% DISAPPEARANCE_RATIONALE: If the current small-block, fee-market architecture disappeared and were replaced with a large-block low-fee design, layer-two infrastructure operators and holders invested in the scarcity narrative would see real value and business-model disruption; low-value transactors and remittance users would regain the payment utility the whitepaper described. Whether the 'world rearranges' depends entirely on which reading of the kernel one holds — this is the structural contest itself, not a side effect of it.
% FOUNDING_PROBLEM: The 2008 whitepaper was written to solve double-spending without a trusted third party so that direct electronic cash payments between parties could occur without a financial institution as intermediary, explicitly citing the cost and reversibility problems of the existing mediation-based payment system.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper's own text and title ('Peer-to-Peer Electronic Cash System') corroborate the founding problem from outside any present beneficiary group — Satoshi Nakamoto is not a party to current fee-policy debates. Merchant-adoption advocates and academic payment-systems researchers external to core development corroborate that the problem (cheap, direct, intermediary-free payment) remains largely unsolved at current fee levels; core developers corroborate a revised reading in which store-of-value and settlement-layer framing supersede the original text, which is itself the live dispute.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply from near-zero (2009-2013, when the network was low-usage and low-fee, cash-like in practice) to a peak during the 2017 block size wars (0.58) when the small-block position was consolidated over community objection, then settles to a moderate-high plateau (0.52) as layer-two solutions absorbed some of the payment demand while base-layer fees remained volatile and periodically prohibitive for small transactions. Theater ratio tracks similarly: the block size debate itself became substantially performative after 2017 as the technical decision had effectively been made and further 'community consensus' discourse served to legitimize a settled outcome rather than genuinely deliberate it. Suppression (governance capture via commit access and mining pool concentration, not physical coercion) rose during the contested fork period and has plateaued.
 *
 * DIRECTIONALITY LOGIC:
 *   Core developers and large holders sit at the beneficiary end: they retain control (agenda_setter) or capture upside (scarcity valuation) from the current architecture with maximal exit options (arbitrage — they can exit to other assets or influence policy directly). Low-value transactors, remittance senders, and merchants sit at the target end: trapped or constrained exit, no influence over the parameters that determine whether the system is usable for their purpose, and rising effective cost as fees escalate. Layer-two operators occupy a beneficiary position that is structurally dependent on base-layer dysfunction — their exit option (mobile) reflects that their business could relocate to other congested-base-layer models but not that they would welcome resolution of the underlying scarcity.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is warranted because a genuine coordination function persists — decentralized, trust-minimized settlement is real and valuable — but it now operates alongside an asymmetric transfer that the coordination story does not fully explain: block space scarcity was a deliberate policy choice defended as decentralization protection, and that same choice transfers value from small transactors to large holders and layer-two intermediaries. Classifying this as pure Mountain (a supposedly technically necessitated fee market) would launder a contested governance decision as physical law; classifying it as pure Snare would understate the genuine, non-manufactured contribution of decentralized settlement to censorship resistance for large-value and settlement-layer transactions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the 2008 whitepaper''s text and Satoshi''s early public statements support the p2p-cash reading as the single controlling intent, or is the text genuinely ambiguous between cash, store-of-value, and stability-first readings such that no reading can claim exclusive textual authority?',
    'Close textual analysis of the whitepaper alongside Satoshi''s forum posts and emails (particularly 2008-2011 correspondence on scalability), cross-referenced against contemporaneous community understanding before the 2015-2017 block size debates hardened into competing camps.',
    'If the text strongly and unambiguously supports the cash reading, the digital-gold and ossification readings bear a heavier burden to justify departure from founding intent, strengthening this reading''s founding_problem_status claim of ''live but abandoned.'' If the text is genuinely polysemous, all three readings have comparable textual claim and the dispute is irreducibly political rather than interpretive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the whitepaper''s original text privileges one reading over the sibling readings, or is genuinely underdetermined.').

omega_variable(
    block_size_technical_necessity,
    'Was the block size ceiling maintained since 2015 a technically necessitated response to bandwidth/storage/decentralization constraints (mountain-like), or a discretionary policy choice defended with technical language (constructed constraint)?',
    'Comparative analysis of large-block alternative chains (e.g., Bitcoin Cash, other high-throughput UTXO chains) for measured node decentralization outcomes versus theoretical predictions made during the 2017 debate; empirical bandwidth/storage cost trends versus consumer hardware cost trends over the interval.',
    'If genuinely technically necessitated at the margin actually chosen, this reading''s tangled_rope classification is more defensible as ''real constraint, real cost, real tradeoff.'' If the specific ceiling chosen was discretionary and other technically viable ceilings existed that were rejected for reasons unrelated to decentralization, the extraction is more clearly a policy choice dressed as necessity, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_technical_necessity, empirical, 'Whether the specific block size limit reflects technical necessity or discretionary policy.').

omega_variable(
    layer_two_substitutability,
    'Does the Lightning Network and other layer-two infrastructure fully substitute for base-layer small-value payment capability, such that the payer group''s loss is fully mitigated, or does it introduce new custodial/liquidity/complexity costs that constitute a distinct and partial loss?',
    'Usage and failure-mode data on Lightning Network channel liquidity, routing failures, and custodial-wallet adoption rates among populations described as unbanked or remittance-dependent.',
    'If layer-two fully substitutes, the victim classification for low_value_transaction_users should be softened; if it does not (particularly for populations without reliable internet/smartphone access needed to manage channels), the victim designation and extraction assessment stand as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer_two_substitutability, empirical, 'Whether off-chain scaling solutions mitigate or merely relocate the harm to small transactors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2019, 0.42).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2013, 0.15).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2019, 0.44).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2013, 0.1).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2017, 0.5).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2021, 0.38).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Part of the bitcoin_whitepaper constraint family (3 readings of one kernel). p2p_cash_reading (this story) evaluates the standing small-block architecture as extractive relative to founding cash-payment intent (tangled_rope). digital_gold_reading evaluates the identical standing arrangement as consistent with store-of-value purpose (expected near-zero extraction, closer to rope/mountain). protocol_ossification_reading evaluates stability itself as the governing virtue and would score departures from current parameters, not their preservation, as the extractive move. Each reading authors its own epsilon over the same underlying facts; the divergence across the three files is the data, not an error to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
