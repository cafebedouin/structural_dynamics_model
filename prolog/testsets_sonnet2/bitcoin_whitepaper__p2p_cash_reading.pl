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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (P2P Cash Reading)
 *   domain: cryptocurrency economics/monetary systems/technology governance
 *
 * SUMMARY:
 *   This story instantiates the peer-to-peer electronic cash reading of the
 *   Bitcoin whitepaper kernel: Bitcoin as a censorship-resistant medium of
 *   exchange for direct, low-fee transactions between parties without
 *   financial intermediaries. This is a distinct constraint from the
 *   digital_gold_reading (which optimizes for scarcity and store-of-value,
 *   treating high fees as acceptable friction on a settlement layer) and from
 *   the protocol_ossification_reading (which treats consensus stability
 *   itself as the primary virtue, largely independent of either monetary
 *   function). Under this reading, block size expansion is legitimate and
 *   even obligatory fidelity to the founding text; the victim set
 *   specifically includes those denied cheap transactional access whenever
 *   fee markets tighten, because that outcome directly negates the reading's
 *   own definition of success. The three readings share a persisting kernel —
 *   the whitepaper and the protocol it specifies — but diverge sharply on
 *   what fidelity to that kernel requires, which is why they are authored as
 *   separate constraints with separate epsilon values rather than one story
 *   with a parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (P2P Cash Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency economics/monetary systems/technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '8f030528-29aa-4b5b-a353-d51246f1114f').
narrative_ontology:cs_kernel_codification('8f030528-29aa-4b5b-a353-d51246f1114f', fixed_text).
narrative_ontology:cs_authority_grounding('8f030528-29aa-4b5b-a353-d51246f1114f', distributed).
narrative_ontology:cs_reading_relation('8f030528-29aa-4b5b-a353-d51246f1114f', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f030528-29aa-4b5b-a353-d51246f1114f', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('8f030528-29aa-4b5b-a353-d51246f1114f', foundational, transactional_utility_is_primary_success_condition).
narrative_ontology:cs_axiom_status(transactional_utility_is_primary_success_condition, holdable).
narrative_ontology:cs_axiom_grounding('8f030528-29aa-4b5b-a353-d51246f1114f', transactional_utility_is_primary_success_condition, instrumental).
narrative_ontology:cs_axiom('8f030528-29aa-4b5b-a353-d51246f1114f', foundational, base_layer_capacity_expansion_is_legitimate_evolution).
narrative_ontology:cs_axiom_status(base_layer_capacity_expansion_is_legitimate_evolution, holdable).
narrative_ontology:cs_axiom_grounding('8f030528-29aa-4b5b-a353-d51246f1114f', base_layer_capacity_expansion_is_legitimate_evolution, conventional).
narrative_ontology:cs_reference_frame('8f030528-29aa-4b5b-a353-d51246f1114f', original_whitepaper_cash_specification).
narrative_ontology:cs_drift_state('8f030528-29aa-4b5b-a353-d51246f1114f', post_blocksize_wars_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f030528-29aa-4b5b-a353-d51246f1114f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_block_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, merchant_payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, early_low_fee_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, onchain_scaling_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, fee_priced_out_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_node_operators_under_bloat).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, satoshi_whitepaper_title_claim).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, electronic_cash_system_design_intent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate mining pools that profit from higher transaction throughput (more fees collected in aggregate, even at lower per-transaction rates) and from block size increases that let them process more volume. Advocate for larger blocks as fidelity to the original design; can redirect hashpower to whichever chain implements their preferred parameters.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_block_miners, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, large_block_miners, agenda_setter).

% Built businesses on the premise of Bitcoin as a cheap, fast payment rail for everyday commerce. Depend on low, predictable fees and confirmation times to serve point-of-sale and remittance customers; a fee market that prices out small transactions undermines their entire business model, so they lobby hard for on-chain scaling.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchant_payment_processors, beneficiary,
    moderate, biographical, constrained, global).

% Users and developers who joined when fees were negligible and transactions were the primary use case discussed in community forums. Benefit from any restoration of that regime and treat the cash-like function as the constraint's founding legitimacy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, early_low_fee_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Maintain alternative client implementations (and historically forked the chain) to push block size limits upward. Set the technical agenda for this reading by writing and promoting the code that would realize a P2P-cash-optimized protocol, competing directly with the small-block maintainers of the sibling readings.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, onchain_scaling_developers, agenda_setter,
    organized, biographical, mobile, global).

% Want to send small everyday payments on-chain. When the fee market prices out sub-dollar transactions during periods of network congestion, they are structurally excluded from the base-layer use case the whitepaper title promises them, and are pushed toward custodial or off-chain alternatives that reintroduce the intermediaries Bitcoin was meant to remove.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users, payer,
    powerless, immediate, constrained, global).

% Rely on low fixed costs to make cross-border transfers of amounts too small to justify traditional remittance-service cuts. High and volatile on-chain fees erase the value proposition that made Bitcoin attractive to them in the first place, and they typically lack the technical means or capital to use layered scaling solutions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_senders, payer,
    powerless, immediate, trapped, global).

% Attempted on-chain transactions during congestion and either paid fees exceeding the transaction's value or had transactions stuck unconfirmed for days. Their experience directly contradicts the censorship-resistant medium-of-exchange claim, since exclusion by fee is functionally similar to exclusion by censorship even though no single party blocked them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, fee_priced_out_users, payer,
    powerless, immediate, constrained, global).

% Run full nodes to independently validate the chain and support decentralization. Larger blocks increase bandwidth, storage, and sync-time costs, gradually squeezing out operators without institutional-grade infrastructure and centralizing validation among well-resourced parties — undermining the decentralization the cash-use-case reading also depends on for its censorship-resistance claim.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_node_operators_under_bloat, payer,
    moderate, biographical, constrained, global).

% Hold the competing digital_gold_reading and protocol_ossification_reading of the same kernel. Within THIS reading's community and governance venues their preference for small blocks and fee-market formation is treated as a mistaken or hostile position rather than a legitimate alternative interpretation, so their objection is heard mainly in separate venues (their own forks and forums), not inside this reading's decision process.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, smallblock_store_of_value_advocates, excluded,
    organized, generational, mobile, global).

% Study empirical fee data, congestion events, and throughput trade-offs without a financial stake in either scaling camp. Document that the whitepaper's original design underspecified the block-size parameter, leaving the P2P-cash and store-of-value readings to fight over what 'faithful' implementation means.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, protocol_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared payment network where participants can transact directly without a trusted third party, using on-chain capacity sized to keep per-transaction fees low enough for everyday and small-value payments.
% TRANSFER_FUNCTION: Moves transactional utility and low-fee access from users who need cheap small payments to those parties (miners, scaling-oriented developers, merchant processors) who benefit from higher throughput and continued on-chain relevance; when capacity is constrained, moves confirmation priority from low-fee payers to high-fee payers via the auction-like fee market.
% ABSENT_VOICES: Advocates of the digital_gold_reading and protocol_ossification_reading are structurally present in the broader Bitcoin ecosystem but excluded from THIS reading's own legitimating narrative — their preference for small blocks and settlement-layer conservatism is treated within this reading's discourse as an abandonment of the whitepaper's stated purpose rather than a coequal interpretation.
% DISAPPEARANCE_RATIONALE: If the P2P-cash framing and the coordination behind it (on-chain scaling advocacy, merchant-processor lobbying, low-fee-optimized client development) disappeared, the ecosystem would consolidate more fully around the settlement/store-of-value framing, on-chain fee markets would harden without on-chain-scaling counter-pressure, and low-value on-chain payments would effectively cease to be viable, pushing that use case entirely to custodial or layered alternatives.
% FOUNDING_PROBLEM: The original whitepaper described a system enabling 'online payments to be sent directly from one party to another without going through a financial institution,' explicitly framed around electronic cash for transactions, including small ones.
% FOUNDING_PROBLEM_CORROBORATION: Merchant processors and on-chain scaling developers attest the cash-payment problem remains live and central. Independent protocol researchers and fee-market data analysts (outside both the beneficiary camp and the rival digital-gold camp) corroborate that base-layer capacity has in practice been insufficient for low-value transactions during congestion, supporting the contested status rather than settling it in either camp's favor.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.58 because under sustained congestion, the fee market this reading opposes (but which persists on the actual chain enforcing this constraint) transfers value from powerless low-value senders to whichever party can pay for priority confirmation — a real transfer, not merely foregone benefit. Suppression is moderate (0.42): no single party censors a transaction, but the auction-like fee mechanism functions as an economic suppression of small transactions, which is structurally similar to exclusion even without an excluding actor. Theater ratio (0.4) reflects that a meaningful share of on-chain activity and blockspace competition, especially during congestion, is inscribed transaction volume unrelated to peer-to-peer payment (e.g., data inscription, MEV-adjacent activity) rather than the cash use case the reading claims to serve. Accessibility collapse is moderate (0.5): off-chain and custodial alternatives exist, so the collapse is not total, but they reintroduce exactly the intermediaries the reading's premise rejects. Resistance is high (0.72) because this reading is actively and continuously contested within the Bitcoin ecosystem itself by adherents of the sibling readings, not merely by external critics.
 *
 * PERSPECTIVAL GAP:
 *   From the on-chain-scaling agenda-setter seat, the constraint looks like coordination toward the founding purpose, temporarily undermined by insufficient capacity that could be fixed with larger blocks. From the fee-priced-out payer seat, the same underlying protocol looks like an enforced auction that excludes exactly the low-value transactions the reading promises to serve — the engine should compute these as different effective classifications from the same structural data, which is the point of not tuning the claim to the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners, scaling developers, and merchant processors are declared beneficiaries because their business models and technical agendas depend on continued relevance of on-chain, high-throughput, low-fee transacting — they collect either fee revenue at volume or the value of a functioning cheap payment rail. Low-value users, remittance senders, and fee-priced-out users are declared victims because the reading's own success condition (cheap, direct transactions) fails precisely for them when the fee market tightens; they bear the cost of capacity constraints the reading argues should not exist. Small node operators sit as payers on a different axis: they bear the decentralization cost of the very block-size increases the reading's beneficiaries favor, producing an internal tension even among ostensible reading-adherents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cheap, direct electronic cash transactions) is contested rather than resolved: it remains technically live in the sense that demand for cheap on-chain payments persists, but whether the current protocol trajectory still serves it is disputed by the sibling readings, who argue settlement-layer conservatism is the more faithful reading. This story avoids conflating 'the sibling readings are simply wrong' with 'the founding problem is dead' — it is authored as contested precisely because both outcomes remain live possibilities depending on which reading's governance preferences prevail.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Does the original whitepaper''s design intent authorize this reading''s low-fee, high-throughput priority, or does it equally support the sibling readings'' emphasis on scarcity or consensus stability?',
    'Close textual and historical analysis of the whitepaper alongside satoshi''s early mailing-list and forum statements; comparison against the actual technical choices made in the original client versus later divergent implementations.',
    'If historical evidence strongly favors one reading, that reading''s claim to fidelity strengthens relative to the others, shifting legitimacy (though not epsilon, which stays reading-indexed) across the constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the founding text privileges this reading over its siblings.').

omega_variable(
    fee_market_as_censorship_equivalence,
    'Is economic exclusion via fee markets structurally equivalent to censorship for purposes of this reading''s own censorship-resistance claim, or is it a categorically different phenomenon that the reading''s framing conflates?',
    'Comparative analysis of exclusion outcomes: track whether low-value users excluded by fees experience functionally similar loss of access as users excluded by direct censorship (e.g., transaction blacklisting), across multiple congestion events.',
    'If fee-based exclusion is functionally equivalent to censorship, the reading''s own success metric is compromised by its most severe congestion episodes, sharpening the victim classification; if categorically distinct, the extractiveness score may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_as_censorship_equivalence, conceptual, 'Whether pricing out of small transactions counts as a failure of the reading''s own censorship-resistance claim.').

omega_variable(
    block_size_decentralization_tradeoff,
    'At what block size does the throughput gain for cash-like usage begin to erode decentralization enough to undermine the censorship resistance the reading also depends upon?',
    'Empirical study of node operation costs, geographic node distribution, and mining centralization at varying historical and hypothetical block sizes.',
    'If the tradeoff point is low, this reading''s core prescription (expand block size) is self-undermining past a threshold; if high, the reading''s beneficiaries have more room before the tradeoff bites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_decentralization_tradeoff, empirical, 'Whether scaling for cash use undermines the decentralization the reading also requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 16, 0.4).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.28).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 16, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Bitcoin whitepaper' / 'what Bitcoin is for.' digital_gold_reading and protocol_ossification_reading are siblings sharing the same kernel (bitcoin_whitepaper) but instantiating structurally distinct claims with different beneficiary/victim sets and different epsilon values. p2p_cash_reading (this story) authors moderate-high extraction concentrated on low-value/unbanked users excluded by fee markets; digital_gold_reading would author low extraction from a store-of-value success frame (fee friction is a feature, not a victim-generating cost); protocol_ossification_reading would author extraction concentrated on those who want protocol evolution and are blocked by consensus-conservatism. All three are linked via affects_constraints rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
