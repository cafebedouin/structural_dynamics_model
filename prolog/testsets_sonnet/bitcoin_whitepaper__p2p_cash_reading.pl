% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (Satoshi/Small-Block-Descendant Reading)
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint isolates the peer-to-peer electronic cash reading of the
 *   Bitcoin whitepaper's kernel claim: that Bitcoin functions as
 *   censorship-resistant money for direct transactions between parties, which
 *   structurally implies that keeping transaction fees low and block capacity
 *   adequate to demand is not merely a technical preference but the
 *   constraint's core legitimacy condition. This reading was the operative
 *   interpretation from Bitcoin's 2009 launch through roughly the 2015-2017
 *   block-size contentious-fork period, after which the dominant chain's
 *   governing coalition settled into a different reading
 *   (digital_gold_reading, favoring fixed small blocks and off-chain scaling)
 *   while a minority chain preserved the cash-first premise. The measurements
 *   track the widening gap between this reading's promise (cheap universal
 *   transacting) and its achieved state on the chain(s) that still claim to
 *   instantiate it, as demand periodically outstripped whatever capacity
 *   commitments were in force. This is a genuinely separate constraint from
 *   digital_gold_reading: the two readings have different beneficiary/victim
 *   sets (large-block miners and remittance users here vs. long-term holders
 *   and base-layer security purists there), different claimed types, and
 *   different ε trajectories — they are not the same constraint measured two
 *   ways.
 *
 * KEY AGENTS:
 *   - large_block_miners: agenda-setting beneficiaries who campaign for capacity expansion
 *   - onchain_scaling_developers: agenda-setters maintaining the cash-first client implementations
 *   - small_transaction_users and unbanked_remittance_senders: powerless payers, the demographic the whitepaper names as intended beneficiaries but who bear the cost when the reading's promise is not met
 *   - store_of_value_holders: excluded from this constraint's operating logic but central to the sibling reading's conflict with it
 *   - protocol_researchers: analytical observers tracking empirical fee-market outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.42).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.31).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (Satoshi/Small-Block-Descendant Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '7c18c037-9ec6-4ed2-a209-403c0f860b21').
narrative_ontology:cs_kernel_codification('7c18c037-9ec6-4ed2-a209-403c0f860b21', fixed_text).
narrative_ontology:cs_authority_grounding('7c18c037-9ec6-4ed2-a209-403c0f860b21', distributed).
narrative_ontology:cs_reading_relation('7c18c037-9ec6-4ed2-a209-403c0f860b21', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c18c037-9ec6-4ed2-a209-403c0f860b21', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('7c18c037-9ec6-4ed2-a209-403c0f860b21', foundational, capacity_must_scale_with_transactional_demand).
narrative_ontology:cs_axiom_status(capacity_must_scale_with_transactional_demand, holdable).
narrative_ontology:cs_axiom_grounding('7c18c037-9ec6-4ed2-a209-403c0f860b21', capacity_must_scale_with_transactional_demand, instrumental).
narrative_ontology:cs_axiom('7c18c037-9ec6-4ed2-a209-403c0f860b21', foundational, low_fee_accessibility_is_the_primary_success_criterion).
narrative_ontology:cs_axiom_status(low_fee_accessibility_is_the_primary_success_criterion, holdable).
narrative_ontology:cs_axiom_grounding('7c18c037-9ec6-4ed2-a209-403c0f860b21', low_fee_accessibility_is_the_primary_success_criterion, empirically_contingent).
narrative_ontology:cs_reference_frame('7c18c037-9ec6-4ed2-a209-403c0f860b21', genesis_block_direct_payment_vision).
narrative_ontology:cs_drift_state('7c18c037-9ec6-4ed2-a209-403c0f860b21', post_2017_fee_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c18c037-9ec6-4ed2-a209-403c0f860b21', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_block_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, merchant_payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, early_low_fee_users).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, onchain_scaling_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, users_priced_out_by_fee_markets).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, competing_small_block_chain_holders).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, electronic_cash_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, low_fee_accessibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mining pools and hardware operators who advocate raising the block size limit so more transactions clear per block, keeping fees low and volume high. They set the practical agenda on which software forks get run and campaigned for on-chain scaling forks. They can redeploy hashpower to whichever chain rewards them, so their exit is comparatively cheap.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_block_miners, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, large_block_miners, beneficiary).

% Maintain client software implementing the cash-first reading, argue block size limits should scale with demand, and control which changes ship in their fork. Their credibility and funding depend on Bitcoin (or a Bitcoin-descended chain) functioning as everyday payment rail rather than settlement-only asset.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, onchain_scaling_developers, agenda_setter,
    organized, generational, mobile, global).

% Businesses that accept on-chain payments for goods and services depend on transaction fees staying low enough that small purchases remain economical. They lobby publicly for capacity increases and route volume toward whichever implementation keeps fees down; if fees rise structurally, their entire business model of accepting direct on-chain payment collapses.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchant_payment_processors, beneficiary,
    moderate, biographical, constrained, global).

% Individuals who want to send small-value payments peer-to-peer without intermediaries. When block space is scarce and fee markets dominate, a fee that once cost cents can exceed the value of the transaction itself, pricing them out of the exact use case the whitepaper describes. Their only recourse is off-chain layers (custodial or semi-custodial) that reintroduce the intermediaries the constraint was meant to eliminate, or abandoning the base chain entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_transaction_users, payer,
    powerless, immediate, constrained, global).

% People without bank access who were promised censorship-resistant electronic cash as an alternative to costly remittance corridors. Fee volatility during network congestion periods has repeatedly made on-chain remittance more expensive than the traditional services it was supposed to disrupt, with no practical fallback since they lack banking access in the first place.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_senders, payer,
    powerless, immediate, trapped, global).

% A broader class of would-be users who evaluated Bitcoin against its stated purpose (direct electronic cash) and found the achieved fee environment incompatible with that purpose during periods of high demand, so they transact elsewhere or not at all.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, users_priced_out_by_fee_markets, payer,
    powerless, immediate, constrained, global).

% Holders and developers of the chain that retained the original small block size and rejected the on-chain scaling premise; they bear reputational and market-value costs from being cast as having 'abandoned' the cash use case, and their chain's market share was structurally disadvantaged once the dominant chain's community and liquidity consolidated around the alternate reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, competing_small_block_chain_holders, payer,
    moderate, biographical, mobile, global).

% Long-term holders who prioritize base-layer security and scarcity over transactional throughput are structurally opposed to this reading's block-size-expansion agenda, since larger blocks raise validation costs and could weaken decentralization guarantees they value. Their objections are visible in the sibling digital-gold reading but are not part of this constraint's own operating logic.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, store_of_value_holders, excluded,
    organized, generational, mobile, global).

% Academic and independent researchers who study the empirical fee-market and transaction-throughput data without holding a stake in either the payments or store-of-value camps, publishing analyses of who is actually priced out and when.
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
% COORDINATION_FUNCTION: Coordinates a payment network in which any two parties can transact directly without a trusted third party, by keeping block capacity sufficient that ordinary transactions clear cheaply and quickly — solving the genuine problem of intermediary-dependent, censorable payment rails.
% TRANSFER_FUNCTION: Moves transaction-processing capacity and fee revenue: when block capacity is constrained relative to demand, value transfers from would-be low-value transactors (priced out or forced into fee auctions) to those who can afford fees, to entities offering off-chain alternatives, and to whichever developer/miner coalition controls the capacity-setting decision.
% ABSENT_VOICES: Unbanked remittance users and small-transaction users in the developing world are the demographic most directly invoked in the whitepaper's stated purpose, but they have no seat in the governance conversations (developer mailing lists, mining pool coordination, protocol conferences) that set block size and fee policy.
% DISAPPEARANCE_RATIONALE: If this reading's practical claim (low fees, expanded capacity as legitimate goal) disappeared as an organizing principle, on-chain small-value payments would become structurally unavailable and off-chain/custodial layers would absorb that demand — a real rearrangement for payment-use-case participants. But holders oriented to the sibling digital-gold reading would see no change at all, since their use case does not depend on this reading prevailing. The verdict is genuinely contested because the two readings' constituencies experience disappearance completely differently.
% FOUNDING_PROBLEM: The 2008 whitepaper was written to solve unauthorized reversal and third-party trust dependency in electronic payments — enabling any two willing parties to transact directly without relying on a financial institution as intermediary.
% FOUNDING_PROBLEM_CORROBORATION: Independent protocol researchers and academic economists studying on-chain fee data corroborate that the small-payment use case has become empirically unreliable during demand spikes, supporting the claim that the founding problem (intermediary-free payment) is no longer straightforwardly solved at the base layer for small transactions. Onchain-scaling developers and merchant processors, who benefit from this reading prevailing, also attest the problem is live and worsening under the rival reading's dominance — but their attestation alone would be self-serving; the researcher corroboration is the material outside-party evidence.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).
:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme because when this reading's capacity commitments hold, the coordination function genuinely operates — cheap peer-to-peer transacting is real and delivered to many users much of the time. But extraction rises during demand-congestion periods when the same infrastructure that promises universal access becomes a fee auction that structurally excludes the lowest-value transactions it was designed to serve, transferring value to those able to pay fee premiums and to off-chain intermediary services that reintroduce exactly the trust dependency the whitepaper sought to eliminate. Suppression is moderate (0.31): there is no single actor coercively blocking alternatives, but network effects, liquidity concentration, and social/ideological pressure within the community make exit to alternative chains costly and reputation-laden, which functions as soft suppression. Theater ratio (0.38) reflects that a meaningful share of continued advocacy for this reading (repeated announcements of layer-2 fixes, promises that scaling is 'coming') has drifted toward reassurance rhetoric relative to delivered base-layer capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats (large-block miners, onchain-scaling developers), this operates as functioning coordination they built and maintain in good faith against resistance from a rival faction. From the payer seats (small-transaction users, unbanked remittance senders), it operates as a promise that periodically fails exactly when they need it, with no institutional recourse — the engine's per-seat computation should register this divergence structurally rather than resolve it toward either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Large-block miners and onchain-scaling developers are near the beneficiary end: they set policy, capture continued relevance/revenue from the payments use case succeeding, and have mobile exit if their preferred chain underperforms. Merchant processors are beneficiaries whose business model depends on the reading holding. Small transaction users, unbanked remittance senders, and fee-priced-out users are targets: they bear the direct cost of capacity shortfalls with constrained or trapped exit (the remittance-dependent population in particular lacks banking alternatives, which is precisely why d should sit near the full-target end for that group even though nominal 'exit' to another payment method nominally exists — the alternative is the costly system Bitcoin was meant to replace).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trust-free direct payment) is contested rather than cleanly live or dead: it remains genuinely solved for users transacting during low-congestion periods, but empirically fails during exactly the demand spikes when the promise matters most, which prevents either a clean 'still functioning as coordination' or 'fully mandatrophied into extraction' verdict. Classifying this as tangled_rope rather than snare or rope captures that both a real coordination function and asymmetric extraction coexist and require active defense (continued advocacy, forking, marketing) to sustain — collapsing it to either pure category would mislabel either the genuine transactions that succeed or the users structurally failed by the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cash_vs_gold_reading_primacy,
    'Which reading of the Bitcoin whitepaper — payments medium or store-of-value asset — represents the ''true'' or founding intent of the system, and does that question even have a determinate answer given the whitepaper''s own ambiguous framing (it invokes both ''electronic cash'' and monetary-scarcity arguments)?',
    'Textual and historical analysis of Satoshi Nakamoto''s early writings, mailing list posts, and the whitepaper''s own emphasis, weighed against which reading the surviving dominant chain''s community and market capitalization have converged upon — though convergence is a sociological fact, not necessarily a resolution of authorial intent.',
    'If the payments reading is judged the authoritative one, the dominant chain''s current fee/capacity policy is a departure from founding purpose (supporting a mandatrophy or drift narrative); if the store-of-value reading is judged authoritative or co-equal, the current policy is a legitimate evolution rather than a broken promise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cash_vs_gold_reading_primacy, conceptual, 'Whether the whitepaper has one determinate founding intent or genuinely supports multiple co-equal readings.').

omega_variable(
    layer_two_adequacy,
    'Do off-chain scaling solutions (payment channels, sidechains) fully substitute for base-layer capacity in delivering the direct-transaction promise, or do they reintroduce enough custodial/trust dependency that the whitepaper''s core claim (no trusted third party) is compromised?',
    'Empirical study of what fraction of layer-2 payment volume flows through custodial versus genuinely non-custodial channels, and whether unbanked/remittance populations in practice access non-custodial layer-2 tools or default to custodial services.',
    'If layer-2 substitution is largely non-custodial and effective, extraction from small-transaction users is lower than the base-layer fee data alone suggests. If substitution is largely custodial, the measured extraction understates the true cost, since users are pushed back toward the intermediary-dependent model the constraint claims to eliminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_adequacy, empirical, 'Whether off-chain layers preserve or undermine the trust-minimization promise for the population priced off the base chain.').

omega_variable(
    which_chain_instantiates_this_reading,
    'After the 2017 fork, does this constraint''s p2p_cash_reading still apply to the dominant chain (via layer-2 development) or does it more accurately describe the minority chain that retained expanded block sizes, or does it apply to neither in its original form?',
    'Track on-chain fee and throughput data across both chains over time relative to demand, and assess which chain''s governing coalition actually still holds capacity-expansion as an operative policy commitment versus rhetorical legacy.',
    'If the reading is best instantiated by the minority chain, this constraint''s stakeholder power/scope figures (built around the historically dominant chain''s scale) would need adjustment; if neither chain cleanly instantiates it, the reading may itself be better modeled as effectively dead rather than contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_chain_instantiates_this_reading, empirical, 'Ambiguity in which existing blockchain, if any, is the live instantiation of this reading after the community split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t20, observed).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t40, observed).
narrative_ontology:measurement(bitc_tr_t60, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t60, observed).
narrative_ontology:measurement(bitc_tr_t80, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement_basis(bitc_tr_t80, observed).
narrative_ontology:measurement(bitc_tr_t100, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(bitc_be_t20, observed).
narrative_ontology:measurement(bitc_be_t40, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(bitc_be_t40, observed).
narrative_ontology:measurement(bitc_be_t60, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(bitc_be_t60, observed).
narrative_ontology:measurement(bitc_be_t80, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement_basis(bitc_be_t80, observed).
narrative_ontology:measurement(bitc_be_t100, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(bitc_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(bitc_su_t20, observed).
narrative_ontology:measurement(bitc_su_t40, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(bitc_su_t40, observed).
narrative_ontology:measurement(bitc_su_t60, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement_basis(bitc_su_t60, observed).
narrative_ontology:measurement(bitc_su_t80, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(bitc_su_t80, observed).
narrative_ontology:measurement(bitc_su_t100, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 100, 0.31).
narrative_ontology:measurement_basis(bitc_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Part of the bitcoin_whitepaper kernel family (3 readings). This story (p2p_cash_reading) shares the founding text with digital_gold_reading and protocol_ossification_reading but instantiates a structurally distinct constraint: different beneficiary/victim sets, different claimed type (tangled_rope here vs. likely rope/mountain framing for digital_gold_reading's scarcity claim, vs. a governance-procedural framing for protocol_ossification_reading), and a different ε trajectory driven by fee-market dynamics rather than monetary-scarcity dynamics or governance-consensus dynamics. All three link to each other because a change in one reading's dominance (e.g. a successful block-size fork) directly alters resource availability and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
