% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Bitcoin Peer-to-Peer Electronic Cash Commitment (Whitepaper p2p_cash Reading)
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   The founding text is titled 'A Peer-to-Peer Electronic Cash System,' and
 *   this story instantiates that self-description as a binding commitment:
 *   Bitcoin's defining purpose is censorship-resistant, low-friction, direct
 *   electronic payment. The standing arrangement under contest — and the sole
 *   referent of the authored epsilon — is the mainnet as it has actually
 *   operated since the 2017 settlement: capped blockspace, access allocated
 *   by fee auction, and governance oriented toward settlement-layer and
 *   reserve-asset use. Assessed by this reading's own lights, that
 *   arrangement prices small payments out of the base layer: during
 *   congestion, confirmation becomes a luxury good, remittance-sized
 *   transfers approach legacy wire costs, and the cash mandate survives
 *   mainly in niches and on second layers. Those who collect from the
 *   arrangement are miners (fee revenue rises with congestion),
 *   scarcity-oriented holders (whose premium depends on capped throughput),
 *   and layer-two operators (whose business the fee gap creates). Those who
 *   bear it are low-value transactors, remittance senders, and the
 *   micro-merchants who never onboard at all. Per the epsilon-invariance
 *   principle this is one of three structurally distinct constraints sharing
 *   the whitepaper kernel — the digital_gold_reading and
 *   protocol_ossification_reading are separate stories with their own epsilon
 *   values and victim sets, linked via network.affects_constraints. The claim
 *   and the metrics are independent authored facts: this reading claims
 *   tangled_rope because it sees a real coordination achievement wrapped
 *   around a real access toll; the engine computes per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - bitcoin_miners: primary beneficiary and co-agenda-setter (organized/arbitrage) — collects fees and subsidy, signals via hashpower, can redirect hash to rival chains
 *   - core_protocol_developers: agenda setter (institutional/identity_locked) — controls the merge process through which expansion lives or dies
 *   - scarcity_oriented_holders: beneficiary (powerful/mobile) — premium depends on capped supply, funds advocacy against expansion
 *   - low_value_transactors: primary target (powerless/constrained) — priced out at congestion
 *   - remittance_senders: target (powerless/constrained) — corridor costs spike with fee floors
 *   - node_operators: dual beneficiary/cost-bearer (organized/identity_locked) — sovereign verification vs. rising storage and bandwidth bills
 *   - lightning_routing_operators: secondary beneficiary (moderate/mobile) — earns routing fees on the gap the base layer leaves
 *   - micro_merchant_businesses: excluded payer (powerless/constrained) — would sell for small sums, has no governance seat
 *   - academic_monetary_economists: analytical observer — studies the fee market without bearing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.66).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.44).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin Peer-to-Peer Electronic Cash Commitment (Whitepaper p2p_cash Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'f50f661c-2e7d-4aba-8f58-84875100dcb1').
narrative_ontology:cs_kernel_codification('f50f661c-2e7d-4aba-8f58-84875100dcb1', fixed_text).
narrative_ontology:cs_authority_grounding('f50f661c-2e7d-4aba-8f58-84875100dcb1', lineage).
narrative_ontology:cs_interpretation_layer_present('f50f661c-2e7d-4aba-8f58-84875100dcb1').
narrative_ontology:cs_reading_relation('f50f661c-2e7d-4aba-8f58-84875100dcb1', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('f50f661c-2e7d-4aba-8f58-84875100dcb1', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('f50f661c-2e7d-4aba-8f58-84875100dcb1', foundational, electronic_cash_constitutes_bitcoin_purpose).
narrative_ontology:cs_axiom_status(electronic_cash_constitutes_bitcoin_purpose, holdable).
narrative_ontology:cs_axiom_grounding('f50f661c-2e7d-4aba-8f58-84875100dcb1', electronic_cash_constitutes_bitcoin_purpose, conventional).
narrative_ontology:cs_axiom('f50f661c-2e7d-4aba-8f58-84875100dcb1', secondary, fee_market_exclusion_defeats_censorship_resistance).
narrative_ontology:cs_axiom_status(fee_market_exclusion_defeats_censorship_resistance, holdable).
narrative_ontology:cs_axiom_grounding('f50f661c-2e7d-4aba-8f58-84875100dcb1', fee_market_exclusion_defeats_censorship_resistance, empirically_contingent).
narrative_ontology:cs_reference_frame('f50f661c-2e7d-4aba-8f58-84875100dcb1', peer_to_peer_electronic_cash_design).
narrative_ontology:cs_drift_state('f50f661c-2e7d-4aba-8f58-84875100dcb1', post_block_wars_fee_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f50f661c-2e7d-4aba-8f58-84875100dcb1', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, scarcity_oriented_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, lightning_routing_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, micro_merchant_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, node_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, proof_of_work_sybil_resistance).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, trust_minimized_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aggregate hashpower into pools, produce blocks, and collect the issuance subsidy plus whatever fees transactors attach. Fee income rises whenever demand for blockspace exceeds the cap, so congestion periods raise their revenue. They signal acceptance or rejection of protocol changes by directing hashpower, and can point that hashpower at competing chains if the economics turn against them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter).

% Maintain the consensus client, review and merge proposed changes, and set the default policy most node operators follow. Their standing rests on years of merged work and on recognition as stewards of the original design; walking away forfeits that accumulated position. Throughput-expansion proposals pass or die in their review process.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_protocol_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold large balances whose value depends on the supply cap and on the asset's monetary-premium narrative. They fund development grants, media, and advocacy, and argue against throughput increases that would dilute the scarcity story. Their balances are portable into any other asset at will.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, scarcity_oriented_holders, beneficiary,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, scarcity_oriented_holders, agenda_setter).

% Send small payments — a few dollars to a few tens of dollars — directly over the network. When demand for blockspace spikes, their transactions wait unconfirmed or the attached fee exceeds the payment itself. Their workarounds are batching, off-peak sending, second-layer channels, or abandoning direct transfer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, low_value_transactors, payer,
    powerless, immediate, constrained, global).

% Move earned wages across borders in the fifty-to-three-hundred-dollar range. On-chain transfer undercut wire services when fees were near zero; during congestion the all-in cost approaches or exceeds legacy remittance pricing, and confirmation delay adds exchange-rate risk. Their alternatives are the legacy services the network was built to displace.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, remittance_senders, payer,
    powerless, immediate, constrained, regional).

% Run full nodes to verify the ledger themselves instead of trusting anyone else's copy. Verification costs them storage and bandwidth, both of which grow with block capacity, so capacity increases raise their ongoing expenses. Many run nodes as a matter of principle and self-conception, which binds them against proposals that would raise their costs even when they sympathize with cheaper payments.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, node_operators, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, node_operators, payer).

% Operate channels and routing nodes that forward payments off the base ledger for a fee. Their business exists because base-layer settlement is slow and at times expensive; they need the base layer secure enough to anchor into and tight enough that routing earns its keep. The largest of them resemble the intermediary operators the base layer was designed to make unnecessary.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, lightning_routing_operators, beneficiary,
    moderate, biographical, mobile, global).

% Would accept direct payment for sub-dollar to few-dollar goods — tips, digital content, machine vending — but the fee floor makes such sales uneconomic, so they never onboard. They hold no seat in protocol discussions, and no one aggregates their absence into a constituency.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, micro_merchant_businesses, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, micro_merchant_businesses, excluded).

% Study the fee market, adoption corridors, and the verification budget as research subjects. They publish on who gets confirmed during congestion and on what happens to validator incentives as issuance declines. They take no side in protocol disputes and bear none of its costs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, academic_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates agreement on a single transaction history among mutually distrusting parties without a trusted intermediary, so that anyone anywhere can pay anyone else directly and no coordinator can reverse or freeze the transfer.
% TRANSFER_FUNCTION: Moves transaction fees from transactors to miners in exchange for block inclusion and finality, alongside a scheduled issuance subsidy that decays by design; beneath that, moves purchasing power directly between peers without an intermediary skim.
% ABSENT_VOICES: Micro-merchants, remittance senders, and fee-excluded users in high-inflation economies have no seat in protocol governance; decisions emerge from developer review, miner signaling, and holder-funded advocacy. Users in sanctioned or onboarding-restricted jurisdictions are doubly absent — priced out of blockspace and barred from the venues where policy is argued.
% DISAPPEARANCE_RATIONALE: If the cash commitment vanished overnight — if the network openly redefined itself as a pure settlement asset with no transactional pretense — remittance corridors built on it would collapse back to legacy rails, second-layer economies anchored to it would lose their reason for existence, censorship-evading payments would migrate to rival networks, and miners' fee expectations and holders' liquidity assumptions would reprice violently.
% FOUNDING_PROBLEM: Internet commerce relied almost entirely on trusted third parties to process electronic payments: financial institutions stood between every transaction, could reverse or block them, and imposed costs and gatekeeping on anyone outside their perimeter. The arrangement was built to make direct, irreversible, permissionless electronic payment possible.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper's own first section attests the founding problem. Independent corroboration from outside the benefiting parties: payment-systems and financial-inclusion research documents that cross-border and small-value rails remain costly and censorable for billions of people, and press-freedom and humanitarian organizations document transactional censorship against dissidents and aid recipients. No party outside the holder-and-miner beneficiary coalition attests that the problem is dead; the 'solved or superseded' verdict originates inside that coalition and is therefore treated as contested rather than corroborated.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.66 at interval end) because the fee floor during congestion routinely exceeds the cost of legacy rails for small sums — the December 2017 and 2021 spikes, and the 2023-24 inscription congestion, each pushed sub-hundred-dollar payments off the base layer. Suppression is moderate (0.44) and mostly social rather than technical: exit was always possible (the 2017 fork proved it), but within-governance dissent carried heavy costs during the block wars — forum moderation, funding pressure, and ostracism of expansion advocates — before normalizing into defaults after the split removed the internal opposition. Theater is low-moderate (0.26): corridor use, humanitarian disbursement, and layered payment volume are real cash function, while state-level adoption ceremonies and tourist demonstrations inflate the visible cash story beyond the base layer's actual inclusiveness. Accessibility collapse is moderate-low (0.42): alternatives persist (rival chains, the 2017 fork, fiat rails, second layers) but each carries a network-effect penalty that keeps most users on the dominant chain. Resistance is high (0.72): a decade of open block-size warfare, a user-activated soft-fork threat, and a permanent schism. Temporally, fee pressure oscillates on roughly four-year halving and market cycles; the shared grid samples cycle endpoints, so the series traces the rising trough-floor of each cycle rather than intra-cycle spikes — a finer grid would show sawtooth oscillation around the monotonic trend, and each congestion wave functions partly as an extraction mechanism by normalizing a higher fee floor before the next trough. The suppression_requirement series tracks enforcement-capacity specifically: it builds through the 2015-2018 governance war and decays afterward as enforcement became routine default-following rather than active combat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the collector seats should compute very differently from identical protocol facts. From the low_value_transactor and remittance_sender positions the arrangement operates as enforced exclusion — a toll booth on money itself — and their constrained exit amplifies effective extraction toward the snare end. From the miner and holder positions the same arrangement is a functioning market they built and defend, with extraction damped toward subsidy. The developer seat experiences stewardship, not collection. The engine derives these divergences from the declared roles and exit options; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners sit nearest the beneficiary pole (declared beneficiary, arbitrage-grade exit — hashpower is redeployable across chains), holders nearly as far (beneficiary, mobile exit), layer-two operators somewhat closer to symmetric (they earn from the gap but depend on base-layer health). Transactors and remitters sit near the full-target pole: declared victims with constrained exit and immediate horizons, so effective extraction is amplified for them. Node operators are genuinely dual-positioned — they collect verification sovereignty while paying capacity-dependent costs — placing them near the middle, with identity lock pulling their effective position toward defense of the status quo. The economist seat is analytical and contributes no directional pull.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — internet commerce's dependence on trusted, censorable intermediaries — remains live for censored populations, sanctioned jurisdictions, and the underbanked, so the mandate has not outlived its function and mandatrophy is not resolved. The classification guards against mislabeling in both directions: a pure-snare verdict would erase the genuine coordination achievement (trust-minimized, uncensorable final settlement that no incumbent rail provides), while a pure-rope verdict would erase the identifiable class of people the fee market excludes. Tangled_rope preserves both facts. The trajectory hinges on the open omegas: if layered scaling restores inclusive access non-custodially, the arrangement drifts toward rope; if subsidy decay forces irreversible fee escalation, it drifts toward snare; if the cash function fully atrophies into ceremony while the fee machinery persists, the piton signature becomes the live hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the bitcoin_whitepaper kernel (p2p_cash_reading); which sibling reading governs the protocol''s operative trade-offs — cash access, scarcity, or change-stability?',
    'Observed governance outcomes: throughput and blockspace decisions, fee-floor behavior, and which coalition supplies merge reviewers and hashpower backing; a sustained reversal (expansion proposals passing with covenant support) would resolve toward this reading.',
    'If the digital_gold reading fully prevails, the victim set shifts from fee-excluded transactors toward would-be spenders facing frozen throughput; if this reading prevails, scarcity-oriented holders'' premium narrative bears the adjustment cost; the standing arrangement''s classification swings between tangled_rope and snare-flavored profiles accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which sibling reading of the whitepaper kernel sets protocol priorities.').

omega_variable(
    lightning_sufficiency,
    'Does second-layer routing restore the cash function sufficiently that on-chain fee-market exclusion stops counting as denial of transactional access?',
    'Longitudinal layer-two metrics: channel capacity distribution, routing success rates for small payments, custody concentration share, and end-to-end cost of a five-dollar payment compared with early-era on-chain cost.',
    'If layers suffice non-custodially, effective extraction falls and the arrangement trends toward coordination-with-relief; if routing concentrates in a few large hubs behind custodial entry points, the exclusion is relocated rather than removed and the measured extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_sufficiency, empirical, 'Whether layered scaling genuinely restores inclusive transactional access.').

omega_variable(
    security_budget_fee_transition,
    'As the issuance subsidy halves toward zero, will fee escalation borne by transactors intensify irreversibly, or can throughput expansion reconcile access with the verification budget?',
    'Compare observed fee floors per halving epoch against subsidy-decay schedules; test whether fee floors rise faster than efficiency gains; watch whether expansion proposals regain traction as the subsidy shrinks.',
    'Irreversible fee escalation pushes the arrangement toward snare-flavored extraction from transactors; reconcilable security economics would support the tangled_rope reading with declining extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_fee_transition, empirical, 'Whether the fee market''s burden on transactors deepens structurally with subsidy decay.').

omega_variable(
    fee_market_framing,
    'Is fee-market allocation of blockspace neutral price discovery among equal bidders, or a structural rationing of access that falls hardest on small payments?',
    'Distributional analysis of confirmation outcomes during congestion: fee-rate distributions by transaction-size class, replacement behavior, and whether small transactions clear at any reasonable fee.',
    'Under the price-discovery frame the same fee data reads as coordination cost and lowers culpable extraction; under the rationing frame it reads as access denial and raises it — the classification differs by frame with identical data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fee_market_framing, conceptual, 'Framing ambiguity: rationing-by-price versus neutral auction for blockspace.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.06).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t9, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 14, 0.23).
narrative_ontology:measurement_basis(bitc_tr_t14, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.07).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(bitc_be_t9, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 14, 0.63).
narrative_ontology:measurement_basis(bitc_be_t14, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(bitc_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.12).
narrative_ontology:measurement_basis(bitc_su_t3, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement_basis(bitc_su_t9, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 14, 0.47).
narrative_ontology:measurement_basis(bitc_su_t14, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement_basis(bitc_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin' conflates at least three structurally distinct constraints sharing one kernel text: this cash-access reading (substantial epsilon, victim set = fee-excluded transactors), the digital-gold reading (different epsilon, victim set = would-be spenders under frozen throughput), and the protocol-ossification reading (a governance doctrine with its own beneficiary and cost-bearing structure). Each is authored separately per the epsilon-invariance principle and linked here. Causal texture: the shared whitepaper text feeds all three; this reading's 2017 defeat transferred governance weight to the gold reading, whose ascendancy raised the fee floors that keep this reading's grievance alive — the siblings are upstream and downstream of one another through the fee market itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
