% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Electronic Cash Reading: Binding On-Chain Capacity Mandate
 *   domain: distributed systems/monetary theory/technology governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the bitcoin_whitepaper_purpose
 *   kernel: the electronic_cash_reading, under which the whitepaper title's
 *   'cash' telos binds and the protocol must support everyday transactional
 *   use with low fees. The standing arrangement under contest — the
 *   constraint's ε referent — is the large-block regime this reading
 *   institutes (8MB+ blocks, fee-minimizing capacity policy,
 *   merchant-adoption priority), assessed as it actually operates, never the
 *   sibling reading's small-block arrangement. Structurally the arrangement
 *   fuses a genuine coordination function (cheap peer-to-peer payments on a
 *   shared ledger) with asymmetric cost-shifting (permanent storage and
 *   bandwidth burdens land on node operators, who lose agenda-setting voice
 *   as validation concentrates). The claim and the metrics are independent
 *   authored facts: claimed_type records what I believe is structurally true
 *   of this arrangement; the metric values record what I believe is
 *   descriptively true of its operation. The kernel contest itself is routed
 *   to omega variables per the committer-frame rules; the sibling reading is
 *   linked via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - - low_value_transactors: primary beneficiary (powerless/mobile) — the demand side the cash mandate exists to serve; exit is trivial, so their benefit is real but their voice is diffuse
 *   - - payment_processors: secondary beneficiary (organized/arbitrage) — collect predictable margins from fee stability; multi-chain posture caps their exposure
 *   - - crypto_merchants: tertiary beneficiary (moderate/constrained) — small-ticket viability depends on the fee floor staying near zero
 *   - - node_operators: primary target (moderate/identity_locked) — bear permanent storage and bandwidth costs; exit is nominally free but identity-fused to verification-as-participation
 *   - - mining_pools: agenda setter with beneficiary overlap (powerful/mobile) — enact capacity changes via hash signaling; best-connected pools gain relative advantage
 *   - - bch_protocol_developers: agenda setter (institutional/identity_locked) — administer the capacity roadmap; professional identity bound to it
 *   - - store_of_value_advocates: excluded voice (powerful/analytical) — hold the rival reading and govern the sibling chain
 *   - - protocol_economists: analytical observer — measure fee/node/adoption trajectories against each reading's predictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Electronic Cash Reading: Binding On-Chain Capacity Mandate").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed systems/monetary theory/technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '0eb70fef-43df-4036-a831-0026a7408923').
narrative_ontology:cs_kernel_codification('0eb70fef-43df-4036-a831-0026a7408923', fixed_text).
narrative_ontology:cs_authority_grounding('0eb70fef-43df-4036-a831-0026a7408923', lineage).
narrative_ontology:cs_interpretation_layer_present('0eb70fef-43df-4036-a831-0026a7408923').
narrative_ontology:cs_reading_relation('0eb70fef-43df-4036-a831-0026a7408923', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('0eb70fef-43df-4036-a831-0026a7408923', foundational, whitepaper_cash_telos_binding).
narrative_ontology:cs_axiom_status(whitepaper_cash_telos_binding, holdable).
narrative_ontology:cs_axiom_grounding('0eb70fef-43df-4036-a831-0026a7408923', whitepaper_cash_telos_binding, conventional).
narrative_ontology:cs_axiom('0eb70fef-43df-4036-a831-0026a7408923', foundational, onchain_capacity_necessary_for_cash_utility).
narrative_ontology:cs_axiom_status(onchain_capacity_necessary_for_cash_utility, holdable).
narrative_ontology:cs_axiom_grounding('0eb70fef-43df-4036-a831-0026a7408923', onchain_capacity_necessary_for_cash_utility, empirically_contingent).
narrative_ontology:cs_reference_frame('0eb70fef-43df-4036-a831-0026a7408923', p2p_ecash_design_intent).
narrative_ontology:cs_drift_state('0eb70fef-43df-4036-a831-0026a7408923', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0eb70fef-43df-4036-a831-0026a7408923', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, crypto_merchants).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, on_chain_scaling_preserves_cash_utility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sending small payments — remittances, retail purchases, tips — who need per-transaction costs to stay near zero for the medium to be usable at all. They choose wallets and networks freely and will route around any ledger whose fees exceed a few cents; their loyalty runs to price, not to any particular chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, mobile, global).

% Companies building payment products on top of the chain: gateways, point-of-sale integrations, remittance corridors. Predictable sub-cent fees and reliable confirmation let them price flat-rate services to merchants. They operate multi-chain and allocate engineering effort wherever unit economics currently favor them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, arbitrage, global).

% Online and physical retailers accepting the coin directly at checkout. Low fees make small-ticket acceptance viable and irreversible settlement removes chargeback risk. Their exposure is bounded — they can drop the payment option if volume disappoints — but retooling checkout flows carries real friction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, crypto_merchants, beneficiary,
    moderate, biographical, constrained, global).

% Hobbyists, businesses, and institutions running full validating nodes. Every block-size increase permanently raises their storage, bandwidth, and hardware costs. Most could stop operating a node at little direct expense, but many treat independent verification of every transaction as the entire point of participating, which makes stepping back feel like abandoning the project rather than exercising an option.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    moderate, generational, identity_locked, global).

% Pooled hash-power operators who signal and enact consensus-rule changes. Block-size decisions alter their orphan rates and connectivity requirements in ways that favor the best-connected pools. They can redirect hash rate to any competing chain sharing the same proof-of-work algorithm within hours.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, agenda_setter,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, beneficiary).

% Client-maintenance teams who implement the capacity schedule and ship protocol upgrades. Their professional standing rests on fidelity to the founding document's stated purpose; careers and reputations are bound to the capacity roadmap they administer, and reversing it would repudiate their own body of work.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, bch_protocol_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Developers, investors, and researchers who hold that verifiability outranks throughput and that capacity must stay small. They governed the majority chain until the 2017 split and continue publishing critiques of large-block designs; they stand outside this arrangement's decision loop by construction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_advocates, excluded,
    powerful, generational, analytical, global).

% Academic and industry researchers measuring fee markets, node operating costs, and adoption against each camp's published predictions. They collect from neither side, and both communities cite their findings selectively.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single append-only ledger that lets strangers exchange value without trusted intermediaries; expanding block capacity keeps per-transaction confirmation cheap enough for everyday purchase-sized payments to clear on-chain.
% TRANSFER_FUNCTION: Moves payment value directly between transactors; moves the cost of ledger upkeep — permanent storage growth, bandwidth, validation hardware — onto node operators, whose ranks thin as blocks grow, shifting day-to-day verification trust toward well-resourced operators and wallet providers.
% ABSENT_VOICES: Store-of-value advocates, security researchers, and small-block developers would object that capacity expansion erodes universal full-node verifiability; they sit outside this reading's governance, administering the sibling chain since the 2017 split.
% DISAPPEARANCE_RATIONALE: If the capacity commitment vanished overnight — blocks capped at legacy size — fees would spike with any real usage, merchant and remittance payment flows would collapse, processors would withdraw integrations, and the chain would reorganize around infrequent large-value settlement while node operating costs fell. The arrangement's beneficiary base depends on it continuing to bind.
% FOUNDING_PROBLEM: The whitepaper's founding problem: a purely peer-to-peer version of electronic cash that lets online payments move directly between parties without passing through a financial institution, solving double-spending with proof-of-work so that small, frequent, low-cost internet-native payments become possible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on both sides: World Bank remittance-cost datasets and academic payment-economics literature attest that a low-cost digital payment rail problem is real and unsolved by legacy rails (supporting 'live'); store-of-value economists and second-layer developers attest that consumer payment demand is now served by stablecoins, cards, and off-chain channels, making the on-chain cash mandate obsolete (supporting 'dead'). Neither attestation comes from this reading's beneficiary set.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction sits mid-range (0.52) because the cost-shifting is real but bounded: at 8MB-class blocks the absolute node burden is modest, yet the trajectory permanently raises the floor of participation and pushes casual operators toward trusted-lightweight clients — an asymmetric transfer even when each increment is small. Suppression (0.55) is a raw structural property, unscaled by power or scope: the consensus rules themselves rule out the small-block alternative inside this arrangement, and the historical enforcement record (forum moderation wars, denial-of-service campaigns against opposing voices during 2015–2017) shows the alternative had to be actively kept out rather than merely losing on merits. Theater (0.30) reflects real but underwhelming payment activity beneath sustained 'electronic cash' branding — anniversary campaigns and adoption drives perform the cash identity more than realized volume sustains it, easing slightly after 2023 as the community settled into niche expectations. Accessibility_collapse is low (0.40): understanding this constraint does not close alternatives — second-layer designs, rival chains, and the sibling reading all remain live and argued. Resistance is high (0.70): the block-size wars were among the most contentious episodes in the domain's history, ending in a permanent chain split rather than consent. The suppression_requirement series traces enforcement-capacity change specifically: it peaks at the 2017 fork (0.75) when the arrangement had to be forced through against maximal opposition, then decays (0.42) as opponents exited to the sibling chain and consolidation reduced the enforcement load — while the standing structural suppression scalar (0.55) remains higher because the ruled-out alternative stays ruled out regardless of how much active force current maintenance requires. All three series share one time grid (2015, 2017, 2019, 2021, 2023, 2025) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the node_operator seat the structure reads as extraction they subsidize: they pay permanent costs so that strangers' coffee purchases stay cheap, and their identity-lock means they cannot exit without abandoning what participation meant to them. From the low_value_transactor and payment_processor seats the same structure reads as straightforward coordination they would voluntarily buy: sub-cent settlement on a neutral rail. From the agenda-setter seats (developers, pools) it reads as mandate fidelity — the founding document said cash, so capacity is obedience, not choice. Two same-level actors illustrate constraint-specific differentiation: transactors and node operators are both nominally just 'users' of one network with equal formal standing, yet the transactor's exit is a wallet switch while the operator's exit is a small apostasy — identical global position, opposite effective leverage, driven entirely by what the constraint fuses to identity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: low_value_transactors, payment_processors, and crypto_merchants receive the arrangement's subsidy (cheap block space) and carry low directionality — damped effective extraction, approaching net subsidy for the most price-sensitive transactors, whose mobility pushes them further toward the beneficiary pole. node_operators are the declared victims: they absorb the costs that fund everyone else's cheap transactions, and their identity_lock places them near the full-target end despite their nominal ability to quit — trapped-by-commitment rather than trapped-by-barrier. The agenda setters straddle: mining_pools carry a secondary beneficiary role (connectivity advantages compound with block size) pulling their derived d below symmetric, while bch_protocol_developers sit nearer symmetric — they administer the arrangement more than they collect from it. Scope is global, which amplifies effective extraction modestly for targets since verification of node-burden claims is harder at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cash-like payments without intermediaries — is contested rather than dead: legacy rails still levy 6%+ on remittances, yet consumer payment flow has migrated to rails this reading does not govern. Because the mandate has not clearly outlived its function, this is not a resolved-mandatrophy case, and the classification guards both failure directions. Reading the arrangement as pure extraction (snare) would erase the genuine coordination: sub-cent settlement on a shared ledger is a real collective good that low-value transactors demonstrably want and use. Reading it as pure coordination (rope) would erase the asymmetry: node operators bear compounding costs with shrinking voice, and the beneficiary set skews toward actors with infrastructure. Tangled_rope holds both truths in one structure — coordination and extraction flowing through the same capacity decision — which is exactly what the block-size dispute was about. The theater_ratio trajectory (rising to 0.34, easing to 0.30) is watched rather than decisive: if on-chain cash volume never materializes, the mandate's performance layer will dominate and the arrangement will decay toward inertial maintenance; the onchain_cash_feasibility omega tracks precisely that boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel bitcoin_whitepaper_purpose (reading: electronic_cash_reading). What structural differences would obtain if the store_of_value_reading governed instead?',
    'Long-run comparison of the two chains'' adoption, fee levels, node-count trajectories, and payment volume: whichever arrangement durably attracts both payment and holding usage reveals which telos the market treats as binding.',
    'If the store_of_value_reading prevails, this constraint survives only as local governance of a minority chain and its epsilon should be reassessed against a shrinking beneficiary base; if this reading prevails, the sibling''s verifiability-first arrangement faces equivalent downward revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: the sibling reading would subordinate capacity to verifiability, restructuring beneficiaries and victims entirely.').

omega_variable(
    node_cost_vs_coordination_price,
    'Are node operators'' storage and bandwidth costs a genuine coordination price of trustless verification, or extractive externalization of costs onto agents with no agenda-setting voice?',
    'Cost surveys of full-node operation at successive block-size tiers, crossed with measured transactor willingness-to-pay; if operators exit faster than replacements arrive, the imposed price exceeds the coordination value it purchases.',
    'If externalization dominates, effective extraction rises and the arrangement trends toward snare; if the burden tracks genuine service cost, the coordination component dominates and the rope side firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_cost_vs_coordination_price, empirical, 'Whether the node-operator burden is the price of the service or a shifted cost.').

omega_variable(
    decentralization_collapse_threshold,
    'At what sustained block size does non-specialist full-node operation become impractical, converting the capacity commitment from distributed coordination into validation concentration?',
    'Track full-node counts and operator demographics against realized block sizes across the interval; identify the tier at which hobbyist-operated share collapses.',
    'Crossing the threshold restructures the beneficiary set: custodial operators and well-connected pools become concentrated beneficiaries of everyone else''s displaced verification, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_collapse_threshold, empirical, 'The block-size tier at which the arrangement''s cost-shifting changes kind, not just degree.').

omega_variable(
    onchain_cash_feasibility,
    'Can everyday cash-like usage be delivered on-chain at sustainably low fees at all, or does the cash telos structurally require off-chain settlement regardless of block capacity?',
    'Observe whether sustained retail-scale payment volume ever materializes on high-capacity chains, compared against stablecoin and card rails serving the same demand profile.',
    'If on-chain cash is infeasible, this reading''s coordination function is largely theatrical and the arrangement decays toward inertial maintenance; if feasible, the coordination claim is vindicated and the theater ratio should fall as usage replaces branding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(onchain_cash_feasibility, empirical, 'Whether the reading''s core promise is deliverable by the means it mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(electronic_cash_reading_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2015, observed).
narrative_ontology:measurement(electronic_cash_reading_tr_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2017, observed).
narrative_ontology:measurement(electronic_cash_reading_tr_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2019, observed).
narrative_ontology:measurement(electronic_cash_reading_tr_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2021, 0.31).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2021, observed).
narrative_ontology:measurement(electronic_cash_reading_tr_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2023, 0.34).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2023, observed).
narrative_ontology:measurement(electronic_cash_reading_tr_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(electronic_cash_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(electronic_cash_reading_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2015, observed).
narrative_ontology:measurement(electronic_cash_reading_be_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2017, observed).
narrative_ontology:measurement(electronic_cash_reading_be_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2019, 0.53).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2019, observed).
narrative_ontology:measurement(electronic_cash_reading_be_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2021, observed).
narrative_ontology:measurement(electronic_cash_reading_be_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2023, 0.53).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2023, observed).
narrative_ontology:measurement(electronic_cash_reading_be_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(electronic_cash_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(electronic_cash_reading_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2015, observed).
narrative_ontology:measurement(electronic_cash_reading_su_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2017, 0.75).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2017, observed).
narrative_ontology:measurement(electronic_cash_reading_su_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2019, observed).
narrative_ontology:measurement(electronic_cash_reading_su_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2021, 0.5).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2021, observed).
narrative_ontology:measurement(electronic_cash_reading_su_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2023, 0.45).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2023, observed).
narrative_ontology:measurement(electronic_cash_reading_su_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(electronic_cash_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Bitcoin's purpose' covers two structurally distinct claims that this corpus separates. The electronic_cash_reading (this file) authors epsilon for the large-block arrangement — capacity expansion coordinating payments while shifting costs onto node operators (epsilon 0.52, tangled_rope). The store_of_value_reading (sibling file) authors epsilon for the small-block arrangement — verifiability preserved by constrained capacity, with extraction expressed as elevated fees excluding low-value transactors. The two arrangements have different beneficiary sets, different victim sets, different failure modes, and materially different epsilon values; forcing them into one story would make epsilon observable-dependent, which DP-001 forbids. Each file links the other via network.affects_constraints. Both readings are downstream of the interpretive-vacuum condition captured by the nakamoto_oracle_opacity reading, which is documented in commentary.kernel_context rather than declared as an edge, since the causal arrow runs from that condition into both telos readings rather than out of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
