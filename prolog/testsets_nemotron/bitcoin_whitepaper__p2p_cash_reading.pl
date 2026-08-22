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
 *   human_readable: Bitcoin P2P Cash Reading — Low-Fee Transactional Access
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   This constraint story captures the p2p_cash_reading of the
 *   bitcoin_whitepaper kernel: the claim that Bitcoin's primary function is
 *   censorship-resistant, low-fee electronic cash for direct peer-to-peer
 *   transactions. The reading asserts that block space should scale to keep
 *   fees low, that fee markets excluding low-value users are a betrayal of
 *   the whitepaper's promise, and that the victim set includes the global
 *   unbanked, micro-merchants, and low-value remittance senders who are
 *   priced out of on-chain access. The sibling readings —
 *   digital_gold_reading (store of value primacy) and
 *   protocol_ossification_reading (stability over change) — instantiate
 *   different constraints with different ε, different victim sets, and
 *   different type classifications. This story authors ONLY the
 *   p2p_cash_reading constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.38).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin P2P Cash Reading — Low-Fee Transactional Access").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/technological/social").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '7ece8075-0c37-4f9e-a781-ffb1a5961e23').
narrative_ontology:cs_kernel_codification('7ece8075-0c37-4f9e-a781-ffb1a5961e23', fixed_text).
narrative_ontology:cs_authority_grounding('7ece8075-0c37-4f9e-a781-ffb1a5961e23', lineage).
narrative_ontology:cs_interpretation_layer_present('7ece8075-0c37-4f9e-a781-ffb1a5961e23').
narrative_ontology:cs_reading_relation('7ece8075-0c37-4f9e-a781-ffb1a5961e23', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ece8075-0c37-4f9e-a781-ffb1a5961e23', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('7ece8075-0c37-4f9e-a781-ffb1a5961e23', foundational, low_fees_are_necessary_for_censorship_resistance).
narrative_ontology:cs_axiom_status(low_fees_are_necessary_for_censorship_resistance, holdable).
narrative_ontology:cs_axiom_grounding('7ece8075-0c37-4f9e-a781-ffb1a5961e23', low_fees_are_necessary_for_censorship_resistance, empirically_contingent).
narrative_ontology:cs_axiom('7ece8075-0c37-4f9e-a781-ffb1a5961e23', foundational, block_size_expansion_is_legitimate_coordination).
narrative_ontology:cs_axiom_status(block_size_expansion_is_legitimate_coordination, holdable).
narrative_ontology:cs_axiom_grounding('7ece8075-0c37-4f9e-a781-ffb1a5961e23', block_size_expansion_is_legitimate_coordination, instrumental).
narrative_ontology:cs_axiom('7ece8075-0c37-4f9e-a781-ffb1a5961e23', foundational, the_unbanked_are_the_primary_beneficiary_of_the_whitepaper_vision).
narrative_ontology:cs_axiom_status(the_unbanked_are_the_primary_beneficiary_of_the_whitepaper_vision, holdable).
narrative_ontology:cs_axiom_grounding('7ece8075-0c37-4f9e-a781-ffb1a5961e23', the_unbanked_are_the_primary_beneficiary_of_the_whitepaper_vision, deontological).
narrative_ontology:cs_reference_frame('7ece8075-0c37-4f9e-a781-ffb1a5961e23', whitepaper_electronic_cash).
narrative_ontology:cs_drift_state('7ece8075-0c37-4f9e-a781-ffb1a5961e23', post_blocksize_wars, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ece8075-0c37-4f9e-a781-ffb1a5961e23', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, institutional_custodians).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, global_unbanked_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, microtransaction_merchants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_low_value).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, lightning_network_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, censorship_resistance_requires_low_fees).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, block_space_is_a_public_good_not_a_scarcity_rent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the hashing infrastructure that secures the chain and selects which transactions enter blocks. Earn block subsidies and transaction fees. Can signal support for or against protocol changes (soft forks, block size adjustments). Their revenue depends on fee markets remaining viable; low-fee, high-throughput designs reduce per-block fee income unless volume scales superlinearly. Exit is arbitrage-grade: they can mine other SHA-256 chains or sell hardware.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, beneficiary).

% Maintain the reference implementation (Bitcoin Core) and control the merge process for consensus changes. Their professional identity and institutional authority are bound to the 'conservative protocol stewardship' framing. They benefit from status, grant funding, and influence. Exit is identity-locked: leaving the project means abandoning the epistemic community and career capital built around Bitcoin's specific technical culture.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers, beneficiary).

% Provide regulated on-ramps, custody, and settlement layers (exchanges, ETFs, prime brokers). They benefit from Bitcoin's 'digital gold' narrative and high settlement fees that price out retail users — it reinforces their intermediation role. They can pivot to other assets or chains; exit is mobile.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, institutional_custodians, beneficiary,
    powerful, biographical, mobile, global).

% Need censorship-resistant, low-cost payments for daily commerce and survival remittances. When median fees exceed daily wages, they are functionally excluded from on-chain access. They cannot 'wait for lower fees' — their time horizon is immediate. No realistic alternative: stablecoins require KYC/rails they lack; other chains lack liquidity or censorship resistance. Trapped by fee market dynamics they did not create and cannot influence.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, global_unbanked_users, payer,
    powerless, immediate, trapped, global).

% Run businesses that depend on sub-dollar payments (content micropayments, API metering, IoT data markets). On-chain fees make their unit economics impossible. They can migrate to Layer 2 (Lightning, Liquid) but face adoption friction, liquidity management complexity, and UX barriers. Exit is constrained: technically possible but economically costly and uncertain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, microtransaction_merchants, payer,
    moderate, biographical, constrained, global).

% Send $20–$100 cross-border to family. On-chain fees of $5–$50 consume 10–50% of principal. They are trapped: traditional corridors (Western Union) are similarly extractive but more accessible; stablecoin rails require exchange access they may not have. The fee market prices them out of the very censorship resistance the whitepaper promised.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_low_value, payer,
    powerless, immediate, trapped, global).

% Operate Lightning routing nodes, watchtowers, and wallet infrastructure. They benefit from high on-chain fees driving demand for L2. Their business model depends on the constraint (high base-layer fees) persisting. Exit is mobile: they can deploy on other L2s or chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, lightning_network_operators, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, lightning_network_operators, agenda_setter).

% Study fee market dynamics, censorship resistance, and protocol governance from outside the system. They analyze but do not collect fees nor pay them. Their exit is analytical — they can shift research focus without material cost.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, academic_researchers_crypto, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, censorship-resistant ledger for final settlement of electronic transactions without trusted intermediaries, using proof-of-work consensus and a shared UTXO set.
% TRANSFER_FUNCTION: Moves value from transactors (who pay fees) to miners (who collect fees + subsidy) in exchange for block space. As block subsidy declines, the transfer increasingly relies on fee markets — moving value from low-value, high-urgency users to miners and L2 operators who capture the scarcity rent.
% ABSENT_VOICES: The global unbanked, micro-merchants in emerging economies, and low-value remittance senders are structurally absent from Bitcoin governance (no hash power, no Core commit access, no institutional lobbying). They would object to fee markets that price them out of the censorship resistance they were promised. Their absence is not accidental — the governance structure (hash-power signaling, Core merge control) has no mechanism for their inclusion.
% DISAPPEARANCE_RATIONALE: If the low-fee transactional-access constraint vanished (i.e., if Bitcoin permanently settled into a high-fee, store-of-value-only equilibrium), the unbanked and micro-transactors would lose their only censorship-resistant base layer. They would be forced onto custodial rails (exchanges, stablecoins) or less-censorship-resistant chains. The world rearranges: the promise of 'peer-to-peer electronic cash for the world' is broken, and the arrangement that replaces it is structurally different — intermediated, permissioned, or less robust.
% FOUNDING_PROBLEM: The 2008 whitepaper identified the founding problem: 'A purely peer-to-peer version of electronic cash would allow online payments to be sent directly from one party to another without going through a financial institution.' The problem was trusted-third-party dependence — censorship, reversal risk, and cost overhead — not merely 'digital scarcity.'
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself (Satoshi Nakamoto, 2008) corroborates the transactional-access framing — the abstract opens with 'electronic cash' and 'direct' payments. Early community archives (BitcoinTalk 2009–2011) show 'low fees' and 'micropayments' as live design goals. The digital_gold_reading proponents (e.g., Blockstream, Bitcoin Core majority post-2017) argue the founding problem was 'uncensorable digital scarcity' and that fees are a feature, not a bug. No neutral third-party adjudication exists; the dispute is structural.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects the structural reality: the fee market transfers value from transactors to miners, and as subsidy declines, this transfer becomes the dominant miner revenue. The victims (unbanked, micro-merchants, remittance senders) pay disproportionately high effective fees relative to income — extraction is real but not total because Layer 2 and custodial off-ramps provide partial relief. Suppression (0.42) is moderate: the constraint does not actively prevent alternatives (Lightning, other chains, stablecoins exist), but the *promise* of base-layer censorship resistance is suppressed by fee dynamics — users who need censorship resistance *on the base layer* find it economically inaccessible. Theater (0.28) captures the gap between 'Bitcoin is electronic cash' rhetoric and 'Bitcoin is digital gold' reality — Core developers and institutions perform the cash narrative while building for the gold equilibrium. Accessibility collapse (0.55) is mid-range: alternatives exist but are imperfect (Lightning UX, custodial trust, altchain liquidity). Resistance (0.61) is high: the block size wars, UASF, and ongoing policy debates show active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the miner/Core developer seat, the constraint is a Rope: coordination on a stable, scarce block space solves the tragedy-of-the-commons for chain security. From the unbanked seat, it is a Snare: the coordination story ('secure block space') is cover for extraction that prices them out of censorship resistance. From the micro-merchant seat, it is a Tangled Rope: Lightning provides real coordination (instant, cheap payments) but the base-layer fee pressure that drives L2 adoption is extractive. The engine computes this per-seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners and Core developers are structural beneficiaries (d low): miners collect fee revenue; developers gain status and funding from stewardship of the 'conservative' protocol. Institutional custodians benefit from high-fee equilibrium reinforcing their intermediation (d low). The unbanked and low-value remittance senders are full targets (d near 1.0): trapped, immediate horizon, no exit, priced out of the very property (censorship resistance) the constraint claims to provide. Micro-merchants are constrained payers (d ~0.7): they have technical exit (L2) but high switching cost. Lightning operators are beneficiaries of the high-fee regime (d low) but also agenda_setters for L2 adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trusted-third-party-free electronic cash) is contested: digital_gold proponents argue it was solved by the existence of uncensorable digital scarcity; p2p_cash proponents argue it remains unsolved because the cash function is economically inaccessible to the populations that need it most. Mandatrophy is NOT resolved — the arrangement persists but the original mandate (cash for the world) is either dead (gold view) or live but betrayed (cash view). The classification as tangled_rope captures this: genuine coordination (censorship-resistant settlement) coexists with asymmetric extraction (fee rents on the excluded).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_market_necessity,
    'Is a competitive fee market structurally necessary for Bitcoin''s censorship resistance, or is it a contingent artifact of the 1MB block limit (and its successors) that could be removed without compromising security?',
    'Empirical observation of chains with larger blocks (BCH, BSV) and their censorship resistance track records; game-theoretic analysis of miner incentives under low-fee, high-volume regimes; measurement of whether fee pressure actually correlates with hash rate security.',
    'If fee markets are necessary, the extraction is structural (coordination cost) and the constraint leans toward rope/tangled_rope. If contingent, the extraction is imposed (snare/tangled_rope with higher χ) and block size expansion is a legitimate coordination improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_necessity, empirical, 'Whether fee extraction is inherent to censorship resistance or a policy choice.').

omega_variable(
    lightning_sufficiency,
    'Does the Lightning Network (and related L2s) genuinely solve the low-fee transactional access problem for the victim populations, or does it recreate custodial trust and exclusion at a different layer?',
    'Longitudinal adoption data for non-custodial Lightning among unbanked/low-income users; UX friction metrics; liquidity management burden measurement; censorship resistance tests of L2 routing under adversarial conditions.',
    'If L2 suffices, the base-layer fee market is a coordination mechanism pushing users to better tech (tangled_rope coordination function real). If L2 fails the victim populations, the base-layer constraint remains extractive with no viable exit (snare-leaning).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lightning_sufficiency, empirical, 'Whether Layer 2 actually provides the exit the constraint promises.').

omega_variable(
    reading_foreclosure_structure,
    'Does the p2p_cash_reading logically foreclose the digital_gold_reading within a single commitment framework, or do they coexist as competing but compatible framings?',
    'Analyze whether a single protocol governance process can simultaneously optimize for low-fee transactional throughput AND maximum store-of-value credibility (fixed supply, minimal changes, maximal decentralization). If the optimizations conflict irreducibly, foreclosure obtains; if they can be sequenced or layered, coexistence obtains.',
    'If forecloses: the kernel cannot stably support both readings — one must win governance. If coexists_with: the kernel sustains a permanent contest with institutionalized factions. If influences: one reading''s dominance creates structural pressure on the other (e.g., digital_gold dominance raises fees, pressuring p2p_cash users to L2).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and its siblings in the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 2009, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_p2p_cash_tr_t2009, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(btc_p2p_cash_tr_t2012, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2012, 0.08).
narrative_ontology:measurement(btc_p2p_cash_tr_t2015, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(btc_p2p_cash_tr_t2017, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(btc_p2p_cash_tr_t2020, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(btc_p2p_cash_tr_t2022, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(btc_p2p_cash_tr_t2024, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(btc_p2p_cash_tr_t2030, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2030, 0.32).

% Extraction over time
narrative_ontology:measurement(btc_p2p_cash_be_t2009, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2009, 0.02).
narrative_ontology:measurement(btc_p2p_cash_be_t2012, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2012, 0.05).
narrative_ontology:measurement(btc_p2p_cash_be_t2015, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(btc_p2p_cash_be_t2017, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(btc_p2p_cash_be_t2020, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2020, 0.31).
narrative_ontology:measurement(btc_p2p_cash_be_t2022, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2022, 0.28).
narrative_ontology:measurement(btc_p2p_cash_be_t2024, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement(btc_p2p_cash_be_t2030, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2030, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(btc_p2p_cash_su_t2009, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(btc_p2p_cash_su_t2012, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2012, 0.15).
narrative_ontology:measurement(btc_p2p_cash_su_t2015, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(btc_p2p_cash_su_t2017, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(btc_p2p_cash_su_t2020, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(btc_p2p_cash_su_t2022, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2022, 0.38).
narrative_ontology:measurement(btc_p2p_cash_su_t2024, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement(btc_p2p_cash_su_t2030, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2030, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, lightning_network_routing_constraint).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miner_revenue_model).

% DUAL FORMULATION NOTE:
% This constraint is one member of the bitcoin_whitepaper constraint family. The p2p_cash_reading, digital_gold_reading, and protocol_ossification_reading are three distinct constraints with different ε, different beneficiary/victim structures, and different type trajectories, linked by shared kernel identity. The p2p_cash_reading has ε=0.38 (moderate extraction), victims = low-value transactors, claimed_type = tangled_rope. The digital_gold_reading has ε≈0.15 (low extraction, beneficiaries = savers/institutions), claimed_type = rope. The protocol_ossification_reading has ε≈0.25 (moderate extraction via suppressed innovation), victims = developers/experimenters, claimed_type = tangled_rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, powerless, 0.95).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, moderate, 0.65).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, organized, 0.2).
constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
