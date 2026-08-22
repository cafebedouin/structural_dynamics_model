% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__digital_gold_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold: Scarcity-Optimized Store of Value
 *   domain: cryptocurrency/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'digital gold' reading of the Bitcoin
 *   whitepaper kernel — the interpretation that prioritizes Bitcoin's 21M
 *   supply cap and resulting scarcity as its defining virtue, accepting high
 *   transaction fees and low throughput as acceptable trade-offs. The reading
 *   is instantiated by holders, custodians, and miners who benefit from
 *   appreciation and fee revenue, while late entrants and high-frequency
 *   users bear the cost. This is one of three live readings of the same
 *   kernel (bitcoin_whitepaper), alongside the P2P cash reading (prioritizing
 *   medium-of-exchange) and protocol ossification reading (prioritizing
 *   stability above all). The three readings coexist in public discourse but
 *   create structural tensions: the digital gold reading's fee market
 *   acceptance directly pressures the P2P cash reading's viability, while the
 *   ossification reading's resistance to change protects the digital gold
 *   reading's scarcity guarantee.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.32).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Scarcity-Optimized Store of Value").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'f5466148-9ae9-451d-a637-48128b9a625e').
narrative_ontology:cs_kernel_codification('f5466148-9ae9-451d-a637-48128b9a625e', fixed_text).
narrative_ontology:cs_authority_grounding('f5466148-9ae9-451d-a637-48128b9a625e', lineage).
narrative_ontology:cs_interpretation_layer_present('f5466148-9ae9-451d-a637-48128b9a625e').
narrative_ontology:cs_reading_relation('f5466148-9ae9-451d-a637-48128b9a625e', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5466148-9ae9-451d-a637-48128b9a625e', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('f5466148-9ae9-451d-a637-48128b9a625e', foundational, scarcity_above_throughput).
narrative_ontology:cs_axiom_status(scarcity_above_throughput, holdable).
narrative_ontology:cs_axiom_grounding('f5466148-9ae9-451d-a637-48128b9a625e', scarcity_above_throughput, deontological).
narrative_ontology:cs_axiom('f5466148-9ae9-451d-a637-48128b9a625e', secondary, fee_market_necessity).
narrative_ontology:cs_axiom_status(fee_market_necessity, holdable).
narrative_ontology:cs_axiom_grounding('f5466148-9ae9-451d-a637-48128b9a625e', fee_market_necessity, instrumental).
narrative_ontology:cs_reference_frame('f5466148-9ae9-451d-a637-48128b9a625e', genesis_block_social_contract).
narrative_ontology:cs_drift_state('f5466148-9ae9-451d-a637-48128b9a625e', post_blockspace_market_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f5466148-9ae9-451d-a637-48128b9a625e', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters_long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_custodians).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_operations).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants_priced_out).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, high_frequency_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, developing_nation_users).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, fixed_supply_monetary_soundness).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, separate_money_from_state).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, deflationary_incentive_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired BTC at low cost basis during protocol infancy. Benefit from scarcity-driven appreciation and fee market development. Can liquidate or hedge through mature derivatives markets. Exit is near-costless due to deep liquidity and low average entry price.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters_long_term_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Provide custody, ETF vehicles, and regulated access to BTC. Collect management fees and bid-ask spreads. Influence protocol governance through concentrated holdings and regulatory relationships. Exit is trivial — they are service providers, not holders of last resort.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_custodians, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, institutional_custodians, agenda_setter).

% Secure the network through proof-of-work, earning block subsidies and transaction fees. Revenue increasingly shifts to fees as subsidy halves. Capital-intensive operations with sunk costs in ASICs and energy contracts. Exit requires selling hardware or repurposing energy contracts — non-trivial but possible.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_operations, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, mining_operations, beneficiary).

% Enter at high price levels where meaningful accumulation requires significant capital. Face transaction fees that represent substantial percentage of transfer value for small amounts. Cannot practically use Bitcoin for daily payments due to fee/confirmation dynamics. Exit means accepting loss or holding depreciating fiat — structurally constrained by the same monetary system they sought to escape.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants_priced_out, payer,
    moderate, biographical, constrained, global).

% Require low-fee, fast-finality transactions for commerce or remittances. Driven to Layer 2 solutions (Lightning) or alternative chains by base-layer fee market. Exit is mobile — they can and do route around the constraint via L2s or competing networks, but this fragments the very network effect that gives Bitcoin its monetary premium.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, high_frequency_transactors, payer,
    moderate, immediate, mobile, global).

% Seek censorship-resistant savings and payments in high-inflation environments. Face dual barrier: BTC price denominated in strong currencies makes accumulation prohibitively expensive, while base-layer fees exceed daily wages. Lightning adoption requires technical sophistication and reliable internet. No viable exit — local fiat collapses, USD access restricted, BTC base layer unaffordable.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, developing_nation_users, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, developing_nation_users, excluded).

% Maintain reference implementation and propose protocol changes. Operate under social consensus norms where changes require near-universal agreement. Neither collect fees nor pay them directly. Observe the fee market dynamics and scarcity economics as system properties rather than personal costs or benefits.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, protocol_developers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global consensus on a fixed-supply monetary asset without central issuer, solving the double-spend problem and establishing credible scarcity through proof-of-work and social consensus on the 21M cap.
% TRANSFER_FUNCTION: Moves purchasing power from late entrants and high-frequency users (via price appreciation and transaction fees) to early holders and miners (via block subsidies and fee revenue). The fee market transfers blockspace access from low-value to high-value uses as defined by willingness to pay.
% ABSENT_VOICES: Unbanked populations in hyperinflationary economies who need censorship-resistant payments but cannot afford base-layer fees. Small merchants who would accept Bitcoin for commerce but are priced out by fee volatility. These voices are absent because the fee market structurally excludes sub-dollar transaction values.
% DISAPPEARANCE_RATIONALE: If the digital gold framing vanished overnight, Bitcoin would either pivot to a medium-of-exchange priority (lower fees, higher throughput, different security model) or lose its monetary premium as the scarcity narrative collapsed. The mining ecosystem, custodial infrastructure, and regulatory frameworks would all reorganize around a different value proposition.
% FOUNDING_PROBLEM: Create a peer-to-peer electronic cash system that removes the need for trusted third parties in digital payments, solving the double-spend problem without a central server.
% FOUNDING_PROBLEM_CORROBORATION: Satoshi's whitepaper and early communications describe electronic cash / peer-to-peer payments as the primary use case. The digital gold narrative emerged later (2013+) from holders and advocates who benefited from the scarcity framing. No external corroboration exists for 'store of value above all' as the founding intent — the pivot is documented in mailing list archives and forum discussions.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial wealth transfer from late entrants paying premium prices and high fees to early holders who acquired at low cost. Suppression (0.32) is moderate — the constraint does not actively prevent alternatives (other chains, L2s, fiat), but the fee market structurally excludes low-value transactions and the social consensus resists protocol changes that would increase throughput. Theater ratio (0.22) captures the growing gap between 'peer-to-peer electronic cash' branding and store-of-value reality. Accessibility collapse (0.38) is partial — alternatives exist but the network effect and monetary premium create switching costs. Resistance (0.45) reflects ongoing protocol debates (block size wars, Taproot activation, ordinals controversy) where the digital gold coalition successfully defends scarcity over throughput.
 *
 * PERSPECTIVAL GAP:
 *   From the early holder seat, the constraint is a successful coordination mechanism that created sound money. From the developing-nation user seat, it is a snare — promising censorship resistance but delivering unaffordable access. From the miner seat, it is a tangled rope — genuine coordination (network security) fused with extraction (fee revenue dependence). The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the system-level hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and institutions are structural beneficiaries (d ~ 0.1-0.2): they collect appreciation gains and fee revenue, with arbitrage-grade exit. Miners are agenda-setters with constrained exit (d ~ 0.4): they administer the constraint but face capital lock-in. Late entrants and developing-nation users are targets (d ~ 0.7-0.9): they pay the extraction via price and fees with limited exit. High-frequency transactors are mobile payers (d ~ 0.5): they pay but can route around via L2s. The directionality derivation from beneficiary/victim + exit options captures this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (P2P electronic cash) is contested as dead vs. live. The digital gold reading resolves mandatrophy by redefining the problem: scarcity *is* the solution, and the electronic cash function is delegated to Layer 2. This is not pure extraction — the coordination function (credible scarcity without central authority) is real and valued. But the fee market that sustains miner security *also* extracts from users priced out of base-layer transactions. The tangling is structural: you cannot have the fee market without the scarcity, and the scarcity without the fee market eventually fails (post-subsidy security).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_market_security_sufficiency,
    'Will transaction fees alone provide sufficient miner revenue to secure the network after block subsidies become negligible (~2032+)?',
    'Empirical observation of fee revenue trends relative to security requirements; game-theoretic analysis of fee market equilibrium under different demand scenarios.',
    'If fees are insufficient, the digital gold reading faces a trilemma: accept inflation (breaking scarcity), accept reduced security (breaking trust), or accept centralization (breaking decentralization). Any resolution changes the constraint''s type classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fee_market_security_sufficiency, empirical, 'Long-term security budget sustainability under pure fee market').

omega_variable(
    layer2_capture_vs_liberation,
    'Do Layer 2 solutions (Lightning, Ark, etc.) genuinely liberate transactors from base-layer fee extraction, or do they create new rent-extraction layers controlled by the same beneficiaries?',
    'Analysis of L2 fee structures, liquidity provider economics, and custody patterns over time. Comparison of end-user costs on L2 vs. theoretical competitive baseline.',
    'If L2s become captured rent layers, the extraction is merely displaced, not resolved — the constraint''s effective extraction remains high. If L2s achieve competitive fee markets, the digital gold reading''s victim set shrinks and the constraint moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer2_capture_vs_liberation, empirical, 'Whether L2s solve or displace the fee extraction problem').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the digital gold reading''s core premise (scarcity as supreme virtue) logically foreclose the P2P cash reading within a single protocol framework, or can both be simultaneously satisfied through Layer 2 scaling?',
    'Protocol-level analysis: can a single base layer optimize for both censorship-resistant low-value payments AND maximum scarcity/fee market? Or does optimizing for one necessarily degrade the other?',
    'If forecloses, the kernel has a genuine schism — only one reading can be the ''true'' Bitcoin. If coexists_with (current assignment), both remain live positions and the kernel remains a contested coordination point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Logical compatibility of sibling readings within one protocol').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_dg_tr_t2009, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2009, 0.02).
narrative_ontology:measurement(btc_dg_tr_t2012, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2012, 0.05).
narrative_ontology:measurement(btc_dg_tr_t2015, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(btc_dg_tr_t2018, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(btc_dg_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(btc_dg_tr_t2024, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(btc_dg_be_t2009, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2009, 0.02).
narrative_ontology:measurement(btc_dg_be_t2012, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2012, 0.08).
narrative_ontology:measurement(btc_dg_be_t2015, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement(btc_dg_be_t2018, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2018, 0.32).
narrative_ontology:measurement(btc_dg_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement(btc_dg_be_t2024, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_dg_su_t2009, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(btc_dg_su_t2012, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2012, 0.08).
narrative_ontology:measurement(btc_dg_su_t2015, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(btc_dg_su_t2018, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(btc_dg_su_t2021, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2021, 0.28).
narrative_ontology:measurement(btc_dg_su_t2024, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, lightning_network_fee_market).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, stablecoin_dollar_hegemony).

% DUAL FORMULATION NOTE:
% This constraint (digital_gold_reading) and p2p_cash_reading are sibling readings of the bitcoin_whitepaper kernel. They share the same kernel (the whitepaper's fixed text and genesis block) but instantiate different constraints with different ε, beneficiaries, victims, and claimed types. The protocol_ossification_reading is a downstream constraint influenced by the digital gold reading's scarcity prioritization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
