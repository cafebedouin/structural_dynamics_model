% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__store_of_value_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Store-of-Value Reading: Base Layer Capacity Subordinated to Decentralization
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the store-of-value reading of the
 *   bitcoin_whitepaper_purpose kernel. It reads Satoshi's fixed kernel (the
 *   whitepaper and early protocol choices) as structurally subordinating
 *   on-chain transaction capacity to the preservation of decentralization and
 *   full-node verifiability. The 1MB block-size limit and the fee market
 *   enforce this subordination, pricing low-value users off the base layer
 *   and routing them toward Lightning Network or custodial alternatives. The
 *   constraint is claimed as tangled rope: it coordinates a genuine
 *   decentralization goal while asymmetrically extracting from users who need
 *   cheap on-chain settlement.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (organized/mobile) â accrue store-of-value security from the constrained base layer
 *   - full_node_operators: Agenda-setter and beneficiary (organized/mobile) â enforce consensus rules and benefit from low operational cost
 *   - low_fee_onchain_users: Primary payer/target (powerless/constrained) â priced off the base layer by the fee market
 *   - large_block_advocates: Excluded voice (organized/mobile) â lost the scaling dispute and were structurally sidelined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.6).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Store-of-Value Reading: Base Layer Capacity Subordinated to Decentralization").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '6087b54c-5fe4-4a67-b448-365194e17a35').
narrative_ontology:cs_kernel_codification('6087b54c-5fe4-4a67-b448-365194e17a35', fixed_text).
narrative_ontology:cs_authority_grounding('6087b54c-5fe4-4a67-b448-365194e17a35', distributed).
narrative_ontology:cs_reading_relation('6087b54c-5fe4-4a67-b448-365194e17a35', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('6087b54c-5fe4-4a67-b448-365194e17a35', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('6087b54c-5fe4-4a67-b448-365194e17a35', foundational, onchain_capacity_risk_to_decentralization).
narrative_ontology:cs_axiom_status(onchain_capacity_risk_to_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('6087b54c-5fe4-4a67-b448-365194e17a35', onchain_capacity_risk_to_decentralization, empirically_contingent).
narrative_ontology:cs_axiom('6087b54c-5fe4-4a67-b448-365194e17a35', foundational, base_layer_monetary_sovereignty).
narrative_ontology:cs_axiom_status(base_layer_monetary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6087b54c-5fe4-4a67-b448-365194e17a35', base_layer_monetary_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('6087b54c-5fe4-4a67-b448-365194e17a35', decentralized_verification_floor).
narrative_ontology:cs_drift_state('6087b54c-5fe4-4a67-b448-365194e17a35', post_blocksize_war_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6087b54c-5fe4-4a67-b448-365194e17a35', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a long-term store of value and base-layer settlement asset. Benefit from the hard-capped supply and the security model that depends on widely distributed, cheaply runnable full nodes. Their wealth preservation thesis relies on the protocol remaining resistant to centralized capture.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run fully validating Bitcoin nodes on commodity hardware, enforcing consensus rules including the block-size limit by rejecting blocks and chains that violate them. They bear the cost of storage, bandwidth, and uptime, but the constrained block size keeps that cost low enough for individual operation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, generational, mobile, global).

% Need base-layer bitcoin transactions for remittances, small payments, or savings access but cannot compete in the fee market during congestion. They are priced off the base layer and pushed toward Lightning Network, custodial wrappers, or alternative chains, none of which replicate the base-layer trust guarantees they originally sought.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users, payer,
    powerless, immediate, constrained, global).

% Argued that the block size should rise to accommodate low-fee on-chain transactions. Lost the Blocksize War consensus battle; their exclusion was effected by the economic majority of nodes and holders rejecting their software forks. They persist in separate chains but no longer influence the kernel's dominant reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, large_block_advocates, excluded,
    organized, biographical, mobile, global).

narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve decentralization by ensuring that the resource cost of independently verifying the entire blockchain remains low enough that individuals can run full nodes on commodity hardware, removing the need to trust intermediaries for final settlement.
% TRANSFER_FUNCTION: Moves the cost of scarce base-layer block space from the network's security budget and node operators' hardware requirements onto users who need cheap on-chain transactions, pricing them out of the base layer and into off-chain or alternative systems.
% ABSENT_VOICES: Large-block advocates who argued for on-chain scaling via bigger blocks, and low-income users in the Global South who need sub-dollar remittances but cannot afford base-layer fees; both were structurally sidelined during the Blocksize War consensus consolidation.
% DISAPPEARANCE_RATIONALE: If the capacity constraint vanishedâif block size were raised indefinitelyâthe cost to run a fully validating node would rise to data-center levels, the validator set would collapse to a handful of infrastructure providers, and the ecosystem would reorganize around trusted custodial verification. The trustless settlement guarantee that underpins the store-of-value function would dissolve.
% FOUNDING_PROBLEM: How to create a peer-to-peer electronic money system that operates without a trusted third party, avoiding the centralization failures that destroyed earlier digital-currency experiments.
% FOUNDING_PROBLEM_CORROBORATION: The cypherpunk mailing list and early digital-cash literature corroborate the founding problem of trusted-third-party risk from outside the benefiting parties. No independent corroboration exists for the specific claim that base-layer capacity must be permanently subordinated to store-of-value functionality; that resolution is asserted by the benefiting parties themselves.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the deliberate pricing out of low-fee users from the base layer; suppression (0.60) reflects the protocol-level enforcement of the block limit and the social consensus that marginalizes forks. Theater ratio (0.30) is moderate: the decentralization rhetoric is partly performative (many nodes are hobbyists rather than economically sovereign validators), but the technical constraint on block size is functionally real. Resistance (0.70) captures the Blocksize War and ongoing dissent. Accessibility collapse (0.60) captures the fact that while altcoins exist, the specific property of trustless base-layer Bitcoin settlement has no substitute once one is committed to the Bitcoin network.
 *
 * PERSPECTIVAL GAP:
 *   From the full-node operator seat, the constraint is necessary technical coordination to preserve decentralization; from the low-fee user seat, the same protocol is enforced extraction that denies them settlement access. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   long_term_holders and full_node_operators are declared beneficiaries, giving them directionality near the subsidy end (low d). Their exit options are mobile and their power is organized. low_fee_onchain_users are declared victims with constrained exit (Lightning requires capital and liquidity, altcoins lack the network effect) and powerless status, placing them near the full-target end (high d). The asymmetry is structural: the same consensus rule that subsidizes node operators by keeping hardware costs low extracts from transactors by converting block space into a scarce fee-good.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling because it possesses a genuine coordination functionâdecentralized verificationâthat would be lost if block size were uncapped. However, the asymmetric extraction (pricing out low-value users) is not incidental; it is the mechanism by which the coordination is preserved and funded. Without the victim group, the constraint would read as a rope; the presence of priced-out users makes it tangled rope rather than pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    blocksize_centralization_empirics,
    'Does increasing the block size beyond 1MB actually cause a material centralization of the full-node network, or is this relationship underspecified?',
    'Empirical measurement of node-operator bandwidth, storage, and compute costs correlated with block-size stress tests or natural experiments from forked chains with larger blocks.',
    'If the centralization risk is overstated, the constraint''s extraction is higher than its coordination function justifies, pushing classification toward snare. If validated, the tangled-rope balance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocksize_centralization_empirics, empirical, 'Empirical ambiguity around block size and node centralization.').

omega_variable(
    lightning_custody_replacement,
    'Does the Lightning Network preserve the trustlessness of the base layer, or does it reintroduce custodial intermediation that substitutes for the base-layer constraint?',
    'Measurement of Lightning channel liquidity concentration, custodial wallet market share, and routing-node centralization.',
    'If Lightning reintroduces trusted intermediaries, the coordination function (decentralized verification) is undermined by the supposed solution, increasing theater and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_custody_replacement, empirical, 'Whether off-chain scaling replicates or replaces trust.').

omega_variable(
    kernel_telos_ambiguity,
    'Is the store-of-value reading an authentic expression of the whitepaper kernel, or an imposition that retroactively redefines the project''s telos?',
    'Textual and historical analysis of the whitepaper, mailing-list posts, and Satoshi''s early communications; sociological study of narrative retrojection.',
    'If the SOV reading is a retroactive imposition, the constraint''s legitimacy is conventional rather than lineage-based, affecting authority_grounding and potentially reclassifying the commitment-system structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_telos_ambiguity, conceptual, 'Whether the SOV reading is authentic to the kernel or retroactively imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_sov_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(btc_sov_tr_t3, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(btc_sov_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(btc_sov_tr_t9, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(btc_sov_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(btc_sov_tr_t15, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(btc_sov_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(btc_sov_be_t3, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(btc_sov_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(btc_sov_be_t9, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(btc_sov_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(btc_sov_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(btc_sov_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(btc_sov_su_t3, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 3, 0.25).
narrative_ontology:measurement(btc_sov_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(btc_sov_su_t9, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(btc_sov_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(btc_sov_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_whitepaper_purpose kernel, decomposed from the electronic_cash_reading per the Îµ-invariance principle. The two readings share the same kernel text but instantiate different constraints with different Îµ values and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
