% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Whitepaper Purpose — Store of Value Reading (Decentralization/Verifiability Supreme)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'store of value' reading of the
 *   Bitcoin whitepaper's purpose: that decentralization and full-node
 *   verifiability are the binding constraints, and on-chain capacity is
 *   explicitly subordinated to these goals. The 1MB block size limit
 *   (retained after the 2017 block size war and SegWit activation) functions
 *   as the enforcement mechanism. The constraint extracts from users needing
 *   low-fee, high-frequency on-chain transactions — pricing them onto
 *   Lightning (with custodial tradeoffs) or off Bitcoin entirely — while
 *   subsidizing long-term holders (via scarcity-driven appreciation) and node
 *   operators (via low resource requirements). Lightning Network operators
 *   emerge as a new beneficiary class. The claimed_type is tangled_rope:
 *   genuine coordination (trust-minimized verification) coexists with
 *   asymmetric extraction (transaction users subsidize holders/operators).
 *   The kernel contest with electronic_cash_reading is structural: both
 *   readings draw from the same whitepaper text but elevate different
 *   passages as binding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.45).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose — Store of Value Reading (Decentralization/Verifiability Supreme)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '987e6185-ca05-49b4-bba2-af9581dd26b5').
narrative_ontology:cs_kernel_codification('987e6185-ca05-49b4-bba2-af9581dd26b5', fixed_text).
narrative_ontology:cs_authority_grounding('987e6185-ca05-49b4-bba2-af9581dd26b5', lineage).
narrative_ontology:cs_interpretation_layer_present('987e6185-ca05-49b4-bba2-af9581dd26b5').
narrative_ontology:cs_reading_relation('987e6185-ca05-49b4-bba2-af9581dd26b5', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('987e6185-ca05-49b4-bba2-af9581dd26b5', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('987e6185-ca05-49b4-bba2-af9581dd26b5', foundational, decentralization_verifiability_supremacy).
narrative_ontology:cs_axiom_status(decentralization_verifiability_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('987e6185-ca05-49b4-bba2-af9581dd26b5', decentralization_verifiability_supremacy, deontological).
narrative_ontology:cs_axiom('987e6185-ca05-49b4-bba2-af9581dd26b5', secondary, base_layer_settlement_only).
narrative_ontology:cs_axiom_status(base_layer_settlement_only, holdable).
narrative_ontology:cs_axiom_grounding('987e6185-ca05-49b4-bba2-af9581dd26b5', base_layer_settlement_only, instrumental).
narrative_ontology:cs_reference_frame('987e6185-ca05-49b4-bba2-af9581dd26b5', satoshi_whitepaper_decentralization_primacy).
narrative_ontology:cs_drift_state('987e6185-ca05-49b4-bba2-af9581dd26b5', post_2017_blocksize_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('987e6185-ca05-49b4-bba2-af9581dd26b5', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, global_south_micropayment_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, miners).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, decentralization_as_supreme_value).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_verifiability_nonnegotiable).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, base_layer_as_settlement_not_transactions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a savings vehicle; benefit from the scarcity narrative and price appreciation driven by constrained on-chain capacity. Their wealth grows when the base layer prioritizes settlement over transactions. Can exit by selling but have minimal incentive to do so.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run full nodes to independently verify the chain; the 1MB block limit keeps hardware/storage requirements low enough for commodity equipment. Their ability to verify without trusting third parties is the coordination good. Identity-locked because 'running a node' is constitutive of their self-concept as sovereign participants. Collectively enforce consensus rules through node majority.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, agenda_setter).

% Need to make frequent, low-value on-chain transactions (remittances, daily commerce, micro-earnings). Priced off the base layer by fee markets that clear at $5-$50+ during congestion. Forced onto Lightning Network (custodial tradeoffs, liquidity management complexity) or alternative chains. Exit is constrained: leaving Bitcoin means losing the network effect and liquidity; staying means paying extraction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_transaction_users, payer,
    moderate, immediate, constrained, global).

% Users in developing economies for whom even Lightning custodial fees are prohibitive and self-custody Lightning is technically inaccessible. The whitepaper's 'cash' promise is most salient for this group, but the store-of-value reading structurally excludes them. No voice in governance; no practical exit to a system with similar liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, global_south_micropayment_users, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, global_south_micropayment_users, excluded).

% Operate routing nodes, liquidity providers, and custodial services on Lightning. Direct beneficiaries of the base layer's capacity constraint — the constraint creates demand for their layer-2 services. Collect routing fees and custody revenue. Mobile: can redeploy capital to other layer-2s or chains if Lightning loses dominance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, beneficiary,
    organized, biographical, mobile, global).

% Maintain Bitcoin Core reference implementation; guardian of the 1MB limit (via consensus rules). Identity-locked: 'protecting decentralization' is the professional and ideological identity of this group. Their legitimacy derives from the store-of-value reading's framing. Could change the limit but would fracture their identity and community.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Collect transaction fees and block subsidies. Constrained by 1MB limit on fee revenue per block, but benefit from fee market bidding wars during congestion. Arbitrage-grade exit: can redirect hashpower to other SHA-256 chains (BCH, BSV) or mine empty blocks, but Bitcoin's liquidity and price premium make exit costly. Dual-positioned: pay opportunity cost from capacity limit, collect rents from fee pressure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, miners, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, miners, beneficiary).

% Analyze Bitcoin's monetary properties, fee markets, and scaling tradeoffs from outside the protocol. No skin in the game; provide the analytical seat that sees the full structure including the kernel contest between readings.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, monetary_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the Byzantine Generals Problem for a global monetary ledger without trusted coordinators: enables any participant to independently verify the entire transaction history and supply schedule on commodity hardware, ensuring no single party can inflate or censor.
% TRANSFER_FUNCTION: Moves transaction throughput from the base layer (capped at ~3-7 TPS by 1MB blocks) to layer-2 systems (Lightning) and alternative chains. Transfers fee revenue from base-layer transactors to miners (via fee market) and routing revenue to Lightning operators. Transfers the burden of custody and liquidity management from the protocol to users.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the structural transfer: base-layer transactors pay fees that clear at monopoly-rent levels during congestion, while holders capture the scarcity premium. Suppression (0.45) is structural not coercive: the protocol rules themselves exclude high-throughput use cases; no active censorship needed — the capacity ceiling does the work. Theater_ratio (0.25) is low because the constraint genuinely delivers its stated coordination good (verifiable decentralization); the extraction is a byproduct, not a performance. Accessibility_collapse (0.58) is moderate: alternatives exist (Lightning, altcoins, custodial solutions) but each carries significant tradeoffs (trust, complexity, liquidity). Resistance (0.52) reflects the ongoing but contained block-size debate; the 2017 resolution (SegWit + 1MB base) settled the governance contest for now.
 *
 * PERSPECTIVAL GAP:
 *   From the node_operator/core_developer seat: this is a Mountain — the 1MB limit is the price of decentralization, a non-negotiable natural law of the system. From the low_fee_transaction_user seat: this is a Snare — the coordination story ('decentralization') is cover for extracting fee revenue and protecting holder wealth. From the long_term_holder seat: this is a Rope — everyone benefits from a credibly scarce, censorship-resistant asset. The engine computes these divergences from the structural data; the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and node operators sit at the beneficiary end (d ≈ 0.1-0.2): the constraint subsidizes their position (scarcity premium, low verification cost). Low-fee transaction users and global south micropayment users sit at the target end (d ≈ 0.8-0.95): they bear the full extraction with constrained/trapped exit. Lightning operators are beneficiaries (d ≈ 0.15) — the constraint creates their market. Core developers are agenda_setters with identity-locked exit (d ≈ 0.1): they administer the constraint and their identity fuses with its maintenance. Miners are dual-positioned (d ≈ 0.5): they collect fee rents but face opportunity cost from the capacity cap. Monetary economists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trust-minimized digital cash) is contested: store-of-value proponents say the problem is solved (Bitcoin exists, is decentralized, is verifiable); electronic_cash proponents say the problem is alive (Bitcoin fails as cash for most users). The arrangement persists because the mandatrophy is unresolved — the coordination function (decentralization) is real and live, but the extraction function (pricing out transactional use) has grown. Not a piton because the constraint is actively maintained and enforced by identity-locked agenda_setters, not inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the store_of_value_reading a genuine structural interpretation of the whitepaper, or a post-hoc rationalization for the 1MB limit retained after the 2017 block size war?',
    'Textual analysis of whitepaper passages cited by each reading; historical reconstruction of when ''store of value'' framing became dominant vs. ''electronic cash'' framing; correlate with miner/developer incentive shifts.',
    'If post-hoc rationalization, the constraint''s coordination claim is weakened and extraction profile increases; if genuine interpretation, the tangled_rope classification holds with coordination as authentic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the store-of-value reading reflects the whitepaper''s original structure or a later reinterpretation.').

omega_variable(
    electronic_cash_structural_delta,
    'What would the electronic_cash_reading change structurally if it were the binding constraint?',
    'Counterfactual modeling: simulate Bitcoin with larger blocks (e.g., 8MB, 32MB) or adaptive block size — measure node count decentralization, fee levels, miner centralization, and censorship resistance over time.',
    'If electronic_cash_reading''s predicted outcomes (low fees, adequate decentralization) are plausible, the store_of_value_reading''s extraction is revealed as a choice not a necessity; if electronic_cash_reading leads to centralization/capture, the store_of_value_reading''s coordination claim is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electronic_cash_structural_delta, empirical, 'The structural consequences of adopting the sibling reading''s parameter choices.').

omega_variable(
    disagreement_location,
    'At what specific structural element do the readings diverge: the whitepaper title (''cash''), the technical design (1MB implicit in SPV/merkle proofs), the threat model (state vs. commercial), or the time horizon (bootstrap vs. steady state)?',
    'Map each reading''s argument to specific whitepaper sections and implicit parameters; identify the minimal set of premises whose truth-values differ between readings.',
    'If divergence is at the title only, the readings may be reconciled; if at threat model or time horizon, they are structurally incommensurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'The precise structural locus of disagreement between store_of_value_reading and electronic_cash_reading.').

omega_variable(
    lightning_as_extraction_mechanism,
    'Is Lightning Network a genuine scaling solution that preserves user sovereignty, or an extraction mechanism that captures transaction users in custodial relationships while preserving the base layer''s scarcity rent?',
    'Longitudinal study of Lightning adoption patterns: custodial vs. self-custody ratios, liquidity provider concentration, user loss rates, fee comparison to on-chain and altcoins.',
    'If predominantly custodial/extractive, the store_of_value_reading''s coordination claim is undermined — the constraint creates a new extraction layer. If predominantly self-custodial/sovereign, the coordination function extends to layer 2.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lightning_as_extraction_mechanism, empirical, 'Whether the designated off-chain scaling solution extends or subverts the coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_sov_tr_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(btc_sov_tr_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2011, 0.08).
narrative_ontology:measurement(btc_sov_tr_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(btc_sov_tr_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(btc_sov_tr_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement(btc_sov_tr_t2019, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2019, 0.24).
narrative_ontology:measurement(btc_sov_tr_t2021, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(btc_sov_tr_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(btc_sov_be_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement(btc_sov_be_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2011, 0.1).
narrative_ontology:measurement(btc_sov_be_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2013, 0.15).
narrative_ontology:measurement(btc_sov_be_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(btc_sov_be_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(btc_sov_be_t2019, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(btc_sov_be_t2021, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(btc_sov_be_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(btc_sov_su_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(btc_sov_su_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2011, 0.1).
narrative_ontology:measurement(btc_sov_su_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2013, 0.15).
narrative_ontology:measurement(btc_sov_su_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(btc_sov_su_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement(btc_sov_su_t2019, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement(btc_sov_su_t2021, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2021, 0.44).
narrative_ontology:measurement(btc_sov_su_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.08).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_liquidity_constraint).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miner_fee_market).

% DUAL FORMULATION NOTE:
% This constraint and electronic_cash_reading form a constraint family decomposing the 'Bitcoin whitepaper purpose' kernel. The store_of_value_reading has higher extractiveness (0.68 vs. estimated ~0.35 for electronic_cash_reading) because it accepts capacity constraint as binding; electronic_cash_reading would raise capacity, reducing fee extraction but increasing node operation cost (different extraction profile). Both claim coordination function; they differ on which coordination good is supreme (verifiability vs. transactional accessibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, organized, 0.2).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, moderate, 0.8).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
