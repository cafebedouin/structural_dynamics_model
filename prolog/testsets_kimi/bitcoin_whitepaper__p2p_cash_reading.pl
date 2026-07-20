% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Bitcoin Base-Layer P2P Electronic Cash Constraint
 *   domain: economic/monetary/technological
 *
 * SUMMARY:
 *   This constraint instantiates the p2p_cash_reading of the
 *   bitcoin_whitepaper kernel, which holds that Bitcoin is defined by its
 *   function as a censorship-resistant medium of exchange for direct
 *   electronic transactions. Under this reading, low fees and block size
 *   expansion are legitimate mechanisms. The actual protocol enforces a
 *   constrained block size that produces a fee market, extracting from retail
 *   transactors who are priced out of base-layer settlement while still
 *   coordinating global final settlement for those who can pay. The sibling
 *   digital_gold_reading treats fee markets as security-preserving scarcity,
 *   while the protocol_ossification_reading treats any base-layer expansion
 *   as illegitimate.
 *
 * KEY AGENTS:
 *   - miners: primary agenda-setter and beneficiary (organized/constrained) â enforces consensus and collects fees
 *   - retail_transactors: primary target (powerless/constrained) â priced out by fee markets
 *   - sovereign_settlement_users: primary beneficiary (powerful/mobile) â purchases censorship-resistant settlement
 *   - core_protocol_maintainers: secondary agenda-setter (institutional/mobile) â sets throughput parameters
 *   - unbanked_emerging_market_users: excluded voice (powerless/trapped) â needs low-fee rails but absent from governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.55).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.45).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin Base-Layer P2P Electronic Cash Constraint").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/monetary/technological").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'd0869954-35ea-440d-9397-9a98208afc16').
narrative_ontology:cs_kernel_codification('d0869954-35ea-440d-9397-9a98208afc16', fixed_text).
narrative_ontology:cs_authority_grounding('d0869954-35ea-440d-9397-9a98208afc16', distributed).
narrative_ontology:cs_reading_relation('d0869954-35ea-440d-9397-9a98208afc16', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0869954-35ea-440d-9397-9a98208afc16', bitcoin_whitepaper__protocol_ossification_reading, forecloses).
narrative_ontology:cs_axiom('d0869954-35ea-440d-9397-9a98208afc16', foundational, peer_to_peer_electronic_cash_is_kernel_purpose).
narrative_ontology:cs_axiom_status(peer_to_peer_electronic_cash_is_kernel_purpose, holdable).
narrative_ontology:cs_axiom_grounding('d0869954-35ea-440d-9397-9a98208afc16', peer_to_peer_electronic_cash_is_kernel_purpose, conventional).
narrative_ontology:cs_axiom('d0869954-35ea-440d-9397-9a98208afc16', foundational, block_size_increase_preserves_decentralization).
narrative_ontology:cs_axiom_status(block_size_increase_preserves_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('d0869954-35ea-440d-9397-9a98208afc16', block_size_increase_preserves_decentralization, empirically_contingent).
narrative_ontology:cs_reference_frame('d0869954-35ea-440d-9397-9a98208afc16', whitepaper_p2p_cash_intent).
narrative_ontology:cs_drift_state('d0869954-35ea-440d-9397-9a98208afc16', post_2017_fee_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0869954-35ea-440d-9397-9a98208afc16', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, sovereign_settlement_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, retail_transactors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Secure the network by producing blocks and enforcing consensus rules including the block size limit. They collect transaction fees and block subsidies. Their infrastructure is locked into SHA-256 mining, making exit to other consensus systems costly.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners, beneficiary).

% Use Bitcoin for everyday payments, remittances, or small-value transfers. When on-chain fees rise above a few dollars, they are priced out of base-layer settlement and must rely on custodial exchanges or complex second-layer infrastructure that reintroduces counterparty risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, retail_transactors, payer,
    powerless, immediate, constrained, global).

% Move large capital sums on-chain and can afford volatile fees during congestion. They benefit from Bitcoin's censorship resistance and final settlement guarantees, which the fee market pays for without excluding them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, sovereign_settlement_users, beneficiary,
    powerful, biographical, mobile, global).

% Maintain the Bitcoin Core reference implementation and influence consensus parameter defaults, including block weight limits. Their technical decisions directly determine base-layer throughput and the intensity of the fee market.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_protocol_maintainers, agenda_setter,
    institutional, generational, mobile, global).

% Need censorship-resistant payment rails for subsistence-level transactions but lack access to banking, Lightning infrastructure, or the capital to absorb high base-layer fees. They are structurally absent from protocol governance and developer prioritization.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_emerging_market_users, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides global, permissionless, censorship-resistant final settlement of electronic value transfers without trusted intermediary clearance or account-based exclusion.
% TRANSFER_FUNCTION: Moves purchasing power from transactors seeking block inclusion to miners via transaction fees; moves economic access from low-value transactors to high-fee bidders during congestion.
% ABSENT_VOICES: Unbanked populations in high-inflation economies and small merchants who accepted Bitcoin in the early low-fee era are priced out of on-chain settlement and excluded from protocol governance. They would argue for base-layer capacity expansion but are not in the room where consensus parameters are set.
% DISAPPEARANCE_RATIONALE: If the base-layer censorship-resistant settlement mechanism disappeared, unbanked remittance corridors, grey-market commerce, and sovereign wealth preservation flows would lose their primary non-intermediated rail and be forced back onto regulated banking or less secure altcoin networks.
% FOUNDING_PROBLEM: Double-spending in digital cash without a trusted third party; creating a non-sovereign medium of exchange for direct electronic transactions between parties.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper and early cypherpunk literature attest the p2p electronic cash founding problem from within the kernel tradition. Independent development economists and Global South remittance researchers corroborate that low-fee digital cash remains an unsolved problem, while institutional custody providers and monetary hedge funds argue the founding problem has shifted to store-of-value settlement.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness rises from negligible to 0.55 as block space saturates and fee markets price out low-value transactions. Theater rises to 0.45 because the p2p electronic cash narrative is maintained in protocol culture and whitepaper citation even as empirical base-layer usage shifts toward high-value settlement. Suppression spikes during the block size wars (time points 6-9) as social consensus enforces the small-block roadmap and marginalizes fork alternatives, then moderates as the new equilibrium stabilizes. Accessibility collapse is 0.60 because alternatives (altcoins, Lightning, custodial layers) exist but require significant friction, counterparty risk, or capital. Resistance is 0.50 reflecting the ongoing big-block dissent, hard forks, and competing chain formations.
 *
 * PERSPECTIVAL GAP:
 *   From the miner and sovereign settlement seats, the fee market is a necessary cost of decentralized security and anti-censorship infrastructure; from the retail transactor and unbanked seats, the same structure operates as extraction that denies access to the base layer. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners sit near the beneficiary end: they collect fees and enforce the scarcity. Sovereign settlement users also sit near the beneficiary end: they capture censorship-resistant finality subsidized by the fee market. Retail transactors sit near the full-target end: they bear the cost of fee-driven exclusion with constrained exit. Unbanked emerging market users are excluded rather than coordinated â their absence from the fee market is the structural result.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of p2p electronic cash is contested: the whitepaper articulates it, but empirical base-layer fee behavior has shifted practical usage toward store-of-value settlement. Mislabeling the constraint as a snare would ignore the genuine coordination function (global censorship-resistant settlement); mislabeling it as a rope would ignore the asymmetric extraction (retail transactors priced out). Tangled rope captures both: the constraint coordinates real final settlement while actively enforcing a scarcity that extracts from low-value users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_market_necessity,
    'Is the base-layer fee market an inevitable result of decentralized security budgeting, or an artifact of the protocol''s chosen block size limit?',
    'Natural experiment via a block size increasing fork: if security decentralization collapses, the fee market is structurally necessary; if security holds, the scarcity is artificial extraction.',
    'If artificial, the constraint is more extractive and the victim set larger; if necessary, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_necessity, conceptual, 'Whether fee market scarcity is structurally necessary or constructed.').

omega_variable(
    p2p_cash_viability,
    'Can the base layer still serve as p2p electronic cash given empirical fee market behavior, or has the kernel drifted to a different function?',
    'Longitudinal analysis of median transaction value versus median fee; if median fee consistently exceeds everyday transaction thresholds, the p2p cash reading is empirically overridden.',
    'If empirically overridden, this reading''s axioms may shift to overridden status and the constraint reclassifies toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p2p_cash_viability, empirical, 'Whether empirical fee behavior overrides the p2p cash reading.').

omega_variable(
    kernel_reading_legitimacy,
    'Does the p2p cash reading retain enough protocol-adherent support to be considered a live reading, or has it been structurally displaced by the digital gold reading?',
    'Measure developer commit activity, node signaling, and community governance participation aligned with each reading.',
    'Determines whether this reading is holdable or overridden within the kernel''s tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Whether p2p cash reading remains live within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btcp2p_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(btcp2p_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement(btcp2p_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(btcp2p_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.45).
narrative_ontology:measurement(btcp2p_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(btcp2p_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(btcp2p_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(btcp2p_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.1).
narrative_ontology:measurement(btcp2p_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(btcp2p_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(btcp2p_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(btcp2p_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(btcp2p_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(btcp2p_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement(btcp2p_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(btcp2p_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.6).
narrative_ontology:measurement(btcp2p_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(btcp2p_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
