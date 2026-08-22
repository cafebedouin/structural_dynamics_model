% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Bitcoin Whitepaper Purpose — Electronic Cash Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'electronic cash' reading of the
 *   Bitcoin whitepaper's purpose: the title's 'cash' telos is binding,
 *   requiring the system to support everyday transactional use with low fees.
 *   This reading demands expanded on-chain capacity (8MB+ blocks),
 *   prioritizes merchant payment adoption, and identifies payment processors
 *   and low-value transactors as beneficiaries while node operators bear
 *   storage and bandwidth costs. The sibling reading (store_of_value_reading)
 *   subordinates on-chain capacity to decentralization and full-node
 *   verifiability. The two readings are distinct constraints with different ε
 *   values, different beneficiary/victim structures, and different types —
 *   linked by the kernel they contest.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper Purpose — Electronic Cash Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '6984494b-faab-4c7c-91c4-00b05b3fa3d9').
narrative_ontology:cs_kernel_codification('6984494b-faab-4c7c-91c4-00b05b3fa3d9', fixed_text).
narrative_ontology:cs_authority_grounding('6984494b-faab-4c7c-91c4-00b05b3fa3d9', distributed).
narrative_ontology:cs_reading_relation('6984494b-faab-4c7c-91c4-00b05b3fa3d9', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('6984494b-faab-4c7c-91c4-00b05b3fa3d9', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('6984494b-faab-4c7c-91c4-00b05b3fa3d9', foundational, whitepaper_title_cash_telos_binding).
narrative_ontology:cs_axiom_status(whitepaper_title_cash_telos_binding, holdable).
narrative_ontology:cs_axiom_grounding('6984494b-faab-4c7c-91c4-00b05b3fa3d9', whitepaper_title_cash_telos_binding, conventional).
narrative_ontology:cs_axiom('6984494b-faab-4c7c-91c4-00b05b3fa3d9', foundational, everyday_transactional_use_required).
narrative_ontology:cs_axiom_status(everyday_transactional_use_required, holdable).
narrative_ontology:cs_axiom_grounding('6984494b-faab-4c7c-91c4-00b05b3fa3d9', everyday_transactional_use_required, instrumental).
narrative_ontology:cs_axiom('6984494b-faab-4c7c-91c4-00b05b3fa3d9', secondary, low_fees_mandatory).
narrative_ontology:cs_axiom_status(low_fees_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('6984494b-faab-4c7c-91c4-00b05b3fa3d9', low_fees_mandatory, instrumental).
narrative_ontology:cs_reference_frame('6984494b-faab-4c7c-91c4-00b05b3fa3d9', whitepaper_electronic_cash_system).
narrative_ontology:cs_drift_state('6984494b-faab-4c7c-91c4-00b05b3fa3d9', post_block_size_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6984494b-faab-4c7c-91c4-00b05b3fa3d9', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, storage_bandwidth_bearers).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, whitepaper_title_cash_telos_binding).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, everyday_transactional_use_required).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, low_fees_mandatory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build businesses processing on-chain payments at low fees; benefit from expanded block space and transaction volume. Can pivot to other chains if on-chain capacity constraints persist.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Need affordable on-chain transactions for daily purchases; high fees exclude them entirely. Limited alternatives — custodial wallets reintroduce trust, Lightning requires technical competence and liquidity management.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Push for on-chain scaling to enable merchant point-of-sale adoption; benefit from network effects of payment use. Can advocate for protocol changes or migrate to competing payment-focused chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_advocates, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_advocates, agenda_setter).

% Bear storage, bandwidth, and compute costs of larger blocks; validate all transactions to enforce consensus rules. Cannot easily exit — running a node is identity-locked for many (ideological commitment to verification), but hardware costs scale with block size.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    moderate, generational, constrained, global).

% Absorb marginal cost of block propagation and state growth; includes hobbyist node runners in bandwidth-constrained regions. No voice in governance; exit means stopping validation, which undermines the security model they depend on.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, storage_bandwidth_bearers, payer,
    powerless, immediate, trapped, global).

% Maintain reference implementation; mediate between competing scaling proposals through BIP process. Their authority derives from technical stewardship, not formal power — but their merge decisions shape the live protocol.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_developers, agenda_setter,
    institutional, generational, analytical, global).

% Hold the competing reading: decentralization and full-node verifiability are the binding constraints; on-chain capacity is subordinated. Would object to unbounded block growth as existential threat to the value proposition. Their exclusion from the electronic-cash framing is structural — the two readings cannot both be the primary telos.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_proponents, excluded,
    organized, civilizational, identity_locked, global).

% Analyze tradeoffs between on-chain scaling, layer-2 solutions, and decentralization preservation. No direct stake; provide empirical grounding for capacity-fee-decentralization surface.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, independent_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a permissionless, censorship-resistant electronic cash system for everyday peer-to-peer transactions without trusted intermediaries — solving the double-spend problem in a decentralized network.
% TRANSFER_FUNCTION: Moves transaction fees from transactors (payers) to miners (block producers) and absorbs node operation costs across all validators; expanded blocks shift cost burden from fee-payers to node operators via increased resource requirements.
% ABSENT_VOICES: Store-of-value proponents and long-term holders who prioritize unchanging monetary policy over payment utility are structurally excluded from the electronic-cash telos — their objection is that this reading redefines Bitcoin's purpose away from its achieved success as digital gold.
% DISAPPEARANCE_RATIONALE: If the electronic-cash reading vanished overnight, the protocol would likely ossify around the store-of-value reading: block size would remain capped, fee markets would become permanent, and Lightning/layer-2 would become the only path for payments — the 'cash' use case would be abandoned as a first-class goal.
% FOUNDING_PROBLEM: The 2008 financial crisis exposed the fragility of trusted intermediaries in electronic payments; the whitepaper proposed a peer-to-peer electronic cash system that removed the need for trust in banks or payment processors.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself (Nakamoto 2008) is the primary corroboration for the electronic-cash reading — but the store-of-value reading cites the same text's emphasis on fixed supply and decentralization as evidence of a different founding problem. No external source corroborates one reading over the other; the contest is internal to the kernel's interpretive community.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the cost shift from transactors to node operators under expanded blocks: fee-payers benefit from low fees, but validators absorb unbounded resource growth. Suppression (0.55) captures the active enforcement needed to prevent block size increases (the 1MB cap, later SegWit's effective limit) — the constraint's persistence depends on suppressing the electronic-cash alternative. Theater ratio (0.28) is moderate: the 'decentralization' justification for small blocks is real but increasingly performs as cover for store-of-value entrenchment. The measurement series shows the 2017 block size war as an inflection: extractiveness and suppression spiked during the fork conflict, theater peaked as both sides performed ideological purity, then partially receded as the store-of-value reading consolidated control.
 *
 * PERSPECTIVAL GAP:
 *   From the electronic-cash seat, the constraint is a rope: genuine coordination for permissionless payments, with costs distributed across willing participants. From the node-operator seat, it is a snare: they are forced to subsidize low fees for others with no exit. From the store-of-value seat (excluded), the electronic-cash reading itself is a snare threatening the kernel's achieved value. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors and low-value transactors are structural beneficiaries (d ~0.2) — they gain from low fees and on-chain access. Node operators are structural targets (d ~0.8) — they bear unbounded cost growth with no compensation mechanism. Storage/bandwidth bearers in constrained regions are trapped targets (d ~0.95) — they cannot exit validation without losing security guarantees. Protocol developers sit near symmetric (d ~0.5) — they mediate but their stewardship role gives them agenda-setting power. Store-of-value proponents are excluded (identity-locked) — their exit is foreclosed by ideological commitment to the competing reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trust-minimized electronic cash) remains live — custodial solutions and layer-2 systems have not fully solved it for global, low-value, non-technical users. But the arrangement built to solve it (on-chain scaling) has been suppressed by a competing reading that captured the protocol's governance. The electronic-cash reading persists as a contested mandate — not resolved, not dead, actively disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the electronic-cash reading a structurally distinct constraint from the store-of-value reading, or do they represent different measurement bases of the same constraint?',
    'Apply the ε-invariance test: if the beneficiary/victim structure, extractiveness profile, and suppression mechanisms differ irreducibly between the two readings, they are distinct constraints. The whitepaper title binds ''cash'' for one reading; the other reading binds ''decentralization'' from the same text — different telos, different structural commitments.',
    'If distinct, each reading gets its own classification and the kernel is a family of linked constraints. If same constraint, the framework must model observable-dependent ε — which violates DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s contested readings instantiate separate constraints per ε-invariance.').

omega_variable(
    store_of_value_capture_timing,
    'When did the store-of-value reading capture the protocol''s governance — 2017 (block size war resolution), 2013-2015 (early developer consensus), or gradually without a decisive moment?',
    'Trace protocol decision records (BIPs, mailing list consensus, fork outcomes) against the beneficiary structure of each reading. The 2017 SegWit2x failure is the clearest candidate for capture moment, but earlier developer alignment may have pre-determined it.',
    'If capture was early and structural, the electronic-cash reading was never the live governance reading — it was a defeated faction from near-inception. If capture was 2017, the electronic-cash reading was the live governance reading until actively displaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(store_of_value_capture_timing, empirical, 'Timing and mechanism of store-of-value reading''s governance capture.').

omega_variable(
    node_operator_cost_bearing_capacity,
    'What is the actual marginal cost curve for node operation as block size scales, and at what threshold does it exclude non-institutional validators?',
    'Empirical measurement of storage, bandwidth, and compute costs for full validation at various block sizes across geographic and economic contexts. Compare against hobbyist operator demographics.',
    'If costs scale linearly and remain accessible to hobbyists globally at 8MB+, the electronic-cash reading''s victim claim is exaggerated. If costs scale superlinearly or exclude regional operators at modest increases, the victim structure is real and the extraction is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(node_operator_cost_bearing_capacity, empirical, 'Whether node operator victimhood under expanded blocks is empirically grounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2009, 0.02).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2012, 0.03).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2021, 0.32).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2012, 0.08).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2019, 0.52).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2023, 0.53).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_scaling_governance).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, lightning_network_adoption).

% DUAL FORMULATION NOTE:
% This constraint and store_of_value_reading form a constraint family decomposed from the bitcoin_whitepaper_purpose kernel. Electronic-cash reading: ε=0.62, beneficiaries=payment_processors/low_value_transactors, victims=node_operators, type=tangled_rope. Store-of-value reading: ε≈0.15 (low extraction from holders), beneficiaries=long_term_holders, victims=payment_users (excluded from on-chain), type=rope or mountain from its seat. They share the kernel text but instantiate different structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, moderate, 0.8).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
