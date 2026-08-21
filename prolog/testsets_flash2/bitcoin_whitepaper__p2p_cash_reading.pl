% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (P2P Cash Reading)
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'P2P Cash' reading of the Bitcoin
 *   whitepaper, which emphasizes Bitcoin's original intent as a
 *   censorship-resistant medium for direct electronic transactions. From this
 *   perspective, the constraint is a Tangled Rope: it provides a genuine
 *   coordination function (decentralized transactions) but has developed
 *   asymmetric extraction (high transaction fees) and suppression (limited
 *   block size, resistance to scaling solutions) that disproportionately
 *   affect low-value transactors and the unbanked. The metrics reflect the
 *   current state where the 'cash' function is significantly degraded by
 *   these factors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.4).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (P2P Cash Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '271a213b-3628-478e-96f4-5c68c7b56e71').
narrative_ontology:cs_kernel_codification('271a213b-3628-478e-96f4-5c68c7b56e71', fixed_text).
narrative_ontology:cs_authority_grounding('271a213b-3628-478e-96f4-5c68c7b56e71', distributed).
narrative_ontology:cs_reading_relation('271a213b-3628-478e-96f4-5c68c7b56e71', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('271a213b-3628-478e-96f4-5c68c7b56e71', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('271a213b-3628-478e-96f4-5c68c7b56e71', foundational, low_transaction_fees_are_essential).
narrative_ontology:cs_axiom_status(low_transaction_fees_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('271a213b-3628-478e-96f4-5c68c7b56e71', low_transaction_fees_are_essential, instrumental).
narrative_ontology:cs_axiom('271a213b-3628-478e-96f4-5c68c7b56e71', foundational, block_size_must_scale_with_demand).
narrative_ontology:cs_axiom_status(block_size_must_scale_with_demand, holdable).
narrative_ontology:cs_axiom_grounding('271a213b-3628-478e-96f4-5c68c7b56e71', block_size_must_scale_with_demand, empirically_contingent).
narrative_ontology:cs_reference_frame('271a213b-3628-478e-96f4-5c68c7b56e71', satoshi_vision_low_cost_transactions).
narrative_ontology:cs_drift_state('271a213b-3628-478e-96f4-5c68c7b56e71', contemporary_fee_market_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('271a213b-3628-478e-96f4-5c68c7b56e71', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, transaction_processors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from early network effects and low transaction costs, aligning with the original vision of cheap, direct electronic cash. They continue to benefit from the network's utility as a medium of exchange, though their influence on protocol development has waned.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, early_adopters, beneficiary,
    powerful, generational, mobile, global).

% These are the entities (miners/mining pools) that validate transactions and secure the network. While they benefit from transaction fees, this reading emphasizes their role in enabling the peer-to-peer cash function by processing transactions efficiently and at scale, ideally with low fees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, transaction_processors, agenda_setter,
    organized, biographical, constrained, global).

% Individuals or businesses attempting to use Bitcoin for small, everyday transactions. They bear the cost of rising transaction fees, which makes the network impractical for its intended 'cash' use case, effectively excluding them from direct participation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, low_value_transactors, payer,
    powerless, immediate, constrained, global).

% Individuals in regions with limited access to traditional banking, for whom Bitcoin was envisioned as a direct, low-cost alternative. High transaction fees and network congestion deny them this access, trapping them in existing financial exclusion or forcing them to use less secure alternatives.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_users, payer,
    powerless, immediate, trapped, global).

% Maintain the Bitcoin protocol. From this reading's perspective, their role is to ensure the network remains viable as a medium of exchange, which implies supporting technical changes (like block size increases) that facilitate low-cost transactions, even against resistance from other factions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developers, observer,
    institutional, generational, analytical, global).

% Proponents of Bitcoin as a store of value, who prioritize scarcity and immutability over transaction throughput. From the P2P cash reading, their influence has led to policies (e.g., small block size) that actively suppress the cash function, effectively excluding those who would use it as such.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, digital_gold_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, trustless system for direct electronic transactions, enabling individuals to send and receive value without intermediaries, thereby solving the double-spending problem.
% TRANSFER_FUNCTION: Facilitates the transfer of digital value (Bitcoin) directly between parties, bypassing traditional financial institutions. The transfer mechanism is secured by cryptographic proof and network consensus.
% ABSENT_VOICES: Users and developers who advocate for larger block sizes and lower transaction fees to restore Bitcoin's utility as electronic cash are often marginalized in protocol governance discussions, which are dominated by store-of-value narratives. The unbanked and low-value transactors are structurally excluded by high fees.
% DISAPPEARANCE_RATIONALE: If Bitcoin's censorship-resistant, peer-to-peer cash function disappeared, a significant portion of the global digital economy would lose a critical alternative to traditional finance. Users in authoritarian regimes would lose a tool for financial freedom, and the vision of a decentralized monetary system would be severely set back, forcing a reorganization around centralized alternatives or less robust cryptocurrencies.
% FOUNDING_PROBLEM: The problem of 'trusted third parties' in electronic transactions, where all financial transactions had to go through an institution, leading to high costs, censorship, and lack of privacy.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for financial privacy and inclusion, particularly in developing nations or regions with unstable financial systems, corroborate that the problem of trusted third parties and transactional censorship remains live. They point to ongoing government surveillance, capital controls, and high remittance fees as evidence that the original problem Bitcoin aimed to solve is still highly relevant.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the high transaction fees that make Bitcoin impractical for everyday use, diverting its utility from a medium of exchange. Suppression (0.40) is due to the limited block size and the social/technical resistance to scaling solutions that would enable more transactions. The theater ratio (0.20) indicates that while the core technology still functions, a portion of the 'peer-to-peer cash' narrative is maintained despite the practical reality of high fees. Resistance (0.70) is high from those who still advocate for the original vision, pushing for scaling solutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'digital gold' advocates, the current state of high fees and limited throughput is a feature, not a bug, preserving scarcity and security. From the 'P2P cash' perspective, this is a degradation of the original vision, turning a potential global cash system into a settlement layer for large transactions. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and transaction processors (miners) are beneficiaries, as they profit from the network's operation and the fees. Low-value transactors and unbanked users are victims, as they are priced out of the network's utility as cash. Core developers and digital gold advocates are observers or excluded, depending on their stance on scaling and the 'cash' vs. 'store of value' debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the constraint as a pure Snare, acknowledging its genuine coordination function (decentralized, censorship-resistant transactions). However, it also highlights the significant extraction and suppression that have accumulated, indicating a drift from its original mandate as a low-cost electronic cash system. The 'contested' status of the founding problem further underscores this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    block_size_consensus_mechanism,
    'Is the current block size limit a technical necessity for decentralization, or a social/political constraint imposed by a dominant faction?',
    'Empirical analysis of network centralization under various block size limits, or a successful, widely adopted hard fork that increases block size without compromising decentralization.',
    'If a social/political constraint, the suppression metric is higher and more directly attributable to human agency, strengthening the Tangled Rope classification. If a technical necessity, the suppression is more ''mountain-like'' and inherent to the technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_consensus_mechanism, empirical, 'Ambiguity regarding the nature of the block size limit.').

omega_variable(
    transaction_fee_market_efficiency,
    'Are high transaction fees a natural outcome of market demand for block space, or are they artificially inflated by limited supply and lack of scaling solutions?',
    'Economic modeling comparing Bitcoin''s fee market to other payment systems, or the successful implementation of layer-2 scaling solutions (e.g., Lightning Network) that significantly reduce on-chain transaction demand and fees.',
    'If fees are artificially inflated, the extractiveness metric is more severe and less justifiable as a ''coordination cost,'' pushing the classification closer to a Snare. If purely market-driven, the extraction is a consequence of network utility, though still a barrier to the ''cash'' use case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transaction_fee_market_efficiency, empirical, 'Whether transaction fees reflect genuine market dynamics or artificial scarcity.').

omega_variable(
    identity_lock_of_bitcoin_maximalists,
    'To what extent are ''digital gold'' advocates identity-locked into their interpretation, making compromise on the ''cash'' function impossible?',
    'Sociological study of Bitcoin community discourse, analysis of developer and investor behavior in response to scaling proposals, and observation of shifts in core narratives over time.',
    'If identity-locked, the ''protocol ossification'' and ''digital gold'' readings are more entrenched, making resolution of the ''cash'' vs. ''store of value'' debate a deeper, more intractable conflict, and increasing the effective suppression on the ''cash'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_of_bitcoin_maximalists, conceptual, 'The degree to which ideological commitment prevents resolution of the scaling debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.3).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Bitcoin whitepaper. This 'P2P Cash' reading focuses on its utility as a medium of exchange, contrasting with the 'Digital Gold' (store of value) and 'Protocol Ossification' (stability over change) readings. The different readings lead to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
