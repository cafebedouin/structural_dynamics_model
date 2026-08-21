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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (Whitepaper Reading)
 *   domain: Cryptocurrency Economics / Monetary Systems / Technology Governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'peer-to-peer electronic cash'
 *   reading of the Bitcoin whitepaper. It describes Bitcoin as a system
 *   intended for direct, censorship-resistant electronic transactions,
 *   prioritizing low fees and broad accessibility. The story highlights the
 *   divergence between this founding vision and Bitcoin's current operational
 *   reality, where high transaction fees and limited throughput have made it
 *   impractical for everyday transactional use, effectively extracting from
 *   those who would use it as cash.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.78).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.85).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (Whitepaper Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "Cryptocurrency Economics / Monetary Systems / Technology Governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'd2770958-6d90-411e-b11b-61bf0edec59d').
narrative_ontology:cs_kernel_codification('d2770958-6d90-411e-b11b-61bf0edec59d', fixed_text).
narrative_ontology:cs_authority_grounding('d2770958-6d90-411e-b11b-61bf0edec59d', practice).
narrative_ontology:cs_interpretation_layer_present('d2770958-6d90-411e-b11b-61bf0edec59d').
narrative_ontology:cs_reading_relation('d2770958-6d90-411e-b11b-61bf0edec59d', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2770958-6d90-411e-b11b-61bf0edec59d', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('d2770958-6d90-411e-b11b-61bf0edec59d', foundational, low_fees_for_all_transactions).
narrative_ontology:cs_axiom_status(low_fees_for_all_transactions, holdable).
narrative_ontology:cs_axiom_grounding('d2770958-6d90-411e-b11b-61bf0edec59d', low_fees_for_all_transactions, empirically_contingent).
narrative_ontology:cs_axiom('d2770958-6d90-411e-b11b-61bf0edec59d', foundational, permissionless_transactional_access).
narrative_ontology:cs_axiom_status(permissionless_transactional_access, holdable).
narrative_ontology:cs_axiom_grounding('d2770958-6d90-411e-b11b-61bf0edec59d', permissionless_transactional_access, deontological).
narrative_ontology:cs_reference_frame('d2770958-6d90-411e-b11b-61bf0edec59d', satoshi_vision_p2p_cash).
narrative_ontology:cs_drift_state('d2770958-6d90-411e-b11b-61bf0edec59d', contemporary_fee_market_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d2770958-6d90-411e-b11b-61bf0edec59d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_bitcoin_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_individuals).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, digital_gold_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who wish to use Bitcoin for everyday purchases or microtransactions, but are priced out by high transaction fees, making direct electronic cash use impractical.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_transaction_users, payer,
    powerless, immediate, constrained, global).

% Populations without access to traditional banking services who could benefit from a censorship-resistant, low-cost medium of exchange, but find Bitcoin's current fee structure prohibitive for their needs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_individuals, payer,
    powerless, immediate, constrained, global).

% Businesses that wish to accept Bitcoin for goods and services, but face high transaction costs and slow confirmation times, making it less competitive than traditional payment rails for many transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin, payer,
    moderate, biographical, constrained, global).

% Operators of specialized hardware that validate transactions and secure the network. They benefit directly from high transaction fees, which are a primary incentive for their work, and enforce the network's block size limits.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter,
    organized, biographical, mobile, global).

% A subset of the Bitcoin core development community who prioritize the original whitepaper's vision of Bitcoin as a peer-to-peer electronic cash system, often advocating for solutions like larger block sizes or second-layer scaling to reduce fees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developers_p2p_cash_vision, agenda_setter,
    organized, generational, identity_locked, global).

% Individuals or entities holding significant amounts of Bitcoin, who often benefit from the 'digital gold' narrative and see high transaction fees as a necessary consequence of scarcity, which reinforces Bitcoin's store-of-value proposition.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_bitcoin_holders, beneficiary,
    powerful, generational, mobile, global).

% A community of users and developers who primarily view Bitcoin as a store of value, akin to digital gold, and are less concerned with its use as a medium of exchange, often opposing changes that might compromise its scarcity or security for transactional throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, digital_gold_advocates, beneficiary,
    organized, generational, identity_locked, global).

% A community of users and developers who prioritize the stability and immutability of the Bitcoin protocol, resisting significant changes (like block size increases) even if they would facilitate the p2p cash vision, believing that stability is the primary virtue.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Other blockchain projects that offer lower transaction fees and higher throughput, aiming to fulfill the 'electronic cash' role. They are excluded from Bitcoin's network effect and brand recognition, but offer a competitive alternative for transactional use.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, alternative_cryptocurrencies, excluded,
    powerful, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable direct electronic transactions between parties without the need for a trusted third party, providing a censorship-resistant medium of exchange.
% TRANSFER_FUNCTION: Facilitates the transfer of value (Bitcoin) from one party to another. Currently, it also transfers significant transaction fees from users to miners.
% ABSENT_VOICES: Millions of unbanked individuals globally who could benefit from low-cost, permissionless digital cash, and early adopters who envisioned Bitcoin as a cheap transactional layer. Their needs are often sidelined by the 'digital gold' narrative and protocol stability concerns.
% DISAPPEARANCE_RATIONALE: If Bitcoin vanished overnight, the entire cryptocurrency market would experience a catastrophic shock. The vision of decentralized digital cash would lose its most prominent (if currently compromised) instantiation, forcing a reorganization of efforts and capital into alternative projects or new infrastructure.
% FOUNDING_PROBLEM: The problem of 'electronic cash that allows online payments to be sent directly from one party to another without going through a financial institution,' as articulated in the Bitcoin whitepaper.
% FOUNDING_PROBLEM_CORROBORATION: The original Bitcoin whitepaper, early forum discussions, and statements from Satoshi Nakamoto strongly corroborate the founding problem. However, a significant portion of the current Bitcoin community (digital gold advocates, protocol ossification advocates) contests whether this remains its primary or most important function, arguing for a shift towards store-of-value. Independent economic analyses also highlight the shift in Bitcoin's use case from transactional to settlement/store-of-value.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the substantial transaction fees that users must pay, which are disproportionate for small transactions and effectively transfer wealth to miners and large holders. Suppression (0.85) is severe because the fixed block size limit and resulting fee market actively suppress the ability to conduct low-cost, high-volume transactions, making the 'p2p cash' ideal largely inaccessible. The theater ratio (0.45) indicates that while the rhetoric of 'p2p cash' persists, a significant portion of the network's activity and development focus has shifted towards its 'digital gold' or store-of-value function, with the original transactional utility becoming more performative. Accessibility collapse (0.80) is high because for many potential users, especially in developing economies, the cost of a Bitcoin transaction makes it completely inaccessible as a medium of exchange. Resistance (0.60) is moderate, evidenced by ongoing debates about block size, the development of second-layer solutions (like Lightning Network), and the emergence of alternative cryptocurrencies explicitly targeting the 'electronic cash' use case.
 *
 * PERSPECTIVAL GAP:
 *   The 'p2p cash' advocates experience Bitcoin as a Tangled Rope, where a genuine coordination function (decentralized transactions) is undermined by asymmetric extraction (high fees) and suppression (limited throughput). In contrast, 'digital gold' advocates might experience the same system as a Rope or even a Mountain, seeing high fees as a natural consequence of scarcity and a necessary trade-off for security and store-of-value properties. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   From the perspective of this reading, small transaction users, unbanked individuals, and merchants are the primary targets (victims) of the constraint, as they bear the costs of high fees and limited access. Bitcoin miners and large Bitcoin holders are beneficiaries, as they profit from the fee market and the 'digital gold' narrative that often accompanies it. Core developers advocating for the p2p cash vision are agenda-setters, but their influence is contested by other factions. Digital gold and protocol ossification advocates are beneficiaries of the current state, as it aligns with their priorities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''p2p_cash_reading'' of the Bitcoin whitepaper, or is it conflating with other interpretations?',
    'Analysis of historical primary sources (Satoshi''s writings, early forum posts) and contemporary advocacy from proponents of this specific reading.',
    'If conflated, the metrics and classification may be inaccurate, requiring decomposition into more precise readings. If accurate, it strengthens the validity of this specific constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the fidelity of this story to the ''p2p cash'' interpretation.').

omega_variable(
    block_size_consensus_ambiguity,
    'Is the current block size limit a technical necessity for decentralization and security, or a policy choice driven by specific ideological factions?',
    'Empirical analysis of network performance and security under various block size scenarios, alongside historical documentation of the ''block size wars'' and the motivations of key actors.',
    'If a policy choice, the suppression metric is more directly attributable to human agency and less to ''natural'' network limits, potentially increasing the effective extractiveness for victims. If a technical necessity, the constraint leans more towards a Mountain for network limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_consensus_ambiguity, empirical, 'Ambiguity regarding the nature of Bitcoin''s block size limit.').

omega_variable(
    transaction_fee_market_naturalness,
    'Are high transaction fees a natural, unavoidable market outcome for a scarce block space, or a consequence of artificial scarcity maintained by protocol governance?',
    'Comparative economic analysis with other blockchain networks and traditional payment systems, alongside counterfactual modeling of different block size policies.',
    'If natural, the extractiveness is an inherent cost of the system. If artificial, the extractiveness is a rent captured by miners and large holders, strengthening the Snare-like aspects of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transaction_fee_market_naturalness, empirical, 'Whether high fees are a natural market outcome or policy-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2009, 0.1).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2012, 0.25).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2012, 0.2).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'Bitcoin Whitepaper' kernel. This 'p2p_cash_reading' focuses on transactional utility, while 'digital_gold_reading' emphasizes store-of-value, and 'protocol_ossification_reading' prioritizes immutability. Each reading represents a structurally different constraint, linked here to show their interdependencies within the broader Bitcoin ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
