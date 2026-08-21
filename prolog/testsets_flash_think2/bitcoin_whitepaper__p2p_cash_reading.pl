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
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'p2p_cash_reading' of the Bitcoin
 *   whitepaper, which emphasizes Bitcoin's role as a censorship-resistant
 *   medium for direct electronic transactions. From this perspective, the
 *   current state of Bitcoin, characterized by high transaction fees and
 *   limited throughput, represents a significant departure from its founding
 *   vision. The constraint is claimed as a 'Rope' based on its ideal
 *   function, but the metrics reflect its current, more extractive operation,
 *   particularly for small transactors. This divergence is a key measurement
 *   of the system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.75).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '2fa7962e-5af2-48bc-a496-06e3257531b7').
narrative_ontology:cs_kernel_codification('2fa7962e-5af2-48bc-a496-06e3257531b7', fixed_text).
narrative_ontology:cs_authority_grounding('2fa7962e-5af2-48bc-a496-06e3257531b7', practice).
narrative_ontology:cs_interpretation_layer_present('2fa7962e-5af2-48bc-a496-06e3257531b7').
narrative_ontology:cs_reading_relation('2fa7962e-5af2-48bc-a496-06e3257531b7', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('2fa7962e-5af2-48bc-a496-06e3257531b7', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('2fa7962e-5af2-48bc-a496-06e3257531b7', foundational, low_transaction_fees_essential).
narrative_ontology:cs_axiom_status(low_transaction_fees_essential, holdable).
narrative_ontology:cs_axiom_grounding('2fa7962e-5af2-48bc-a496-06e3257531b7', low_transaction_fees_essential, empirically_contingent).
narrative_ontology:cs_axiom('2fa7962e-5af2-48bc-a496-06e3257531b7', foundational, block_size_scalability_necessary).
narrative_ontology:cs_axiom_status(block_size_scalability_necessary, holdable).
narrative_ontology:cs_axiom_grounding('2fa7962e-5af2-48bc-a496-06e3257531b7', block_size_scalability_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('2fa7962e-5af2-48bc-a496-06e3257531b7', satoshi_vision_p2p_cash).
narrative_ontology:cs_drift_state('2fa7962e-5af2-48bc-a496-06e3257531b7', contemporary_fee_market_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2fa7962e-5af2-48bc-a496-06e3257531b7', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_users).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, users_seeking_fast_confirmations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, bitcoin_investors_digital_gold_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to use Bitcoin for direct, low-cost, censorship-resistant transactions. They benefit from the network's security and immutability but bear the cost of high transaction fees and slow confirmation times, which limit its utility as cash.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_users, payer).

% Are effectively priced out of using Bitcoin for everyday transactions due to high and volatile fees. They are victims of the network's current fee market dynamics, which prioritize larger value transfers.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_transactors, payer,
    powerless, immediate, trapped, global).

% Benefit from censorship-resistant payment rails and final settlement but face challenges with transaction fees, confirmation times, and price volatility, which can make accepting Bitcoin for goods and services impractical.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin, payer).

% Process transactions and secure the network, earning block rewards and transaction fees. From the p2p cash perspective, their economic incentives (maximizing fees) contribute to the constraint's failure to serve as cheap cash.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, agenda_setter,
    organized, generational, mobile, global).

% Advocate for the original vision of Bitcoin as peer-to-peer electronic cash, often pushing for technical solutions like block size increases or layer-2 scaling to reduce fees and improve throughput. They are constrained by network consensus and political dynamics.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developers_p2p_cash_advocates, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, core_developers_p2p_cash_advocates, observer).

% Primarily view Bitcoin as a store of value or 'digital gold,' prioritizing scarcity and immutability over transaction throughput. From the p2p cash perspective, their influence and support for high fees (as a security mechanism) contribute to the constraint's extractive nature for transactors.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_investors_digital_gold_advocates, payer,
    powerful, generational, mobile, global).

% Are structurally excluded from the direct peer-to-peer transaction layer of Bitcoin. They would offer competing services but are not part of the Bitcoin protocol's design or governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, traditional_payment_processors, excluded,
    institutional, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, trustless, and censorship-resistant medium for direct electronic transactions, eliminating the need for financial intermediaries.
% TRANSFER_FUNCTION: Facilitates the transfer of value directly between parties, with transaction fees paid to miners for network security and processing.
% ABSENT_VOICES: Small transactors and users in developing economies who are priced out by high fees; they would advocate for lower fees and greater transaction capacity to enable Bitcoin's use as everyday cash.
% DISAPPEARANCE_RATIONALE: If Bitcoin vanished, the global financial system would lose its only truly decentralized, censorship-resistant digital cash system. While other digital payment methods exist, none offer the same properties, leading to a significant rearrangement of how value is transferred outside traditional institutions.
% FOUNDING_PROBLEM: The need for 'electronic cash that would allow online payments to be sent directly from one party to another without going through a financial institution,' as articulated in the Bitcoin whitepaper.
% FOUNDING_PROBLEM_CORROBORATION: The original whitepaper and early forum discussions corroborate the founding problem. However, its 'status' is contested: p2p cash advocates argue it remains a live problem that Bitcoin is failing to solve, while digital gold advocates argue the problem has evolved or is being solved by layer-2 solutions, making the base layer a settlement network.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The 'p2p_cash_reading' views high transaction fees as a form of extraction, hence the high 'extractiveness' score (0.68). 'Suppression' is also high (0.75) because these fees effectively suppress the ability of many users to conduct small, frequent transactions, limiting Bitcoin's accessibility as cash. 'Resistance' is high (0.70) due to ongoing debates and advocacy for scaling solutions. 'Theater_ratio' is low (0.20) because the core function of censorship-resistant value transfer is still active, even if its 'cash' aspect is degraded. The temporal measurements reflect a trend of increasing extractiveness and suppression as transaction fees have risen over time, moving away from the low-cost ideal.
 *
 * PERSPECTIVAL GAP:
 *   The 'p2p_cash_reading' fundamentally clashes with the 'digital_gold_reading' and 'protocol_ossification_reading'. While p2p cash advocates see high fees as a failure, digital gold advocates may see them as a necessary security feature or a sign of Bitcoin's success as a settlement layer. The engine's per-seat classification will highlight this divergence, showing the same protocol operating as a degraded 'Tangled Rope' or 'Snare' for transactors, but potentially a 'Rope' or even 'Mountain' for those focused on its store-of-value properties.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading's perspective, 'p2p_cash_users' are beneficiaries of censorship resistance but payers of high fees. 'Small_transactors' are clear victims, priced out of the system. 'Miners' are agenda-setters who benefit from the fee market. 'Bitcoin_investors_digital_gold_advocates' are considered 'payers' in this context because their support for a high-fee, store-of-value narrative contributes to the extractive environment for cash users.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    block_size_policy_vs_technical_limit,
    'Is the current block size limit a necessary technical constraint for decentralization and security, or a policy choice that limits Bitcoin''s scalability as electronic cash?',
    'Empirical analysis of alternative scaling solutions (e.g., larger blocks, layer-2 networks) and their impact on decentralization metrics (e.g., node count, mining centralization).',
    'If a policy choice, it strengthens the argument for the constraint being a ''Snare'' or ''Tangled Rope'' for transactors, as scalability is artificially suppressed. If a technical necessity, it shifts the classification closer to a ''Mountain'' for the base layer, with extraction being an unavoidable cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(block_size_policy_vs_technical_limit, empirical, 'Ambiguity over block size as a technical vs. policy constraint.').

omega_variable(
    censorship_resistance_in_practice,
    'To what extent does Bitcoin truly achieve censorship resistance for transactions, given potential vulnerabilities at the mining pool, exchange, or internet service provider level?',
    'Observation of state-level attempts to censor transactions or block access to the network, and the network''s resilience to such attacks.',
    'If censorship resistance is found to be significantly compromised, the core ''Rope'' function of the p2p cash reading is undermined, potentially reclassifying it as a ''Piton'' (theatrical resistance) or ''Snare'' (covert control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_resistance_in_practice, empirical, 'Practical limits of Bitcoin''s censorship resistance.').

omega_variable(
    layer_2_solution_impact,
    'Do layer-2 scaling solutions (e.g., Lightning Network) genuinely restore Bitcoin''s function as peer-to-peer electronic cash, or do they introduce new forms of centralization and intermediation that contradict the original vision?',
    'Long-term adoption and decentralization metrics of layer-2 networks, assessing their accessibility, cost, and trust assumptions compared to direct on-chain transactions.',
    'If layer-2 solutions effectively restore p2p cash functionality without significant new intermediation, the base layer''s ''extractiveness'' for transactors might decrease, and the overall classification could shift closer to a ''Rope''. If they introduce new centralizing forces, the ''Snare'' or ''Tangled Rope'' aspects might simply shift to the layer-2 operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_2_solution_impact, conceptual, 'Impact of layer-2 solutions on Bitcoin''s p2p cash function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'bitcoin_whitepaper' kernel. This 'p2p_cash_reading' focuses on Bitcoin as a medium of exchange, contrasting with the 'digital_gold_reading' (store of value) and 'protocol_ossification_reading' (stability above all). Each reading yields different ε values and stakeholder experiences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
