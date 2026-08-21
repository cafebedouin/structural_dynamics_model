% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold (Store of Value Reading)
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint describes the 'digital gold' reading of the Bitcoin
 *   whitepaper, where Bitcoin is primarily valued as a scarce, appreciating
 *   asset for long-term holding and inflation hedging. This reading
 *   prioritizes protocol stability and limited block space, leading to higher
 *   transaction fees and reduced utility as a medium of exchange. The
 *   constraint is a Tangled Rope because it provides a coordination function
 *   (secure, decentralized store of value) but also involves significant
 *   asymmetric extraction from late entrants and small transaction users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.45).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold (Store of Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '6210b8f3-f3c4-43cf-964a-e2f674fff3e0').
narrative_ontology:cs_kernel_codification('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', fixed_text).
narrative_ontology:cs_authority_grounding('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', lineage).
narrative_ontology:cs_interpretation_layer_present('6210b8f3-f3c4-43cf-964a-e2f674fff3e0').
narrative_ontology:cs_reading_relation('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', foundational, absolute_scarcity_is_paramount).
narrative_ontology:cs_axiom_status(absolute_scarcity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', absolute_scarcity_is_paramount, deontological).
narrative_ontology:cs_axiom('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', secondary, transaction_fees_are_acceptable_cost_for_security).
narrative_ontology:cs_axiom_status(transaction_fees_are_acceptable_cost_for_security, holdable).
narrative_ontology:cs_axiom_grounding('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', transaction_fees_are_acceptable_cost_for_security, instrumental).
narrative_ontology:cs_reference_frame('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', bitcoin_as_inflation_hedge).
narrative_ontology:cs_drift_state('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6210b8f3-f3c4-43cf-964a-e2f674fff3e0', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, large_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_pools).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired Bitcoin at low prices, benefiting significantly from its appreciation. They advocate for policies and narratives that reinforce Bitcoin's 'digital gold' status, prioritizing scarcity and long-term holding over transactional utility. Their large holdings give them influence in the ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Hold substantial amounts of Bitcoin, often acquired after the earliest phase but still benefiting from its store-of-value narrative. They align with early adopters in resisting protocol changes that might dilute scarcity or introduce inflationary mechanisms, even if it means higher transaction fees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, large_hodlers, beneficiary,
    powerful, generational, mobile, global).

% Process transactions and secure the network, earning block rewards and transaction fees. They benefit from high transaction fees, which are a natural outcome of prioritizing scarcity and limited block space. They have significant power in approving or rejecting protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_pools, agenda_setter,
    organized, biographical, constrained, global).

% Enter the market at higher prices, hoping to benefit from future appreciation. They bear the risk of price volatility and are often priced out of using Bitcoin for small transactions due to high fees, making it primarily a speculative asset rather than a usable currency.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_investors, payer,
    moderate, biographical, constrained, global).

% Attempt to use Bitcoin for everyday purchases or remittances. They are directly impacted by high transaction fees and slow confirmation times, which make the asset impractical for its original 'peer-to-peer electronic cash' purpose. Their alternatives are other cryptocurrencies or traditional payment systems.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, constrained, global).

% Maintain and propose changes to the Bitcoin protocol. While technically independent, their influence is often aligned with the 'digital gold' narrative due to the existing power structures and the perceived need for stability to maintain value. Their identity is deeply tied to the project's long-term success as a store of value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Offer Bitcoin investment products (ETFs, custodial services) to clients, benefiting from its status as a recognized asset class. They reinforce the 'digital gold' narrative as it aligns with their existing investment frameworks and attracts capital.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global, decentralized network for secure, immutable record-keeping of ownership, enabling Bitcoin to function as a scarce, censorship-resistant store of value.
% TRANSFER_FUNCTION: Transfers wealth from late entrants and small transaction users (who pay high fees and bear price volatility) to early adopters, large hodlers, and mining pools (who benefit from appreciation and transaction fees).
% ABSENT_VOICES: Users and developers who prioritize Bitcoin's original 'peer-to-peer electronic cash' vision are marginalized by the 'digital gold' narrative and its associated high transaction fees. They would advocate for scaling solutions that enable cheap, fast transactions, but their proposals are often rejected by the dominant 'digital gold' faction.
% DISAPPEARANCE_RATIONALE: If Bitcoin's 'digital gold' status vanished overnight, the global financial system would experience significant disruption as a major speculative asset class disappeared. Investors would reallocate capital, and the narrative around digital scarcity would collapse, leading to a re-evaluation of other cryptocurrencies. The entire crypto-asset market would undergo a profound reordering.
% FOUNDING_PROBLEM: The problem of creating a decentralized, censorship-resistant digital currency that could not be inflated by central authorities, offering an alternative to traditional fiat money.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and large hodlers attest the problem of fiat inflation and centralized control is still live, justifying Bitcoin's store-of-value role. Critics (including proponents of the 'p2p cash' reading) argue that while the original problem was real, the current 'digital gold' manifestation has abandoned the original solution's emphasis on transactional utility for the common person, making the solution itself a new problem for many.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is high due to the wealth transfer from late investors and small users to early adopters and miners through appreciation and fees. Suppression (0.45) is moderate, reflecting the difficulty for users to exit the Bitcoin ecosystem if they want exposure to its 'digital gold' properties, and the active resistance to protocol changes that would enable cheaper transactions. Theater ratio (0.1) is low, as the core function of securing the ledger and maintaining scarcity is genuinely performed, though the narrative of 'decentralized cash for everyone' has become largely performative for this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of early adopters and large hodlers, the constraint is a successful coordination mechanism for wealth preservation. From the perspective of small transaction users and late retail investors, it is an extractive system that has priced them out of its utility. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters, large hodlers, and financial institutions are beneficiaries, gaining from asset appreciation and the reinforcement of the 'digital gold' narrative. Mining pools and Bitcoin Core developers act as agenda-setters, shaping the protocol and benefiting from transaction fees and ecosystem influence. Late retail investors and small transaction users are victims, bearing the costs of high fees and limited utility. The 'digital gold' reading structurally favors those with existing holdings and the infrastructure to manage them.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    store_of_value_vs_medium_of_exchange,
    'Is Bitcoin''s primary function fundamentally a store of value, or is it intended to be a medium of exchange, and can it be both simultaneously without structural compromise?',
    'Empirical observation of transaction volume vs. holding periods, and the adoption rate of layer-2 scaling solutions. If layer-2 solutions enable widespread transactional use without compromising scarcity, the functions may be reconcilable.',
    'If primarily a store of value, the current extractive structure is a feature, not a bug. If primarily a medium of exchange, the current structure is a snare, and re-prioritizing transactional utility would reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(store_of_value_vs_medium_of_exchange, conceptual, 'Ambiguity in Bitcoin''s core economic function.').

omega_variable(
    decentralization_vs_governance_capture,
    'To what extent has the ''digital gold'' narrative and its associated power structures (mining pools, large hodlers, core developers) captured Bitcoin''s governance, making it less decentralized than claimed?',
    'Analysis of voting patterns on protocol changes, concentration of mining power, and the influence of large financial institutions on development priorities. Compare with metrics of decentralization in other crypto networks.',
    'If governance is substantially captured, the constraint''s suppression and extractiveness are higher than currently measured, as the ''decentralized'' narrative serves as cover for concentrated power. This would shift classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_vs_governance_capture, empirical, 'The degree of centralization in Bitcoin''s governance under the ''digital gold'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.07).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.09).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.095).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, cryptocurrency_market_volatility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bitcoin whitepaper, focusing on its 'digital gold' aspect. It is linked to other readings (p2p_cash_reading, protocol_ossification_reading) which represent alternative interpretations of Bitcoin's core purpose and design principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
