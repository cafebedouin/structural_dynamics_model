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
 *   This constraint represents the 'digital gold' reading of the Bitcoin
 *   whitepaper, where Bitcoin is primarily valued as a scarce store of value
 *   and an inflation hedge. This reading prioritizes asset appreciation and
 *   network security over transaction utility, leading to high transaction
 *   fees and limited throughput. The constraint is claimed as a 'rope' by its
 *   proponents (a coordination mechanism for a decentralized store of value),
 *   but the authored metrics reflect its extractive nature for late entrants
 *   and small transaction users, making it a 'tangled rope' in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.75).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold (Store of Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'caf7f19f-f6fc-4a5a-8e36-a25190b916a3').
narrative_ontology:cs_kernel_codification('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', fixed_text).
narrative_ontology:cs_authority_grounding('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', practice).
narrative_ontology:cs_interpretation_layer_present('caf7f19f-f6fc-4a5a-8e36-a25190b916a3').
narrative_ontology:cs_reading_relation('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', foundational, scarcity_is_primary_virtue).
narrative_ontology:cs_axiom_status(scarcity_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', scarcity_is_primary_virtue, conventional).
narrative_ontology:cs_axiom('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', secondary, transaction_fees_are_acceptable_cost_for_security).
narrative_ontology:cs_axiom_status(transaction_fees_are_acceptable_cost_for_security, holdable).
narrative_ontology:cs_axiom_grounding('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', transaction_fees_are_acceptable_cost_for_security, instrumental).
narrative_ontology:cs_reference_frame('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', bitcoin_as_inflation_hedge).
narrative_ontology:cs_drift_state('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', contemporary_macroeconomic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('caf7f19f-f6fc-4a5a-8e36-a25190b916a3', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, large_institutional_investors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, developing_world_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from early adoption and subsequent price appreciation, holding significant portions of the scarce supply. Their interest aligns with maintaining Bitcoin's 'digital gold' narrative and high value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Invest in Bitcoin as a hedge against inflation and a store of value, driving demand and price. They benefit from its scarcity and the narrative that discourages its use for everyday transactions, which could dilute its value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, large_institutional_investors, beneficiary,
    institutional, biographical, mobile, global).

% Process transactions and secure the network, earning block rewards and transaction fees. They benefit from high transaction fees, which are a natural outcome of prioritizing scarcity and store-of-value over high transaction throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators, agenda_setter,
    organized, immediate, constrained, global).

% Enter the market at higher prices, facing significant volatility and the risk of being 'priced out' by further appreciation. They bear the cost of high transaction fees if they attempt to use Bitcoin for anything other than long-term holding.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_investors, payer,
    moderate, biographical, constrained, global).

% Find Bitcoin impractical for small, everyday transactions due to high and volatile fees, which are a direct consequence of the 'digital gold' narrative's prioritization of scarcity and security over throughput. They are effectively excluded from using Bitcoin as peer-to-peer cash.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, constrained, global).

% Often seek Bitcoin as a hedge against hyperinflation or for remittances, but are disproportionately affected by high transaction fees and price volatility, making it less accessible and useful for their immediate needs. Their economic vulnerability makes exit options limited.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, developing_world_users, payer,
    powerless, immediate, trapped, global).

% Advocate for Bitcoin's original vision as a medium of exchange, but their proposals for scaling solutions (e.g., larger block sizes) are often rejected by the 'digital gold' faction, leading to their marginalization within the Bitcoin ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global, decentralized network to maintain a scarce, censorship-resistant digital asset, providing a common, verifiable store of value outside traditional financial systems.
% TRANSFER_FUNCTION: Transfers wealth from late entrants and small transaction users (via high prices and fees) to early holders, large investors, and miners, in exchange for maintaining the 'digital gold' narrative and network security.
% ABSENT_VOICES: Advocates for Bitcoin as peer-to-peer electronic cash are largely excluded from the core development and narrative-setting processes, as their vision conflicts with the 'digital gold' prioritization of scarcity over transaction utility. They would argue for scaling solutions that reduce fees and increase throughput.
% DISAPPEARANCE_RATIONALE: If Bitcoin vanished overnight, a significant portion of global wealth held as 'digital gold' would evaporate, leading to a scramble for alternative inflation hedges and a re-evaluation of decentralized monetary systems. The cryptocurrency market would undergo a massive restructuring.
% FOUNDING_PROBLEM: The problem of centralized control over money, inflation, and the need for a censorship-resistant, peer-to-peer electronic cash system.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and the 'digital gold' faction attest that the problem of centralized monetary control is still live and Bitcoin addresses it as a store of value. The 'p2p cash' faction, however, argues that the original problem of peer-to-peer electronic cash remains largely unsolved by this reading, citing high fees and slow transactions. Independent economists and financial analysts offer corroboration for both sides, depending on their focus.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because the design choices (fixed supply, limited block size) that support the 'digital gold' narrative inherently create high demand relative to transaction capacity, leading to high prices and fees that disproportionately affect late entrants and small users. Suppression (0.75) is high because the protocol's resistance to change, driven by the 'ossification' and 'digital gold' factions, effectively suppresses alternative scaling solutions that would reduce fees and increase utility for everyday transactions. Theater ratio is low (0.20) because the network's security and store-of-value functions are genuinely robust, but the narrative of 'decentralized cash' is increasingly performative for many users.
 *
 * PERSPECTIVAL GAP:
 *   The 'digital gold' proponents (early holders, institutional investors) perceive the high fees and limited throughput as necessary costs for a secure store of value, thus experiencing the constraint as a 'rope'. Conversely, small transaction users and developing world users experience the same constraint as a 'snare' due to its prohibitive costs and lack of utility for their needs. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and large institutional investors are clear beneficiaries, profiting from appreciation and the 'digital gold' narrative. Mining pool operators also benefit from high transaction fees. Late retail investors, small transaction users, and developing world users are victims, bearing the costs of high prices and fees, and effectively being priced out of using Bitcoin as a medium of exchange. P2P cash advocates are excluded, as their vision is actively suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_transaction_cost_vs_fee,
    'What is the actual marginal cost of processing a Bitcoin transaction, and how does it compare to the average transaction fee under the ''digital gold'' reading?',
    'Independent economic analysis of mining operations and network infrastructure costs, compared with average transaction fees over time.',
    'A significant disparity would further highlight the extractive nature of the ''digital gold'' reading, suggesting that high fees are primarily rent extraction rather than a reflection of operational costs. This would strengthen the ''snare'' classification for payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_transaction_cost_vs_fee, empirical, 'Assesses whether transaction fees are cost-justified or primarily rent-seeking.').

omega_variable(
    narrative_influence_on_protocol_development,
    'To what extent does the ''digital gold'' narrative actively suppress protocol changes that would enhance transaction utility (e.g., larger block sizes, alternative scaling solutions)?',
    'Analysis of developer forums, community governance proposals, and voting patterns on protocol upgrades, specifically tracking the arguments used to reject scaling solutions.',
    'If the narrative is found to be a primary driver of suppressing utility-enhancing changes, it would increase the ''suppression'' metric and strengthen the ''tangled rope'' or ''snare'' classification, as the coordination story becomes more of a cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_influence_on_protocol_development, conceptual, 'Examines the causal link between the ''digital gold'' narrative and the suppression of alternative protocol developments.').

omega_variable(
    store_of_value_vs_medium_of_exchange_priority,
    'Is Bitcoin''s primary function structurally fixed as a store of value, or is its role as a medium of exchange merely underdeveloped due to current design choices and narrative emphasis?',
    'Long-term observation of adoption patterns in economies with high inflation and capital controls, and the success of layer-2 scaling solutions in enabling micro-transactions.',
    'If it''s primarily underdeveloped, it suggests the ''digital gold'' reading is a contingent outcome of current choices, not an inherent property, potentially opening pathways for re-prioritizing medium-of-exchange functions. If structurally fixed, the current extraction is an unavoidable consequence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(store_of_value_vs_medium_of_exchange_priority, empirical, 'Determines if the ''digital gold'' function is inherent or a consequence of design/narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.64).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.72).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, cryptocurrency_market_volatility).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, global_remittance_systems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bitcoin whitepaper kernel. Its focus on 'digital gold' directly influences the viability and perceived legitimacy of other readings, such as 'p2p_cash_reading' and 'protocol_ossification_reading', by shaping resource allocation and developer priorities within the Bitcoin ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
