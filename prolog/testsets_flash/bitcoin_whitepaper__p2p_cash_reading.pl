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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   This constraint models Bitcoin as a censorship-resistant medium of
 *   exchange, emphasizing low transaction fees and direct electronic
 *   transactions, consistent with the original whitepaper's vision. This
 *   'p2p_cash_reading' prioritizes transactional utility and scalability
 *   (e.g., via block size increases) to ensure broad access. It stands in
 *   contrast to other interpretations that emphasize Bitcoin as a store of
 *   value ('digital_gold_reading') or prioritize protocol immutability over
 *   scalability ('protocol_ossification_reading'). The constraint's
 *   effectiveness is measured by its ability to facilitate low-cost,
 *   permissionless transactions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.4).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.2).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '2dffdb8a-b5f4-414a-878c-830298978048').
narrative_ontology:cs_kernel_codification('2dffdb8a-b5f4-414a-878c-830298978048', fixed_text).
narrative_ontology:cs_authority_grounding('2dffdb8a-b5f4-414a-878c-830298978048', distributed).
narrative_ontology:cs_reading_relation('2dffdb8a-b5f4-414a-878c-830298978048', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dffdb8a-b5f4-414a-878c-830298978048', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('2dffdb8a-b5f4-414a-878c-830298978048', foundational, low_transaction_fees_essential).
narrative_ontology:cs_axiom_status(low_transaction_fees_essential, holdable).
narrative_ontology:cs_axiom_grounding('2dffdb8a-b5f4-414a-878c-830298978048', low_transaction_fees_essential, instrumental).
narrative_ontology:cs_axiom('2dffdb8a-b5f4-414a-878c-830298978048', foundational, scalability_via_block_size_expansion_legitimate).
narrative_ontology:cs_axiom_status(scalability_via_block_size_expansion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2dffdb8a-b5f4-414a-878c-830298978048', scalability_via_block_size_expansion_legitimate, conventional).
narrative_ontology:cs_reference_frame('2dffdb8a-b5f4-414a-878c-830298978048', original_whitepaper_vision).
narrative_ontology:cs_drift_state('2dffdb8a-b5f4-414a-878c-830298978048', contemporary_network_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2dffdb8a-b5f4-414a-878c-830298978048', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, users_seeking_censorship_resistance).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, users_denied_transactional_access_by_fees).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, decentralized_transaction_processing).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, permissionless_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize Bitcoin for transactions that might be censored or blocked by traditional financial systems. They benefit from the permissionless nature but are sensitive to transaction fees and network congestion.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, users_seeking_censorship_resistance, beneficiary,
    moderate, biographical, constrained, global).

% Accept Bitcoin as payment, benefiting from lower processing fees compared to traditional credit cards and access to a global customer base. Their benefit is contingent on Bitcoin's transactional utility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin, beneficiary,
    moderate, biographical, mobile, global).

% Are priced out of using Bitcoin for small, everyday transactions due to high and volatile transaction fees. They bear the cost of the network's limited capacity and prioritization of higher-value transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, users_denied_transactional_access_by_fees, payer,
    powerless, immediate, trapped, global).

% Process transactions and secure the network, earning fees and block rewards. They have significant influence over network capacity and transaction prioritization, acting as de facto enforcers of the fee market.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter,
    organized, generational, mobile, global).

% Maintain and propose changes to the Bitcoin protocol. Their decisions on scalability and protocol design directly impact transaction fees and the network's capacity as a medium of exchange.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Are largely excluded from directly controlling or censoring Bitcoin transactions, which is a core feature of its censorship resistance. They would prefer a more centralized or regulated system.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, traditional_financial_institutions, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, permissionless, and censorship-resistant network for direct electronic transactions, coordinating value transfer without intermediaries.
% TRANSFER_FUNCTION: Transfers digital value (Bitcoin) from sender to receiver, with a portion of the value (transaction fees) transferred to network miners for processing and security.
% ABSENT_VOICES: Users and developers advocating for alternative scaling solutions (e.g., larger block sizes) are often marginalized in protocol governance debates, leading to a network that may not fully serve the 'p2p cash' vision for all users.
% DISAPPEARANCE_RATIONALE: If Bitcoin as a censorship-resistant medium of exchange vanished, a significant portion of the global digital economy would lose a critical tool for permissionless transactions, forcing a return to centralized or less secure alternatives, and impacting users and merchants who rely on its unique properties.
% FOUNDING_PROBLEM: The problem of 'trusted third parties' in electronic payments, where financial institutions mediate all transactions, leading to censorship, fraud, and privacy concerns.
% FOUNDING_PROBLEM_CORROBORATION: The problem of trusted third parties remains live, as evidenced by ongoing financial censorship, privacy breaches, and the desire for permissionless innovation. Independent privacy advocates and civil liberties organizations corroborate the continued relevance of censorship resistance, while traditional financial institutions dispute the necessity of a decentralized alternative.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).
:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) reflects the transaction fees, which, while variable, can become significant enough to exclude small transactions, creating a cost for users. Suppression (0.2) is low, as the system is permissionless, but the economic cost of fees can act as a soft suppression mechanism. Theater ratio (0.1) is low, as the core function of transaction processing is real, though debates around scalability introduce some performative aspects. Accessibility collapse (0.7) is relatively high because once Bitcoin is adopted, alternatives for censorship-resistant digital cash are limited. Resistance (0.3) comes from those advocating for alternative scaling solutions or different monetary policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of users prioritizing transactional utility, the constraint is a valuable tool, but rising fees can make it feel extractive. From the perspective of those advocating for a store-of-value narrative, transaction fees are a necessary cost for security and scarcity, and thus less extractive. The engine's classification will highlight this divergence based on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Users seeking censorship resistance and merchants accepting Bitcoin are beneficiaries (d near 0.0) as they gain access to a unique payment rail. Users denied transactional access by high fees are victims (d near 1.0). Miners, while enabling the network, are compensated by fees and block rewards, making their position complex but generally aligned with the network's operation. Core developers and protocol maintainers are agenda-setters, influencing the direction of the protocol.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p2p_cash_vs_digital_gold_framing,
    'Is Bitcoin primarily a medium of exchange (p2p_cash_reading) or a store of value (digital_gold_reading)?',
    'Long-term trends in transaction volume vs. holding periods, and the average transaction fee relative to typical transaction value. If fees consistently price out small transactions, the digital_gold_reading gains empirical support.',
    'If the digital_gold_reading prevails, the constraint''s extractiveness for everyday users increases, and its classification shifts towards a Snare for those seeking transactional utility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p2p_cash_vs_digital_gold_framing, empirical, 'Contested primary function of Bitcoin.').

omega_variable(
    block_size_scalability_debate,
    'Is block size expansion a legitimate mechanism to maintain low transaction fees and transactional utility (p2p_cash_reading), or does it compromise decentralization and security (protocol_ossification_reading)?',
    'Empirical data on network decentralization (node count, geographic distribution) and security (attack vectors, cost of attack) following block size increases, balanced against transaction fee trends.',
    'If block size expansion is deemed illegitimate, the p2p_cash_reading''s core tenet is undermined, leading to higher fees and a shift towards the digital_gold_reading''s implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_scalability_debate, conceptual, 'Debate over Bitcoin''s scalability path and its impact on core properties.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''p2p_cash_reading'' of the ''bitcoin_whitepaper'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of community consensus shifts, developer roadmap changes, and market behavior. If the ''digital_gold_reading'' gains dominance, the focus on low fees and transactional access would diminish. If ''protocol_ossification_reading'' dominates, any attempts at block size expansion would be rejected.',
    'Adoption of ''digital_gold_reading'' would increase effective extraction for transactional users. Adoption of ''protocol_ossification_reading'' would make the network less adaptable to scaling needs, potentially increasing fees and limiting transactional utility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the Bitcoin whitepaper kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'bitcoin_whitepaper' kernel, each with different structural properties and implications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
