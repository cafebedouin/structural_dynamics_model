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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint story models the 'digital gold' reading of Bitcoin, where
 *   its primary function is as a scarce, appreciating store of value and
 *   inflation hedge. This reading prioritizes asset appreciation and network
 *   security, accepting high transaction fees as a necessary cost. It is one
 *   reading of the broader 'bitcoin_whitepaper' kernel, which also includes
 *   'p2p_cash' and 'protocol_ossification' readings. The 'digital gold'
 *   framing has become dominant, influencing development priorities and
 *   market perception.
 *
 * KEY AGENTS:
 *   - early_bitcoin_holders: Primary beneficiary (powerful/arbitrage) — benefits from asset appreciation.
 *   - bitcoin_mining_pools: Agenda setter (organized/constrained) — benefits from transaction fees and block rewards.
 *   - bitcoin_core_developers: Agenda setter (institutional/identity_locked) — influences protocol direction, aligning with stability.
 *   - late_retail_investors: Primary payer (moderate/constrained) — bears volatility risk and high entry costs.
 *   - small_transaction_users: Primary payer (powerless/constrained) — priced out by high fees.
 *   - developing_world_users: Primary payer (powerless/constrained) — original use case undermined by high fees.
 *   - alternative_cryptocurrencies: Excluded (organized/mobile) — suppressed by Bitcoin's dominant narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.75).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold (Store of Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '5b7a0ffb-1fc6-447b-a6af-1c55dc88d311').
narrative_ontology:cs_kernel_codification('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', fixed_text).
narrative_ontology:cs_authority_grounding('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', lineage).
narrative_ontology:cs_interpretation_layer_present('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311').
narrative_ontology:cs_reading_relation('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', foundational, scarcity_is_primary_virtue).
narrative_ontology:cs_axiom_status(scarcity_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', scarcity_is_primary_virtue, conventional).
narrative_ontology:cs_axiom('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', secondary, transaction_fees_are_acceptable_cost).
narrative_ontology:cs_axiom_status(transaction_fees_are_acceptable_cost, holdable).
narrative_ontology:cs_axiom_grounding('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', transaction_fees_are_acceptable_cost, instrumental).
narrative_ontology:cs_reference_frame('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', bitcoin_as_inflation_hedge).
narrative_ontology:cs_drift_state('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5b7a0ffb-1fc6-447b-a6af-1c55dc88d311', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_mining_pools).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_core_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, developing_world_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold significant amounts of Bitcoin acquired at low prices, benefiting immensely from its appreciation as a store of value. They advocate for policies that reinforce scarcity and price stability, often at the expense of transaction utility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Control significant hash power, validating transactions and securing the network. They benefit from high transaction fees and block rewards, aligning with a 'digital gold' narrative that prioritizes security and scarcity over cheap, frequent transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_mining_pools, agenda_setter,
    organized, biographical, constrained, global).

% Maintain the Bitcoin protocol. While technically open-source, their influence on protocol changes is substantial. Many align with the digital gold narrative, prioritizing stability and security, which often means resisting changes that would increase transaction throughput at the cost of decentralization or perceived scarcity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Enter the Bitcoin market at high prices, hoping for continued appreciation. They bear the risk of volatility and may find their investment diluted by high transaction fees if they attempt to use Bitcoin for anything other than long-term holding.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_investors, payer,
    moderate, immediate, constrained, national).

% Attempt to use Bitcoin for everyday transactions, but are increasingly priced out by high and volatile transaction fees, making it impractical for micro-payments or remittances. Their options are to absorb the cost, use alternative cryptocurrencies, or abandon crypto entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, constrained, local).

% Initially saw Bitcoin as a way to circumvent unstable local currencies or remittance fees. The 'digital gold' narrative, with its emphasis on high fees and slow confirmation times, makes Bitcoin less accessible and useful for their original purpose, forcing them to seek alternatives or bear significant costs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, developing_world_users, payer,
    powerless, biographical, constrained, global).

% Offer faster, cheaper transactions but struggle to gain the same network effect and perceived security as Bitcoin. The 'digital gold' narrative implicitly suppresses their utility as a medium of exchange by framing Bitcoin as the only 'sound' digital money.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, alternative_cryptocurrencies, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global, decentralized network to maintain a scarce, censorship-resistant digital ledger, providing a secure store of value independent of traditional financial systems.
% TRANSFER_FUNCTION: Transfers wealth from late entrants and small transaction users (via high prices and fees) to early holders and miners (via asset appreciation and transaction revenue), in exchange for network security and scarcity.
% ABSENT_VOICES: Advocates for Bitcoin as a peer-to-peer electronic cash system, particularly those in developing economies or those seeking financial inclusion, are marginalized by the 'digital gold' narrative. They would argue for protocol changes to enable cheaper, faster transactions.
% DISAPPEARANCE_RATIONALE: If Bitcoin vanished, the global cryptocurrency market would undergo a massive re-evaluation, with significant capital flowing into alternative assets. The concept of decentralized digital scarcity would be severely impacted, and many investment portfolios would collapse. New digital store-of-value assets would emerge, but the landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of centralized control over money, inflation, and the need for a censorship-resistant, scarce digital asset that could serve as a store of value outside traditional financial systems.
% FOUNDING_PROBLEM_CORROBORATION: The problem of inflation and centralized monetary control is widely attested by economists, financial analysts, and a significant portion of the global population, particularly in countries with high inflation or authoritarian regimes. This corroboration comes from outside the direct beneficiaries of Bitcoin's appreciation.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).

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
 *   The 'digital gold' reading operates as a Tangled Rope. It provides a genuine coordination function (a decentralized, scarce store of value) but exhibits significant asymmetric extraction. Extractiveness (0.65) is driven by asset appreciation benefiting early holders and high transaction fees benefiting miners, while suppressing utility for everyday transactions. Suppression (0.75) is high due to the technical and social barriers to changing the protocol to increase throughput, and the narrative's dominance over alternative use cases. Theater ratio is low (0.1) as the core function of securing the ledger is real, though its benefits are unevenly distributed. The rising extractiveness and suppression over time reflect the increasing dominance of this narrative and its consequences.
 *
 * PERSPECTIVAL GAP:
 *   Early holders and miners perceive the constraint as a successful Rope, delivering on its promise of decentralized scarcity and security. Late investors and small transaction users experience it as a Snare, where the costs (high prices, fees) outweigh the benefits, and their original use cases are suppressed. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and mining pools are clear beneficiaries (low d) due to asset appreciation and transaction fees. Bitcoin Core developers, while not directly profiting from appreciation, are identity-locked into maintaining the protocol's 'digital gold' characteristics, making them agenda-setters with a low-to-moderate d. Late investors and small transaction users are clear targets (high d) due to high entry costs and prohibitive transaction fees. Developing world users, initially beneficiaries of a p2p cash system, become targets under the digital gold reading. Alternative cryptocurrencies are excluded, their potential as a medium of exchange suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'digital gold' reading resolves the potential mandatrophy of Bitcoin's original 'peer-to-peer electronic cash' mandate. While the original problem of centralized money remains live, the solution has shifted from a transactional currency to a store of value. This re-framing allows the constraint to persist and extract value, even as its original transactional utility atrophies for many users. The 'contested' status of the founding problem reflects this ongoing re-interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_gold_vs_p2p_cash_mandate,
    'Is Bitcoin''s primary mandate to be a store of value (''digital gold'') or a medium of exchange (''peer-to-peer electronic cash'')?',
    'Empirical observation of transaction patterns (average transaction size, frequency, fees) and user demographics over time. If transaction utility for small payments becomes viable, the p2p_cash reading gains ground.',
    'If ''digital gold'' is the true mandate, the current high fees and slow transactions are acceptable. If ''p2p_cash'' is the true mandate, the current state represents a significant failure of the constraint''s original purpose, reclassifying it closer to a Snare for many users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_gold_vs_p2p_cash_mandate, conceptual, 'Ambiguity in Bitcoin''s core purpose.').

omega_variable(
    protocol_governance_centralization,
    'To what extent is Bitcoin''s protocol development and direction truly decentralized, or is it effectively controlled by a small group of core developers and large mining pools?',
    'Analysis of BIP (Bitcoin Improvement Proposal) acceptance rates, developer influence on major forks, and the concentration of mining hash power. If a small group consistently dictates changes, it indicates centralization.',
    'If governance is centralized, the ''decentralized'' aspect of Bitcoin''s value proposition is theatrical, increasing the effective extractiveness and suppression, potentially reclassifying it as a Snare for those who believe in true decentralization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_governance_centralization, empirical, 'Decentralization vs. centralization of Bitcoin governance.').

omega_variable(
    scarcity_vs_utility_tradeoff,
    'Is the trade-off between Bitcoin''s scarcity (fixed supply, limited block size) and its utility (transaction speed, cost) an inherent, unchangeable technical constraint, or a policy choice driven by the ''digital gold'' narrative?',
    'Technical analysis of alternative scaling solutions (e.g., Lightning Network adoption, sidechains) and their impact on decentralization. If scaling can be achieved without compromising core principles, the current state is a policy choice.',
    'If it''s an unchangeable technical constraint, the ''digital gold'' reading is closer to a Mountain. If it''s a policy choice, the constraint is more clearly a Tangled Rope or Snare, with identifiable beneficiaries of that choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_vs_utility_tradeoff, empirical, 'Inherent technical limit vs. policy choice for Bitcoin''s design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2010, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(bitc_tr_t2014, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2014, 0.08).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.12).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2010, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(bitc_be_t2014, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2014, 0.3).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.7).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2010, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(bitc_su_t2014, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'digital gold' reading of the Bitcoin whitepaper, emphasizing its store-of-value function. It is linked to the 'p2p cash' reading (focus on transaction utility) and the 'protocol ossification' reading (focus on immutability) as part of a constraint family where different interpretations of the same kernel lead to structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
