% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin's Store of Value Priority (Decentralization over On-Chain Capacity)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'store of value' reading of the Bitcoin
 *   whitepaper's purpose, emphasizing decentralization and full-node
 *   verifiability as binding constraints, with on-chain capacity subordinated
 *   to these goals. This reading stands in contrast to the
 *   'electronic_cash_reading', which prioritizes low-fee transactional use.
 *   The persistence of the 1MB block limit and the development of off-chain
 *   scaling solutions like the Lightning Network are direct consequences of
 *   this interpretation. While providing a highly secure and decentralized
 *   base layer, it effectively prices out users requiring low-cost on-chain
 *   transactions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.7).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.65).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin's Store of Value Priority (Decentralization over On-Chain Capacity)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '3f933e10-8ebd-402c-837e-5a171030d9f6').
narrative_ontology:cs_kernel_codification('3f933e10-8ebd-402c-837e-5a171030d9f6', fixed_text).
narrative_ontology:cs_authority_grounding('3f933e10-8ebd-402c-837e-5a171030d9f6', practice).
narrative_ontology:cs_interpretation_layer_present('3f933e10-8ebd-402c-837e-5a171030d9f6').
narrative_ontology:cs_reading_relation('3f933e10-8ebd-402c-837e-5a171030d9f6', bitcoin_whitepaper_purpose__electronic_cash_reading, forecloses).
narrative_ontology:cs_axiom('3f933e10-8ebd-402c-837e-5a171030d9f6', foundational, decentralization_maximization).
narrative_ontology:cs_axiom_status(decentralization_maximization, holdable).
narrative_ontology:cs_axiom_grounding('3f933e10-8ebd-402c-837e-5a171030d9f6', decentralization_maximization, deontological).
narrative_ontology:cs_axiom('3f933e10-8ebd-402c-837e-5a171030d9f6', foundational, full_node_verifiability_primacy).
narrative_ontology:cs_axiom_status(full_node_verifiability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3f933e10-8ebd-402c-837e-5a171030d9f6', full_node_verifiability_primacy, deontological).
narrative_ontology:cs_reference_frame('3f933e10-8ebd-402c-837e-5a171030d9f6', satoshi_vision_decentralized_immutable).
narrative_ontology:cs_drift_state('3f933e10-8ebd-402c-837e-5a171030d9f6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3f933e10-8ebd-402c-837e-5a171030d9f6', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_cheap_on_chain_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the network's security and enforce the protocol rules, including the 1MB block limit. They bear the costs of running nodes but benefit from the network's decentralization and their influence over its direction. Their collective action is essential for this reading's persistence.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, generational, constrained, global).

% Benefit from the network's perceived scarcity, security, and censorship resistance, which underpins its store-of-value narrative. They are generally aligned with prioritizing decentralization over on-chain capacity, as it supports their investment thesis.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Are priced off the base layer for everyday transactions due to high and volatile fees, making direct on-chain use impractical for small transfers. Their alternatives are off-chain solutions (with different trust assumptions) or other cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users, payer,
    powerless, immediate, constrained, global).

% Find it difficult to accept small Bitcoin payments directly on-chain due to high fees and confirmation times. They are forced to use off-chain solutions or alternative payment rails, which may introduce new complexities or trust requirements.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_cheap_on_chain_settlement, payer,
    moderate, biographical, constrained, global).

% Their off-chain scaling solution is promoted and becomes more necessary under this constraint. They benefit from the base layer's limited capacity driving users to their technology, even if it introduces new trade-offs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers, beneficiary,
    organized, biographical, mobile, global).

% Argue that Bitcoin's original purpose was electronic cash for everyday use, which requires high on-chain capacity and low fees. Their proposals for larger blocks or alternative scaling approaches are largely rejected by the dominant 'store of value' interpretation, leaving them with limited influence over the protocol's direction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a highly decentralized, censorship-resistant, and verifiable global ledger for secure value transfer, prioritizing the long-term security and immutability of the base layer over high transaction throughput.
% TRANSFER_FUNCTION: Transfers transaction fees from users (especially those needing on-chain settlement) to miners, incentivizing network security, and indirectly benefits long-term holders through enhanced network robustness and scarcity.
% ABSENT_VOICES: Advocates for on-chain scaling and low-fee transactional use (electronic_cash_advocates) are structurally excluded from influencing the core protocol's capacity decisions, as their vision is deemed incompatible with the primary goal of decentralization and verifiability.
% DISAPPEARANCE_RATIONALE: If the commitment to prioritizing decentralization and full-node verifiability over on-chain capacity vanished, the network would likely undergo significant changes, potentially leading to a different, more centralized, or less secure system. The 'store of value' narrative would collapse, and the entire ecosystem would reorganize around new design principles.
% FOUNDING_PROBLEM: How to create a decentralized digital cash system that prevents double-spending without relying on a trusted third party, ensuring censorship resistance and immutability.
% FOUNDING_PROBLEM_CORROBORATION: Cryptographers, computer scientists, and economists outside the immediate Bitcoin community corroborate the foundational problem of achieving decentralized digital trust. However, the specific prioritization of decentralization over on-chain capacity as the *solution* is a point of ongoing debate and interpretation, not universally corroborated as the sole path.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because the limited on-chain capacity, a direct consequence of prioritizing decentralization, leads to high transaction fees that act as a tax on base-layer usage. Suppression (0.65) is also significant, as the technical and economic realities of the network effectively suppress alternative visions for on-chain scaling and make low-value on-chain transactions impractical. The theater ratio is low (0.1) because the commitment to decentralization and verifiability is genuine and actively maintained by the network's participants, not merely performative. Resistance is moderate (0.55) due to ongoing debates and forks (e.g., Bitcoin Cash) that challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of full node operators and long-term holders, this constraint is a necessary 'rope' for maintaining Bitcoin's core value proposition as a decentralized store of value. From the perspective of low-value users and electronic cash advocates, it operates as a 'snare' or 'tangled rope,' extracting value and suppressing alternative uses that they believe align with the whitepaper's original intent. The engine's computation of per-seat classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Full node operators and long_term_holders are primary beneficiaries, as their interests in network security, decentralization, and value appreciation are prioritized. Low_value_on_chain_users and merchants_requiring_cheap_on_chain_settlement are victims, bearing the costs of high fees and limited access to the base layer. Lightning_network_developers benefit as their solutions become necessary. Electronic_cash_advocates are excluded, as their vision is actively deprioritized by this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_primary_purpose,
    'Is the Bitcoin whitepaper''s primary purpose ''electronic cash'' (prioritizing transactional utility) or ''store of value'' (prioritizing decentralization and security)?',
    'Historical analysis of Satoshi Nakamoto''s early writings and forum posts, combined with a conceptual analysis of the ''cash'' vs. ''gold'' metaphors in early cryptocurrency discourse. However, a definitive resolution is unlikely due to Satoshi''s disappearance.',
    'If resolved as ''electronic cash'', the current constraint''s high extractiveness and suppression would be reclassified as a ''snare'' or ''tangled rope'' from a more critical perspective. If resolved as ''store of value'', the current classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whitepaper_primary_purpose, conceptual, 'Ambiguity regarding Bitcoin''s foundational telos.').

omega_variable(
    decentralization_capacity_tradeoff,
    'Is the current level of decentralization and full-node verifiability truly dependent on the 1MB block limit, or could it be maintained with higher on-chain capacity through alternative technical solutions?',
    'Empirical studies and simulations of network performance under increased block sizes, or the long-term success/failure of alternative protocols that implement larger blocks while maintaining decentralization. This is a complex engineering and economic question.',
    'If higher capacity is shown to be compatible with decentralization, the current constraint''s extractiveness and suppression would be seen as unnecessary, strengthening arguments for reclassification towards a ''snare'' or ''tangled_rope'' due to artificial scarcity. If the dependency is confirmed, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_capacity_tradeoff, empirical, 'Whether the technical trade-off between decentralization and on-chain capacity is absolute or contingent.').

omega_variable(
    low_value_user_exclusion_justification,
    'Is the effective exclusion of low-value on-chain users an unavoidable and justified consequence of prioritizing decentralization, or an extractive side effect that could be mitigated without compromising core principles?',
    'Policy debates and community consensus on the acceptable social and economic costs of the current design, potentially informed by philosophical arguments about access to public goods. This involves value judgments.',
    'If deemed an unjustified side effect, it would strengthen the ''snare'' or ''tangled_rope'' classification by highlighting the ethical costs. If deemed unavoidable, it would be accepted as a necessary cost of the ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_value_user_exclusion_justification, preference, 'Ethical justification for pricing out low-value users from the base layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2008, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2011, 0.1).
narrative_ontology:measurement(bitc_tr_t2014, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2008, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2011, 0.4).
narrative_ontology:measurement(bitc_be_t2014, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2023, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2008, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(bitc_su_t2011, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2011, 0.5).
narrative_ontology:measurement(bitc_su_t2014, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2023, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
