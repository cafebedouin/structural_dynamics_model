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
 *   human_readable: Bitcoin Whitepaper Purpose (Store of Value Reading): Prioritizing Decentralization and Full-Node Verifiability
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint is the 'store_of_value_reading' of the
 *   'bitcoin_whitepaper_purpose' kernel, emphasizing decentralization and
 *   full-node verifiability as binding constraints, to which on-chain
 *   capacity is subordinated. This reading leads to limited block sizes,
 *   higher transaction fees on the base layer, and the promotion of off-chain
 *   scaling solutions like the Lightning Network. It stands in contrast to
 *   the 'electronic_cash_reading' which prioritizes everyday transactional
 *   use with low fees.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.78).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.72).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose (Store of Value Reading): Prioritizing Decentralization and Full-Node Verifiability").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'e4d5012e-4b62-4664-8b48-a0245d649fc8').
narrative_ontology:cs_kernel_codification('e4d5012e-4b62-4664-8b48-a0245d649fc8', fixed_text).
narrative_ontology:cs_authority_grounding('e4d5012e-4b62-4664-8b48-a0245d649fc8', practice).
narrative_ontology:cs_interpretation_layer_present('e4d5012e-4b62-4664-8b48-a0245d649fc8').
narrative_ontology:cs_reading_relation('e4d5012e-4b62-4664-8b48-a0245d649fc8', bitcoin_whitepaper_purpose__electronic_cash_reading, forecloses).
narrative_ontology:cs_axiom('e4d5012e-4b62-4664-8b48-a0245d649fc8', foundational, decentralization_maximization).
narrative_ontology:cs_axiom_status(decentralization_maximization, holdable).
narrative_ontology:cs_axiom_grounding('e4d5012e-4b62-4664-8b48-a0245d649fc8', decentralization_maximization, deontological).
narrative_ontology:cs_axiom('e4d5012e-4b62-4664-8b48-a0245d649fc8', foundational, full_node_verifiability_priority).
narrative_ontology:cs_axiom_status(full_node_verifiability_priority, holdable).
narrative_ontology:cs_axiom_grounding('e4d5012e-4b62-4664-8b48-a0245d649fc8', full_node_verifiability_priority, deontological).
narrative_ontology:cs_axiom('e4d5012e-4b62-4664-8b48-a0245d649fc8', secondary, on_chain_capacity_subordination).
narrative_ontology:cs_axiom_status(on_chain_capacity_subordination, holdable).
narrative_ontology:cs_axiom_grounding('e4d5012e-4b62-4664-8b48-a0245d649fc8', on_chain_capacity_subordination, conventional).
narrative_ontology:cs_reference_frame('e4d5012e-4b62-4664-8b48-a0245d649fc8', minimal_trust_decentralized_system).
narrative_ontology:cs_drift_state('e4d5012e-4b62-4664-8b48-a0245d649fc8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e4d5012e-4b62-4664-8b48-a0245d649fc8', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, small_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the network's high security, censorship resistance, and predictable monetary policy, which underpins Bitcoin's value proposition as a store of value. They are largely unconcerned with high on-chain transaction fees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Actively enforce the protocol rules, including the 1MB block limit, by validating all transactions and blocks. They benefit from the low resource requirements for running a node, which maintains the network's decentralization and their ability to verify independently.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary).

% Influence the protocol's direction and prioritize design choices that maximize decentralization and security, often at the expense of on-chain transactional capacity. They maintain the technical and social consensus around the current block size limit.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Are effectively priced off the base layer for everyday transactions due to high and volatile fees, forcing them to use off-chain solutions or alternative cryptocurrencies, which may have different trust assumptions or user experiences.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transactors, payer,
    powerless, immediate, constrained, global).

% Face challenges in accepting Bitcoin for small-value purchases due to high on-chain fees and confirmation times, making it impractical for point-of-sale use. They are pushed towards off-chain solutions or other payment methods.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, small_merchants, payer,
    moderate, immediate, constrained, local).

% Benefit from the base layer's limited capacity, as it creates a strong incentive for users and businesses to adopt their off-chain scaling solution. Their work is seen as complementary and necessary for Bitcoin's broader utility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for Bitcoin's original vision as a peer-to-peer electronic cash system capable of supporting everyday transactions with low fees on the base layer. Their proposals for larger block sizes or alternative scaling approaches are largely rejected by the dominant store-of-value consensus.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a highly secure, censorship-resistant, and verifiable digital store of value by limiting on-chain capacity to ensure full nodes can be run by many, thus maintaining decentralization and network integrity.
% TRANSFER_FUNCTION: Transfers the cost of on-chain transaction fees from the network's security budget (borne by node operators) to users who require base-layer transactions, effectively transferring value from transactional utility to network security and decentralization.
% ABSENT_VOICES: Electronic cash advocates, users in developing economies needing low-fee transactions, and proponents of alternative scaling methods (e.g., larger blocks) are largely sidelined or priced out of the base layer, with their concerns often framed as secondary to the core tenets of decentralization and security.
% DISAPPEARANCE_RATIONALE: If the commitment to limited on-chain capacity and prioritization of decentralization vanished, the network would likely fork, leading to a different, potentially less decentralized but more transaction-friendly system, fundamentally altering Bitcoin's economic and technical properties and its role in the global financial system.
% FOUNDING_PROBLEM: How to create a decentralized digital currency that is resistant to censorship and inflation, without relying on trusted third parties, and that can maintain its integrity over time.
% FOUNDING_PROBLEM_CORROBORATION: The core developer community, full node operators, and long-term holders attest that the problem of maintaining a truly decentralized and censorship-resistant digital money is ongoing and requires the current design choices. Critics (electronic cash advocates) attest that the founding problem has been over-solved for decentralization at the expense of transactional utility, and that the current arrangement is a form of rent-seeking.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the design choices (limited block size) effectively price out low-value transactors from the base layer, transferring value to those who prioritize and benefit from network security and decentralization. Suppression is also high, as the social and technical consensus actively resists changes that would increase on-chain capacity. The theater ratio is low because the commitment to decentralization and verifiability is genuine, and the costs incurred are direct consequences of these priorities, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of long-term holders and full node operators, this constraint is a necessary 'rope' for maintaining Bitcoin's core value proposition. From the perspective of low-value transactors and small merchants, it operates as a 'snare' or 'tangled rope', extracting value and limiting utility due to design choices that prioritize other stakeholders' interests. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders, full node operators, and Lightning Network developers are beneficiaries, as the constraint's design choices directly support their interests (secure store of value, network integrity, off-chain scaling). Low-value transactors and small merchants are victims, bearing the costs of high fees and limited utility for on-chain transactions. Core developers act as agenda-setters, shaping the protocol's direction, while electronic cash advocates are excluded, their alternative vision largely unheeded.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electronic_cash_vs_store_of_value_ambiguity,
    'Is Bitcoin''s primary purpose, as articulated in the whitepaper, to be a peer-to-peer electronic cash system for everyday transactions, or a decentralized store of value?',
    'Analysis of Satoshi Nakamoto''s early writings and forum posts, combined with a survey of the original user base''s expectations, and the long-term economic behavior of the network (e.g., average transaction value, fee sensitivity).',
    'If resolved towards ''electronic cash'', the current constraint''s high extractiveness and suppression would be reclassified as a significant deviation from the founding mandate, potentially shifting its type towards Snare. If resolved towards ''store of value'', the current classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electronic_cash_vs_store_of_value_ambiguity, conceptual, 'Ambiguity regarding Bitcoin''s foundational purpose as electronic cash versus store of value.').

omega_variable(
    necessity_of_1mb_block_limit,
    'Is the 1MB block size limit genuinely necessary to maintain sufficient decentralization and full-node verifiability, or could a larger block size be accommodated without compromising these goals?',
    'Empirical studies on the cost and technical requirements of running full nodes with larger block sizes, combined with simulations of network centralization under various block size scenarios. Comparison with other decentralized networks'' scaling approaches.',
    'If a larger block size is found to be feasible without significant decentralization loss, the current constraint''s suppression and extractiveness would be seen as unnecessary, strengthening the Snare aspects. If the 1MB limit is confirmed as critical, the Tangled Rope classification would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_1mb_block_limit, empirical, 'Whether the 1MB block limit is a technical necessity or a policy choice.').

omega_variable(
    satoshi_nakamoto_authoritative_interpretation,
    'Does Satoshi Nakamoto''s disappearance mean there is no longer an authoritative interpreter of the whitepaper''s purpose, leaving it open to contested readings?',
    'Analysis of the social and technical governance mechanisms that emerged post-Satoshi, and whether any single entity or group has successfully claimed and maintained authoritative interpretive power over the whitepaper''s core tenets.',
    'If no authoritative interpretation exists, the ''store of value'' reading''s persistence relies more heavily on social consensus and active enforcement by its beneficiaries, rather than a foundational, unchallengeable truth, potentially increasing its perceived extractiveness from alternative readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(satoshi_nakamoto_authoritative_interpretation, conceptual, 'The impact of Satoshi''s disappearance on authoritative interpretation of the whitepaper.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 9, 0.72).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_whitepaper_purpose' kernel, focusing on store of value. It is structurally distinct from the 'electronic_cash_reading' which emphasizes transactional utility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
