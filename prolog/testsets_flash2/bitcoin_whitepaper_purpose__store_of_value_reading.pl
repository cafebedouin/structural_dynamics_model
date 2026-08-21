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
 *   human_readable: Bitcoin Whitepaper Purpose: Store of Value Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'store of value' reading of the Bitcoin
 *   whitepaper's purpose, where decentralization and full-node verifiability
 *   are prioritized over on-chain transaction capacity. This leads to a
 *   limited block size (1MB) and high transaction fees, effectively pricing
 *   low-value users off the base layer and promoting off-chain scaling
 *   solutions. The constraint is claimed as a Tangled Rope because it
 *   provides a genuine coordination function (decentralized, secure monetary
 *   base) but also involves significant asymmetric extraction from users who
 *   need cheap on-chain transactions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.75).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose: Store of Value Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '694d2520-7585-4ff6-8c35-fe54e803837b').
narrative_ontology:cs_kernel_codification('694d2520-7585-4ff6-8c35-fe54e803837b', fixed_text).
narrative_ontology:cs_authority_grounding('694d2520-7585-4ff6-8c35-fe54e803837b', practice).
narrative_ontology:cs_interpretation_layer_present('694d2520-7585-4ff6-8c35-fe54e803837b').
narrative_ontology:cs_reading_relation('694d2520-7585-4ff6-8c35-fe54e803837b', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_axiom('694d2520-7585-4ff6-8c35-fe54e803837b', foundational, decentralization_is_paramount).
narrative_ontology:cs_axiom_status(decentralization_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('694d2520-7585-4ff6-8c35-fe54e803837b', decentralization_is_paramount, deontological).
narrative_ontology:cs_axiom('694d2520-7585-4ff6-8c35-fe54e803837b', foundational, verifiability_must_be_universal).
narrative_ontology:cs_axiom_status(verifiability_must_be_universal, holdable).
narrative_ontology:cs_axiom_grounding('694d2520-7585-4ff6-8c35-fe54e803837b', verifiability_must_be_universal, deontological).
narrative_ontology:cs_reference_frame('694d2520-7585-4ff6-8c35-fe54e803837b', satoshi_vision_of_decentralized_money).
narrative_ontology:cs_drift_state('694d2520-7585-4ff6-8c35-fe54e803837b', contemporary_scaling_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('694d2520-7585-4ff6-8c35-fe54e803837b', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_fast_cheap_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the prioritization of decentralization and verifiability, which they believe secures Bitcoin's long-term value proposition as a store of value. They are less concerned with high on-chain transaction fees as their use cases are infrequent and high-value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Actively maintain the network's decentralization by running full nodes, which is made feasible by limited block size. They enforce the protocol rules and resist changes that would increase on-chain capacity at the expense of verifiability. They bear the cost of running nodes but gain influence over protocol direction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, biographical, constrained, global).

% Are priced off the base layer due to high transaction fees, forcing them to use off-chain solutions like the Lightning Network or alternative cryptocurrencies. Their ability to use Bitcoin for everyday transactions is severely limited by this reading's priorities.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users, payer,
    powerless, immediate, constrained, global).

% Find it difficult to accept Bitcoin directly for small, frequent transactions due to high fees and variable confirmation times on the base layer. They are forced to integrate complex off-chain solutions or use other payment systems, increasing their operational overhead.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_fast_cheap_settlement, payer,
    moderate, immediate, constrained, global).

% Benefit from the base layer's capacity constraints, as it creates a strong incentive for users and businesses to adopt off-chain scaling solutions like the Lightning Network, which they develop and promote. Their work is directly vindicated by this reading's priorities.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers, beneficiary,
    organized, biographical, mobile, global).

% Argue that Bitcoin's original purpose was electronic cash for everyday use, and that current policies betray this vision. They are largely excluded from core protocol development decisions and their proposals for on-chain scaling are consistently rejected by the dominant 'store of value' faction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the network around a shared understanding of Bitcoin's primary purpose as a decentralized, verifiable store of value, ensuring long-term security and censorship resistance by limiting on-chain capacity.
% TRANSFER_FUNCTION: Transfers the cost of network decentralization (high transaction fees, limited on-chain throughput) from long-term holders and node operators to users requiring low-value, frequent on-chain transactions.
% ABSENT_VOICES: Electronic cash advocates and users requiring low-fee on-chain transactions are largely excluded from the decision-making processes that maintain the limited block size. Their concerns about usability and accessibility are subordinated to the goals of decentralization and verifiability.
% DISAPPEARANCE_RATIONALE: If this reading's dominance vanished, the debate over Bitcoin's purpose would re-ignite, likely leading to proposals for increased on-chain capacity. This would fundamentally alter the network's economic model, fee structure, and potentially its decentralization characteristics, causing a significant rearrangement of the ecosystem.
% FOUNDING_PROBLEM: The problem of creating a decentralized digital currency that is resistant to censorship and inflation, without relying on trusted third parties.
% FOUNDING_PROBLEM_CORROBORATION: Long-term holders and full node operators attest that the problem of censorship resistance and inflation remains live, justifying the current design. Electronic cash advocates contest that the problem of everyday digital cash has been abandoned, citing high fees and limited throughput.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because a significant portion of the network's value accrues to long-term holders and node operators, while low-value users bear the cost of high fees. Suppression (0.75) is also high, as the technical and social mechanisms (limited block size, resistance to scaling proposals) actively suppress alternative visions for Bitcoin's use. Theater ratio is low (0.1) because the commitment to decentralization and verifiability is genuine, not merely performative. The metrics reflect the structural reality of this reading, independent of its claimed type.
 *
 * PERSPECTIVAL GAP:
 *   Long-term holders and node operators perceive this as a necessary and beneficial constraint for Bitcoin's long-term health and security, a 'Rope' or even 'Mountain' of sound monetary policy. Low-value users and electronic cash advocates, however, experience it as a 'Snare' or 'Tangled Rope' that extracts value and limits utility, betraying the original vision of 'electronic cash'.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full node operators are beneficiaries (low d) as their interests align with the constraint's priorities. Low-value on-chain users and merchants are victims (high d) as they bear the direct costs of high fees and limited throughput. Lightning Network developers are also beneficiaries, as their solutions become necessary under this constraint. Electronic cash advocates are excluded, as their vision is actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_purpose_ambiguity,
    'Is the Bitcoin whitepaper''s primary purpose truly ''store of value'' or ''electronic cash''?',
    'Historical analysis of Satoshi Nakamoto''s early writings and forum posts, combined with a re-evaluation of the ''electronic cash'' title, to determine the original intent. However, given Nakamoto''s disappearance, this is likely to remain contested.',
    'If ''electronic cash'' were definitively established as the primary purpose, the current constraint would be reclassified as a Snare, as its coordination function (store of value) would be seen as a cover for extraction from its intended users. If ''store of value'' is confirmed, the Tangled Rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whitepaper_purpose_ambiguity, conceptual, 'Ambiguity in the Bitcoin whitepaper''s foundational purpose.').

omega_variable(
    decentralization_scaling_tradeoff,
    'Is the tradeoff between on-chain scaling and decentralization a fundamental, unavoidable ''Mountain'' constraint, or a ''Tangled Rope'' policy choice that could be re-negotiated with different technical solutions?',
    'Empirical evidence from other blockchain projects that have achieved higher on-chain throughput without compromising decentralization, or new cryptographic research demonstrating novel scaling techniques. This is an ongoing area of research and development.',
    'If the tradeoff is a fundamental Mountain, the current constraint is a necessary evil. If it''s a policy choice, the high extractiveness and suppression could be reduced by adopting alternative scaling strategies, potentially reclassifying it as a Rope or even a Scaffold (if temporary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_scaling_tradeoff, empirical, 'Whether the decentralization-scaling tradeoff is a natural law or a policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_whitepaper_purpose' kernel, focusing on its role as a store of value. It is structurally distinct from the 'electronic_cash_reading' which prioritizes transactional utility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
