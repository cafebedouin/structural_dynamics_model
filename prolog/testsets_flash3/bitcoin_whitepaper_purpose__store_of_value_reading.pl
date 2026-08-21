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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Whitepaper Purpose: Store of Value Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'store of value' reading of the Bitcoin
 *   whitepaper's purpose, where decentralization and full-node verifiability
 *   are prioritized over on-chain transaction capacity. This leads to limited
 *   block sizes, high transaction fees, and the promotion of off-chain
 *   scaling solutions like the Lightning Network. The constraint is claimed
 *   as a Tangled Rope because it provides a genuine coordination function
 *   (secure, decentralized digital scarcity) but also involves asymmetric
 *   extraction (low-value users pay higher fees to subsidize network security
 *   for long-term holders).
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
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '04535d80-836c-40da-b38d-6cdeb7e1f01c').
narrative_ontology:cs_kernel_codification('04535d80-836c-40da-b38d-6cdeb7e1f01c', fixed_text).
narrative_ontology:cs_authority_grounding('04535d80-836c-40da-b38d-6cdeb7e1f01c', practice).
narrative_ontology:cs_interpretation_layer_present('04535d80-836c-40da-b38d-6cdeb7e1f01c').
narrative_ontology:cs_reading_relation('04535d80-836c-40da-b38d-6cdeb7e1f01c', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_axiom('04535d80-836c-40da-b38d-6cdeb7e1f01c', foundational, decentralization_maximalism).
narrative_ontology:cs_axiom_status(decentralization_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('04535d80-836c-40da-b38d-6cdeb7e1f01c', decentralization_maximalism, deontological).
narrative_ontology:cs_axiom('04535d80-836c-40da-b38d-6cdeb7e1f01c', foundational, verifiability_over_scalability).
narrative_ontology:cs_axiom_status(verifiability_over_scalability, holdable).
narrative_ontology:cs_axiom_grounding('04535d80-836c-40da-b38d-6cdeb7e1f01c', verifiability_over_scalability, conventional).
narrative_ontology:cs_reference_frame('04535d80-836c-40da-b38d-6cdeb7e1f01c', satoshi_vision_of_sound_money).
narrative_ontology:cs_drift_state('04535d80-836c-40da-b38d-6cdeb7e1f01c', contemporary_scaling_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('04535d80-836c-40da-b38d-6cdeb7e1f01c', '').
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

% Actively maintain the network's decentralization by running full nodes, which is made feasible by limited block sizes. They enforce the protocol rules and resist changes that would increase on-chain capacity at the expense of verifiability, thereby benefiting from the network's stability and security.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, biographical, constrained, global).

% Bear the cost of high transaction fees and slow confirmation times on the base layer, making small, frequent on-chain transactions impractical. They are effectively priced off the base layer, forcing them to use off-chain solutions or alternative cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users, payer,
    powerless, immediate, constrained, global).

% Struggle to integrate Bitcoin for everyday transactions due to the high fees and variable confirmation times. They are forced to adopt off-chain solutions like the Lightning Network, which adds complexity and counterparty risk, or to use other payment systems entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_fast_cheap_settlement, payer,
    moderate, immediate, constrained, global).

% Benefit from the base layer's capacity constraints, as it creates a strong incentive for users and businesses to adopt off-chain scaling solutions like the Lightning Network, which they develop and promote. Their work is essential for the 'store of value' reading to accommodate any transactional use.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers, beneficiary,
    organized, biographical, mobile, global).

% Argue that Bitcoin's original purpose was electronic cash for everyday use, and that current policies betray this vision. They are largely excluded from the core development and governance discussions that prioritize store-of-value characteristics, despite their historical claims.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the network's participants around a shared understanding of Bitcoin's primary purpose as a decentralized, verifiable store of value, ensuring long-term security and censorship resistance by limiting on-chain capacity.
% TRANSFER_FUNCTION: Transfers the cost of on-chain transaction fees from long-term holders and full node operators to low-value users and merchants, effectively subsidizing network decentralization and security for the former group.
% ABSENT_VOICES: Electronic cash advocates and users requiring low-fee on-chain transactions are largely excluded from the decision-making processes that maintain the limited block size. They would argue for increased on-chain capacity to fulfill Bitcoin's original 'electronic cash' promise.
% DISAPPEARANCE_RATIONALE: If this reading of Bitcoin's purpose vanished, the network would likely pursue aggressive on-chain scaling, leading to a fundamental shift in its technical architecture, economic model, and user base. The value proposition would change dramatically, and the current beneficiaries would likely see their assets devalued or their influence diminished.
% FOUNDING_PROBLEM: The problem of creating a decentralized digital currency that could resist censorship and maintain its integrity without relying on trusted third parties, ensuring its long-term viability as 'sound money'.
% FOUNDING_PROBLEM_CORROBORATION: The problem of censorship resistance and trustless digital money remains live, as attested by ongoing geopolitical events and the continued reliance on centralized financial institutions. This is corroborated by a broad consensus among cryptocurrency researchers and economists, not just the benefiting parties.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because low-value users are priced off the base layer, effectively paying for the network's security and decentralization, which primarily benefits long-term holders. Suppression is also high (0.75) due to the active resistance against increasing block sizes and the technical barriers to entry for alternative on-chain scaling solutions. Theater ratio is low (0.1) as the commitment to decentralization and verifiability is genuine, not merely performative. Accessibility collapse is 0.7 because while off-chain solutions exist, they are not direct substitutes for base-layer transactions, and resistance is 0.4 from those advocating for electronic cash functionality.
 *
 * PERSPECTIVAL GAP:
 *   Long-term holders and full node operators perceive this constraint as a necessary 'Rope' for maintaining Bitcoin's core value proposition. Low-value users and merchants, however, experience it as a 'Snare' or 'Tangled Rope' due to the high costs and limited utility. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full node operators are beneficiaries (low d) as their assets are secured and their influence over the network is maintained. Low-value on-chain users and merchants are victims (high d) as they bear the costs of high fees and limited utility. Lightning Network developers are also beneficiaries, as their solutions become necessary. Electronic cash advocates are excluded, their vision suppressed by the dominant reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    on_chain_capacity_necessity,
    'Is the current limited on-chain capacity (1MB blocks) truly necessary for maintaining decentralization and full-node verifiability, or could capacity be increased without compromising these goals?',
    'Empirical studies on the impact of block size increases in other cryptocurrencies on node count and network centralization, or a controlled, temporary increase in Bitcoin''s block size in a test environment.',
    'If capacity could be increased without compromise, the current constraint would be reclassified as more extractive (Snare), as the coordination justification would weaken. If compromise is inevitable, the Tangled Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(on_chain_capacity_necessity, empirical, 'Whether limited on-chain capacity is a technical necessity or a policy choice.').

omega_variable(
    whitepaper_telos_ambiguity,
    'Is the Bitcoin whitepaper''s primary telos ''electronic cash'' or ''store of value''?',
    'Historical linguistic analysis of Satoshi''s early communications, or a community-wide consensus mechanism (e.g., a binding protocol vote, though this is unlikely).',
    'If ''electronic cash'' were definitively established as the primary telos, this ''store of value'' reading would be reclassified as a Snare, as its coordination function would be seen as a cover for extraction that deviates from the original mandate. If ''store of value'' were definitively established, the Tangled Rope classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whitepaper_telos_ambiguity, conceptual, 'Ambiguity in Bitcoin whitepaper''s foundational purpose.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (technical barriers, economic incentives) or internalized (ideological commitment to ''store of value'' narrative)?',
    'Post-exit suppression trajectory: if users and developers continue to self-limit on-chain use even after technical barriers are reduced, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for on-chain capacity limits.').


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
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_whitepaper_purpose' kernel. It prioritizes store-of-value characteristics, influencing the 'electronic_cash_reading' by limiting on-chain capacity and pushing transactional use to off-chain layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
