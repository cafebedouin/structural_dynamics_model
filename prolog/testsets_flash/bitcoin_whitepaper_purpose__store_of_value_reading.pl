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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Whitepaper Purpose: Store of Value Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'store of value' reading of the Bitcoin
 *   whitepaper's purpose, where decentralization and full-node verifiability
 *   are prioritized over on-chain transaction capacity. This interpretation
 *   leads to a fixed block size (1MB) and high transaction fees, effectively
 *   pricing out low-value users from the base layer and promoting off-chain
 *   scaling solutions like the Lightning Network. The constraint is claimed
 *   as a Tangled Rope because it coordinates network security and
 *   decentralization for long-term holders while extracting value from users
 *   who need affordable on-chain transactions.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (powerful/mobile) — benefits from scarcity and security
 *   - full_node_operators: Primary beneficiary (moderate/identity_locked) — maintains network integrity, benefits from influence
 *   - low_value_on_chain_users: Primary victim (powerless/constrained) — bears high transaction costs
 *   - new_users_requiring_on_chain_access: Primary victim (powerless/constrained) — faces barriers to entry due to fees
 *   - bitcoin_core_developers: Agenda setter (institutional/constrained) — enforces block size limit and protocol rules
 *   - lightning_network_developers: Beneficiary (organized/mobile) — benefits from the need for off-chain scaling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.6).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.7).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose: Store of Value Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '763b9d0b-f478-4b49-9593-1c58a010d7e0').
narrative_ontology:cs_kernel_codification('763b9d0b-f478-4b49-9593-1c58a010d7e0', fixed_text).
narrative_ontology:cs_authority_grounding('763b9d0b-f478-4b49-9593-1c58a010d7e0', distributed).
narrative_ontology:cs_reading_relation('763b9d0b-f478-4b49-9593-1c58a010d7e0', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('763b9d0b-f478-4b49-9593-1c58a010d7e0', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('763b9d0b-f478-4b49-9593-1c58a010d7e0', foundational, decentralization_is_paramount).
narrative_ontology:cs_axiom_status(decentralization_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('763b9d0b-f478-4b49-9593-1c58a010d7e0', decentralization_is_paramount, deontological).
narrative_ontology:cs_axiom('763b9d0b-f478-4b49-9593-1c58a010d7e0', foundational, full_node_verifiability_is_non_negotiable).
narrative_ontology:cs_axiom_status(full_node_verifiability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('763b9d0b-f478-4b49-9593-1c58a010d7e0', full_node_verifiability_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('763b9d0b-f478-4b49-9593-1c58a010d7e0', satoshi_vision_decentralized_cash).
narrative_ontology:cs_drift_state('763b9d0b-f478-4b49-9593-1c58a010d7e0', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('763b9d0b-f478-4b49-9593-1c58a010d7e0', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, new_users_requiring_on_chain_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, miners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or entities who hold Bitcoin primarily as a long-term investment, valuing its scarcity and censorship resistance. They benefit from the 'store of value' narrative, which drives up demand and price, and from the network's security, which is prioritized by the limited block size.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Individuals or groups who run full Bitcoin nodes, verifying all transactions and blocks. They are crucial for decentralization and verifiability, and their role is reinforced by the 'store of value' reading. They gain influence and a sense of contributing to the network's integrity, but bear the costs of hardware and bandwidth.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    moderate, biographical, identity_locked, global).

% Users who wish to make small, frequent transactions directly on the Bitcoin blockchain. They are victims of the high transaction fees, which make such use cases economically unfeasible, forcing them to use off-chain solutions or other cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users, payer,
    powerless, immediate, constrained, global).

% Individuals new to Bitcoin who need to make an initial on-chain transaction to acquire or move funds. They face a significant barrier to entry due to high fees, which can deter adoption for everyday use.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, new_users_requiring_on_chain_access, payer,
    powerless, immediate, constrained, global).

% The primary maintainers of the Bitcoin protocol, who enforce the 1MB block size limit and other rules that prioritize decentralization and verifiability. They act as the de facto agenda setters for the 'store of value' reading, guiding protocol development in line with this philosophy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Developers and companies building on the Lightning Network, an off-chain scaling solution for Bitcoin. They benefit directly from the base layer's limited capacity and high fees, as it creates a strong incentive for users to adopt their technology for faster, cheaper transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_developers, beneficiary,
    organized, biographical, mobile, global).

% Entities that process transactions and secure the Bitcoin network through proof-of-work. They receive transaction fees, which are higher due to the limited block space, making them direct beneficiaries of the 'store of value' reading's prioritization of scarcity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, miners, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Bitcoin network's security and decentralization by limiting on-chain capacity, ensuring that full nodes can be run by many participants globally, thus maintaining censorship resistance and verifiability.
% TRANSFER_FUNCTION: Transfers value from users requiring on-chain transactions (via high fees) to miners (as revenue) and indirectly to long-term holders (by reinforcing scarcity and security, which supports price appreciation).
% ABSENT_VOICES: Advocates for larger block sizes and on-chain scaling, who argue for Bitcoin's original 'electronic cash' purpose, are largely marginalized in the current development discourse. Their proposals are suppressed by the dominant 'store of value' interpretation and its technical enforcement.
% DISAPPEARANCE_RATIONALE: If the prioritization of decentralization and verifiability over on-chain capacity vanished, the 1MB block size limit would likely be removed, leading to lower transaction fees and increased on-chain throughput. This would fundamentally alter Bitcoin's economic model, its user base, and its role in the broader financial system, potentially shifting its primary purpose from a store of value back towards electronic cash.
% FOUNDING_PROBLEM: The founding problem Bitcoin aimed to solve was creating a decentralized, peer-to-peer electronic cash system that could operate without trusted third parties, addressing issues of double-spending and central control inherent in traditional financial systems.
% FOUNDING_PROBLEM_CORROBORATION: The 'store of value' proponents argue that the original problem of creating a truly decentralized and secure system is still live, and that limited on-chain capacity is essential for this. However, proponents of the 'electronic cash' reading, citing the whitepaper's title and early forum posts, argue that the problem of cheap, everyday digital payments is still live and is being neglected by the current approach. Independent economic analyses and historical records of early Bitcoin usage patterns corroborate the initial intent for transactional use, while the sustained growth of the Lightning Network corroborates the ongoing need for scalable digital payments.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).

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
 *   The extractiveness (0.6) is due to high transaction fees, which transfer value from users to miners and long-term holders. Suppression (0.7) is high because the 1MB block size limit is actively enforced by the protocol rules and full node operators, effectively suppressing alternative scaling approaches that would increase on-chain capacity. Theater ratio is low (0.1) as the stated goals of decentralization and verifiability are genuinely pursued, even if they lead to extraction for some users. Accessibility collapse is high (0.8) as the high fees make on-chain transactions inaccessible for many, pushing them to off-chain solutions or other cryptocurrencies. Resistance (0.4) is moderate, as there is an ongoing debate about block size and scaling, but no successful challenge to the current paradigm.
 *
 * PERSPECTIVAL GAP:
 *   Long-term holders and full node operators experience this as a Rope, ensuring the network's integrity and scarcity, which benefits their investment. Low-value users and new users experience it as a Snare, as they are priced out of the base layer and forced to use more complex off-chain solutions or alternative chains. Bitcoin Core developers, as agenda setters, view it as a necessary coordination mechanism to maintain the network's core properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full node operators are beneficiaries (low d) as the constraint protects their investment and influence. Low-value on-chain users and new users are victims (high d) due to high fees and limited access. Bitcoin Core developers are agenda setters, enforcing the rules that create this structure. Lightning Network developers benefit from the demand for off-chain solutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to preserve decentralization and verifiability. While these are still live concerns, the 'store of value' reading has led to a situation where the coordination function (network security) is intertwined with extraction (high fees). The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine coordination of decentralization). The persistence of the 1MB block size, despite technological advancements, suggests a potential for mandatrophy if the original technical constraints on decentralization become less relevant, but the rule persists due to ideological commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is Bitcoin''s primary purpose a store of value, or electronic cash?',
    'Community consensus shift, or a hard fork that explicitly alters block size or transaction fee structure to prioritize one over the other.',
    'If resolved towards electronic cash, the constraint would need to support high transaction throughput and low fees, fundamentally altering its structure and beneficiary/victim sets. If resolved towards store of value, the current structure is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''bitcoin_whitepaper_purpose'' kernel, specifically the ''store_of_value_reading''. The ''electronic_cash_reading'' is a sibling that would prioritize transactional utility.').

omega_variable(
    nakamoto_oracle_opacity_impact,
    'How does Satoshi Nakamoto''s disappearance affect the authoritative interpretation of the Bitcoin whitepaper''s purpose?',
    'The emergence of a new, widely accepted, and singular authoritative voice or institution for Bitcoin''s development and philosophy.',
    'The absence of an ''oracle'' means that interpretations like the ''store_of_value_reading'' gain legitimacy through community adoption and technical implementation (e.g., block size limits), rather than direct authorial intent. If an oracle re-emerged, it could re-center the ''electronic_cash_reading'', shifting the constraint''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nakamoto_oracle_opacity_impact, conceptual, 'The ''nakamoto_oracle_opacity'' kernel highlights the lack of an authoritative interpreter, which allows different readings of the whitepaper''s purpose to coexist and compete for dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_whitepaper_purpose' kernel. Its structural properties (limited on-chain capacity, high fees) are in tension with the 'electronic_cash_reading', which prioritizes transactional utility. The 'nakamoto_oracle_opacity' constraint describes the lack of an authoritative interpreter, which allows these competing readings to persist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
