% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__electronic_cash_reading, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin's Electronic Cash Telos
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'electronic cash' reading of the Bitcoin
 *   whitepaper's purpose, which asserts that the system must support everyday
 *   transactional use with low fees. The constraint is framed as a
 *   'tangled_rope' because while there's a foundational coordination function
 *   (a shared vision for Bitcoin), the current network's operation (high
 *   fees, limited on-chain capacity) extracts from those who adhere to this
 *   telos, benefiting those who prioritize Bitcoin as a 'store of value'. The
 *   metrics reflect the increasing difficulty and cost of using Bitcoin as
 *   electronic cash over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.8).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.85).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin's Electronic Cash Telos").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df').
narrative_ontology:cs_kernel_codification('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', fixed_text).
narrative_ontology:cs_authority_grounding('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', distributed).
narrative_ontology:cs_reading_relation('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', foundational, low_fees_for_transactional_use).
narrative_ontology:cs_axiom_status(low_fees_for_transactional_use, holdable).
narrative_ontology:cs_axiom_grounding('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', low_fees_for_transactional_use, empirically_contingent).
narrative_ontology:cs_axiom('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', secondary, on_chain_scaling_priority).
narrative_ontology:cs_axiom_status(on_chain_scaling_priority, holdable).
narrative_ontology:cs_axiom_grounding('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', on_chain_scaling_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', original_electronic_cash_vision).
narrative_ontology:cs_drift_state('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', contemporary_bitcoin_network, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1ebdbe1f-c1ec-4417-8bbc-cce9ea2c59df', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_proponents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, electronic_cash_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for Bitcoin's original vision as a peer-to-peer electronic cash system with low fees and high transactional capacity. They bear the costs of high transaction fees and limited on-chain scaling, often seeking alternative implementations or off-chain solutions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, electronic_cash_proponents, payer,
    organized, generational, constrained, global).

% Prioritize Bitcoin's role as a decentralized, censorship-resistant store of value, often viewing high transaction fees as a feature that secures the network and discourages 'spam' transactions. They influence development priorities and resist changes that would increase on-chain capacity at the perceived expense of decentralization.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_proponents, agenda_setter,
    powerful, generational, mobile, global).

% Run full nodes to verify transactions and secure the network. They benefit from smaller block sizes, which reduce storage and bandwidth requirements, making it easier and cheaper to operate a node. They often align with the 'store of value' interpretation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators, beneficiary,
    moderate, biographical, constrained, global).

% Seek to integrate Bitcoin into mainstream commerce by facilitating transactions. They are negatively impacted by high and volatile transaction fees, which make Bitcoin less viable for everyday payments and increase operational costs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, payer,
    organized, biographical, constrained, global).

% Individuals who wish to use Bitcoin for small, everyday purchases or remittances. They are effectively priced out of on-chain transactions by high fees, forcing them to rely on less decentralized off-chain solutions or alternative cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, payer,
    powerless, immediate, trapped, global).

% Businesses that want to accept Bitcoin as payment. High transaction fees and slow confirmation times make it impractical for many retail scenarios, leading to low adoption and often abandonment of Bitcoin payment options.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchants, payer,
    moderate, immediate, mobile, global).

% Maintain and develop the core Bitcoin protocol. Their decisions on scaling and protocol changes heavily influence whether the 'electronic cash' telos can be realized. They often prioritize decentralization and security over transactional throughput, aligning with the 'store of value' interpretation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Academics, researchers, and economists who analyze Bitcoin's technical and economic properties, including its scaling challenges and the implications of different interpretations of its purpose. They provide independent analysis but have no direct power over protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational, albeit contested, vision for Bitcoin's primary utility as a peer-to-peer electronic cash system, aiming to coordinate development and adoption towards enabling everyday transactional use with low fees.
% TRANSFER_FUNCTION: Aims to facilitate the transfer of digital value efficiently and cheaply between transactors. In its current state, it effectively transfers high transaction fees to miners and benefits those prioritizing Bitcoin's 'store of value' characteristics.
% ABSENT_VOICES: Early Bitcoin adopters who envisioned micro-transactions, merchants who ceased accepting Bitcoin due to high fees, and developers of alternative on-chain scaling solutions (e.g., larger blocks) that were rejected or forked off. These voices would advocate for protocol changes to enable the 'electronic cash' telos.
% DISAPPEARANCE_RATIONALE: If the binding 'electronic cash' telos were universally accepted and fully implemented, the Bitcoin network would undergo significant structural changes, including expanded on-chain capacity, lower fees, and a shift in economic incentives. This would fundamentally alter its current 'store of value' focus, reorganizing its technical parameters, user base, and economic function.
% FOUNDING_PROBLEM: The problem of centralized financial institutions and the need for a peer-to-peer electronic cash system that enables direct online payments without relying on a trusted third party, thereby avoiding double-spending.
% FOUNDING_PROBLEM_CORROBORATION: The Bitcoin whitepaper itself, early forum posts by Satoshi Nakamoto, and the stated goals of projects like Bitcoin Cash corroborate the original intent. However, the current Bitcoin Core development philosophy and a large segment of the community contest this as the primary or sole problem Bitcoin solves today; legislative-hearing testimony and independent economic analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.8) because the current state of the Bitcoin network, with its high transaction fees and limited throughput, significantly extracts value from users and businesses who wish to use it as electronic cash. Suppression is also high (0.85) due to the active resistance from dominant factions (e.g., 'store of value' proponents, Bitcoin Core developers) against scaling solutions that would enable low-fee transactional use. The theater ratio is low (0.1) as the debate over Bitcoin's purpose and scaling is a genuine, high-stakes conflict, not a performative one. The temporal measurements show a clear trend of increasing extractiveness and suppression as the network has evolved away from its initial 'electronic cash' functionality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'electronic cash' proponents, the constraint (the binding telos) is being violated, leading to significant extraction. From the 'store of value' perspective, the current state is optimal, and the 'electronic cash' telos is either secondary or has been superseded. The engine's classification will highlight this divergence by computing different per-seat types based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Electronic cash proponents, payment processors, low-value transactors, and merchants are the primary targets (payers/victims) of the current system's failure to meet the 'electronic cash' telos. They bear the costs of high fees and limited utility. Conversely, 'store of value' proponents and full node operators are beneficiaries, as the current structure aligns with their priorities (decentralization, security, high value settlement layer). Bitcoin Core developers act as agenda-setters, influencing the protocol's direction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_primary_intent,
    'What was the Bitcoin whitepaper''s primary, binding intent: ''electronic cash'' for everyday transactions, or a ''store of value'' / censorship-resistant digital gold?',
    'Historical linguistic analysis of Satoshi Nakamoto''s early communications, combined with a formal analysis of the whitepaper''s technical design choices and their implications for scaling.',
    'If ''electronic cash'' is definitively established as the primary intent, the current network''s structure would be reclassified as a severe deviation from its founding purpose, strengthening calls for protocol changes. If ''store of value'' is established, the current structure would be seen as aligned, and the ''electronic cash'' reading would be reclassified as a secondary or superseded interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_primary_intent, conceptual, 'Ambiguity regarding the Bitcoin whitepaper''s core purpose.').

omega_variable(
    scaling_decentralization_tradeoff,
    'Is there an irreducible technical trade-off between on-chain scaling (for low fees/high throughput) and decentralization (number of full nodes, censorship resistance)?',
    'Empirical observation of alternative blockchain designs and scaling solutions (e.g., sharding, layer-2 networks) over time, assessing their impact on both throughput and decentralization metrics.',
    'If the trade-off is proven to be less severe or solvable, the suppression of on-chain scaling solutions would be reclassified as purely extractive. If the trade-off is irreducible, the suppression would be seen as a necessary cost of maintaining decentralization, making the ''electronic cash'' telos inherently difficult to achieve on the main chain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaling_decentralization_tradeoff, empirical, 'The nature of the scaling vs. decentralization trade-off in blockchain design.').

omega_variable(
    nakamoto_oracle_opacity_impact,
    'How does Satoshi Nakamoto''s disappearance and the resulting lack of an authoritative interpreter impact the persistence and contestation of the ''electronic cash'' telos?',
    'Sociological analysis of community governance, developer consensus mechanisms, and the role of ''founding texts'' in leaderless movements. Compare Bitcoin''s evolution to other decentralized projects with and without active founders.',
    'If Nakamoto''s opacity is a primary driver of the telos''s contestation, it highlights a structural vulnerability in decentralized governance. If other factors (e.g., economic incentives, technological limitations) are more dominant, the opacity is a secondary influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nakamoto_oracle_opacity_impact, conceptual, 'The role of Satoshi Nakamoto''s absence in Bitcoin''s interpretive disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 15, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_whitepaper_purpose' kernel. This 'electronic_cash_reading' emphasizes transactional utility and low fees, contrasting with the 'store_of_value_reading' and being influenced by the 'nakamoto_oracle_opacity' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
