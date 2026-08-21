% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Reading: Immutable Monetary Policy
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'maximalist' reading of the Bitcoin
 *   whitepaper, which interprets its principles as establishing an immutable
 *   monetary policy and a fixed protocol layer. Any deviation from this
 *   original vision is seen as a violation of the founding covenant. This
 *   reading prioritizes scarcity and censorship resistance above all else,
 *   often at the expense of scalability and innovation. The high
 *   extractiveness and suppression reflect the costs imposed on those who
 *   advocate for change or are impacted by the protocol's limitations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.92).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, snare).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Reading: Immutable Monetary Policy").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, 'ee785d8c-58e2-497c-8744-b42bdae4ddbe').
narrative_ontology:cs_kernel_codification('ee785d8c-58e2-497c-8744-b42bdae4ddbe', fixed_text).
narrative_ontology:cs_authority_grounding('ee785d8c-58e2-497c-8744-b42bdae4ddbe', lineage).
narrative_ontology:cs_interpretation_layer_present('ee785d8c-58e2-497c-8744-b42bdae4ddbe').
narrative_ontology:cs_reading_relation('ee785d8c-58e2-497c-8744-b42bdae4ddbe', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('ee785d8c-58e2-497c-8744-b42bdae4ddbe', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('ee785d8c-58e2-497c-8744-b42bdae4ddbe', foundational, monetary_policy_immutability).
narrative_ontology:cs_axiom_status(monetary_policy_immutability, holdable).
narrative_ontology:cs_axiom_grounding('ee785d8c-58e2-497c-8744-b42bdae4ddbe', monetary_policy_immutability, deontological).
narrative_ontology:cs_axiom('ee785d8c-58e2-497c-8744-b42bdae4ddbe', foundational, base_layer_sacrosanct).
narrative_ontology:cs_axiom_status(base_layer_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('ee785d8c-58e2-497c-8744-b42bdae4ddbe', base_layer_sacrosanct, conventional).
narrative_ontology:cs_reference_frame('ee785d8c-58e2-497c-8744-b42bdae4ddbe', satoshi_original_vision).
narrative_ontology:cs_drift_state('ee785d8c-58e2-497c-8744-b42bdae4ddbe', contemporary_scalability_debates, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ee785d8c-58e2-497c-8744-b42bdae4ddbe', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perceived scarcity and immutability of Bitcoin's monetary policy, which they believe guarantees long-term value appreciation. They actively resist any changes to the core protocol that could dilute this value proposition.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Similar to long-term holders, they benefit from the established narrative of Bitcoin's fixed supply and resistance to change. Their influence is often amplified by their early participation and accumulated wealth.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of extreme conservatism, as their proposals for technical improvements or scalability solutions are often rejected if perceived to violate the 'founding covenant' of immutability. Their ability to innovate is severely constrained.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_developers, payer,
    moderate, biographical, constrained, global).

% Face significant resistance and often outright rejection for solutions that might alter the base layer or introduce new monetary policy mechanisms, even if designed to improve network utility. They are forced to build on a highly restrictive foundation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers, payer,
    moderate, biographical, constrained, global).

% Experience high transaction fees and slow confirmation times due to the protocol's limited scalability, which is a direct consequence of the maximalist reading's resistance to base-layer changes. They are forced to accept these limitations or seek alternatives.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, new_users, payer,
    powerless, immediate, mobile, global).

% Advocate for a layered approach where the base layer remains stable but upper layers can innovate. Their arguments are often dismissed by maximalists as violating the core principles, effectively excluding them from influencing the core protocol's direction.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared belief in Bitcoin's absolute scarcity and resistance to inflation, fostering a strong community identity and a common understanding of its value proposition.
% TRANSFER_FUNCTION: Transfers perceived long-term value and ideological purity to long-term holders and early adopters, at the cost of innovation, scalability, and utility for developers and new users.
% ABSENT_VOICES: Advocates for more flexible, utility-focused, or layered approaches to Bitcoin's development are often marginalized or excluded from the core consensus-building process, as their views are deemed heretical to the maximalist interpretation.
% DISAPPEARANCE_RATIONALE: If the maximalist reading of immutable monetary policy vanished, the Bitcoin community would immediately fracture. Developers would propose and implement changes, leading to multiple forks and a loss of the unified 'store of value' narrative. The market would reprice Bitcoin based on new expectations of supply and utility, fundamentally altering its economic function.
% FOUNDING_PROBLEM: The problem of centralized control over monetary policy, leading to inflation and debasement of fiat currencies, and the need for a truly decentralized, censorship-resistant digital cash system.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist community and many long-term holders attest that the problem of centralized monetary control remains live and Bitcoin's immutability is the only solution. Critics, including many developers and scalability advocates, argue that while the problem is live, the maximalist reading has created new problems of its own, such as limited utility and governance paralysis.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the maximalist reading imposes significant costs on developers and users by resisting changes that could improve utility or reduce transaction fees. Suppression is very high as this reading actively suppresses alternative interpretations and development paths through social consensus, ideological pressure, and the threat of community ostracization. Theater ratio is low because the maximalist stance is genuinely held and actively defended, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of long-term holders, this is a 'mountain' of economic law and cryptographic truth, ensuring their wealth. From the perspective of developers and new users, it operates as a 'snare,' trapping them in a system with high costs and limited flexibility, enforced by an ideological consensus that resists change.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and early adopters are clear beneficiaries, as their wealth and ideological position are reinforced by the immutability narrative. Protocol developers, scalability solution providers, and new users are victims, bearing the costs of limited innovation and high transaction fees. Pragmatic synthesis advocates are excluded, as their views are deemed incompatible with the maximalist interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maximalist_vs_utility_reading_ambiguity,
    'Is the Bitcoin whitepaper a prescriptive document for immutable monetary policy, or a foundational technical specification for an evolving system?',
    'Historical analysis of Satoshi Nakamoto''s later communications and early developer discussions, or a community-wide vote on a constitutional amendment to the protocol (if such a mechanism were ever adopted).',
    'If prescriptive, the maximalist reading is reinforced. If foundational, the utility reading gains legitimacy, potentially leading to a reclassification of the constraint as a ''rope'' or ''scaffold'' for innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maximalist_vs_utility_reading_ambiguity, conceptual, 'Ambiguity in the foundational intent of the Bitcoin whitepaper.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., code review processes, network effects) or internalized (e.g., ideological purity tests, social pressure within the community)?',
    'Post-exit suppression trajectory: if developers leave the Bitcoin ecosystem but continue to self-censor or face reputational damage for proposing changes, reclassify as partially internalized. Analysis of developer forums for explicit vs. implicit pressure.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — developers carry the suppression with them after exit, making the ''snare'' more potent. If purely structural, changes to governance mechanisms could alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Bitcoin maximalist community.').

omega_variable(
    mandatrophy_of_immutability,
    'Has the ''founding covenant'' of immutability, while solving the original problem of centralized monetary control, become a new problem by hindering necessary evolution for global adoption?',
    'Empirical data on Bitcoin''s transaction capacity, fees, and user growth compared to other cryptocurrencies or payment systems over time. A clear divergence where Bitcoin lags due to immutability would indicate mandatrophy.',
    'If mandatrophy is confirmed, the constraint''s ''snare'' classification is strengthened, as its original coordination function has atrophied into pure extraction of ideological purity from those who bear the costs of stagnation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_immutability, empirical, 'Whether the immutability mandate has outlived its functional utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This maximalist reading directly influences the operating environment and legitimacy of the pragmatic synthesis and utility readings by defining the boundaries of acceptable change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
