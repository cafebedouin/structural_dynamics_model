% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__utility_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'utility reading' of the Bitcoin consensus
 *   kernel, which interprets the whitepaper as establishing a minimum viable
 *   consensus mechanism designed for iterative improvement. This reading
 *   views soft forks and layer-2 protocols as legitimate and necessary
 *   evolutions to enhance the network's utility and scalability.
 *   Beneficiaries are adopters and builders who thrive on this adaptability,
 *   while victims are those who prioritize an immutable, ossified monetary
 *   policy above all else. The claimed type is 'rope' because it facilitates
 *   coordination for a broad set of participants, with moderate extraction
 *   primarily from those who resist any change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.45).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.3).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'a23e1bf9-7bb5-4911-989f-50dc8a95d403').
narrative_ontology:cs_kernel_codification('a23e1bf9-7bb5-4911-989f-50dc8a95d403', fixed_text).
narrative_ontology:cs_authority_grounding('a23e1bf9-7bb5-4911-989f-50dc8a95d403', lineage).
narrative_ontology:cs_interpretation_layer_present('a23e1bf9-7bb5-4911-989f-50dc8a95d403').
narrative_ontology:cs_reading_relation('a23e1bf9-7bb5-4911-989f-50dc8a95d403', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a23e1bf9-7bb5-4911-989f-50dc8a95d403', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('a23e1bf9-7bb5-4911-989f-50dc8a95d403', foundational, iterative_improvement_is_core_to_utility).
narrative_ontology:cs_axiom_status(iterative_improvement_is_core_to_utility, holdable).
narrative_ontology:cs_axiom_grounding('a23e1bf9-7bb5-4911-989f-50dc8a95d403', iterative_improvement_is_core_to_utility, instrumental).
narrative_ontology:cs_axiom('a23e1bf9-7bb5-4911-989f-50dc8a95d403', secondary, layer_2_solutions_are_legitimate_evolution).
narrative_ontology:cs_axiom_status(layer_2_solutions_are_legitimate_evolution, holdable).
narrative_ontology:cs_axiom_grounding('a23e1bf9-7bb5-4911-989f-50dc8a95d403', layer_2_solutions_are_legitimate_evolution, conventional).
narrative_ontology:cs_reference_frame('a23e1bf9-7bb5-4911-989f-50dc8a95d403', minimum_viable_consensus_for_cash_system).
narrative_ontology:cs_drift_state('a23e1bf9-7bb5-4911-989f-50dc8a95d403', contemporary_scaling_challenges, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a23e1bf9-7bb5-4911-989f-50dc8a95d403', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters_and_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantee_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, miners_and_nodes).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, miners_and_nodes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the network's flexibility to evolve through soft forks and layer-2 solutions, enabling new use cases and scaling. They actively contribute to development and advocate for pragmatic evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters_and_builders, beneficiary,
    organized, generational, mobile, global).

% Their work on scaling solutions and new applications is directly enabled by the utility reading's acceptance of iterative improvement. They see the base layer as a foundation, not a rigid cage.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_2_developers, beneficiary,
    moderate, biographical, mobile, global).

% Bear the cost of perceived instability or deviation from a fixed monetary policy. They prioritize the 'sound money' aspect above all else and view any change as a violation of the original covenant, even if it enables utility.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantee_advocates, payer,
    powerful, civilizational, constrained, global).

% Maintain the Bitcoin protocol, proposing and implementing changes (soft forks) that align with the utility reading's philosophy of iterative improvement. They balance stability with necessary evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Enforce the consensus rules and validate transactions. They incur costs for upgrades and may face social pressure from different ideological camps, but ultimately benefit from a growing, adaptable network.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners_and_nodes, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, miners_and_nodes, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, secure, and adaptable digital monetary system that can evolve to meet new demands and scale, coordinating a global network of users, developers, and validators.
% TRANSFER_FUNCTION: Facilitates the transfer of value and enables the creation of new financial applications, moving economic activity and innovation from centralized systems to a distributed network.
% ABSENT_VOICES: Those who believe in a completely static, unchangeable base layer, viewing any evolution as a betrayal of the original vision, are often marginalized in discussions focused on utility and growth.
% DISAPPEARANCE_RATIONALE: If the utility reading of the Bitcoin consensus kernel vanished, the network would likely ossify, hindering innovation and scalability. This would lead to a significant shift in the cryptocurrency landscape, with other, more adaptable protocols gaining prominence, and a substantial rearrangement of economic activity built on Bitcoin.
% FOUNDING_PROBLEM: The original Bitcoin whitepaper aimed to create a 'peer-to-peer electronic cash system' that could function without trusted third parties, solving the double-spending problem and enabling digital transactions.
% FOUNDING_PROBLEM_CORROBORATION: The problem of secure, decentralized digital cash remains live, as attested by ongoing global demand for such systems and the continuous development efforts by a broad community of engineers, economists, and users outside of any single ideological camp. The utility reading emphasizes that the 'cash system' aspect implies a need for ongoing functionality and adaptation.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).
:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs associated with network upgrades and the ideological friction from those who prefer immutability. Suppression (0.30) is low, as the consensus mechanism is designed to be open to proposals, though social and technical hurdles exist for significant changes. The theater ratio (0.10) is low, indicating that most activity is genuinely functional, focused on development and improvement rather than performative maintenance. Accessibility collapse is moderate (0.40) because while the core protocol is hard to change, layer-2 solutions offer alternative paths for innovation. Resistance (0.25) is present from those advocating for a more rigid interpretation, but it is not overwhelming.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adopters and builders, this reading is a 'rope' that enables a vibrant ecosystem. For advocates of monetary ossification, it might feel more like a 'tangled rope' or even a 'snare' due to the perceived erosion of their core principles. The engine's per-seat classification will capture this divergence based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters and builders, along with layer-2 developers, are clear beneficiaries (low d) as the utility reading directly enables their activities. Monetary ossification guarantee advocates are victims (high d) as their core value of immutability is 'extracted' or compromised by this reading's acceptance of change. Core developers act as agenda-setters, balancing different interests, while miners and nodes are payers who also benefit from a healthy network.
 *
 * MANDATROPHY ANALYSIS:
 *   The utility reading actively prevents mandatrophy by embracing iterative improvement. It ensures the constraint's mandate (a functional peer-to-peer electronic cash system) remains live by allowing adaptation, rather than letting the system ossify and become a 'piton' or 'snare' due to irrelevance or inability to scale. The contest with other readings is precisely about whether this adaptation is legitimate or a deviation from the original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_vs_immutability_balance,
    'What is the optimal balance between maintaining the core principles of the Bitcoin whitepaper and allowing for iterative improvements to enhance utility and scalability?',
    'Long-term observation of network adoption, security, and decentralization metrics under different upgrade philosophies, coupled with economic modeling of trade-offs.',
    'If a more rigid approach proves superior for long-term security and decentralization, the utility reading''s extractiveness might be re-evaluated as a cost to those values. Conversely, if adaptability proves crucial for survival and growth, the utility reading''s benefits would be further validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_vs_immutability_balance, empirical, 'The trade-off between protocol immutability and functional adaptability.').

omega_variable(
    interpretation_legitimacy,
    'Is the ''utility reading'' a legitimate interpretation of the Bitcoin whitepaper''s original intent, or a post-hoc rationalization for desired changes?',
    'Historical analysis of early developer discussions, Satoshi Nakamoto''s communications, and the evolution of the whitepaper''s reception within the community. Conceptual analysis of ''cash system'' implications.',
    'If deemed a post-hoc rationalization, the authority grounding of this reading could shift towards ''practice'' or ''distributed'' rather than ''lineage'', potentially increasing perceived extractiveness for those who value strict adherence to original intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretation_legitimacy, conceptual, 'The legitimacy of the utility-focused interpretation of the whitepaper.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__utility_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This 'utility_reading' emphasizes iterative improvement and adaptability, contrasting with the 'maximalist_reading' (immutable monetary policy) and the 'pragmatic_synthesis' (immutable base layer, flexible upper layers). Each reading constitutes a distinct constraint with different beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
