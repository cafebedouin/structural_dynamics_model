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
 *   prioritizes adaptability through soft forks and layer-2 protocols,
 *   viewing them as legitimate evolution rather than violations of a founding
 *   covenant. It acknowledges a moderate level of extractiveness inherent in
 *   maintaining a decentralized system, but sees the primary beneficiaries as
 *   adopters and builders who leverage this flexibility. The 'victims' are
 *   the abstract concept of absolute monetary ossification guarantees, which
 *   are necessarily compromised by this evolutionary approach. The claimed
 *   type is 'rope' because it aims for coordination and collective benefit
 *   through a flexible framework.
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
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '716b18e3-44ac-4f1e-9aa2-2aceef9855ba').
narrative_ontology:cs_kernel_codification('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', fixed_text).
narrative_ontology:cs_authority_grounding('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', practice).
narrative_ontology:cs_interpretation_layer_present('716b18e3-44ac-4f1e-9aa2-2aceef9855ba').
narrative_ontology:cs_reading_relation('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', foundational, protocol_adaptability_is_key_to_longevity).
narrative_ontology:cs_axiom_status(protocol_adaptability_is_key_to_longevity, holdable).
narrative_ontology:cs_axiom_grounding('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', protocol_adaptability_is_key_to_longevity, instrumental).
narrative_ontology:cs_axiom('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', foundational, soft_forks_are_legitimate_evolutionary_mechanisms).
narrative_ontology:cs_axiom_status(soft_forks_are_legitimate_evolutionary_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', soft_forks_are_legitimate_evolutionary_mechanisms, conventional).
narrative_ontology:cs_reference_frame('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', minimum_viable_consensus_for_evolution).
narrative_ontology:cs_drift_state('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('716b18e3-44ac-4f1e-9aa2-2aceef9855ba', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters_and_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the flexibility to adapt the protocol through soft forks and build layer-2 solutions, enabling new use cases and scaling. They see the whitepaper as a starting point for evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters_and_builders, beneficiary,
    organized, biographical, mobile, global).

% Directly benefit from the ability to innovate on top of the base layer without requiring hard forks. Their work is enabled by the utility reading's emphasis on iterative improvement.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_2_developers, beneficiary,
    moderate, immediate, mobile, global).

% Represents the abstract concept of an unchangeable, immutable monetary policy. This reading 'pays' by allowing changes that maximalists would consider violations of the original covenant, thus eroding the absolute guarantee of ossification.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees, payer,
    powerless, generational, identity_locked, global).

% Would argue vehemently against any changes to the base layer, viewing the whitepaper as an immutable constitution. Their voice is marginalized in this reading's framework, which prioritizes utility and evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_advocates, excluded,
    organized, generational, identity_locked, global).

% Observe and analyze the utility reading's approach, often seeking a middle ground where base layer immutability is preserved for monetary policy, but innovation is encouraged on higher layers. They represent a competing, but not entirely foreclosed, perspective.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, pragmatic_synthesis_advocates, observer,
    powerful, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for a distributed network to agree on the state of a ledger and evolve its capabilities through consensus-driven upgrades, fostering innovation and adaptability.
% TRANSFER_FUNCTION: Facilitates the transfer of value and utility by enabling a flexible, programmable monetary system, moving the 'cost' of ossification from the base layer to the realm of potential innovation.
% ABSENT_VOICES: Strict maximalists who believe in absolute immutability of the base layer are largely excluded from the discourse that shapes this reading, as their core premise is seen as hindering necessary evolution.
% DISAPPEARANCE_RATIONALE: If this reading of the Bitcoin consensus kernel vanished, the ecosystem would likely fracture. Innovation would slow significantly, layer-2 development would be stifled, and the network's ability to adapt to new challenges would be severely hampered, leading to a re-evaluation of its long-term viability.
% FOUNDING_PROBLEM: The problem of creating a decentralized digital cash system that could achieve consensus without a central authority, while also allowing for future improvements and scaling.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for scaling solutions and new functionalities in a decentralized context, attested by numerous developers, researchers, and industry participants outside of the immediate beneficiaries of this reading.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the costs of maintaining decentralized consensus and the implicit 'tax' on absolute immutability. Suppression (0.30) is relatively low, as this reading thrives on open discussion and community consensus for changes, rather than coercive enforcement. Theater ratio (0.10) is low, indicating that the stated purpose of iterative improvement genuinely drives activity. The metrics reflect a system that, while not perfectly frictionless, is primarily functional and adaptive.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is internally coherent, it creates a significant perspectival gap with maximalist readings, which would view any deviation from the original whitepaper as a betrayal. The utility reading sees this as necessary evolution, while maximalists see it as a fundamental compromise of Bitcoin's core value proposition. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters and builders, especially layer-2 developers, are clear beneficiaries as their innovation is enabled. The abstract 'monetary ossification guarantees' are the 'payer' in the sense that their absolute nature is diluted by this reading's flexibility. Maximalist advocates are 'excluded' as their rigid interpretation is not accommodated by this framework. Pragmatic synthesis advocates are 'observers' as they analyze this reading's implications from a slightly different, but not entirely opposed, perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_adaptability_tradeoff,
    'What is the optimal balance between protocol immutability and adaptability for a long-term decentralized monetary system?',
    'Long-term empirical observation of other decentralized systems'' success or failure in adapting to technological and economic changes, alongside theoretical modeling of protocol evolution.',
    'If absolute immutability proves unsustainable or leads to irrelevance, this reading''s utility-focused approach would be vindicated. If adaptability introduces critical vulnerabilities or governance capture, the maximalist reading would gain credence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_adaptability_tradeoff, empirical, 'The fundamental tradeoff between a protocol''s fixedness and its capacity to evolve.').

omega_variable(
    legitimacy_of_soft_forks,
    'Are soft forks, which can alter protocol rules without requiring all nodes to upgrade, a legitimate form of consensus-driven evolution or a subtle form of coercion?',
    'Analysis of network participation rates in soft fork activations, the degree of dissent, and the long-term impact on network decentralization and censorship resistance.',
    'If soft forks are consistently adopted with broad, uncoerced consensus, they reinforce this reading''s view of iterative improvement. If they consistently marginalize dissenting minorities, it would lend weight to claims of subtle coercion, pushing the constraint towards a more extractive classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_soft_forks, conceptual, 'Whether soft forks represent genuine consensus or a form of implicit rule enforcement.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the Bitcoin whitepaper fundamentally a blueprint for an immutable monetary system, or a minimum viable consensus mechanism for an evolving digital cash system?',
    'Historical analysis of early developer discussions, Satoshi Nakamoto''s later communications, and the philosophical underpinnings of distributed systems design. This is a conceptual framing choice.',
    'Adopting the maximalist framing would reclassify this constraint as a Snare (coercive deviation from original intent), while adopting the pragmatic synthesis framing would shift its boundaries to focus more on layer-2 innovation as the primary evolutionary path. This reading''s classification as a Rope depends on its own framing of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'The choice of kernel framing (immutable blueprint vs. evolving mechanism) fundamentally alters the constraint''s classification.').


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
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This 'utility reading' emphasizes iterative improvement and adaptability, contrasting with the 'maximalist_reading' (immutable monetary policy) and the 'pragmatic_synthesis' (immutable base layer, flexible upper layers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
