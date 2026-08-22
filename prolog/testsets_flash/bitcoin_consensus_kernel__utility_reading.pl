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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'utility reading' of the Bitcoin consensus
 *   kernel, where the whitepaper is understood as establishing a minimum
 *   viable consensus mechanism designed for iterative improvement. This
 *   reading emphasizes the network's adaptability through soft forks and
 *   layer-2 protocols as legitimate evolution, crucial for its long-term
 *   utility and adoption. Beneficiaries are adopters and builders who
 *   leverage this adaptability, while 'monetary ossification guarantees' are
 *   the conceptual victims, as the expectation of absolute immutability is
 *   challenged.
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
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '814e8802-e315-43ca-92f1-d12ede9c0e8f').
narrative_ontology:cs_kernel_codification('814e8802-e315-43ca-92f1-d12ede9c0e8f', fixed_text).
narrative_ontology:cs_authority_grounding('814e8802-e315-43ca-92f1-d12ede9c0e8f', distributed).
narrative_ontology:cs_reading_relation('814e8802-e315-43ca-92f1-d12ede9c0e8f', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('814e8802-e315-43ca-92f1-d12ede9c0e8f', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('814e8802-e315-43ca-92f1-d12ede9c0e8f', foundational, protocol_utility_trumps_absolute_immutability).
narrative_ontology:cs_axiom_status(protocol_utility_trumps_absolute_immutability, holdable).
narrative_ontology:cs_axiom_grounding('814e8802-e315-43ca-92f1-d12ede9c0e8f', protocol_utility_trumps_absolute_immutability, instrumental).
narrative_ontology:cs_axiom('814e8802-e315-43ca-92f1-d12ede9c0e8f', secondary, soft_forks_are_legitimate_evolution).
narrative_ontology:cs_axiom_status(soft_forks_are_legitimate_evolution, holdable).
narrative_ontology:cs_axiom_grounding('814e8802-e315-43ca-92f1-d12ede9c0e8f', soft_forks_are_legitimate_evolution, conventional).
narrative_ontology:cs_reference_frame('814e8802-e315-43ca-92f1-d12ede9c0e8f', minimum_viable_consensus_for_evolution).
narrative_ontology:cs_drift_state('814e8802-e315-43ca-92f1-d12ede9c0e8f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('814e8802-e315-43ca-92f1-d12ede9c0e8f', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters_and_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, maximalist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the network's adaptability, allowing for new use cases and scaling solutions. They actively contribute to the ecosystem's growth and see soft forks and layer-2 protocols as essential for long-term utility.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters_and_builders, beneficiary,
    organized, biographical, mobile, global).

% Directly benefit from the ability to build new protocols and applications on top of Bitcoin, extending its functionality without altering the base layer. They view the base layer as a secure foundation for innovation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_2_developers, beneficiary,
    moderate, biographical, mobile, global).

% Maintain the Bitcoin protocol, proposing and implementing changes (primarily soft forks) that enhance its utility and security. They operate under a strong ethos of minimal, carefully considered changes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Run the software that validates transactions and enforces consensus rules. They bear the cost of upgrading software and validating new features introduced by soft forks, but also benefit from network improvements.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, node_operators, payer,
    moderate, immediate, constrained, global).

% Adhere to a strict interpretation of Bitcoin's whitepaper as an immutable monetary policy. They perceive any change, even soft forks, as a violation of the original covenant, leading to a perceived 'monetary ossification guarantee' being eroded.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_advocates, payer,
    organized, generational, identity_locked, global).

% Represents the abstract concept of an unchangeable monetary policy, which is 'paid' or eroded by the utility-driven evolution of the protocol. This is not an agent but a conceptual victim of the reading's emphasis on iterative improvement.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decentralized, secure, and adaptable digital monetary system that can evolve through consensus-driven improvements (soft forks) and layered protocols, enabling broad utility.
% TRANSFER_FUNCTION: Facilitates the transfer of value across the network, with the 'cost' of adaptability borne by those who prioritize absolute immutability, and the 'benefit' accruing to those who build and use new functionalities.
% ABSENT_VOICES: Those who would advocate for a completely static, 'finished' protocol are present as maximalist advocates, but their arguments for absolute immutability are often sidelined in favor of practical utility and iterative development within the core development community.
% DISAPPEARANCE_RATIONALE: If the consensus mechanism allowing iterative improvement vanished, the Bitcoin network would either stagnate, failing to adapt to new challenges and use cases, or fracture into incompatible versions, leading to a loss of its global monetary utility and network effect. The entire cryptoeconomic landscape would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of creating a decentralized digital cash system that could achieve consensus without a central authority, while also being robust enough to evolve and adapt to future needs.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for scaling solutions, security enhancements, and new application layers (e.g., Lightning Network) attests to the live status of the problem. Independent cryptographers and economists, not directly benefiting from specific protocol changes, corroborate the necessity of iterative improvement for the network's long-term viability.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, reflecting the 'cost' of perceived deviation from absolute immutability for some, balanced against the benefits of innovation. Suppression (0.30) is low, as the mechanism relies on voluntary consensus rather than active coercion, though social pressure exists. Theater ratio (0.10) is low, indicating that most activity genuinely contributes to the network's function and evolution. The claimed type is 'rope' because it facilitates coordination for a broad set of users and developers, with moderate costs for those who prioritize a static protocol.
 *
 * PERSPECTIVAL GAP:
 *   From the utility reading's perspective, the constraint is a flexible rope enabling innovation. From a maximalist perspective, it might be seen as a tangled rope or even a snare, extracting the promise of immutable monetary policy. The engine's per-seat classification will highlight this divergence based on the declared power, exit options, and roles of each stakeholder.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters and builders, along with layer-2 developers, are clear beneficiaries (low d) as they gain from the network's evolving utility. Core developers, while agenda-setters, operate under a strong ethos of minimal change, making them closer to symmetric. Node operators bear some costs of upgrades but also benefit from network health. Maximalist advocates and the abstract 'monetary ossification guarantees' are the targets (high d), as their preference for absolute immutability is 'extracted' or eroded by the utility-driven evolution.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_utility_tradeoff,
    'Is the perceived ''extraction'' from monetary ossification guarantees an unavoidable cost of achieving broader utility and adaptability, or is it a sign of a fundamental betrayal of Bitcoin''s original promise?',
    'Long-term empirical observation of network security, decentralization, and adoption rates under various soft fork implementations, coupled with philosophical analysis of the ''original promise'' of Bitcoin.',
    'If the ''extraction'' is deemed an unavoidable cost for essential utility, the ''rope'' classification is strengthened. If it''s seen as a betrayal, the constraint might lean towards a ''tangled rope'' or ''snare'' from the perspective of maximalist advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_utility_tradeoff, conceptual, 'Ambiguity regarding the fundamental tradeoff between immutability and utility in Bitcoin''s evolution.').

omega_variable(
    soft_fork_legitimacy,
    'To what extent do soft forks genuinely represent ''iterative improvement'' through broad consensus, versus being a form of ''governance capture'' by a subset of core developers?',
    'Analysis of soft fork adoption patterns, including node signaling, miner support, and community discourse, across multiple protocol upgrades. Examination of the power dynamics within the core development community.',
    'If soft forks are consistently driven by a narrow group without broad consensus, the ''agenda_setter'' role''s directionality might shift towards higher extraction, potentially reclassifying the constraint as a ''tangled rope'' from certain seats. If broad, decentralized consensus is consistently demonstrated, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_legitimacy, empirical, 'The true nature of consensus for protocol changes (soft forks).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2008, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2012, 0.07).
narrative_ontology:measurement(bitc_tr_t2016, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2008, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2012, 0.25).
narrative_ontology:measurement(bitc_be_t2016, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2016, 0.35).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2008, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2012, 0.15).
narrative_ontology:measurement(bitc_su_t2016, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2016, 0.2).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This 'utility_reading' emphasizes iterative improvement and adaptability, contrasting with the 'maximalist_reading' (immutable monetary policy) and the 'pragmatic_synthesis' (immutable base, flexible upper layers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
