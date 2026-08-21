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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   consensus mechanism designed for iterative improvement. This perspective
 *   emphasizes the protocol's capacity for evolution through soft forks and
 *   layer-2 solutions to ensure its long-term relevance and security. It
 *   stands in contrast to more rigid interpretations that prioritize absolute
 *   immutability. The classification as a Tangled Rope reflects its genuine
 *   coordination function (secure, decentralized transactions) combined with
 *   an asymmetric extraction from those who value absolute monetary
 *   ossification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.45).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.6).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '98023653-e161-48ac-8026-6f92570e3c23').
narrative_ontology:cs_kernel_codification('98023653-e161-48ac-8026-6f92570e3c23', fixed_text).
narrative_ontology:cs_authority_grounding('98023653-e161-48ac-8026-6f92570e3c23', practice).
narrative_ontology:cs_interpretation_layer_present('98023653-e161-48ac-8026-6f92570e3c23').
narrative_ontology:cs_reading_relation('98023653-e161-48ac-8026-6f92570e3c23', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('98023653-e161-48ac-8026-6f92570e3c23', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('98023653-e161-48ac-8026-6f92570e3c23', foundational, protocol_flexibility_is_virtue).
narrative_ontology:cs_axiom_status(protocol_flexibility_is_virtue, holdable).
narrative_ontology:cs_axiom_grounding('98023653-e161-48ac-8026-6f92570e3c23', protocol_flexibility_is_virtue, deontological).
narrative_ontology:cs_axiom('98023653-e161-48ac-8026-6f92570e3c23', foundational, iterative_improvement_is_necessary).
narrative_ontology:cs_axiom_status(iterative_improvement_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('98023653-e161-48ac-8026-6f92570e3c23', iterative_improvement_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('98023653-e161-48ac-8026-6f92570e3c23', evolutionary_protocol_design).
narrative_ontology:cs_drift_state('98023653-e161-48ac-8026-6f92570e3c23', contemporary_protocol_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('98023653-e161-48ac-8026-6f92570e3c23', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, maximalist_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, miners).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, iterative_improvement_principle).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, protocol_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Bitcoin protocol, propose and review changes (BIPs), and guide the technical direction, enabling iterative improvements through soft forks and other upgrades. They are crucial for the 'utility' aspect of the protocol.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Validate transactions and blocks, enforcing the current consensus rules. Their computational power secures the network and their signaling power is essential for adopting protocol changes. They benefit from a stable, evolving network.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners, agenda_setter,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, miners, beneficiary).

% Use Bitcoin for transactions, value storage, and other applications. They benefit from a protocol that can adapt to new use cases and security challenges, ensuring its long-term viability and utility.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters, beneficiary,
    moderate, biographical, mobile, global).

% Develop applications, services, and infrastructure on top of Bitcoin. They benefit from a flexible protocol that allows for innovation and integration with new technologies, expanding the ecosystem's capabilities.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, builders, beneficiary,
    powerful, biographical, mobile, global).

% Create scaling solutions and advanced applications that rely on Bitcoin's base layer. They directly benefit from the base layer's ability to support and integrate with these innovations, which is a core tenet of the utility reading.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_2_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Hold Bitcoin primarily as a store of value, often with a strong belief in its absolute immutability and fixed monetary policy. They 'pay' by experiencing the erosion of their 'monetary ossification guarantees' as the protocol evolves, which they perceive as a violation of Bitcoin's founding principles.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_investors, payer,
    powerful, generational, identity_locked, global).

% Advocate for a strict, immutable interpretation of the Bitcoin whitepaper, viewing any protocol change as a deviation from its core design. While they participate in discourse, their arguments for absolute ossification are often sidelined in favor of practical utility and iterative improvement within this reading.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_theorists, excluded,
    moderate, civilizational, identity_locked, global).

% Study the Bitcoin protocol, its evolution, and its economic and social implications from an academic or research perspective. They analyze the trade-offs between immutability and utility, providing external commentary without direct participation in consensus.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a secure, decentralized mechanism for transaction validation and a shared ledger, while providing a framework for the community to iteratively improve the protocol through consensus-driven changes (e.g., soft forks) to meet evolving needs and security requirements.
% TRANSFER_FUNCTION: Transfers the power to evolve the protocol from a theoretical state of absolute immutability to a dynamic, community-governed process, allowing for the 'transfer' of new features and capabilities into the system, while extracting the 'guarantee' of a perfectly static monetary policy from maximalist adherents.
% ABSENT_VOICES: Maximalist theorists and investors who prioritize absolute immutability are often excluded from the core decision-making processes regarding protocol evolution, as their fundamental premise (no change) directly conflicts with the utility reading's emphasis on iterative improvement. They would argue against many proposed changes.
% DISAPPEARANCE_RATIONALE: If the consensus mechanism enabling iterative improvement vanished overnight, the Bitcoin protocol would either ossify completely, leading to technological stagnation and eventual irrelevance, or fragment into incompatible chains, destroying its network effect and monetary value. The entire cryptoeconomic ecosystem built on Bitcoin would collapse or be forced to reorganize around a fundamentally different, likely less secure or less adaptable, base layer.
% FOUNDING_PROBLEM: The original problem was to create a decentralized digital cash system that could achieve robust consensus without a central authority, while also being resilient and adaptable enough to survive and thrive in a rapidly changing technological landscape.
% FOUNDING_PROBLEM_CORROBORATION: Independent cryptographers, network engineers, and economists (outside the maximalist camp) corroborate that the challenges of maintaining a secure, decentralized, and relevant digital monetary system are ongoing, requiring continuous vigilance and the capacity for iterative improvement to address new threats and opportunities. This is evidenced by ongoing research into scaling, privacy, and security, and the adoption of various soft forks over Bitcoin's history.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is moderate (0.45) because while the protocol provides immense utility, the continuous evolution (from the perspective of maximalists) 'extracts' the guarantee of a perfectly static, immutable monetary policy. Suppression (0.6) is present as the social and technical costs of hard forks or creating alternative chains are high, effectively suppressing radical deviations from the established evolutionary path. Theater ratio is low (0.1) because the consensus mechanism is highly functional and actively maintained, with minimal performative overhead. Accessibility collapse (0.5) is moderate; while alternatives to Bitcoin exist, migrating to them is costly. Resistance (0.7) is high, reflecting the ongoing ideological battles with maximalist factions who resist any changes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adopters and builders, the constraint is a Rope, enabling innovation and adaptation. From the maximalist perspective, it functions as a Snare, extracting their desired immutability. The engine's computation of a Tangled Rope reflects this inherent tension and the dual nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters, builders, and layer-2 developers are clear beneficiaries (low d) as they gain from a flexible, evolving protocol. Core developers and miners act as agenda-setters and beneficiaries, guiding and securing the evolution. Maximalist investors are the primary targets (high d) as their desired 'monetary ossification guarantees' are undermined by the iterative improvement process. Maximalist theorists are excluded, their arguments for absolute immutability often failing to gain consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from maximalists) or a pure Snare (ignoring the genuine coordination function for the broader ecosystem). It acknowledges that the mandate for iterative improvement is live and functional, but not without cost to certain ideological positions. The constraint's persistence is driven by the ongoing need for adaptation in a dynamic technological environment, rather than mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''utility_reading'' of the Bitcoin whitepaper, or is it a rationalization for a de facto ''pragmatic_synthesis'' that selectively permits change?',
    'Analysis of historical protocol changes: if changes consistently prioritize utility over all other considerations, it supports the utility reading. If changes are selectively permitted only when they don''t touch core monetary policy, it leans towards pragmatic synthesis.',
    'If it''s a pragmatic synthesis, the extractiveness from maximalists might be lower, as some of their core tenets are preserved, potentially shifting the classification closer to a Rope. If it''s a pure utility reading, the extraction from ossification guarantees is more direct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the true intent and scope of protocol evolution.').

omega_variable(
    monetary_ossification_validity,
    'Is ''monetary ossification guarantee'' a valid concept to be ''victimized'', or is it a preference held by a specific group that is not structurally extracted?',
    'Philosophical and economic analysis of ''guarantees'' in decentralized systems: if such guarantees are demonstrably impossible or contradictory, then their ''victimization'' is a mischaracterization of a preference. If they are a coherent, albeit difficult to achieve, state, then the extraction is real.',
    'If not a valid concept, then the ''victims'' are misidentified, reducing the base extractiveness and potentially reclassifying the constraint as a pure Rope. If valid, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_ossification_validity, conceptual, 'Whether the ''victim'' concept is structurally coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2012, 0.08).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2009, 0.3).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2018, 0.43).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2012, 0.48).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_layer_2_protocols).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, decentralized_finance_protocols).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This 'utility_reading' emphasizes iterative improvement, contrasting with the 'maximalist_reading' (immutable monetary policy) and 'pragmatic_synthesis' (immutable base layer, flexible upper layers). Each reading has distinct beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
