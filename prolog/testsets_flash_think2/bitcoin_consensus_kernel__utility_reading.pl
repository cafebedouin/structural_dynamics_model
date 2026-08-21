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
 *   This constraint represents the 'utility_reading' of the Bitcoin consensus
 *   kernel, which interprets the original whitepaper as establishing a
 *   minimum viable consensus mechanism designed for iterative improvement.
 *   This perspective emphasizes the protocol's adaptability through soft
 *   forks and the development of layer-2 solutions, viewing these as
 *   legitimate evolutions rather than violations of a fixed covenant. The
 *   constraint is claimed as a Tangled Rope because it coordinates
 *   development and network stability but imposes costs on those who
 *   prioritize absolute monetary immutability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.45).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.55).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'ca4eda23-ae9d-46f7-acd9-0e5ac8647138').
narrative_ontology:cs_kernel_codification('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', fixed_text).
narrative_ontology:cs_authority_grounding('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', practice).
narrative_ontology:cs_interpretation_layer_present('ca4eda23-ae9d-46f7-acd9-0e5ac8647138').
narrative_ontology:cs_reading_relation('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', foundational, protocol_evolves_via_consensus).
narrative_ontology:cs_axiom_status(protocol_evolves_via_consensus, holdable).
narrative_ontology:cs_axiom_grounding('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', protocol_evolves_via_consensus, conventional).
narrative_ontology:cs_axiom('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', secondary, layer_separation_for_innovation).
narrative_ontology:cs_axiom_status(layer_separation_for_innovation, holdable).
narrative_ontology:cs_axiom_grounding('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', layer_separation_for_innovation, instrumental).
narrative_ontology:cs_reference_frame('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', iterative_decentralized_evolution).
narrative_ontology:cs_drift_state('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ca4eda23-ae9d-46f7-acd9-0e5ac8647138', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters_and_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_maximalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The distributed network of miners, node operators, and core developers who collectively maintain the Bitcoin protocol and validate transactions. They enforce the consensus rules and implement changes via soft forks, balancing stability with necessary evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, network_participants, agenda_setter,
    institutional, generational, constrained, global).

% Individuals and organizations who use Bitcoin for transactions, build applications, or develop new technologies on top of the protocol. They benefit from the flexibility to adapt the base layer and integrate new features, seeing iterative improvement as essential for long-term utility.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters_and_builders, beneficiary,
    organized, biographical, mobile, global).

% A segment of the Bitcoin community that believes in the absolute immutability of Bitcoin's monetary policy and core protocol rules. They perceive any change or potential for change (even via consensus) as a violation of Bitcoin's founding principles, bearing the cost of perceived uncertainty or 'debasement' of the original vision. Their commitment to Bitcoin is often tied to this immutable ideal.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_maximalists, payer,
    powerful, generational, identity_locked, global).

% Developers building scaling solutions and applications on layers above the Bitcoin base chain (e.g., Lightning Network). They benefit from the base layer's security and the ability for it to evolve to support their innovations, even if their primary focus is off-chain.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_2_developers, beneficiary,
    moderate, biographical, arbitrage, global).

% Governmental and financial authorities observing the Bitcoin network. They analyze its stability, potential for illicit use, and impact on traditional financial systems, considering how its evolutionary path might affect their oversight responsibilities.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decentralized, secure, and censorship-resistant digital ledger that can iteratively improve through community consensus, enabling a platform for ongoing innovation and adaptation.
% TRANSFER_FUNCTION: Transfers the right to propose and implement changes (via soft forks) from a centralized authority to a distributed developer/miner community. It also transfers the perceived cost of absolute monetary immutability (for maximalists) to the benefit of adaptability and utility (for builders).
% ABSENT_VOICES: Proponents of other blockchain protocols or traditional fiat systems, who operate under different consensus mechanisms or monetary philosophies, are not directly part of this internal Bitcoin debate.
% DISAPPEARANCE_RATIONALE: If the Bitcoin consensus mechanism vanished overnight, the entire cryptoeconomic ecosystem built upon it would collapse. Billions in value would be lost, and the global financial and technological landscape would be profoundly disrupted, as a foundational decentralized monetary system would cease to exist.
% FOUNDING_PROBLEM: How to create a decentralized, censorship-resistant digital cash system that can evolve to meet future needs and challenges without relying on a central authority.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for secure, decentralized digital value transfer, the continuous development of the protocol by a global community of developers and users, and the persistent challenges of scaling and security, all attest to the founding problem's continued relevance. This is corroborated by independent academic research and industry reports, not just by benefiting parties.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Base extractiveness is moderate (0.45) as the flexibility for iterative improvement comes at the cost of perceived uncertainty or deviation from a strictly immutable monetary policy for some stakeholders. Suppression (0.55) is moderate, reflecting the active enforcement of consensus rules while still allowing for community-driven evolution. Theater ratio is low (0.1) as the mechanism is highly functional and directly impacts network operation. The slight increase in extractiveness and suppression over time reflects the growing tension between different interpretations of Bitcoin's purpose as the network matures and faces new challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adopters and builders, the iterative improvement mechanism is a clear benefit, enabling the network's long-term utility. For monetary maximalists, this same mechanism represents a cost, as it introduces the possibility of change to what they view as an immutable monetary system. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters and builders, along with Layer-2 developers, are beneficiaries (low d) as they gain from the protocol's adaptability and the ability to innovate. Monetary maximalists are the primary targets (high d) as they bear the perceived cost of any deviation from absolute immutability. Network participants (miners, node operators, core developers) act as agenda-setters, balancing these competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimal_consensus_definition,
    'Is the ''minimum viable consensus mechanism'' truly minimal, or does it implicitly encode more specific, potentially contestable, design choices?',
    'Detailed historical analysis of early Bitcoin development debates and a formal specification of the ''minimal'' components versus emergent conventions.',
    'If the ''minimal'' consensus is found to be more extensive than claimed, it could increase the perceived extractiveness for those who desire even greater flexibility or a different foundational design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimal_consensus_definition, conceptual, 'Ambiguity in the scope of ''minimum viable consensus''.').

omega_variable(
    immutability_vs_adaptability_tradeoff,
    'What is the optimal balance between protocol immutability and adaptability for a decentralized monetary system, and how does this reading''s approach impact long-term network security and value proposition?',
    'Long-term empirical observation of network security, adoption rates, and economic stability in Bitcoin versus other protocols with different immutability/adaptability tradeoffs, combined with ongoing cryptoeconomic research.',
    'If excessive adaptability leads to security vulnerabilities or loss of monetary credibility, the extractiveness for all users could increase; if excessive immutability stifles necessary innovation, the utility and adoption could decline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutability_vs_adaptability_tradeoff, preference, 'The fundamental tradeoff between immutability and adaptability.').

omega_variable(
    cost_of_soft_forks_and_layer_2s,
    'What are the actual, quantifiable costs (e.g., developer time, network risk, user confusion) associated with iterative improvements via soft forks and the complexity of layer-2 protocols?',
    'Empirical studies tracking developer resource allocation, incident reports related to upgrades, and user surveys on complexity and perceived risk over time.',
    'Higher-than-expected costs would increase the effective extractiveness of the ''utility_reading'' for all participants, potentially shifting its classification towards a Snare if the benefits do not outweigh these costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_soft_forks_and_layer_2s, empirical, 'Quantifying the costs of protocol evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(bitc_be_t18, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 18, 0.43).
narrative_ontology:measurement(bitc_be_t24, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(bitc_be_t30, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(bitc_su_t18, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(bitc_su_t24, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(bitc_su_t30, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
