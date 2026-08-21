% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__optimization_latitude_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Specification: Optimization Latitude Reading
 *   domain: network_protocol_engineering/distributed_systems
 *
 * SUMMARY:
 *   This constraint story represents the 'optimization latitude' reading of
 *   RFC 9293, the core TCP specification. This reading emphasizes that the
 *   RFC defines a semantic contract for reliable byte stream delivery, but
 *   intentionally grants implementers flexibility to optimize performance
 *   (e.g., congestion control algorithms like BBR or DCTCP) as long as the
 *   observable behavior remains compliant with the semantic contract. This
 *   allows for innovation and adaptation without breaking global
 *   interoperability. The constraint is classified as a Rope due to its
 *   strong coordination function and low extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.15).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.2).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Optimization Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/distributed_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '7445f577-6120-4f7e-9f7c-9b0d2e57ca59').
narrative_ontology:cs_kernel_codification('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', fixed_text).
narrative_ontology:cs_authority_grounding('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', expertise).
narrative_ontology:cs_interpretation_layer_present('7445f577-6120-4f7e-9f7c-9b0d2e57ca59').
narrative_ontology:cs_reading_relation('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', foundational, semantic_contract_over_implementation_details).
narrative_ontology:cs_axiom_status(semantic_contract_over_implementation_details, holdable).
narrative_ontology:cs_axiom_grounding('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', semantic_contract_over_implementation_details, conventional).
narrative_ontology:cs_axiom('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', foundational, performance_innovation_within_interoperability_bounds).
narrative_ontology:cs_axiom_status(performance_innovation_within_interoperability_bounds, holdable).
narrative_ontology:cs_axiom_grounding('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', performance_innovation_within_interoperability_bounds, instrumental).
narrative_ontology:cs_reference_frame('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', flexible_interoperable_tcp).
narrative_ontology:cs_drift_state('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7445f577-6120-4f7e-9f7c-9b0d2e57ca59', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and maintain TCP stacks (e.g., Linux kernel, Windows, BSD). They benefit from the clear semantic contract and the freedom to innovate on performance, allowing them to differentiate their products while ensuring interoperability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers, beneficiary,
    organized, biographical, mobile, global).

% Manage large-scale networks and rely on TCP's reliable byte stream for application traffic. They benefit from the stability and predictability of the protocol's core behavior, while optimization latitude allows for better network utilization and performance.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).

% Build applications that use TCP as a transport. They benefit from the 'illusion' of a perfect, reliable pipe, abstracting away network complexities. Performance optimizations improve user experience without requiring application-level changes.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    moderate, immediate, mobile, global).

% The standards body responsible for RFC 9293. They define the semantic contract and the boundaries of acceptable behavior, balancing strictness for interoperability with flexibility for innovation. Their role is to maintain the integrity of the internet's core protocols.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_engineering_task_force_ietf, agenda_setter,
    institutional, civilizational, analytical, universal).

% Produce network devices (firewalls, NATs, load balancers) that often inspect or modify TCP traffic. While RFC 9293's flexibility is generally beneficial, it can complicate middlebox design if optimizations introduce unexpected behaviors that break their assumptions about TCP's internal state.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global development and deployment of TCP implementations by providing a clear, stable semantic contract for reliable data transfer, while allowing diverse performance optimizations to coexist without breaking interoperability.
% TRANSFER_FUNCTION: Facilitates the transfer of data across the internet by ensuring that different TCP implementations can communicate reliably, effectively transferring the burden of network unreliability from applications to the transport layer.
% ABSENT_VOICES: While the IETF process is open, the voices of those advocating for extremely strict, invariant TCP implementations (who might fear any deviation from a canonical state machine) are often marginalized in favor of practical performance gains. Similarly, those advocating for middlebox-aware protocol design might find their concerns secondary to endpoint flexibility.
% DISAPPEARANCE_RATIONALE: If the RFC 9293 specification (and its interpretive latitude) vanished, TCP implementations would rapidly diverge, leading to widespread interoperability failures, performance regressions, and a breakdown of the internet's core communication fabric. The global network would become a collection of incompatible islands.
% FOUNDING_PROBLEM: The original TCP specification needed to define a reliable, ordered, error-checked byte stream over an unreliable packet network, while also allowing for future performance improvements and adaptation to diverse network conditions.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers, academic researchers, and major cloud providers consistently attest that the core problem of reliable data transfer and the need for ongoing performance optimization in a dynamic internet environment remain live. The IETF's continuous work on TCP extensions and updates corroborates this.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).
:- end_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the specification primarily serves to coordinate behavior for mutual benefit, with minimal overhead beyond the inherent complexity of protocol design. Suppression is low (0.2) as compliance is largely driven by the self-interest of interoperability rather than active coercion; deviations are typically self-correcting due to network effects. Theater ratio is negligible (0.05) as the specification is highly functional and directly impacts network operation. The metrics are stable over time, reflecting the enduring nature of this interpretive stance.
 *
 * PERSPECTIVAL GAP:
 *   While this reading sees the specification as a beneficial coordination mechanism, a 'strict invariance' reading might perceive any implementation latitude as a source of potential instability or non-compliance, leading to a higher perceived 'suppression' of strict adherence. A 'middlebox realism' reading might see the specification as less authoritative than the de facto behavior enforced by deployed middleboxes, shifting the perceived 'agenda-setter' role.
 *
 * DIRECTIONALITY LOGIC:
 *   TCP implementers, network operators, and application developers are all beneficiaries, gaining from the interoperability and performance enabled by the specification's balance of strictness and flexibility. The IETF acts as the agenda-setter, defining and maintaining this balance. Middlebox vendors might experience some 'payer' aspects if they need to adapt their devices to new, compliant TCP optimizations, but this is generally a cost of doing business in a dynamic ecosystem rather than direct extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_of_semantic_compliance,
    'What constitutes ''semantic compliance'' when implementation details vary? At what point does an optimization cross the line into non-compliant behavior?',
    'Formal verification methods for protocol behavior, extensive interoperability testing across diverse implementations, and IETF consensus on specific edge cases.',
    'If the boundary is too ambiguous, it could lead to interoperability issues, increasing the effective extractiveness (cost of debugging/patching) and suppression (fear of non-compliance). If too strict, it stifles innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_of_semantic_compliance, empirical, 'Ambiguity in defining the precise boundary between compliant optimization and non-compliant deviation.').

omega_variable(
    middlebox_impact_on_optimization,
    'To what extent do deployed middleboxes (firewalls, NATs, proxies) implicitly constrain or break TCP optimizations, effectively reducing the ''latitude'' intended by the specification?',
    'Large-scale network measurements of TCP behavior in the presence of middleboxes, and analysis of middlebox vendor implementation details.',
    'If middleboxes significantly curtail optimization latitude, the effective suppression for implementers is higher than perceived, and the coordination function is degraded by external factors. This would push the constraint towards a Tangled Rope or even Snare from the implementer''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_impact_on_optimization, empirical, 'The gap between specified optimization latitude and actual latitude due to middlebox interference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the RFC 9293 TCP specification kernel. This 'optimization latitude' reading emphasizes flexibility for performance within semantic bounds, contrasting with 'strict invariance' (exact replication) and 'middlebox realism' (network behavior dictates specification authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
