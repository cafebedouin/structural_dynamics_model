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
 *   RFC defines a semantic contract (reliable byte stream) but allows
 *   implementers freedom to optimize for performance (e.g., congestion
 *   control algorithms like BBR, DCTCP) as long as the contract is met. This
 *   fosters innovation without breaking interoperability. The constraint is
 *   classified as a Rope due to its strong coordination function and low
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.15).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.1).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Optimization Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/distributed_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '22451e8b-825a-4f9c-9dc5-c894aec49a4d').
narrative_ontology:cs_kernel_codification('22451e8b-825a-4f9c-9dc5-c894aec49a4d', formalized).
narrative_ontology:cs_authority_grounding('22451e8b-825a-4f9c-9dc5-c894aec49a4d', expertise).
narrative_ontology:cs_interpretation_layer_present('22451e8b-825a-4f9c-9dc5-c894aec49a4d').
narrative_ontology:cs_reading_relation('22451e8b-825a-4f9c-9dc5-c894aec49a4d', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('22451e8b-825a-4f9c-9dc5-c894aec49a4d', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('22451e8b-825a-4f9c-9dc5-c894aec49a4d', foundational, semantic_contract_over_implementation_details).
narrative_ontology:cs_axiom_status(semantic_contract_over_implementation_details, holdable).
narrative_ontology:cs_axiom_grounding('22451e8b-825a-4f9c-9dc5-c894aec49a4d', semantic_contract_over_implementation_details, conventional).
narrative_ontology:cs_axiom('22451e8b-825a-4f9c-9dc5-c894aec49a4d', foundational, performance_innovation_is_desirable).
narrative_ontology:cs_axiom_status(performance_innovation_is_desirable, holdable).
narrative_ontology:cs_axiom_grounding('22451e8b-825a-4f9c-9dc5-c894aec49a4d', performance_innovation_is_desirable, instrumental).
narrative_ontology:cs_reference_frame('22451e8b-825a-4f9c-9dc5-c894aec49a4d', evolving_interoperable_internet).
narrative_ontology:cs_drift_state('22451e8b-825a-4f9c-9dc5-c894aec49a4d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('22451e8b-825a-4f9c-9dc5-c894aec49a4d', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the clear semantic contract while retaining freedom to innovate on performance. They can deploy new congestion control algorithms (e.g., BBR, DCTCP) as long as they adhere to the specified reliable byte stream behavior, fostering competition and progress.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers, beneficiary,
    organized, biographical, mobile, global).

% Benefits from the interoperability guaranteed by the specification, allowing diverse TCP implementations to coexist on their networks. They also benefit from performance improvements that optimize network resource utilization.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).

% Benefits from the reliable and predictable transport service provided by TCP, allowing them to focus on application logic without worrying about packet loss or reordering. They implicitly benefit from performance optimizations that improve user experience.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    moderate, immediate, mobile, global).

% The body responsible for developing and maintaining RFCs. They define the specification, balancing the need for interoperability with the desire for innovation. Their role is to adjudicate what constitutes 'within semantic bounds'.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_engineering_task_force_ietf, agenda_setter,
    institutional, generational, analytical, global).

% Advocates for a rigid interpretation of TCP, believing any deviation from a precise state machine implementation risks breaking global interoperability. They are excluded from the dominant interpretation that allows optimization latitude.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_advocates, excluded,
    organized, biographical, constrained, global).

% Must ensure their network devices (firewalls, NATs, load balancers) correctly handle diverse TCP implementations that adhere to the semantic contract. This requires more sophisticated design than if all TCP implementations were strictly identical.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_manufacturers, payer,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of diverse TCP implementations across the global internet, ensuring interoperability and a reliable byte stream service for applications, while allowing for performance innovation.
% TRANSFER_FUNCTION: Primarily transfers the benefit of interoperability and performance gains to network participants. There is minimal direct extraction, as the specification itself is a public good.
% ABSENT_VOICES: Advocates for a strict, invariant TCP state machine are often marginalized in discussions that prioritize performance and innovation, arguing that such latitude introduces subtle interoperability risks that are hard to detect.
% DISAPPEARANCE_RATIONALE: If RFC 9293 vanished, the internet's core transport layer would rapidly fragment. Implementations would diverge without a common semantic contract, leading to widespread interoperability failures, application breakage, and a collapse of reliable communication.
% FOUNDING_PROBLEM: The need for a robust, reliable, and interoperable transport protocol that could evolve to meet changing network demands and performance requirements.
% FOUNDING_PROBLEM_CORROBORATION: The IETF and network operators consistently attest to the ongoing need for both interoperability and performance evolution in TCP. Independent researchers and application developers corroborate that the core problem of reliable data transfer in a dynamic network environment remains live, with continuous pressure for optimization.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low because the specification primarily provides a common framework, with minimal costs imposed beyond adherence to the semantic contract. Suppression is low as implementers are free to choose their optimization strategies, and the IETF's enforcement is primarily through peer review and consensus, not coercion. Theater ratio is very low, as the specification's function is genuinely to coordinate and enable, not to obscure. The slight increase in extractiveness and suppression over time reflects the growing complexity of maintaining interoperability across an increasingly diverse set of optimized implementations.
 *
 * PERSPECTIVAL GAP:
 *   While most stakeholders benefit from this reading, middlebox manufacturers face increased complexity. Strict invariance advocates perceive this latitude as a risk to the core stability of TCP, a perspective that is not dominant in the IETF's current approach. The engine's classification will reflect the overall low extraction, but the 'payer' and 'excluded' seats will show higher directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   TCP implementers, network operators, and application developers are all beneficiaries, gaining from interoperability and performance. The IETF acts as an agenda-setter, defining and evolving the standard. Middlebox manufacturers bear some cost in ensuring compatibility with diverse implementations, making them payers. Strict invariance advocates are excluded, as their position is not the dominant interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_bounds_ambiguity,
    'How precisely are ''semantic bounds'' defined, and at what point does an optimization cross the line into non-compliance?',
    'Formal verification of new congestion control algorithms against a rigorous semantic model of TCP, or IETF consensus on specific edge cases.',
    'If bounds are too loose, interoperability could degrade, shifting the constraint towards a Tangled Rope. If too strict, innovation is stifled, increasing extractiveness for implementers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_bounds_ambiguity, conceptual, 'Ambiguity in defining the precise limits of ''optimization latitude'' within TCP''s semantic contract.').

omega_variable(
    middlebox_interference_impact,
    'To what extent do deployed middleboxes (e.g., firewalls, NATs) implicitly enforce a ''strict invariance'' interpretation, hindering optimization latitude in practice?',
    'Large-scale empirical studies of TCP behavior across diverse networks, measuring the impact of middleboxes on various optimized TCP implementations.',
    'If middleboxes significantly constrain optimization, the effective suppression for implementers is higher than specified, potentially shifting the constraint towards a Tangled Rope or Snare in practice, despite the specification''s intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_interference_impact, empirical, 'The practical limits of optimization latitude imposed by the installed base of network middleboxes.').

omega_variable(
    kernel_reading_difference,
    'This constraint is one reading of the ''rfc9293_tcp_specification'' kernel. What structural elements would change if a ''strict_invariance_reading'' or ''middlebox_realism_reading'' were adopted?',
    'Analysis of IETF working group debates, historical RFC revisions, and academic papers advocating for alternative interpretations.',
    'A ''strict_invariance_reading'' would increase suppression and extractiveness for implementers by limiting innovation. A ''middlebox_realism_reading'' would shift authority from the IETF specification to empirical observation, potentially leading to a more ''implicit'' kernel codification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Structural differences between the ''optimization latitude'' reading and its sibling readings of the TCP specification kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the RFC 9293 TCP specification kernel. This 'optimization latitude' reading emphasizes flexibility for performance innovation within semantic bounds, contrasting with 'strict invariance' and 'middlebox realism' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
