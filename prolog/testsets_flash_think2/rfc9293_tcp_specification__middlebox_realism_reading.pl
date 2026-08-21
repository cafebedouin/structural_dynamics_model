% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification (Middlebox Realism Reading)
 *   domain: network_protocol_engineering
 *
 * SUMMARY:
 *   This constraint represents the 'middlebox realism' reading of RFC 9293,
 *   which specifies the Transmission Control Protocol (TCP). While RFC 9293
 *   describes an ideal, end-to-end protocol, the reality of the internet is
 *   shaped by a pervasive population of 'middleboxes' (e.g., firewalls, NATs,
 *   deep packet inspection systems) that modify, filter, or otherwise
 *   interfere with TCP traffic. This reading asserts that the de facto
 *   authority over TCP behavior has shifted from the RFC specification to the
 *   deployed middlebox population, making the RFC an aspiration rather than
 *   an enforceable standard. The constraint is classified as a Tangled Rope
 *   because it still attempts to coordinate (the RFC's ideal behavior) but
 *   simultaneously extracts control and imposes costs due to middlebox
 *   interference.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.8).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.85).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, 'b48ff734-473e-4eea-a6f6-7485565f7daa').
narrative_ontology:cs_kernel_codification('b48ff734-473e-4eea-a6f6-7485565f7daa', fixed_text).
narrative_ontology:cs_authority_grounding('b48ff734-473e-4eea-a6f6-7485565f7daa', practice).
narrative_ontology:cs_interpretation_layer_present('b48ff734-473e-4eea-a6f6-7485565f7daa').
narrative_ontology:cs_reading_relation('b48ff734-473e-4eea-a6f6-7485565f7daa', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('b48ff734-473e-4eea-a6f6-7485565f7daa', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('b48ff734-473e-4eea-a6f6-7485565f7daa', foundational, network_behavior_trumps_specification).
narrative_ontology:cs_axiom_status(network_behavior_trumps_specification, holdable).
narrative_ontology:cs_axiom_grounding('b48ff734-473e-4eea-a6f6-7485565f7daa', network_behavior_trumps_specification, empirically_contingent).
narrative_ontology:cs_axiom('b48ff734-473e-4eea-a6f6-7485565f7daa', foundational, middleboxes_are_active_participants).
narrative_ontology:cs_axiom_status(middleboxes_are_active_participants, holdable).
narrative_ontology:cs_axiom_grounding('b48ff734-473e-4eea-a6f6-7485565f7daa', middleboxes_are_active_participants, empirically_contingent).
narrative_ontology:cs_reference_frame('b48ff734-473e-4eea-a6f6-7485565f7daa', end_to_end_principle_adherence).
narrative_ontology:cs_drift_state('b48ff734-473e-4eea-a6f6-7485565f7daa', contemporary_internet_deployment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b48ff734-473e-4eea-a6f6-7485565f7daa', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, internet_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devices (servers, clients) that implement TCP according to RFC 9293, but whose traffic is often modified, filtered, or shaped by middleboxes, leading to non-standard behavior and performance issues. They bear the cost of adapting to an unpredictable network path.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints, payer,
    powerless, immediate, constrained, global).

% Internet Service Providers, enterprise network administrators, and state actors who deploy and configure middleboxes (e.g., firewalls, NATs, DPI systems) that inspect and modify TCP traffic. They gain control over network policy, security, and surveillance, often at the expense of end-to-end transparency and protocol adherence.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, biographical, arbitrage, global).

% Develop applications that rely on TCP's specified behavior. They face challenges due to middlebox interference, requiring complex workarounds or accepting degraded performance, which increases development cost and reduces reliability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Experience the direct consequences of middlebox interference, such as slower connections, blocked services, or reduced privacy, without direct control or awareness of the underlying protocol modifications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_users, payer,
    powerless, immediate, constrained, global).

% The community responsible for writing and maintaining RFCs like 9293. While they define the ideal protocol, their authority is often superseded by the operational reality of deployed middleboxes, leading to a gap between specification and practice.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standard_authors, observer,
    organized, generational, analytical, global).

% Study the behavior of TCP in real-world networks, documenting the impact of middleboxes and the divergence from RFC specifications. They provide empirical evidence of the constraint's operation but have limited direct power to change it.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define a universally interoperable, reliable, and efficient transport protocol (TCP) that allows any two endpoints on the internet to communicate predictably and robustly.
% TRANSFER_FUNCTION: Transfers effective control over network traffic behavior from endpoint applications and their users to middlebox operators, and transfers the burden of adapting to non-standard, path-dependent behavior to application developers and users.
% ABSENT_VOICES: Advocates for the 'end-to-end principle' (where network intelligence resides at the endpoints), privacy advocates, and individual internet users in heavily surveilled or censored regions. These groups would argue for strict adherence to open standards and transparent network operations, but their concerns are often overridden by middlebox deployment rationales.
% DISAPPEARANCE_RATIONALE: If middleboxes strictly adhered to RFC 9293 or ceased to exist, network behavior would become significantly more predictable and transparent. Application development would simplify, endpoint autonomy would increase, and the internet's operational reality would fundamentally shift towards its original architectural principles.
% FOUNDING_PROBLEM: The fundamental need for a robust, reliable, and interoperable transport layer protocol to enable communication between diverse hosts across packet-switched networks, ensuring data integrity and ordered delivery.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers, academic researchers, and internet service providers universally attest to the ongoing critical need for a reliable transport protocol. The problem of reliable data transfer across heterogeneous networks remains central to internet functionality, even as its implementation is complicated by middleboxes.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because middleboxes impose significant costs on endpoints and application developers through unpredictable behavior, performance degradation, and blocked functionality. Suppression is very high as endpoints have limited means to resist or circumvent middlebox interference, which is often enforced by powerful network operators. The theater ratio is moderate, reflecting that while RFC 9293 is still a foundational document, its practical authority is diminished by the operational reality it purports to govern. The increasing trends in extractiveness and suppression over time reflect the growing prevalence and sophistication of middleboxes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IETF standard authors, RFC 9293 is a foundational Rope, coordinating global interoperability. From the perspective of middlebox operators, it's a flexible framework allowing necessary network management. However, from the perspective of endpoints and application developers, the same RFC, when filtered through the middlebox reality, functions as a Tangled Rope, extracting control and imposing costs. The engine's classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators are the primary beneficiaries and agenda-setters, gaining control and policy enforcement capabilities. TCP endpoints, application developers, and internet users are the victims, bearing the costs of non-standard behavior, reduced performance, and loss of end-to-end transparency. IETF standard authors and network researchers act as observers, documenting the discrepancy between specification and reality but having limited direct power to alter the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_necessity_vs_extraction,
    'To what extent is middlebox interference a necessary function for network security and policy enforcement, versus a mechanism for pure extraction of control or data?',
    'Detailed, independent audits of middlebox configurations and traffic logs, coupled with analysis of alternative security architectures that preserve end-to-end principles.',
    'If interference is primarily extractive, the constraint''s effective extractiveness is higher than currently estimated, pushing it closer to a Snare. If largely necessary, the coordination function is more robust, supporting the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_extraction, empirical, 'Distinguishing between legitimate network functions and rent-seeking behavior by middleboxes.').

omega_variable(
    rfc_authority_relevance,
    'Is RFC 9293 still a relevant normative guide for TCP implementations, or has it become largely descriptive of a historical ideal?',
    'Surveys of network engineers and developers on their primary reference for TCP behavior, and analysis of new protocol designs (e.g., QUIC) that attempt to circumvent middlebox interference.',
    'If the RFC''s normative power is negligible, the ''coordination'' aspect of the Tangled Rope diminishes, potentially reclassifying it closer to a Snare. If it retains significant normative pull, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rfc_authority_relevance, conceptual, 'Assessing the current normative authority of the TCP specification in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of endpoint autonomy structural (due to network topology and middlebox deployment) or internalized (due to lack of awareness or perceived futility of resistance)?',
    'Analysis of endpoint and application developer efforts to circumvent middleboxes, and educational campaigns to raise awareness of middlebox impacts. If resistance increases with awareness, internalized suppression was a factor.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them even if structural barriers are theoretically surmountable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in network protocol adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rfc9_tr_t6, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(rfc9_tr_t12, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(rfc9_tr_t18, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(rfc9_tr_t24, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(rfc9_tr_t30, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(rfc9_be_t6, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(rfc9_be_t12, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(rfc9_be_t18, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(rfc9_be_t24, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(rfc9_be_t30, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(rfc9_su_t6, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(rfc9_su_t12, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(rfc9_su_t18, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(rfc9_su_t24, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(rfc9_su_t30, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_end_to_end_principle).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_protocol_adoption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
