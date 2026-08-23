% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Specification vs. Middlebox-Enforced Reality
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   RFC 9293 specifies TCP as an invariant state machine for reliable
 *   byte-stream delivery. In practice, the deployed middlebox population
 *   (NATs, firewalls, DPI, traffic shapers, censors) modifies packets in
 *   flight — terminating connections, rewriting headers, blocking unknown
 *   options, enforcing policy. The specification has become aspirational:
 *   implementations must conform to middlebox tolerance, not the RFC.
 *   Middlebox operators (ISPs, enterprises, states) benefit from the
 *   extracted control — surveillance, policy enforcement, traffic shaping.
 *   Endpoints (applications, users, protocol designers) bear the cost —
 *   broken assumptions, privacy loss, ossification. The constraint is a
 *   Tangled Rope: genuine coordination (TCP works for most traffic) coexists
 *   with asymmetric extraction (middleboxes extract autonomy). Active
 *   enforcement is the middlebox population itself — it persists because no
 *   single party can unilaterally remove it, and beneficiaries actively
 *   maintain it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.72).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.78).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Specification vs. Middlebox-Enforced Reality").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '470b4721-0395-4a12-849d-886fe38d44cd').
narrative_ontology:cs_kernel_codification('470b4721-0395-4a12-849d-886fe38d44cd', formalized).
narrative_ontology:cs_authority_grounding('470b4721-0395-4a12-849d-886fe38d44cd', extraction).
narrative_ontology:cs_interpretation_layer_present('470b4721-0395-4a12-849d-886fe38d44cd').
narrative_ontology:cs_reading_relation('470b4721-0395-4a12-849d-886fe38d44cd', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('470b4721-0395-4a12-849d-886fe38d44cd', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('470b4721-0395-4a12-849d-886fe38d44cd', foundational, specification_authority_subordinate_to_deployed_behavior).
narrative_ontology:cs_axiom_status(specification_authority_subordinate_to_deployed_behavior, holdable).
narrative_ontology:cs_axiom_grounding('470b4721-0395-4a12-849d-886fe38d44cd', specification_authority_subordinate_to_deployed_behavior, empirically_contingent).
narrative_ontology:cs_axiom('470b4721-0395-4a12-849d-886fe38d44cd', foundational, middlebox_ossification_extracts_endpoint_autonomy).
narrative_ontology:cs_axiom_status(middlebox_ossification_extracts_endpoint_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('470b4721-0395-4a12-849d-886fe38d44cd', middlebox_ossification_extracts_endpoint_autonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('470b4721-0395-4a12-849d-886fe38d44cd', rfc9293_formal_specification).
narrative_ontology:cs_drift_state('470b4721-0395-4a12-849d-886fe38d44cd', middlebox_ossification_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('470b4721-0395-4a12-849d-886fe38d44cd', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, postel_robustness_principle).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_principle_as_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy and operate middleboxes (NATs, firewalls, DPI, traffic shapers) across global internet infrastructure. They set de facto protocol behavior by modifying packets in flight. They benefit from surveillance capability, traffic management control, and policy enforcement leverage. Their exit options are strong — they control the infrastructure layer.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_middlebox_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_middlebox_operators, beneficiary).

% Operate middleboxes for corporate policy enforcement (DLP, compliance monitoring, access control). They benefit from the ability to inspect and modify employee traffic. Their exit is constrained by regulatory requirements and organizational inertia — they cannot easily abandon middlebox deployments.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_middlebox_operators, beneficiary,
    organized, biographical, constrained, regional).

% Operate middleboxes for national security surveillance, censorship, and traffic analysis. They benefit from the protocol's susceptibility to inspection and modification. Their exit options are maximal — they mandate middlebox deployment by law.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Build applications assuming TCP semantics from RFC 9293. Middlebox interference breaks assumptions (connection termination, packet modification, protocol ossification), forcing workarounds (TLS, QUIC, keepalives). They bear development and maintenance costs. Exit is constrained — they must reach users through middlebox-laden paths.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Experience privacy loss, reduced protocol features, and connection failures due to middlebox interference. They have no meaningful exit — they cannot choose ISP middlebox policies, and encrypted alternatives (QUIC) are often blocked. They bear the cost of extracted autonomy without consent.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Design and standardize transport protocols (IETF TCPM, QUIC working groups). Their specifications become aspirational when middleboxes ossify the wire image. They bear the cost of designing around middlebox intolerance. Exit is constrained — they must achieve deployability through the existing middlebox population.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers, observer).

% Develop encrypted transports (QUIC, TLS 1.3, MASQUE) to restore endpoint autonomy. They would object to middlebox interference as a design principle but are structurally excluded from the de facto standardization process — middlebox vendors and operators do not participate in good faith.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, encryption_protocol_developers, excluded,
    moderate, biographical, constrained, global).

% Measure and analyze the gap between RFC specifications and deployed behavior. They document middlebox prevalence, ossification patterns, and protocol evolution barriers. They neither collect nor pay — they observe the structural dynamics.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TCP provides a reliable, ordered byte stream delivery service across heterogeneous, unreliable networks — solving the genuine coordination problem of universal interoperable transport without requiring per-path negotiation.
% TRANSFER_FUNCTION: Middleboxes extract endpoint autonomy (control over packet handling, privacy of payload, freedom to evolve protocols) and transfer it to middlebox operators as surveillance capability, policy enforcement leverage, and traffic shaping authority.
% ABSENT_VOICES: Encryption advocates, alternative transport protocol designers, privacy advocates, and users in censored regimes — they would object to middlebox interference but are structurally excluded from the de facto standardization process where middlebox vendors and operators determine wire-image tolerance.
% DISAPPEARANCE_RATIONALE: If the middlebox population vanished overnight, endpoints would regain full control over packet handling, encryption would work end-to-end without interception, new transport protocols could deploy without ossification barriers, and the Internet would reorganize around the end-to-end principle — the de facto constraint is the middlebox population itself.
% FOUNDING_PROBLEM: Reliable transport across unreliable, heterogeneous networks with diverse middlebox behaviors — the original problem was achieving interoperability; middleboxes emerged as a solution to network management but became a constraint on protocol evolution.
% FOUNDING_PROBLEM_CORROBORATION: Middlebox vendors and network operators attest the problem is live (need for traffic management, security inspection, IPv4 exhaustion mitigation via NAT). Encryption advocates, QUIC designers, and IETF transport area participants attest the founding problem (reliable transport) is substantially solved and the arrangement persists as ossification — independent measurement studies (e.g., PATHspider, Middlebox Measurement Platform) corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because middleboxes systematically extract endpoint autonomy for beneficiary gain. Suppression (0.78) is higher because the constraint persists through active packet modification — endpoints cannot opt out. Theater ratio (0.48) is moderate: the RFC still governs implementation in middlebox-free paths (data centers, QUIC-over-UDP), but a growing share of the wire image is shaped by middlebox tolerance. Accessibility collapse (0.71) is high — once middleboxes are deployed, alternatives (new TCP options, alternative transports) collapse unless they mimic existing tolerated patterns. Resistance (0.52) is moderate: encryption (TLS 1.3, QUIC) and protocol ossification countermeasures exist but middleboxes adapt (QUIC blocking, SNI inspection).
 *
 * PERSPECTIVAL GAP:
 *   From the ISP middlebox operator seat, the arrangement is coordination — they enable IPv4 longevity (NAT), enforce policy, provide security. From the application developer seat, the same structure is extraction — they build workarounds for broken semantics. From the end user seat, it is a snare — they have no voice and no exit. The engine computes this per-seat divergence from the structural data; the claimed type (tangled_rope) reflects the aggregate structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators (ISPs, enterprises, states) are structural beneficiaries — they collect surveillance/control value (d near 0.0). Application developers, end users, protocol designers are targets — they bear extraction with constrained/trapped exit (d near 1.0). Network researchers are analytical observers (d=0.5). Encryption protocol developers are excluded — their exit is constrained by the same middlebox population they try to circumvent. The derivation chain from beneficiary/victim declarations + exit options produces the correct directional gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable transport) is live but substantially solved by TCP itself. The mandate has outlived its function: middleboxes now persist for extraction (surveillance, control) not coordination. The specification's authority is subordinate to deployed behavior — a classic mandatrophy where the standard becomes a ritual citation while the real constraint is the middlebox population. This reading prevents mislabeling the coordination function (TCP reliability) as pure extraction, while exposing the extraction layered onto it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_structure,
    'This constraint is one reading (middlebox_realism_reading) of contested kernel rfc9293_tcp_specification. Sibling readings: strict_invariance_reading, optimization_latitude_reading. Where is the structural disagreement located?',
    'Comparative analysis of the three constraint stories: each instantiates a different beneficiary/victim structure and extraction profile from the same RFC text. The disagreement is located on whether specification authority is normative (strict), semantic with latitude (optimization), or aspirational subordinate to deployed behavior (this reading).',
    'If strict_invariance_reading is structurally true, middlebox interference is non-compliance (bug). If this reading is structurally true, middlebox interference is the de facto standard (feature). Classification diverges: strict → mountain/rope; this → tangled_rope/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_structure, conceptual, 'Committer-frame ambiguity: which reading of the RFC 9293 kernel instantiates the actual constraint?').

omega_variable(
    surveillance_vs_management_ambiguity,
    'What fraction of observed middlebox behavior is legitimate network management (NAT traversal, congestion signaling) versus surveillance/policy extraction (DPI, censorship, traffic shaping)?',
    'Longitudinal measurement studies classifying middlebox interventions by stated purpose vs. observable effect; regulatory disclosure requirements for middlebox vendors.',
    'If predominantly management, extraction is lower and coordination function stronger (rope-leaning). If predominantly surveillance/policy, extraction is higher and coordination is cover (snare-leaning). Current 0.72 extractiveness assumes substantial surveillance fraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_vs_management_ambiguity, empirical, 'Proportion of middlebox interventions that are extractive vs. coordinative.').

omega_variable(
    encryption_restoration_efficacy,
    'Will encryption (QUIC, TLS 1.3, ECH) actually restore endpoint autonomy, or will middleboxes adapt (blocking, interception, mandated proxies) preserving the extraction?',
    'Deployment tracking: QUIC adoption rates, middlebox QUIC-blocking prevalence, regulatory mandates for lawful access. Natural experiment in jurisdictions with/without encryption restrictions.',
    'If encryption restores autonomy, extractiveness declines and constraint may shift toward rope/scaffold. If middleboxes adapt, extractiveness stabilizes or rises and constraint hardens as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(encryption_restoration_efficacy, empirical, 'Whether cryptographic restoration of endpoint autonomy is structurally viable against adaptive middleboxes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t25, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t30, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t35, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 35, 0.47).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t40, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t25, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t30, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t35, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 35, 0.71).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t40, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t25, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t30, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t35, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 35, 0.77).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t40, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.22).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_deployment_ossification).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, tls13_middlebox_interference).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_censorship_infrastructure).

% DUAL FORMULATION NOTE:
% Kernel rfc9293_tcp_specification decomposes into three readings with distinct ε and stakeholder structures. This reading (middlebox_realism) has ε=0.72 (high extraction from endpoints to middlebox operators). strict_invariance_reading has ε≈0.05 (specification as mountain — negligible extraction). optimization_latitude_reading has ε≈0.25 (moderate extraction from implementation variance). The readings are linked because the strict invariance claim is often cited to delegitimize middlebox behavior, while the optimization latitude claim is cited to justify implementation divergence — both are invoked in the discourse this reading analyzes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
