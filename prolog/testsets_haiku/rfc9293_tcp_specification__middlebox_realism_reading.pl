% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification vs. Middlebox-Shaped Reality
 *   domain: network_protocol_engineering/distributed_systems
 *
 * SUMMARY:
 *   RFC 9293 is the normative specification for TCP behavior, written as an
 *   invariant state machine and published as international standard. But the
 *   deployed internet contains hundreds of millions of middleboxes
 *   (firewalls, NATs, DPI devices, load balancers, state-reset proxies) that
 *   inspect and modify TCP packets for policy enforcement, surveillance,
 *   address translation, and load distribution. These modifications violate
 *   RFC 9293 in measurable ways: they reset connections, shorten timeouts,
 *   drop packets that comply with the specification, and rewrite state that
 *   the specification assumes is endpoint-owned. Endpoint implementations
 *   must choose between strict RFC compliance (which fails on paths with
 *   non-compliant middleboxes) and pragmatic adaptation (which violates the
 *   specification to work on the real network). Application developers invest
 *   in workarounds (application-level keepalives, connection retry, protocol
 *   tunneling) to compensate. This story instantiates the
 *   middlebox_realism_reading: the specification's authority is subordinate
 *   to what the deployed network actually does. Real TCP is path-dependent:
 *   behavior depends on which middleboxes a packet encounters, not on what
 *   RFC 9293 says. The coordination function (single interoperable transport
 *   semantics) is real, but the extraction function (middlebox control over
 *   endpoints) is the dominant structural fact.
 *
 * KEY AGENTS:
 *   - rfc_editor_authority: Maintains the specification as normative; claims authority to define TCP. Power: institutional. Does not control deployed paths.
 *   - endpoint_implementers: Build TCP stacks; face the bind between spec compliance and middlebox reality. Power: powerful but constrained. Exit: must work around middleboxes.
 *   - middlebox_operators: Deploy state-enforcing network devices; extract control by unilateral packet modification. Power: institutional. Authority is de facto (what they do is what actually happens).
 *   - application_developers: Build apps that depend on TCP's spec; must compensate for middlebox interference. Power: moderate. Payer seat.
 *   - end_users: Use TCP through ISP/enterprise networks containing middleboxes; experience blocked connections, surveillance, throttling. Power: powerless. Trapped exit.
 *   - standards_body_researchers: Measure middlebox behavior empirically. Role: observer. Authority: analytical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.68).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.71).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification vs. Middlebox-Shaped Reality").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/distributed_systems").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6').
narrative_ontology:cs_kernel_codification('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', fixed_text).
narrative_ontology:cs_authority_grounding('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', extraction).
narrative_ontology:cs_interpretation_layer_present('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6').
narrative_ontology:cs_reading_relation('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', foundational, deployed_middleboxes_define_de_facto_protocol).
narrative_ontology:cs_axiom_status(deployed_middleboxes_define_de_facto_protocol, holdable).
narrative_ontology:cs_axiom_grounding('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', deployed_middleboxes_define_de_facto_protocol, empirically_contingent).
narrative_ontology:cs_axiom('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', foundational, endpoint_autonomy_subordinate_to_network_operator_control).
narrative_ontology:cs_axiom_status(endpoint_autonomy_subordinate_to_network_operator_control, holdable).
narrative_ontology:cs_axiom_grounding('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', endpoint_autonomy_subordinate_to_network_operator_control, deontological).
narrative_ontology:cs_reference_frame('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', rfc_specification_as_normative_invariant).
narrative_ontology:cs_drift_state('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', contemporary_deployed_middlebox_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65f6fd88-e4c8-42ba-bd9c-5cc3c636d8b6', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_users_privacy_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).

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
 *   Extractiveness is 0.68 (high) because middleboxes unilaterally impose control on endpoints without negotiation or compensation. Endpoints pay by losing autonomy and bearing reliability costs. Suppression is 0.71 because the control is enforced physically (packets are modified or dropped) and endpoints cannot refuse without losing network connectivity. Theater is 0.42 (moderate) because middlebox policy enforcement is real (not purely performative) but a significant fraction of their activity is architectural theater — replaying RFC assertions while actually running a different protocol. Accessibility_collapse is 0.58 (moderate) because endpoints have constrained alternatives (QUIC and other protocols inherit the same middlebox problem; IPv6 is not yet dominant; switching networks does not avoid middleboxes) but encryption and tunneling provide some workaround capability. Resistance is 0.64 because endpoint implementers, application developers, and standards bodies actively push back (IETF standards for middlebox-aware TCP, TAPS work, encryption adoption), though the large installed base of middleboxes and the power of network operators limit the practical effect. The measurement series shows extractiveness rising from 0.45 to 0.68 over the interval, driven by accumulation of middlebox deployment and sophisticated policy enforcement (post-2010 DPI expansion, post-2015 encrypted traffic blocking). Theater_ratio rises from 0.18 to 0.42, indicating an increasing share of middlebox activity is devoted to defending the specification-vs-reality gap rather than performing the founding functions (NAT, basic firewall). Suppression_requirement rises from 0.48 to 0.71, indicating escalating effort required to suppress alternative routing (QUIC adoption, encrypted DNS, VPN use) that would bypass middleboxes.
 *
 * PERSPECTIVAL GAP:
 *   From the RFC editor seat: the specification is normative, deviations are implementation defects, and the authority structure (the IETF process) is legitimate because it is open and consensus-driven. From the middlebox operator seat: the specification is advisory, deployed network reality is law, and their authority to modify packets derives from control of the physical path. From the endpoint implementer seat: the specification is the goal but middlebox compatibility is the constraint — they must optimize both and the gap is painful. From the end-user seat: the specification is invisible; they experience blocked connections and surveillance, with no way to negotiate with the network. The engine computes these per-seat divergences from the structural data: rfc_editor is analytical (external authority), middlebox_operators and endpoint_implementers are both institutional but with opposite directionalities (beneficiary vs. payer), end_users are powerless and trapped. The computed type divergence — rope from the RFC seat, snare from the middlebox seat, tangled_rope from the implementer seat — is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox_operators are beneficiaries (d near 0.0): they extract control, have arbitrage-level exit options (they can deploy alternative blocking mechanisms or cooperate with other network operators), and operate at institutional power. Endpoint_implementers are near full targets (d near 1.0): they pay by losing autonomy, are constrained by the need to interoperate, and bear the cost of implementing workarounds. The RFC editor authority is a beneficiary in a narrow sense (their authority is vindicated by the specification's acceptance) but analytically positioned (they do not collect rents). End_users are targets (d near 1.0): they are trapped, powerless, and experience the constraint as blocking and surveillance with no negotiation path. Application_developers are payers (d moderate-high): they pay by implementing workarounds but retain moderate power (they can adopt encryption, use QUIC, pressure vendors) and have some arbiting mobility (they can choose which applications to build). The directionality logic maps to effective extraction: the constraint extracts heavily from powerless end-users (d=1.0 → high χ), significantly from endpoint implementers (d=0.85 → high χ), moderately from app developers (d=0.65 → moderate χ), and captures the gains at middlebox operators and state surveillance agencies (d=0.0 → no χ, they collect).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (address scarcity and firewall state management) is dead: IPv6 solves address scarcity, endpoint firewalls are mature, TLS is ubiquitous. Middleboxes persist because they provide benefits unrelated to the founding problem: ISPs use them for subscriber policy enforcement and revenue collection (traffic shaping, service-level differentiation), enterprises use them for access control and monitoring, state actors use them for surveillance. The constraint is mandatroph: the arrangement persists despite the founding problem being solved because the institutions that deployed middleboxes benefit from their continued operation and have the power to defend them. The specification (RFC 9293) has become theater: endpoints are told to implement it, but middleboxes silently enforce a different protocol, and the entire ecosystem has adapted to expect deviations. The classification as tangled_rope (rather than snare) rests on the genuine coordination function the specification provides (single interoperable semantics) plus the active enforcement required to suppress alternatives (blocking QUIC, encrypted DNS, VPNs, alternative routing). If the middleboxes were purely extractive with no coordination function, this would be snare. The presence of both functions makes it tangled_rope, though the balance has shifted significantly toward extraction over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_vs_middlebox_authority_contest,
    'Whose authority is primary: RFC 9293 as written standard, or deployed middlebox population as de facto protocol?',
    'This omega documents the fundamental kernel contest. The three sibling readings resolve it differently. Strict_invariance_reading: RFC authority is primary, middleboxes are defects. Optimization_latitude_reading: both are legitimate within semantic bounds. Middlebox_realism_reading (this story): middleboxes are primary, RFC is aspiration. The resolution is not empirical — it is a reading choice.',
    'The reading chosen determines the constraint''s classification, the beneficiary/victim assignment, and the type divergence across seats. This is the kernel-reading framing documented in cs_structure.reading_relations and cs_structure.axioms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_vs_middlebox_authority_contest, conceptual, 'RFC 9293 kernel reading contest: authority locus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(rfc9_tr_t25, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(rfc9_tr_t30, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(rfc9_tr_t35, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(rfc9_be_t25, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(rfc9_be_t30, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(rfc9_be_t35, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(rfc9_su_t25, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(rfc9_su_t30, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(rfc9_su_t35, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.05).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_middlebox_deployment).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, tls_encryption_adoption_pressure).

% DUAL FORMULATION NOTE:
% RFC 9293 TCP specification is a kernel with three structurally distinct readings. This story instantiates middlebox_realism_reading: the specification's authority is subordinate to deployed middlebox reality. The strict_invariance_reading treats RFC 9293 as an invariant protocol law (mountain). The optimization_latitude_reading treats RFC 9293 as defining outcomes with implementation freedom (rope). Each reading is a separate constraint with its own ε, classification, and beneficiary/victim structure. The readings coexist as live positions held by different institutional actors (RFC editors, network operators, endpoint vendors). Link all three via network.affects_constraints to document the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
