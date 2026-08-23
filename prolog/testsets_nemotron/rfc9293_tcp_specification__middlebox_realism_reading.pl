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
 *   human_readable: TCP Specification Subordination to Middlebox Population
 *   domain: network_protocol_engineering/internet_standards/distributed_systems
 *
 * SUMMARY:
 *   RFC 9293 (TCP specification) nominally defines the authoritative behavior
 *   of TCP endpoints. In practice, the deployed population of middleboxes —
 *   NATs, firewalls, traffic shapers, DPI boxes, censors — modifies TCP
 *   packets in ways that violate the specification. Endpoints must adapt to
 *   these modifications to maintain connectivity. The specification has
 *   evolved to document and accommodate middlebox behavior rather than
 *   proscribe it. This constraint captures the reading that the
 *   specification's authority is subordinate to the middlebox population: the
 *   real protocol is what the network does, not what the RFC says. Middlebox
 *   operators (ISPs, enterprises, states, vendors) benefit from this
 *   arrangement; endpoint implementers, application developers, and end-users
 *   bear the costs.
 *
 * KEY AGENTS:
 *   - isp_middlebox_operators: Primary beneficiary (institutional/arbitrage) — deploys and profits from middlebox population
 *   - enterprise_network_operators: Beneficiary (organized/mobile) — uses middleboxes for policy enforcement
 *   - state_surveillance_agencies: Beneficiary (institutional/arbitrage) — uses middleboxes for interception and censorship
 *   - middlebox_vendors: Beneficiary (organized/mobile) — sells appliances that depend on modification tolerance
 *   - endpoint_implementers: Primary payer (organized/constrained) — bears ossification workarounds and innovation blockade
 *   - application_developers: Payer (moderate/constrained) — bears unpredictable transport behavior
 *   - end_users: Payer (powerless/trapped) — experiences degraded service and privacy loss with no exit
 *   - protocol_designers: Payer/observer (organized/constrained) — designs around ossification
 *   - ietf_standards_community: Agenda setter (institutional/analytical) — nominal authority, actual subordination
 *   - middlebox_researchers: Observer (moderate/analytical) — documents the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.68).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.72).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Specification Subordination to Middlebox Population").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '5e4c540e-9f8c-4dc3-9a4e-fac3619796b5').
narrative_ontology:cs_kernel_codification('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', formalized).
narrative_ontology:cs_authority_grounding('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', practice).
narrative_ontology:cs_interpretation_layer_present('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5').
narrative_ontology:cs_reading_relation('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', foundational, specification_authority_subordinate_to_deployment).
narrative_ontology:cs_axiom_status(specification_authority_subordinate_to_deployment, holdable).
narrative_ontology:cs_axiom_grounding('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', specification_authority_subordinate_to_deployment, empirically_contingent).
narrative_ontology:cs_axiom('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', foundational, middlebox_operators_extract_transport_control).
narrative_ontology:cs_axiom_status(middlebox_operators_extract_transport_control, holdable).
narrative_ontology:cs_axiom_grounding('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', middlebox_operators_extract_transport_control, empirically_contingent).
narrative_ontology:cs_reference_frame('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', rfc793_original_specification_authority).
narrative_ontology:cs_drift_state('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', post_quic_deployment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5e4c540e-9f8c-4dc3-9a4e-fac3619796b5', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendors).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, path_dependency_dominates_specification).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, network_evolution_is_ossification).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, interoperability_requires_middlebox_compatibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy transparent proxies, traffic shapers, and NAT devices that modify TCP headers and payloads for traffic management, caching, and policy enforcement. They benefit from the inability of endpoints to detect or resist modifications, and from the specification's deference to deployed behavior. Their business models depend on inspecting and controlling traffic flows.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_middlebox_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Deploy firewalls, DPI boxes, and SSL inspection middleboxes that terminate and modify TCP connections for security policy, data loss prevention, and regulatory compliance. They benefit from specification tolerance for middlebox behavior and from endpoints adapting to their modifications rather than resisting.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_network_operators, beneficiary,
    organized, biographical, mobile, continental).

% Deploy middleboxes for traffic interception, censorship, and metadata collection. They benefit from TCP's lack of encryption at the transport layer and from the ossification that prevents deployment of encrypted transports that would bypass their inspection.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Sell appliances that modify TCP behavior for 'optimization,' 'security,' and 'visibility.' Their market depends on the protocol's tolerance for in-path modification and on endpoints working around rather than rejecting their modifications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendors, beneficiary,
    organized, biographical, mobile, global).

% Build TCP stacks (OS kernels, user-space implementations) that must work around deployed middlebox behaviors — ossification workarounds, heuristic fallbacks, version negotiation failures. They bear the cost of complexity, reduced innovation, and the inability to deploy new TCP features because middleboxes drop or corrupt unknown options.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers, payer,
    organized, biographical, constrained, global).

% Build applications on top of TCP that experience unpredictable behavior due to middlebox interference — connection resets, throughput degradation, latency spikes, header stripping. They bear the cost of implementing application-layer workarounds (QUIC, application-level framing) and cannot rely on TCP semantics.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Experience degraded performance, privacy violations, and service failures caused by middlebox interference. They have no viable exit — changing ISPs or networks rarely eliminates middleboxes, and they cannot modify the transport protocol.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Design transport protocols (IETF TCPM, QUIC working groups) that must account for middlebox ossification from day one. They bear the cost of designing around deployed middleboxes rather than designing for clean semantics, and the cost of failed deployments when middleboxes block new protocols.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, protocol_designers, observer).

% Produces RFC 9293 and related standards. Nominally sets the specification but in practice documents deployed behavior rather than prescribing it. The specification authority is subordinate to the middlebox population — changes that break middlebox compatibility are not adopted.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standards_community, agenda_setter,
    institutional, generational, analytical, global).

% Measure and document middlebox behaviors (ICSI Netalyzr, Tracy, pathspider). They observe the constraint's operation but do not set its agenda or bear its direct costs.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_researchers, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TCP coordinates reliable byte-stream delivery between endpoints across heterogeneous networks. The specification provides a shared reference for interoperability.
% TRANSFER_FUNCTION: Moves control over transport semantics from endpoints to in-path middlebox operators. Middleboxes extract the ability to inspect, modify, block, and prioritize traffic; endpoints lose the ability to innovate at the transport layer and to rely on protocol semantics.
% ABSENT_VOICES: Future protocol designers and end-users who would benefit from an evolvable transport layer but are not represented in current standardization. Also, operators of networks that choose not to deploy middleboxes but suffer collateral ossification.
% DISAPPEARANCE_RATIONALE: If the subordination of specification to middlebox behavior vanished overnight — i.e., if endpoints could reliably deploy new transport features without middlebox interference — the transport layer would become a platform for innovation again. QUIC deployment would not need UDP encapsulation. TCP extensions (MPTCP, TCP-AO, RACK) would deploy cleanly. The internet's transport architecture would reorganize around endpoint sovereignty.
% FOUNDING_PROBLEM: Early internet needed a reliable transport protocol that worked across heterogeneous networks with minimal coordination. TCP provided this as a best-effort specification that endpoints implemented voluntarily.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's own TCPM working group archives document the shift from 'specification leads' to 'specification follows deployment.' RFC 8903 (The Impact of Middleboxes on TCP) and RFC 9293's own acknowledgment of middlebox-driven changes corroborate that the original coordination problem (interoperability via shared spec) has been superseded by ossification management. No party outside the middlebox beneficiary set argues the founding problem is still live.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.68) is substantial: middlebox operators extract control over transport semantics, inspection capabilities, and policy enforcement leverage. The coordination function (reliable byte stream) is genuine but increasingly compromised — the specification no longer reliably predicts behavior. Suppression (0.72) is high: endpoints cannot exit the middlebox-modified path (trapped users, constrained implementers), and new transport deployments are actively blocked. Theater ratio (0.45) is significant: the specification process maintains the appearance of authority while actually documenting deployed behavior. The measurement series (2000-2024) shows rising extraction, rising theater, and rising suppression as middlebox deployment intensified and encryption (TLS 1.3, QUIC) threatened inspection capabilities.
 *
 * PERSPECTIVAL GAP:
 *   From the IETF/agenda_setter seat, the constraint appears as a coordination mechanism with some unfortunate middlebox interference. From the endpoint_implementer and application_developer seats, it appears as an extraction mechanism that blocks innovation. From the end_user seat, it appears as a snare with no exit. From the middlebox_operator seats, it appears as a necessary coordination tool (traffic management, security). The engine computes per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators (ISPs, enterprises, states, vendors) are structural beneficiaries: they collect control, inspection capability, and revenue from the arrangement. Their exit options are arbitrage/mobile — they can deploy different middleboxes or shift strategies. Endpoint implementers and application developers are payers: they bear complexity costs and innovation blockade. Their exit is constrained — they must work around middleboxes to reach users. End users are trapped payers: no viable exit, bear privacy and performance costs. The IETF is the agenda_setter with analytical exit — it observes but cannot change the structural dynamic. Protocol designers are payers with constrained exit — they must design for ossification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable transport via shared specification) is dead — the specification no longer leads deployment. The arrangement persists because middlebox operators benefit from the ossification that prevents encrypted/evolvable transports. This is mandatrophy: the coordination mandate has atrophied, but the constraint persists through active enforcement (middlebox deployment) that benefits identifiable parties. The tangled_rope classification captures this: genuine coordination function (TCP still mostly works) + asymmetric extraction (middlebox control) + active enforcement (middlebox deployment and endpoint adaptation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (middlebox_realism_reading) of the contested kernel rfc9293_tcp_specification. What does the sibling reading structure imply for classification?',
    'Compare the three readings'' beneficiary/victim structures and extractiveness assessments. The middlebox_realism_reading identifies middlebox operators as beneficiaries and endpoints as victims. The strict_invariance_reading would identify endpoint implementers as beneficiaries (of a stable target) and middlebox operators as violators. The optimization_latitude_reading would identify a more symmetric coordination function. The structural delta between readings is the kernel''s contention surface.',
    'If the kernel has multiple stable readings with divergent extraction profiles, the constraint classification is reading-indexed — a property of the reading, not the kernel label. This validates the committer frame''s Rule 1: each reading instantiates a separate constraint with its own ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Kernel-reading decomposition and its classification consequences').

omega_variable(
    specification_authority_vs_deployment,
    'Is the specification''s subordination to deployed middleboxes a contingent historical outcome or a structural inevitability of decentralized protocol deployment?',
    'Counterfactual analysis: if the IETF had mandated cryptographic protection of TCP headers from the start (like QUIC), would middlebox ossification have been prevented? Or would middleboxes have evolved to block/corrupt encrypted transports anyway (as some already do with QUIC)?',
    'If structural inevitability, the tangled_rope classification reflects a permanent condition — the coordination function (interoperability) is permanently coupled to extraction (middlebox control). If contingent, the classification may change with different design choices (e.g., universal transport encryption).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_vs_deployment, conceptual, 'Contingency vs. inevitability of specification subordination').

omega_variable(
    ossification_as_extraction_mechanism,
    'Is protocol ossification (the inability to deploy new transport features) an extraction mechanism in itself, or a side effect of middlebox extraction?',
    'Analyze whether middlebox vendors and operators actively benefit from ossification (it protects their installed base and inspection capabilities) or merely tolerate it. The QUIC deployment experience — middleboxes blocking QUIC because they cannot inspect it — suggests ossification is actively maintained.',
    'If ossification is an active extraction mechanism, the constraint is more snare-like (coordination story is cover for control). If it''s a side effect, the tangled_rope classification (genuine coordination + asymmetric extraction) is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_as_extraction_mechanism, empirical, 'Whether ossification serves as an active extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2000, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2005, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2010, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2015, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2020, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(rfc9293_middlebox_realism_tr_t2024, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2000, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2005, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2010, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2010, 0.51).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2015, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2020, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(rfc9293_middlebox_realism_be_t2024, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2000, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2005, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2010, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2015, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2020, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(rfc9293_middlebox_realism_su_t2024, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.12).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_deployment_constraint).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, tcp_extension_ossification).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, transport_layer_encryption_adoption).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% This is the middlebox_realism_reading of the rfc9293_tcp_specification kernel. The strict_invariance_reading treats the specification as an invariant coordination target (lower extraction, rope-like). The optimization_latitude_reading treats it as a flexible coordination framework with bounded variance. This reading treats it as a constraint whose authority has been captured by in-path middlebox operators. The three readings have divergent beneficiary/victim structures and extractiveness assessments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, institutional, 0.15).
constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, organized, 0.25).
constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, powerless, 0.95).
constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
