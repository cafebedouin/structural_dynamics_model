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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: RFC 9293 TCP Specification Authority Under Middlebox Subordination (Realism Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems
 *
 * SUMMARY:
 *   RFC 9293 describes the TCP protocol as a set of behavioral invariants
 *   that implementations should replicate. In the middlebox_realism_reading,
 *   this specification is treated as an ideal that real networks violate
 *   systematically and with impunity. Deployed middleboxes (firewalls, NATs,
 *   DPI systems, TCP accelerators) modify TCP packets in transit, breaking
 *   the end-to-end principle. Endpoints cannot guarantee their packets arrive
 *   unmodified; the specification becomes an aspirational document rather
 *   than an enforceable standard. The constraint being modeled here is not
 *   'what RFC 9293 says' but 'the actual structure of TCP control authority
 *   in the real network,' where specification authority is subordinate to
 *   what the middlebox-deployed network actually does. This reading
 *   instantiates middlebox deployment as a tangled-rope constraint:
 *   middleboxes solve genuine coordination problems (IPv4 scarcity, DDoS
 *   filtering, policy enforcement) while extracting control authority from
 *   endpoints and making protocol behavior path-dependent and unpredictable.
 *
 * KEY AGENTS:
 *   - Middlebox operators (ISPs, enterprise admins, state surveillance): set the agenda through packet modification; extract control authority from endpoints.
 *   - Endpoint autonomy (non-agent structural property): the capacity of endpoints to control their own protocol behavior; violated by middlebox modification.
 *   - Client and server applications: benefit from connectivity maintained by middleboxes but pay the cost of unpredictable, path-dependent TCP behavior.
 *   - RFC 9293 specification authority (IETF): observes the divergence between specification and reality but has minimal enforcement power over deployed middleboxes.
 *   - Protocol implementers (OS vendors, libraries): must code defensively around middlebox behavior, layering workarounds into production TCP stacks.
 *   - Academic research community (excluded): documents middlebox prevalence and path-dependence but cannot compel compliance or transparency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.81).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.77).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification Authority Under Middlebox Subordination (Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '32b2ac13-b1ed-48f1-ad12-cf514a945fda').
narrative_ontology:cs_kernel_codification('32b2ac13-b1ed-48f1-ad12-cf514a945fda', fixed_text).
narrative_ontology:cs_authority_grounding('32b2ac13-b1ed-48f1-ad12-cf514a945fda', extraction).
narrative_ontology:cs_interpretation_layer_present('32b2ac13-b1ed-48f1-ad12-cf514a945fda').
narrative_ontology:cs_reading_relation('32b2ac13-b1ed-48f1-ad12-cf514a945fda', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('32b2ac13-b1ed-48f1-ad12-cf514a945fda', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('32b2ac13-b1ed-48f1-ad12-cf514a945fda', foundational, specification_authority_subordinate_to_deployed_infrastructure).
narrative_ontology:cs_axiom_status(specification_authority_subordinate_to_deployed_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('32b2ac13-b1ed-48f1-ad12-cf514a945fda', specification_authority_subordinate_to_deployed_infrastructure, empirically_contingent).
narrative_ontology:cs_axiom('32b2ac13-b1ed-48f1-ad12-cf514a945fda', foundational, end_to_end_principle_violated_by_deployed_middleboxes).
narrative_ontology:cs_axiom_status(end_to_end_principle_violated_by_deployed_middleboxes, holdable).
narrative_ontology:cs_axiom_grounding('32b2ac13-b1ed-48f1-ad12-cf514a945fda', end_to_end_principle_violated_by_deployed_middleboxes, empirically_contingent).
narrative_ontology:cs_reference_frame('32b2ac13-b1ed-48f1-ad12-cf514a945fda', endpoint_control_transparency).
narrative_ontology:cs_drift_state('32b2ac13-b1ed-48f1-ad12-cf514a945fda', contemporary_ubiquitous_middlebox_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32b2ac13-b1ed-48f1-ad12-cf514a945fda', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_network_administrators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_apparatus).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_autonomy).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_end_to_end_principle).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, cryptographic_integrity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, client_application).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, server_application).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, client_application).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, server_application).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy and configure middleboxes (firewalls, proxies, DPI systems, WAN optimizers) that examine, modify, and drop TCP packets in transit. Justify their behavior as security (malware filtering), performance optimization (TCP acceleration), or policy enforcement (content control, traffic shaping). Middleboxes operate at the network infrastructure level and control what packets reach endpoints; RFC 9293 compliance is negotiable when the middlebox's operational goals demand packet modification. They extract control authority over connection state from the endpoints that initiated the connection.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, mobile, global).

% The structural capacity of endpoints (clients, servers) to control their own TCP implementations and behavior. When middleboxes modify packets, endpoint autonomy is violated: the endpoint cannot guarantee its packets arrive as sent, cannot fully observe the state of its connection, and cannot implement protocol features if middleboxes strip or rewrite them. This is a non-agent entity representing a structural property of the system.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_autonomy, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_autonomy).

% The architectural principle that protocol intelligence and state should reside at the endpoints, with the network layer providing only dumb forwarding. Middleboxes violate this principle by inserting intelligence into the network path itself, creating a multi-hop protocol state that no single endpoint controls. When middleboxes modify TCP state, the end-to-end principle is degraded into a fiction: the network is no longer transparent to the application.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_end_to_end_principle, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(rfc9293_tcp_specification__middlebox_realism_reading, protocol_end_to_end_principle).

% The structural guarantee that signed/authenticated data cannot be altered in transit without detection. When middleboxes modify TCP headers (sequence numbers, flags, payload hints), they corrupt the authenticatable channel. If TLS encrypts the payload but TCP headers remain modifiable, the middlebox can disrupt the encrypted stream by altering header state. Cryptographic integrity is a structural victim of middlebox intervention.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, cryptographic_integrity, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(rfc9293_tcp_specification__middlebox_realism_reading, cryptographic_integrity).

% Gains connectivity: packets usually reach the destination, content is usually delivered. Bears the cost of unpredictable connection behavior: middleboxes may drop connections silently, reorder or duplicate packets, break keep-alives, strip TCP options, or terminate long-lived connections arbitrarily. The client experiences TCP as a best-effort service with hidden state managed by invisible infrastructure, not as a predictable protocol it can reason about.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, client_application, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, client_application, payer).

% Receives incoming connections (a benefit from middlebox-enforced filtering that blocks some attacks). Bears the cost of asymmetric visibility: the server sees the middlebox's rewritten packets, not the original client's state. Long-lived connections fail mysteriously when middleboxes timeout or reset connections. TCP options negotiated during the handshake may be silently stripped downstream, leaving the server to discover the asymmetry through timeout and retry.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, server_application, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, server_application, payer).

% Deploy and enforce middlebox policies on corporate networks: DPI firewalls, proxy servers, bandwidth shapers, and exfiltration prevention systems. They modify TCP to enforce corporate policy (block certain protocols, rate-limit categories of traffic, inspect and log all connections). They have direct control over the middleboxes on their network and use TCP modification as a primary enforcement mechanism. RFC 9293 is subordinate to corporate policy objectives.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_network_administrators, agenda_setter,
    powerful, biographical, mobile, national).

% Operates middleboxes (often deployed with ISP cooperation) that intercept TCP connections for lawful-intercept, content filtering, and traffic analysis. State surveillance modifies TCP to enable selective packet capture, connection injection, and forced protocol downgrade (e.g., stripping opportunistic encryption, forcing fallback to unencrypted variants). The surveillance apparatus has no obligation to RFC 9293 and uses TCP state modification as a tool of state power. They benefit from middlebox deployment because it enables surveillance capabilities impossible at the endpoint layer.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Deploy middleboxes throughout their network infrastructure: CGN (Carrier-Grade NAT) for IPv4 exhaustion management, TCP accelerators for performance marketing, DPI systems for traffic classification and monetization. ISPs modify TCP packets to implement business models (traffic shaping favoring their own content, zero-rating schemes that treat certain protocols specially) and to manage scarce resources (NAT, QoS). RFC 9293 compliance is negotiable when it conflicts with business objectives or infrastructure constraints.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_service_providers, agenda_setter,
    institutional, generational, mobile, national).

% IETF and TCP specification maintainers who author RFC 9293 as the definitive description of correct TCP behavior. They observe the gap between the specification and actual network behavior. The specification authority has influence over protocol evolution and can recommend changes to TCP (e.g., TCP Fast Open, ECN negotiation) but has minimal power to enforce compliance with existing specifications once middleboxes are deployed. The specification authority's role has become advisory rather than prescriptive.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_specification_authority, observer,
    institutional, generational, analytical, global).

% Implement TCP stacks in operating systems, libraries, and embedded devices. They benefit from a stable specification that describes what they should implement. They pay the cost of specification drift: implementers must code defensively around middlebox behavior (detect and work around connection resets, sequence number anomalies, option stripping), layering workarounds into production code. Implementations become more complex as they try to cope with a de facto protocol that diverges from the written specification.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers, payer).

% Would advocate for measurement and standardization of middlebox behavior, for protocol transparency, and for designs that route around middleboxes. Their research on middlebox prevalence and TCP path-dependence has documented the specification-reality gap. But the research community is excluded from the actual enforcement of TCP behavior: they cannot compel middlebox operators to comply with specifications or to report what they are doing. Their ability to influence the constraint is limited to publishing findings.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, academic_research_community, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides default packet handling for the global Internet: firewalls filter dangerous packets, NAT middleboxes extend IPv4 address space, TCP accelerators reduce latency, DPI systems enforce network policy. These are genuine coordination problems (scarce resources, security threats, policy enforcement) that require infrastructure-level intervention. Without middleboxes, many networks would be unmanageable; with them, the network remains operational despite constraints middleboxes solve.
% TRANSFER_FUNCTION: Moves packet inspection and modification authority from the endpoint (the entity that opened the TCP connection) to the middlebox operator (ISP, enterprise, state). Endpoints lose the ability to guarantee their packets arrive unmodified; middleboxes gain visibility into and control over all connection state. The transfer is enforced by physical network topology: middleboxes sit on the path and can drop, modify, or inject packets. Endpoints have no mechanism to prevent modification; the transfer is unilateral.
% ABSENT_VOICES: End-to-end protocol advocates and cryptographic integrity defenders would object to specification subordination but are structurally excluded from the middlebox deployment decision: when an ISP deploys a DPI box, the endpoint users have no veto. Application developers discover middlebox effects only after deployment, when connections break. Users cannot see what middleboxes are doing and cannot consent to packet modification.
% DISAPPEARANCE_RATIONALE: If all middleboxes vanished and RFC 9293 compliance became globally enforced overnight, the immediate chaos would be: IPv4 address space would become scarce again (no CGN), some DDoS attacks would reach targets unfiltered, unencrypted traffic would be inspectable only at endpoints, enterprise networks would lose enforcement mechanisms, state surveillance would lose passive interception. Within months, network operators would deploy new middleboxes because the coordination problems they solve (resource scarcity, security, policy) remain real. The constraint would re-emerge in a different form, but the disappearance would force an explicit redesign of how those problems are solved — the current design is path-dependent on middlebox ubiquity.
% FOUNDING_PROBLEM: Early TCP assumed transparent networks where the path between endpoints was stateless. IPv4 address exhaustion, DDoS attacks, and the impossibility of policy enforcement at the endpoint layer created intractable coordination problems that middleboxes solved pragmatically. Middleboxes were deployed as infrastructure-level shortcuts to problems that the protocol specification could not address without major redesign.
% FOUNDING_PROBLEM_CORROBORATION: Middlebox operators and network engineers universally attest that the founding problems remain: IPv6 adoption is incomplete, DDoS threats are persistent, and enterprise policy enforcement is essential. The IETF acknowledges middlebox prevalence in RFC 3234 and RFC 7498 (architectural constraints around middleboxes). Internet measurement studies (Bauer et al., Medina et al.) document that >70% of deployed TCP paths include at least one middlebox. The specification authority (IETF) has not disputed that the problems are live; it disputes whether middlebox solutions should be formalized or worked around.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81 at interval end, growing from 0.15 in 1995) because middleboxes subordinate endpoint control to infrastructure-level policy. The trend reflects middlebox ubiquity: from optional enterprise security to default ISP infrastructure (CGN, DPI) to pervasive state surveillance. Suppression is equally high (0.77) because the constraint's persistence depends on maintaining middlebox invisibility: if users and applications could see what middleboxes do, they would demand transparency and remediation. Theater ratio is moderate (0.42) because the security/performance justifications are partially real — middleboxes do filter attacks and manage congestion — but an increasing share of middlebox activity is policy enforcement and surveillance not directly tied to the founding coordination problems. The measurement series spans the constraint's maturation: 1995 marks the pre-middlebox era (open networks, transparent paths); 2002 marks NAT and basic firewalling ubiquity; 2010 marks DPI and TCP acceleration deployment; 2015–2024 mark state surveillance and granular policy enforcement maturity. The time grid is shared across all three metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the middlebox operator's seat: the constraint is legitimate and necessary. Endpoints do not pay attention to resource management or policy enforcement; middleboxes solve coordination problems endpoints cannot or will not solve. From the endpoint's seat: the constraint is invisible and arbitrary. Packets vanish, connections reset, protocol features become unreliable. The endpoint cannot see the middlebox or negotiate with it. From the specification authority's seat: the constraint is a failure of the specification to remain relevant. The IETF authored an ideal protocol; the network evolved to violate it systematically. From the research community's seat: the constraint is a tragedy of the commons. Everyone knows middleboxes break the specification, but the coordination problem (who replaces middlebox functionality if middleboxes are removed?) is intractable. The engine computes these divergences from the stakeholder power levels, exit options, and time horizons; the narrative describes the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators and state surveillance apparatus occupy the beneficiary/agenda-setter position (d near 1.0, full target end): they extract authority, set policy, control visibility. Endpoints and endpoint autonomy occupy the victim position (d approaching 1.0): they lose visibility into their own connections and cannot guarantee packet integrity. Protocol implementers occupy a mixed position (d near 0.5): they benefit from middlebox solutions (no longer need to handle all attack types directly) but pay the cost of specification drift (more complex code). Clients and servers are symmetric-ish (d around 0.4–0.6): they depend on middleboxes for connectivity but suffer from unexpected failures. The IETF specification authority has low d relative to the constraint (~0.1): middlebox operators do not depend on IETF consensus and can modify TCP unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'live' because the coordination problems middleboxes solve (IPv4 scarcity, DDoS, policy enforcement) persist and would re-emerge immediately if middleboxes were removed. The disappearance_verdict is 'world_rearranges' because the specification authority and endpoint autonomy are real structural properties that middleboxes violate. There is no mandatrophy: the constraint's founding justification remains valid. However, the theater_ratio trend (rising from 0.05 to 0.42) indicates growing divergence between the security/performance function and the actual enforcement activity: surveillance and granular policy have become larger shares of middlebox behavior relative to the original coordination problems. This is not mandatrophy (the founding problem is still live) but drift toward a secondary extraction function (state authority extraction). The classification remains Tangled Rope: genuine coordination layered with asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_vs_reality_gap_causal,
    'Is the specification-reality gap caused by specification over-specification (authors did not anticipate deployment constraints), by middlebox operators choosing to violate for legitimate reasons (coordination problems the spec didn''t solve), or by state/corporate actors choosing to violate for extraction (surveillance, policy imposition)?',
    'Structured interviews with middlebox operators and state surveillance apparatus members to elicit their justifications; historical analysis of middlebox deployment timelines relative to specification releases; case studies of specific protocol features (TCP options, fast open, ECN) to trace why they failed to deploy despite specification.',
    'If over-specification: the specification is the problem, and relaxing it would reduce extraction. If legitimate-constraint: middleboxes are solving real problems that specification revision could address; the current constraint is necessary until the spec evolves. If extraction-primary: the specification-reality gap is sustained for control and surveillance purposes; compliance could be forced with sufficient political will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_vs_reality_gap_causal, empirical, 'Whether the specification-reality gap is caused by specification inadequacy, legitimate coordination problems, or intentional extraction.').

omega_variable(
    invisibility_as_suppression_mechanism,
    'Is the high suppression value (0.77) maintained primarily through technical invisibility (endpoints cannot see what middleboxes are doing) or through structural powerlessness (endpoints cannot do anything about visible middleboxes)?',
    'Measurement: deploy transparent TCP monitoring to show endpoints exactly what middleboxes are doing; observe whether endpoint resistance increases when visibility is granted without exit alternatives. If resistance rises significantly with visibility alone, suppression is primarily internalized (powerlessness belief); if resistance rises only when exit becomes available, suppression is primarily structural (actual powerlessness).',
    'If internalized: transparent disclosure and user awareness campaigns could reduce measured suppression even without changing middlebox behavior. If structural: transparency is performative; only adding exit options (alternative protocols, circumvention tools, jurisdictional alternatives) would reduce suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(invisibility_as_suppression_mechanism, empirical, 'Whether suppression is maintained through invisibility or structural powerlessness.').

omega_variable(
    specification_authority_erosion,
    'Has RFC 9293''s authority to govern TCP behavior degraded over the 1995–2024 interval, or has the perceived authority remained stable while the constraint''s actual enforcement by non-IETF actors has grown?',
    'Citation analysis of RFC 9293 in middlebox specifications; IETF meeting records of compliance debates; timeline of when major vendors (Microsoft, Apple, Linux kernel developers) stopped attempting full RFC 9293 compliance in favor of ''best effort'' defensive coding.',
    'If authority has eroded: the specification is becoming decorative; new specifications would have even less effect. If actual enforcement has grown by non-IETF actors: the specification was never the enforcement mechanism; authority is institutional/state, not technical. Either outcome supports the reading that specification authority is subordinate to deployed middlebox infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_erosion, empirical, 'Whether RFC 9293''s specification authority has degraded or been superseded by middlebox operator authority.').

omega_variable(
    end_to_end_principle_violation_scope,
    'Does the middlebox population systematically violate the end-to-end principle for all TCP connections, or are there observed path classes (some data centers, some residential networks) where end-to-end is maintained?',
    'Network measurement: sample TCP connections across diverse paths (ISP home network, data center, enterprise LAN, mobile carrier, state-controlled transit); measure packet loss, modification, and option stripping rates by path class. Construct a path classification (end-to-end-preserving vs. middlebox-modified).',
    'If systematic across all paths: the constraint''s scope is universal; endpoint developers must assume their packets will be modified. If stratified by path: the constraint affects users/endpoints more than cloud infrastructure; exit options are better for institutional actors than for individual users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(end_to_end_principle_violation_scope, empirical, 'Whether middlebox-induced protocol violation is universal or stratified by path class.').

omega_variable(
    surveillance_apparatus_distinguishability,
    'Are state surveillance middleboxes technically and operationally distinguishable from enterprise/ISP middleboxes that are optimizing for coordination problems, or are they operationally identical and the distinction is only institutional?',
    'Technical analysis: examine the feature set (does the box do content inspection? can it inject packets? does it maintain long-term state per flow?) and compare across operators. Interview operators about their policy objectives. Conduct forensic analysis of observed packet modifications to reverse-engineer middlebox policy.',
    'If technically distinguishable: different regulation/enforcement could target surveillance-specific behaviors while preserving coordination-necessary behaviors. If operationally identical: any enforcement that permits middlebox operation automatically enables surveillance; the constraint cannot be split into ''good'' (coordination) and ''bad'' (extraction) versions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surveillance_apparatus_distinguishability, empirical, 'Whether state surveillance middleboxes are operationally distinct from coordination-solving middleboxes.').

omega_variable(
    specification_alternative_framings,
    'Does RFC 9293 best instantiate as a normative specification (describing what implementations should do), a descriptive reference (documenting what works in deployed networks), or a foundational invariant (defining the state machine all TCP must respect)?',
    'Textual analysis of RFC 9293 preface and normative language (RFC 2119 keywords); historical analysis of the IETF''s stated intent in the RFC 9293 standards-track document; comparison with how implementers cite RFC 9293 (as a binding standard vs. as a reference for understanding).',
    'If normative: middlebox violations are specification non-compliance, and enforcement mechanisms (standards action, certification) should hold operators accountable. If descriptive: the specification documents what is, not what should be; middlebox modifications are simply described as part of the real-world protocol, and no enforcement is expected. If foundational-invariant: any middlebox modification breaks an invariant; the specification cannot be reinterpreted; compliance is binary. The reading that wins reshapes the authority structure''s legitimacy claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_alternative_framings, conceptual, 'Alternative framings of RFC 9293''s epistemic function (normative, descriptive, or foundational-invariant).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1995, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t1995, observed).
narrative_ontology:measurement(rfc9_tr_t2002, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement_basis(rfc9_tr_t2002, observed).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(rfc9_tr_t2010, observed).
narrative_ontology:measurement(rfc9_tr_t2015, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(rfc9_tr_t2015, observed).
narrative_ontology:measurement(rfc9_tr_t2020, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(rfc9_tr_t2020, observed).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(rfc9_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1995, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement_basis(rfc9_be_t1995, observed).
narrative_ontology:measurement(rfc9_be_t2002, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2002, 0.35).
narrative_ontology:measurement_basis(rfc9_be_t2002, observed).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(rfc9_be_t2010, observed).
narrative_ontology:measurement(rfc9_be_t2015, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement_basis(rfc9_be_t2015, observed).
narrative_ontology:measurement(rfc9_be_t2020, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement_basis(rfc9_be_t2020, observed).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(rfc9_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1995, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement_basis(rfc9_su_t1995, observed).
narrative_ontology:measurement(rfc9_su_t2002, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2002, 0.4).
narrative_ontology:measurement_basis(rfc9_su_t2002, observed).
narrative_ontology:measurement(rfc9_su_t2010, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement_basis(rfc9_su_t2010, observed).
narrative_ontology:measurement(rfc9_su_t2015, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(rfc9_su_t2015, observed).
narrative_ontology:measurement(rfc9_su_t2020, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement_basis(rfc9_su_t2020, observed).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2024, 0.77).
narrative_ontology:measurement_basis(rfc9_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.12).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% The TCP specification kernel (rfc9293_tcp_specification) decomposes into three reading constraints reflecting institutional positions: (1) strict_invariance_reading: the IETF normative position (RFC 9293 specifies an invariant); (2) optimization_latitude_reading: the implementer position (RFC 9293 permits latitude within semantic bounds); (3) middlebox_realism_reading (this one): the empiricist position (RFC 9293 is an ideal violated systematically by deployed middleboxes). Each reading has its own ε, stakeholder structure, and classification. They coexist as live positions across different institutional factions. This reading (middlebox_realism) influences both siblings by creating structural pressure: if deployed reality diverges from specification, then both strict invariance and optimization latitude become contextual rather than absolute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
