% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: RFC 9293 TCP Specification Under Middlebox Realism
 *   domain: network/protocol/systems
 *
 * SUMMARY:
 *   RFC 9293 specifies ideal TCP endpoint behavior: a state machine for
 *   reliable, in-order, flow-controlled byte-stream delivery. But deployed
 *   TCP is constrained by the middlebox population — firewalls, NATs, DPI
 *   systems, and surveillance infrastructure that transparently modify
 *   packets in flight. The middlebox realism reading asserts that the RFC's
 *   specification authority is subordinate to what the network actually
 *   enforces. Endpoints must adapt to middleboxes, not vice versa. The
 *   specification becomes an aspirational document, not a binding contract.
 *   This reading is one of three: the strict-invariance reading says RFC 9293
 *   describes an unchangeable protocol contract; the optimization-latitude
 *   reading says the RFC permits semantic-preserving implementation
 *   variation. This reading differs from both: it claims the RFC is
 *   overridden by middlebox behavior and that this override extracts control
 *   from endpoints.
 *
 * KEY AGENTS:
 *   - IETF TCP working group — publishes the specification, owns its authority (analytical/advisory role)
 *   - Middlebox operators (ISPs, enterprise administrators, surveillance infrastructure) — enforce de facto behavior, extract control and visibility
 *   - Endpoint application developers — depend on RFC compliance, constrained by middlebox reality
 *   - Endpoint users — experience application behavior determined by invisible packet modification
 *   - End-to-end security advocates — depend on specification stability; undercut by middlebox realism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.82).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.79).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification Under Middlebox Realism").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network/protocol/systems").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '967cea65-25fa-455f-a09d-4676507cb239').
narrative_ontology:cs_kernel_codification('967cea65-25fa-455f-a09d-4676507cb239', fixed_text).
narrative_ontology:cs_authority_grounding('967cea65-25fa-455f-a09d-4676507cb239', extraction).
narrative_ontology:cs_reading_relation('967cea65-25fa-455f-a09d-4676507cb239', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('967cea65-25fa-455f-a09d-4676507cb239', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('967cea65-25fa-455f-a09d-4676507cb239', foundational, specification_authority_subordinate_to_deployed_reality).
narrative_ontology:cs_axiom_status(specification_authority_subordinate_to_deployed_reality, holdable).
narrative_ontology:cs_axiom_grounding('967cea65-25fa-455f-a09d-4676507cb239', specification_authority_subordinate_to_deployed_reality, empirically_contingent).
narrative_ontology:cs_axiom('967cea65-25fa-455f-a09d-4676507cb239', foundational, endpoint_autonomy_unrecoverable_within_framework).
narrative_ontology:cs_axiom_status(endpoint_autonomy_unrecoverable_within_framework, holdable).
narrative_ontology:cs_axiom_grounding('967cea65-25fa-455f-a09d-4676507cb239', endpoint_autonomy_unrecoverable_within_framework, empirically_contingent).
narrative_ontology:cs_reference_frame('967cea65-25fa-455f-a09d-4676507cb239', endpoint_autonomy_and_rfc_compliance).
narrative_ontology:cs_drift_state('967cea65-25fa-455f-a09d-4676507cb239', contemporary_deployed_network, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('967cea65-25fa-455f-a09d-4676507cb239', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, network_administrators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_security_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and updates RFC 9293 (TCP specification), documents ideal protocol behavior, owns the normative standard. Publishes specifications describing endpoint-to-endpoint communication; has no direct enforcement authority over deployed middleboxes. Authority is advisory/aspirational — the working group's stated role is to document best practices, not to police network intermediaries.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_working_group, agenda_setter,
    organized, generational, analytical, global).

% ISPs, enterprise network administrators, state surveillance infrastructure, DPI systems, firewalls, NATs, proxies. Inspect and modify TCP packets in flight to implement policy (rate limiting, DPI-based blocking, traffic shaping, surveillance, censorship). The middlebox population is the de facto enforcer of TCP behavior on the network — what middleboxes permit or require overrides what RFC 9293 specifies. They benefit from control, visibility, and the ability to enforce arbitrary policy over endpoint communications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter).

% Write TCP-using applications, expect RFC 9293 to describe actual network behavior. Encounter middleboxes that violate RFC 9293 (reorder packets, inject RST, modify payload, strip options, implement non-standard flow control). Must either work around the violations, accept degraded behavior, or abandon TCP for a constrained protocol. Cannot force the network to comply with the specification they depend on.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_application_developers, payer,
    moderate, biographical, constrained, global).

% Experience TCP application behavior determined by the actual network they are on, not by RFC 9293. Have no visibility into middlebox policy, no recourse for violations, no ability to opt out of the network. Their experience is the intersection of what the application's developer built (assuming RFC 9293 compliance) and what the middlebox population permits.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users, payer,
    powerless, immediate, trapped, global).

% Researchers, standards bodies, cryptographers advocating for end-to-end encryption and endpoint autonomy. Depend on RFC 9293 to describe a stable, verifiable protocol that middleboxes cannot subvert. The middlebox realism reading devalues their position: the protocol specification is not enforceable, security margins are eroded by invisible modification, and surveillance intermediaries have de facto design authority.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_security_advocates, payer,
    moderate, generational, constrained, global).

% Old TCP implementations on deployed systems that genuinely implement RFC 9293. Cannot be updated (embedded, proprietary, discontinued). Encounter middleboxes that violate the specification they were built to match. Their existence and broken interaction serves as evidence that the middlebox realism reading is descriptively true.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, legacy_endpoint_applications, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(rfc9293_tcp_specification__middlebox_realism_reading, legacy_endpoint_applications).

% Standards purists, academic protocol researchers, interoperability testing communities. Would argue that RFC 9293 describes an invariant state machine that MUST be preserved; middlebox modification is a violation that should trigger remediation. This reading explicitly excludes their framing — the middlebox realism reading asserts that enforcement is impossible, so invariance is an aspiration that reality rejects.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, strict_invariance_advocates, excluded,
    moderate, generational, constrained, global).

% Performance engineers, congestion-control researchers, implementers of TCP variants (CUBIC, BBR, etc.). Focus on semantic outcomes (reliable byte stream delivery) and interpret RFC 9293 as permitting implementation latitude on the path. Operate within the specification's bounds but optimize aggressively. This reading coexists with their framing (both accept that reality diverges from pristine specification) but differs on WHO controls the divergence and WHAT the consequences are.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, optimization_latitude_advocates, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: RFC 9293 coordinates endpoint-to-endpoint communication: provides a stable protocol contract (reliable, in-order, flow-controlled byte stream) so applications can rely on consistent TCP behavior across the Internet. The specification enables interoperability by giving all implementers a common reference — if everyone implements RFC 9293, the network is predictable.
% TRANSFER_FUNCTION: Transfers control over what happens to packets in flight from endpoint application developers (who authored the application assuming RFC 9293 compliance) to middlebox operators (who modify the packets, implement policy, surveil traffic, enforce constraints). The middlebox population extracts visibility, control, and policy-enforcement authority; endpoint developers lose autonomy to define what their application actually does on the real network.
% ABSENT_VOICES: Applications written for networks that don't have deployed middleboxes (private networks, research testbeds, historical networks) are not present in this conversation. Their testimony would describe TCP that actually matches RFC 9293. Also absent: the internal perspectives of middlebox operators (security teams, DPI vendors, ISP engineers who would argue their modifications are necessary for security or network management) — their framing is implicit in the beneficiary set but not directly seated.
% DISAPPEARANCE_RATIONALE: If this constraint (the requirement that RFC 9293 authority is subordinate to middlebox behavior, and that endpoint autonomy is extracted by middleboxes) disappeared, the application ecosystem would reorganize: TCP implementations could assume RFC 9293 compliance; end-to-end encryption would work as designed; network behavior would become predictable and testable; surveillance and policy control would require explicit, auditable gateways rather than invisible packet modification. The disappearance is equivalent to eliminating middlebox realism — enforcing RFC 9293 compliance across the network.
% FOUNDING_PROBLEM: Early Internet routers had to forward packets across heterogeneous physical media and link types (Ethernet, X.25, dial-up, satellite). TCP needed to adapt to real network conditions (loss, reordering, latency, MTU variability). Middleboxes emerged to solve operational problems: firewalls for security, NATs for address exhaustion, proxies for performance. The founding problem was real — networks are not ideal channels.
% FOUNDING_PROBLEM_CORROBORATION: Network operators and security teams attest that middleboxes are essential for network management, DPI for copyright enforcement, surveillance infrastructure for security. RFC 9293 editors and endpoint-side researchers attest that the founding problem is substantially solved (modern routing and hardware can handle RFC 9293 compliance without modification), and that middlebox modification solves operator problems at the cost of extracting endpoint autonomy. Standards bodies (IETF TAPS, TCP extensions working groups) acknowledge the founding problem was live but now solved; they debate whether middleboxes' continued modification is still necessary or has become an extraction mechanism. Legislative testimony on surveillance and encryption backdoors (from multiple jurisdictions) confirms that middleboxes are now primarily a control/surveillance tool, not a solutions mechanism.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because middleboxes extract control over what packets actually do on the network, overriding the endpoint's intended behavior. Suppression is correspondingly high (0.79) because endpoints have no contractual remedy, no technical recourse, and increasingly no alternative protocols — blocking is the enforcement mechanism. Theater is moderate (0.41): middleboxes claim their modifications are for 'security' or 'network health,' but the actual function is surveillance and policy control. Accessibility collapse is moderate-high (0.72): once a middlebox is deployed and enforcing policy, the endpoint's option to use standard TCP becomes inaccessible — the endpoint must either adapt to the middlebox's requirements or use a completely different protocol (QUIC, which itself gets blocked). Resistance is moderate (0.68): endpoint developers mount real resistance via encryption, protocol obfuscation, and migration to QUIC, but the middleboxes adapt by simply blocking encrypted protocols. The measurement series tracks the rising extractiveness and suppression as DPI and surveillance-grade middleboxes proliferated over the past 25 years.
 *
 * PERSPECTIVAL GAP:
 *   The IETF working group sees itself as custodians of a specification that enables interoperability — they claim RFC 9293 is descriptively accurate and normatively binding. Middlebox operators see themselves as necessary gatekeepers enforcing policy and security — they claim modifications are operational necessities, not violations. Endpoint developers see a gap between the specification they depend on and the actual network behavior — they report that RFC 9293 is not descriptively accurate. The engine should compute different types from different seats: from the IETF seat, this might appear as rope (coordination enabling interoperability); from the middlebox operator seat, it appears as a coordinating arrangement with built-in control; from the endpoint seat, it is snare (the specification is a false promise, the network extracts from endpoints). The authored claim is tangled-rope because the coordination function (RFC 9293 as a stable interoperability baseline) coexists with the extraction function (middleboxes enforce policy, endpoints lose autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   IETF working group: analytical seat, no extraction/subsidy axis, d near 0.5 (defines the nominal protocol but has no control over network behavior). Middlebox operators: beneficiaries (extract visibility, control, policy authority), d near 1.0 (target state — they want to inspect, modify, block). Endpoint developers: victims (lose autonomy to define behavior), d near 0.9 (target state — the middlebox realism reading subordinates their specification assumptions to network reality). Endpoint users: victims (powerless, trapped on networks they don't control), d near 1.0. End-to-end security advocates: victims (their guarantees depend on RFC 9293 compliance, which is now unenforceable), d near 0.9. The asymmetry is structural: middleboxes are intermediaries with institutional power and control over routing; endpoints are edges with no leverage over the path. The directionality derivation should confirm high d for all victim seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (heterogeneous network media, need for address translation, DPI for security) was real and solved in the 1990s–2000s. But RFC 9293 was also solved — modern networks can route TCP traffic that complies with the specification. The mandate (middleboxes modifying TCP for operational necessity) is now dead: modern routing, hardware, and software can handle standard TCP without modification. Yet the constraint persists because middleboxes have discovered a secondary use case: surveillance and policy control. The theater ratio rising from 0.10 to 0.41 captures this mandatrophy: as the operational justification faded, the rhetorical cover ('network security,' 'copyright protection') intensified. The constraint is now maintained primarily by institutional inertia (middleboxes exist, operators benefit from control, surveillance is profitable) rather than by genuine need. This reading classifies the constraint as tangled-rope because the coordination function is now vestigial, but the extraction function is very live. A pure piton reading would require that no party benefited enough to maintain it — but middlebox operators clearly benefit (they gain visibility and control). The snare reading would require that victims have no role in the coordination function — but endpoint developers do use TCP (even if constrained by middleboxes). The tangled-rope classification holds: coordination (TCP as interoperability baseline) coexists with extraction (middleboxes enforce policy, endpoints lose autonomy); both are actively maintained by the institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_necessity_vs_control,
    'Are middlebox modifications still necessary for network operation, or have they become primarily a control/surveillance mechanism?',
    'Network deployment experiment: disable middlebox packet modification on a representative ISP segment and measure application failure rates, performance degradation, and security incidents over a six-month period.',
    'If failure rates are near zero and performance is stable, middlebox necessity is resolved as false — the constraint is pure extraction (snare) rather than tangled-rope. If significant failures occur, the founding problem is still live and the constraint remains tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_control, empirical, 'Whether middlebox modifications are operationally necessary or primarily extractive.').

omega_variable(
    endpoint_autonomy_recovery_path,
    'Is endpoint autonomy recoverable through protocol encryption (QUIC, etc.), or do middlebox operators maintain control through blocking rather than modification?',
    'Longitudinal measurement of QUIC adoption rates, blocking rates, and encrypted-traffic throttling across major ISPs and regions over the next 5 years.',
    'If encryption enables escape, the constraint is self-limiting (endpoints can migrate away). If blocking becomes the enforcement mechanism, suppression remains high and the constraint is more entrenched (pure snare if middleboxes gain total control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endpoint_autonomy_recovery_path, empirical, 'Whether endpoint autonomy can escape via encryption or whether middleboxes will block encrypted protocols.').

omega_variable(
    specification_authority_under_contestation,
    'Can the IETF or any standards body enforce RFC 9293 compliance across deployed middleboxes, or is the authority structure fundamentally asymmetric (specification writers vs. network operators)?',
    'Policy intervention (regulatory mandate for RFC compliance, antitrust enforcement against blocking non-approved protocols, international standards alignment) or research (measurement of actual protocol compliance rates by middlebox type and region).',
    'If enforcement is possible, the constraint is remediable and the specification can reassert authority. If authority is structurally asymmetric (operators have power, standards bodies don''t), the middlebox realism reading is locked in — the specification becomes permanently subordinate to deployed reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_under_contestation, conceptual, 'Whether specification authority can be restored or whether the asymmetry is structural.').

omega_variable(
    kernel_reading_divergence,
    'Which sibling reading (strict_invariance or optimization_latitude) best describes the deployed network from the perspective of endpoint developers, and what are the structural consequences of divergence?',
    'Qualitative study of endpoint developer decision-making: how do developers choose protocol implementations when they encounter middlebox behavior? Do they assume RFC 9293 invariance, or do they assume implementation latitude?',
    'If developers assume strict invariance, they are systematically surprised and forced to redesign — the constraint extracts from them. If developers assume optimization latitude, they may expect middlebox modification and design accordingly — extraction is reduced because expectations are adjusted. The reading relationship (forecloses/coexists/influences) depends on what developers actually assume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'How the kernel reading structure affects endpoint developer expectations and behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(rfc9_tr_t25, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(rfc9_tr_t35, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(rfc9_be_t25, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(rfc9_be_t35, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(rfc9_su_t25, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(rfc9_su_t35, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 35, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.18).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_protocol_migration).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, encrypted_traffic_blocking).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_policy_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rfc9293_tcp_specification kernel. The sibling strict_invariance_reading asserts that RFC 9293 specifies an unchangeable protocol contract; the optimization_latitude_reading asserts that the RFC permits semantic-preserving implementation variation. This reading asserts that deployed middleboxes override the specification; specification authority is subordinate to what the network actually enforces. Each reading has its own ε value, beneficiary/victim set, and classification. The readings coexist as live positions held by different parties (IETF purists vs. network operators vs. endpoint pragmatists). The network edges represent the structural dependency: endpoint behavior is constrained by middleboxes (affects_constraints edges model this influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, institutional, 0.92).
constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, moderate, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
