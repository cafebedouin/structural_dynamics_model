% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Behavior Under Middlebox-Realist Reading of RFC 9293
 *   domain: network_protocol_engineering
 *
 * SUMMARY:
 *   This story instantiates the middlebox-realism reading of the RFC 9293
 *   kernel: the specification describes an idealized two-endpoint state
 *   machine, but the actually-deployed network is shaped by a dense
 *   population of NATs, firewalls, traffic shapers, and intercept boxes that
 *   rewrite, drop, or reset traffic according to their own operators'
 *   interests. Under this reading, the RFC's authority is subordinate to what
 *   these intermediaries actually do — implementers must build for the
 *   deployed network, not the document. This is NOT a story about
 *   implementation latitude within the spec (the
 *   optimization_latitude_reading) or about strict endpoint-to-endpoint
 *   conformance (the strict_invariance_reading); those are separate
 *   constraints with separate ε values. Here the specification is read as
 *   increasingly aspirational, and the real governing force is the
 *   distributed, uncoordinated, and often opaque middlebox layer that
 *   extracts control from endpoints without their consent.
 *
 * KEY AGENTS:
 *   - isp_traffic_management_operators: primary beneficiary — collects operational and regulatory leverage from opaque packet modification
 *   - state_surveillance_apparatus: primary beneficiary — uses the middlebox layer as an interception point the RFC never contemplated
 *   - protocol_implementers: primary target — bears the engineering cost of building for the deployed network rather than the spec
 *   - endpoint_users_seeking_privacy: primary target — trapped, no visibility into or control over path-level modification
 *   - ietf_tcpm_working_group: analytical observer — documents the ossification gap in informational RFCs
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
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Behavior Under Middlebox-Realist Reading of RFC 9293").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '067c8471-b194-4077-b557-3d3dd6777d5c').
narrative_ontology:cs_kernel_codification('067c8471-b194-4077-b557-3d3dd6777d5c', fixed_text).
narrative_ontology:cs_authority_grounding('067c8471-b194-4077-b557-3d3dd6777d5c', extraction).
narrative_ontology:cs_interpretation_layer_present('067c8471-b194-4077-b557-3d3dd6777d5c').
narrative_ontology:cs_reading_relation('067c8471-b194-4077-b557-3d3dd6777d5c', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('067c8471-b194-4077-b557-3d3dd6777d5c', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('067c8471-b194-4077-b557-3d3dd6777d5c', foundational, deployed_network_behavior_governs_over_specification_text).
narrative_ontology:cs_axiom_status(deployed_network_behavior_governs_over_specification_text, holdable).
narrative_ontology:cs_axiom_grounding('067c8471-b194-4077-b557-3d3dd6777d5c', deployed_network_behavior_governs_over_specification_text, empirically_contingent).
narrative_ontology:cs_axiom('067c8471-b194-4077-b557-3d3dd6777d5c', secondary, middlebox_mediated_control_transfer_is_nonconsensual_extraction).
narrative_ontology:cs_axiom_status(middlebox_mediated_control_transfer_is_nonconsensual_extraction, holdable).
narrative_ontology:cs_axiom_grounding('067c8471-b194-4077-b557-3d3dd6777d5c', middlebox_mediated_control_transfer_is_nonconsensual_extraction, deontological).
narrative_ontology:cs_reference_frame('067c8471-b194-4077-b557-3d3dd6777d5c', endpoint_to_endpoint_state_machine_authority).
narrative_ontology:cs_drift_state('067c8471-b194-4077-b557-3d3dd6777d5c', post_ossification_measurement_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('067c8471-b194-4077-b557-3d3dd6777d5c', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_apparatus).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendor_ecosystem).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users_seeking_privacy).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, novel_transport_protocol_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy carrier-grade NAT, traffic shapers, and deep packet inspection boxes that rewrite sequence numbers, window sizes, and options fields to manage congestion, enforce tiered service, and comply with lawful-intercept mandates. They collect operational leverage and, in many jurisdictions, revenue or regulatory cover from this control. They are not bound by RFC 9293's endpoint-only state machine and face no interoperability penalty for deviating from it, since their boxes sit invisibly between endpoints who cannot detect the rewriting.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators, agenda_setter).

% Terminate and re-originate TCP connections at network boundaries, inspect payloads, and reset connections that appear anomalous relative to expected patterns, using RFC 9293 conformance as an operational baseline while treating deviations from that baseline as grounds to drop or reset the flow. They gain security posture and policy control at the cost of breaking protocols the specification would otherwise permit.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, beneficiary,
    organized, biographical, constrained, national).

% Requires ISPs and transit providers to expose or preserve TCP session metadata and, in some regimes, mandates specific middlebox behaviors (session logging, connection reset on flagged patterns) that are invisible to the RFC's endpoint model entirely. Benefits from a network where 'what the wire does' rather than 'what the spec says' governs, because that gap is exactly where interception and policy enforcement live.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Sells the boxes that perform the rewriting, inspection, and enforcement. Has a direct commercial interest in the persistence of a network where compliant behavior alone does not guarantee interoperability, since that gap is the market for their proprietary heuristics, ossification-avoidance patches, and workaround engineering.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendor_ecosystem, beneficiary,
    organized, generational, arbitrage, global).

% Write TCP stacks that must diverge from the letter of RFC 9293 to actually work in the field — implementing workarounds for options-stripping, MSS clamping, sequence number randomization side effects, and connection resets from stateful middleboxes never mentioned in the specification. Cannot simply implement the RFC and expect a working stack; must reverse-engineer the deployed middlebox population instead, at ongoing engineering cost with no path to a stable target.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers, payer,
    moderate, biographical, constrained, global).

% Build applications assuming RFC-conformant transport behavior, then discover in production that middleboxes silently mangle TCP options, terminate long-idle connections, or throttle flows that do not match expected fingerprints. Must add retry logic, keepalives, and protocol-detection heuristics purely to survive a network that does not implement the specification they were told to trust.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_application_developers, payer,
    moderate, biographical, constrained, global).

% Have no visibility into or control over which middleboxes sit on their path, what they log, or what they modify. Their traffic is shaped, throttled, or exposed by intermediaries the specification never contemplated as legitimate parties to the connection. Exit requires tunneling or encryption overlays that themselves become targets of further middlebox policy.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users_seeking_privacy, payer,
    powerless, immediate, trapped, national).

% Attempt to deploy new transport behaviors (multipath extensions, novel congestion signaling, QUIC-style UDP encapsulation) and find that middlebox ossification — not the RFC — is the actual deployment barrier. Many are forced onto UDP encapsulation specifically to escape middlebox interference with TCP, which is itself evidence that TCP's real governing authority is the deployed box population, not the text of 9293. Their mobility comes at the cost of abandoning TCP's ecosystem entirely.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, novel_transport_protocol_designers, payer,
    organized, generational, mobile, global).

% Maintains and revises the specification, documents known middlebox interference patterns in informational RFCs, and designs protocol extensions defensively around expected ossification. Aware that the document it stewards functions increasingly as aspiration and historical record rather than binding behavioral law.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_tcpm_working_group, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, RFC 9293 still provides a shared vocabulary and baseline state machine that lets independently built stacks interoperate MOST of the time, and gives operators and implementers a common reference point for diagnosing failures — a real coordination function that predates and partly survives the middlebox layer.
% TRANSFER_FUNCTION: Moves effective control over connection semantics — what counts as a valid packet, when a connection is considered dead, what options survive transit — from the two communicating endpoints (as the specification designs) to whichever middlebox operators sit on the path, without compensation or consent from the endpoints whose traffic is reshaped.
% ABSENT_VOICES: End users whose traffic is throttled, logged, or reset have no seat in IETF process and no visibility into which boxes touch their connections. Application developers in jurisdictions with heavy state-mandated middlebox deployment are structurally unable to raise the issue without political risk. Their exclusion is precisely why the gap between 'what RFC 9293 says' and 'what the network does' persists undocumented in most operational contexts.
% DISAPPEARANCE_RATIONALE: If middlebox modification of TCP behavior vanished overnight, protocol implementers and application developers would see dramatic simplification — no more MSS-clamping workarounds, no more silent option-stripping — and the RFC's endpoint model would become directly enforceable. But ISPs would lose congestion management and lawful-intercept tooling, enterprises would lose perimeter security controls, and state surveillance regimes would lose a primary interception layer; those parties would argue the world does NOT unchange, it loses essential control functions. The two camps do not agree on which counterfactual is the honest one.
% FOUNDING_PROBLEM: RFC 9293 (and its predecessor RFC 793) was built to solve the problem of reliable, ordered, congestion-aware byte-stream delivery between two cooperating endpoints across an unreliable, best-effort packet network — a purely technical interoperability problem with no third-party intermediary contemplated as a legitimate protocol participant.
% FOUNDING_PROBLEM_CORROBORATION: The IETF TCPM working group's own informational RFCs (e.g. RFC 3234 on middlebox taxonomy, and subsequent ossification studies) attest from outside any single beneficiary group that the original endpoint-to-endpoint problem is now entangled with a middlebox layer never part of the founding design — this is documented by protocol researchers and measurement studies independent of ISPs, enterprises, or surveillance operators, and constitutes corroboration from outside the benefiting parties that the founding problem has been structurally altered, not merely extended.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, contested).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects that middlebox operators capture real control over connection semantics without compensating the endpoints whose behavior is reshaped; this is asymmetric and non-consensual, distinguishing it from the optimization_latitude reading's benign implementation freedom. Suppression (0.71) is high because implementers and users cannot detect, contest, or route around most middlebox behavior — it is invisible by design and enforced by network position, not negotiation. Theater ratio (0.42) captures that a meaningful share of 'RFC-conformant' claims by vendors and operators is performative: boxes advertise standards compliance while actively deviating from the endpoint model in operationally significant ways. Accessibility collapse (0.58) is moderate rather than near-total because encrypted/tunneled overlays (QUIC, VPNs) still offer partial escape, which is also why novel_transport_protocol_designers carry 'mobile' rather than 'trapped' exit — at the cost of abandoning TCP's ecosystem.
 *
 * PERSPECTIVAL GAP:
 *   From the middlebox operator seat, the arrangement looks like legitimate network management operating in a space the RFC simply doesn't address — coordination plus necessary security/policy layering. From the protocol implementer and endpoint user seat, the identical structure is uncompensated extraction of control that was never ceded and cannot be effectively contested. The engine should compute divergent per-seat classifications from these structurally different exit options and power levels, not from any claim either side makes about legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators (ISPs, enterprises, surveillance bodies, vendors) are the structural beneficiaries: they gain leverage, revenue, security posture, or intercept capability from the gap between specified and deployed behavior, and their institutional power plus arbitrage-grade exit (they choose whether and how to comply with the RFC) puts them near the beneficiary end of directionality. Protocol implementers, application developers, and endpoint users are targets: they bear the cost of the gap in engineering effort, degraded reliability, or lost privacy, with constrained-to-trapped exit options since they cannot detect or bypass most middlebox behavior. Novel transport designers are a partial exception — their mobility (shifting to UDP encapsulation) is itself a symptom of the extraction, not an escape from directionality pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (shared vocabulary, baseline interoperability) is real and has not vanished — this prevents collapsing the reading into a pure snare. But the specification's founding problem (endpoint-to-endpoint reliable delivery with no legitimate third party) is contested as still-live: middlebox operators would say their function serves a NEW live problem (congestion management, security, lawful intercept) that the original RFC never addressed, while implementers and researchers document that this new function was layered onto, not replacing, the original coordination — hence tangled_rope rather than mountain (no natural inevitability), rope (no asymmetric extraction), or pure snare (a genuine coordination function persists underneath the extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_authority_vs_deployed_reality,
    'Is RFC 9293 still the authoritative definition of TCP, with middlebox deviations classified as protocol violations to be corrected, or has deployed middlebox behavior become the de facto specification that the RFC now merely documents historically?',
    'Track whether IETF-driven remediation (e.g., protocol ossification workarounds, encrypted transport headers via QUIC-style designs) succeeds in restoring endpoint control, versus whether new RFCs increasingly codify middlebox behavior as expected/normative rather than as deviation.',
    'If the RFC retains authority and middlebox interference is successfully engineered around or eliminated over time, this reading weakens toward a scaffold (temporary interference during a transitional ossified period) or even a rope. If deployed behavior increasingly displaces the RFC as the operative standard, this reading strengthens toward snare, since the coordination function would be swallowed entirely by the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_vs_deployed_reality, empirical, 'Whether specification text or deployed middlebox population is the actual governing authority for TCP behavior.').

omega_variable(
    surveillance_vs_engineering_motive_mix,
    'What proportion of middlebox-driven TCP deviation is attributable to legitimate engineering needs (congestion management, security) versus surveillance/policy control extraction with no engineering justification?',
    'Comparative measurement studies across jurisdictions with different regulatory regimes (heavy state-mandated interception vs. minimal) controlling for network engineering practices, to isolate the surveillance-attributable share of middlebox interference.',
    'A high surveillance-attributable share strengthens the case for snare classification (extraction with a coordination cover story); a low share strengthens tangled_rope (genuine mixed function) or even suggests some middlebox behavior belongs in a separate, less extractive reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surveillance_vs_engineering_motive_mix, empirical, 'Decomposing middlebox interference into surveillance-extraction versus legitimate-engineering components.').

omega_variable(
    reading_boundary_determination,
    'Where exactly does the middlebox_realism_reading''s boundary sit relative to the optimization_latitude_reading — is a given middlebox behavior (e.g., MSS clamping for path MTU) legitimate optimization latitude or extractive control-taking?',
    'This is a conceptual boundary question, not resolvable by data alone: it depends on whether one treats the middlebox as a legitimate protocol participant (optimization_latitude framing) or an illegitimate intermediary (middlebox_realism framing). The IETF''s own classification of a given practice as ''recommended'' versus ''harmful'' in informational RFCs is the closest available signal, but is itself contested.',
    'Shifts stakeholder and metric assignment between this story and its optimization_latitude sibling for specific technical practices, without changing either story''s overall ε — this is the committer-axis disagreement itself, located precisely at which behaviors count as coordination versus extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_determination, conceptual, 'The specific structural location of disagreement between the middlebox_realism and optimization_latitude readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement(rfc9_tr_t1995, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(rfc9_tr_t2004, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(rfc9_tr_t2012, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2012, 0.31).
narrative_ontology:measurement(rfc9_tr_t2018, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1981, 0.15).
narrative_ontology:measurement(rfc9_be_t1995, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(rfc9_be_t2004, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2004, 0.45).
narrative_ontology:measurement(rfc9_be_t2012, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(rfc9_be_t2018, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1981, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1981, 0.1).
narrative_ontology:measurement(rfc9_su_t1995, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(rfc9_su_t2004, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement(rfc9_su_t2012, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement(rfc9_su_t2018, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2018, 0.66).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.1).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'RFC 9293 defines TCP.' Each reading is ε-invariant on its own: strict_invariance_reading (mountain/rope framing, low extraction, endpoint conformance is the governing logic) and optimization_latitude_reading (rope framing, low-moderate extraction, implementation freedom within semantic bounds) sit at the low end of the family's ε range; this middlebox_realism_reading sits substantially higher (0.68) because it identifies an asymmetric, non-consensual control transfer to intermediaries that the other two readings do not treat as structurally present. The three are linked via affects_constraints rather than merged, per the ε-invariance principle — averaging or hedging across them would misrepresent all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
