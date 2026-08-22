% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__strict_invariance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP Strict State Machine Invariance
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 specifies the TCP state machine and associated behavior that all
 *   implementations must replicate to preserve global interoperability. The
 *   strict invariance reading interprets this as a binding specification
 *   contract: implementations must conform exactly; deviations are violations
 *   that threaten the downstream assumptions of endpoints and applications.
 *   This reading emphasizes the role of RFC 9293 as a coordination mechanism
 *   that solved a real historical problem (implementation divergence) and
 *   that persists because the coordination benefit is real and continues to
 *   generate value. The strict invariance reading is one of three readings of
 *   the same kernel; sibling readings (optimization_latitude_reading and
 *   middlebox_realism_reading) contest what RFC 9293 permits and what role
 *   deployed middleboxes play in shaping actual TCP behavior.
 *
 * KEY AGENTS:
 *   - strict_implementation_vendors: Vendors whose business model depends on producing correct, portable TCP stacks (Linux, BSD, Windows)
 *   - interoperable_endpoints: Systems and applications that rely on RFC 9293 semantics to communicate
 *   - application_layer: Protocol standards (HTTP, TLS) and applications that outsource reliability to TCP
 *   - middlebox_operators: Operators of network devices that modify TCP state machines for policy and performance (EXCLUDED from strict invariance reading)
 *   - standards_authority_ietf: The IETF, which publishes RFC 9293 and maintains the specification as a coordination contract
 *   - specification_readers: Implementers, testers, and researchers who interpret what RFC 9293 requires
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.12).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP Strict State Machine Invariance").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '9a8adb2a-5817-45d9-8a5d-a68c01f769d6').
narrative_ontology:cs_kernel_codification('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', fixed_text).
narrative_ontology:cs_authority_grounding('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', expertise).
narrative_ontology:cs_interpretation_layer_present('9a8adb2a-5817-45d9-8a5d-a68c01f769d6').
narrative_ontology:cs_reading_relation('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', foundational, specification_binding_invariance).
narrative_ontology:cs_axiom_status(specification_binding_invariance, holdable).
narrative_ontology:cs_axiom_grounding('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', specification_binding_invariance, conventional).
narrative_ontology:cs_axiom('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', secondary, middlebox_deviation_is_violation).
narrative_ontology:cs_axiom_status(middlebox_deviation_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', middlebox_deviation_is_violation, deontological).
narrative_ontology:cs_reference_frame('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', rfc9293_as_binding_contract).
narrative_ontology:cs_drift_state('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', contemporary_network_middlebox_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a8adb2a-5817-45d9-8a5d-a68c01f769d6', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, interoperable_endpoints).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_layer).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, strict_implementation_vendors).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, global_tcp_interoperability).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, specification_as_contract).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations that implement RFC 9293 exactly as specified. They benefit from deterministic protocol semantics: the cost of development is high, but once conformant, their implementation works predictably across the global internet. They face no lock-in because the standard is open and portable. Their exit is migration to a different protocol or a different implementation of TCP.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, strict_implementation_vendors, beneficiary,
    organized, generational, mobile, global).

% End systems (clients, servers, intermediate nodes) that rely on RFC 9293 semantics to communicate reliably. They benefit from the invariance: they can assume that any conformant TCP stack will handle their byte stream correctly. Their exit is using a different transport protocol (QUIC, SCTP, custom protocols) or relying on middleboxes that violate the specification.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, interoperable_endpoints, beneficiary,
    organized, generational, mobile, global).

% Application developers and protocols that assume TCP provides the contracted semantics: ordered, reliable delivery of a byte stream, with defined behavior under packet loss and reordering. They benefit from not having to implement transport-layer reliability themselves. Their exit is implementing reliability at the application layer or using an alternative transport.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_layer, beneficiary,
    organized, generational, mobile, global).

% Network operators of proxies, firewalls, NAT devices, and TCP optimizers who modify TCP state machines to achieve policy, performance, or access control goals. They are excluded from the conformance contract: the strict invariance reading treats their modifications as violations that break assumptions downstream endpoints rely on. They would argue that deployed reality requires protocol flexibility and that strict specification is incompatible with operational necessity.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_operators, excluded,
    powerful, biographical, constrained, global).

% The Internet Engineering Task Force, which publishes RFC 9293 and maintains the authoritative specification. They set the standard as a binding contract for conformance and bear responsibility for ensuring the specification is implementable and correct. They enforce conformance through community consensus and testing frameworks, not through legal authority or technical gatekeeping.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, standards_authority_ietf, agenda_setter,
    institutional, generational, analytical, global).

% Implementers, operators, security researchers, and specification conformance testers who read RFC 9293 and make judgments about what compliance requires. They observe the constraint in operation and report divergences between specification and practice.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, specification_readers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Specifies an invariant state machine that all TCP implementations must replicate to ensure that TCP endpoints can communicate reliably regardless of implementation vendor or deployment context. Solves the coordination problem: how can thousands of independent implementations agree on what 'TCP' means so that any endpoint can talk to any other endpoint.
% TRANSFER_FUNCTION: The constraint transfers no resources. It is a pure coordination mechanism: it specifies behavioral obligations (state transitions, timeout handling, sequence number arithmetic) that every implementation must honor. Implementers invest development effort to conform; they receive the benefit of global interoperability. No party collects rents from conformance.
% ABSENT_VOICES: Middlebox operators (firewalls, proxies, NAT devices, TCP optimizers) are structurally excluded from the strict invariance reading. They would argue that deployed networks require protocol flexibility, that real TCP has always been path-dependent, and that strict specification is operationally unrealistic. Their exclusion is the reading's defining boundary: strict invariance means no modification, period.
% DISAPPEARANCE_RATIONALE: If RFC 9293's strict state machine requirement vanished, implementations would diverge from each other. TCP would become vendor-specific; interoperability would degrade to negotiation and fallback. Applications relying on TCP semantics would need to implement contingency logic for variant TCP behaviors. The global internet's transport layer would fragment into compatibility domains. The constraint's disappearance would require the application layer to re-solve the coordination problem it currently outsources to TCP.
% FOUNDING_PROBLEM: Early TCP implementations diverged on critical details (state machine transitions, retransmission timers, sequence number arithmetic, window scaling). These divergences caused interoperability failures. The founding problem was: how can we specify TCP precisely enough that all implementers produce compatible code, without over-constraining implementers and preventing optimization?
% FOUNDING_PROBLEM_CORROBORATION: The IETF standards community and TCP implementation vendors attest the founding problem was historically real and RFC 9293 addresses it. However, middlebox operators and network researchers (Langley, Ford, Karpinski) attest that the founding problem was solved for endpoint-to-endpoint TCP but that deployed networks now require middlebox modifications the strict specification forbids — the founding problem was displaced, not solved.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).
:- end_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because RFC 9293 is a pure coordination mechanism: it specifies obligations (benefits via interoperability) but extracts no value from compliance. All beneficiary seats gain genuine coordination benefit; there are no victims in the strict invariance reading. Suppression is similarly low (0.12) because the constraint is enforced through open standards process, not through technical gatekeeping or coercion — implementers conform because conformance is in their interest. Theater ratio is minimal (0.05) because the specification's function is transparent: every performance claim about TCP can be traced to the specification. Accessibility collapse is high (0.92) because once the strict invariance reading is understood (that RFC 9293 is binding), alternatives are nearly invisible: deviations are seen as violations, not innovations. Resistance is low (0.18) because the constraint's legitimacy is widely accepted; resistance comes only from middlebox operators whose exclusion is deliberate. The measurement series shows stable extractiveness and suppression across the interval (2022-2026), with slight theater increase reflecting growing application-layer audit and compliance testing.
 *
 * PERSPECTIVAL GAP:
 *   The strict invariance reading is adopted by endpoint implementers and application-layer actors who benefit from predictable protocol semantics. Middlebox operators and network researchers adopt competing readings (optimization_latitude_reading, middlebox_realism_reading) that see RFC 9293 as specifying ideal endpoint behavior but permitting operational flexibility. The engine will compute different type classifications for these seats: endpoints and vendors compute RFC 9293 as a coordination mechanism (rope) with low extraction; middlebox operators compute it as a constraint that excludes their practices (snare-adjacent, high suppression via exclusion). This divergence is structural, not a measurement error — it reflects the reading's definitional choice to exclude middleboxes from conformance expectations.
 *
 * DIRECTIONALITY LOGIC:
 *   The strict invariance reading imposes no directionality variance: all beneficiary seats (implementations, endpoints, applications) have symmetric relationships to RFC 9293. They all benefit equally from the coordination benefit (d ≈ 0.2, beneficiary-leaning). Middlebox operators are excluded, not target-positioned; exclusion is not extraction. The IETF is the specification authority (agenda_setter) but does not extract value — they maintain the standard as a public good (d ≈ 0.5, symmetric). This constraint has no high-d target seats in the strict invariance reading; the piton-adjacent dynamics (middlebox exclusion, growing theater) emerge from competing readings.
 *
 * MANDATROPHY ANALYSIS:
 *   RFC 9293 is NOT subject to mandatrophy in the strict invariance reading. The founding problem (implementation divergence leading to interoperability failure) is live and remains solve-worthy by the specification. Endpoints and applications continue to benefit from knowing they can rely on RFC 9293 semantics. The founding_problem_status is 'contested' rather than 'live' or 'dead' because middlebox operators and some researchers argue the founding problem was only solved for endpoints, not for the full path; real TCP must accommodate middleboxes. But the constraint itself (the strict state machine specification) continues to do coordinated work for the seats that adopt the strict invariance reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_binding_force,
    'Is RFC 9293 a binding specification that all TCP implementations must conform to exactly, or does it describe ideal endpoint behavior while permitting implementation latitude and accommodating deployed middleboxes?',
    'Empirical: survey implemented TCP stacks and identify deviations from RFC 9293; measure interoperability failure rates correlating with specification deviation. Normative: IETF working group consensus on what RFC 9293 permits vs. forbids.',
    'If binding: strict invariance reading stands; deviations are violations that threaten assumptions downstream. If permissive: optimization_latitude and middlebox_realism readings gain ground; specification becomes descriptive rather than normative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_binding_force, conceptual, 'Whether RFC 9293 is a binding contract or a permissive guideline.').

omega_variable(
    middlebox_violation_vs_necessity,
    'Are middlebox modifications of TCP state machines violations of the specification, or are they necessary operational accommodations to deployed network realities?',
    'Comparative analysis: measure network path characteristics (packet loss, reordering, middlebox presence) and correlate with interoperability outcomes; identify whether middlebox modifications improve or degrade application-layer reliability.',
    'If violations: strict invariance holds and middlebox modifications are an extraction mechanism (snare-like) that benefits operators at the cost of endpoint assumptions. If necessary: middleboxes are part of the real TCP ecosystem and the strict invariance reading is operationally unrealistic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(middlebox_violation_vs_necessity, empirical, 'Whether middlebox TCP state machine modification is violation or operational necessity.').

omega_variable(
    specification_vs_deployed_authority,
    'Does authority for ''what TCP is'' rest in RFC 9293 (the specification) or in deployed network behavior (what middleboxes and implementations actually do)?',
    'Historical case studies: trace specific TCP features (window scaling, SACK, TCP Fast Open) through specification authoring, deployment divergence, middlebox adaptation, and interoperability outcomes. Identify which authority source (spec or practice) determined actual TCP behavior.',
    'If specification authority: strict invariance and etic (outsider) reading of RFC 9293 stands. If practice authority: middlebox_realism_reading gains ground; TCP is emic (insider), path-dependent, and specification is post-hoc description of what deployed network does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_vs_deployed_authority, conceptual, 'Authority for TCP specification: normative text or deployed practice.').

omega_variable(
    reading_foreclosure_query,
    'Is the strict invariance reading''s core premise (specification is binding and deviations are violations) logically foreclosed by the middlebox_realism_reading''s core premise (deployed TCP is path-dependent and specification authority is subordinate to network behavior)?',
    'Logical analysis of the axioms: if both readings could be true within a single commitment framework (e.g., if specification could be binding AND reality could be path-dependent as two layers of the same system), then they coexist. If one premise directly contradicts the other within any single coherent framework, then foreclosure obtains.',
    'If foreclosed: exactly one reading is tenable; the other must be rejected at the commitment-system level. If coexist: both readings remain live for different parties or layers; classification divergence is expected and unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_query, conceptual, 'Whether strict invariance and middlebox realism axioms can coexist in one framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 2022, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2022, 0.03).
narrative_ontology:measurement_basis(rfc9_tr_t2022, observed).
narrative_ontology:measurement(rfc9_tr_t2023, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2023, 0.04).
narrative_ontology:measurement_basis(rfc9_tr_t2023, observed).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2024, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t2024, observed).
narrative_ontology:measurement(rfc9_tr_t2025, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2025, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t2025, observed).
narrative_ontology:measurement(rfc9_tr_t2026, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2026, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2022, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2022, observed).
narrative_ontology:measurement(rfc9_be_t2023, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2023, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2023, observed).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2024, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2024, observed).
narrative_ontology:measurement(rfc9_be_t2025, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2025, observed).
narrative_ontology:measurement(rfc9_be_t2026, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2026, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t2022, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2022, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t2022, observed).
narrative_ontology:measurement(rfc9_su_t2023, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2023, 0.09).
narrative_ontology:measurement_basis(rfc9_su_t2023, observed).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2024, 0.11).
narrative_ontology:measurement_basis(rfc9_su_t2024, observed).
narrative_ontology:measurement(rfc9_su_t2025, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2025, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t2025, observed).
narrative_ontology:measurement(rfc9_su_t2026, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.05).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% RFC 9293 is a contested kernel (fixed_text) with three distinct readings corresponding to three different constraint stories. strict_invariance_reading (this story) treats the specification as binding; optimization_latitude_reading permits semantic-preserving variance; middlebox_realism_reading subordinates specification to deployed behavior. The three readings persist simultaneously across different institutional seats (endpoint vendors, optimization researchers, network operators) and are linked by network.affects_constraints. Decomposition rationale: each reading produces a different ε, different beneficiary/victim structure, and different classification. A single constraint story cannot capture the reading contest without violating ε-invariance; the contest is modeled as a constraint family with linked stories and omega variables naming the interpretation divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
