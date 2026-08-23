% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: TCP Specification Optimization Latitude Reading
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   RFC 9293 (2022) consolidates and obsoletes the original TCP specification
 *   (RFC 793) and decades of updates. This reading—the
 *   optimization_latitude_reading—holds that the specification defines a
 *   semantic contract (reliable, in-order byte stream with flow control and
 *   congestion signaling) but deliberately does not mandate implementation
 *   mechanisms. Endpoints may implement any congestion control, loss
 *   recovery, or pacing algorithm provided observable behavior stays within
 *   the semantic bounds. This enables high-performance variants (BBR, DCTCP,
 *   PCC, Copa) to deploy without breaking interoperability. The constraint is
 *   a Rope: it coordinates interoperable outcomes while granting
 *   implementation freedom, with low extractiveness and low suppression. The
 *   IETF standards process provides active enforcement of the semantic
 *   contract (interoperability testing, errata, bis updates) without
 *   dictating implementation internals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.15).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.2).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "TCP Specification Optimization Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__optimization_latitude_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, 'ca42f85c-6f08-4f3f-9a31-e419cfacd383').
narrative_ontology:cs_kernel_codification('ca42f85c-6f08-4f3f-9a31-e419cfacd383', formalized).
narrative_ontology:cs_authority_grounding('ca42f85c-6f08-4f3f-9a31-e419cfacd383', expertise).
narrative_ontology:cs_interpretation_layer_present('ca42f85c-6f08-4f3f-9a31-e419cfacd383').
narrative_ontology:cs_reading_relation('ca42f85c-6f08-4f3f-9a31-e419cfacd383', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca42f85c-6f08-4f3f-9a31-e419cfacd383', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('ca42f85c-6f08-4f3f-9a31-e419cfacd383', foundational, semantic_contract_sufficiency).
narrative_ontology:cs_axiom_status(semantic_contract_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('ca42f85c-6f08-4f3f-9a31-e419cfacd383', semantic_contract_sufficiency, conventional).
narrative_ontology:cs_axiom('ca42f85c-6f08-4f3f-9a31-e419cfacd383', foundational, implementation_freedom_enabled).
narrative_ontology:cs_axiom_status(implementation_freedom_enabled, holdable).
narrative_ontology:cs_axiom_grounding('ca42f85c-6f08-4f3f-9a31-e419cfacd383', implementation_freedom_enabled, empirically_contingent).
narrative_ontology:cs_reference_frame('ca42f85c-6f08-4f3f-9a31-e419cfacd383', rfc9293_semantic_contract).
narrative_ontology:cs_drift_state('ca42f85c-6f08-4f3f-9a31-e419cfacd383', contemporary_congestion_control_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca42f85c-6f08-4f3f-9a31-e419cfacd383', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_researchers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, interoperable_reliable_transport).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, permissionless_innovation_in_congestion_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the TCP specification through the IETF standards process: working groups produce bis documents, errata, and new RFCs. Administers interoperability testing at hackathons. Collects no revenue from the specification; authority derives from demonstrated competence in producing interoperable standards.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_transport_area, agenda_setter,
    institutional, generational, analytical, universal).

% Implement TCP stacks in OS kernels (Linux, Windows, BSD, Fuchsia) and user-space libraries. Gain a stable semantic target (RFC 9293) and freedom to choose congestion control, loss recovery, pacing. Can ship BBR, DCTCP, Cubic, Reno, or proprietary algorithms without breaking interoperability. Exit is mobile: they could implement a different transport (QUIC, SCTP) but TCP's ubiquity makes staying beneficial.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, tcp_implementers, beneficiary,
    organized, biographical, mobile, global).

% Build applications on TCP's reliable byte-stream abstraction. Benefit from performance improvements in underlying congestion control without code changes. Exit is mobile: can migrate to QUIC, WebTransport, or application-layer protocols, but TCP's universal reach makes it the default choice.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    organized, biographical, mobile, global).

% Experience TCP through applications (web, video, messaging, gaming). Benefit from lower latency, higher throughput, better fairness as congestion control evolves. Have no direct exit—they use whatever transport the application and OS provide. Their situation improves when the constraint enables better implementations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, end_users, beneficiary,
    powerless, immediate, analytical, universal).

% Design and publish new congestion control algorithms (BBR, DCTCP, PCC, Copa, etc.). The optimization latitude means they can deploy innovations in production kernels without standardizing the algorithm itself. Exit is mobile: they could research QUIC congestion control or application-layer rate adaptation, but TCP's install base makes it the primary venue.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_researchers, beneficiary,
    organized, biographical, mobile, global).

% Build firewalls, NATs, load balancers, DPI engines that inspect or modify TCP headers. Their products must correctly forward optimized TCP variants (BBR's pacing, DCTCP's ECN usage). They are not coordinated by this reading—their constraints appear in middlebox_realism_reading. They observe IETF discussions to anticipate compatibility issues.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors, observer,
    powerful, biographical, constrained, global).

% Embedded, industrial, or legacy OS TCP stacks that cannot be updated to exploit new optimization latitude (e.g., no ECN support, fixed Cubic). They interoperate at baseline semantics but cannot benefit from performance innovations. They are structurally excluded from the innovation cycle the constraint enables. Their voice is absent from IETF working groups.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, legacy_tcp_implementations, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interoperable reliable byte-stream delivery across heterogeneous endpoint implementations by specifying observable behavioral outcomes (in-order delivery, no duplication, flow control, congestion signaling) while leaving the implementation path—congestion control algorithms, loss recovery, pacing—to endpoint discretion.
% TRANSFER_FUNCTION: Moves implementation freedom to endpoints; constrains only the observable semantic contract. No value transfer between parties; the constraint enables rather than extracts.
% ABSENT_VOICES: Operators of ossified middlebox fleets that cannot parse or correctly forward optimized TCP variants; legacy embedded stacks that cannot be updated to exploit the latitude. They are structurally excluded from the innovation cycle the constraint enables.
% DISAPPEARANCE_RATIONALE: If the semantic contract and its permitted latitude vanished, every TCP implementation would need to converge on a single mandated algorithm or interoperability would fracture—endpoint innovation in congestion control (BBR, DCTCP, PCC, etc.) would be legally or technically foreclosed.
% FOUNDING_PROBLEM: The early Internet needed a single, universally implementable reliable transport protocol that could run over heterogeneous link layers without requiring per-network customization.
% FOUNDING_PROBLEM_CORROBORATION: The IETF transport area working groups, the RFC series archive, and three decades of multi-vendor interoperability testing (IETF hackathons, university testbeds) all attest that the founding problem—interoperable reliable transport across diverse networks—remains live and the semantic-contract approach remains the chosen solution.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint enables rather than extracts—it creates a commons of implementation approaches. Suppression is low (0.20) because alternative congestion control algorithms are not suppressed; they are explicitly permitted within semantic bounds. Theater ratio is low (0.10) because the coordination function (interoperable reliable transport) is genuine and the enforcement machinery (IETF process, interop testing) serves that function. Accessibility collapse is moderate (0.40) because implementations must conform to the semantic contract—alternatives that violate it (e.g., non-congestion-controlled transports) are excluded. Resistance is low (0.25) because the standard is voluntarily adopted by all major OS vendors and network equipment manufacturers.
 *
 * PERSPECTIVAL GAP:
 *   The strict_invariance_reading would compute higher suppression and extractiveness from the implementer seat (mandated state machine replication feels like enforcement). The middlebox_realism_reading would compute higher extractiveness from the middlebox vendor seat (endpoint optimization breaks middlebox assumptions). This reading computes low values from the implementer/application seats because the semantic contract is experienced as enabling. The engine will compute per-seat types from the structural data; the divergence across readings is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The IETF transport area (agenda_setter) administers the specification but collects no rents—its directionality is near-symmetric (d≈0.5). TCP implementers (beneficiary) gain a stable target to implement against and freedom to innovate—d near beneficiary end (d≈0.2). Application developers and end users (beneficiary) receive reliable transport without lock-in—d≈0.15. Congestion control researchers (beneficiary) gain a permissionless innovation space—d≈0.1. Middlebox vendors (observer) are affected but not coordinated by this reading—they appear in the middlebox_realism_reading. Legacy implementations (excluded) cannot easily adopt new optimizations but are not victimized by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interoperable reliable transport across heterogeneous networks) remains live. The arrangement has not atrophied—it has evolved through the bis process to explicitly codify the optimization latitude that was always implicit. No mandatrophy: the coordination function is active and the specification is actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading (optimization_latitude_reading) of the contested kernel rfc9293_tcp_specification. What structural elements do sibling readings (strict_invariance_reading, middlebox_realism_reading) change?',
    'Compare the three readings'' beneficiary/victim structures, extractiveness assessments, and claimed types. The kernel_id and reading_id are fixed; sibling readings are separate constraint stories linked via network.affects_constraints.',
    'If the semantic-contract reading is structurally a Rope while strict_invariance_reading computes as Mountain and middlebox_realism_reading as Tangled Rope, the kernel itself has no single classification—the classification is reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Committee-frame committer structure: kernel_id, reading_id, sibling readings').

omega_variable(
    semantic_bounds_clarity,
    'Are the ''semantic bounds'' of RFC 9293 sufficiently well-defined to prevent interoperability fragmentation when implementations exploit optimization latitude?',
    'Empirical analysis of interoperability failures between modern congestion control variants (BBR, DCTCP, PCC, Copa) and legacy stacks across diverse middlebox populations. Track IETF working group discussions on semantic boundary disputes.',
    'If bounds are ambiguous, the constraint drifts toward Tangled Rope (coordination function undermined by implementation divergence). If bounds are clear, Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_bounds_clarity, empirical, 'Whether the specification''s semantic contract is precise enough to coordinate without central enforcement of implementation details').

omega_variable(
    middlebox_traversal_impact,
    'Does the optimization latitude permitted by this reading enable or undermine traversal of deployed middleboxes that inspect or modify TCP headers?',
    'Measurement studies of modern congestion control variants (BBRv2, DCTCP) traversing enterprise firewalls, carrier-grade NATs, and stateful inspectors compared to legacy Reno/Cubic. Correlate with middlebox vendor firmware update cycles.',
    'If latitude increases middlebox-induced failures, the coordination function degrades and extractiveness rises (implementers must add workaround complexity). If latency improves traversal, the Rope strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middlebox_traversal_impact, empirical, 'Interaction between endpoint optimization freedom and path-dependent middlebox behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 2022, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_opt_lat_tr_t2022, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2022, 0.08).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t2023, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2023, 0.09).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t2024, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(rfc9293_opt_lat_be_t2022, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2022, 0.12).
narrative_ontology:measurement(rfc9293_opt_lat_be_t2023, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2023, 0.14).
narrative_ontology:measurement(rfc9293_opt_lat_be_t2024, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_opt_lat_su_t2022, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2022, 0.18).
narrative_ontology:measurement(rfc9293_opt_lat_su_t2023, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2023, 0.19).
narrative_ontology:measurement(rfc9293_opt_lat_su_t2024, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_algorithm_deployment).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_behavior_evolution).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, transport_protocol_interoperability_testing).

% DUAL FORMULATION NOTE:
% Part of the rfc9293_tcp_specification constraint family (3 readings). This reading emphasizes the semantic-contract/implementation-freedom decomposition. strict_invariance_reading emphasizes state-machine fidelity. middlebox_realism_reading emphasizes path-dependence. The three readings share the same RFC text but instantiate different constraints with different ε, beneficiary structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
