% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_optimization_latitude, []).

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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Specification: Optimization Latitude Reading
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 specifies TCP behavior as a semantic contract: reliable,
 *   in-order, flow-controlled byte-stream delivery. Under this
 *   optimization-latitude reading, the RFC defines WHAT TCP must do but
 *   leaves implementation HOW to the implementer, permitting innovations in
 *   congestion control (BBR, DCTCP), window management, and timer strategies
 *   within semantic bounds. The constraint coordinates endpoints on
 *   observable outcomes while enabling diverse internal strategies. This
 *   reading presents TCP specification as a Rope (low extractiveness, genuine
 *   coordination function, no party coerces others)—contrasted with the
 *   strict-invariance reading (which treats RFC 9293 as an invariant state
 *   machine and classified as higher-extraction Tangled Rope or Snare) and
 *   the middlebox-realism reading (which subordinates the RFC to
 *   deployed-network behavior). The claim/metric gap is intentional: this
 *   reading authoritatively claims Rope classification while acknowledging
 *   that resistance from strict-invariance advocates and real-world middlebox
 *   brittleness introduce some friction—the engine will measure where the
 *   actual operation sits.
 *
 * KEY AGENTS:
 *   - Transport-layer implementers: maintain OS TCP stacks, benefit from optimization freedom
 *   - Performance researchers: develop novel congestion-control algorithms (BBR, DCTCP), enabled by latitude interpretation
 *   - IETF working group: sets and clarifies the RFC interpretation; agenda setter
 *   - Application developers: benefit from performance improvements without code changes
 *   - Network operators: benefit from tuning capability but bear coordination costs
 *   - Legacy-system operators: structurally disadvantaged when innovation leaves them behind
 *   - Strict-invariance advocates: excluded from this reading; argue for tighter specification
 *   - Middlebox operators: pay costs of adapting to new TCP variants
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Optimization Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '8580e4d6-37c9-4f1a-b786-cf3633836455').
narrative_ontology:cs_kernel_codification('8580e4d6-37c9-4f1a-b786-cf3633836455', fixed_text).
narrative_ontology:cs_authority_grounding('8580e4d6-37c9-4f1a-b786-cf3633836455', expertise).
narrative_ontology:cs_interpretation_layer_present('8580e4d6-37c9-4f1a-b786-cf3633836455').
narrative_ontology:cs_reading_relation('8580e4d6-37c9-4f1a-b786-cf3633836455', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('8580e4d6-37c9-4f1a-b786-cf3633836455', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('8580e4d6-37c9-4f1a-b786-cf3633836455', foundational, semantic_contract_defines_interoperability).
narrative_ontology:cs_axiom_status(semantic_contract_defines_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('8580e4d6-37c9-4f1a-b786-cf3633836455', semantic_contract_defines_interoperability, instrumental).
narrative_ontology:cs_axiom('8580e4d6-37c9-4f1a-b786-cf3633836455', foundational, implementation_latitude_enables_innovation).
narrative_ontology:cs_axiom_status(implementation_latitude_enables_innovation, holdable).
narrative_ontology:cs_axiom_grounding('8580e4d6-37c9-4f1a-b786-cf3633836455', implementation_latitude_enables_innovation, conventional).
narrative_ontology:cs_reference_frame('8580e4d6-37c9-4f1a-b786-cf3633836455', rfc9293_semantic_contract_era).
narrative_ontology:cs_drift_state('8580e4d6-37c9-4f1a-b786-cf3633836455', contemporary_multi_algorithm_deployment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8580e4d6-37c9-4f1a-b786-cf3633836455', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, transport_layer_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, performance_optimization_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, legacy_system_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement TCP stacks in operating systems, embedded systems, and specialized hardware. Under the optimization-latitude reading, the RFC permits them to innovate on congestion control algorithms (BBR, DCTCP, QUIC-style approaches), window management, and timer strategies while maintaining the semantic contract (reliable ordered delivery). They benefit from the flexibility to tune for specific deployment contexts (data centers, mobile networks, satellite links) without forking from the standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, transport_layer_implementers, beneficiary,
    organized, generational, arbitrage, global).

% Academic and industry researchers develop new transport algorithms and congestion control strategies. The latitude reading permits them to prototype and deploy variants (BBR, DCTCP, Copa, etc.) without claiming to violate the RFC—they argue they satisfy the semantic contract (delivery, ordering, flow control) while optimizing different objectives (throughput, latency, fairness). Their career incentives and research funding depend on demonstrating innovation; the latitude reading legitimizes their work.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, performance_optimization_researchers, beneficiary,
    organized, biographical, mobile, global).

% Write applications that rely on TCP. They benefit from the diversity of implementations because it drives performance improvements and platform-specific optimizations. They do not directly participate in TCP implementation but benefit when their target platforms (Linux, Windows, macOS, iOS, Android) deploy newer congestion-control variants. Their software runs unchanged across all compliant implementations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Deploy and manage the network infrastructure that carries TCP traffic. They benefit from implementers' ability to tune TCP behavior to match their specific networks (data-center congestion control, long-fat-pipe routing, buffering strategies). They also bear costs when divergent implementations cause interoperability surprises or when they must police or work around non-compliant variants. Their control is limited by the fact that implementations are often external (endpoints, kernels they do not control).
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, payer).

% Maintains and evolves RFC 9293 and related TCP standards. Under the latitude reading, they interpret their role as specifying the semantic contract (what TCP must do) rather than prescribing the implementation path (how TCP must do it). They create and review errata, respond to implementer questions about edge cases, and occasionally issue clarifications. Their authority rests on broad consensus that the RFC captures the interoperability-critical contract; they do not have enforcement power over implementations, only the legitimacy of consensus.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_working_group, agenda_setter,
    institutional, generational, analytical, global).

% A minority of network researchers and protocol purists argue that RFC 9293 specifies a precise state machine and that divergent implementations create hidden interoperability risks. They would prefer a reading that treats the RFC as invariant and classifies variants as violations. They are excluded from the optimization-latitude reading by its core premise: their position would require a different constraint story (the strict_invariance_reading). They sometimes object in standards forums but lack the votes to shift the dominant interpretation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_advocates, excluded,
    organized, biographical, constrained, global).

% Deploy and maintain network appliances (firewalls, NAT boxes, traffic shapers, proxies) that observe and sometimes modify TCP traffic. The optimization-latitude reading permits implementers to use novel TCP options and behaviors that middleboxes may not recognize or handle correctly. Middlebox operators bear the cost of testing and updating their devices to work with new variants; if they fail, they must either update or accept that some traffic bypasses their appliances. They lack the power to mandate conservatism.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators, payer,
    moderate, biographical, constrained, global).

% Operate older TCP implementations or hardware that cannot be easily updated (embedded systems, industrial networks, long-lifecycle equipment). They pay the cost when new TCP variants become common and their systems cannot participate efficiently. They are trapped by the cost of replacement and the fact that the network evolves around them. The latitude reading enables innovation but leaves them behind.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, legacy_system_operators, payer,
    moderate, biographical, trapped, global).

% Other standards bodies (3GPP, IEEE, cellular standards consortia) track TCP evolution and sometimes need to make decisions about which TCP variants to mandate or permit in their own standards. They observe the latitude reading's permissiveness and must decide whether to adopt variants wholesale, require specific conservative behaviors, or remain agnostic. They have no direct enforcement power over RFC 9293 but influence which implementations are built.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, observer_standardization_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Specifies a semantic contract for TCP behavior (reliable, ordered, flow-controlled byte-stream delivery) that permits implementers to optimize the means of achieving that contract. Enables diverse implementations to coexist and interoperate by defining what matters (observable outcomes) rather than how to achieve it (internal algorithms). Solves the coordination problem of allowing innovation without forking the protocol.
% TRANSFER_FUNCTION: The IETF working group maintains the RFC; implementers accept the semantic contract and forgo the freedom to break interoperability in exchange for the freedom to optimize within the contract bounds. Application developers and users receive the benefit of interoperability; they do not directly pay (though they implicitly accept the coordination discipline). Middlebox operators and legacy-system operators bear the cost of adapting to new variants.
% ABSENT_VOICES: Protocol purists and strict-invariance advocates are structurally excluded from this reading. They would argue the RFC must specify implementation details to prevent covert violations and maintain true interoperability. Deployed-network realists (middlebox operators working in closed networks with strict policies) would argue for more prescriptive guidance. Parties with very long product-lifecycle constraints (embedded-systems vendors) would prefer slower innovation cycles and more conservative constraints. Their perspectives are not in the room when the latitude reading is authored.
% DISAPPEARANCE_RATIONALE: If the latitude reading disappeared and the strict-invariance reading took its place, the TCP ecosystem would reorganize immediately: research on novel congestion control would slow (variants would require exemptions or forking), OS vendors would face pressure to revert to RFC-dictated algorithms, performance optimization in data centers would become constrained, and the diversity of implementations would narrow. Conversely, if the middlebox-realism reading took over, the RFC would be reframed as aspirational and its enforcement would devolve to what the network actually permits—a more chaotic equilibrium.
% FOUNDING_PROBLEM: Early TCP implementations in the 1980s–90s had wide variation in congestion control and were vulnerable to collapse under high load. The RFC series (starting with RFC 793, updated by RFC 9293) was designed to specify the interoperability-critical behaviors (three-way handshake, sequence numbers, flow control) while leaving room for reasonable implementation choice on algorithms that did not break the wire contract. The founding problem was: how to standardize enough to prevent chaos but not so much that innovation is stifled.
% FOUNDING_PROBLEM_CORROBORATION: The IETF process records (working group minutes, mailing lists, published RFCs and errata) document the ongoing effort to clarify what RFC 9293 mandates vs. permits. Independent researchers (Van Jacobson, Jon Postel, later work on congestion-control diversity) and implementer communities (Linux kernel developers, BSD maintainers, Windows networking teams) attest that the founding problem—balancing standardization with innovation—remains unresolved and that the latitude reading is one coherent approach to it. The research literature on transport-layer innovation (BBR, DCTCP, etc.) demonstrates active reliance on the latitude interpretation.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very low (0.12) because the reading defines a genuine coordination function (semantic contract for interoperability) from which no party captures surplus. Implementers do not extract rent from other implementers; the IETF working group does not extract from implementers. Suppression is also minimal (0.08) because the constraint is maintained by voluntary adoption (implementations choose to comply because it buys them the entire installed base of TCP-speaking systems) rather than coercion. Theater is very low (0.05): there is minimal performative activity; the coordination function is straightforward and the measurement data reflects real operational alignment. Accessibility-collapse is low (0.15): alternatives exist (QUIC, SCTP, custom protocols), but TCP's dominance for general-purpose transport is structural. Resistance is moderate (0.35) because strict-invariance advocates actively object to the latitude interpretation and middlebox operators encounter real friction with new variants. The measurement series is flat because the RFC 9293 interpretation has been stable for decades; minor variation reflects occasional errata and edge-case clarifications but no fundamental drift in how the latitude reading is instantiated. All measurements share the same time grid (interval 0–40, sampled at 5-unit intervals plus endpoints 0 and 40).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (IETF) and beneficiaries (implementers, researchers) perceive this as genuine coordination enabling innovation. Legacy-system operators and strictness advocates perceive it as under-specification that leaves them exposed. Middlebox operators perceive it as a constraint they must adapt to but do not control. The engine will compute directionality from power and exit options: organized implementers with arbitrage exit will sit near beneficiary (d near 0.0); trapped legacy operators will sit near target (d near 1.0); the IETF as analytical observer (exit_options: analytical) will sit at symmetric (d = 0.5). These divergences are structural, not metric disagreements.
 *
 * DIRECTIONALITY LOGIC:
 *   Transport-layer implementers benefit from the latitude reading (can innovate without RFC violation). They have organized power and arbitrage exits (they can fork if needed, though they do not). Directionality for them is low (near 0.0, full beneficiary). Performance researchers likewise benefit and have mobile exits. Application developers benefit (free performance improvements). Network operators are dual-positioned: they benefit from the ability to tune TCP behavior to their networks, but also bear costs when implementations diverge (some complexity, some support burden). Directionality for them is symmetric (d ≈ 0.5). Middlebox operators and legacy-system operators bear costs (must adapt, risk left behind) but cannot exit (trapped by economic or technical constraints). Directionality for them is high (near 1.0, targets of the constraint's structure). Strict-invariance advocates are excluded; they do not sit inside this reading—they would populate a different constraint (strict_invariance_reading). The IETF working group is the agenda-setter (they interpret the RFC and issue clarifications) with analytical power; directionality is symmetric (they maintain the rule but do not extract from its operation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to standardize for interoperability without stifling innovation) is live: the constraint persists because both standardization and innovation are ongoing needs, and the latitude reading remains the dominant interpretation that balances them. If the founding problem became 'solved' (either all stakeholders agreed on tight specification or all agreed innovation speed was acceptable at any cost of interoperability), the latitude reading would face mandatrophy. Currently, the constraint's mandate is fresh because implementers and researchers continue to cite the latitude reading as the legitimacy source for their work. The constraint is classified as Rope—pure coordination—not Tangled Rope, because there is no identified asymmetric extraction or party forced into the arrangement. The IETF working group operates by consensus; implementers opt into RFC compliance; no party is coerced. Mandatrophy is not present (the constraint solves a live coordination problem with genuine mutual benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_contract_boundary_ambiguity,
    'Where exactly is the boundary between semantic contract (what RFC 9293 mandates) and implementation latitude (what it permits)? Specifically, which TCP behaviors are interoperability-critical outcomes vs. which are implementation-internal choices?',
    'Systematic interoperability testing: deploy diverse TCP implementations (Linux bbr, FreeBSD, Windows, QUIC-inspired variants) and measure which behavioral divergences cause packet loss, connection failures, or protocol violations. Behaviors that do not cause failures are truly internal; behaviors that do are contract-critical and should be mandated.',
    'If the boundary is discovered to be more permissive than currently assumed, the optimization-latitude reading is validated and extractiveness remains low. If the boundary is discovered to be stricter (some currently-variable behaviors must be synchronized for true interoperability), the strict-invariance reading gains strength and this reading''s classification might shift to lower purity. If the boundary is fuzzy or deployment-dependent, the middlebox-realism reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_contract_boundary_ambiguity, empirical, 'The semantic contract boundary is not precisely defined by RFC 9293 text; ambiguity around which TCP behaviors are interoperability-critical vs. implementation-free.').

omega_variable(
    middlebox_brittleness_risk,
    'To what extent do deployed middleboxes (firewalls, NAT boxes, traffic shapers) cause optimization variants to fail in practice? Is the latitude reading''s assumption that semantic-contract compliance guarantees interoperability valid in real networks?',
    'Large-scale measurement: instrument diverse client populations to measure TCP connection success rates with and without novel TCP options and behaviors. Track middlebox-caused failures (RST packets, option stripping, timeout patterns) and correlate with TCP variant. Compare to RFC-compliant implementations.',
    'If middlebox failures are rare, the latitude reading holds and interoperability is preserved through the semantic contract alone. If middlebox failures are common, the middlebox-realism reading is validated: deployed networks impose their own constraints that override the RFC. This could reclassify the constraint as Tangled Rope or Snare from the perspective of implementers trying to use new variants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_brittleness_risk, empirical, 'Whether middleboxes'' inability to recognize novel TCP options and behaviors undermines the optimization-latitude reading''s interoperability guarantee.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Is the optimization-latitude reading''s core premise logically foreclosed by the strict-invariance reading''s core premise, or do they genuinely coexist as different interpretive framings of the same RFC text?',
    'Logical analysis: examine RFC 9293 text to determine whether it explicitly mandates specific algorithms (state machine details, timer values, congestion-control parameters) or whether it permits multiple algorithmic paths. If explicit mandates exist, one reading is foreclosed; if the text is permissive, both readings can claim textual support and coexist.',
    'If foreclosed: the strict-invariance reading logically eliminates the optimization-latitude reading (they cannot both be true about what RFC 9293 requires). If coexistent: both remain live interpretations and the engine classifies them as coexists_with relation. The foreclosure test determines whether the kernel contest is a genuine disagreement or a logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the optimization-latitude and strict-invariance readings are logically foreclosed from each other or genuinely coexistent readings of the RFC 9293 kernel.').

omega_variable(
    beneficiary_extraction_asymmetry,
    'Do implementers or performance researchers capture disproportionate benefit (rent extraction) from the latitude interpretation, or is benefit genuinely mutual among all parties that adopt compliant implementations?',
    'Economic analysis: measure whether implementers that adopt novel congestion-control algorithms gain market share, premium pricing, or reduced support costs beyond what they would achieve with RFC-mandated algorithms. Measure researcher career gains (citations, funding, positions) correlated with novel-variant deployment success.',
    'If implementers/researchers extract concentrated benefit, the constraint could shift from Rope to Tangled Rope (coordinating other implementers and users, while extracting from legacy-system operators who are left behind). If benefit is mutual or diffuse, the Rope classification holds and extractiveness remains low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_asymmetry, empirical, 'Whether the flexibility enabled by the optimization-latitude reading is distributed fairly or concentrated in certain implementer/researcher seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_opt_lat_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t0, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t5, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t5, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t10, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t10, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t15, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t15, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t20, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t20, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t25, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t25, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t30, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t30, observed).
narrative_ontology:measurement(rfc9293_opt_lat_tr_t40, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(rfc9293_opt_lat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(rfc9293_opt_lat_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t0, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t5, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t5, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t10, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t10, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t15, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t15, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t20, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t20, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t25, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t25, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t30, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 30, 0.13).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t30, observed).
narrative_ontology:measurement(rfc9293_opt_lat_be_t40, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(rfc9293_opt_lat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_opt_lat_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.07).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t0, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t5, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t5, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t10, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t10, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t15, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t15, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t20, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t20, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t25, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 25, 0.09).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t25, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t30, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 30, 0.09).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t30, observed).
narrative_ontology:measurement(rfc9293_opt_lat_su_t40, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(rfc9293_opt_lat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% RFC 9293 TCP Specification forms a kernel with three coherent readings, each instantiating a different constraint. The optimization-latitude reading (this file) frames RFC 9293 as a semantic contract enabling diverse implementations; the strict-invariance reading frames it as an invariant state machine requiring implementation fidelity; the middlebox-realism reading frames it as subordinate to deployed-network behavior. The optimization-latitude reading influences both siblings: it defines the permissiveness space that strict-invariance advocates argue against, and it implicitly models an idealized network against which the middlebox-realism reading contrasts actual deployments. All three readings share the same RFC text kernel; they diverge in what they read the RFC as committing to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
