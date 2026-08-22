% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: RFC 9293 TCP Specification: Outcome-Based Coordination with Implementation Flexibility
 *   domain: network/standards/distributed-systems
 *
 * SUMMARY:
 *   RFC 9293 (the TCP specification standard) instantiates a choice about how
 *   to specify distributed protocols: by outcome (semantic contract: reliable
 *   in-order delivery) or by implementation (prescriptive state machine:
 *   exact behavior dictated). This constraint story is the OUTCOME-BASED
 *   READING, which holds that specification of what-happens suffices for
 *   interoperability, and the latitude on how-it-happens enables innovation
 *   and performance optimization. This reading is one of three contested
 *   readings of the same RFC kernel: (1) the optimization-latitude reading
 *   (this one) — outcome specification enables diversity; (2) the
 *   strict-invariance reading — exact state-machine replication is necessary
 *   for safety; (3) the middlebox-realism reading — RFC authority is
 *   subordinate to deployed network behavior. Each reading is instantiated as
 *   a separate constraint story. This story models the optimization-latitude
 *   reading as genuine coordination (Rope) with low extractiveness: the
 *   standards body coordinates implementers on outcomes, benefiting all by
 *   enabling optimization latitude and preventing congestion collapse,
 *   without extracting value from that coordination.
 *
 * KEY AGENTS:
 *   - IETF/standards_body: sets the RFC specification and maintains it through consensus. Agenda-setter position: their authority is to define the semantic contract. Power: institutional (norm-setting through widespread adoption, not coercive). Exit: implementers can fork or adopt alternative protocols, but exit is costly.
 *   - protocol_implementers (OS vendors, embedded systems, user-space stacks): benefit from outcome latitude. Beneficiary position: they can optimize congestion control, buffer management, hardware acceleration within the semantic bounds. Power: organized (multiple vendors, but coordinated through standards compliance). Exit: mobile (can implement alternative algorithms or protocols), but adoption of new protocol requires ecosystem migration.
 *   - network_operators (ISPs, CDNs, enterprise networks): benefit from outcome latitude to tune for their network conditions. Beneficiary position: can deploy different middlebox strategies without breaking TCP endpoints. Power: institutional (control network path). Exit: constrained (must interoperate with endpoints they do not control; cannot unilaterally change the protocol).
 *   - end_users: benefit indirectly from optimization latitude (faster, more responsive networks). Beneficiary position: receive adaptive performance. Power: powerless (passive consumers of TCP service). Exit: trapped (cannot choose TCP implementation or network path).
 *   - strict_invariance_advocates (excluded): argue exact state-machine replication necessary for safety. Power: powerful (include academic institutions, some regulators). Exit: constrained (their position is incompatible with this reading's authority; they advocate for amendment but do not control standards process).
 *   - middlebox_realism_advocates (excluded): argue RFC authority subordinate to deployed network. Power: moderate (include network operators, researchers). Exit: constrained (excluded from this reading's framing; present in amendments and dispute resolution).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.18).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Outcome-Based Coordination with Implementation Flexibility").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network/standards/distributed-systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '5b806339-4b52-4681-9852-64a06c219d0f').
narrative_ontology:cs_kernel_codification('5b806339-4b52-4681-9852-64a06c219d0f', fixed_text).
narrative_ontology:cs_authority_grounding('5b806339-4b52-4681-9852-64a06c219d0f', expertise).
narrative_ontology:cs_interpretation_layer_present('5b806339-4b52-4681-9852-64a06c219d0f').
narrative_ontology:cs_reading_relation('5b806339-4b52-4681-9852-64a06c219d0f', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b806339-4b52-4681-9852-64a06c219d0f', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('5b806339-4b52-4681-9852-64a06c219d0f', foundational, outcome_specification_sufficient_for_interoperability).
narrative_ontology:cs_axiom_status(outcome_specification_sufficient_for_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('5b806339-4b52-4681-9852-64a06c219d0f', outcome_specification_sufficient_for_interoperability, empirically_contingent).
narrative_ontology:cs_axiom('5b806339-4b52-4681-9852-64a06c219d0f', secondary, implementation_latitude_enables_performance_innovation).
narrative_ontology:cs_axiom_status(implementation_latitude_enables_performance_innovation, holdable).
narrative_ontology:cs_axiom_grounding('5b806339-4b52-4681-9852-64a06c219d0f', implementation_latitude_enables_performance_innovation, instrumental).
narrative_ontology:cs_reference_frame('5b806339-4b52-4681-9852-64a06c219d0f', outcome_based_semantic_contract).
narrative_ontology:cs_drift_state('5b806339-4b52-4681-9852-64a06c219d0f', contemporary_multi_algorithm_deployment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5b806339-4b52-4681-9852-64a06c219d0f', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, protocol_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, end_users).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, semantic_interoperability_without_implementation_prescriptivism).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, performance_optimization_compatible_with_protocol_contract).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% TCP implementers (OS kernels, user-space stacks like QUIC, embedded systems) benefit from outcome-based specification: RFC 9293 defines what the protocol DOES (reliable in-order delivery) but not HOW to achieve it internally. This latitude enables them to swap congestion-control algorithms (BBR vs. DCTCP vs. Reno), tune buffer management, and deploy hardware acceleration without rewriting the entire stack or breaking interoperability with others. Their exit is migration to alternative protocols or custom implementations; the freedom to optimize within semantic bounds makes the specification durable rather than constraining.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_implementers, beneficiary,
    organized, generational, mobile, global).

% Operators deploy middleboxes (NATs, firewalls, load balancers, traffic shapers) that observe TCP flows. The specification's latitude on implementation details means operators can optimize for their network conditions (congestion patterns, hardware constraints, traffic mix) without breaking backward compatibility. They benefit from tunable performance within a stable contract. Their constraints are regulatory (RFCs are consensus-driven, not operator-mandated) and technical (must remain interoperable with endpoints they do not control).
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).

% Users experience the protocol as a transparent service: TCP appears as a reliable delivery guarantee regardless of internal congestion-control algorithm or buffer strategy. Outcome-based specification benefits them because implementers can optimize for their use case (low-latency gaming, bulk transfer, interactive sessions) without the specification forcing a one-size-fits-all approach. They have no direct exit: they use whatever TCP implementation the OS provides.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, end_users, beneficiary,
    powerless, immediate, trapped, global).

% IETF sets and maintains the RFC through consensus-based process. This reading instantiates their choice to define TCP by outcome contract rather than prescriptive state machine. They administer the specification, field new algorithm proposals, and resolve interoperability disputes through amendment. Their power is norm-setting: they cannot compel adoption, but widespread implementation of RFC-compliant behavior creates de facto standardization. Exit for implementers is costly (protocol forking) but possible.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, standards_body, agenda_setter,
    institutional, generational, analytical, global).

% Some researchers, regulators, and conservative implementers argue for exact state-machine replication to guarantee global interoperability. They view optimization latitude as a source of subtle incompatibilities and prefer prescriptive specification. They are excluded from the outcome-based reading's authority structure because this reading's legitimacy rests on trusting outcome equivalence as sufficient; strict-invariance advocates would dispute that trust. Their objections are present (academic papers, regulatory testimony) but structurally subordinate to the dominant outcome-based framing.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_advocates, excluded,
    powerful, generational, constrained, global).

% A constituency arguing that RFC 9293 describes idealized endpoint behavior but deployed networks are shaped by actual middlebox populations (NATs that break certain flag combinations, firewalls that enforce packet timing, content-delivery networks that rewrite TCP options). They contend the specification's authority is subordinate to what the network actually does. They are excluded from this reading because this reading's core premise is that semantic specification IS sufficient; middlebox-realism advocates deny that sufficiency. Their perspective informs bug reports and amendments but does not set the reading's boundary.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_realism_advocates, excluded,
    moderate, generational, constrained, global).

% Independent test suites (like TPKT, RFC test vectors) verify that implementations respect the outcome contract. Testers observe and validate that diverse TCP stacks (Linux, Windows, macOS, embedded) interoperate despite internal differences. Their role is measurement and certification, not rule-setting. They validate the premise that outcome-based specification suffices for interoperability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_testing_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, protocol_implementers).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: RFC 9293 solves the distributed-systems coordination problem: heterogeneous endpoints (OS kernels, embedded systems, mobile devices, specialized hardware) need a shared protocol for reliable ordered delivery across untrusted networks. The specification coordinates by defining the semantic contract (what happens) rather than the implementation (how it happens), allowing each endpoint to optimize for its constraints while maintaining interoperability with all others.
% TRANSFER_FUNCTION: The specification transfers authority from prescriptive implementation dictates to outcome-based performance guarantees. Implementers receive freedom to allocate engineering resources to optimization within semantic bounds; the standards body receives the power to define 'semantic bounds' and amend them; end users receive performance that adapts to their hardware and use case without incompatibility risk.
% ABSENT_VOICES: Strict-invariance advocates are structurally excluded: their position (exact state-machine replication necessary for safety) is incompatible with this reading's core premise (outcome equivalence suffices). Middlebox-realism advocates are also excluded: they would argue the specification's authority is subordinate to deployed network behavior. Neither group controls the standards process, though their concerns shape amendments and bug reports.
% DISAPPEARANCE_RATIONALE: If RFC 9293's outcome-based latitude disappeared overnight and the IETF rewrote TCP as a prescriptive state machine, implementers would face a massive rewrite to match exact behavior, many existing optimizations would violate the spec, and fragmentation would accelerate (custom forks, side protocols, protocol replacements like QUIC gaining adoption faster). The network would not break, but optimization velocity would drop and complexity would rise. Performance-critical deployments would migrate to newer protocols faster, and the TCP ecosystem would shrink.
% FOUNDING_PROBLEM: Early TCP specifications (RFC 793, RFC 1122) were prescriptive but incomplete: they left gaps on congestion control, retransmission timing, and state-machine edge cases, which led to implementer divergence and subtle incompatibilities. By the 1990s, the Internet faced congestion collapse because different TCP variants competed for bandwidth and some were highly aggressive. The founding problem was: how to specify TCP precisely enough to prevent global instability, but flexibly enough to allow innovation in algorithms that do not violate the semantic contract?
% FOUNDING_PROBLEM_CORROBORATION: Multiple independent sources corroborate this status: (1) RFC 2581 (1999) explicitly identified congestion collapse as a live threat and specified mandatory Tahoe/Reno minimum behavior; (2) subsequent RFCs (2988, 3168, 5681, 9293) continually refine the minimum semantics while preserving latitude for algorithm innovation (BBR, DCTCP, now deployed in major OSes); (3) independent academic literature on TCP variant interoperability (NIST papers, university network labs) documents the founding problem as ongoing — new algorithms must be validated against the semantic contract to ensure they do not break global congestion balance. The problem is not solved; it is managed through outcome-based specification.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.18 at end) because the specification's value flows to all parties: implementers get optimization freedom (no extraction from them), operators get adaptability (no extraction from them), users get performance (no extraction from them), standards body gets authority to set semantic bounds (not extraction, but norm-setting power). There is no party paying for the other's benefit; it is genuine mutual benefit. Suppression is VERY LOW (0.08) because the constraint's persistence depends on voluntary adoption and interoperability validation, not coercive enforcement. Implementers comply because the specification is incentive-compatible (better for them than fragmentation). Theater is MINIMAL (0.05) because the specification's function is real: it actually coordinates distributed systems. Small theater ratio reflects some management overhead (amendments, testing infrastructure, dispute resolution) but little performative activity. Accessibility collapse is HIGH (0.72) because once the outcome-based contract is understood, the alternatives are limited: implementers must choose between the RFC semantics or forking (high cost); operators must choose between interoperability or network isolation (unattractive); users have no choice at all. Resistance is MODERATE (0.42) because strict-invariance advocates and middlebox-realism advocates mount real intellectual and institutional resistance to this reading, published in peer review and standards debates, even though the outcome-based framing is dominant. The measurement series shows extractiveness and suppression rising slightly over 40 years (interval represents 1983–2023), reflecting gradual accumulation of algorithmic variants and testing overhead, but remaining low and stable — the core coordination function has not degraded into extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the implementer, operator, and user seats, this reading is genuine coordination enabling innovation (Rope, high beneficiary d, low χ). From the strict-invariance seat, the same reading is extractive foreclosure of their alternative (the outcome-based framing rules out exact-replication as interoperability strategy). From the middlebox-realism seat, the reading is a false authority claim (subordinates network behavior to specification text). The engine should compute per-seat type: beneficiary seats see Rope; excluded seats see Snare (foreclosure without exit). This divergence is structurally true and is exactly what per-seat classification is designed to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   See perspectival_gap above. All three beneficiary groups sit at beneficiary pole (d ≈ 0.0 to 0.2). Standards body sits near symmetric (d ≈ 0.5): sets rules but does not extract. Excluded strict-invariance and middlebox-realism advocates sit at target pole (d ≈ 0.8 to 1.0) because this reading's existence extracts their epistemic authority — the outcome-based framing is incompatible with their core premises.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (prevent congestion collapse while enabling innovation) remains live and is actively corroborated by (a) published literature on new TCP variants (BBR, DCTCP), (b) deployment records showing these algorithms in production without global instability, (c) continued IETF amendments to RFC 9293 accommodating new algorithms. The disappearance verdict is world_rearranges: removing the outcome-based specification would force either strict-invariance compliance (massive rewrite, performance loss) or protocol fragmentation (new forks for each optimization approach). The constraint has not resolved its founding problem; it is actively managing it. No mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcome_equivalence_sufficiency,
    'Is outcome-based specification (semantic contract: reliable delivery) sufficient for global interoperability across diverse implementations, or do subtle algorithmic differences create incompatibilities that require prescriptive state-machine specification?',
    'Long-term empirical observation of TCP ecosystem: if diverse implementations (BBR, DCTCP, Reno, Cubic, Hybla, etc.) coexist without widespread incompatibilities for 20+ years, outcome-equivalence is sufficient; if incompatibilities accumulate faster than amendment, prescriptive specification would be vindicated.',
    'If outcome-equivalence suffices, this reading is Rope (genuine coordination); if it fails, the constraint degrades toward Tangled Rope (perceived coordination masking hidden incompatibilities). If it fails severely, it becomes Piton (specification is performative, real behavior is determined by deployed network).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outcome_equivalence_sufficiency, empirical, 'Whether outcome contract suffices for interoperability or prescriptive specification is necessary.').

omega_variable(
    reading_foreclosure_mutual_exclusivity,
    'Are the three readings (outcome-based, strict-invariance, middlebox-realism) mutually exclusive within a single implementer''s framework, or can an implementer adopt outcome-based optimization WHILE ALSO respecting strict-invariance constraints AND adapting to middlebox reality?',
    'Structural analysis of implemented TCP stacks (Linux kernel, QUIC, Windows) to determine if they instantiate one reading or a hybrid. If hybrids are common, readings coexist_with rather than foreclose.',
    'If readings coexist, the engine should compute coexists_with relations and allow multiple readings to be simultaneously valid from different organizational seats. If they foreclose, only one reading survives in each implementation context, and the engine should compute Snare-like dynamics (one reading forecloses others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mutual_exclusivity, conceptual, 'Whether the three readings of the RFC kernel are logically incompatible or empirically coexistent in deployed systems.').

omega_variable(
    authority_subordination_vs_coordination,
    'Does the IETF''s authority to specify TCP rest on genuine coordination power (implementers voluntarily adopt the spec because it benefits them) or on subordination of deployer reality to specification text (implementers are forced to comply by interoperability pressure, not choice)?',
    'Empirical: track fork rate and out-of-spec implementations. If implementers freely choose RFC compliance and alternative protocols grow, coordination power is genuine. If implementers are locked in by network effects and forced compliance appears, it shifts toward subordination.',
    'If coordination power is genuine, this reading is Rope; if subordination power is real, it is Tangled Rope or Snare. This omega addresses whether the extracted value (behavioral conformance) is voluntary or coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_subordination_vs_coordination, empirical, 'Whether RFC authority derives from genuine coordination or from network-effect lock-in.').

omega_variable(
    excluded_voices_structural_necessity,
    'Are strict-invariance and middlebox-realism voices excluded because their positions are logically incompatible with outcome-based coordination, or because the standards process has institutionally suppressed them despite their logical validity?',
    'Genealogy: audit IETF archives for strict-invariance and middlebox-realism proposals. If proposals were rejected on merit (logically undermined by deployed evidence), exclusion is structural. If they were suppressed procedurally or by coalition pressure, exclusion is institutional suppression masquerading as structural.',
    'If structural, the excluded voices are genuinely foreclosed and the reading stands. If institutional suppression, the reading may be Snare-like (enforced through governance procedure rather than logical necessity), and re-opening the excluded voices could degrade the constraint''s classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voices_structural_necessity, conceptual, 'Whether excluded voices are logically foreclosed or institutionally suppressed.').

omega_variable(
    performance_optimization_boundary,
    'What is the semantic boundary between outcome-preserving optimization and outcome-altering divergence? At what point does an algorithmic variant (e.g., BBR''s model-based approach vs. Reno''s loss-based approach) stop being an optimization within semantic bounds and start being a different protocol?',
    'Formal specification of the semantic contract: define in machine-readable terms exactly what ''reliable delivery'' and ''flow control'' MEAN such that algorithmic variants can be verified against them. Absent formal specification, the boundary is informal consensus, which is reversible.',
    'If the boundary can be formalized and verified, the outcome-based reading is robust (implementers can certify compliance). If it remains informal, the reading is vulnerable to drift and interpretation disputes, which could degrade it toward Piton (specification becomes performative, actual behavior is determined by deployed network practice rather than RFC authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_optimization_boundary, conceptual, 'Whether the semantic contract can be formalized or remains informal consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(rfc9_tr_t0, observed).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement_basis(rfc9_tr_t5, observed).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement_basis(rfc9_tr_t10, observed).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t20, observed).
narrative_ontology:measurement(rfc9_tr_t30, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t30, observed).
narrative_ontology:measurement(rfc9_tr_t40, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(rfc9_be_t0, observed).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(rfc9_be_t5, observed).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement_basis(rfc9_be_t10, observed).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(rfc9_be_t20, observed).
narrative_ontology:measurement(rfc9_be_t30, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(rfc9_be_t30, observed).
narrative_ontology:measurement(rfc9_be_t40, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(rfc9_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(rfc9_su_t0, observed).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement_basis(rfc9_su_t5, observed).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement_basis(rfc9_su_t10, observed).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t20, observed).
narrative_ontology:measurement(rfc9_su_t30, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t30, observed).
narrative_ontology:measurement(rfc9_su_t40, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% RFC 9293 constraint family: three readings of the same kernel (the TCP specification). Each reading instantiates a different constraint with different beneficiary/victim structures, extracted values, and types. The optimization-latitude reading (this story) treats RFC 9293 as outcome-based coordination enabling innovation (Rope, low extraction). The strict-invariance reading treats it as enforced behavioral conformity (Tangled Rope or Scaffold, moderate extraction). The middlebox-realism reading treats it as subordinate to deployed network behavior (Snare, high extraction). Each reading's ε-value is distinct and referent-fixed: each reading assesses the SAME standing arrangement (the RFC specification as deployed in the Internet) but from different framings. The differences in ε, beneficiary/victim, and type are structural, not observational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__optimization_latitude_reading, powerless, 0.0).
constraint_indexing:directionality_override(rfc9293_tcp_specification__optimization_latitude_reading, organized, 0.15).
constraint_indexing:directionality_override(rfc9293_tcp_specification__optimization_latitude_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
