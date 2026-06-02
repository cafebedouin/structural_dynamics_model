% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Specification: Optimization Latitude Reading
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 is the IETF standard specifying the Transmission Control
 *   Protocol. The specification defines what TCP MUST do (deliver bytes in
 *   order, respond to congestion signals, retransmit lost segments) but is
 *   deliberately permissive about HOW implementations achieve these outcomes.
 *   This constraint story instantiates the optimization_latitude_reading: the
 *   specification's semantic binding creates a coordination mechanism (all
 *   implementations must produce the correct output stream) while its
 *   implementation latitude enables performance optimization (each
 *   implementer can choose congestion control algorithms, buffer sizes, and
 *   retransmission policies within semantic bounds). This reading coexists
 *   with two sibling readings: the strict_invariance_reading (which claims
 *   RFC 9293 is a complete, fully-specified protocol admitting no variance)
 *   and the middlebox_realism_reading (which acknowledges that real network
 *   operators cannot enforce semantic compliance universally, and thus
 *   optimization latitude is constantly threatened by middlebox
 *   interference). The optimization_latitude_reading treats the specification
 *   as a coordination mechanism (Rope) that enables both semantic
 *   interoperability and implementation diversity. It is the dominant reading
 *   in the implementer community and justifies the proliferation of TCP
 *   variants (BBR, DCTCP, CUBIC) that all pass RFC 9293 conformance tests
 *   while optimizing for different network conditions.
 *
 * KEY AGENTS:
 *   - Implementer Community (organized/mobile): Primary beneficiary — freedom to optimize within semantic bounds enables BBR, DCTCP, and other variants to coexist. Captures performance gains without forcing network-wide changes.
 *   - Cloud Infrastructure Providers (institutional/arbitrage): Primary beneficiary — deploy optimized TCP in data centers while maintaining interoperability with external networks. Massive efficiency gains.
 *   - IETF/Protocol Stewards (powerful/arbitrage): Authority/beneficiary — specification's flexibility preserves the standard's legitimacy by binding to outcomes rather than implementation path. Reduces revision pressure.
 *   - Middlebox Operators (moderate/constrained): Secondary victim — face constrained exit if implementations diverge in ways that expose middlebox vulnerabilities. Absorb interoperability risk.
 *   - Academic/Industrial Research Teams (organized/constrained): Users of specification flexibility — experiment with novel TCP variants; implicit sunset when variants mature or obsolete.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective — risks naturalizing a design choice (outcome-binding specification) as an invariant principle of distributed systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.18).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Optimization Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, 'd7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d').
narrative_ontology:cs_kernel_codification('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', formalized).
narrative_ontology:cs_authority_grounding('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', expertise).
narrative_ontology:cs_interpretation_layer_present('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d').
narrative_ontology:cs_reading_relation('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', foundational, outcome_binding_enables_implementation_diversity).
narrative_ontology:cs_axiom_status(outcome_binding_enables_implementation_diversity, holdable).
narrative_ontology:cs_axiom_grounding('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', outcome_binding_enables_implementation_diversity, instrumental).
narrative_ontology:cs_axiom('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', foundational, semantic_compliance_is_verifiable).
narrative_ontology:cs_axiom_status(semantic_compliance_is_verifiable, holdable).
narrative_ontology:cs_axiom_grounding('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', semantic_compliance_is_verifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', rfc9293_as_semantic_contract).
narrative_ontology:cs_drift_state('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', contemporary_tcp_implementation_diversity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d7e4c1f2-9a8b-4c6e-8d3a-2f5b7e9c1a4d', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, implementers_seeking_performance_optimization).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, edge_network_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPLEMENTER COMMUNITY (ROPE) — Organized implementers (Linux, Windows, FreeBSD, cloud vendors) experience the specification as enabling coordination on the reliable byte stream contract while preserving freedom to optimize congestion control algorithms, buffer management, and retransmission policies. Mobile exit: implementers can adopt or abandon specific optimizations within the bounds of RFC 9293 semantics. Low extraction because the specification coordinates outcomes without dictating implementation path — each implementer captures optimization gains. This is the primary beneficiary perspective.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTOCOL STEWARDSHIP (POWERFUL/ARBITRAGE) (ROPE) — IETF/IANA experience the specification as a coordination device: RFC 9293 defines semantic bounds (in-order delivery, congestion response, retransmission logic) while explicitly permitting implementation variance. This reading preserves the standard's authority by binding to outcomes rather than implementation path. Exit: IETF has institutional arbitrage (can revise or supersede), but the strategy is to maintain loose coupling. Experiences constraint as low-extraction coordination — the standard's legitimacy derives from enabling diverse implementations to interoperate, not from controlling implementation details.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MIDDLEBOX OPERATOR (TANGLED ROPE) — Network operators managing firewalls, NAT devices, and TCP proxies experience mixed coordination and extraction. The optimization latitude reading is benign from their perspective: as long as TCP behavior remains semantically compliant (packets flow in order, congestion signals are honored), middleboxes can remain agnostic to implementation details. However, they face constrained exit: if implementations diverge significantly (novel congestion control exploiting RFC 9293 semantics in unexpected ways), middleboxes cannot easily update — they must continue passing packets without deep packet inspection. The constraint shows both coordination (shared protocol semantics) and asymmetric cost (middleboxes absorb risk of interoperability failures from divergent implementations).
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CLOUD INFRASTRUCTURE (INSTITUTIONAL/ARBITRAGE) (ROPE) — Major cloud providers (AWS, Google Cloud, Azure) experience the optimization latitude reading as pure coordination with massive benefits. They can deploy optimized TCP variants (BBR, DCTCP) in data centers while remaining interoperable with external networks that run standard implementations. The specification's flexibility enables them to capture efficiency gains (lower latency, higher throughput) without forcing network-wide changes. Exit: institutional arbitrage — they can return to standard implementations if optimization becomes costly. Low extraction: the constraint enables value creation rather than extraction.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the optimization latitude reading reflects a fundamental structural principle: a protocol specification that binds to observable outcomes (reliable in-order byte delivery) rather than implementation path is invariant to optimization. Any implementation producing the correct output stream for a given input stream satisfies the contract. This appears as a natural law of distributed systems: if you want semantic interoperability without performance ossification, you must specify interfaces, not implementations. However, this perspective risks false-summit naturalization — it treats what is actually a design choice (outcome-binding vs implementation-binding) as inevitable law.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: EXPERIMENTAL RESEARCH COMMUNITY (SCAFFOLD) — Academic and industry research teams (QUIC, DCCP migration, TCP enhancement initiatives) use the optimization latitude in RFC 9293 as a temporary support structure for experimentation. The specification permits research variants (e.g., TCP-FRR, TCP-Westwood, BBR evolution) to be tested without protocol obsolescence. The constraint has an implicit sunset: as research variants mature and prove beneficial at scale, the specification may evolve to codify the winning implementations or retire obsolete ones. Constrained exit: research groups cannot unilaterally change the standard, but they can migrate findings into new RFCs or new protocol generations (QUIC). Theater is moderate: the experimental scaffolding is functionally real, but it eventually gives way to standardization or retirement.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__optimization_latitude_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

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
 *   Extractiveness (0.18): Low. This reading treats the specification as a pure coordination mechanism. The semantic contract (reliable in-order delivery) is binding on all implementations, but the implementation path is flexible. Implementers benefit from optimization freedom; no agent bears significant extraction cost. The specification does not extract from implementers or restrict their options — it enables them. The small non-zero value (not 0.0) reflects that the specification does constrain what implementations can do: they must remain semantically compliant, and compliance verification is a real cost. But this cost is symmetrical (all agents bear it) and functional (necessary for coordination), so it doesn't count as extraction proper. Suppression (0.08): Very low. No agent is suppressed or prevented from implementing TCP variants. The specification explicitly permits variance within semantic bounds. There are no legal, economic, or institutional barriers to implementation diversity. Suppression would be relevant if the specification forced all implementations into a single path, but it does not. Theater ratio (0.25): Low. The coordination function is real and non-performative: semantic compliance is verifiable through conformance testing, and implementations either pass or fail. The small non-zero value reflects that RFC 9293 does include some procedural elements (header format specifications, state machine definitions) that could be read as more prescriptive than necessary. But the core function (outcome binding) is functionally substantive, not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The optimization_latitude_reading produces Rope classifications across all beneficiary/implementer perspectives and an institutional (IETF) perspective. The middlebox operator perspective shifts to Tangled Rope because middleboxes face constrained exit: they cannot easily update to handle novel TCP variants. The analytical observer risks seeing this as Mountain (a natural law of distributed systems), but the structural data reveals it as a design choice — the specification could have been more rigid, and some protocols are. The gap reveals the reading's core claim: the specification is a coordination device, not a restriction device. Implementers experience freedom and benefit; stewards experience legitimacy and reduced revision pressure; only those trapped in inflexible infrastructure (middleboxes) experience asymmetric cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The optimization_latitude_reading derives directionality from beneficiary/victim declarations and exit options. Implementers are beneficiaries with mobile exit — they can adopt or abandon optimizations. They experience low directionality d (around 0.15–0.20), yielding low effective extraction chi. Cloud providers are institutional beneficiaries with arbitrage exit — they experience even lower directionality (0.05–0.10) and often negative chi (institutional context canonically near -0.12). Middlebox operators are neither clear beneficiaries nor clear victims (they don't innovate within TCP; they pass packets), but the constraint creates constrained exit because they cannot easily adapt if implementations diverge unexpectedly. Their directionality is moderate (0.50–0.55), yielding moderate chi through the sigmoid. The IETF steward perspective is institutional with arbitrage, yielding institutional canonical d (~0.00), and very low chi. No directionality overrides are needed — the structural data produces the right shape.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy: extractiveness is 0.18 (well below the 0.70 threshold for mandatrophy resolution). The rope classification is stable across all perspectives. The only potential mandatrophy emerges if the analytical observer's mountain view is taken seriously — but the structural data (omega variables on semantic compliance boundary, kernel reading disambiguation) routes this back to design choice rather than natural law. The constraint resolves the seeming paradox that RFC 9293 can be simultaneously 'rigid' (strict reading) and 'flexible' (optimization reading): it achieves this by binding to outcomes, not implementation path. This is not paradoxical — it is the core insight of the optimization_latitude_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_compliance_boundary,
    'Where exactly does the optimization latitude boundary lie? What behavioral deviations from RFC 9293 semantics are still permissible under the ''in-order delivery'' contract?',
    'Empirical testing of non-standard implementations against RFC 9293 conformance test suites; analysis of failed interoperability cases (e.g., middlebox incompatibilities with novel congestion control) to identify violated semantic bounds',
    'If boundary is tight (strict semantics enforcement): constraint shifts toward Piton or Mountain (rigid specification). If boundary is loose (wide optimization latitude): constraint remains Rope but with higher theater (unclear enforcement criteria).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_compliance_boundary, empirical, 'Definition of semantic compliance boundary for optimization latitude').

omega_variable(
    kernel_reading_disambiguation,
    'This reading (optimization_latitude_reading) claims the RFC 9293 kernel can be interpreted as outcome-binding rather than implementation-binding. Does this reading coexist with the strict_invariance_reading (which insists RFC 9293 is a complete, invariant specification permitting no variance) or does it foreclose it?',
    'Textual analysis of RFC 9293 language: does the specification explicitly permit implementation variance or only implicitly allow it? If explicit, readings coexist; if implicit, the strict reading can claim the specification is complete and the optimization reading misreads it.',
    'If coexist: both readings remain live, and the network of TCP implementations exhibits heterogeneity as a legitimate choice. If foreclose: strict reading dominates, and optimization latitude is deviation/non-compliance. This determines whether the constraint is a coordination mechanism or a violation mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether optimization_latitude_reading coexists with or forecloses strict_invariance_reading').

omega_variable(
    middlebox_realism_pressure,
    'Does the middlebox_realism_reading (acknowledging that real network operators cannot enforce semantic compliance universally) create sufficient structural pressure to force the optimization_latitude_reading to concede constraints it claims to avoid?',
    'Case study of TCP deployments exhibiting middlebox-induced semantic violations (e.g., NAT incompatibility with timestamp options, firewall blocking of TCP option extensions). Measure frequency and impact.',
    'If middlebox pressure is severe: optimization_latitude_reading''s assumption (that implementations can vary while preserving semantics) breaks down — the constraint shifts from Rope toward Tangled Rope or Snare. If middlebox pressure is manageable: optimization_latitude_reading remains valid Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_realism_pressure, empirical, 'Whether middlebox realism creates structural pressure on optimization latitude reading').

omega_variable(
    rfc9293_natural_law_vs_design_choice,
    'The analytical perspective (mountain view) treats outcome-binding specification as a natural law of distributed systems. Is this genuine natural law (reflecting information-theoretic or logical limits) or a design choice (reflecting human preferences for flexibility)?',
    'Comparative analysis: examine protocols that bind to implementation (rare; e.g., hardware specifications) vs those that bind to outcomes (common; e.g., HTTP, DNS). Assess whether strict implementation-binding protocols achieve superior semantic invariance or whether they merely shift costs to implementation maintainers.',
    'If natural law: mountain classification is legitimate, and optimization latitude is inevitable. If design choice: mountain is a false summit — naturalization of a contingent institutional decision. This affects mandatrophy resolution at the civilizational time horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rfc9293_natural_law_vs_design_choice, conceptual, 'Whether outcome-binding specification is natural law or design choice').

omega_variable(
    optimization_convergence_hypothesis,
    'Does the ecosystem of TCP implementations tend to converge on common optimization strategies (as the scaffold/research perspective predicts with a sunset) or diverge into incompatible variants?',
    'Longitudinal analysis of TCP implementation landscape: BBR adoption rate, DCTCP deployment patterns, Westwood/Vegas survival, variance in RTT estimation and retransmission timing across kernel versions and cloud vendors. Measure divergence/convergence trend over 10-year window.',
    'If converge: scaffold perspective confirmed, optimization latitude is temporary support. If diverge: latitude is permanent structural feature, constraint remains Rope indefinitely. Affects theater_ratio trajectory and piton classification risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_convergence_hypothesis, empirical, 'Whether TCP implementations converge or diverge on optimizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_optlat_theater_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(rfc9293_optlat_theater_t8, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(rfc9293_optlat_theater_t16, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 16, 0.28).

% Extraction over time
narrative_ontology:measurement(rfc9293_optlat_extractiveness_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(rfc9293_optlat_extractiveness_t8, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 8, 0.18).
narrative_ontology:measurement(rfc9293_optlat_extractiveness_t16, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 16, 0.21).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, tcp_congestion_control_algorithm_ecosystem).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_compatibility_asymmetry).

% DUAL FORMULATION NOTE:
% RFC 9293 is a kernel that grounds three structurally distinct constraints. The optimization_latitude_reading treats it as Rope (outcome-binding coordination enabling implementation diversity). The strict_invariance_reading treats it as a complete specification, shifting toward Mountain or Piton if implementations diverge. The middlebox_realism_reading treats the practical protocol as defined by lowest-common-denominator middlebox behavior, shifting toward Tangled Rope or Snare. These are separate constraint stories, linked by kernel identity. Each has its own ε, perspectives, and classifications. The network edges record structural influence: the optimization_latitude_reading influences the middlebox_realism_reading (if implementations diverge significantly, middlebox operators face pressure). Together, the three readings form a complete diagnostic for protocol specification under conditions of implementation heterogeneity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
