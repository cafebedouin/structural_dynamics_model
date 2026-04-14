% ============================================================================
% CONSTRAINT STORY: tcp_congestion_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_congestion_control, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tcp_congestion_control
 *   human_readable: TCP Congestion Control as Coordination and Extraction
 *   domain: networking/infrastructure/protocol_design
 *
 * SUMMARY:
 *   TCP congestion control is a foundational constraint on internet
 *   infrastructure that exhibits genuine coordination and genuine extraction
 *   simultaneously. Deployed in 1988 (Van Jacobson's Tahoe algorithm) to
 *   prevent network collapse during the first congestion catastrophe, it
 *   represents a negotiated limit on sending rates proportional to available
 *   bandwidth. From the perspective of network operators and well-provisioned
 *   CDNs, it is a coordination mechanism that prevents overload and reduces
 *   infrastructure costs. From the perspective of latency-sensitive
 *   applications (real-time trading, IoT devices, video conferencing) and
 *   bandwidth-constrained users, it is an extraction mechanism that
 *   deliberately reduces throughput and increases latency. The constraint's
 *   persistence despite superior alternatives (QUIC, proprietary transport
 *   protocols) reflects institutional lock-in: TCP is baked into operating
 *   system kernels, middleboxes, and application assumptions. The
 *   extractiveness value (0.38) reflects that the mechanism achieves genuine
 *   coordination (preventing collapse) but with asymmetric costs borne by
 *   flows that cannot benefit from bulk transfer. The theater ratio (0.35)
 *   reflects that congestion control's original function (preventing network
 *   meltdown) remains real, though its mechanisms are increasingly mistuned
 *   for modern networks with heterogeneous link qualities, deep buffering,
 *   and application diversity.
 *
 * KEY AGENTS:
 *   - Backbone Network Operator: Primary beneficiary (institutional/arbitrage) — benefits from congestion control's network stability and reduced infrastructure cost
 *   - Content Delivery Network: Institutional beneficiary (institutional/mobile) — optimizes TCP variants and can deploy QUIC; experiences constraint as tunable coordination
 *   - Latency-Sensitive Application: Primary victim (powerless/trapped) — real-time trading, video conferencing, IoT; forced to accept degradation with no negotiation
 *   - Throughput-Constrained User: Secondary victim (moderate/constrained) — experiences both coordination benefit and extraction; could adopt QUIC but faces ecosystem barriers
 *   - Protocol Evolution Coalition: Organized coalition (organized/constrained) — IETF working groups, standards bodies, browser vendors building QUIC alternatives with sunset horizon
 *   - Legacy Middlebox: Institutional actor in decline (institutional/arbitrage) — maintains TCP compatibility through inertia; increasingly bypassed by QUIC and encryption
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees both genuine coordination and genuine extraction; identifies lock-in preventing displacement by superior protocols
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_congestion_control, 0.38).
domain_priors:suppression_score(tcp_congestion_control, 0.42).
domain_priors:theater_ratio(tcp_congestion_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_congestion_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(tcp_congestion_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tcp_congestion_control, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_congestion_control, tangled_rope).
narrative_ontology:human_readable(tcp_congestion_control, "TCP Congestion Control as Coordination and Extraction").
narrative_ontology:topic_domain(tcp_congestion_control, "networking/infrastructure/protocol_design").

domain_priors:requires_active_enforcement(tcp_congestion_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_congestion_control, backbone_operators).
narrative_ontology:constraint_beneficiary(tcp_congestion_control, content_delivery_networks).
narrative_ontology:constraint_victim(tcp_congestion_control, latency_sensitive_applications).
narrative_ontology:constraint_victim(tcp_congestion_control, throughput_constrained_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATENCY-SENSITIVE APPLICATION (SNARE) — Cannot exit TCP without losing reliability guarantees. IoT devices, real-time trading platforms, and interactive applications are trapped by the requirement for ordered delivery. Congestion control deliberately backs off when latency increases, starving real-time flows. The constraint suppresses alternatives (QUIC adoption is slow; UDP unreliability is unacceptable for many use cases). Maximum extraction experienced — the application must accept degradation with no negotiation.
constraint_indexing:constraint_classification(tcp_congestion_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THROUGHPUT-CONSTRAINED USER (TANGLED ROPE) — Locked into TCP by OS defaults and application design, but benefits from congestion control's network stability. The constraint both coordinates (prevents network collapse) and extracts (deliberately backs off sending, reducing individual throughput by 30-40% during congestion). Exit options exist (QUIC, custom protocols) but carry switching costs and loss of ecosystem support. Significant extraction but not maximal — some coordination benefit acknowledged.
constraint_indexing:constraint_classification(tcp_congestion_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BACKBONE NETWORK OPERATOR (ROPE) — Primary beneficiary (institutional/arbitrage). Congestion control coordinates shared link utilization by throttling senders voluntarily. The operator experiences the constraint as a coordination solution: it reduces their infrastructure costs, prevents buffer overflow, and avoids aggressive escalation wars that would degrade service for all users. Can exit (deploy QoS policies, enforce rate limits, update infrastructure) but these alternatives cost more than relying on TCP's built-in cooperation. Net beneficiary — the constraint extracts cooperation from senders for the operator's stability.
constraint_indexing:constraint_classification(tcp_congestion_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT DELIVERY NETWORK (ROPE) — Benefits from congestion control by reducing infrastructure cost and network congestion, enabling better average user experience. Modern CDNs (Akamai, Cloudflare) have mobile exit options: they can tune TCP window sizes, implement custom congestion algorithms (BBR, CUBIC variants), deploy edge caching, and shift to QUIC. The constraint appears as coordination with substantial room for optimization. Experiences cooperation mechanism, not extraction.
constraint_indexing:constraint_classification(tcp_congestion_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL EVOLUTION COALITION (SCAFFOLD) — IETF working groups, standards bodies, and browser vendors (organized/constrained) see TCP congestion control as a temporary coordination mechanism with a sunset clause. QUIC, DCCP, and transport innovations are building alternatives that combine reliability with lower latency. Exit is constrained by installed base lock-in and slow adoption curves, but the coalition has agency and sees an exit path. Sunset horizon: 10-15 years for QUIC to become the default transport for new applications. Theater ratio is low because the congestion control mechanism solves a real coordination problem; the theater will rise only if the protocol persists after its functional need declines.
constraint_indexing:constraint_classification(tcp_congestion_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY MIDDLEBOX (PITON) — Firewalls, NAT devices, and load balancers that depend on TCP's sequential processing model are increasingly theatrical — they inspect packet streams that QUIC encrypts, perform actions (connection reset, rate limiting) that QUIC makes transparent to end-to-end encryption. Legacy middleboxes maintain TCP compatibility through institutional inertia rather than functional necessity. New deployments use QUIC passthrough or explicit signaling. The theater ratio reflects that congestion control's original function (avoiding overload collapse) is now partially redundant for well-provisioned modern networks; congestion control persists because replacing it globally requires perfect coordination.
constraint_indexing:constraint_classification(tcp_congestion_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal scope, TCP congestion control is a hybrid mechanism: it genuinely coordinates shared link utilization (rope function), but it also implements asymmetric extraction by constraining high-bandwidth applications and latency-sensitive flows while favoring steady-state bulk transfer. The constraint's persistence despite superior alternatives (QUIC, proprietary protocols) reflects institutional lock-in rather than optimality. Classification remains tangled_rope because both functions (coordination and extraction) are structurally necessary — removing either would degrade the system.
constraint_indexing:constraint_classification(tcp_congestion_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_congestion_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tcp_congestion_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_congestion_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(tcp_congestion_control, TR),
    TR >= 0.70.

:- end_tests(tcp_congestion_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. TCP congestion control achieves its primary goal of preventing network collapse and coordinates shared link utilization — this is genuine coordination, not pure extraction. However, the mechanism is asymmetric: flows that cannot adjust their sending rate (interactive applications, IoT devices) bear disproportionate cost compared to bulk transfer flows that can tolerate latency. The value reflects that significant extraction occurs but is balanced against real coordination benefits. Over the measurement interval, extractiveness increased from 0.20 to 0.38 as application workloads diversified — earlier TCP assumed mostly bulk transfer, but modern applications (video streaming, real-time comms, gaming) suffer more from congestion control backoff. Suppression (0.42): Moderate. Multiple barriers prevent escape: TCP is embedded in OS kernels (exit cost is high for applications), QUIC adoption is slow (network effects favor incumbents), and middlebox interference makes alternative protocols unreliable. But suppression is not total — QUIC deployment exists, custom transport layers are possible, and new applications can design around TCP. Theater ratio (0.35): Low-moderate. The congestion control mechanism solves a real problem (preventing collapse), so there is genuine functional content. Theater has increased slightly over the interval as the actual problem (preventing all-out sending races) has become less acute with provisioning improvements, but the protocol persists for lock-in reasons. The theater ratio remains below 0.5 because congestion control still prevents real harm — middleboxes that depend on inspecting TCP flows score higher theater (piton perspective) because their function is increasingly vestigial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The backbone operator (institutional/arbitrage) sees the constraint as pure coordination (rope) — it solves their infrastructure problem elegantly and costs them nothing to enforce (senders voluntarily back off). The latency-sensitive application (powerless/trapped) sees pure extraction (snare) — it is forced to accept degradation with no negotiation and no exit option. The protocol coalition (organized/constrained) sees a temporary problem with a known exit (scaffold) — QUIC and other transports are building alternatives with a realistic sunset timeline. The legacy middlebox (institutional/arbitrage) sees a degraded ritual (piton) — it depends on TCP's sequential nature, which QUIC encrypts away, forcing its own redundancy. The gap between operator's rope and application's snare is the gap between who benefits and who bears cost. The gap between scaffold and piton reflects different timeframes: the coalition sees a sunset mechanism (10-15 years), while the middlebox sees indefinite persistence through inertia. The analytical observer sees tangled_rope because both functions (coordination and extraction) are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: network operators benefit from the constraint (low d, negative f(d)), latency-sensitive applications are victimized (high d, high f(d)), and moderate actors experience mixed cost-benefit (mid-range d, moderate f(d)). The beneficiary (backbone operator) with arbitrage exit options derives d ≈ 0.15, producing negative effective extraction because they can tune or exit if the constraint becomes unfavorable. The victim (latency-sensitive application) with trapped exit derives d ≈ 0.90, producing high f(d) and high experienced extraction because they have no alternative. The moderate actor (throughput-constrained user) with constrained exit derives d ≈ 0.55, producing moderate f(d) because they could exit (switch to QUIC, use UDP) but face significant switching costs. The analytical observer derives d ≈ 0.72 (between victim and beneficiary) because the constraint's extractiveness is genuinely bidirectional — it coordinates for some, extracts from others.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by refusing false unification: it is not 'really' coordination masked as extraction, nor 'really' extraction masked as coordination. Both characterizations are structurally accurate from different positions. The backbone operator's rope perspective is not wrong — the operator genuinely does solve a collective action problem. The latency-sensitive application's snare perspective is not wrong — the application genuinely is forced to degrade with no exit. The tangled_rope classification accommodates both truths: the mechanism coordinates shared link utilization (genuine rope function) while implementing asymmetric extraction (genuine snare function). The mandatrophy is resolved by showing that the constraint serves two structurally different purposes for two structurally different agents. Mandatrophy would arise if we tried to claim the constraint 'is really' one type — the data demand both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congestion_signal_reliability,
    'Are packet loss and round-trip time increase reliable signals of actual network congestion, or do they reflect middlebox interference, buffering heterogeneity, and wireless link volatility?',
    'Cross-ISP studies comparing congestion signal correlation with actual queue depth; analysis of false positive rates in cellular vs wired networks',
    'If signals are reliable: congestion control is primarily coordination (rope). If highly noisy: much of the backed-off sending is extraction based on false signals; classification shifts toward snare for affected flows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_signal_reliability, empirical, 'Reliability of packet loss and RTT as congestion signals').

omega_variable(
    fairness_across_technologies,
    'Do competing congestion algorithms (TCP Reno, CUBIC, BBR) converge to fair sharing or does algorithm diversity create systematic extraction where slow algorithms starve aggressive ones?',
    'Long-term simulation and testbed experiments measuring throughput distribution across algorithm pairs; analysis of de facto fairness vs theoretical fairness',
    'If convergence to fairness: constraint is primarily coordination mechanism. If algorithm wars persist: extraction is embedded in protocol adoption cycles; classification trends toward snare for non-dominant algorithms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_across_technologies, empirical, 'Whether competing algorithms converge to fair sharing').

omega_variable(
    quic_adoption_tipping_point,
    'Will QUIC adoption reach critical mass (>60% of new connections) within 10 years, or will TCP''s installed base resistance prevent displacement?',
    'Longitudinal tracking of QUIC adoption rates; measurement of barriers to deployment in legacy infrastructure; network ossification analysis',
    'If tipping point: scaffold sunset is real; congestion control transitions from fundamental to vestigial. If stalled: TCP persists indefinitely; extraction from legacy lock-in becomes permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quic_adoption_tipping_point, empirical, 'QUIC adoption and TCP displacement timeline').

omega_variable(
    buffer_bloat_extraction_mechanism,
    'How much of the congestion control backoff reflects actual network saturation vs middlebox buffering that itself is extractive (forcing latency on all flows)?',
    'AQM deployment analysis; measurement of latency reduction when buffer sizes are constrained; comparison of congestion response before/after buffering limits',
    'If large fraction is buffer-bloat-driven: extraction is partly a response to prior infrastructure flaw; reducing suppression may require fixing buffering, not just tuning algorithms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(buffer_bloat_extraction_mechanism, empirical, 'Buffer bloat as extraction mechanism vs legitimate congestion response').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_congestion_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_tr_t0, tcp_congestion_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tcp_tr_t5, tcp_congestion_control, theater_ratio, 5, 0.25).
narrative_ontology:measurement(tcp_tr_t10, tcp_congestion_control, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(tcp_be_t0, tcp_congestion_control, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tcp_be_t5, tcp_congestion_control, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(tcp_be_t10, tcp_congestion_control, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_congestion_control, resource_allocation).
narrative_ontology:affects_constraint(tcp_congestion_control, buffer_bloat).
narrative_ontology:affects_constraint(tcp_congestion_control, network_latency_heterogeneity).
narrative_ontology:affects_constraint(tcp_congestion_control, quic_protocol_adoption).

% DUAL FORMULATION NOTE:
% TCP congestion control is upstream of several derived constraints. Buffer bloat (the middlebox buffering that amplifies congestion signals) has its own extractiveness. Network latency heterogeneity (the diversity of link qualities that makes uniform congestion response suboptimal) has its own constraint on fairness. QUIC adoption represents a downstream constraint: displacement of TCP requires solving coordination and extraction problems differently. All three are structurally linked — fixing buffer bloat reduces the congestion control backoff requirement; optimizing latency diversity reduces the extraction asymmetry; deploying QUIC reduces lock-in suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_congestion_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
