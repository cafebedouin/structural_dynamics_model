% ============================================================================
% CONSTRAINT STORY: rfc9293_interoperability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_interoperability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_interoperability
 *   human_readable: TCP Interoperability & Reliability Requirements (RFC 9293)
 *   domain: technological/network_protocols
 *
 * SUMMARY:
 *   RFC 9293 codifies the TCP protocol specification, defining requirements
 *   for interoperability and reliability across heterogeneous networks and
 *   devices. This constraint is best understood as a pure coordination
 *   mechanism — all stakeholders benefit from having a common transport layer
 *   that reliably delivers ordered packets. The specification emerges from
 *   the fundamental physical and mathematical properties of packet-switched
 *   networks: packets can be lost, corrupted, reordered, or duplicated in
 *   transit; no single device can control the network path; therefore, the
 *   endpoint must implement logic to detect and correct these failures. TCP
 *   provides this logic as a shared, standardized implementation that
 *   eliminates the need for each application to solve the problem
 *   independently. Unlike constraints that extract value asymmetrically
 *   (snares) or mix coordination with coercive extraction (tangled ropes),
 *   TCP is near-pure rope: symmetric burden, symmetrically distributed
 *   benefit, minimal suppression of alternatives, low coercion overhead. The
 *   theater ratio remains low (0.15) because the constraint's function is
 *   entirely transparent — the benefit of reliable delivery is direct and
 *   measurable, with minimal performative or theatrical element.
 *
 * KEY AGENTS:
 *   - Device Manufacturers: Institutional beneficiary (institutional/arbitrage) — TCP standardization enables interoperability; no extraction
 *   - Network Operators: Institutional beneficiary (institutional/arbitrage) — TCP enables service delivery across heterogeneous networks; coordination benefit
 *   - End Users & Applications: Moderate beneficiary (moderate/mobile) — Reliable delivery without implementing custom reliability logic; low exit cost (UDP alternative available)
 *   - Legacy Systems: Constrained beneficiary (powerless/constrained) — Benefit from ability to communicate with modern systems; bear symmetric implementation cost
 *   - Standards Body (IETF) & Research Community: Organized beneficiary (organized/arbitrage) — TCP specification serves as coordination point for research and innovation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — TCP requirements emerge from mathematical constraints on packet-switched networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_interoperability, 0.12).
domain_priors:suppression_score(rfc9293_interoperability, 0.08).
domain_priors:theater_ratio(rfc9293_interoperability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_interoperability, extractiveness, 0.12).
narrative_ontology:constraint_metric(rfc9293_interoperability, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_interoperability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_interoperability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_interoperability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_interoperability, rope).
narrative_ontology:human_readable(rfc9293_interoperability, "TCP Interoperability & Reliability Requirements (RFC 9293)").
narrative_ontology:topic_domain(rfc9293_interoperability, "technological/network_protocols").

domain_priors:emerges_naturally(rfc9293_interoperability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_interoperability, all_internet_users).
narrative_ontology:constraint_beneficiary(rfc9293_interoperability, device_manufacturers).
narrative_ontology:constraint_beneficiary(rfc9293_interoperability, network_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, reliable ordered packet delivery over unreliable networks is a mathematical and physical constraint, not a convention. TCP solves the end-to-end problem of ordered delivery on networks with packet loss, corruption, and reordering. This is an irreducible requirement of any packet-switched network, not a choice. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(rfc9293_interoperability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: DEVICE MANUFACTURERS & NETWORK OPERATORS (ROPE) — Institutional actors benefit from RFC 9293 standardization: it enables interoperability across heterogeneous devices and networks. No single manufacturer or operator has incentive to defect from the standard. The constraint is pure coordination: agree on packet format, sequence numbering, flow control, congestion handling. Low coercion overhead. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.001. Net neutral or slightly beneficial.
constraint_indexing:constraint_classification(rfc9293_interoperability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: END USERS & APPLICATION DEVELOPERS (ROPE) — Moderate-power agents benefit from TCP's reliability guarantees without bearing significant extraction. Can develop applications assuming ordered delivery. Exit options exist: UDP for applications that tolerate loss. The constraint is coordination: TCP provides a common assumption that eliminates the burden of implementing reliability in each application. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.10.
constraint_indexing:constraint_classification(rfc9293_interoperability, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY SYSTEMS & CONSTRAINED DEVICES (ROPE) — Devices with limited memory or processing power face implementation burden from RFC 9293 requirements (sequence number tracking, retransmission logic, congestion control state). However, this is a coordination cost, not extraction: all devices face the same requirement symmetrically. The standard enables them to communicate with modern systems. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.09. Coordinated burden, not asymmetric extraction.
constraint_indexing:constraint_classification(rfc9293_interoperability, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONGESTION CONTROL (MOUNTAIN) — TCP's congestion control mechanism (slow start, additive increase, multiplicative decrease) emerges from the physical reality that network capacity is finite and overload causes collapse. This is not negotiable. RFC 9293 codifies the mathematical relationship between sending rate and network health. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. No degrees of freedom.
constraint_indexing:constraint_classification(rfc9293_interoperability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: STANDARDS BODY & RESEARCH COMMUNITY (ROPE) — IETF and academic network research benefit from RFC 9293 as a coordination point. The standard enables research, testing, and continuous improvement (congestion control variants, performance tuning). No extraction occurs — the standard is explicitly designed to be open and extensible. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.006.
constraint_indexing:constraint_classification(rfc9293_interoperability, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_interoperability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rfc9293_interoperability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_interoperability, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(rfc9293_interoperability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. TCP is fundamentally a coordination mechanism. The 'extraction' score reflects the implementation burden on devices and the slight asymmetry that some actors (particularly legacy systems) face higher costs relative to benefit. However, this is not extraction in the sense of one party taking value from another — all parties share the burden and all benefit. The base ε=0.12 is justified by the symmetric nature of the requirement and the availability of alternatives. Suppression (0.08): Very low. Alternatives exist (UDP, SCTP, QUIC). Network effects create some path dependency, but this is not suppression — it is the natural consequence of standardization success. Devices can technically implement custom transport protocols if they choose. Theater ratio (0.15): Low and stable. The benefit of TCP is direct and measurable. Unlike institutions that perform legitimacy rituals, TCP's function is transparent. The small theater component reflects documentation, compliance testing, and specification review processes. Claimed type (Rope): Correct. ε=0.12, suppression=0.08, extractiveness ≤ 0.35, base extraction ≤ 0.45 all satisfy rope gates. No active enforcement required; the standard propagates through voluntary adoption.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits near-uniform classification across all perspectives, which is characteristic of a pure coordination mechanism. All perspectives perceive Rope or Mountain — these are not contradictory but complementary. The Mountain perspectives (analytical observer, congestion control) reflect that the underlying mathematical constraints are immutable. The Rope perspectives (manufacturers, operators, users, standards body) reflect that the implementation of these constraints is a voluntary, standardized choice that benefits all. No agent perceives extraction or coercion. This uniformity is correct and indicates that the constraint is genuinely pure coordination rather than mixed extraction. The perspectival gaps that do exist (legacy systems experience higher implementation burden than modern systems) are symmetric — legacy devices face the same requirements as modern ones, and both benefit from interoperability.
 *
 * DIRECTIONALITY LOGIC:
 *   All beneficiaries + symmetric exit options → d≈0.50-0.15 across perspectives. No victims. Device manufacturers and operators: beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. End users: beneficiary + mobile → d≈0.50, f(d)≈0.65. Legacy systems: beneficiary (symmetric burden) + constrained → d≈0.55, f(d)≈0.75. Standards body: beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Analytical observer: analytical → d≈0.72, f(d)≈1.15. None of these produce high d values. The absence of victims is crucial: no group bears asymmetric costs. This confirms Rope classification and rules out Snare, Tangled Rope, or higher-extraction types.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low extractiveness with high beneficiary agreement is the hallmark of pure coordination (Rope). The risk of mandatrophy would be classifying TCP as a Mountain-only natural law and thus removing it from the domain of human design and policy — which would be incorrect. TCP is a designed specification, not a law of nature, even though it codifies responses to physical constraints. The analytical observer's Mountain classification is justified (the underlying packet-switching reality is immutable) but secondary to the Rope classification (the implementation choice is designed and can be revised). RFC 9293 is currently the best available solution, not the only possible solution. Future transport protocols (QUIC, specialized options for constrained devices) may replace it. The constraint's legitimacy derives from coordination benefit and symmetric distribution, not from being an immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_vs_conventional,
    'Is TCP interoperability a fundamental constraint of distributed systems mathematics or a conventional coordination choice?',
    'Comparison with alternative transport layer designs (QUIC, SCTP, custom protocols); analysis of which requirements are mathematically derived vs historically contingent',
    'If fundamental: mountain classification confirmed across all perspectives. If conventional: rope classification should dominate, and accessibility_collapse and resistance scores are overestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_vs_conventional, conceptual, 'Whether TCP requirements are fundamental or conventional').

omega_variable(
    congestion_control_optimality,
    'Does RFC 9293 congestion control (AIMD) represent the only mathematically optimal solution or one choice among equally valid alternatives?',
    'Game-theoretic analysis; comparison of convergence properties, fairness outcomes, and network stability across different congestion control algorithms',
    'If optimal: mountain gates strengthen (ε < 0.10). If one choice among alternatives: rope gates strengthen (beneficiaries have some discretion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_control_optimality, empirical, 'Whether AIMD congestion control is uniquely optimal').

omega_variable(
    exit_option_viability,
    'Are UDP and alternative transport protocols genuine exit options for TCP, or do network externalities make TCP mandatory?',
    'Survey of application distribution by protocol; cost analysis of implementing UDP-based reliability; measurement of performance trade-offs across transport choices',
    'If alternatives viable: rope classification confirmed (low suppression, mobile exit). If TCP mandatory: tangled_rope or snare gates may fire despite low ε (network effect creates suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_viability, empirical, 'Whether UDP and alternatives provide genuine exit from TCP').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_interoperability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_rfc_tr_t0, rfc9293_interoperability, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tcp_rfc_tr_t15, rfc9293_interoperability, theater_ratio, 15, 0.12).
narrative_ontology:measurement(tcp_rfc_tr_t30, rfc9293_interoperability, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(tcp_rfc_be_t0, rfc9293_interoperability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tcp_rfc_be_t15, rfc9293_interoperability, base_extractiveness, 15, 0.1).
narrative_ontology:measurement(tcp_rfc_be_t30, rfc9293_interoperability, base_extractiveness, 30, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_interoperability, information_standard).
narrative_ontology:affects_constraint(rfc9293_interoperability, network_packet_loss_recovery).
narrative_ontology:affects_constraint(rfc9293_interoperability, internet_congestion_prevention).
narrative_ontology:affects_constraint(rfc9293_interoperability, device_interoperability).

% DUAL FORMULATION NOTE:
% TCP interoperability exists at two levels: (1) The mathematical constraint of reliable ordered delivery over unreliable networks (Mountain, ε≈0.05) and (2) The standardized protocol specification that implements this solution (Rope, ε≈0.12). These are structurally distinct — one is invariant, the other is a particular historical choice. RFC 9293 is the downstream story that instantiates the upstream mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
