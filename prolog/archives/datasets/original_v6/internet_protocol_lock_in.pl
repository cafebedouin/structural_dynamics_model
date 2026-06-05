% ============================================================================
% CONSTRAINT STORY: internet_protocol_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_internet_protocol_lock_in, []).

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
 *   constraint_id: internet_protocol_lock_in
 *   human_readable: Internet Protocol Lock-In: TCP/IP Ossification and Infrastructure Capture
 *   domain: internet_infrastructure/network_architecture
 *
 * SUMMARY:
 *   TCP/IP lock-in represents a canonical infrastructure constraint where
 *   coordination benefits and extractive lock-in are structurally
 *   inseparable. The protocols emerged in the 1980s-1990s as the winning
 *   coordination mechanism for global packet-switched networks. They solved a
 *   genuine coordination problem: enabling heterogeneous networks to
 *   interoperate at scale. But over 30+ years, they have ossified into a
 *   barrier to more efficient transport mechanisms. Middleboxes (firewalls,
 *   NATs, DPI systems) deployed throughout the Internet enforce TCP/UDP
 *   orthodoxy by blocking or degrading packets that don't conform to familiar
 *   protocols. New protocols like QUIC, SCTP, and DCCP face deployment
 *   barriers so high that they must either remain confined to single
 *   operators' infrastructure or wrap themselves in UDP packets to bypass the
 *   enforcement mechanisms. The constraint exhibits a perspectival chasm: the
 *   vendor and operator perspectives see coordination and stability; the
 *   researcher and network objectives perspectives see pure extraction and
 *   suppression; the cloud provider perspective shows that sufficiently
 *   powerful actors can escape the lock-in. The encapsulation coalition (QUIC
 *   in UDP) represents an organized response with a sunset clause — over
 *   10-20 years, if application-layer protocols accumulate enough value, ISPs
 *   may invest in native middlebox upgrades, enabling protocols to graduate
 *   from encapsulation to native deployment. The extractiveness has increased
 *   over the measurement interval as the cost of deploying new protocols has
 *   grown with Internet scale and the complexity of the deployed middlebox
 *   ecosystem.
 *
 * KEY AGENTS:
 *   - Protocol Innovator: Primary victim (powerless/trapped) — researchers designing new transports face insurmountable deployment barriers; cannot exit without fundamental infrastructure change
 *   - Equipment Vendor: Primary beneficiary (institutional/arbitrage) — incumbents benefit from TCP/IP standardization through massive deployment scale; can arbitrage between standardization and market consolidation
 *   - Network Operator: Secondary victim (moderate/constrained) — both benefits from TCP/IP stability and trapped by inability to deploy more efficient protocols without expensive infrastructure replacement
 *   - Encapsulation Coalition: Organized agent (organized/mobile) — QUIC, HTTP/3, and tunneling advocates building workarounds with a sunset clause as infrastructure upgrades mature
 *   - Internet Engineering Task Force: Institutional actor (institutional/arbitrage) — maintains standardization process but role is increasingly performative; standards no longer drive deployment
 *   - Major Cloud Provider: Powerful agent (powerful/mobile) — sufficiently resourced to escape lock-in through custom infrastructure and proprietary implementations while benefiting from TCP/IP interoperability
 *   - Network Efficiency Objectives: Primary victim (powerless/trapped) — abstract objective (lower latency, higher throughput, better congestion control) that cannot organize or exit; suppressed by lock-in mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_protocol_lock_in, 0.58).
domain_priors:suppression_score(internet_protocol_lock_in, 0.68).
domain_priors:theater_ratio(internet_protocol_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_protocol_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(internet_protocol_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(internet_protocol_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_protocol_lock_in, tangled_rope).
narrative_ontology:human_readable(internet_protocol_lock_in, "Internet Protocol Lock-In: TCP/IP Ossification and Infrastructure Capture").
narrative_ontology:topic_domain(internet_protocol_lock_in, "internet_infrastructure/network_architecture").

domain_priors:requires_active_enforcement(internet_protocol_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_protocol_lock_in, incumbent_equipment_vendors).
narrative_ontology:constraint_beneficiary(internet_protocol_lock_in, established_service_providers).
narrative_ontology:constraint_beneficiary(internet_protocol_lock_in, legacy_protocol_implementers).
narrative_ontology:constraint_victim(internet_protocol_lock_in, protocol_innovation_researchers).
narrative_ontology:constraint_victim(internet_protocol_lock_in, emerging_transport_mechanisms).
narrative_ontology:constraint_victim(internet_protocol_lock_in, network_efficiency_objectives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTOCOL INNOVATOR (SNARE) — Researchers designing alternative transport protocols (QUIC, SCTP, DCCP) face deployment barriers that are almost insurmountable. Middleboxes (firewalls, NATs, DPI systems) enforce TCP/UDP orthodoxy by dropping unfamiliar packets. ISPs and enterprises have no incentive to enable new protocols. The innovator cannot exit: their work has no deployment path without fundamental infrastructure change. Maximum experienced extraction — the lock-in mechanism suppresses alternatives before they can gain traction.
constraint_indexing:constraint_classification(internet_protocol_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NETWORK OPERATOR (TANGLED ROPE) — ISPs and enterprise network administrators both benefit from and are trapped by TCP/IP ossification. The benefit: TCP/IP is stable, proven, and requires no infrastructure investment. The trap: they cannot deploy more efficient protocols without expensive middlebox replacement and coordination across multiple autonomous systems. Active enforcement required: operators must actively block or degrade non-standard protocols to maintain their known, manageable network. Significant extraction (suppression of operator choice) but genuine coordination function (stable, interoperable backbone).
constraint_indexing:constraint_classification(internet_protocol_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT VENDOR (ROPE) — Incumbent hardware and software vendors (Cisco, Arista, Linux kernel maintainers for stable releases) benefit from TCP/IP standardization through massive deployment scale and predictable market dynamics. They experience the constraint as pure coordination: standardization on TCP/UDP enables them to ship certified, tested, interoperable products globally. Network effects work in their favor — the larger the installed base, the more valuable their equipment. Low experienced extraction because this agent controls the standardization body and can arbitrage between early adoption and market consolidation.
constraint_indexing:constraint_classification(internet_protocol_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENCAPSULATION COALITION (SCAFFOLD) — A coalition of protocol researchers and application developers (QUIC in UDP, HTTP/3 proponents, tunneling advocates) is working around TCP/IP lock-in by encapsulating new protocols inside TCP or UDP payloads. This bypasses middlebox blocking because the outer packet uses approved protocols. The coalition is organized (IETF working groups, major cloud providers) and sees an exit path: over 10-20 years, application-layer protocols can accumulate enough benefit that operators will invest in middlebox upgrades to support them natively. This is a classic scaffold: temporary workaround with a sunset clause as infrastructure upgrades mature.
constraint_indexing:constraint_classification(internet_protocol_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNET ENGINEERING TASK FORCE (PITON) — The IETF maintains the TCP/IP standard and defines new protocols, but the organization's primary function (coordinating protocol development) has become substantially performative. The IETF produces specifications for new transports (SCTP, DCCP, QUIC variants), but deployment is decoupled from standardization. The organization persists through institutional inertia: it claims to coordinate Internet architecture, but actual deployment decisions are made by equipment vendors and ISPs. The theater ratio is high (many working groups, RFCs, standards documents) relative to actual influence on deployed infrastructure. The IETF sees its own role as degraded — it standardizes protocols the market will not deploy.
constraint_indexing:constraint_classification(internet_protocol_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MAJOR CLOUD PROVIDER (TANGLED ROPE) — Large cloud operators (Google, Amazon, Meta) have sufficient power to deploy custom network protocols at scale within their infrastructure and to their end users. They benefit from TCP/IP standardization (interoperability with the broader Internet) and simultaneously escape the lock-in through application-layer workarounds (QUIC deployment, custom congestion control). They coordinate with smaller peers (through IETF, open standards) while extracting efficiency gains through proprietary implementations. Their exit options are mobile — they can fund protocol innovations and absorb deployment costs. Extraction runs in both directions: they extract efficiency from the constraint and contribute to breaking it.
constraint_indexing:constraint_classification(internet_protocol_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of protocol standardization is inherent to networked systems: any distributed system must converge on common communication rules. The larger the network, the higher the coordination cost of changing those rules. This perspective views TCP/IP lock-in as an immutable property of network scale — a natural law of distributed systems. However, this classification is a false summit: the base structural data reveals that TCP/IP dominance is maintained through active enforcement (middlebox blocking, vendor lock-in), not through immutable physics. The 'natural law' framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(internet_protocol_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_protocol_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(internet_protocol_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_protocol_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(internet_protocol_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(internet_protocol_lock_in, TR),
    TR >= 0.70.

:- end_tests(internet_protocol_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The lock-in mechanism prevents deployment of demonstrably more efficient transport mechanisms. QUIC achieves 5-10% latency reduction through 0-RTT connection establishment; custom congestion control can improve throughput 15-30% in specific conditions; DCCP enables real-time applications. But these protocols cannot deploy natively — they must encapsulate or remain confined to single operators. The extraction is asymmetric: vendors and large operators extract efficiency and deployment control; innovators and small operators are suppressed. The extractiveness has grown as Internet scale has increased, making coordination change harder. Suppression (0.68): High. Multiple mechanisms enforce TCP/UDP orthodoxy: (1) middlebox packet filtering — firewalls, NATs drop unrecognized protocols; (2) vendor incentive misalignment — equipment vendors profit from existing deployments and have weak incentives to accelerate protocol change; (3) coordination cost — changing protocols requires coordinating across millions of autonomous systems with different incentives; (4) path dependence — decades of TCP/IP optimization means new protocols must beat TCP by large margins to justify migration. Theater ratio (0.55): Moderate. The IETF standards process produces detailed RFCs and maintains working groups for new protocols, but standardization is decoupled from deployment. Standards provide legitimacy and intellectual rigor but do not drive market adoption. Application-layer workarounds (QUIC) bypass standardization entirely, suggesting theater is shifting upward in the stack. Claimed type: Tangled Rope. The constraint has genuine coordination function (TCP/IP enables global Internet interoperability) and asymmetric extraction (vendors benefit, innovators suppressed). Active enforcement is required (middleboxes actively block new protocols). The rope part is real; the snare overlay is also real.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap runs between powerful and powerless actors. The vendor and incumbent operator see a Rope: standardization on TCP/IP solved the problem of heterogeneous network interoperability and continues to deliver coordination benefits. Changing protocols is costly and risky; sticking with TCP/IP is prudent. The protocol innovator sees a Snare: their research into more efficient mechanisms has no deployment path. The middleboxes were not designed to block their work — they were designed to enforce security and manage networks — but the effect is the same: suppression without alternative. The major cloud provider sees a Tangled Rope with an escape hatch: they have enough power and resources to deploy custom protocols within their infrastructure and to drive standards like QUIC that bypass middleboxes through encapsulation. They experience both extraction (they cannot deploy arbitrary protocols globally) and benefit (they profit from TCP/IP interoperability). The encapsulation coalition sees a Scaffold: QUIC demonstrates that application-layer protocols can achieve feature parity with would-be transport-layer innovations, and once enough applications adopt QUIC, ISPs may invest in native middlebox support, providing a sunset path. The IETF sees a Piton: the organization maintains the standardization process, but its primary function (coordinating protocol development) has atrophied. RFCs are published; working groups meet; but deployment decisions are made elsewhere. The theater persists through institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Equipment vendors are beneficiaries with arbitrage options (d ≈ 0.15) — they profit from the status quo and can shift market share without bearing lock-in costs. Protocol innovators are victims with trapped exit (d ≈ 0.95) — they cannot deploy their work without fundamental infrastructure change. Network operators are victims with constrained exit (d ≈ 0.75) — they could theoretically deploy new protocols but face high costs and coordination barriers. Cloud providers are beneficiaries with mobile exit (d ≈ 0.48) — they benefit from TCP/IP interoperability but have sufficient power to deploy alternatives. The encapsulation coalition is organized with mobile exit (d ≈ 0.40) — they have agency and a visible exit path. The IETF is an institutional maintainer with arbitrage options (d ≈ 0.20) — they coordinate standards but do not bear deployment costs. The constraint exhibits maximum directionality dispersion: vendors at d ≈ 0.15, cloud providers at d ≈ 0.48, operators at d ≈ 0.75, innovators at d ≈ 0.95. This dispersion explains the perspectival gap: same constraint, vastly different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   TCP/IP lock-in resolves the mandatrophy by demonstrating that tangled rope is a coherent classification for constraints that genuinely coordinate (TCP/IP enables global interoperability) while asymmetrically extracting (vendors profit, innovators suppressed). The constraint is NOT a rope (pure coordination without extraction) because vendors and operators benefit disproportionately while innovators are suppressed. It is NOT a snare (pure extraction without coordination) because TCP/IP continues to solve the real problem of heterogeneous network interoperability — removing it would break the Internet, not liberate it. The constraint is tangled: coordination and extraction are structurally fused. Active enforcement (middleboxes blocking new protocols) maintains both the coordination function and the extraction asymmetry. The mandatrophy is avoided by recognizing that asymmetric extraction within a genuine coordination mechanism is the defining signature of tangled rope, not a contradiction. The constraint requires both beneficiaries (vendors) and victims (innovators) to function — remove either and the constraint changes type or collapses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_deployment_threshold,
    'What percentage of Internet paths must support a new protocol natively (not via encapsulation) before deployment becomes economically viable for that protocol?',
    'Measurement of native vs encapsulated protocol deployment rates over time; analysis of adoption curves for protocols that have crossed the threshold (QUIC, IPv6) vs those that have not (SCTP, DCCP)',
    'If threshold < 20%: protocol innovation should proceed faster than observed. If threshold > 70%: lock-in is quasi-immutable on decade timescales. Suggests different sunset timeline for scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_deployment_threshold, empirical, 'Native protocol support threshold for economic viability').

omega_variable(
    vendor_incentive_alignment,
    'Are equipment vendors actively investing in next-generation protocol support, or do they benefit from lock-in and oppose protocol change?',
    'Analysis of vendor R&D spending on new protocols; examination of patent filings and product roadmaps; measurement of time-to-market for new protocol support after standards publication',
    'If vendors are aligned with innovation: rope classification dominates, lock-in is coordination. If vendors oppose change: snare classification dominates, lock-in is extraction. Determines whether beneficiary perspective is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_incentive_alignment, empirical, 'Vendor incentive alignment with protocol innovation').

omega_variable(
    application_layer_workaround_sufficiency,
    'Can encapsulation-based workarounds (QUIC in UDP, custom congestion control in applications) provide sufficient performance and features to make native protocol deployment unnecessary?',
    'Performance comparison of encapsulated vs native protocols; measurement of application-layer innovation rate in protocol design; analysis of whether encapsulation creates secondary lock-ins (e.g., reliance on specific cloud platforms)',
    'If workarounds are sufficient: scaffold is permanent (encapsulation layer becomes new standard), ossification shifts layers upward. If workarounds are insufficient: native deployment is eventually mandatory, sunset timeline accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(application_layer_workaround_sufficiency, empirical, 'Whether encapsulation-based workarounds provide sufficient functionality').

omega_variable(
    nation_state_fragmentation_risk,
    'Are national regulatory regimes (data localization, censorship, network sovereignty) fragmenting the Internet into protocol islands, creating multiple lock-ins rather than one global one?',
    'Analysis of national network regulation, measurement of protocol fragmentation (China Great Firewall protocol filters, EU technical standards), examination of whether national lock-ins reduce or increase global TCP/IP dependency',
    'If fragmentation occurs: global Internet becomes multiple local lock-ins with different constraints. Classification changes from global tangled_rope to regional/national snares. Affects timeline for protocol innovation globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nation_state_fragmentation_risk, empirical, 'Risk of nation-state protocol fragmentation creating multiple lock-ins').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_protocol_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipli_tr_t0, internet_protocol_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ipli_tr_t10, internet_protocol_lock_in, theater_ratio, 10, 0.5).
narrative_ontology:measurement(ipli_tr_t20, internet_protocol_lock_in, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ipli_be_t0, internet_protocol_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ipli_be_t10, internet_protocol_lock_in, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ipli_be_t20, internet_protocol_lock_in, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_protocol_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(internet_protocol_lock_in, ipv6_adoption_stagnation).
narrative_ontology:affects_constraint(internet_protocol_lock_in, middlebox_ossification).
narrative_ontology:affects_constraint(internet_protocol_lock_in, real_time_application_transport).

% DUAL FORMULATION NOTE:
% TCP/IP lock-in is upstream of three downstream constraints: IPv6 adoption stagnation (a specific protocol replacement failure), middlebox ossification (the enforcement infrastructure), and real-time application transport (the functional requirement that new protocols must meet). Each downstream constraint has its own ε value reflecting different aspects of the same infrastructure lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(internet_protocol_lock_in, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
