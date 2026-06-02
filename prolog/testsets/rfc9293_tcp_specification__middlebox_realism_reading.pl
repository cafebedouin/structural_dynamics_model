% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification Subordinated to Deployed Middlebox Behavior (Middlebox Realism Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems
 *
 * SUMMARY:
 *   RFC 9293 (TCP Specification Version 9) formally describes TCP endpoint
 *   behavior as a distributed protocol with specific packet format, state
 *   machine transitions, and algorithmic requirements. However, the deployed
 *   network deviates from this specification in systematic ways: stateless
 *   firewalls and DPI systems (middleboxes) modify TCP packets in flight to
 *   enforce policies (filtering, traffic classification, surveillance) that
 *   the RFC does not describe. Endpoints cannot implement the specification
 *   as written because their packets will be dropped or mangled by
 *   middleboxes; they must implement to the deployed middlebox population
 *   instead. This constraint is the MIDDLEBOX REALISM READING of the RFC 9293
 *   TCP specification kernel — the reading that treats the network's actual
 *   behavior (determined by the middlebox population) as normative,
 *   subordinating the formal specification's authority to what the network
 *   actually does. This reading coexists with two siblings: the STRICT
 *   INVARIANCE reading (which treats RFC 9293 as an invariant specification
 *   that middleboxes violate, making middleboxes the problem) and the
 *   OPTIMIZATION LATITUDE reading (which treats RFC 9293 as a specification
 *   with intentional design latitude, allowing middlebox modifications as
 *   legitimate optimization within bounds). This story instantiates the
 *   middlebox realism reading as a clean, ε-invariant constraint: Snare from
 *   the endpoint perspective (trapped, no exit, extracted control), Rope from
 *   the middlebox operator perspective (beneficiary, coordination function),
 *   Tangled Rope from the application developer perspective (constrained,
 *   mixed coordination and extraction), and Piton from the formal standards
 *   authority perspective (degraded, theatrical authority). The constraint's
 *   extractiveness has risen over two decades (0.28 in 2002 → 0.58 in 2018)
 *   as middlebox deployment has intensified and endpoint autonomy has eroded.
 *   The suppression requirement tracks with extractiveness: as more
 *   middleboxes deploy, endpoints must suppress (hide, encrypt, or work
 *   around) their TCP behavior to avoid modification.
 *
 * KEY AGENTS:
 *   - Endpoint Implementation: Powerless/trapped (biography) — cannot implement RFC 9293 because deployed middleboxes will reject it; must implement to middlebox population instead; victim of extracted control
 *   - Standards Body (IETF): Moderate/constrained (generation) — maintains specification authority but cannot enforce; resources spent on standards that become decorative rather than normative; tragic loss of authority
 *   - Middlebox Operator Coalition: Institutional/arbitrage (immediate) — ISPs, firewalls, DPI vendors, state surveillance systems; collectively enforce protocol interpretation over RFC; beneficiary with high agency; experience constraint as coordination
 *   - Application Developer Coalition: Organized/constrained (biography) — must implement workarounds (QUIC, TLS-wrapped TCP, application encryption) to circumvent middleboxes; mixed coordination (resilience) and extraction (duplication of transport-layer functions)
 *   - Surveillance State Infrastructure: Institutional/arbitrage (immediate) — state actors operating or relying on middleboxes to intercept/classify traffic at protocol level; pure beneficiary; extract information from plaintext TCP
 *   - Formal Standards Authority: Institutional/arbitrage (civilizational) — RFC 9293 persists as institutional standard but actual prescriptive force has atrophied; theater ratio high; authority is piton (degraded, maintained through inertia)
 *   - Analytical Observer: Analytical/analytical (civilizational) — observes that specification authority has been expropriated by deployed middlebox population; deployed behavior IS the specification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.58).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.68).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, snare).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification Subordinated to Deployed Middlebox Behavior (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, 'tcp-middlebox-realism-reading-2026-02-26').
narrative_ontology:cs_kernel_codification('tcp-middlebox-realism-reading-2026-02-26', fixed_text).
narrative_ontology:cs_authority_grounding('tcp-middlebox-realism-reading-2026-02-26', extraction).
narrative_ontology:cs_reading_relation('tcp-middlebox-realism-reading-2026-02-26', strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('tcp-middlebox-realism-reading-2026-02-26', optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('tcp-middlebox-realism-reading-2026-02-26', foundational, deployed_behavior_is_normative_specification).
narrative_ontology:cs_axiom_status(deployed_behavior_is_normative_specification, holdable).
narrative_ontology:cs_axiom_grounding('tcp-middlebox-realism-reading-2026-02-26', deployed_behavior_is_normative_specification, empirically_contingent).
narrative_ontology:cs_axiom('tcp-middlebox-realism-reading-2026-02-26', foundational, formal_rfc_authority_subordinate_to_network_behavior).
narrative_ontology:cs_axiom_status(formal_rfc_authority_subordinate_to_network_behavior, holdable).
narrative_ontology:cs_axiom_grounding('tcp-middlebox-realism-reading-2026-02-26', formal_rfc_authority_subordinate_to_network_behavior, deontological).
narrative_ontology:cs_reference_frame('tcp-middlebox-realism-reading-2026-02-26', formal_specification_authority).
narrative_ontology:cs_drift_state('tcp-middlebox-realism-reading-2026-02-26', contemporary_ubiquitous_middleboxes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('tcp-middlebox-realism-reading-2026-02-26', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, surveillance_infrastructure).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_autonomy).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_specification_integrity).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, transport_layer_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDPOINT IMPLEMENTATION (SNARE) — Trapped. Endpoints cannot implement RFC 9293 as specified; they must implement to the deployed middlebox population instead. Packets that conform to the standard are dropped or modified by stateless inspection; packets that deviate from the standard traverse the network. The endpoint has no exit: it must transmit, and middleboxes intercept all traffic. Maximum suppression — alternatives (tunnel, encapsulation, cryptography) become necessary to escape but constitute workarounds rather than protocol compliance.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STANDARDS BODY / IETF (SNARE) — Constrained. The IETF maintains the specification authority but cannot enforce it. Publishing a standard that the network rejects is a form of powerlessness — the standards body loses authority over time as implementations must diverge to be practical. The IETF sees the constraint as tragic: they write the standard, but the deployed middleboxes have greater control over what actually happens on the network. Substantial extraction: IETF resources are spent on specifications that become decorative rather than normative.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MIDDLEBOX OPERATOR COALITION (ROPE) — Beneficiaries. ISPs, enterprise firewalls, DPI vendors, and state surveillance systems collectively enforce their own protocol interpretation over the standard. They experience this as pure coordination: they are solving the operational problem of managing traffic (legitimate classification, DDoS mitigation, copyright enforcement, state monitoring). The constraint appears to them as coordination among middleboxes that happens to override the endpoint specification. Net beneficiary — extraction runs toward this coalition; they gain control and operational authority.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: APPLICATION DEVELOPER COALITION (TANGLED ROPE) — Constrained with agency. App developers (browsers, CDNs, messaging apps) see the middlebox-driven deviation from spec as both a coordination problem and an extraction mechanism. They must implement workarounds (QUIC, TLS-wrapped TCP, application-layer encryption) that duplicate the transport layer's functions. The workarounds benefit them by increasing application-layer control and reducing middlebox visibility into application state. But they also constrain the entire developer ecosystem by requiring expensive engineering effort to circumvent the network layer. Genuine coordination (resilience against middleboxes) coexists with asymmetric extraction (middleboxes extract information from plaintext TCP).
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SURVEILLANCE STATE INFRASTRUCTURE (ROPE) — Beneficiary. State actors rely on the middlebox population (or operate their own middleboxes) to intercept and classify traffic at the protocol level. From the surveillance perspective, the constraint is pure coordination: traffic classification is the legitimate operational goal, and the middlebox network solves it. The constraint appears transparent to the beneficiary — they are simply doing their function. Net beneficiary with high agency.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: FORMAL STANDARDS AUTHORITY (PITON) — Institutionalized but degraded. RFC 9293 persists as an authoritative standard document; publishers, textbooks, and courses teach it as the normative specification. But the document's actual prescriptive force has atrophied — implementations must diverge to function. The standard persists through institutional inertia: it carries legitimacy (IETF authority, peer-reviewed process, formal grammar) but minimal functional authority. Theater ratio is high: the ritual of standards development, the formal RFC process, and the pretense of 'compliance' continue even though the network's actual behavior is determined elsewhere.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SPECIFICATION AUTHORITY ILLUSION (SNARE) — From a civilizational view, the TCP specification no longer governs the protocol. Instead, the deployed middlebox population IS the specification — it defines what packets traverse the network, what gets modified, and what gets dropped. The RFC claims authority over endpoint behavior but that authority is subordinate to the middlebox population's actual control. This is not a technical problem (traffic engineering) viewed from the analytical frame — it is an authority structure problem: specification authority has been expropriated by the middlebox operators. The endpoints experience snare (powerlessness, no exit, extraction of control). The analytical frame confirms: specification authority is subordinate to what the network actually does.
constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_tcp_specification__middlebox_realism_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, TR),
    TR >= 0.70.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. Endpoints extract from endpoints via middleboxes: control over packet transmission is extracted from endpoints and concentrated in middlebox operators (ISPs, firewalls, surveillance systems). The extraction is not total because: (a) endpoints retain some agency through encryption/tunneling workarounds, and (b) the extracted control is not pure rent-seeking — middleboxes do solve real operational problems (DDoS mitigation, traffic classification, network management). However, the extraction far exceeds the coordination benefit: middleboxes extract data visibility and control authority; the coordination function they provide (traffic management) could theoretically be delegated to endpoints with less extraction. Suppression (0.68): Moderate-high. Endpoints have significant barriers to implementing RFC 9293: packets that conform to the standard are dropped or modified. Suppression includes structural barriers (no alternative physical network path), practical barriers (encryption/tunneling have performance costs), and institutional barriers (ISP blocking of VPNs, state prohibition of encryption in some jurisdictions). Suppression is not total because workarounds exist (TLS, QUIC, application-layer encryption, VPNs), but they are costly and imperfect. Theater ratio (0.64): Moderate-high. The RFC standards process is substantially performative: standards are published, reviewed, and formally designated as Internet Standards, but the actual protocol behavior is determined by the middlebox population, not by the RFC text. The theater includes: textbooks teaching RFC 9293 as normative, conferences discussing TCP protocol design, implementations claiming 'RFC compliance' while actually implementing to middleboxes, and IETF working groups proposing TCP revisions that are ignored by deployed middleboxes. The theater tracks with extractiveness because the more middleboxes extract control, the more the formal standards process becomes performative rather than prescriptive. Extractiveness trajectory (0.28 → 0.58): The constraint has accumulated over two decades as middleboxes have proliferated. In 2002, many network paths had few or no middleboxes; endpoints could implement closer to the RFC. By 2010, enterprise firewalls and ISP DPI systems were common; endpoints had to accommodate them. By 2018, middleboxes were ubiquitous; endpoints almost always encountered at least one, and often many. The suppression trajectory mirrors this: fewer workarounds were necessary in 2002; by 2018, encryption is essential. Theater ratio tracks because the standards authority's degradation is cumulative: each time a RFC revision fails to eliminate middlebox behavior, the standards process loses credibility.
 *
 * PERSPECTIVAL GAP:
 *   The middlebox realism reading produces a maximum perspectival gap. Endpoints see Snare (trapped, no exit, extracted control). Middlebox operators see Rope (coordination, beneficiary, no asymmetric extraction from their perspective). Application developers see Tangled Rope (constrained exit, mixed coordination and extraction). Standards body sees tragic Snare (authority subordinated, resources spent on advisory documents). Surveillance state sees Rope (pure coordination). Formal standards authority sees Piton (degraded, theatrical). The analytical observer confirms Snare (specification authority has been expropriated). The gap between Rope (middlebox operator view) and Snare (endpoint view) is the core of this reading: the same structural mechanism appears as benign coordination from one perspective and as entrapment from another. The sibling reading (strict invariance) would collapse this gap by declaring middleboxes the problem and endpoints the victims. The optimization latitude reading would re-interpret the gap as legitimate design latitude. The middlebox realism reading preserves the gap: it accurately reflects that the constraint IS experienced differently depending on structural position, and that the deployed network's actual behavior (dominated by middleboxes) has become normative.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's power, exit options, and structural position. Endpoints (powerless/trapped/global): d ≈ 0.95 (full target of extraction). f(d) ≈ 1.42 (maximum experienced extractiveness). Middlebox operators (institutional/arbitrage/global): d ≈ 0.05 (full beneficiary; arbitrage exit allows them to diverge from RFC freely). f(d) ≈ -0.12 (negative experienced extractiveness from their perspective — they benefit). Application developers (organized/constrained/global): d ≈ 0.60 (both benefit from workarounds and extract engineering costs from middlebox incompatibility). f(d) ≈ 0.85 (moderate experienced extraction). IETF standards body (moderate/constrained/global): d ≈ 0.72 (victim of authority erosion, not primary target). f(d) ≈ 1.15 (analytical-level experienced extraction). Analytical observer (analytical/analytical/universal): d ≈ 0.72 (observer position; canonical fallback). f(d) ≈ 1.15 (analytical chi). The derivation chain prioritizes explicit structural relationships: the RFC specification kernel defines beneficiaries (middlebox operators) and victims (endpoint autonomy, protocol integrity). The engine computes d from these declared relationships and exit options, producing the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve into a single pure type — the mandatrophy is structural, not reducible. From the endpoint perspective, it is Snare: trapped, high extraction, high suppression. From the middlebox operator perspective, it is Rope: coordination function, beneficiary, no asymmetric extraction from their standpoint. From the application developer perspective, it is Tangled Rope: mixed coordination (workarounds enable resilience) and extraction (forced engineering duplication). The mandatrophy is resolved by recognizing that the constraint's CLASSIFICATION is relative to observational position, not an objective property of the network. The middlebox realism reading resolves the mandatrophy by accepting that specification authority is subordinate to deployed behavior — the constraint IS what the network does, not what the RFC says it should do. This reading forecloses the strict invariance reading: if specification authority is subordinate to deployment, then the strict invariance interpretation (RFC is the standard, middleboxes are violations) becomes incoherent. However, it coexists with the optimization latitude reading: deployment could still be legitimate optimization within specification-intended design latitude.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the TCP specification''s authority grounded in endpoint implementation (strict invariance reading) or in deployed network behavior (middlebox realism reading), or in protocol design latitude (optimization latitude reading)?',
    'Network archaeology: trace the historical evolution of middlebox deployment and specification revision. Analyze divergence points between RFC text and empirical packet behavior. Identify which reading best explains why standards revisions have not rolled back middlebox modifications.',
    'Strict invariance: specification regains authority; middleboxes are the problem. Middlebox realism: specification is advisory; deployed behavior is normative; new reading confirms snare classification. Optimization latitude: specification provides bounds; middleboxes are legitimate optimization within bounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading correctly describes TCP specification authority grounding').

omega_variable(
    extractiveness_threshold_middlebox_density,
    'At what deployed middlebox density does endpoint autonomy transition from ''constrained'' (high-cost workarounds available) to ''trapped'' (no practical exit options)?',
    'Empirical measurement: path diversity analysis. For each common endpoint-to-destination path, count middleboxes that inspect/modify TCP. Calculate percentage of global paths where at least one middlebox enforces deviation from RFC. Threshold: if >80% of paths contain at least one middlebox, constraint shifts from tangled_rope to snare for endpoint perspective.',
    'If threshold crossed: snare classification confirmed from endpoint view. If below threshold: tangled_rope more accurate for constrained exit. Extractiveness should rise as middlebox density increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_threshold_middlebox_density, empirical, 'Middlebox deployment density threshold determining endpoint trapped vs constrained status').

omega_variable(
    specification_authority_grounding_legitimacy,
    'Does formal RFC authority (IETF consensus process, Internet Standard designation) retain legitimacy as normative specification when deployed middleboxes have de facto veto power?',
    'Institutional analysis: survey network engineers, standards body participants, and middlebox operators. Assess whether RFC 9293 is cited as normative in: endpoint implementation decisions, middlebox design justifications, ISP policy documentation, academic protocol design. If citation rate differs by audience, authority is contested.',
    'High citation as normative: RFC retains authority despite middlebox override; constraint is tangled_rope (coordination exists alongside extraction). Low citation / only decorative: RFC is piton (authority has atrophied); constraint is snare (specification is subordinate to deployment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_grounding_legitimacy, conceptual, 'Whether RFC authority remains legitimate when middleboxes have veto power').

omega_variable(
    endpoint_implementation_divergence_degree,
    'To what degree do actual TCP implementations (Linux, BSD, Windows, MacOS, embedded stacks) deviate from RFC 9293 to accommodate middlebox realities?',
    'Code archaeology: diff major TCP stack implementations against RFC 9293 specification. Classify deviations: conformance workarounds (intentional RFC-deviation to pass middleboxes), optimization enhancements (RFC-allowed design choices), and outright violations. Measure percentage of implementation size devoted to middlebox compatibility.',
    'High deviation (>30% of implementation): middlebox realism reading confirmed; endpoints are trapped. Low deviation (<10%): endpoints retain specification compliance capacity; constraint is constrained rather than trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endpoint_implementation_divergence_degree, empirical, 'Degree to which actual TCP implementations deviate from RFC to accommodate middleboxes').

omega_variable(
    cs_kernel_revision_failure,
    'Why have RFC TCP revisions (including RFC 9293) failed to eliminate middlebox-driven specification deviation? Is it due to path-dependent lock-in, extraction benefit concentration among middlebox operators, or technical impossibility?',
    'History of IETF TCP proposals: analyze rejected or stalled proposals that would have formalized middlebox behavior or required middlebox standardization. Identify which proposals failed due to: deployment complexity, middlebox vendor opposition, lack of incentive for implementation, or authority erosion (the constraint itself preventing repair).',
    'Path-dependent lock-in: constraint is stable; specification authority is permanently subordinate. Extraction benefits: middleboxes gain from preventing standardization revision; snare classification confirmed. Technical impossibility: constraint approaches mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_kernel_revision_failure, empirical, 'Why RFC revisions have not resolved middlebox specification deviation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_mb_theater_t0_2002, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tcp_mb_theater_t5_2010, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement(tcp_mb_theater_t10_2018, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(tcp_mb_extractiveness_t0_2002, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tcp_mb_extractiveness_t5_2010, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tcp_mb_extractiveness_t10_2018, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp_mb_suppression_t0_2002, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tcp_mb_suppression_t5_2010, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(tcp_mb_suppression_t10_2018, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_encryption_arms_race).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_ossification).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, transport_layer_surveillance_capability).

% DUAL FORMULATION NOTE:
% The RFC 9293 specification constraint family decomposes into three reading-specific constraints: (1) strict_invariance_reading (ε ≈ 0.15, Mountain from IETF perspective; specification is normative and unchanging), (2) optimization_latitude_reading (ε ≈ 0.35, Tangled Rope from network engineer perspective; middleboxes optimize within specification-intended bounds), (3) middlebox_realism_reading (ε ≈ 0.58, Snare from endpoint perspective; deployed behavior is normative). Each reading has its own epsilon, its own perspectives, and its own classification. The family is linked via network.affects_constraints: strict invariance affects optimization latitude (if invariance is false, latitude is necessary) and middlebox realism (if invariance fails, realism describes what actually happens). The epsilon values differ by a factor of 3.8 because they describe different structural claims about specification authority, not different measurements of the same claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
