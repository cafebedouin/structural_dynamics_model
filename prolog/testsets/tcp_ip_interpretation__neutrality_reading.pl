% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_neutrality_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle: Neutrality Reading
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The TCP/IP end-to-end principle is contested as a kernel — multiple
 *   readings coexist in regulatory, technical, and policy domains. This
 *   constraint story instantiates the NEUTRALITY READING: TCP/IP embodies a
 *   requirement for non-discrimination among applications and content
 *   providers. Under this reading, ISPs must forward packets regardless of
 *   application identity, source, or destination; they cannot charge content
 *   providers for priority or offer paid fast lanes; innovation at the
 *   network edge is protected from gatekeeping by ISP operators. The
 *   neutrality reading has been the dominant regulatory interpretation in the
 *   US (2015 FCC Open Internet Order), EU (Net Neutrality Directive
 *   2015/2120), and India (DoT Prohibition Orders 2016, 2020), though
 *   enforcement has been contested and technical workarounds (zero-rating,
 *   DPI-based traffic shaping) have eroded its practical force. This reading
 *   exists in tension with the PRIORITIZATION READING (TCP/IP permits ISPs to
 *   manage network resources by prioritizing certain applications) and the
 *   ZERO-RATING READING (TCP/IP permits ISPs to exempt certain applications
 *   from data caps without violating non-discrimination). The constraint
 *   exhibits the six DR types from different actor perspectives, revealing
 *   both the coordination function (managing shared infrastructure) and the
 *   extraction tension (ISP revenue optimization constrained by neutrality
 *   rules).
 *
 * KEY AGENTS:
 *   - Edge Developers: Powerless/trapped (powerless/biological/trapped/global) — small innovators depend entirely on ISP infrastructure with no alternative; bear suppression costs from throttling and gray-box discrimination
 *   - Large Content Platforms: Powerful/mobile (powerful/biographical/mobile/global) — Netflix, Google, Facebook negotiate exemptions and zero-rating deals; benefit from neutrality rules against ISP extraction but also benefit from platform-specific exemptions that smaller competitors cannot access
 *   - Open Internet Coalition: Institutional/arbitrage (institutional/immediate/arbitrage/global) — ISOC, EFF, Mozilla, open-source communities; see neutrality as coordination mechanism enabling low-friction ecosystem; can exit advocacy by supporting alternative frameworks but choose alignment with open principles
 *   - ISP Operators: Institutional/constrained (institutional/biographical/constrained/national) — Comcast, AT&T, Vodafone, etc.; constrained by neutrality rules preventing price discrimination and paid prioritization; can lobby to relax rules but face reputational/political cost
 *   - Telecom Regulators: Institutional/arbitrage (institutional/generational/arbitrage/national) — FCC, BEREC, DoT; maintain neutrality enforcement while permitting gray-area exemptions; authority degraded by technical complexity and ISP workarounds
 *   - Alternative Infrastructure Movement: Organized/constrained (organized/generational/constrained/global) — Freifunk, NYC Mesh, municipal broadband, satellite operators; see neutrality as temporary scaffold while alternative networks mature; sunset logic built in
 *   - Analytical Observer: Analytical/analytical (analytical/civilizational/analytical/universal) — risks naturalizing a particular organizational choice (non-discriminating interconnection) as inherent TCP/IP law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.52).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle: Neutrality Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'fe51796f-3dc3-467c-8931-5cc39fd3d61e').
narrative_ontology:cs_kernel_codification('fe51796f-3dc3-467c-8931-5cc39fd3d61e', formalized).
narrative_ontology:cs_authority_grounding('fe51796f-3dc3-467c-8931-5cc39fd3d61e', lineage).
narrative_ontology:cs_interpretation_layer_present('fe51796f-3dc3-467c-8931-5cc39fd3d61e').
narrative_ontology:cs_reading_relation('fe51796f-3dc3-467c-8931-5cc39fd3d61e', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe51796f-3dc3-467c-8931-5cc39fd3d61e', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('fe51796f-3dc3-467c-8931-5cc39fd3d61e', foundational, isp_non_discrimination_requirement).
narrative_ontology:cs_axiom_status(isp_non_discrimination_requirement, holdable).
narrative_ontology:cs_axiom_grounding('fe51796f-3dc3-467c-8931-5cc39fd3d61e', isp_non_discrimination_requirement, deontological).
narrative_ontology:cs_axiom('fe51796f-3dc3-467c-8931-5cc39fd3d61e', foundational, network_layer_application_layer_independence).
narrative_ontology:cs_axiom_status(network_layer_application_layer_independence, holdable).
narrative_ontology:cs_axiom_grounding('fe51796f-3dc3-467c-8931-5cc39fd3d61e', network_layer_application_layer_independence, empirically_contingent).
narrative_ontology:cs_axiom('fe51796f-3dc3-467c-8931-5cc39fd3d61e', secondary, edge_innovation_protection).
narrative_ontology:cs_axiom_status(edge_innovation_protection, holdable).
narrative_ontology:cs_axiom_grounding('fe51796f-3dc3-467c-8931-5cc39fd3d61e', edge_innovation_protection, instrumental).
narrative_ontology:cs_reference_frame('fe51796f-3dc3-467c-8931-5cc39fd3d61e', open_internet_baseline).
narrative_ontology:cs_drift_state('fe51796f-3dc3-467c-8931-5cc39fd3d61e', contemporary_post_2015, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe51796f-3dc3-467c-8931-5cc39fd3d61e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, open_internet_ecosystem).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, isp_revenue_optimization).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, network_management_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EDGE DEVELOPERS (SNARE) — Small innovators depend entirely on ISP infrastructure with no ability to negotiate terms. Neutrality rules protect against discrimination but enforcement is weak; ISPs can implement gray-box throttling, zero-rating exemptions for preferred applications, or subtle QoS manipulation that violates the spirit of non-discrimination while remaining technically compliant. Trapped agents experience high suppression (cannot build alternative infrastructure) and extract no benefit from neutrality enforcement when surveillance-based differentiation replaces explicit blocking.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LARGE CONTENT PLATFORMS (TANGLED ROPE) — Benefit from neutrality rules that prevent ISPs from extracting rent at interconnection points (e.g., Netflix pays Comcast for preferential peering). But large platforms also benefit from zero-rating exemptions negotiated with ISPs (Netflix Free in India, Facebook Free Basics) and from their ability to absorb costs that smaller competitors cannot. The constraint enforces non-discrimination while simultaneously creating bargaining positions through technical/economic differentiation. Mixed extraction: ISPs cannot legally demand payment for non-discrimination, but platforms pay for preferred interconnection anyway (booked as 'CDN costs'). Agents with mobility and market power experience moderate extraction.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: OPEN INTERNET COALITION (ROPE) — Public-interest institutions (ISOC, EFF, Mozilla, open-source communities) benefit from neutrality rules as coordination mechanism: the rules state the principle that governs interconnection disputes and enables low-friction innovation. The constraint is fundamentally a coordination function from this perspective — it solves the collective-action problem of managing shared infrastructure. Arbitrage position: coalition actors can exit by advocating for alternative regulatory frameworks but choose to maintain neutrality because it aligns with their epistemic commitments. Low effective extraction.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISP OPERATORS (TANGLED ROPE) — Neutrality rules constrain revenue optimization (cannot charge content providers for priority or offer paid fast lanes) while simultaneously creating a coordination infrastructure that enables the ISP to operate a shared network serving all applications equally. The constraint solves the routing/congestion coordination problem. But it extracts from ISPs by preventing price discrimination that would fund network upgrades. ISPs can exit by lobbying to relax rules (constrained exit — high political/reputational cost) or by deploying exemptions (zero-rating, sponsored data) that technically comply but undermine the spirit. Moderate extraction from a regulated incumbent with constrained exit options.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TELECOM REGULATORS (PITON) — The end-to-end principle and neutrality enforcement have become increasingly performative as ISPs deploy technical workarounds that are difficult to police: zero-rating (exempting content platforms' traffic from data caps), sponsored data programs, application-layer traffic optimization, and DPI-based management. Regulators declare neutrality while permits exemptions that undermine it. Theater ratio high because enforcement focuses on high-visibility cases (Netflix throttling) while permitting systematic gray-area discrimination that serves ISP revenue interests. Regulatory authority degraded — maintained through institutional inertia but functionally compromised by technical complexity and ISP economic pressure.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE INFRASTRUCTURE MOVEMENT (SCAFFOLD) — Community networks (Freifunk, NYC Mesh), municipal broadband, and satellite internet (Starlink, OneWeb) are building parallel infrastructure that bypasses ISP gatekeeping entirely. These agents see neutrality rules as temporary coordination scaffolding while genuinely alternative networks mature. The sunset is built-in: if viable alternative infrastructure exists, neutrality enforcement becomes less critical. Constrained exit — high barriers to deployment (fiber costs, spectrum regulations, capital) but a visible technical path forward. Theater low because these networks operate on genuinely different principles (non-profit, cooperative, open).
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, packet routing on shared infrastructure inherently requires non-discrimination at the physical layer: packets cannot be routed based on application identity while maintaining the logical independence of the network layer from the application layer. This separation of concerns is presented as a technological law — the OSI model layers are immutable. However, the structural data contradicts the mountain classification: ISPs actively discriminate through DPI (deep packet inspection), traffic shaping, and prioritization, proving the 'law' is not immutable. The neutrality reading naturalizes a particular organizational choice (non-discriminating interconnection) as if it were inherent to TCP/IP architecture itself.
constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_ip_interpretation__neutrality_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, TR),
    TR >= 0.70.

:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The neutrality reading prohibits ISPs from directly charging content providers for priority or from offering paid fast lanes — these are explicit extraction mechanisms ruled out by the constraint. However, neutrality enforcement has created a secondary extraction layer: large platforms navigate zero-rating and sponsored data exemptions (negotiated benefits); small innovators bear the full suppression costs of throttling and algorithmic discrimination that remain undetected. ISPs extract through subtle mechanisms (DPI prioritization, zero-rating exemptions) rather than explicit price discrimination. The trajectory over 16 years shows increasing extractiveness (0.35 → 0.52) as technical workarounds proliferate and enforcement capacity fails to keep pace. Suppression (0.58): Moderate-high and increasing. Barriers to edge innovation include: (a) throttling and latency discrimination by ISPs for non-exempted applications; (b) inability of small developers to negotiate zero-rating exemptions; (c) capital/political barriers to deploying alternative infrastructure; (d) surveillance-based differentiation that is technically difficult to police. Suppression requirement increasing (0.48 → 0.58) reflects that maintaining the neutrality constraint requires stronger detection and enforcement mechanisms as technical workarounds become more sophisticated. Theater ratio (0.65): Moderate-high and increasing. Regulatory enforcement is increasingly performative: high-visibility cases (Netflix throttling settlements) receive attention while systematic gray-area discrimination (zero-rating exemptions, DPI-based QoS) persists. The regulatory theater has grown (0.42 → 0.65) because enforcement relies on complaint-based mechanisms and public cases while the actual discrimination mechanisms operate at the technical layer beneath regulatory visibility. The constraint functions partly as a Piton — maintained through institutional inertia (regulators must defend neutrality rules) but increasingly degraded by technical evasion.
 *
 * PERSPECTIVAL GAP:
 *   The seven perspectives reveal a presheaf of different classifications depending on observation position. Edge developers see pure extraction (Snare) — throttling and suppression with no offsetting benefit. Large platforms see mixed extraction-coordination (Tangled Rope) — the constraint prevents ISP gatekeeping but also enables platforms to benefit from zero-rating and sponsored data exemptions. The open internet coalition sees pure coordination (Rope) — solving the shared-infrastructure problem. ISPs see mixed constraint-and-coordination (Tangled Rope) — the rules prevent revenue extraction but enable predictable routing infrastructure. Regulators see a degrading ritual (Piton) — enforcement persists through institutional inertia while actual discrimination continues undetected. Alternative infrastructure builders see temporary coordination (Scaffold) — rules provide a coordination baseline during the transition to alternative networks. The analytical observer risks seeing an immutable TCP/IP law (Mountain) — but the structural data contradicts this: ISPs actively discriminate through DPI and prioritization, proving non-discrimination is not inherent to the protocol itself. The false summit analysis reveals that 'non-discrimination as inherent TCP/IP requirement' naturalizes a particular regulatory choice (requiring ISPs to be non-discriminating common carriers) as if it were a technical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: their power level, exit options, and relationship to the extraction flow. Edge developers with no exit (trapped/powerless) derive d ≈ 1.0 (full target) producing high f(d) ≈ 1.42 and thus high experienced extraction chi. Large platforms with arbitrage options and negotiating capacity derive d ≈ 0.35 (partial beneficiary) producing low f(d) ≈ 0.25 and thus low experienced extraction. ISPs with constrained exit (cannot easily exit the market but can lobby rules) derive d ≈ 0.65 (partial target) producing moderate f(d) ≈ 1.0. The open coalition with genuine arbitrage (can exit by supporting alternative frameworks) derives d ≈ 0.10 (net beneficiary) producing low f(d) ≈ 0.05. These directionality differences explain the perspectival gap: the constraint's experienced extractiveness varies by a factor of 10+ depending on the observer's structural position, even though the base extractiveness (0.52) is constant.
 *
 * MANDATROPHY ANALYSIS:
 *   The neutrality reading resolves the mandatrophy by clarifying what COUNT as the 'constraint' and what count as 'benefits to some agents.' The constraint is: ISPs must forward all packets non-discriminatorily. The beneficiaries (edge innovators, open-source communities, users) experience this as coordination: predictable routing enables innovation without ISP gatekeeping. The victims (ISP revenue optimization, network management flexibility) experience this as extraction: ISPs cannot charge for priority or implement application-specific pricing. The tension is not between 'is this coordination or extraction' but between 'which agents' interests count as coordination and which count as extraction?' From the platform perspective, the constraint partly enables extraction (zero-rating exemptions create competitive advantages). From the ISP perspective, the constraint extracts by preventing price discrimination. From the edge developer perspective, the constraint barely functions because enforcement is weak and suppression mechanisms (DPI throttling) operate beneath regulatory visibility. The mandatrophy resolves by recognizing that the neutrality reading is a policy choice about WHO gets to extract (platforms via exemptions vs ISPs via prioritization pricing). The constraint redistributes extraction from ISPs to platforms, rather than eliminating it. The Tangled Rope classification captures this: genuine coordination function (non-discriminating routing) embedded in asymmetric extraction (ISP revenue prevented, platform exemptions enabled). The theater ratio (0.65) captures the performative element: regulatory enforcement focuses on visible violations while the actual extraction mechanisms (zero-rating, DPI-based QoS) persist in the technical infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_compliance_ambiguity,
    'Do zero-rating programs and sponsored data comply with or violate the non-discrimination principle of TCP/IP neutrality?',
    'Regulatory determination of whether exempting specific applications from data caps constitutes content-based discrimination or network management. Analysis of whether traffic from exempted applications receives preferential treatment or merely avoids capacity-based penalties.',
    'If zero-rating violates neutrality: extraction measured as the revenue ISPs capture by exempting platform partners; suppression increases for non-exempted innovators. If zero-rating complies: extraction decreases significantly; suppression only applies to platforms outside exemption agreements. Classification could shift from Tangled Rope (both platforms and ISPs) to Rope (platforms as beneficiaries) depending on interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zero_rating_compliance_ambiguity, conceptual, 'Whether zero-rating and sponsored data programs comply with neutrality principle').

omega_variable(
    dpi_enforcement_detection,
    'Can regulating authorities reliably detect and enforce against ISP discrimination implemented through deep packet inspection (DPI) and algorithmic traffic shaping?',
    'Technical auditing of ISP networks; comparison of measured QoS metrics (latency, jitter, throughput) for identical traffic classes differing only in application identity; documentation of DPI rules and their deployment.',
    'If detection is reliable: theater ratio decreases; enforcement becomes structural rather than performative. If detection is unreliable: theater ratio remains high (regulatory theater persists while actual discrimination continues); suppression of edge innovators increases because violation occurs without consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dpi_enforcement_detection, empirical, 'Feasibility of enforcing neutrality against covert ISP discrimination').

omega_variable(
    kernel_reading_conflict,
    'Does the neutrality reading''s core premise (non-discrimination as a TCP/IP requirement) logically foreclose the prioritization reading''s core premise (ISP-controlled prioritization as a TCP/IP feature)?',
    'Examine whether the TCP/IP specification itself mandates or permits prioritization. Analyze whether both readings can be held simultaneously within a single technical and regulatory framework.',
    'If they foreclose each other: one reading must eventually dominate (reclassification event). If they coexist: the constraint involves genuine structural tension between incompatible technical commitments. If the prioritization reading influences the neutrality reading: the constraint is degrading from Tangled Rope toward Piton as prioritization workarounds accumulate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_conflict, conceptual, 'Logical relationship between neutrality and prioritization readings of TCP/IP').

omega_variable(
    edge_vs_core_extraction_asymmetry,
    'Is the measured extraction (0.52) dominated by ISP extraction from large platforms (easier to measure, subject to regulatory attention) or by more subtle extraction from edge innovators (harder to measure, lower-visibility)?',
    'Decompose extraction into: (a) ISP-to-platform negotiation (peering, zero-rating exemptions); (b) ISP-to-innovator suppression (throttling of unpopular applications, degradation of background traffic); (c) platform-to-innovator gatekeeping (large platforms can navigate zero-rating; small innovators cannot). Measure each component separately.',
    'If edge extraction dominates: true suppression is higher than measured (0.58); beneficiary structure shifts toward large platforms only. If platform extraction dominates: ISP regulation is working for large actors but failing for edge developers; two separate constraints may be operating. If roughly balanced: the tangled rope classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edge_vs_core_extraction_asymmetry, empirical, 'Decomposition of extraction across ISP-platform and platform-developer relationships').

omega_variable(
    reading_axiom_ground_shift,
    'Has the grounding of the non-discrimination axiom shifted from deontological (net neutrality as a right) to instrumental (net neutrality as an efficiency condition)?',
    'Historical analysis of regulatory justifications, legislative intent, and advocacy rhetoric: Are advocates claiming neutrality as inherent right to access? Or as condition for innovation efficiency? Analysis of FCC orders from 2015, 2017, and 2020 showing stated rationale for neutrality.',
    'If deontological grounding dominates: the axiom is resistant to empirical challenge (violation of rights persists despite efficiency arguments). If instrumental grounding dominates: the axiom forecloses if efficiency evidence contradicts neutrality (e.g., if prioritization demonstrably improves network outcomes). Grounding type determines which drift vectors (axiom_overriding, authority_erosion) apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_axiom_ground_shift, conceptual, 'Shift in normative grounding of non-discrimination axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcpneutrality_theater_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tcpneutrality_theater_t8, tcp_ip_interpretation__neutrality_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(tcpneutrality_theater_t16, tcp_ip_interpretation__neutrality_reading, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(tcpneutrality_extract_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcpneutrality_extract_t8, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(tcpneutrality_extract_t16, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 16, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tcpneutrality_suppress_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(tcpneutrality_suppress_t8, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(tcpneutrality_suppress_t16, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, isp_zero_rating_exemptions).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, dpi_traffic_shaping_enforcement).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, platform_gatekeeping_dynamics).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel is instantiated by three constraint stories: neutrality_reading (this file, ε=0.52), prioritization_reading (sibling, ε≈0.45), and zero_rating_reading (sibling, ε≈0.60). Each reading has different epsilon values reflecting different empirical status and different extraction mechanisms. They are not alternative perspectives on a single constraint — they are structurally distinct constraints grounded in different axioms and different beneficiary/victim relationships. Neutrality_reading enforces non-discrimination at ISP level, protecting edge innovation; prioritization_reading permits application-aware routing at ISP level, enabling network management; zero_rating_reading permits data-cap exemptions, creating platform favoritism. These are three structurally incommensurable regulatory choices, not three ways of measuring the same underlying constraint. All three are live in the current regulatory landscape (different countries, different ISPs, different eras of FCC rule-making).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
