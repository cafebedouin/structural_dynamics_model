% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__prioritization_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Differentiated Service Quality (Prioritization Reading)
 *   domain: technology_governance/internet_policy
 *
 * SUMMARY:
 *   This constraint instantiates the prioritization reading of the TCP/IP
 *   interpretation kernel. The reading asserts that TCP/IP's layered
 *   architecture and absence of explicit non-discrimination mandates permit
 *   Internet Service Providers to implement differentiated service quality:
 *   paid fast lanes for prioritized content, deprioritization for non-paying
 *   services, and traffic shaping to manage congestion. The reading justifies
 *   this interpretation by appealing to network management necessity,
 *   infrastructure investment incentives, and the technical validity of QoS
 *   (Quality of Service) as a congestion-management tool. This is one of
 *   three contested readings of the same TCP/IP kernel; the
 *   neutrality_reading asserts that the end-to-end principle forbids
 *   discrimination; the zero_rating_reading permits selective exemptions for
 *   sponsored content. Each reading produces a different constraint with
 *   different beneficiary/victim structures and different ε values. The
 *   prioritization reading instantiated here produces substantial extraction
 *   (ε=0.68) because the constraint benefits ISPs and well-capitalized
 *   content providers at the cost of unfunded services and startups. Theater
 *   ratio (0.41) reflects that while congestion management is a real
 *   coordination function, an increasing fraction of enforcement activity
 *   defends the fast-lane revenue model rather than managing actual
 *   congestion.
 *
 * KEY AGENTS:
 *   - tier1_isps: Institutional agenda-setter (powerful, arbitrage exit). Controls last-mile infrastructure; interprets TCP/IP as permitting prioritization; implements DPI and traffic shaping to enforce it.
 *   - content_providers_with_capital: Institutional beneficiary (powerful, arbitrage exit). Can afford fast lanes; benefits from guaranteed throughput; pays ISPs for priority access.
 *   - unfunded_edge_services: Powerless payers (trapped exit). Non-profits, educational platforms, community networks. Deprioritized; no capital to afford fast lanes; no leverage to negotiate.
 *   - startups_without_fast_lane_access: Moderate payers (constrained exit). Cannot afford or scale to justify fast-lane fees. Structurally disadvantaged relative to well-capitalized competitors.
 *   - users_on_congested_networks: Powerless payers (identity-locked exit). Geographic and service-bound to ISP choice. Bear deprioritization costs; cannot individually afford fast lanes.
 *   - regulators: Institutional observers (analytical seat). Can interpret the TCP/IP kernel and issue policy that constrains or permits the prioritization reading.
 *   - end_to_end_principle_advocates: Excluded organized actors (constrained exit). Academic researchers and civil society. Argue the reading violates TCP/IP's foundational architecture. Not at the table where rules are set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.71).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Differentiated Service Quality (Prioritization Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'cfa16476-5682-4ba9-8cd0-872122379ba6').
narrative_ontology:cs_kernel_codification('cfa16476-5682-4ba9-8cd0-872122379ba6', fixed_text).
narrative_ontology:cs_authority_grounding('cfa16476-5682-4ba9-8cd0-872122379ba6', extraction).
narrative_ontology:cs_interpretation_layer_present('cfa16476-5682-4ba9-8cd0-872122379ba6').
narrative_ontology:cs_reading_relation('cfa16476-5682-4ba9-8cd0-872122379ba6', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('cfa16476-5682-4ba9-8cd0-872122379ba6', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('cfa16476-5682-4ba9-8cd0-872122379ba6', foundational, tcp_ip_permits_service_differentiation).
narrative_ontology:cs_axiom_status(tcp_ip_permits_service_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('cfa16476-5682-4ba9-8cd0-872122379ba6', tcp_ip_permits_service_differentiation, conventional).
narrative_ontology:cs_axiom('cfa16476-5682-4ba9-8cd0-872122379ba6', secondary, congestion_management_requires_prioritization).
narrative_ontology:cs_axiom_status(congestion_management_requires_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('cfa16476-5682-4ba9-8cd0-872122379ba6', congestion_management_requires_prioritization, empirically_contingent).
narrative_ontology:cs_reference_frame('cfa16476-5682-4ba9-8cd0-872122379ba6', isp_network_management_authority).
narrative_ontology:cs_drift_state('cfa16476-5682-4ba9-8cd0-872122379ba6', post_fcc_classification_reversal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cfa16476-5682-4ba9-8cd0-872122379ba6', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, tier1_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, startups_without_fast_lane_access).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, users_on_congested_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, users_on_congested_networks).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, network_equipment_manufacturers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the physical last-mile infrastructure and packet-routing equipment. Under the prioritization reading, they interpret TCP/IP's layered architecture as permitting service differentiation based on application type, sender identity, or willingness to pay. They argue that differential QoS is necessary for managing congestion, ensuring critical services (VoIP, emergency systems) get priority, and funding network upgrades. They implement fast lanes, charge content providers for priority routing, and set enforcement through DPI (deep packet inspection) and traffic shaping.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, tier1_isps, agenda_setter,
    institutional, generational, arbitrage, national).

% Large platforms (streaming, social media, video conferencing) can afford to purchase fast-lane access from ISPs. They benefit from guaranteed throughput and reduced latency for their services, which improves user experience and competitive advantage. They pay the ISPs' prioritization fees as a new cost of doing business at scale. The constraint benefits them more than it costs them because they have the capital to negotiate and the user base to justify the expense.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital, payer).

% Non-profit services, educational platforms, health-information systems, and community networks that cannot afford prioritization fees. Their traffic is deprioritized on congested networks, degrading service quality and user experience. They have no exit: they cannot move to alternative infrastructure, cannot afford fast lanes, and have no leverage to negotiate with ISPs. They are trapped by the constraint.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    powerless, immediate, trapped, global).

% Early-stage companies building internet services lack both the capital to purchase fast lanes and the scale to absorb network degradation. They compete on a tilted playing field where fast-lane access is a fixed cost they cannot afford, putting them at a structural disadvantage relative to well-capitalized competitors. Exit is theoretically possible (pivot to a non-network-dependent business model) but practically very constrained for a cohort whose entire value proposition depends on network reliability.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, startups_without_fast_lane_access, payer,
    moderate, biographical, constrained, global).

% End users whose ISP implements prioritization. They bear the costs of deprioritization when using non-paying services, experiencing degraded access to unfunded content while fast-lane services work smoothly. The identity lock is geographic and service-dependent: they cannot easily switch ISPs (geographic monopoly), cannot pay for fast lanes individually (cost prohibitive), and their internet identity is bound to their location and provider choice. Some experience a genuine benefit if critical services they depend on (emergency, medical) get priority.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, users_on_congested_networks, payer,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, users_on_congested_networks, beneficiary).

% Companies that build DPI (deep packet inspection), traffic-shaping hardware, and QoS management systems benefit from ISP investment in differentiation infrastructure. Prioritization reading justifies capital spending on new equipment; neutral reading would require far less sophisticated routing gear.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, network_equipment_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% National and supranational authorities (FCC, EU, etc.) interpret TCP/IP governance and set policy on permissible service differentiation. They have analytical authority to read the TCP/IP kernel and issue orders constraining or permitting the prioritization interpretation. They are not trapped in the constraint; they are positioned to potentially overturn it.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, regulators, observer,
    institutional, generational, analytical, national).

% Academic researchers, network architects, and civil society groups who argue that TCP/IP's design embeds the end-to-end principle (dumb network, smart endpoints) and that prioritization violates this foundational architecture. They are structurally excluded from ISP governance: they have no seat at the table where traffic-shaping rules are set, their objections are not binding, and the prioritization reading silences their interpretation by asserting its own.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, end_to_end_principle_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, tier1_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages network congestion and ensures critical services (VoIP, emergency, medical monitoring) get reliable quality of service. Allocates scarce bandwidth according to application priority and, under this reading, according to ability and willingness to pay. Funds ISP network upgrades and reinvestment by generating revenue from content providers.
% TRANSFER_FUNCTION: Moves capital from content providers and their users (via degraded service quality for non-paying alternatives) to ISPs and network equipment manufacturers. Content providers with capital purchase fast-lane access; startups and unfunded services absorb the deprioritization cost in the form of degraded user experience.
% ABSENT_VOICES: End-to-end principle advocates, unfunded service providers, and the research community that designed TCP/IP are structurally excluded from ISP governance. They would argue that prioritization violates the architectural principle that kept TCP/IP open to innovation. Their objections are organized but not binding on ISP implementation.
% DISAPPEARANCE_RATIONALE: If prioritization disappeared and the neutrality reading were enforced, ISPs would lose fast-lane revenue, content providers would lose their QoS guarantee purchase option, and network investment incentives would shift. The internet ecosystem would reorganize around different funding models (likely more government infrastructure spending, different service tiers, or usage-based pricing for all rather than priority-based pricing).
% FOUNDING_PROBLEM: Rapid growth in bandwidth demand, especially from video streaming and real-time applications, created network congestion. ISP investment in infrastructure was needed but returns were uncertain. Prioritization reading offered a solution: allow ISPs to extract value from high-demand services to fund upgrades, and use traffic differentiation to manage congestion.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and equipment manufacturers attest the founding problem is live and unsolved without prioritization. Network engineers outside ISPs argue the problem is solvable through other means (more fiber deployment, different pricing, regulatory infrastructure investment); academic network architects argue prioritization created a new problem (innovation gatekeeping) worse than the original problem. Testimony from startup founders and unfunded service operators, recorded in regulatory dockets and academic studies, supports the assessment that the founding problem has shifted: initial congestion was addressed; ongoing prioritization now extracts from unfunded services without congestion justification.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__prioritization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__prioritization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs steeply from t=0 (0.48) to t=8 (0.60), then more gradually to plateau at t=17 (0.68). The early climb reflects ISPs' growing deployment of DPI and traffic-shaping capabilities; the plateau reflects market saturation and regulatory ceiling effects (FCC and EU investigations create enforcement cost that rises faster than extraction value). Suppression follows the same trajectory: early climb (0.52→0.65 at t=8) then slower rise to plateau (0.71 at t=21). The suppression plateau is high because maintaining the prioritization reading requires actively excluding alternative interpretations—DPI hardware itself is a suppression mechanism that prevents unfunded services from accessing equivalent throughput even in non-congested periods. Theater ratio starts low (0.22, mostly genuine congestion management) but climbs to 0.41 by t=12 as the constraint matures and ISPs shift from congestion management justification to revenue optimization. The plateau at 0.41 suggests that 41% of the enforcement activity is performative (maintaining the fast-lane revenue mechanism, not managing actual congestion) while 59% is still functional coordination. The measurements sit on one shared time grid (t=0,4,8,12,17,21,25); every metric is authored at every time point. The trajectory shows extraction accumulation without theater collapse—characteristic of a maturing tangled rope where the coordination function persists (theaters don't rise above ~0.5) but extraction grows as the constraint's value is realized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seat should experience dramatically different constraint types. From the ISP seat (agenda-setter, powerful, arbitrage exit), the constraint is rope-like: a coordination mechanism they maintain, which they benefit from, with multiple exit options (regulation could force a different reading; they can pivot to usage-based pricing). From the unfunded-service seat (powerless, trapped), the constraint is snare-like: no choice in entry, no exit, pure extraction via deprioritization. From the regulator seat, the constraint's type depends on which reading they choose to enforce. The engine computes this per-seat divergence from the structural data; the commentary explains why it arises (power asymmetry, exit differentiation, beneficiary concentration). The authored claim (tangled_rope) sits between these poles: it is the structure that most parties experience, where both coordination and extraction are present but distributed unevenly.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs (agenda_setter) are full beneficiaries: they set the rules, collect the revenue, and have arbitrage-exit options (they can shift to alternative revenue models if regulation changes). Their directionality d is near 0.0. Content providers with capital are near-symmetric or slight beneficiaries (d≈0.3-0.4): they benefit from fast lanes more than they pay in fees (they can pass fees to users, and fast-lane access creates competitive advantage), but they do depend on ISP goodwill and cannot exit the internet. Unfunded services and startups are full targets (d≈1.0): they pay (via deprioritization, inability to afford fast lanes) and cannot exit. Users on congested networks are high-target (d≈0.85): they pay via degraded service quality and geographic/service lock-in, though critical-service prioritization provides some benefit (identity lock is geographic and service-dependent, trapping them). The engine derives these values from the declared beneficiary/victim structure: ISPs benefit, unfunded services and startups are victims; exit options differentiate the targets (startups have slightly more exit via pivoting; unfunded services have none). This directionality distribution—concentrated benefit for ISPs and equipment makers, distributed cost for unfunded services and users—is characteristic of tangled rope: genuine coordination (congestion management) paired with asymmetric extraction (prioritization fees).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (congestion management, infrastructure investment) is contested in status. ISPs say it is live; unfunded-service operators and startup founders say it has shifted—initial congestion was addressed through fiber deployment and more efficient protocols; ongoing prioritization now extracts beyond what congestion management requires. The disappearance verdict is world_rearranges: removing the prioritization reading and enforcing neutrality would reorganize the internet economy (different funding sources, different service tiers, different investment incentives). The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the mandatrophy signal: the constraint persists even though its founding problem is contested, suggesting either a dead problem being extracted from (mandatrophy candidate) or a genuinely live problem that payers deny for political reasons. The theater ratio plateau at 0.41 supports the mandatrophy reading: 41% of the constraint's enforcement activity is performative (maintaining the fast-lane narrative) rather than managing actual congestion. A mandate to manage congestion should show theater_ratio dropping toward 0.1-0.2 (mostly functional) once congestion abates; instead, theater_ratio stays high and stable, suggesting the mandate (manage congestion) persists in narrative while the function (prevent degradation on non-paying services) has shifted to extraction. This is the hallmark of constraint-life extension without functional justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the prioritization reading a valid interpretation of TCP/IP''s architecture, or does the end-to-end principle logically foreclose it?',
    'Formal analysis of RFC documents (RFC 791, RFC 2474, RFC 3474, RFC 3168) by scholars with no stake in the outcome. Examination of TCP/IP''s original design intent from Cerf, Kahn, Baran, and subsequent Internet Society doctrine.',
    'If the reading is logically foreclosed by TCP/IP''s design, the constraint is a false interpretation and should be reclassified as a snare (pure coerced extraction) not a tangled rope. If the reading is valid, the classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the prioritization reading is a defensible interpretation of the TCP/IP kernel or a violation of its foundational architecture.').

omega_variable(
    congestion_justification_empirical,
    'How much of the current prioritization enforcement is necessary to manage actual network congestion, and how much is surplus revenue extraction?',
    'Measurement of network utilization, packet loss, and latency on unshaping ISP networks vs. ISP networks with DPI; analysis of ISP revenue per byte prioritized vs. marginal cost per byte deprioritized; testimony from ISP engineers on congestion thresholds vs. prioritization rules.',
    'If >60% of prioritization activity is necessary for congestion management, the tangled_rope classification holds and theater_ratio should be lower. If <40%, the constraint is primarily extraction and should be reclassified as snare; theater_ratio of 0.41 suggests we are near the threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_justification_empirical, empirical, 'Whether prioritization is justified by congestion management or by revenue extraction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (DPI hardware blocking unfunded services, geographic monopoly preventing exit) or partly internalized (unfunded services believing they cannot afford fast lanes even when technically possible)?',
    'Post-regulation experiment: if a jurisdiction mandates neutrality, do unfunded services immediately resume service levels (structural suppression) or does recovery lag due to entrenched assumptions (internalized)?',
    'If structural, the suppression is enforced by infrastructure; if internalized, the constraint carries psychological cost even after removal. This affects the reclassification consequence if regulation changes: structural suppression → immediate market reorganization; internalized → slow recovery requiring education and trust-building.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is enforced by ISP infrastructure or by internalized belief.').

omega_variable(
    alternative_readings_coexistence,
    'Can the prioritization reading and the neutrality reading coexist in the same legal/policy framework, or do they logically foreclose each other?',
    'Regulatory analysis: examine jurisdictions (EU, China, India) that have issued policy decrees on one reading and whether the other reading can be simultaneously held within the same authority structure.',
    'If readings foreclose each other, regulatory choice is binary (one or the other). If they coexist, multiple readings can be held by different parties in different jurisdictions, and the constraint''s type is reading-dependent (tangled_rope in prioritization jurisdictions, different type in neutrality jurisdictions). This informs whether the sibling readings are ''forecloses'' or ''coexists_with'' relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_coexistence, conceptual, 'Whether the prioritization and neutrality readings logically foreclose each other or can coexist.').

omega_variable(
    equipment_manufacturer_capture,
    'To what extent do DPI and traffic-shaping equipment manufacturers (Cisco, Sandvine, etc.) lobby for the prioritization reading to maintain their market?',
    'Analysis of industry associations, regulatory testimony, and campaign finance from equipment makers; comparison of lobbying intensity for prioritization vs. neutrality readings.',
    'If capture is substantial, the prioritization reading is partly maintained by beneficiary pressure not by genuine technical necessity or merit. This supports a piton or snare reclassification if the founding-problem congestion justification is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equipment_manufacturer_capture, empirical, 'Whether equipment manufacturers are capturing policy to maintain prioritization demand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(tcp__tr_t17, tcp_ip_interpretation__prioritization_reading, theater_ratio, 17, 0.41).
narrative_ontology:measurement(tcp__tr_t21, tcp_ip_interpretation__prioritization_reading, theater_ratio, 21, 0.41).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(tcp__be_t17, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 17, 0.67).
narrative_ontology:measurement(tcp__be_t21, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.59).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(tcp__su_t17, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 17, 0.71).
narrative_ontology:measurement(tcp__su_t21, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel decomposes into three structurally distinct constraints, one per reading. Each reading produces different beneficiary/victim sets and different ε values. The prioritization_reading permits ISP service differentiation; the neutrality_reading forbids it; the zero_rating_reading permits selective exemptions. These readings are not three ways of measuring the same constraint—they instantiate three incompatible interpretations of the TCP/IP kernel. The three files are linked via network.affects_constraints to indicate that regulatory choice of one reading affects the operative structure of the others. The epsilon values differ significantly: neutrality_reading has lower extraction (ISPs cannot extract via prioritization fees), prioritization_reading has higher extraction (fast lanes enabled), zero_rating_reading has moderate extraction (some differentiation, but not open-market prioritization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
