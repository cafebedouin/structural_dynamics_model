% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__neutrality_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle: Non-Discrimination Reading
 *   domain: technology/governance/internet_policy
 *
 * SUMMARY:
 *   TCP/IP's end-to-end principle—that intelligence should reside at network
 *   edges, not in the core—is read by network architects and public-interest
 *   advocates as requiring non-discrimination: ISPs must treat all traffic
 *   equally, regardless of source, destination, or commercial arrangement.
 *   This reading prohibits paid prioritization, sponsored data, and
 *   content-based routing decisions. ISPs and backbone operators experience
 *   this as a constraint on revenue optimization; edge innovators and users
 *   experience it as protection from gatekeeping. The neutrality reading is
 *   contested by two sibling readings: the prioritization reading (which
 *   interprets TCP/IP as permitting quality-of-service differentiation for
 *   network management) and the zero-rating reading (which permits selective
 *   exemptions for sponsored content). This constraint story instantiates
 *   only the neutrality reading, not the contest itself.
 *
 * KEY AGENTS:
 *   - Edge innovators (moderate power, global scope): benefit from non-discrimination; cannot be locked out by ISP deals
 *   - Internet users (powerless, constrained exit): benefit from open access; depend on ISPs for physical connectivity
 *   - ISPs and backbone operators (institutional power, regional scope): bear the constraint; prohibited from revenue optimization through discrimination
 *   - Telecommunications regulators (institutional power, national scope): enforce the reading through interconnection rules and regulatory intervention
 *   - Legacy telecom carriers (institutional power, trapped exit): excluded from relaxing the rule despite conflicting incentives
 *   - Public-interest advocates (organized power, analytical horizon): defend the reading against ISP claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.71).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle: Non-Discrimination Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology/governance/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '3ce29406-62df-4a91-9694-ec73300b0da4').
narrative_ontology:cs_kernel_codification('3ce29406-62df-4a91-9694-ec73300b0da4', fixed_text).
narrative_ontology:cs_authority_grounding('3ce29406-62df-4a91-9694-ec73300b0da4', lineage).
narrative_ontology:cs_interpretation_layer_present('3ce29406-62df-4a91-9694-ec73300b0da4').
narrative_ontology:cs_reading_relation('3ce29406-62df-4a91-9694-ec73300b0da4', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ce29406-62df-4a91-9694-ec73300b0da4', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('3ce29406-62df-4a91-9694-ec73300b0da4', foundational, end_to_end_principle_prohibits_discrimination).
narrative_ontology:cs_axiom_status(end_to_end_principle_prohibits_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('3ce29406-62df-4a91-9694-ec73300b0da4', end_to_end_principle_prohibits_discrimination, deontological).
narrative_ontology:cs_axiom('3ce29406-62df-4a91-9694-ec73300b0da4', foundational, network_intelligence_belongs_at_edges).
narrative_ontology:cs_axiom_status(network_intelligence_belongs_at_edges, holdable).
narrative_ontology:cs_axiom_grounding('3ce29406-62df-4a91-9694-ec73300b0da4', network_intelligence_belongs_at_edges, empirically_contingent).
narrative_ontology:cs_reference_frame('3ce29406-62df-4a91-9694-ec73300b0da4', architecture_permissionless_edges).
narrative_ontology:cs_drift_state('3ce29406-62df-4a91-9694-ec73300b0da4', post_mobile_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3ce29406-62df-4a91-9694-ec73300b0da4', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_startups).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, backbone_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, content_delivery_networks).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_architectural_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, network_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% New applications and services can reach users without negotiating preferential treatment or paying ISP intermediaries for transport quality. Innovation can occur at the edges of the network without requiring infrastructure investment or ISP approval. They capture user value directly through the network without rent extraction at the transport layer.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    moderate, biographical, arbitrage, global).

% Receive equal treatment for all content regardless of source or commercial relationship. Access to edge-innovated services is not gatekept by ISP business relationships. The constraint protects user choice and access to the full internet, though users remain dependent on ISPs for physical connectivity.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    powerless, biographical, constrained, global).

% Bear the cost of backbone infrastructure investment but are prohibited from extracting revenue by prioritizing traffic. Cannot differentiate service quality based on content origin or commercial arrangement. This reading constrains their ability to monetize the network beyond standard access fees. They argue non-discrimination prevents them from recovering infrastructure costs from those who generate peak demand.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, isps, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, isps, agenda_setter).

% Cannot pay ISPs for prioritized delivery of their content. Must rely on standard routing regardless of congestion. However, they benefit from the constraint in that large content firms cannot use exclusive deals to lock out competitors; the playing field remains level for transport.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_delivery_networks, payer,
    powerful, biographical, mobile, global).

% Enforce the non-discrimination reading through interconnection rules, common-carrier regulations, and periodic intervention in disputes. They interpret TCP/IP end-to-end principle as mandating uniform treatment. Their enforcement machinery blocks ISP attempts to create paid prioritization schemes or content-based gatekeeping.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecommunications_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Operate on declining voice and SMS revenue and would benefit from paid prioritization of their own VoIP/video services, or from extracting fees from content firms. The constraint prevents this revenue escape route. They are structurally excluded from relaxing the rule because they are simultaneously ISPs and legacy-business operators—a conflict that regulators manage by holding them to the non-discrimination reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, legacy_telecom_carriers, excluded,
    institutional, generational, trapped, regional).

% Operate intercontinental fiber and switching infrastructure. The constraint prevents them from capturing value through differential routing even when they upgrade capacity. They bear the fixed cost of infrastructure while the constraint prevents the variable revenue optimization strategy (paid prioritization). They argue congestion management requires price signals for traffic sources.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, backbone_operators, payer,
    powerful, generational, constrained, global).

% Monitor and defend the non-discrimination reading as a structural condition for democratic access to information. They argue that allowing ISP gatekeeping would recreate the broadcast era's scarcity model on what should remain an open transport layer. They provide countervailing expert testimony to ISP claims about cost recovery.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, public_interest_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TCP/IP itself (the protocol stack and end-to-end architecture) solves the problem of routing packets across heterogeneous networks without a central authority dictating application design. The non-discrimination reading extends this by prohibiting ISPs from re-introducing centralized gatekeeping at the transport layer—it preserves the architectural principle that allowed the internet to be generative and permissionless.
% TRANSFER_FUNCTION: Transfers the right to earn discriminatory rents from ISPs/backbone operators to edge innovators and users. ISPs must earn revenue from access fees only, not from content prioritization, paid fast lanes, or sponsored data exemptions. The constraint moves potential ISP revenue upstream to network capacity investment (or downstream to regulatory approval processes) rather than to traffic-based extraction.
% ABSENT_VOICES: Equipment vendors who would manufacture specialized inspection/prioritization gear (deep packet inspection, application-aware routing) are excluded from the design conversation. Developing-world ISPs and rural carriers who claim they cannot serve unprofitable areas without paid prioritization are not seated at the table where the rule is enforced. Application developers in constrained bandwidth markets (satellite, low-income regions) would argue for selective exemptions but are structurally absent from standards-setting.
% DISAPPEARANCE_RATIONALE: If the non-discrimination constraint vanished, ISPs would immediately implement paid prioritization, content-based routing decisions, and zero-rating deals. The investment landscape for edge startups would shift—access to users would require negotiation with ISPs. Bandwidth-hungry and price-sensitive applications (streaming, peer-to-peer, emerging services) would face gatekeeping. Internet architecture would reorganize around ISP business relationships rather than architectural elegance, and innovation would concentrate in applications ISPs choose to favor.
% FOUNDING_PROBLEM: Early internet design faced the problem of routing without knowing content: end-to-end principle solved it by making routers 'stupid'—they forward packets based only on destination IP, not on application type or commercial value. This allowed applications to innovate without ISP permission. The founding problem was: how to build a network that does not require central gatekeeping at every layer?
% FOUNDING_PROBLEM_CORROBORATION: Network engineers and internet architects (Jon Postel, David Clark, the IETF community) established the end-to-end principle in technical standards and publications; their accounts are corroborated by 40+ years of internet innovation that followed the principle. ISPs now contest the status, claiming the founding problem (scarcity, congestion, cost recovery) has changed and requires discrimination. Independent economists and public-interest technologists corroborate the engineers' reading that the principle still solves the core problem—gatekeeping prevents rather than enables efficient allocation.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint does extract value from ISPs—it prevents them from charging for prioritization, which would be their natural monopoly rent. However, the extraction is moderate rather than severe because ISPs retain standard access-fee revenue and can still upgrade capacity. The extraction rises from 0.38 to 0.58 over the interval as regulatory enforcement tightens and ISPs' attempted workarounds (zero-rating, sponsored data) are progressively blocked. Suppression is high (0.71) because active regulatory intervention and technological barriers (deep-packet-inspection prohibition) are required to prevent ISP discrimination. Theater ratio rises from 0.25 to 0.42 as regulatory activity increases but the core architectural principle remains unchanged—the theater is in compliance-theater (audits, reporting, regulatory process) rather than functional change. Accessibility collapse is moderate (0.62): alternatives to the ISP transport layer are constrained but not absent (satellite, private networks, mesh protocols exist at higher cost). The constraint is claimed as tangled_rope because it solves a real coordination problem (preventing ISP gatekeeping that would fragment the internet) while extracting from ISPs' revenue options (genuine asymmetry between beneficiaries and payers).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (ISPs, backbone operators) compute a far more extractive and suppressive constraint than the beneficiary seats do. From the ISP seat, the constraint is enforced coercion preventing natural market pricing; from the innovator seat, it is protection from rent-seeking gatekeeping. The regulatory seat bridges the gap by holding both framings in tension—they acknowledge ISP infrastructure costs while refusing to permit extraction as a funding mechanism. The engine computes this divergence from power, exit_options, and the beneficiary/victim declarations; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   From the edge innovator and user seats: d approaches 0.0 (full beneficiary)—they receive protection from gatekeeping and pay only their standard ISP access fees. From the ISP seat: d approaches 1.0 (full target)—they lose potential high-margin revenue from discrimination and face regulatory enforcement overhead. From the backbone operator seat: d approaches 0.85 (near-target)—they must invest in capacity but cannot extract discriminatory rents. The regulatory seat (agenda_setter) computes near-symmetric (d~0.5) because they are coordinating the arrangement but not extracting from it or bearing its direct costs. This per-seat divergence is the structural signature of tangled_rope: genuine coordination (preventing fragmentation) + asymmetric extraction (ISP revenue prohibition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to build networks without central gatekeeping) remains live in this reading's view, so mandatrophy does not apply to the neutrality reading itself. However, this reading's classification as tangled_rope (rather than rope) depends on acknowledging that ISPs do bear genuine costs and that the constraint's enforcement requires active suppression. A rope reading would ignore ISP costs entirely; a snare reading would deny that any coordination happens. The tangled_rope classification preserves both the coordination function and the asymmetric extraction, avoiding both the false-summit (pretending it is mere coordination) and the over-classification (pretending it is pure coercion). The rising theater_ratio indicates increasing regulatory process intensity, but theater is not accumulating faster than the functional constraint, so mandatrophy (where theater overwhelms function) is not triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    architectural_vs_regulatory_scope,
    'Does the end-to-end principle as an architectural guideline logically require non-discrimination as a regulatory rule, or are these scope levels disjoint?',
    'Technical standards analysis: examine whether the principle''s architectural meaning (where to place functionality) entails a claim about ISP discrimination (what ISPs are permitted to do). Compare formal standards documents (RFC 3724, IAB statements) against regulatory interpretations.',
    'If disjoint, the prioritization and zero-rating readings become more structurally defensible as separate interpretations of the same principle; if entailed, the neutrality reading''s core premise forecloses the siblings. This determines whether the readings coexist or whether one forecloses others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_vs_regulatory_scope, conceptual, 'Whether architectural principles logically entail regulatory requirements or whether scope translation introduces choice.').

omega_variable(
    congestion_reality_gap,
    'Do ISPs face genuine congestion and cost-recovery constraints that require discriminatory pricing, or are these claims about scarcity overstated?',
    'Empirical: ISP infrastructure data (capacity growth vs. demand growth over time), backbone operator cost structures, and congestion metrics. Did paid prioritization pilots (when allowed) reduce congestion or merely shift revenue without network effect?',
    'If ISPs face genuine scarcity constraints, the neutrality reading''s constraint becomes more extractive and theater-laden (they are coercively prevented from efficient allocation). If scarcity claims are overstated, the constraint is less extractive (it prevents artificial scarcity creation). This modulates the effective-extraction computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_reality_gap, empirical, 'Whether the payers'' cost-recovery claims reflect structural constraints or preference-masking.').

omega_variable(
    regulatory_enforcement_trajectory,
    'Is regulatory enforcement of non-discrimination strengthening, stable, or degrading over time?',
    'Policy tracking: enforcement actions, regulatory budgets, appeals outcomes, jurisdictional variation. The measurement series shows suppression_requirement rising to 0.71 and plateauing; does enforcement capacity match that curve, or is the theater_ratio rise a signal that enforcement is decoupling from capability?',
    'If enforcement is genuinely strengthening, the constraint will remain binding. If enforcement is becoming theatrical (audits and process without teeth), the constraint may drift toward Piton (performance masking atrophy). This affects whether the classification remains tangled_rope or transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_trajectory, empirical, 'Whether regulatory enforcement of neutrality is sustainable or staged for collapse.').

omega_variable(
    reading_foreclosure_conditional,
    'Do the three readings of the tcp_ip_interpretation kernel actually coexist, or does establishing the factual referent (what TCP/IP architecturally requires) foreclose some readings?',
    'Technical analysis: if the end-to-end principle is discovered to logically entail non-discrimination (rather than merely permitting it), then prioritization and zero_rating readings become untenable within the framework. Conversely, if the principle is silent on discrimination, all three readings remain live.',
    'This determines the reading_relations classification: if foreclosed, the sibling readings are untenable; if coexistent, they remain live options held by different parties. This is an Omega_C (conceptual/framing) uncertainty about what the kernel commits us to.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_conditional, conceptual, 'Whether the tcp_ip_interpretation kernel''s factual referent determines the reading uniquely or admits multiple interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__neutrality_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(tcp__tr_t5, observed).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__neutrality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(tcp__tr_t10, observed).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__neutrality_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(tcp__tr_t15, observed).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t20, observed).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__neutrality_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t25, observed).
narrative_ontology:measurement(tcp__tr_t30, tcp_ip_interpretation__neutrality_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t30, observed).
narrative_ontology:measurement(tcp__tr_t35, tcp_ip_interpretation__neutrality_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(tcp__be_t5, observed).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(tcp__be_t10, observed).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(tcp__be_t15, observed).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(tcp__be_t20, observed).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(tcp__be_t25, observed).
narrative_ontology:measurement(tcp__be_t30, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(tcp__be_t30, observed).
narrative_ontology:measurement(tcp__be_t35, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(tcp__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(tcp__su_t5, observed).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(tcp__su_t10, observed).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(tcp__su_t15, observed).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(tcp__su_t20, observed).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(tcp__su_t25, observed).
narrative_ontology:measurement(tcp__su_t30, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(tcp__su_t30, observed).
narrative_ontology:measurement(tcp__su_t35, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(tcp__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.22).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, isps_market_power_concentration).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, edge_innovation_ecosystem).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel admits three structurally distinct constraint readings: neutrality_reading (this file), prioritization_reading, and zero_rating_reading. Each reading has a different beneficiary/victim structure and classification. They share a kernel (TCP/IP end-to-end principle) but diverge in ε-referent scope: neutrality reads the principle as prohibiting ISP discrimination; prioritization reads it as permitting service-quality variation; zero_rating reads it as permitting selective exemptions. The readings are linked via network.affects_constraints rather than merged into one underspecified story. Each story carries its own ε, beneficiary/victim set, and structural data independently; the engine computes each seat's classification from its own constraint's metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
