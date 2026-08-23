% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: TCP/IP End-to-End Principle as Non-Discrimination Mandate
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The TCP/IP protocol suite was designed with an end-to-end principle:
 *   intelligence resides at the edges, the network merely transports packets
 *   without inspecting or discriminating among them. The neutrality_reading
 *   instantiates this principle as a binding non-discrimination constraint on
 *   ISPs — they may not block, throttle, or prioritize traffic based on
 *   content, application, or service. This reading powered the 2015 FCC Open
 *   Internet Order, the EU Telecoms Single Market Regulation, India's TRAI
 *   recommendations, and California's SB-822. The constraint coordinates edge
 *   innovation by guaranteeing open transport; it extracts from ISPs the
 *   ability to monetize prioritization or exclude rivals. The claimed_type is
 *   tangled_rope because genuine coordination (permissionless edge
 *   innovation) coexists with asymmetric extraction (ISPs forego revenue
 *   optimization). Active enforcement is required: without rules and
 *   monitoring, ISPs have both incentive and technical capability to
 *   discriminate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.38).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.22).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle as Non-Discrimination Mandate").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '2c95c212-4f23-4057-be5c-8280f7e3e42a').
narrative_ontology:cs_kernel_codification('2c95c212-4f23-4057-be5c-8280f7e3e42a', distributed).
narrative_ontology:cs_authority_grounding('2c95c212-4f23-4057-be5c-8280f7e3e42a', practice).
narrative_ontology:cs_interpretation_layer_present('2c95c212-4f23-4057-be5c-8280f7e3e42a').
narrative_ontology:cs_reading_relation('2c95c212-4f23-4057-be5c-8280f7e3e42a', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c95c212-4f23-4057-be5c-8280f7e3e42a', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('2c95c212-4f23-4057-be5c-8280f7e3e42a', foundational, end_to_end_principle_requires_non_discrimination).
narrative_ontology:cs_axiom_status(end_to_end_principle_requires_non_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('2c95c212-4f23-4057-be5c-8280f7e3e42a', end_to_end_principle_requires_non_discrimination, conventional).
narrative_ontology:cs_axiom('2c95c212-4f23-4057-be5c-8280f7e3e42a', secondary, edge_innovation_depends_on_open_transport).
narrative_ontology:cs_axiom_status(edge_innovation_depends_on_open_transport, holdable).
narrative_ontology:cs_axiom_grounding('2c95c212-4f23-4057-be5c-8280f7e3e42a', edge_innovation_depends_on_open_transport, empirically_contingent).
narrative_ontology:cs_reference_frame('2c95c212-4f23-4057-be5c-8280f7e3e42a', non_discriminatory_transport).
narrative_ontology:cs_drift_state('2c95c212-4f23-4057-be5c-8280f7e3e42a', contemporary_fragmented_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2c95c212-4f23-4057-be5c-8280f7e3e42a', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_application_developers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, civil_society_organizations).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, access_tier_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, mobile_network_operators).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, permissionless_innovation).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, network_neutrality_as_design_invariant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide last-mile connectivity to end users. Forbidden from blocking, throttling, or paid prioritization of specific applications or content. Bear compliance costs (transparency reporting, network management disclosure) and forego revenue from prioritization deals and zero-rating partnerships. Can arbitrage across jurisdictions with lighter rules but cannot exit the regulated market without losing the customer base.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, access_tier_isps, payer,
    institutional, generational, constrained, national).

% Provide wireless connectivity with tighter capacity constraints than fixed networks. Argue that 'reasonable network management' requires more latitude for traffic shaping. Bear higher compliance costs per subscriber due to spectrum scarcity arguments. Also act as agenda_setters in standardization bodies (3GPP, GSMA) shaping what 'management' means.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, mobile_network_operators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, mobile_network_operators, agenda_setter).

% Build applications (video, voice, gaming, IoT) that reach users without negotiating with ISPs. The non-discrimination rule guarantees their packets are treated equally, enabling permissionless innovation and global scale from day one. Exit is mobile: they benefit wherever the rule applies, and can route around discrimination via CDNs or encryption, but the rule's value is universal assurance.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_developers, beneficiary,
    organized, biographical, mobile, global).

% Access any lawful content, application, or service without ISP interference. Benefit from competitive application markets and lower switching costs. Exit is mobile: they can use VPNs, switch ISPs (where competition exists), or migrate to jurisdictions with stronger rules, but the rule's value is the default open experience.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    organized, biographical, mobile, global).

% Large video, cloud, and social platforms (Netflix, Google, Meta, Amazon, Microsoft) that generate majority of traffic. Benefit from non-discrimination (no toll for reaching users) but have 'arbitrage' exit: they can deploy private interconnect, CDNs, and edge caches to bypass congestion points, and lobby for regulatory carve-outs. Their benefit is real but they have more exit than small developers.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_providers, beneficiary,
    powerful, biographical, arbitrage, global).

% Advocacy groups (EFF, Access Now, Article 19, Public Knowledge) that treat non-discrimination as a digital rights issue. They benefit from the rule's protection of free expression and innovation but do not directly extract rents. Exit is mobile: they operate transnationally and shift advocacy focus across jurisdictions.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, civil_society_organizations, beneficiary,
    moderate, generational, mobile, global).

% FCC (US), BEREC/NRAs (EU), TRAI (India), Ofcom (UK), CRTC (Canada), Anatel (Brazil) — set rules, enforce transparency, adjudicate 'reasonable network management' exceptions. They administer the constraint but do not directly collect its extraction. Their exit is analytical: they observe the constraint's operation from a governance seat.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecommunications_regulators, agenda_setter,
    institutional, generational, analytical, national).

% IETF (protocol standards), ITU-T (telecom standards), 3GPP (mobile standards), W3C (web standards) — define technical meanings of 'network management', 'congestion control', 'QoS'. They do not enforce policy but their standards shape what the constraint can technically prohibit or permit. Analytical seat with universal scope.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, standardization_bodies, observer,
    institutional, civilizational, analytical, universal).

% DOJ/FTC (US), DG COMP (EU), CMA (UK), CCi (India) — assess whether ISP discrimination constitutes abuse of dominance or anti-competitive tying. Overlap with telecom regulators but distinct mandate. Analytical seat.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of open interconnection: without non-discrimination, each ISP could extract tolls from edge providers, creating a fragmented 'walled garden' internet where innovation requires permission from every network operator. The end-to-end principle coordinates a single global permissionless innovation commons.
% TRANSFER_FUNCTION: Moves the option value of paid prioritization, zero-rating revenue, and vertical integration leverage from ISPs to the edge ecosystem (developers, users, content providers). ISPs forego discrimination revenue; edge actors gain assured market access without gatekeeper negotiation.
% ABSENT_VOICES: Small and rural ISPs (WISPs, municipal networks, cooperatives) who argue the compliance burden is disproportionate to their market power and that 'reasonable network management' flexibility is essential for their viability. They are often excluded from rulemaking dominated by large incumbents and large edge platforms. Also absent: users in unserved/underserved areas for whom the theoretical open internet is irrelevant without physical access.
% DISAPPEARANCE_RATIONALE: If non-discrimination rules vanished overnight, major ISPs would rapidly deploy paid prioritization tiers, zero-rating exclusive deals, and vertical self-preferencing. Edge innovation would shift to a permissioned model (negotiate with each ISP), application markets would consolidate around deep-pocketed incumbents who can pay for fast lanes, and the global interoperability of the internet would fracture into ISP-specific service bundles.
% FOUNDING_PROBLEM: Early commercial internet (1990s) saw network owners (telcos, cablecos) control both the pipe and the services running over it (AOL, CompuServe, proprietary walled gardens). The end-to-end principle was articulated to prevent the transport layer from dictating what applications could exist, enabling the web, email, P2P, VoIP, and video streaming to emerge without carrier permission.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the original protocol designers (Cerf, Kahn, Clark, Saltzer) in retrospective writings (e.g., 'The Design Philosophy of the DARPA Internet Protocols' 1988, 'End-to-End Arguments in System Design' 1984). It is corroborated by economic historians of the internet (Greenstein, 'How the Internet Became Commercial') and competition authorities (EU DG COMP market investigations, FCC 2015 record). ISPs dispute 'live' status, arguing the problem was solved by market competition; that dispute is the 'contested' signal, but independent technical and economic sources confirm the discrimination incentive persists with vertical integration.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).
:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects the revenue ISPs forego from paid prioritization, zero-rating deals, and vertical integration leverage — substantial but not total. Suppression (0.22) is moderate: the constraint forbids specific commercial practices but leaves most network management intact. Theater_ratio (0.18) captures 'reasonable network management' carve-outs that can be gamed, and the procedural overhead of transparency reporting. Accessibility_collapse (0.35) is low because alternatives exist (VPNs, overlay networks, regulatory arbitrage across jurisdictions). Resistance (0.55) is high: ISPs litigate, lobby, and implement discriminatory practices at the boundary of enforcement. The interval 0-28 maps roughly 1998-2026: from early 'best effort' norms through the 2015 peak of rule codification to the current fragmented enforcement landscape.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP seat, the constraint appears as extraction (foregone revenue, compliance burden) with weak coordination upside (they already manage networks). From the edge developer seat, it appears as essential coordination (predictable transport enables investment) with near-zero extraction. From the user seat, it is mostly coordination benefit with diffuse cost (potentially higher flat-rate pricing). The engine computes per-seat effective extraction from these structural asymmetries; the claimed_type does not adjudicate the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs (access_tier_isps, mobile_network_operators) are structural payers: they bear compliance costs and forego discrimination revenue. Their exit_options are 'constrained' — they cannot exit the regulatory regime without exiting the market, but they have 'arbitrage'-grade exit across jurisdictions (lighter regimes). Edge developers and users are beneficiaries with 'mobile' exit (they benefit wherever the rule applies, but the benefit is non-rival). Regulators are agenda_setters with 'analytical' exit. Content providers sit at 'constrained' exit: they benefit from non-discrimination but depend on ISP reach. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing network owners from controlling what applications users can run) remains live — vertical integration of ISPs into content (Comcast/NBCU, AT&T/TimeWarner) renews the discrimination incentive. However, the mandate has partially atrophied: encryption (DoH, QUIC, TLS 1.3) makes discrimination technically harder, reducing the coordination necessity of the rule even as the commercial incentive persists. The constraint is not a piton because active enforcement continues and the coordination function is still invoked in policy debates; it is not a scaffold because no sunset was declared. The tangled_rope classification captures the live tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the neutrality_reading a distinct constraint from prioritization_reading and zero_rating_reading, or are they measurement-variants of the same constraint?',
    'Apply the ε-invariance test: if measuring the constraint via ''discrimination prohibition'' yields ε≈0.38 but measuring via ''network management flexibility'' yields ε≈0.15, they are different constraints. The engine will compile three separate constraint stories with distinct ε and stakeholder structures.',
    'If they are one constraint, the corpus double-counts; if three, each gets its own classification and the network edges between them map the kernel''s internal tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate separate constraints per ε-invariance.').

omega_variable(
    iscp_management_vs_discrimination_boundary,
    'Where does legitimate network management (ISCPs, congestion control) end and content-based discrimination begin?',
    'Technical standard bodies (IETF) and regulatory adjudication (FCC, BEREC, TRAI) produce operational boundaries; track how often ''management'' justifications map to commercial discrimination.',
    'If the boundary is porous, the constraint''s suppression is lower (ISPs have wide discretion) and extraction shifts toward snare; if sharp, the constraint is a cleaner tangled_rope with defined coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iscp_management_vs_discrimination_boundary, empirical, 'Boundary between permissible network management and prohibited discrimination.').

omega_variable(
    zero_rating_as_discrimination,
    'Does zero-rating (sponsored data exemptions) constitute the discrimination this reading prohibits, or a separable pricing practice?',
    'Jurisdictional rulings (India TRAI 2016, California SB-822, EU BEREC guidelines) and economic analysis of whether zero-rating distorts edge competition equivalently to throttling.',
    'If zero-rating is prohibited discrimination, zero_rating_reading is foreclosed by this reading; if separable, they coexist_with as distinct regulatory questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_as_discrimination, conceptual, 'Whether zero-rating falls inside the neutrality reading''s discrimination prohibition.').

omega_variable(
    enforcement_feasibility_at_scale,
    'Can non-discrimination be enforced at internet scale without deep packet inspection that itself creates surveillance risks?',
    'Technical analysis of enforcement mechanisms (flow-level vs packet-level, encrypted traffic visibility) and their collateral surveillance externalities.',
    'If enforcement requires surveillance infrastructure, the constraint''s theater_ratio rises (coordination cover for monitoring) and its classification may shift toward snare from some seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_feasibility_at_scale, empirical, 'Whether enforcing non-discrimination creates a surveillance extraction layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_neutrality_tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t7, tcp_ip_interpretation__neutrality_reading, theater_ratio, 7, 0.1).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t7, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t14, tcp_ip_interpretation__neutrality_reading, theater_ratio, 14, 0.13).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t14, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t21, tcp_ip_interpretation__neutrality_reading, theater_ratio, 21, 0.16).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t21, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t28, tcp_ip_interpretation__neutrality_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(tcp_ip_neutrality_be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t7, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 7, 0.28).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t7, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t14, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t14, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t21, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 21, 0.35).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t21, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t28, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 28, 0.38).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_neutrality_su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t7, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 7, 0.17).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t7, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t14, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 14, 0.19).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t14, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t21, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 21, 0.21).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t21, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t28, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 28, 0.22).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, information_standard).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.03).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, interconnection_agreements).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, content_delivery_network_economics).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, edge_cloud_regulation).

% DUAL FORMULATION NOTE:
% Kernel tcp_ip_interpretation decomposes into three readings with distinct ε: neutrality_reading (ε≈0.38, tangled_rope), prioritization_reading (ε≈0.15, rope), zero_rating_reading (ε≈0.45, snare). The neutrality_reading forecloses neither sibling in public discourse (coexists_with) but structurally pressures both by establishing non-discrimination as the baseline from which deviations must be justified.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, institutional, 0.35).
constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
