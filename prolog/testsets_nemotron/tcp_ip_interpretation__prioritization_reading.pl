% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Prioritization Reading — Differentiated Service Quality as Network Management
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The prioritization reading of TCP/IP interprets the protocol suite's
 *   differentiated services architecture (DiffServ, ECN, QoS bits) as
 *   authorizing commercial paid prioritization — 'fast lanes' for
 *   latency-sensitive traffic — under the banner of 'reasonable network
 *   management.' This reading gained regulatory traction after the 2017 FCC
 *   Restoring Internet Freedom Order vacated the 2015 bright-line rules. The
 *   constraint is the standing arrangement where ISPs may legally offer, and
 *   edge services may be compelled to purchase, prioritized delivery. The
 *   reading's claimed type is tangled_rope: it solves a real coordination
 *   problem (heterogeneous QoS requirements on shared infrastructure) while
 *   simultaneously extracting asymmetric rents from those who cannot pay. The
 *   engine computes per-seat types from the structural data; this story
 *   authors the structural data independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.55).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Prioritization Reading — Differentiated Service Quality as Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '8129d926-c7a5-41d7-948d-2f7045b48e18').
narrative_ontology:cs_kernel_codification('8129d926-c7a5-41d7-948d-2f7045b48e18', fixed_text).
narrative_ontology:cs_authority_grounding('8129d926-c7a5-41d7-948d-2f7045b48e18', lineage).
narrative_ontology:cs_interpretation_layer_present('8129d926-c7a5-41d7-948d-2f7045b48e18').
narrative_ontology:cs_reading_relation('8129d926-c7a5-41d7-948d-2f7045b48e18', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('8129d926-c7a5-41d7-948d-2f7045b48e18', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('8129d926-c7a5-41d7-948d-2f7045b48e18', foundational, qos_mechanisms_authorize_commercial_prioritization).
narrative_ontology:cs_axiom_status(qos_mechanisms_authorize_commercial_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('8129d926-c7a5-41d7-948d-2f7045b48e18', qos_mechanisms_authorize_commercial_prioritization, conventional).
narrative_ontology:cs_axiom('8129d926-c7a5-41d7-948d-2f7045b48e18', foundational, network_investment_requires_differentiated_revenue_streams).
narrative_ontology:cs_axiom_status(network_investment_requires_differentiated_revenue_streams, holdable).
narrative_ontology:cs_axiom_grounding('8129d926-c7a5-41d7-948d-2f7045b48e18', network_investment_requires_differentiated_revenue_streams, instrumental).
narrative_ontology:cs_reference_frame('8129d926-c7a5-41d7-948d-2f7045b48e18', original_tcp_ip_architecture_as_open_interconnection).
narrative_ontology:cs_drift_state('8129d926-c7a5-41d7-948d-2f7045b48e18', post_2017_restoring_internet_freedom_order, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8129d926-c7a5-41d7-948d-2f7045b48e18', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, tier1_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, cdn_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, latency_sensitive_application_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, edge_service_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, nonprofit_digital_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, rural_community_networks).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, end_users_without_premium_tiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, latency_sensitive_application_providers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_investment_incentive_through_qos_revenue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the backbone and last-mile infrastructure; interpret 'reasonable network management' to include paid prioritization tiers. Collect revenue from fast-lane contracts while controlling the technical standards bodies where QoS parameters are set. Can shift capital allocation across service tiers at will.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, tier1_isps, agenda_setter,
    institutional, generational, arbitrage, global).

% Deploy edge caches inside ISP networks under paid peering agreements that function as de facto prioritization. Benefit from the reading's legitimacy because their business model is structurally aligned with differentiated service — they pay for the fast lane and pass costs to their customers. Can relocate cache nodes if a single ISP's terms become unfavorable.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, cdn_operators, beneficiary,
    powerful, biographical, mobile, global).

% Real-time gaming, videoconferencing, telemedicine, and high-frequency trading platforms that purchase prioritized delivery. Gain deterministic latency at the cost of recurring payments to ISPs and CDNs. Cannot build their own last-mile infrastructure; locked into purchasing quality from the incumbents.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, latency_sensitive_application_providers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, latency_sensitive_application_providers, payer).

% Startups, independent developers, and small-to-mid SaaS operators who cannot afford paid prioritization. Their traffic is relegated to best-effort queues that degrade under congestion. Exit means accepting slower user experience or abandoning markets where competitors buy fast lanes.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, edge_service_providers, payer,
    moderate, biographical, constrained, global).

% Wikipedia, Internet Archive, public-interest journalism, civic tech platforms, and educational resources that operate on donated or grant funding. Have zero budget for paid prioritization; their traffic competes with commercial fast-lane traffic on congested best-effort paths. No exit — their mission requires universal accessibility, which the reading structurally undermines.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, nonprofit_digital_services, payer,
    powerless, biographical, trapped, global).

% Cooperatives, municipal broadband, and tribal networks building last-mile infrastructure in underserved areas. Depend on wholesale transit from Tier 1 ISPs; cannot negotiate prioritization terms. Their users experience compounded disadvantage: sparse infrastructure plus best-effort treatment on backbone links.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, rural_community_networks, payer,
    powerless, generational, trapped, regional).

% Residential and mobile subscribers on standard plans. Experience degraded performance for non-prioritized services (buffering video, laggy VoIP, slow software updates) while ISPs market 'premium' tiers that restore baseline quality. Exit is limited by local monopoly/duopoly conditions.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, end_users_without_premium_tiers, payer,
    powerless, biographical, constrained, global).

% Engineers and standards authors in the IETF who maintain DiffServ (RFC 2474/2475), IntServ (RFC 1633), and subsequent QoS architectures. Their work provides the technical vocabulary the reading invokes, but the standards themselves are neutral — they specify mechanisms, not policy. They observe the policy debate from the protocol layer.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, ietf_participants, observer,
    organized, generational, analytical, global).

% FTC, DOJ Antitrust Division, European Commission DG COMP, and national regulators investigating whether paid prioritization constitutes anti-competitive leveraging of bottleneck infrastructure. Can impose structural remedies but operate on multi-year enforcement timelines.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables network operators to manage finite capacity by allocating bandwidth and latency guarantees to applications with different quality requirements — solving the genuine coordination problem of heterogeneous traffic sharing a common medium.
% TRANSFER_FUNCTION: Moves revenue from edge service providers, nonprofit digital services, rural networks, and standard-tier end users to Tier 1 ISPs and CDN operators, in exchange for QoS guarantees that the best-effort baseline no longer reliably provides.
% ABSENT_VOICES: Future entrepreneurs whose services don't exist yet because the capital barrier to entry includes paid prioritization budgets; users in jurisdictions without net neutrality protections who have no regulatory recourse; the global south where wholesale transit costs amplify the fast-lane penalty.
% DISAPPEARANCE_RATIONALE: If the prioritization reading vanished overnight and non-discrimination became the enforced default, ISPs would lose a primary revenue lever for capacity expansion, CDN peering economics would shift toward settlement-free models, edge services would compete on product quality rather than delivery tier, and the investment-incentive argument would be tested against actual capital expenditure data.
% FOUNDING_PROBLEM: Early commercial Internet faced unpredictable congestion as traffic grew beyond academic volumes; operators needed a mechanism to guarantee service quality for emerging real-time applications (voice, video) without over-provisioning the entire network.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and equipment vendors attest the problem persists and worsens with traffic growth (Cisco VNI reports, ITU-T SG13 submissions). Public-interest researchers and former FCC technologists attest that over-provisioning and protocol improvements (QUIC, BBR, L4S) have substantially solved the original congestion problem, and that current prioritization primarily extracts rent from the new baseline scarcity the reading itself helps create (Free Press, Public Knowledge, and EFF technical filings; 2015 Open Internet Order record).
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects that the fast-lane premium is decoupled from marginal cost of priority queueing — it is priced at willingness-to-pay, not cost-plus. The rising trajectory (0.25→0.68 over 1998–2024) tracks the shift from DiffSpec as an engineering tool to paid prioritization as a revenue pillar. Suppression (0.55) is moderate: the constraint does not ban alternatives (anyone can build a CDN, use QUIC, or over-provision), but the structural disadvantage of best-effort traffic under congestion creates de facto coercion. Theater (0.42) captures the growing gap between 'network management' rhetoric and the commercial terms of paid peering / fast-lane contracts. Accessibility collapse (0.38) is partial: alternatives exist but are economically nonviable for many victims. Resistance (0.71) is high: sustained advocacy, litigation, state-level net neutrality laws, and protocol-level workarounds (encrypted traffic hiding, L4S) all contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP seat, the constraint is genuine coordination: they built the pipes, they manage congestion, and QoS markets fund capacity. From the edge-provider and nonprofit seats, the same structure is extraction: the congestion that makes prioritization valuable is partly endogenous to under-investment in best-effort capacity, and the fast-lane price is monopoly rent. The engine computes this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Tier 1 ISPs are structural beneficiaries (collect fast-lane revenue, set terms — d near 0.1). CDN operators and latency-sensitive app providers are secondary beneficiaries who also pay (d ~0.3–0.4). Edge providers, nonprofits, rural networks, and standard-tier users are targets (d 0.7–0.9) — they bear the cost of degraded best-effort service and the barrier to entry. IETF participants and competition authorities are analytical observers (d ~0.5, analytical exit). The derivation chain from beneficiary/victim declarations + power + exit produces these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (congestion management for early real-time apps) has been substantially addressed by protocol evolution and bandwidth growth, but the arrangement persists and has expanded into a general revenue model. The mandatrophy is contested: ISPs claim the problem is live and growing; critics claim the arrangement has metastasized beyond its founding justification. The engine will flag the founding_problem_status=contested + disappearance_verdict=world_rearranges mismatch as a zombie/capture signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prioritization_revenue_vs_capex,
    'What fraction of paid prioritization revenue is actually reinvested in capacity expansion versus extracted as shareholder return?',
    'Regulatory financial reporting requirements compelling ISP capex disaggregation by funding source; independent audit of capital allocation before/after prioritization revenue recognition.',
    'If reinvestment fraction is high, the coordination function is substantiated and extraction is lower; if low, the reading is predominantly rent extraction and ε should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prioritization_revenue_vs_capex, empirical, 'Whether fast-lane revenue funds the capacity it claims to manage.').

omega_variable(
    congestion_endogeneity,
    'To what extent is the congestion that makes prioritization valuable created or maintained by ISP under-investment in best-effort capacity?',
    'Longitudinal analysis of ISP capacity per subscriber, congestion metrics, and capex trends before and after paid prioritization authorization; natural experiments from jurisdictions with/without net neutrality rules.',
    'If congestion is endogenous, the reading creates the scarcity it sells relief from — a self-reinforcing extraction loop. ε and suppression would both be understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congestion_endogeneity, conceptual, 'Whether the constraint manufactures the problem it coordinates.').

omega_variable(
    protocol_neutrality_of_qos_mechanisms,
    'Do the IETF QoS mechanisms (DiffServ, IntServ, L4S) structurally require commercial prioritization, or are they neutral tools that can implement non-discriminatory management?',
    'Technical analysis of whether the protocol mechanisms can enforce per-flow fairness without paid tiers; deployment evidence from networks that use QoS for internal traffic engineering only.',
    'If mechanisms are neutral, the reading''s appeal to ''TCP/IP permits'' is a policy choice masquerading as protocol necessity. If mechanisms require commercialization, the protocol itself embeds the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_neutrality_of_qos_mechanisms, conceptual, 'Whether the technical standards structurally entail the commercial reading.').

omega_variable(
    kernel_reading_relationship_prioritization_vs_neutrality,
    'Does the prioritization reading logically foreclose the neutrality reading within a single regulatory framework, or do they coexist as competing policy positions?',
    'Legal analysis of whether a jurisdiction can simultaneously enforce ''no paid prioritization'' (neutrality) while permitting ''reasonable network management'' that includes QoS — the 2015 Open Internet Order attempted this synthesis; the 2017 reversal rejected it.',
    'If they foreclose, the kernel admits only one stable reading per framework. If they coexist, the kernel is a permanent contested zone. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_prioritization_vs_neutrality, conceptual, 'Structural relationship between this reading and the neutrality_reading sibling.').

omega_variable(
    kernel_reading_relationship_prioritization_vs_zero_rating,
    'Does the prioritization reading structurally influence the zero_rating_reading (creating downstream pressure on sponsored-content exemptions) without foreclosing it?',
    'Policy genealogy tracing whether paid prioritization frameworks normalize the principle that ''some traffic deserves better treatment,'' which then legitimates zero-rating as a downstream variant.',
    'If influences, the readings form a cascade: prioritization reading → zero_rating_reading. If they coexist independently, each stands on its own policy logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_prioritization_vs_zero_rating, conceptual, 'Structural relationship between this reading and the zero_rating_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t1998, tcp_ip_interpretation__prioritization_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement_basis(tcp__tr_t1998, observed).
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(tcp__tr_t2005, observed).
narrative_ontology:measurement(tcp__tr_t2010, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement_basis(tcp__tr_t2010, observed).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(tcp__tr_t2015, observed).
narrative_ontology:measurement(tcp__tr_t2020, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement_basis(tcp__tr_t2020, observed).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t1998, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement_basis(tcp__be_t1998, observed).
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(tcp__be_t2005, observed).
narrative_ontology:measurement(tcp__be_t2010, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(tcp__be_t2010, observed).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(tcp__be_t2015, observed).
narrative_ontology:measurement(tcp__be_t2020, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(tcp__be_t2020, observed).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(tcp__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t1998, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement_basis(tcp__su_t1998, observed).
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement_basis(tcp__su_t2005, observed).
narrative_ontology:measurement(tcp__su_t2010, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(tcp__su_t2010, observed).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(tcp__su_t2015, observed).
narrative_ontology:measurement(tcp__su_t2020, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(tcp__su_t2020, observed).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(tcp__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, interconnection_disputes).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, cdn_peering_economics).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, rural_broadband_deployment).

% DUAL FORMULATION NOTE:
% This story is the prioritization_reading of the tcp_ip_interpretation kernel family. The neutrality_reading (ε ≈ 0.15, claimed mountain) and zero_rating_reading (ε ≈ 0.45, claimed tangled_rope) are separate constraint stories with distinct ε values, stakeholder structures, and classifications. All three share the kernel_id 'tcp_ip_interpretation' and are linked via network.affects_constraints. The prioritization reading's ε (0.68) is substantially higher because it authorizes commercial extraction; the neutrality reading's ε is near-zero because it prohibits discrimination; the zero_rating reading sits between — it permits discrimination but only for sponsored content, not general paid prioritization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
