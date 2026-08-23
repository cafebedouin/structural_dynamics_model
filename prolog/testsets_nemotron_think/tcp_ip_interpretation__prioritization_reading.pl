% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Permits Differentiated Service Quality as Network Management
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The prioritization reading of TCP/IP interprets the protocol suite's
 *   Differentiated Services (DiffServ) architecture, Explicit Congestion
 *   Notification (ECN), and the absence of a mandatory non-discrimination
 *   clause in the core RFCs as affirmative permission for ISPs to offer
 *   commercial quality-of-service tiers. This reading gained regulatory
 *   traction during the 2017 FCC Restoring Internet Freedom Order and
 *   persists in ITU-T recommendations. The constraint is the
 *   regulatory-technical framework that authorizes paid fast lanes. Its
 *   proponents claim it is a coordination mechanism (rope/tangled_rope)
 *   solving real network management problems; critics argue the coordination
 *   story is cover for extraction (snare). The authored metrics reflect the
 *   structural observation that extraction has risen steadily as ISPs
 *   monetize prioritization, while the genuine coordination fraction
 *   (congestion management) has become a smaller share of the activity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.72).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.65).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Permits Differentiated Service Quality as Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'eeeab6a1-d591-47f9-bf5f-039000713855').
narrative_ontology:cs_kernel_codification('eeeab6a1-d591-47f9-bf5f-039000713855', formalized).
narrative_ontology:cs_authority_grounding('eeeab6a1-d591-47f9-bf5f-039000713855', expertise).
narrative_ontology:cs_interpretation_layer_present('eeeab6a1-d591-47f9-bf5f-039000713855').
narrative_ontology:cs_reading_relation('eeeab6a1-d591-47f9-bf5f-039000713855', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeeab6a1-d591-47f9-bf5f-039000713855', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('eeeab6a1-d591-47f9-bf5f-039000713855', foundational, network_management_discretion).
narrative_ontology:cs_axiom_status(network_management_discretion, holdable).
narrative_ontology:cs_axiom_grounding('eeeab6a1-d591-47f9-bf5f-039000713855', network_management_discretion, conventional).
narrative_ontology:cs_axiom('eeeab6a1-d591-47f9-bf5f-039000713855', foundational, paid_prioritization_permissible).
narrative_ontology:cs_axiom_status(paid_prioritization_permissible, holdable).
narrative_ontology:cs_axiom_grounding('eeeab6a1-d591-47f9-bf5f-039000713855', paid_prioritization_permissible, instrumental).
narrative_ontology:cs_axiom('eeeab6a1-d591-47f9-bf5f-039000713855', secondary, diffserv_enables_commercial_qos).
narrative_ontology:cs_axiom_status(diffserv_enables_commercial_qos, holdable).
narrative_ontology:cs_axiom_grounding('eeeab6a1-d591-47f9-bf5f-039000713855', diffserv_enables_commercial_qos, conventional).
narrative_ontology:cs_reference_frame('eeeab6a1-d591-47f9-bf5f-039000713855', rfc2475_diffserv_framework).
narrative_ontology:cs_drift_state('eeeab6a1-d591-47f9-bf5f-039000713855', post_2017_restoring_internet_freedom_order, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eeeab6a1-d591-47f9-bf5f-039000713855', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_content_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, network_equipment_vendors).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_content_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, end_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_management_discretion_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, paid_prioritization_incentivizes_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the network infrastructure and seek regulatory permission to offer paid prioritization tiers. They argue differentiated services are necessary for network management (congestion control, latency-sensitive applications) and that the revenue funds infrastructure investment. They control the technical implementation and lobby for permissive regulatory frameworks.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, isps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, isps, beneficiary).

% Can afford to pay for fast-lane access, ensuring their services (video streaming, cloud gaming, real-time communication) perform reliably. They gain competitive advantage over smaller rivals who cannot pay. Some vertically integrate with ISPs (e.g., Comcast-NBCU, AT&T-Time Warner) creating aligned incentives for prioritization.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Sell QoS-enabled routers, deep packet inspection gear, and traffic shaping appliances. Their revenue grows when networks implement complex differentiated services. They participate in standards bodies (IETF, ITU-T) shaping the technical specifications that make prioritization feasible.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, network_equipment_vendors, beneficiary,
    organized, biographical, mobile, global).

% Startups, non-profits, independent creators, and small businesses that cannot afford paid prioritization. Their traffic is relegated to best-effort queues, suffering higher latency, jitter, and packet loss during congestion. They have no practical exit — the internet is the only distribution channel — but can sometimes use CDNs or alternative protocols as partial mitigation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    moderate, biographical, constrained, global).

% Experience better performance for prioritized services (smooth 4K video, low-lag gaming) but degraded access to non-prioritized content (independent news, niche communities, experimental applications). They pay indirectly through higher subscription costs passed through by large content providers and have limited ISP choice in most markets.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, end_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, end_users, beneficiary).

% Local news outlets, community forums, educational resources, and niche creators. They lack both the capital for paid prioritization and the technical sophistication for workarounds. Their audiences experience noticeably worse performance, creating a structural disadvantage that compounds over time.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_content_providers, payer,
    powerless, immediate, trapped, global).

% Monitor for anti-competitive effects of paid prioritization (vertical foreclosure, barriers to entry). They can impose conditions, block mergers, or enforce transparency rules. Their enforcement capacity varies by jurisdiction and political administration.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, regulators_competition_authorities, observer,
    institutional, generational, analytical, national).

% Maintain the TCP/IP protocol suite (DiffServ, IntServ, ECN, AQM specifications). Their working groups debate whether the architecture permits or forbids discrimination. They provide the technical vocabulary that both readings invoke, but their consensus process is slow and often produces ambiguous guidance.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, ietf_standards_bodies, agenda_setter,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of managing finite network capacity among competing traffic classes with different latency, jitter, and loss requirements (real-time vs. bulk transfer).
% TRANSFER_FUNCTION: Moves revenue from edge service providers (who pay for prioritization) and end users (who pay higher subscription costs) to ISPs and network equipment vendors, in exchange for guaranteed quality of service tiers.
% ABSENT_VOICES: Future innovators whose applications don't exist yet but would be disadvantaged by a prioritized architecture; non-commercial speech communities (activists, artists, researchers) who lack organizational representation; users in monopoly ISP markets who cannot switch providers.
% DISAPPEARANCE_RATIONALE: If the prioritization reading were rejected and neutrality enforced, ISPs would lose a major revenue stream and business model; edge services would compete on merit rather than payment capacity; network equipment markets would shift from QoS gear to capacity expansion; the entire economics of internet interconnection would reorganize around settlement-free peering and capacity-based pricing.
% FOUNDING_PROBLEM: Early internet congestion collapse (1986) showed that pure best-effort FIFO queuing fails under load. The founding problem was how to allocate scarce bandwidth among heterogeneous applications without centralized control.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's own RFC history (RFC 2475 DiffServ, RFC 3168 ECN) attests the technical problem is real. However, public interest groups (EFF, Public Knowledge), academic researchers (van Schewick, Wu), and former FCC commissioners attest that the founding congestion problem has been substantially solved by capacity expansion and active queue management, and that current prioritization proposals serve commercial extraction, not technical necessity.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the paid prioritization revenue stream is decoupled from marginal network management cost — it is monopoly rent from control of the last-mile bottleneck. Suppression (0.65) reflects the regulatory and technical barriers to neutral alternatives: the 2017 Order preempted state neutrality laws, and the technical architecture makes it difficult for edge services to signal preferences without ISP cooperation. Theater ratio (0.42) captures that DiffServ/ECN have legitimate uses (VoIP, gaming) but these are a minority of the commercial prioritization volume. Accessibility collapse (0.55) is moderate: CDNs, QUIC, and application-layer adaptation provide partial workarounds but cannot overcome last-mile queueing discipline. Resistance (0.68) is high: sustained advocacy, state-level legislation, congressional proposals, and litigation continue to challenge the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP/IETF seat, the constraint appears as legitimate network management solving a real coordination problem (DiffServ was designed for this). From the unfunded edge service seat, the same technical machinery operates as a pay-to-play gatekeeper. The engine computes this divergence from the declared power/exit asymmetries — the claimed_type (tangled_rope) acknowledges both faces without adjudicating which is 'true'.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs are structural beneficiaries (collect prioritization revenue, write the queueing rules — d near 0.1). Large content providers are secondary beneficiaries (gain competitive moat — d near 0.25). Network equipment vendors benefit incidentally (d near 0.3). Unfunded edge services and small content providers are full targets (pay with degraded performance, no exit — d near 0.9). End users are dual: beneficiaries for prioritized services, payers for non-prioritized (net d near 0.55). Regulators and IETF are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (congestion collapse) was real but has been substantially solved by overprovisioning, AQM (CoDel, FQ-CoDel), and application-layer adaptation (ABR video). The prioritization reading persists not because the founding problem remains acute, but because the regulatory permission it enables creates a valuable asset for ISPs. This is a classic mandatrophy pattern: the mandate (network management) has atrophied relative to the extraction (paid fast lanes), but the constraint is maintained because the beneficiary (ISPs) captures enough value to defend it, while the victims (edge services) are too dispersed to overturn it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tcpip_architecture_intent_ambiguity,
    'Does the TCP/IP protocol suite''s architecture (as expressed in the core RFCs and the end-to-end principle) inherently permit or forbid commercial traffic discrimination?',
    'Historical analysis of the original design intent (Cerf, Kahn, Clark, Saltzer) vs. later DiffServ/IntServ standardization; examination of whether RFC 2475 (DiffServ) was intended to enable commercial QoS tiers or only technical traffic classes.',
    'If the architecture forbids discrimination, the prioritization reading is a constructed interpretation (snare/tangled_rope); if it permits, the reading has stronger natural-law footing (rope/tangled_rope). This is the core factual dispute underlying the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tcpip_architecture_intent_ambiguity, conceptual, 'Whether TCP/IP''s original design commits to neutrality or permits prioritization.').

omega_variable(
    investment_incentive_empirical_claim,
    'Does paid prioritization revenue actually incentivize additional network capacity investment, or is it extracted from existing capacity?',
    'Longitudinal econometric study of CAPEX trends in jurisdictions with vs. without paid prioritization authorization, controlling for traffic growth, technology cycles, and competitive intensity.',
    'If investment increases, the coordination function is substantiated (tangled_rope); if not, the coordination story is cover for pure extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_empirical_claim, empirical, 'Whether the claimed investment incentive from paid prioritization is real.').

omega_variable(
    suppression_mechanism_regulatory_vs_technical,
    'Is the suppression of neutral alternatives primarily regulatory (preemption of state laws, forbearance from Title II) or technical (architecture makes neutrality costly to implement)?',
    'Counterfactual analysis: if the 2015 Open Internet Order (Title II classification) had remained in force, would ISPs have deployed prioritization anyway using technical means? Comparison with EU''s BEREC guidelines where neutrality coexists with managed services.',
    'If primarily regulatory, suppression is reversible by policy change (lower structural entrenchment); if primarily technical, neutrality requires architectural redesign (higher entrenchment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_regulatory_vs_technical, empirical, 'Whether suppression of neutrality stems from law or architecture.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the TCP/IP kernel support a single coherent commitment system, or do the three readings (neutrality, prioritization, zero-rating) reflect fundamentally different framings of what the kernel *is*?',
    'CS-structure analysis: if each reading has distinct axioms, reference frames, and drift profiles that cannot be mapped onto a single authority structure, the kernel is underdetermined — the ''contest'' is actually multiple kernels sharing a label.',
    'If underdetermined, the prioritization_reading''s classification should be evaluated independently without assuming shared structure with siblings; if a single kernel, cross-reading contamination effects apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three readings share one kernel or constitute three distinct kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_prioritization_tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp_ip_prioritization_tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(tcp_ip_prioritization_tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(tcp_ip_prioritization_tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(tcp_ip_prioritization_tr_t16, tcp_ip_interpretation__prioritization_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(tcp_ip_prioritization_tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp_ip_prioritization_be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcp_ip_prioritization_be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(tcp_ip_prioritization_be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(tcp_ip_prioritization_be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(tcp_ip_prioritization_be_t16, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(tcp_ip_prioritization_be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_prioritization_su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tcp_ip_prioritization_su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(tcp_ip_prioritization_su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(tcp_ip_prioritization_su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(tcp_ip_prioritization_su_t16, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(tcp_ip_prioritization_su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.15).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, interconnection_agreements).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, cdn_market_structure).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, edge_computing_platform_governance).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel decomposes into three constraint stories differing in ε: neutrality_reading (ε ≈ 0.15, claimed mountain/rope), prioritization_reading (ε ≈ 0.72, claimed tangled_rope), zero_rating_reading (ε ≈ 0.55, claimed tangled_rope). The prioritization_reading is downstream of neutrality_reading in regulatory history (the 2017 Order explicitly reversed the 2015 Order) and upstream of zero_rating_reading (paid prioritization generalizes the zero-rating logic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, institutional, 0.15).
constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerful, 0.25).
constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, organized, 0.3).
constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, moderate, 0.85).
constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
