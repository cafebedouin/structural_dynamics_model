% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: TCP/IP End-to-End Neutrality Reading
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint story captures the neutrality_reading of the contested
 *   kernel 'tcp_ip_interpretation'. The reading asserts that TCP/IP's
 *   end-to-end principle structurally requires non-discrimination by network
 *   intermediaries — ISPs must not block, throttle, or prioritize traffic
 *   based on content, application, or service. The sibling readings
 *   (prioritization_reading, zero_rating_reading) claim the protocol permits
 *   or is agnostic about differentiated service quality and sponsored
 *   exemptions. This reading instantiates a rope: genuine coordination with
 *   minimal extraction, active enforcement via regulation, and identifiable
 *   beneficiaries (edge innovators, users) who are net beneficiaries of the
 *   non-discrimination rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.15).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.2).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Neutrality Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'f38c64f8-898c-4b7a-bb04-3f850a9ad7b8').
narrative_ontology:cs_kernel_codification('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', distributed).
narrative_ontology:cs_authority_grounding('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', distributed).
narrative_ontology:cs_reading_relation('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', tcp_ip_interpretation__prioritization_reading, forecloses).
narrative_ontology:cs_reading_relation('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', foundational, non_discrimination_as_protocol_invariant).
narrative_ontology:cs_axiom_status(non_discrimination_as_protocol_invariant, holdable).
narrative_ontology:cs_axiom_grounding('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', non_discrimination_as_protocol_invariant, conventional).
narrative_ontology:cs_axiom('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', foundational, permissionless_innovation_requires_neutral_transport).
narrative_ontology:cs_axiom_status(permissionless_innovation_requires_neutral_transport, holdable).
narrative_ontology:cs_axiom_grounding('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', permissionless_innovation_requires_neutral_transport, instrumental).
narrative_ontology:cs_reference_frame('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', original_end_to_end_architecture).
narrative_ontology:cs_drift_state('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', post_net_neutrality_repeal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f38c64f8-898c-4b7a-bb04-3f850a9ad7b8', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, telecom_carriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, content_providers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, permissionless_innovation).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, network_non_discrimination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build applications and services assuming a neutral transport layer. The neutrality constraint guarantees they can reach users without negotiating with ISPs or paying for prioritization. Their exit is constrained because alternative networks don't exist at scale, but they benefit from the coordination function of a common carriage principle.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    organized, biographical, constrained, global).

% Access content and applications of their choice without ISP gatekeeping. The neutrality constraint protects their ability to use any lawful service. Exit is constrained — users can't practically switch to a 'neutral ISP' if their only provider discriminates — but they are net beneficiaries of the non-discrimination rule.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    moderate, biographical, constrained, global).

% Large content providers (video, cloud, social) benefit from neutrality because they avoid paying ISPs for delivery prioritization. They also pay for their own transit and CDN infrastructure. Their exit is mobile — they can multi-home, build CDNs, and route around congestion — but they still benefit from the rule preventing ISP extraction.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_providers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, content_providers, payer).

% Bear the cost of building and operating last-mile infrastructure while being prohibited from monetizing traffic differentiation. They argue the constraint prevents them from recovering costs through quality-based pricing and from managing congestion via paid prioritization. Their exit is arbitrage-grade: they can lobby for regulatory change, deploy zero-rating where permitted, and shape network management practices within the rules.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, isps, payer,
    institutional, generational, arbitrage, national).

% Long-distance and backbone carriers who would prefer to sell differentiated service tiers. The neutrality constraint limits their product differentiation. Like ISPs, they have arbitrage-grade exit through regulatory capture, litigation, and technical workarounds (zero-rating, sponsored data).
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_carriers, payer,
    institutional, generational, arbitrage, national).

% FCC, Ofcom, BEREC, and national telecom authorities that enact and enforce net neutrality rules. They set the agenda through rulemaking, adjudicate complaints, and can reclassify broadband (Title II vs. Title I in US). They are the enforcement machinery for the coordination function.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, regulators, agenda_setter,
    institutional, generational, analytical, national).

% IETF, W3C, IEEE — bodies that develop the protocols embodying the end-to-end principle. They don't enforce neutrality but their technical choices (e.g., DiffServ, ECN, QUIC) shape what discrimination is technically feasible. They observe the policy contest from the protocol layer.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, standards_bodies, observer,
    analytical, civilizational, analytical, universal).

% Communities without broadband access who are structurally excluded from the neutrality debate. The constraint's benefits (open internet) are moot where there is no internet. They would object to a neutrality regime that doesn't include universal service obligations, but they have no seat at the table.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, rural_unserved_communities, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of interoperable global networking: every endpoint can communicate with every other endpoint without bilateral agreements, permission, or discrimination by intermediaries. The end-to-end principle coordinates a single permissionless innovation space.
% TRANSFER_FUNCTION: Prevents transfer of economic value from edge providers and users to ISPs via paid prioritization, zero-rating, or discriminatory throttling. The constraint blocks a rent stream ISPs could otherwise collect by leveraging last-mile bottleneck control.
% ABSENT_VOICES: Rural and unserved communities who need universal access more than neutrality; developing-nation users for whom zero-rating provides their only affordable internet access; network researchers who argue QoS differentiation is technically necessary for real-time applications.
% DISAPPEARANCE_RATIONALE: If neutrality rules vanished overnight, ISPs would immediately deploy paid prioritization, zero-rating, and throttling. Edge innovators would face pay-to-play barriers; users would see tiered access; content providers would pay transit fees to ISPs. The internet would reorganize into a cable-TV-like model with gatekeepers.
% FOUNDING_PROBLEM: The early internet's architecture (TCP/IP) assumed cooperative, non-adversarial intermediaries. As commercial ISPs emerged with last-mile monopoly power, they gained the technical ability and economic incentive to discriminate — the founding problem is preventing bottleneck operators from extracting rents from the two-sided market they intermediate.
% FOUNDING_PROBLEM_CORROBORATION: Tim Berners-Lee, Vint Cerf, and the IETF community (outside ISP beneficiaries) attest the end-to-end principle was foundational. The FCC's 2015 Open Internet Order and EU's 2015 Telecoms Single Market Regulation independently corroborate the problem remains live. ISPs and telecom carriers contest this, arguing the problem is solved by competition (which corroboration from competition authorities finds insufficient in last-mile markets).
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.15) because the constraint primarily prevents extraction rather than extracting itself — it blocks ISP rent-seeking. Suppression (0.2) reflects regulatory enforcement against discrimination, not suppression of alternatives. Theater ratio (0.1) is low: the rule has real operational bite (Title II classification, BEREC guidelines). Accessibility collapse (0.3) is moderate: alternatives (zero-rating, specialized services) exist at the margins but the core coordination function holds. Resistance (0.75) is high: ISPs have litigated, lobbied, and technically worked around the rule for two decades.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP seat, the constraint looks like extraction (compliance costs, foregone revenue). From the edge innovator seat, it looks like essential coordination. The engine computes this divergence — the claimed_type (rope) reflects the structural reality from the analytical seat: genuine coordination with minimal extraction, actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and carriers are payers (d near 1.0): they bear compliance costs and forego revenue from discrimination. Edge innovators and users are beneficiaries (d near 0.0): the constraint subsidizes their access. Content providers are dual-role: they benefit from non-discrimination but pay for their own delivery infrastructure. Regulators are agenda_setters with analytical exit. The engine computes per-seat directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing bottleneck extraction) remains live — last-mile competition has not materialized in most markets. The constraint has not atrophied; its enforcement waxes and wanes with political cycles but the structural problem persists. No mandatrophy declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Does the TCP/IP protocol suite itself embody a normative non-discrimination requirement, or is the end-to-end principle a design heuristic that permits QoS differentiation?',
    'Historical analysis of the original RFCs (791, 793, 1122) and the Saltzer/Reed/Clark end-to-end arguments; IETF consensus process on DiffServ (RFC 2474/2475) and ECN (RFC 3168) as test cases.',
    'If the kernel itself is normatively neutral, the neutrality_reading is an extrapolation; if the kernel embodies non-discrimination, the prioritization_reading is a deviation. Changes the structural relationship between readings from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the end-to-end principle is a normative constraint or a design heuristic.').

omega_variable(
    last_mile_competition_trajectory,
    'Will emerging technologies (LEO satellite, 5G fixed wireless, municipal fiber) create sufficient last-mile competition to make neutrality rules unnecessary?',
    'Market structure analysis over 5-10 years: HHI trends, entry rates, price/concentration correlation in broadband markets with new entrants.',
    'If competition materializes, the coordination problem the constraint solves diminishes — the constraint could become a scaffold with a sunset clause. If not, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(last_mile_competition_trajectory, empirical, 'Whether technological change obsoletes the bottleneck problem.').

omega_variable(
    zero_rating_separability,
    'Is zero-rating (sponsored data) a neutral network management practice or a form of content-based discrimination that the neutrality_reading structurally forecloses?',
    'Regulatory adjudication (FCC, BEREC, TRAI) on specific zero-rating programs; economic analysis of whether zero-rating forecloses edge competition.',
    'If zero-rating is discrimination, zero_rating_reading is foreclosed by this reading. If zero-rating is neutral management, the readings coexist_with. Current regulatory split: EU bans, US (post-2017) permits, India bans.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_separability, empirical, 'Whether zero-rating falls inside or outside the neutrality constraint''s boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_neutrality_tr_t1995, tcp_ip_interpretation__neutrality_reading, theater_ratio, 1995, 0.02).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2000, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2005, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2010, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2015, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2020, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t2025, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(tcp_ip_neutrality_be_t1995, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 1995, 0.02).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2000, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2005, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2005, 0.12).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2010, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2015, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2015, 0.14).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2020, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2020, 0.16).
narrative_ontology:measurement(tcp_ip_neutrality_be_t2025, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_neutrality_su_t1995, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2000, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2005, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2010, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2015, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2020, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(tcp_ip_neutrality_su_t2025, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, information_standard).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.02).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, interconnection_agreements).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, cd_market_structure).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, universal_service_obligations).

% DUAL FORMULATION NOTE:
% Part of the tcp_ip_interpretation constraint family. This reading (neutrality) claims the kernel embodies non-discrimination; prioritization_reading claims the kernel permits QoS differentiation; zero_rating_reading claims the kernel allows sponsored exemptions. The three readings have different ε values and beneficiary/victim structures — they are distinct constraints linked by the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, institutional, 0.85).
constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, organized, 0.15).
constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, moderate, 0.1).
constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, powerful, 0.25).
constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
