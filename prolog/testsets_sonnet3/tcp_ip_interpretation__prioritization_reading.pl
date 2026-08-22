% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: TCP/IP Read as Permitting Paid Prioritization (Network Management Reading)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates one reading within the contested TCP/IP kernel:
 *   that the protocol's silence on quality-of-service enforcement should be
 *   read as permission for ISPs to implement differentiated traffic handling,
 *   including paid fast lanes, framed as legitimate network management. This
 *   is distinct from the neutrality_reading (which reads the same protocol as
 *   embodying an end-to-end non-discrimination principle) and the
 *   zero_rating_reading (which concerns selective content-cost exemptions
 *   rather than latency/bandwidth prioritization). Each reading is authored
 *   as its own constraint with its own ε; this file addresses only the
 *   prioritization reading's structural claim and its own beneficiary/victim
 *   map, not the kernel contest as a whole.
 *
 * KEY AGENTS:
 *   - last_mile_isps: agenda_setter (institutional/arbitrage) — sets and enforces the prioritization interpretation, collects fast-lane revenue
 *   - large_incumbent_content_platforms: beneficiary (powerful/mobile) — benefits from widened competitive moat via paid prioritization
 *   - unfunded_edge_services: payer (moderate/constrained) — bears degraded delivery without capital to buy parity
 *   - independent_startups: payer (moderate/constrained) — faces raised effective barrier to entry
 *   - residential_broadband_subscribers: payer/beneficiary (powerless/trapped) — mixed effects, no control over which upstream services are prioritized
 *   - network_engineering_standards_bodies: observer (institutional/analytical) — did not adjudicate the interpretive dispute in the protocol text
 *   - telecom_regulators: excluded (institutional/analytical) — jurisdiction to arbitrate has been narrowed in several jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.61).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.52).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Read as Permitting Paid Prioritization (Network Management Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '94a44ceb-ed8b-4516-b565-cd42c216794a').
narrative_ontology:cs_kernel_codification('94a44ceb-ed8b-4516-b565-cd42c216794a', fixed_text).
narrative_ontology:cs_authority_grounding('94a44ceb-ed8b-4516-b565-cd42c216794a', practice).
narrative_ontology:cs_interpretation_layer_present('94a44ceb-ed8b-4516-b565-cd42c216794a').
narrative_ontology:cs_reading_relation('94a44ceb-ed8b-4516-b565-cd42c216794a', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('94a44ceb-ed8b-4516-b565-cd42c216794a', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('94a44ceb-ed8b-4516-b565-cd42c216794a', foundational, network_operators_may_differentiate_traffic_for_management_purposes).
narrative_ontology:cs_axiom_status(network_operators_may_differentiate_traffic_for_management_purposes, holdable).
narrative_ontology:cs_axiom_grounding('94a44ceb-ed8b-4516-b565-cd42c216794a', network_operators_may_differentiate_traffic_for_management_purposes, instrumental).
narrative_ontology:cs_axiom('94a44ceb-ed8b-4516-b565-cd42c216794a', secondary, quality_of_service_is_a_legitimate_paid_commodity).
narrative_ontology:cs_axiom_status(quality_of_service_is_a_legitimate_paid_commodity, holdable).
narrative_ontology:cs_axiom_grounding('94a44ceb-ed8b-4516-b565-cd42c216794a', quality_of_service_is_a_legitimate_paid_commodity, conventional).
narrative_ontology:cs_reference_frame('94a44ceb-ed8b-4516-b565-cd42c216794a', best_effort_delivery_with_engineering_discretion).
narrative_ontology:cs_drift_state('94a44ceb-ed8b-4516-b565-cd42c216794a', post_2015_open_internet_order_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('94a44ceb-ed8b-4516-b565-cd42c216794a', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, last_mile_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_incumbent_content_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, independent_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the physical last-mile bottleneck and interpret TCP/IP's lack of an explicit non-discrimination mandate as license to sell prioritized delivery lanes and interconnection terms. They set traffic-shaping policy, negotiate paid peering and paid prioritization deals, and justify differentiated queuing as legitimate congestion management. They collect the prioritization fees directly and control the technical means of enforcing the reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, last_mile_isps, agenda_setter,
    institutional, generational, arbitrage, national).

% Have the capital to pay for fast lanes or negotiate favorable interconnection, and their scale means differentiated service actually widens their delivery advantage over smaller rivals. They can absorb or pass through prioritization costs and can shift traffic across multiple ISPs and CDNs, so the reading costs them comparatively little while advantaging their competitive position.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_incumbent_content_platforms, beneficiary,
    powerful, biographical, mobile, global).

% Small services, nonprofits, and public-interest applications (e.g. telehealth pilots, community education platforms) that cannot afford fast-lane fees. Under the prioritization reading their packets are technically permitted to be queued behind paid traffic during congestion, degrading service quality in ways they cannot remedy without capital they lack. They have no meaningful exit — leaving means not reaching users on that ISP's network at all.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    moderate, biographical, constrained, national).

% Compete against incumbents who can pay for prioritization; startups must either raise capital to buy equivalent treatment or accept degraded delivery relative to funded rivals. The reading raises the effective cost of market entry for any bandwidth-sensitive service, entrenching incumbents structurally rather than through product quality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, independent_startups, payer,
    moderate, biographical, constrained, national).

% Pay for a connection but experience its effective quality shaped by which upstream services paid for priority, not by their own subscription tier. In markets with limited ISP competition they cannot switch providers to avoid a given prioritization scheme. Some may see genuinely improved reliability for latency-sensitive services (telemedicine, VoIP) that the network-management framing does fund — the benefit is real but distributed unevenly and not something subscribers control or can price.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, beneficiary).

% Bodies like the IETF that wrote and maintain the TCP/IP specification documents. They did not adjudicate the neutrality-versus-prioritization dispute in the protocol text itself; they observe the interpretive fight from outside, sometimes issuing clarifying commentary on what the protocol technically permits versus what policy layers on top.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, network_engineering_standards_bodies, observer,
    institutional, civilizational, analytical, global).

% Would adjudicate whether the prioritization reading constitutes unreasonable discrimination under telecommunications law, but jurisdiction is frequently contested, underfunded, or captured by industry comment processes; in several jurisdictions their authority to act on this reading has been legislatively or judicially narrowed, leaving the ISP's own interpretation operative by default.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, last_mile_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real engineering problem: during congestion, some differentiation of traffic classes (e.g. deprioritizing bulk file transfer behind real-time voice/video) improves aggregate network performance and can be technically necessary rather than arbitrary.
% TRANSFER_FUNCTION: Moves effective delivery quality and market advantage from unfunded and small edge services toward ISPs (fee revenue) and large incumbent platforms (competitive moat), routed through the technical mechanism of packet queuing and paid interconnection.
% ABSENT_VOICES: Small edge services and public-interest applications that cannot afford prioritization fees have no seat in ISP peering negotiations or standards discussions; telecom regulators who might otherwise arbitrate 'reasonable network management' have been structurally sidelined in several jurisdictions.
% DISAPPEARANCE_RATIONALE: ISPs and large platforms would say network investment and legitimate congestion management collapse without the ability to differentiate service, degrading everyone's experience. Edge services and consumer advocates would say the prior open-internet baseline (best-effort, non-discriminatory delivery) is exactly what would be restored, and their situation would improve. The disagreement is genuinely substantive, not merely rhetorical.
% FOUNDING_PROBLEM: TCP/IP's original best-effort delivery model has no formal quality-of-service guarantee; as real-time and bandwidth-intensive applications proliferated, congestion management became a genuine technical problem that the base protocol does not solve on its own.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers and standards bodies attest that congestion management is a live, real technical problem independent of any commercial motive. However, independent economic analyses commissioned by public-interest groups and testimony from smaller ISPs and edge-service operators — outside the beneficiary set of large ISPs and incumbent platforms — argue that paid prioritization schemes as actually deployed exceed what congestion management technically requires and function primarily as rent extraction and competitive gatekeeping.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, contested).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.61 (substantial but not maximal) because the reading rests on a genuine technical function — congestion management is real — layered with a commercial extraction mechanism that goes beyond what congestion management strictly requires. Suppression at 0.52 reflects that alternatives (net-neutrality rules, common-carrier reclassification) exist and are actively fought over in legislatures and courts rather than foreclosed outright; this is not a settled monopoly-style suppression but an ongoing, resistance-facing arrangement. Theater ratio rises across the measured interval (0.20 to 0.40) as 'network management' justifications increasingly cover commercially motivated prioritization deals rather than genuine congestion events — a Goodhart-style drift where the stated function (managing congestion) is increasingly cited to justify activity (selling fast lanes) that exceeds it.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP's agenda-setting seat, this looks like legitimate, engineering-justified network operation they built and maintain at real capital cost. From the unfunded edge-service payer seat, the identical queuing and interconnection policy operates as involuntary tax on market access with no meaningful recourse. The engine computes these as different seat-level types from the same structural data; the divergence is exactly what a contested kernel reading is expected to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Last-mile ISPs sit at the full-beneficiary end: they administer the interpretation, capture the prioritization fee revenue, and have durable market power over the last mile with essentially no exit pressure. Large incumbent platforms are beneficiaries by capital advantage even though they are nominally 'payers' for prioritization — their scale converts a cost into a competitive moat, so their effective directionality is much closer to beneficiary than the naive payer label would suggest. Unfunded edge services and independent startups are structural targets: trapped by the fact that leaving one ISP's network is not a substitute for reaching that ISP's subscriber base. Residential subscribers are the most powerless seat, carrying diffuse costs (traffic shaped by upstream deals they cannot see or negotiate) alongside occasional genuine service-quality gains for latency-sensitive applications they use.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — congestion management on a best-effort network — remains partially live (real-time applications genuinely benefit from differentiated queuing under true congestion), which is exactly why this reading cannot be classified as a pure snare: there is a real coordination function. But the tangled_rope classification captures that the same enforcement machinery that manages genuine congestion is also used to extract rents from parties who have no bearing on the congestion problem itself (a startup's packets are not the cause of a congestion event, yet they pay the cost of it). Treating this as either 'pure coordination' (rope) or 'pure extraction' (snare) would mislabel one real half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the TCP/IP protocol''s lack of an explicit non-discrimination mandate constitute affirmative permission for differentiated service, or is it merely silence that later policy layers (net neutrality rules, common carrier law) are meant to fill?',
    'This is a genealogical and legal question, not a technical one: it would be resolved (in the sense of settling which reading governs a given jurisdiction) by binding regulatory or judicial determination of whether ISPs are common carriers subject to non-discrimination duties, and by historical analysis of IETF documents'' intent regarding quality-of-service extensions.',
    'If the neutrality_reading is judicially/regulatorily entrenched, the prioritization_reading''s enforcement (fast-lane deals) becomes legally foreclosed in that jurisdiction, converting this constraint''s extraction toward zero. If the prioritization_reading remains legally live, ISPs retain the enforcement basis this story describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether protocol silence should be read as permission or as a gap for policy to fill — the core interpretive fork of the kernel.').

omega_variable(
    congestion_management_boundary_ambiguity,
    'Where is the line between traffic differentiation that is technically necessary for congestion management and traffic differentiation that exceeds congestion management and functions as commercial extraction?',
    'Independent technical audit of actual network congestion data correlated with which traffic classes receive prioritization, and whether prioritization deals are struck absent measurable congestion.',
    'A tight correlation between congestion events and differentiation would support a rope-leaning reading of at least part of the mechanism; a loose or absent correlation (prioritization sold regardless of congestion state) would support reclassifying more of the mechanism toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_management_boundary_ambiguity, empirical, 'Whether differentiated service tracks actual congestion or is decoupled commercial extraction.').

omega_variable(
    sibling_reading_relationship_ambiguity,
    'Does adopting the prioritization_reading in a given legal framework logically foreclose the neutrality_reading in that same framework, or can regulators hold both simultaneously (e.g. permitting ''reasonable'' prioritization while formally endorsing a neutrality principle)?',
    'Comparative analysis of actual regulatory frameworks: jurisdictions that have adopted ''reasonable network management'' exceptions within otherwise neutrality-oriented rules suggest coexistence is legally possible; jurisdictions with strict common-carrier reclassification suggest genuine foreclosure.',
    'If coexistence is legally common, this reading''s relationship to neutrality_reading is better modeled as coexists_with (as currently declared) rather than forecloses; if strict regimes are dominant, the relationship may be closer to mutual foreclosure within those specific frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_relationship_ambiguity, conceptual, 'Whether the prioritization and neutrality readings can coexist within a single regulatory framework or are mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(tcp__tr_t2009, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2009, 0.25).
narrative_ontology:measurement(tcp__tr_t2013, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2013, 0.3).
narrative_ontology:measurement(tcp__tr_t2017, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(tcp__tr_t2021, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(tcp__tr_t2025, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(tcp__be_t2009, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2009, 0.4).
narrative_ontology:measurement(tcp__be_t2013, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2013, 0.48).
narrative_ontology:measurement(tcp__be_t2017, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(tcp__be_t2021, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(tcp__be_t2025, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(tcp__su_t2009, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(tcp__su_t2013, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2013, 0.42).
narrative_ontology:measurement(tcp__su_t2017, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(tcp__su_t2021, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2021, 0.5).
narrative_ontology:measurement(tcp__su_t2025, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'net neutrality debate' / 'TCP/IP interpretation' concept per the ε-invariance principle: neutrality_reading (end-to-end non-discrimination, low extraction, rope-leaning), prioritization_reading (this file — paid differentiated service, substantially extractive, tangled_rope), and zero_rating_reading (sponsored-content data exemptions, a distinct commercial mechanism). Each carries its own ε and stakeholder map; none is a measurement of the others under a different observable. Linked bidirectionally via affects_constraints since regulatory action on any one reading changes the legal and commercial viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
