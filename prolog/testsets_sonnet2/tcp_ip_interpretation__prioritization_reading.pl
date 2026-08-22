% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: TCP/IP Prioritization Reading — Paid Fast Lanes as Network Management
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates the prioritization reading of the TCP/IP kernel:
 *   the claim that differentiated service quality (paid fast lanes, QoS-based
 *   traffic management) is a legitimate exercise of the protocol's design
 *   affordances rather than a violation of a neutrality norm. The reading
 *   treats congestion management as the genuine coordination problem and
 *   commercial paid prioritization as a permissible, even necessary,
 *   mechanism for funding network investment. Two sibling constraints exist
 *   for the same kernel and are NOT part of this story: neutrality_reading
 *   (TCP/IP as embodying mandatory non-discrimination) and
 *   zero_rating_reading (selective exemption of sponsored content from data
 *   caps). Each reading has its own ε, beneficiary/victim structure, and
 *   classification; this file does not average or hedge across them.
 *
 * KEY AGENTS:
 *   - dominant_isps: agenda_setter/beneficiary (institutional/arbitrage) — implements and profits from prioritization deals
 *   - large_content_platforms_able_to_pay: beneficiary (powerful/mobile) — buys priority queuing to entrench market position
 *   - unfunded_edge_service_providers: payer (moderate/constrained) — degraded delivery, cannot negotiate directly
 *   - independent_startups: payer (powerless/trapped) — structurally disadvantaged at launch
 *   - residential_broadband_subscribers: payer/beneficiary (powerless/constrained) — pays for uniform access, receives differentiated quality
 *   - network_engineers_and_standards_bodies: observer (analytical) — sees the technical affordance, not the commercial policy layer
 *   - telecom_regulators: observer (institutional) — adjudicates legitimacy of paid prioritization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.47).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Prioritization Reading — Paid Fast Lanes as Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '34be6ebb-e466-4905-8c45-327e0e6dd8b4').
narrative_ontology:cs_kernel_codification('34be6ebb-e466-4905-8c45-327e0e6dd8b4', fixed_text).
narrative_ontology:cs_authority_grounding('34be6ebb-e466-4905-8c45-327e0e6dd8b4', practice).
narrative_ontology:cs_interpretation_layer_present('34be6ebb-e466-4905-8c45-327e0e6dd8b4').
narrative_ontology:cs_reading_relation('34be6ebb-e466-4905-8c45-327e0e6dd8b4', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('34be6ebb-e466-4905-8c45-327e0e6dd8b4', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('34be6ebb-e466-4905-8c45-327e0e6dd8b4', foundational, differentiated_queuing_is_within_protocol_design_intent).
narrative_ontology:cs_axiom_status(differentiated_queuing_is_within_protocol_design_intent, holdable).
narrative_ontology:cs_axiom_grounding('34be6ebb-e466-4905-8c45-327e0e6dd8b4', differentiated_queuing_is_within_protocol_design_intent, conventional).
narrative_ontology:cs_axiom('34be6ebb-e466-4905-8c45-327e0e6dd8b4', secondary, commercial_prioritization_is_legitimate_extension_of_congestion_management).
narrative_ontology:cs_axiom_status(commercial_prioritization_is_legitimate_extension_of_congestion_management, holdable).
narrative_ontology:cs_axiom_grounding('34be6ebb-e466-4905-8c45-327e0e6dd8b4', commercial_prioritization_is_legitimate_extension_of_congestion_management, instrumental).
narrative_ontology:cs_reference_frame('34be6ebb-e466-4905-8c45-327e0e6dd8b4', end_to_end_best_effort_delivery).
narrative_ontology:cs_drift_state('34be6ebb-e466-4905-8c45-327e0e6dd8b4', commercial_broadband_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34be6ebb-e466-4905-8c45-327e0e6dd8b4', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, dominant_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_content_platforms_able_to_pay).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_service_providers).
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

% Control the physical last-mile infrastructure and implement traffic-shaping and paid prioritization arrangements, framing them as legitimate quality-of-service management necessary to fund network buildout. They negotiate paid fast-lane deals directly with large content providers and set the technical criteria for which traffic gets preferential queuing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, dominant_isps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, dominant_isps, beneficiary).

% Have the capital to pay for prioritized delivery of latency-sensitive traffic (video streaming, gaming, video calls) and treat the payment as a cost of doing business that entrenches their market position relative to smaller rivals who cannot pay the same rates.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_content_platforms_able_to_pay, beneficiary,
    powerful, biographical, mobile, global).

% Cannot afford prioritization payments and see their traffic relegated to best-effort delivery, degrading service quality relative to well-funded competitors on the same network. They lack the market power to negotiate directly with ISPs and cannot route around the last-mile bottleneck.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_service_providers, payer,
    moderate, biographical, constrained, national).

% Depend entirely on best-effort delivery to reach residential users through incumbent ISPs; a prioritization regime imposes a structural disadvantage on any latency-sensitive product they might build, since they cannot match incumbents' fast-lane payments at launch scale.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, independent_startups, payer,
    powerless, biographical, trapped, national).

% Pay for a broadband connection expecting it to reach all lawful services equivalently; under prioritization they receive better quality for services that paid for fast lanes and degraded quality for those that did not, without having negotiated or consented to this differential themselves. In markets with real ISP competition they retain some choice; in monopoly/duopoly markets they do not.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, beneficiary).

% Maintain that TCP/IP's design permits differentiated queuing (DiffServ, QoS markings) as a technical matter, but take no position on whether commercial paid prioritization is a legitimate use of that technical affordance or a policy choice riding on top of it.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, network_engineers_and_standards_bodies, observer,
    analytical, civilizational, analytical, global).

% Adjudicate whether paid prioritization arrangements constitute permissible network management or unlawful discrimination, and can impose transparency or nondiscrimination rules that would alter which reading of the protocol governs in practice.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, dominant_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Differentiated queuing lets ISPs allocate scarce last-mile bandwidth to latency-sensitive traffic (real-time video, voice, gaming) ahead of loss-tolerant traffic (bulk downloads, email), which is a genuine engineering problem when links are congested.
% TRANSFER_FUNCTION: Moves priority queuing capacity from services that cannot pay to services that can, and moves the cost of that priority from ISPs (who would otherwise need to invest in more capacity) to paying content providers and, indirectly, to subscribers who experience uneven quality across services on the plan they already purchased.
% ABSENT_VOICES: Independent startups and unfunded edge services would object that the fast-lane market structurally favors incumbents at the moment they can least afford to compete, but they are not party to the private commercial negotiations between ISPs and large platforms that establish prioritization terms.
% DISAPPEARANCE_RATIONALE: If ISPs could no longer implement paid prioritization, large platforms would lose a lever for locking in delivery advantages, edge services would compete on equal best-effort footing, and ISPs would need to rely on capacity investment or usage-based pricing rather than prioritization deals to manage congestion — the commercial relationship between ISPs and large content providers would have to be restructured.
% FOUNDING_PROBLEM: Best-effort-only delivery under real congestion degrades latency-sensitive applications (voice, video conferencing) indiscriminately alongside loss-tolerant bulk transfer, creating a genuine technical case for some form of traffic differentiation during congestion.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers and standards bodies (analytical, outside the beneficiary set) attest that congestion-responsive QoS is a real technical problem TCP/IP's design accommodates; telecom regulators and public-interest technologists attest that commercial paid-prioritization arrangements go well beyond congestion management and function as a rent extraction mechanism on top of a real but narrower technical affordance — the ISPs' own framing is the only voice claiming the two are identical.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.58 (moderate-high, not extreme): there IS a genuine technical coordination function (congestion-responsive QoS) underneath the commercial arrangement, but the arrangement as actually operated extracts a toll disproportionate to the underlying engineering necessity, and the toll rises over the interval as prioritization deals commercialize and standardize. Suppression (0.47) reflects that ISPs hold last-mile bottleneck power over subscribers and edge providers, but is not near-total because some markets retain competitive ISP choice and because standards bodies and regulators remain live counterweights. Theater ratio (0.42) captures that a rising share of 'network management' framing serves to launder commercial prioritization deals as neutral engineering necessity rather than reflecting actual congestion-driven need — the theater grows as the practice becomes routine rather than exceptional.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP agenda-setter seat, this looks like legitimate infrastructure investment funding — a rope. From the unfunded edge-provider and independent-startup payer seats, the identical prioritization mechanism looks like a toll booth erected on a resource they already paid to access via their own upstream connectivity costs. The engine computes these divergent seat classifications from the same structural data; the claimed_type here documents only the analytical/authoring judgment that the tangled structure is real, not a resolution of the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant ISPs sit at the full-beneficiary/agenda-setter pole: they design, negotiate, and enforce the prioritization scheme and collect payment for it. Large content platforms able to pay are beneficiaries with mobile exit (they could in principle build alternate delivery paths, e.g. CDNs, direct peering) but choose to pay because it entrenches their position. Unfunded edge providers and independent startups are targets: they cannot pay, cannot exit the last-mile bottleneck, and structurally absorb the disadvantage. Residential subscribers are dual-positioned — they get real congestion-management benefit in principle, but pay for a connection whose effective quality is now determined by third-party payment decisions they never made, which is why they carry both beneficiary and payer roles with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare or rope is deliberate: there is a real, non-manufactured coordination problem (congestion-responsive queuing) that a pure snare framing would miss, but there is also identifiable asymmetric extraction riding on that coordination function that a pure rope framing would launder away. Treating this as a rope would mistake the commercial toll for the engineering necessity; treating it as a pure snare would deny that any genuine technical problem exists. The tangled_rope classification requires both beneficiaries and victims plus active enforcement — all three are present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engineering_necessity_vs_commercial_toll_boundary,
    'How much of currently deployed paid prioritization is responsive to genuine, measurable congestion versus a commercial arrangement layered onto links with adequate capacity?',
    'Independent network measurement studies correlating prioritization deal terms with actual congestion data on the affected links, ideally via regulatory subpoena of ISP capacity utilization records.',
    'If prioritization tracks real congestion tightly, the coordination function dominates and the constraint reads closer to a rope; if prioritization is deployed on uncongested links or scales with payment rather than need, the extraction component dominates and the constraint reads closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_necessity_vs_commercial_toll_boundary, empirical, 'Whether paid prioritization is congestion-responsive engineering or decoupled rent extraction.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the prioritization reading a faithful technical reading of what TCP/IP''s design permits, or a policy choice dressed in the vocabulary of protocol design to claim the authority of engineering necessity?',
    'Comparative analysis of the original IETF design documents and RFCs discussing QoS/DiffServ intent versus contemporary commercial deployment patterns; testimony from protocol designers on original intent versus current use.',
    'If the reading is a faithful technical account, ISPs'' framing carries real authority; if it is a retrofitted justification, the prioritization reading''s claim to speak for ''what TCP/IP permits'' is itself part of the extraction mechanism — using technical authority to legitimate a commercial arrangement the protocol''s designers did not anticipate or endorse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is a technical fact about the protocol or a policy claim borrowing technical authority — the committer-axis ambiguity at the heart of the kernel contest.').

omega_variable(
    sibling_reading_resource_competition,
    'Does the prioritization_reading''s institutional entrenchment (regulatory approval, commercial normalization) foreclose meaningful adoption of the neutrality_reading in the same jurisdiction, or can both readings coexist across different regulatory regimes and time periods?',
    'Track regulatory history in jurisdictions that have flipped between net neutrality rules and prioritization-permissive rules (e.g., US FCC rule changes 2015/2017/2024) to see whether reversal is structurally difficult once prioritization infrastructure and commercial contracts are established.',
    'If switching costs are high once prioritization deals are in place, this reading''s persistence actively suppresses the neutrality_reading''s future viability even where it doesn''t logically foreclose it — relevant to the reading_relations declared below.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether commercial entrenchment under this reading creates practical (not logical) pressure against reverting to the neutrality reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__prioritization_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__prioritization_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'net neutrality debate' / 'what TCP/IP requires' claim, per the ε-invariance principle: neutrality_reading (Mountain/Rope-leaning, non-discrimination as design commitment), prioritization_reading (this file, Tangled Rope — congestion management coordination function plus commercial extraction), and zero_rating_reading (a narrower selective-exemption claim with its own beneficiary/victim structure around sponsored-content arrangements). The three do not share an ε; each is authored independently and linked here for contamination/family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
