% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: TCP/IP Prioritization Reading: Paid Fast Lane Authorization
 *   domain: technological/governance
 *
 * SUMMARY:
 *   This constraint instantiates the prioritization_reading of the
 *   tcp_ip_interpretation kernel. The kernel is the contested question of
 *   what TCP/IP implies about the legitimacy of traffic differentiation. This
 *   reading holds that TCP/IP's technical affordances for quality of
 *   serviceâDiffServ, IntServ, and packet prioritizationâauthorize
 *   broadband ISPs to sell prioritized delivery to content providers under
 *   the banner of network management. Sibling readings include
 *   neutrality_reading (end-to-end nondiscrimination as a normative
 *   requirement) and zero_rating_reading (selective exemption of sponsored
 *   content from data caps). The structural delta is that ISPs become
 *   gatekeepers extracting rents from edge providers, funded platforms gain
 *   asymmetric advantage, and unfunded services are disadvantaged.
 *
 * KEY AGENTS:
 *   - broadband_isps: agenda-setter and primary beneficiary (institutional power, arbitrage exit) â administers prioritization and collects fast-lane revenue
 *   - major_content_platforms: payer with secondary beneficiary status (powerful, constrained exit) â pays tolls, gains relative advantage
 *   - unfunded_edge_services: payer and victim (powerless, constrained exit) â cannot afford prioritization, suffers degraded reach
 *   - consumer_subscribers: mixed beneficiary/payer (organized, constrained exit) â gains QoS for funded apps, loses edge diversity
 *   - net_neutrality_advocates: excluded voice (organized, constrained exit) â argues for nondiscrimination but lacks forum access
 *   - telecom_regulators: observer (institutional, analytical exit) â adjudicates network-management claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.62).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.58).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Prioritization Reading: Paid Fast Lane Authorization").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technological/governance").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'a8705235-07c4-4046-a69d-1220a5b62f5d').
narrative_ontology:cs_kernel_codification('a8705235-07c4-4046-a69d-1220a5b62f5d', formalized).
narrative_ontology:cs_authority_grounding('a8705235-07c4-4046-a69d-1220a5b62f5d', expertise).
narrative_ontology:cs_interpretation_layer_present('a8705235-07c4-4046-a69d-1220a5b62f5d').
narrative_ontology:cs_reading_relation('a8705235-07c4-4046-a69d-1220a5b62f5d', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8705235-07c4-4046-a69d-1220a5b62f5d', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('a8705235-07c4-4046-a69d-1220a5b62f5d', foundational, network_management_trumps_nondiscrimination).
narrative_ontology:cs_axiom_status(network_management_trumps_nondiscrimination, holdable).
narrative_ontology:cs_axiom_grounding('a8705235-07c4-4046-a69d-1220a5b62f5d', network_management_trumps_nondiscrimination, instrumental).
narrative_ontology:cs_axiom('a8705235-07c4-4046-a69d-1220a5b62f5d', secondary, paid_prioritization_incentivizes_investment).
narrative_ontology:cs_axiom_status(paid_prioritization_incentivizes_investment, holdable).
narrative_ontology:cs_axiom_grounding('a8705235-07c4-4046-a69d-1220a5b62f5d', paid_prioritization_incentivizes_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('a8705235-07c4-4046-a69d-1220a5b62f5d', differentiated_services_framework).
narrative_ontology:cs_drift_state('a8705235-07c4-4046-a69d-1220a5b62f5d', contemporary_policy_gridlock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8705235-07c4-4046-a69d-1220a5b62f5d', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, consumer_subscribers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, major_content_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, consumer_subscribers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, major_content_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate last-mile and transit networks. Implement traffic shaping, packet inspection, and paid prioritization contracts with content providers. Justify differentiated treatment as network management and congestion control. Collect fast-lane fees and capture surplus from edge providers seeking guaranteed delivery quality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, broadband_isps, agenda_setter,
    institutional, generational, arbitrage, national).

% Pay broadband ISPs for prioritized delivery to subscribers. Absorb tolls or pass them through pricing. Gain competitive advantage over unfunded rivals who cannot afford prioritization. Exit is constrained because abandoning prioritized channels means degraded user experience and subscriber churn in markets where competitors pay.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, major_content_platforms, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, major_content_platforms, beneficiary).

% Operate startups, nonprofits, and independent services without capital to pay fast-lane fees. Experience degraded packet delivery during congestion, higher latency, and reduced effective reach to subscribers. Innovation and growth are constrained because prioritization costs represent a barrier to competitive performance before revenue scales.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    powerless, biographical, constrained, global).

% Receive improved quality for funded platforms that pay prioritization tolls. Suffer reduced diversity of viable new services and potential pass-through price increases. Have limited ISP choice in many markets and cannot individually opt out of the prioritization regime without losing broadband access entirely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, consumer_subscribers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, consumer_subscribers, payer).

% Argue that TCP/IP's end-to-end design principle requires nondiscrimination. Are structurally excluded from ISP peering negotiations and technical standardization forums where prioritization rules are operationalized. Would challenge the constraint if given equal bargaining power in regulatory and technical venues.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, net_neutrality_advocates, excluded,
    organized, generational, constrained, national).

% Evaluate whether ISP prioritization constitutes reasonable network management under telecommunications law or impermissible discrimination. Their rulings determine the legal envelope within which the constraint operates. They can impose remedies but are subject to political and jurisdictional limits.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages scarce last-mile bandwidth across heterogeneous traffic types, aiming to preserve application performance for latency-sensitive services and to incentivize capital expenditure on network infrastructure through direct revenue linkage.
% TRANSFER_FUNCTION: Moves capital from content providers to broadband ISPs in exchange for prioritized packet delivery; moves competitive advantage from unfunded edge services to funded platforms by degrading non-prioritized traffic during congestion.
% ABSENT_VOICES: Unfunded edge services and public-interest advocates are excluded from ISP interconnection negotiations and IETF decision processes where prioritization norms are ratified; they would argue for nondiscriminatory routing but lack institutional standing to block the constraint.
% DISAPPEARANCE_RATIONALE: If the prioritization authorization vanished, ISP fast-lane revenue would collapse, content providers would face a level transmission playing field, traffic engineering would revert to congestion-agnostic best-effort routing, and competitive dynamics at the edge would shift toward service quality rather than paid access quality.
% FOUNDING_PROBLEM: Early IP networks faced congestion collapse and could not guarantee performance for emerging real-time applications; best-effort routing alone was insufficient for voice, video, and critical data without some mechanism to differentiate treatment during scarcity.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers attest that DiffServ and QoS solve genuine congestion problems. Net neutrality advocates and independent telecommunications economists attest that overprovisioning has largely resolved last-mile scarcity and that the arrangement now functions primarily as rent extraction; regulatory comments and academic studies from outside the ISP beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is 0.62 because the ISP collects rents decoupled from marginal delivery cost. Suppression is 0.58 because unfunded edge providers lack viable alternative networks to reach subscribers; the constraint actively suppresses competition by making non-payment synonymous with degraded performance. Theater ratio is 0.40 because the network-management justification is partially genuineâQoS mechanisms do handle congestionâbut an increasing share of enforcement activity defends the revenue model rather than technical necessity. Accessibility collapse is 0.48 because alternatives (municipal broadband, mesh networks, platform self-hosting) are technically possible but commercially and geographically constrained. Resistance is 0.55 due to sustained net-neutrality advocacy, regulatory oscillation, and litigation. The measurement series share a single time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The ISP seat experiences the constraint as coordination: it solves congestion, funds capex, and delivers differentiated value. The unfunded edge seat experiences it as extraction: a pay-to-play barrier that taxes market entry. The subscriber seat is splitâbetter streaming versus fewer viable startups. The engine computes these divergences from the same structural data; the authored claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Broadband ISPs are the structural beneficiary (d near 0.0): they set the rules, enforce prioritization, and capture the gains. Unfunded edge services are structural targets (d near 1.0): they bear extraction through degraded performance and have the weakest exit. Major content platforms are also targets (d high) because they pay the toll, though their relative power moderates extraction somewhat. Consumer subscribers sit near symmetric (d ~0.5): they receive genuine QoS benefits while bearing indirect costs in reduced competition and potential pass-through pricing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure snare classification because the coordination functionâcongestion management and guaranteed delivery for latency-sensitive trafficâis structurally real and not merely cover. It avoids pure rope classification because the extraction is asymmetric: not all parties are net beneficiaries, and the arrangement requires active enforcement (packet inspection, contractual fast lanes, SLAs) to persist. The Tangled Rope classification captures the hybrid: genuine coordination fused with asymmetric rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prioritization_as_natural_reading,
    'Is differentiated service quality an inherent technical feature of TCP/IP that naturally supports the prioritization reading, or is the reading a post-hoc legal construction retrofitting business models onto protocol design?',
    'Historical analysis of IETF RFCs and early internet architecture documents versus ISP regulatory filings and court briefs to trace when the prioritization claim emerged.',
    'If the reading is retrofit, the constraint''s authority is conventionally constructed rather than technically grounded, raising effective extraction and lowering coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prioritization_as_natural_reading, conceptual, 'Whether the prioritization reading is technically grounded or post-hoc constructed').

omega_variable(
    congestion_solved_or_manufactured,
    'Is last-mile congestion a genuine technical problem requiring prioritized QoS, or has the problem been sustained by underinvestment to create demand for fast lanes?',
    'Comparative traffic engineering studies measuring congestion under neutral-overprovisioning regimes versus prioritized regimes with equivalent capital expenditure.',
    'If congestion is sustained by underinvestment, the coordination story is cover for extraction; if genuine, part of the extraction metric represents necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_solved_or_manufactured, empirical, 'Whether congestion is genuine or manufactured to justify prioritization revenue').

omega_variable(
    edge_competition_suppression,
    'Does paid prioritization suppress unfunded edge services primarily through technical degradation or through anticipatory deterrence of market entrants?',
    'Longitudinal entry-rate analysis in markets with and without prioritization regimes, controlling for other barriers to entry.',
    'Anticipatory deterrence indicates higher effective suppression than the technical measure alone suggests, because the constraint operates on expected rather than actual packet treatment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(edge_competition_suppression, empirical, 'Whether suppression is active degradation or anticipatory deterrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_prioritization_tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tcp_prioritization_tr_t5, tcp_ip_interpretation__prioritization_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(tcp_prioritization_tr_t10, tcp_ip_interpretation__prioritization_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(tcp_prioritization_tr_t15, tcp_ip_interpretation__prioritization_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(tcp_prioritization_tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(tcp_prioritization_tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(tcp_prioritization_tr_t30, tcp_ip_interpretation__prioritization_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp_prioritization_be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tcp_prioritization_be_t5, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(tcp_prioritization_be_t10, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(tcp_prioritization_be_t15, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(tcp_prioritization_be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(tcp_prioritization_be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(tcp_prioritization_be_t30, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tcp_prioritization_su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp_prioritization_su_t5, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(tcp_prioritization_su_t10, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(tcp_prioritization_su_t15, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(tcp_prioritization_su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(tcp_prioritization_su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(tcp_prioritization_su_t30, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the tcp_ip_interpretation kernel family. The kernel decomposes into three structurally distinct readings because 'TCP/IP interpretation' conflates technical description, normative end-to-end principle, and business-model authorization. Each reading carries a different epsilon and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
