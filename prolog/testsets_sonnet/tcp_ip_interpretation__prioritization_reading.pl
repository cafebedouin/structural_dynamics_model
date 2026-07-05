% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the 'prioritization reading' of the TCP/IP
 *   kernel: the position that the protocol suite's design permits, and in
 *   some framings requires, differentiated handling of packets as legitimate
 *   network management rather than a violation of any embedded neutrality
 *   norm. Under this reading, ISPs implement paid fast lanes and
 *   QoS-differentiated interconnection as a response to real congestion and
 *   capacity constraints. The reading's own literature emphasizes network
 *   investment incentives; independent technical and economic review
 *   documents that most deployed prioritization arrangements exceed anything
 *   congestion management requires and instead function as a toll on
 *   interconnection access, concentrated on the platforms and services least
 *   able to pay. This is a distinct constraint from the sibling
 *   neutrality_reading (which treats non-discrimination as the protocol's
 *   normative core, obligation-bearing rather than option-bearing) and from
 *   zero_rating_reading (which concerns selective content-cost exemption
 *   rather than latency/throughput prioritization) — each carries its own ε
 *   and stakeholder structure per the ε-invariance principle, linked here by
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - last_mile_isps: agenda-setter and beneficiary — administers and profits from prioritization
 *   - well_funded_content_platforms: beneficiary — buys guaranteed delivery quality
 *   - unfunded_edge_startups: payer — degraded relative service quality with no purchasing power
 *   - nonprofit_and_civic_services: payer — trapped at the bottom of the residual best-effort queue
 *   - residential_broadband_subscribers: payer/beneficiary — experiences quality determined by third-party deals
 *   - telecom_regulators: observer — adjudicates reasonable network management vs. discrimination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.61).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.52).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Prioritization Reading — Paid Fast Lanes as Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1').
narrative_ontology:cs_kernel_codification('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', fixed_text).
narrative_ontology:cs_authority_grounding('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', distributed).
narrative_ontology:cs_reading_relation('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', foundational, differentiated_qos_is_legitimate_network_management).
narrative_ontology:cs_axiom_status(differentiated_qos_is_legitimate_network_management, holdable).
narrative_ontology:cs_axiom_grounding('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', differentiated_qos_is_legitimate_network_management, instrumental).
narrative_ontology:cs_axiom('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', secondary, paid_prioritization_incentivizes_infrastructure_investment).
narrative_ontology:cs_axiom_status(paid_prioritization_incentivizes_infrastructure_investment, holdable).
narrative_ontology:cs_axiom_grounding('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', paid_prioritization_incentivizes_infrastructure_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', best_effort_delivery_baseline).
narrative_ontology:cs_drift_state('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', post_2015_open_internet_order_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5bc2bb7-5803-4e0a-8aa1-23afd1cff0e1', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, last_mile_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, well_funded_content_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, nonprofit_and_civic_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_management_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the physical last-mile infrastructure and administers the interconnection and traffic-shaping policies that determine which packets get priority. Sells prioritized delivery tiers to content providers and justifies the practice as necessary congestion management and a funding mechanism for network buildout. Controls the technical means of enforcement (deep packet inspection, QoS tagging) and faces essentially no exit from its own rule-making role.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, last_mile_isps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, last_mile_isps, beneficiary).

% Pays for prioritized delivery lanes to guarantee low-latency streaming and interactive service quality. Has the capital to negotiate favorable interconnection agreements and can multi-home across several ISPs or build private peering, giving it leverage the ISP's other counterparties lack.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, well_funded_content_platforms, beneficiary,
    powerful, biographical, mobile, global).

% Cannot afford paid prioritization and is relegated to best-effort delivery alongside a growing share of congestion-managed traffic. Competing against incumbents who can buy the fast lane, its latency-sensitive features degrade for reasons invisible to its own users, and it has no meaningful way to negotiate directly with every last-mile ISP its users sit behind.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_startups, payer,
    moderate, biographical, constrained, national).

% Runs low-budget public-interest services (telehealth portals, emergency alerting, civic information) with no budget for prioritization fees. Depends entirely on best-effort delivery quality set as a residual after paid traffic is served first, with no bargaining position vis-a-vis the ISPs whose infrastructure it depends on.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, nonprofit_and_civic_services, payer,
    powerless, biographical, trapped, regional).

% Pays a subscription fee for a connection whose effective quality for any given service now depends on side deals between the ISP and content providers rather than solely on the plan purchased. Benefits when a favored service performs well, bears the cost when non-prioritized traffic (including their own uploads, gaming, or civic-service use) is squeezed. Switching ISPs is often infeasible due to limited last-mile competition.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, residential_broadband_subscribers, beneficiary).

% Adjudicates whether specific prioritization arrangements constitute reasonable network management or unlawful discrimination. Hears testimony from all sides, commissions technical studies of congestion patterns, and can rewrite the rules that let this reading of TCP/IP stand or fall.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, last_mile_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Differentiated queuing and QoS marking allow scarce last-mile capacity to be allocated so that latency-sensitive traffic (video calls, live streaming, interactive gaming) is not degraded by bulk transfers sharing the same congested link — a real technical coordination problem when capacity is finite.
% TRANSFER_FUNCTION: Moves guaranteed low-latency delivery to whichever content provider pays the ISP for prioritization, and correspondingly moves degraded best-effort service quality onto everyone who does not pay — including services and subscribers who have no direct commercial relationship with the paying party.
% ABSENT_VOICES: Unfunded edge startups, nonprofit and civic services, and ordinary subscribers whose non-prioritized traffic is squeezed have no seat in the private interconnection negotiations between ISPs and well-funded platforms that set the prioritization terms; they experience the outcome without participating in setting it.
% DISAPPEARANCE_RATIONALE: If paid prioritization were disallowed overnight, ISPs would lose a revenue stream and would need to fund capacity expansion through other means (subscriber rates, public investment, or genuine congestion-responsive management applied uniformly); well-funded platforms would lose a competitive advantage over smaller rivals; civic and nonprofit services would see delivery quality determined by actual congestion rather than by who paid, materially changing competitive dynamics on the edge.
% FOUNDING_PROBLEM: Best-effort, undifferentiated packet delivery struggles under real congestion to support latency-sensitive applications (voice, video, real-time interaction) that did not exist when TCP/IP's original best-effort model was designed; some mechanism for prioritizing time-sensitive traffic during congestion has a genuine technical basis.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers and standards bodies (e.g. IETF differentiated-services literature) corroborate that congestion-responsive QoS mechanisms address a real technical problem. However, competition economists and civil-society technical audits — sources outside the ISPs and paying platforms that benefit from the current implementation — report that most deployed prioritization schemes are used to extract fees from platforms rather than to solve documented congestion, and that ISPs frequently maintain capacity scarcity that priority tiers then monetize.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.61) reflects that prioritization fees are set well above the marginal cost of the queuing/QoS infrastructure itself and are increasingly used as a toll rather than a congestion-response mechanism, evidenced by the rising trajectory across the measurement interval. Suppression (0.52) is moderate: exit for subscribers is constrained by limited last-mile competition, and edge services cannot bypass ISP interconnection chokepoints, but formal legal alternatives (regulatory complaint, litigation) do exist and are exercised. Theater ratio (0.38) captures that a genuine technical congestion-management function underlies the practice, but a growing share of prioritization activity is now oriented toward monetizing artificially preserved scarcity rather than addressing documented congestion. Accessibility collapse (0.47) is mid-range — some ISPs and jurisdictions maintain non-discriminatory alternatives, so the collapse is partial, not total, unlike a genuine natural-law constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the last_mile_isps' seat this reads as coordination it invented and maintains to solve a genuine scarcity problem. From the unfunded_edge_startups' and nonprofit_and_civic_services' seats the same rule structure operates as an unaccountable toll booth on infrastructure they have no alternative route around. The engine's per-seat computation should register this divergence directly from the declared power/exit/scope data rather than from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Last_mile_isps sit at the extreme beneficiary end: they set the priority rules and collect the fees. Well_funded_content_platforms are secondary beneficiaries with mobile exit (multi-homing, private peering) that damps their effective extraction. Unfunded_edge_startups and nonprofit_and_civic_services sit near the full-target end: they cannot pay for priority, cannot exit the ISP's territorial monopoly, and bear the residual degradation. Residential subscribers are intermediate — nominal beneficiaries of any given prioritized service they use, but structurally payers whenever a non-favored use (their own uploads, competing services) is squeezed; their constrained exit (limited last-mile competition) pushes their effective directionality toward the target end despite the beneficiary framing in the ISP's own coordination story.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) preserves the genuine coordination function: congestion-responsive prioritization for latency-sensitive traffic addresses a real technical problem that existed before any commercial fast-lane market. Classifying this outright as a snare would mislabel legitimate QoS engineering as pure extraction. Classifying it as a rope, however, would ignore the asymmetric extraction machinery layered onto that function — the fee structure, artificially preserved scarcity, and the absence of unfunded parties from the negotiations that set prioritization terms. Tangled_rope captures both: real coordination value at the technical layer, concentrated extraction at the commercial layer, requiring active enforcement (DPI, contractual interconnection terms) to sustain the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_congestion_vs_manufactured_scarcity,
    'Is the congestion that prioritization purports to manage a genuine, unavoidable capacity constraint, or is it partly manufactured/maintained by ISPs to justify a paid-priority revenue stream?',
    'Independent technical audit of last-mile capacity utilization and ISP capital expenditure patterns compared against prioritization revenue and pricing of non-prioritized tiers over the same period.',
    'If congestion is substantially manufactured or under-invested-in deliberately, the coordination function underlying the tangled_rope classification is largely cover and the constraint moves toward snare; if congestion is a genuine, unavoidable physical constraint, the coordination function is real and the tangled_rope classification is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_congestion_vs_manufactured_scarcity, empirical, 'Whether the network-management justification tracks real scarcity or manufactured scarcity.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the prioritization_reading''s premise conflict with the neutrality_reading''s premise — is it a disagreement about what TCP/IP''s original design commits to, or a disagreement about what current network economics require regardless of original design intent?',
    'Historical analysis of RFC design documents and IETF working-group records versus contemporary economic analysis of last-mile investment incentives; if the disagreement is purely about original intent, it is a historical/interpretive question; if it is about present economic necessity, it is a policy question independent of protocol history.',
    'If the disagreement is interpretive/historical, the two readings are genuinely incompatible accounts of the same fixed text (supporting a forecloses relation); if the disagreement is about present-day policy needs layered on top of an ambiguous original design, the readings can coexist as competing policy positions built on the same underspecified substrate (supporting coexists_with, as declared).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the prioritization/neutrality split is a historical-interpretive dispute or a live policy dispute over a genuinely ambiguous kernel.').

omega_variable(
    beneficiary_capture_of_regulatory_definition,
    'Do the ''reasonable network management'' exemptions that regulators recognize track independently defined technical necessity, or have they been substantially shaped by the ISPs whose prioritization practices the exemptions legitimate?',
    'Comparative analysis of regulatory rulemaking records — who submitted technical definitions of ''reasonable network management,'' and how closely final regulatory language tracks ISP-submitted language versus independent engineering-body submissions.',
    'If regulatory definitions were substantially shaped by ISP submissions, the coordination function claimed for this reading is partly self-certified by the same party that profits from it, strengthening the case for higher effective extraction than the base metrics currently capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_regulatory_definition, empirical, 'Whether the regulatory standard legitimating prioritization was captured by the beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__prioritization_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__prioritization_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.31).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.1).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the tcp_ip_interpretation kernel. neutrality_reading claims the protocol's end-to-end design obligates non-discrimination (expected low ε, rope-leaning); prioritization_reading (this file) claims differentiated service quality is legitimate network management layered with commercial extraction (moderate-high ε, tangled_rope); zero_rating_reading addresses a structurally distinct exemption mechanism (sponsored-content cost exemption) with its own beneficiary/victim structure. All three share the same underlying technical substrate (TCP/IP) but diverge sharply in claimed normative content, beneficiary structure, and measured extraction — per the ε-invariance principle, they are authored as separate constraints rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
