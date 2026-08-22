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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Non-Discrimination Principle (Neutrality Reading)
 *   domain: technology_governance/internet_policy
 *
 * SUMMARY:
 *   This constraint instantiates the neutrality reading of the TCP/IP
 *   interpretation kernel: ISPs are required to treat all traffic equally,
 *   prohibiting content-based or application-based discrimination. This
 *   reading claims the end-to-end principle is an architectural commitment
 *   embedded in TCP/IP's design, and that ISP discrimination violates it. The
 *   kernel itself is contested: a prioritization reading holds that
 *   differentiated service quality is compatible with TCP/IP; a zero-rating
 *   reading holds that sponsored-content exemptions are permissible. This
 *   story describes ONLY the neutrality reading—the claim that
 *   non-discrimination is structurally required. The constraint operates as
 *   tangled rope: it coordinates innovation incentives at the network edge
 *   (genuine coordination function) while extracting from ISPs the revenue
 *   opportunity of paid prioritization (asymmetric extraction). Enforcement
 *   is active: regulators must constantly adjudicate discrimination disputes
 *   and levy penalties.
 *
 * KEY AGENTS:
 *   - Edge innovators: StartUp developers and platforms that benefit from open, non-discriminatory access to users. Power = organized; exit = mobile (can lobby, can use CDNs to route around degradation).
 *   - ISPs / network operators: Infrastructure providers that are constrained from revenue optimization through traffic shaping or paid prioritization. Power = institutional; exit = trapped (sunk infrastructure investment; cannot disinvest without service failure).
 *   - Content publishers: Large media and streaming companies that benefit from traffic neutrality. Power = powerful; exit = arbitrage (can invest in private networks, carrier relations, or regulatory capture).
 *   - End users: Individual subscribers who theoretically benefit from equal access to all content. Power = powerless; exit = constrained (limited ISP choice, no visibility into discrimination).
 *   - Telecom regulators: Government agencies (FCC, EC, OFCOM) that interpret and enforce non-discrimination rules. Power = institutional; role = agenda_setter (set the boundary of what counts as discrimination).
 *   - Specialized service advocates (EXCLUDED): ISPs and vendors arguing that some traffic requires QoS differentiation. Structurally excluded from the neutrality reading's authority frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.62).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Non-Discrimination Principle (Neutrality Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '651a526f-3bdc-4f63-8b9f-36994ed2fbcb').
narrative_ontology:cs_kernel_codification('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', fixed_text).
narrative_ontology:cs_authority_grounding('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', lineage).
narrative_ontology:cs_interpretation_layer_present('651a526f-3bdc-4f63-8b9f-36994ed2fbcb').
narrative_ontology:cs_reading_relation('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', foundational, end_to_end_principle_is_constitutive).
narrative_ontology:cs_axiom_status(end_to_end_principle_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', end_to_end_principle_is_constitutive, deontological).
narrative_ontology:cs_axiom('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', foundational, isp_discrimination_violates_original_design).
narrative_ontology:cs_axiom_status(isp_discrimination_violates_original_design, holdable).
narrative_ontology:cs_axiom_grounding('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', isp_discrimination_violates_original_design, empirically_contingent).
narrative_ontology:cs_reference_frame('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', original_end_to_end_architectural_commitment).
narrative_ontology:cs_drift_state('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', contemporary_commercial_internet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('651a526f-3bdc-4f63-8b9f-36994ed2fbcb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_publishers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, network_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers and startups building applications and services at the network edge. Under the neutrality reading, they are protected from ISP discrimination: their traffic receives equal treatment regardless of their business model, funding, or competitive threat to ISPs. They benefit from the constraint's enforcement because it guarantees their innovation can reach users without ISP gatekeeping. Exit options are relatively high: if one ISP becomes hostile, they can advocate for regulatory intervention or work with content delivery networks (CDNs) to route around discrimination.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    organized, biographical, mobile, global).

% Large media companies, streaming services, and publishing platforms. They benefit from non-discrimination because their traffic competes on a neutral basis with competitors and cannot be degraded by ISPs seeking leverage. Large publishers have exit options: they can invest in private networks, pay for premium interconnection, or lobby regulators. The neutrality constraint prevents ISPs from capturing surplus through traffic prioritization.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Individuals purchasing internet access. Under neutrality, they theoretically benefit from equal treatment of all content and applications: their ISP cannot slow traffic from competitors of the ISP's own services, and cannot extract side payments from application providers to speed their content. In practice, users have limited exit options (often one or two local ISP choices) and limited visibility into whether discrimination occurs, making their benefit often theoretical rather than experienced.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, end_users, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, end_users, payer).

% Internet Service Providers and network operators that carry traffic over last-mile and backbone infrastructure. The neutrality constraint requires them to treat all traffic identically, regardless of source, content, or commercial relationship. They bear costs in constrained revenue optimization: they cannot engage in paid prioritization, cannot degrade competitors' traffic, cannot offer specialized services leveraging their network control. Their exit options are limited: they cannot easily leave the infrastructure market without abandoning their infrastructure investments, and regulatory pressure makes exit difficult.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, isps, payer,
    institutional, generational, constrained, national).

% Backbone and peering operators managing the interconnection layer. Like ISPs, they are constrained from discriminating based on content or application type. Peering disputes historically involved leverage over traffic routing; neutrality rules force settlement at non-discriminatory terms. Their exit is trapped: they are infrastructure operators whose investments are sunk, and they cannot disinvest without causing service degradation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, network_operators, payer,
    institutional, generational, trapped, regional).

% Government bodies and regulatory agencies (FCC, EC, OFCOM, etc.) that interpret and enforce net neutrality rules. They set the boundary of what counts as discrimination, adjudicate disputes, and levy penalties. Under the neutrality reading, they enforce the end-to-end principle by prohibiting ISPs from performing content-based traffic shaping. Their enforcement creates the constraint's persistence.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% ISPs and network equipment vendors advocating for carve-outs for specialized services (IPTV, VoIP, medical telemetry, autonomous vehicle communication). These actors are largely excluded from the neutrality reading's legitimacy frame: their argument that some traffic requires QoS guarantees that differ from best-effort routing is treated as a cover story for discrimination. They would contest the non-discrimination principle if seated at the rule-setting table.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, specialized_service_proponents, excluded,
    institutional, biographical, constrained, global).

% Technical standards bodies (IETF, W3C) and internet governance institutions (ICANN, various RIRs) that document and preserve the TCP/IP protocol standards and end-to-end principle. They operate as the epistemic authority: they attest that the end-to-end principle is architecturally fundamental and that discrimination violates the original design intent. Their authority grounds the reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, public_internet_foundation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified, non-discriminatory transport layer that permits arbitrary applications to be deployed at the network edge without ISP gatekeeping. This solves the coordination problem of how to allow innovation at the edge while protecting ISPs' infrastructure investments: the rule states that ISPs receive their return from access fees and service availability, not from controlling which applications reach users.
% TRANSFER_FUNCTION: Transfers authority over traffic prioritization and application selection FROM ISPs (who control the physical network) TO end users and application developers (who control what runs at the edge). It also transfers the revenue constraint: ISPs cannot collect payments from application providers for favorable treatment, and cannot degrade competitors' traffic to extract leverage.
% ABSENT_VOICES: Network operators advocating for specialized service carve-outs, ISPs pursuing traffic-based revenue optimization, and equipment vendors selling differentiated QoS infrastructure are largely excluded from the neutrality reading's legitimacy space. They are not at the table where 'end-to-end principle' is authoritatively interpreted; regulatory proceedings hear them as party interests rather than co-authorities. They would argue the constraint is technically infeasible and economically harmful if seated as equal voices.
% DISAPPEARANCE_RATIONALE: If non-discrimination requirements were removed, ISPs would immediately implement paid prioritization, degrade competitors' traffic, and extract payments from application providers to avoid degradation. The application layer would reorganize around which services could afford ISP fees. Edge innovation would concentrate among well-funded actors; small startups would face an additional gatekeeping layer at the network level. The internet's current architecture—innovation distributed across thousands of edge players—depends on ISPs' inability to discriminate.
% FOUNDING_PROBLEM: Early internet architecture embodied the end-to-end principle: intelligence at the network edge, simplicity in the middle. As networks became commercially operated, operators sought to recapture rents by implementing traffic shaping, paid prioritization, and selective blocking. The founding problem the neutrality reading addresses is: how to preserve the original architectural principle (open edge) against commercial pressure to close the network to only ISP-approved applications.
% FOUNDING_PROBLEM_CORROBORATION: Internet architects and standards bodies (IETF, W3C) attest the end-to-end principle is a fundamental design commitment and its violation poses architectural risk. ISPs and operators attest the founding problem is solved: modern QoS management and traffic engineering are necessary for network stability, and the 'principle' is an outdated doctrine incompatible with infrastructure economics. Regulatory testimony, academic network research, and economics literature from outside the ISP lobby support both readings; the contest is live.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.58): the neutrality reading constrains ISPs from a revenue source (paid prioritization) that would otherwise be available, and transfers authority over application selection to the edge. However, extractiveness is not extreme because ISPs still collect access fees and can optimize network operations through other means (capacity planning, congestion management outside the discrimination boundary). Suppression is similarly moderate (0.62): enforcement requires active regulatory monitoring and penalty authority, but ISPs cannot simply ignore the constraint—they must at least publicly comply. Theater is moderate (0.41): compliance theater exists (ISPs may implement technical workarounds like 'zero-rating' or specialized service carve-outs that technically comply while violating the spirit), but the underlying constraint is functionally enforced. Accessibility_collapse (0.52) reflects that alternatives to the current constraint ARE accessible to ISPs (they can lobby for deregulation, they can argue the burden is unsustainable), so alternatives have not completely collapsed—but collapse is partial because the architectural and regulatory commitment is substantive. Resistance is high (0.68): ISPs mount active legal and political resistance; they lobby regulators, fund think tanks, and seek carve-out exemptions. The measurement series shows extractiveness rising from 0.42 to 0.58 over the interval, indicating regulatory enforcement tightening and ISP revenue loss accumulating as regulators clarify what counts as discrimination. Theater ratio rises modestly (0.28 to 0.41), suggesting increasing use of compliance workarounds as ISPs adapt to enforcement pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP seat, the constraint appears as an unfunded mandate: regulators require non-discrimination but do not fund the infrastructure to deliver equivalent QoS to all traffic. The ISP perspective would compute this constraint as a snare (pure extraction, coercive, no genuine coordination function for the ISP itself). From the edge-innovator seat, it appears as genuine coordination: the constraint solves the problem of how to innovate without ISP gatekeeping. The engine computes this divergence from the structural data (beneficiary/victim declarations + power + exit options); the authored claim (tangled_rope) does not reconcile the seats. The gap itself is the measurement the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge innovators and content publishers sit at the beneficiary end of directionality (d near 0.0): they benefit without running the constraint and have mobile exit options. ISPs and network operators sit at the target end (d near 1.0): they bear costs (constrained revenue), have trapped exit options (infrastructure sunk), and cannot easily avoid the constraint. End users are near-symmetric (d ≈ 0.5): theoretical benefit from neutrality, diffuse cost if it reduces ISP investment in network capacity. Regulators are analytical observers. The neutrality reading creates structural asymmetry because it protects the edge while constraining the middle (ISPs)—this is the intentional redistribution the reading embodies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested': the neutrality reading claims the problem (ISP gatekeeping threatens edge innovation) is live and persistent; the prioritization reading claims it is substantially solved and the constraint now represents overreach. The disappearance_verdict is 'world_rearranges', indicating the arrangement is causally load-bearing, not merely epistemic. This alignment (contested status + world_rearranges) does NOT trigger mandatrophy—the constraint is still actively contested and enforced. Mandatrophy would arise if status were 'dead' (the founding problem no longer exists) while disappearance_verdict remained 'world_rearranges' (the world still depends on it). That mismatch would signal the constraint persists as theater, not function. Here, the contest keeps both status and verdict aligned: the constraint is believed to be necessary and is actively defended, even if the necessity claim is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    end_to_end_principle_architectural_necessity,
    'Is the end-to-end principle a constitutive design commitment of TCP/IP, or a descriptive pattern that emerged for implementation convenience?',
    'Genealogical analysis of TCP/IP standards (RFCs 791, 793, etc.) and interviews with protocol designers; examination of whether the principle is formally required or incidentally satisfied by early implementations.',
    'If constitutive, non-discrimination is an architectural requirement and deviations (QoS, specialized services) are design violations. If descriptive, the principle is a default implementation choice compatible with discrimination where operationally justified. The classification moves from tangled_rope toward snare if the principle is only descriptive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_to_end_principle_architectural_necessity, conceptual, 'Whether the end-to-end principle is constitutive or descriptive in TCP/IP design.').

omega_variable(
    technical_necessity_of_discrimination,
    'Is differentiated service quality (QoS, traffic prioritization) technically necessary for stable network operation, or is it a feature ISPs implement for revenue optimization?',
    'Network engineering studies comparing best-effort vs. QoS-enabled networks under equivalent load; measurement of packet loss, latency variance, and application performance with and without discrimination.',
    'If technical necessity is high, some discrimination is coordination cost (extraction component of tangled_rope contracts); if low, discrimination is pure extraction (tangled_rope shifts toward snare). This feeds the Boltzmann floor calculation for enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_necessity_of_discrimination, empirical, 'Whether traffic differentiation is technically necessary or revenue-optimization motivated.').

omega_variable(
    regulatory_enforcement_sustainability,
    'Can non-discrimination rules be enforced indefinitely without eroding ISP investment incentives for infrastructure expansion?',
    'Time-series analysis of capital expenditure in network infrastructure before and after neutrality enforcement; economic modeling of return-on-investment thresholds for network operators under different regulatory regimes.',
    'If investment does not erode significantly, the constraint is sustainable as tangled_rope. If investment declines precipitously, the constraint creates a regulatory/economic feedback loop where ISPs underinvest, congestion increases, and pressure for exemptions grows—reclassifying the constraint toward piton (theater maintaining a dead foundation problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_sustainability, empirical, 'Whether neutrality enforcement is compatible with sustained ISP investment in infrastructure.').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Does the neutrality reading''s assertion of end-to-end principle necessity logically foreclose the prioritization and zero-rating readings, or do all three represent live policy choices?',
    'Formal analysis of the three readings'' foundational axioms: if neutrality''s axioms directly contradict prioritization''s or zero-rating''s core premises such that no single framework could hold both, foreclosure applies; otherwise, readings coexist.',
    'If foreclosure is present, the sibling readings are in logical conflict with this one—the engine''s foreclosure detection would mark them as contradictory commitments. If coexistence is the structure, the three readings represent genuinely different policy choices held by different regulatory jurisdictions, and both this story and the sibling stories remain valid constraints in parallel. This omega directs the relation classification in cs_structure.reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, conceptual, 'Whether the neutrality, prioritization, and zero-rating readings logically foreclose one another or coexist as live policy choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__neutrality_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__neutrality_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__neutrality_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__neutrality_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested TCP/IP interpretation kernel. The network links define the constraint family: neutrality_reading affects both prioritization_reading and zero_rating_reading because the neutrality interpretation establishes what counts as 'discrimination' in the architectural frame. Changes to neutrality enforcement (tightening or loosening) directly alter the operating environment for the other readings: if neutrality rules relax, prioritization becomes more feasible; if they tighten, prioritization becomes the target of enforcement. The three stories share the same underlying kernel (TCP/IP as a design commitment) but instantiate different readings of what that commitment requires. All three are structurally valid as constraint stories; they represent different positions in a live regulatory contest, not alternative theories of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
