% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: TCP/IP Neutrality Reading: End-to-End Non-Discrimination Mandate
 *   domain: technology governance / internet policy / telecommunications
 *
 * SUMMARY:
 *   The TCP/IP protocol suite has been interpreted since its
 *   commercialization as embodying an end-to-end design philosophy: the
 *   network core should remain simple while intelligence resides at the
 *   edges. The neutrality reading treats this architectural property as a
 *   binding normative constraint that prohibits broadband providers from
 *   content or application-based discrimination. Under this reading, TCP/IP
 *   does not merely describe packet routing but mandates a non-discriminatory
 *   transmission layer protecting edge innovation by preventing access
 *   providers from monetizing prioritization, blocking, or zero-rating. The
 *   constraint coordinates global application interoperability while
 *   asymmetrically extracting opportunity costs from network operators barred
 *   from differentiated-service revenue models.
 *
 * KEY AGENTS:
 *   - internet_architecture_community: agenda_setter (institutional/constrained) â maintains the end-to-end interpretation through standards and research
 *   - telecom_regulators: agenda_setter (institutional/mobile) â codifies and enforces neutrality mandates
 *   - broadband_isps: primary payer (institutional/constrained) â bears opportunity cost of foregone discrimination and prioritization revenue
 *   - edge_service_providers: beneficiary (organized/constrained) â innovates without carrier negotiation
 *   - end_users: beneficiary (organized/constrained) â accesses neutral transmission
 *   - differentiated_services_advocates: excluded (powerful/trapped) â structurally marginalized by the interpretive frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.62).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP Neutrality Reading: End-to-End Non-Discrimination Mandate").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology governance / internet policy / telecommunications").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'c8030527-9fdb-4e5e-a60b-082d496a3aa0').
narrative_ontology:cs_kernel_codification('c8030527-9fdb-4e5e-a60b-082d496a3aa0', formalized).
narrative_ontology:cs_authority_grounding('c8030527-9fdb-4e5e-a60b-082d496a3aa0', expertise).
narrative_ontology:cs_interpretation_layer_present('c8030527-9fdb-4e5e-a60b-082d496a3aa0').
narrative_ontology:cs_reading_relation('c8030527-9fdb-4e5e-a60b-082d496a3aa0', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8030527-9fdb-4e5e-a60b-082d496a3aa0', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('c8030527-9fdb-4e5e-a60b-082d496a3aa0', foundational, end_to_end_non_discrimination).
narrative_ontology:cs_axiom_status(end_to_end_non_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('c8030527-9fdb-4e5e-a60b-082d496a3aa0', end_to_end_non_discrimination, empirically_contingent).
narrative_ontology:cs_axiom('c8030527-9fdb-4e5e-a60b-082d496a3aa0', foundational, prohibition_on_paid_prioritization).
narrative_ontology:cs_axiom_status(prohibition_on_paid_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('c8030527-9fdb-4e5e-a60b-082d496a3aa0', prohibition_on_paid_prioritization, conventional).
narrative_ontology:cs_reference_frame('c8030527-9fdb-4e5e-a60b-082d496a3aa0', end_to_end_internet_architecture).
narrative_ontology:cs_drift_state('c8030527-9fdb-4e5e-a60b-082d496a3aa0', contemporary_broadband_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8030527-9fdb-4e5e-a60b-082d496a3aa0', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, broadband_isps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and articulates the end-to-end design philosophy embedded in TCP/IP. They publish technical standards, RFCs, and peer-reviewed analyses asserting that intelligence properly belongs at the network edge and the core must remain simple and non-discriminatory. Their professional authority and identity are fused to this architectural reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_architecture_community, agenda_setter,
    institutional, generational, constrained, global).

% Codify the neutrality reading into enforceable open-internet rules, translating the architectural principle into legal prohibitions on blocking, throttling, and paid prioritization. They inspect network-management practices and impose sanctions for deviation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Operate physical access and backbone infrastructure under norms that prohibit charging content providers for priority delivery or favoring specific applications. They bear the opportunity cost of foregone prioritization revenue and must expand capacity to accommodate growth rather than monetizing traffic shaping.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, broadband_isps, payer,
    institutional, biographical, constrained, national).

% Develop and deliver applications and content over the internet without negotiating individualized carriage agreements with every access provider. They rely on a uniform transmission layer to lower barriers to entry and prevent ISPs from taxing their revenue streams or blocking competing services.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_service_providers, beneficiary,
    organized, biographical, constrained, global).

% Access the full range of internet content and applications without ISP interference, throttling, or preferential treatment of favored services. Their practical choices are bounded by local broadband markets, but within those markets the constraint ensures they receive neutral carriage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, end_users, beneficiary,
    organized, biographical, constrained, global).

% ISPs and network-equipment vendors advocating for quality-of-service differentiation, paid prioritization, and zero-rating business models. They are structurally excluded from the neutrality reading's interpretive framework, which treats their preferred arrangements as violations of the architecture rather than legitimate network management.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, differentiated_services_advocates, excluded,
    powerful, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global application interoperability by ensuring that any edge device can reach any other over a simple, unified transmission layer without negotiating carriage terms with every intermediate network operator.
% TRANSFER_FUNCTION: Transfers control over application reach and transmission quality from network operators to edge innovators and end users; transfers potential revenue from prioritized traffic arrangements away from broadband providers toward the unpriced public benefit of open innovation.
% ABSENT_VOICES: Differentiated-services advocates and network-equipment vendors who would commercialize quality-of-service tiers are excluded from the normative frame; their arguments are treated as network-management exceptions at best and violations of architecture at worst.
% DISAPPEARANCE_RATIONALE: If the neutrality reading vanished overnight, broadband providers would rapidly implement paid prioritization, zero-rating, and application-specific throttling; edge startups would face carriage-negotiation barriers, and the internet's innovation ecology would reorganize around gatekeeper access providers.
% FOUNDING_PROBLEM: The fragmentation of early networks demonstrated that smart networks throttle innovation by requiring application developers to negotiate with every carrier; TCP/IP was designed to solve this by placing intelligence at the edge and keeping the core simple.
% FOUNDING_PROBLEM_CORROBORATION: The internet architecture community attests the problem from inside the design tradition. Broadband providers and some telecommunications economists contest it, arguing that bandwidth scarcity and quality-of-service requirements have replaced fragmentation as the central challenge. Competition authorities and edge providers offer mixed corroboration depending on jurisdiction and market structure.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   The neutrality reading is structurally a tangled rope because it coordinates a genuine collective-action problemâglobal application interoperability without bilateral carrier negotiationâwhile asymmetrically extracting from broadband operators who lose a major class of revenue models. Extractiveness (0.58) is moderate-to-high because the constraint systematically forecloses ISP prioritization and tiering. Suppression (0.62) is substantial because persistence depends on actively delegitimizing differentiated-service arrangements through regulation and technical norm-setting. Theater is low (0.25): the coordination function is genuinely operational, though enforcement has accumulated bureaucratic performative layers. Accessibility collapse (0.55) is moderateâprioritized network alternatives exist technically but are normatively marginalized once the neutrality frame is accepted. Resistance (0.68) is high because institutional ISPs consistently litigate, lobby, and engineer workarounds.
 *
 * PERSPECTIVAL GAP:
 *   From the architecture community's seat, the constraint is a natural entailment of protocol designâa mountain of engineering logic. From the ISP seat, it is an externally imposed ideological and regulatory structure that extracts value from infrastructure investment without compensating carriage. The engine computes this divergence from the structural data: identical constraint, opposite directionalities, producing divergent per-seat classifications. The neutrality reading's recurrent claim to technical naturality is precisely what the omega variables interrogate.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge service providers and end users are structural beneficiaries: they receive a uniform transmission layer that lowers entry barriers and prevents intermediary extraction (low d). Broadband ISPs are the structural payers: they lose the ability to monetize gatekeeper position and must absorb capacity costs without corresponding service-tier revenue (high d). The internet architecture community sits near the low-d end as agenda-setter but is better characterized by identity-fusion with the coordination function itself. Differentiated-services advocates are excluded entirely; their absence is the enforcement boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate to maintain non-discrimination has not fully atrophied; the founding problem of network fragmentation persists in the form of walled-garden re-emergence. However, the mandate's justification has shifted from technical necessity to normative policy preference and legal codification. This shift increases the theater component and raises the question of whether the constraint is accruing institutional inertia distinct from its original coordination function, though it has not yet degraded into pure piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_vs_normative_embodiment,
    'Does TCP/IP''s packet-switching architecture technically enforce non-discrimination, or is non-discrimination a normative choice layered onto protocols that are technically capable of discrimination?',
    'Engineering audit of router operating systems, QoS mechanisms, and differential-service field usage to determine whether the protocol stack is technically capable of content-based discrimination.',
    'If purely normative, the constraint cannot claim mountain status and is confirmed as tangled rope; if technically self-enforcing, the constraint would trend toward rope or mountain with negligible extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_vs_normative_embodiment, empirical, 'Whether non-discrimination is a built-in technical property or a normative overlay').

omega_variable(
    founding_problem_continuity,
    'Is network fragmentation still the central threat to internet value, or has the problem shifted to bandwidth scarcity, security, and quality-of-service coordination that the end-to-end model handles poorly?',
    'Comparative performance analysis of neutral versus differentiated networks in high-bandwidth, low-latency applications; historical trend analysis of walled-garden market share.',
    'If the founding problem is dead or transformed, the constraint''s persistence may indicate mandatrophy or extraction rather than live coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_continuity, empirical, 'Whether the original problem the constraint was built to solve still exists').

omega_variable(
    kernel_reading_relation_uncertainty,
    'As the neutrality_reading of tcp_ip_interpretation, does our core premise that TCP/IP requires non-discrimination foreclose the prioritization_reading, or can both readings coexist within a single regulatory framework?',
    'Legal and institutional analysis of frameworks that contain both neutrality mandates and ''reasonable network management'' exceptions; logical analysis of whether a single party could simultaneously hold both readings.',
    'If foreclosed, the kernel generates mutually exclusive attractors and winner-take-all policy dynamics; if coexistent, the kernel produces persistent oscillation rather than structural resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_uncertainty, conceptual, 'Whether the sibling readings are logically mutually exclusive or practically coexistent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t6, tcp_ip_interpretation__neutrality_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__neutrality_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__neutrality_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__neutrality_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(tcp__tr_t30, tcp_ip_interpretation__neutrality_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tcp__be_t6, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(tcp__be_t30, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp__su_t6, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(tcp__su_t30, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, global_infrastructure).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% Decomposition of tcp_ip_interpretation kernel into three structurally distinct readings per the Îµ-invariance principle. The neutrality reading influences the policy space within which prioritization and zero-rating readings must operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
