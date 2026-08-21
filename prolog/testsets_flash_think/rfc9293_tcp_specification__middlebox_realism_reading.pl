% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Specification as Subordinate to Middlebox Reality
 *   domain: network_protocol_engineering
 *
 * SUMMARY:
 *   RFC 9293, the current specification for TCP, describes an ideal
 *   end-to-end protocol. However, this constraint,
 *   'middlebox_realism_reading', asserts that the actual behavior of TCP in
 *   the wild is fundamentally shaped by the pervasive deployment of
 *   middleboxes (firewalls, NATs, DPI devices) that inspect, modify, or block
 *   traffic. The formal specification becomes an aspiration rather than an
 *   enforceable standard, with real-world authority residing with the
 *   deployed network infrastructure. This reading frames the constraint as a
 *   Tangled Rope, acknowledging TCP's underlying coordination function while
 *   highlighting the asymmetric extraction of control by middlebox operators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.8).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.75).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Specification as Subordinate to Middlebox Reality").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '5fdf589f-1ecc-4fa4-b948-48709308caf7').
narrative_ontology:cs_kernel_codification('5fdf589f-1ecc-4fa4-b948-48709308caf7', fixed_text).
narrative_ontology:cs_authority_grounding('5fdf589f-1ecc-4fa4-b948-48709308caf7', practice).
narrative_ontology:cs_interpretation_layer_present('5fdf589f-1ecc-4fa4-b948-48709308caf7').
narrative_ontology:cs_reading_relation('5fdf589f-1ecc-4fa4-b948-48709308caf7', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('5fdf589f-1ecc-4fa4-b948-48709308caf7', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('5fdf589f-1ecc-4fa4-b948-48709308caf7', foundational, deployed_network_dictates_protocol_reality).
narrative_ontology:cs_axiom_status(deployed_network_dictates_protocol_reality, holdable).
narrative_ontology:cs_axiom_grounding('5fdf589f-1ecc-4fa4-b948-48709308caf7', deployed_network_dictates_protocol_reality, empirically_contingent).
narrative_ontology:cs_axiom('5fdf589f-1ecc-4fa4-b948-48709308caf7', secondary, end_to_end_principle_is_subordinate_to_network_control).
narrative_ontology:cs_axiom_status(end_to_end_principle_is_subordinate_to_network_control, holdable).
narrative_ontology:cs_axiom_grounding('5fdf589f-1ecc-4fa4-b948-48709308caf7', end_to_end_principle_is_subordinate_to_network_control, conventional).
narrative_ontology:cs_reference_frame('5fdf589f-1ecc-4fa4-b948-48709308caf7', ideal_end_to_end_tcp).
narrative_ontology:cs_drift_state('5fdf589f-1ecc-4fa4-b948-48709308caf7', contemporary_internet_deployment, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5fdf589f-1ecc-4fa4-b948-48709308caf7', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, internet_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the client and server machines running TCP. Their traffic is often inspected, modified, or blocked by middleboxes, leading to unexpected behavior, performance degradation, or connectivity issues, despite adhering to RFC 9293.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints, payer,
    powerless, immediate, trapped, global).

% They design applications that rely on TCP's specified behavior. They must contend with the reality of middlebox interference, often implementing workarounds or designing new protocols (like QUIC) to bypass or mitigate these effects, increasing development complexity and reducing interoperability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Experience the consequences of middlebox interference as degraded application performance, blocked services, or unexpected network behavior. They have no direct control over the network path or middlebox operations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_users, payer,
    powerless, immediate, trapped, global).

% These include ISPs, enterprise network administrators, and state-level actors who deploy and manage middleboxes (firewalls, NATs, DPI devices, proxies). They actively modify TCP traffic to enforce security policies, manage network resources, or implement surveillance, effectively dictating the 'real' TCP behavior on their networks.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Responsible for publishing and maintaining RFCs, including RFC 9293. From this reading, their specifications represent an ideal that is often subverted by deployed network reality, leading to a gap between normative authority and actual practice. They can update RFCs but cannot directly control deployed infrastructure.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standard_bodies, observer,
    institutional, generational, analytical, global).

% They study the behavior of TCP in real-world networks, documenting middlebox interference and its impact on end-to-end connectivity and protocol evolution. They provide the empirical evidence for the 'middlebox realism' perspective.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TCP's fundamental coordination function is to provide a reliable, ordered, and error-checked byte stream between endpoints. Middleboxes, in some contexts, also coordinate network-wide security policies, traffic management, or regulatory compliance.
% TRANSFER_FUNCTION: This constraint transfers effective control over TCP traffic behavior from the endpoints (as specified in RFC 9293) to the middlebox operators. It transfers the burden of adapting to non-standard behavior to application developers and internet users.
% ABSENT_VOICES: Advocates for strict adherence to the end-to-end principle and transparent network paths are often marginalized in discussions dominated by operational realities and security concerns. Many application developers and internet users, while affected, lack a unified voice or platform to challenge the pervasive influence of middleboxes.
% DISAPPEARANCE_RATIONALE: If middleboxes ceased to modify TCP traffic, the internet's operational characteristics would fundamentally change. While end-to-end connectivity might improve, existing security, policy enforcement, and traffic management functions (some legitimate, some extractive) would vanish, requiring a complete reorganization of network security and operations.
% FOUNDING_PROBLEM: The original TCP specification aimed to provide robust, end-to-end communication. The 'problem' this constraint (middlebox realism) addresses, from the perspective of middlebox operators, is the perceived need for network-level control, security, and policy enforcement that endpoints alone cannot or do not provide.
% FOUNDING_PROBLEM_CORROBORATION: Middlebox operators and some network administrators attest that the need for network-level control (security, policy, traffic management) is still live and critical. Network researchers and application developers, however, provide extensive evidence that this 'solution' often subverts the original protocol design and creates new problems for innovation and interoperability.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because middleboxes actively interfere with endpoint autonomy, often for purposes beyond simple network management, such as surveillance or policy enforcement that benefits the operator. Suppression is also high (0.75) as endpoints and users have very limited options to bypass or resist middlebox interference on their network paths. The theater ratio is moderate (0.4) because RFC 9293 is still formally maintained and cited, but its practical authority over deployed network behavior is significantly diminished, making its 'enforcement' partly performative. The increasing trends in all metrics reflect the growing sophistication and pervasiveness of middlebox deployments over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of middlebox operators, their actions are often framed as necessary for network security, policy enforcement, or resource management (a coordination function). From the perspective of TCP endpoints, application developers, and internet users, these same actions represent an extraction of control, a violation of the end-to-end principle, and a source of operational friction. The engine's classification will capture this divergence by computing different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators are the primary beneficiaries and agenda-setters, gaining control over network traffic and enforcing their policies (low directionality). TCP endpoints, application developers, and internet users are the targets, bearing the costs of adaptation, degraded performance, and reduced autonomy (high directionality). IETF standard bodies and network researchers act as observers, documenting the gap between specification and reality without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_intent_ambiguity,
    'To what extent do middlebox operations serve legitimate network management/security functions versus extractive surveillance/control functions?',
    'Independent audits of middlebox configurations and traffic logs, coupled with analysis of their impact on end-to-end connectivity and user privacy, particularly in jurisdictions with strong regulatory oversight.',
    'If primarily extractive, the constraint''s effective extractiveness would be higher, pushing it closer to a Snare. If primarily legitimate coordination, it would reinforce the Tangled Rope classification, acknowledging a genuine, albeit asymmetric, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_intent_ambiguity, empirical, 'Distinguishing legitimate coordination from pure extraction in middlebox operations.').

omega_variable(
    protocol_design_resistance_efficacy,
    'How effectively can new protocol designs (e.g., QUIC) bypass or resist middlebox interference, thereby reasserting endpoint control?',
    'Empirical studies of new protocol deployment, measuring their ability to traverse middleboxes transparently and maintain end-to-end semantics, compared to TCP''s historical experience.',
    'If new protocols prove highly resistant, it would suggest a potential shift in power back to endpoints, reducing the long-term suppression and extractiveness of this constraint. If they are quickly co-opted or blocked, it would reinforce the current high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_design_resistance_efficacy, empirical, 'The long-term impact of protocol innovation on middlebox dominance.').

omega_variable(
    specification_authority_reassertion,
    'Can the IETF''s specification authority be reasserted over deployed network behavior, or is it permanently subordinate to operational practice?',
    'Analysis of policy shifts, regulatory interventions, or industry-wide agreements that prioritize strict protocol adherence over middlebox modifications, and their measurable impact on network behavior.',
    'If authority can be reasserted, the ''theater_ratio'' would decrease, and the ''claimed_type'' might shift towards a Rope or even a Mountain (if end-to-end principles become universally enforced). If not, the current classification as a Tangled Rope (or even Snare) is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specification_authority_reassertion, conceptual, 'The potential for normative authority to regain primacy over de facto network practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t2000, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(rfc9_tr_t2005, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(rfc9_tr_t2015, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(rfc9_tr_t2020, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t2000, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(rfc9_be_t2005, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(rfc9_be_t2015, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(rfc9_be_t2020, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t2000, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(rfc9_su_t2005, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(rfc9_su_t2010, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(rfc9_su_t2015, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(rfc9_su_t2020, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
