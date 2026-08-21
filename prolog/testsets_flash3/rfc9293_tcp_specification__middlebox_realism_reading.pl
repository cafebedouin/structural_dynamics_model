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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification (Middlebox Realism Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   This constraint describes the de facto reality of TCP operation, where
 *   the ideal endpoint behavior specified in RFC 9293 is consistently
 *   overridden or modified by deployed middleboxes. This reading frames the
 *   RFC as an aspirational document whose authority is subordinate to the
 *   actual behavior of the network, which is shaped by middlebox operators
 *   for various purposes including policy enforcement, traffic management,
 *   and surveillance. The constraint is a Snare because it extracts control
 *   and predictability from endpoints and developers, benefiting middlebox
 *   operators and state agencies, and is maintained through active, often
 *   opaque, enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.85).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.92).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, snare).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, 'f708fff1-4f70-4d1b-aa97-cc1992ea361a').
narrative_ontology:cs_kernel_codification('f708fff1-4f70-4d1b-aa97-cc1992ea361a', formalized).
narrative_ontology:cs_authority_grounding('f708fff1-4f70-4d1b-aa97-cc1992ea361a', extraction).
narrative_ontology:cs_interpretation_layer_present('f708fff1-4f70-4d1b-aa97-cc1992ea361a').
narrative_ontology:cs_reading_relation('f708fff1-4f70-4d1b-aa97-cc1992ea361a', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('f708fff1-4f70-4d1b-aa97-cc1992ea361a', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('f708fff1-4f70-4d1b-aa97-cc1992ea361a', foundational, network_behavior_is_de_facto_standard).
narrative_ontology:cs_axiom_status(network_behavior_is_de_facto_standard, holdable).
narrative_ontology:cs_axiom_grounding('f708fff1-4f70-4d1b-aa97-cc1992ea361a', network_behavior_is_de_facto_standard, empirically_contingent).
narrative_ontology:cs_axiom('f708fff1-4f70-4d1b-aa97-cc1992ea361a', foundational, middlebox_policy_trumps_endpoint_autonomy).
narrative_ontology:cs_axiom_status(middlebox_policy_trumps_endpoint_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f708fff1-4f70-4d1b-aa97-cc1992ea361a', middlebox_policy_trumps_endpoint_autonomy, conventional).
narrative_ontology:cs_reference_frame('f708fff1-4f70-4d1b-aa97-cc1992ea361a', middlebox_dominated_internet).
narrative_ontology:cs_drift_state('f708fff1-4f70-4d1b-aa97-cc1992ea361a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f708fff1-4f70-4d1b-aa97-cc1992ea361a', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy and manage network devices (firewalls, NATs, proxies, DPI boxes) that inspect and modify TCP traffic, often violating RFC 9293's endpoint-to-endpoint semantics. They benefit from the ability to enforce policy, manage traffic, and extract data, often without endpoint consent or knowledge. Their actions shape the de facto TCP standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Leverage middlebox capabilities for monitoring, censorship, and data interception. They benefit from the opacity and lack of endpoint control over network paths, which allows them to operate without direct interaction with endpoints or adherence to published protocol specifications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Experience degraded performance, broken applications, and compromised privacy due to middlebox interference. They have no control over the network path their traffic takes and are largely unaware of the modifications occurring. Their autonomy over their own network communication is extracted.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users, payer,
    powerless, immediate, trapped, global).

% Must design applications to be robust against unpredictable middlebox behavior, often resorting to workarounds that compromise efficiency or security. They bear the cost of debugging and maintaining compatibility with a constantly shifting 'real' TCP, rather than a stable, specified one.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Struggle to develop new protocols or optimize existing ones based on ideal specifications, as real-world deployment is dominated by middlebox constraints. Their work is often reduced to reverse-engineering and adapting to existing, often undocumented, network behaviors, rather than advancing the protocol itself.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, payer,
    analytical, biographical, analytical, global).

% Publish RFCs like 9293, intending them as authoritative specifications for interoperable internet protocols. However, their authority is undermined by the de facto power of middlebox deployments. They would advocate for strict adherence to standards but lack the enforcement mechanisms to counter widespread middlebox deviations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standard_bodies, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RFC 9293 specification attempts to coordinate endpoint behavior to ensure reliable, interoperable data transfer across diverse networks. However, in this reading, this function is largely aspirational, as middleboxes introduce uncoordinated, path-dependent modifications.
% TRANSFER_FUNCTION: The constraint transfers control over network behavior and data integrity from endpoints (users, developers) to middlebox operators and state agencies. It also transfers the burden of adapting to an unpredictable network from middlebox operators to application developers and users.
% ABSENT_VOICES: Endpoint users and application developers, whose autonomy and functionality are directly impacted, are largely absent from the decision-making processes that lead to middlebox deployment and configuration. IETF standard bodies, while present, lack the power to enforce their specifications against deployed middlebox realities.
% DISAPPEARANCE_RATIONALE: If the de facto middlebox-driven TCP behavior vanished overnight, and RFC 9293's ideal behavior became universally enforced, network performance would become more predictable, application development simpler, and endpoint autonomy restored. However, the policy enforcement and surveillance capabilities of middlebox operators and state agencies would collapse, leading to a significant reorganization of network control and security paradigms.
% FOUNDING_PROBLEM: The original TCP specification aimed to provide a robust, reliable, end-to-end byte stream over an unreliable packet network, enabling global interoperability and application development without network interference.
% FOUNDING_PROBLEM_CORROBORATION: Network researchers and application developers widely attest that the original problem of end-to-end reliability is now complicated by, and often subordinate to, the problem of middlebox interference. While the RFC still describes an ideal, the 'real' TCP is a product of deployed middleboxes, rendering the original problem's solution effectively superseded by a new, more complex reality. Independent analyses of network traffic and application failures corroborate this shift.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because middleboxes fundamentally alter the end-to-end contract of TCP, imposing costs on users and developers without their consent. Suppression is very high (0.92) because endpoints have virtually no means to bypass or resist middlebox interference; their traffic must traverse these devices. The theater ratio is high (0.60) because the formal specification (RFC 9293) continues to be published and referenced as if it were the primary authority, while the real-world behavior deviates significantly, making the specification itself a form of performative maintenance for an ideal that no longer holds.
 *
 * PERSPECTIVAL GAP:
 *   Middlebox operators perceive their actions as necessary for network management and security, viewing RFC 9293 as a flexible guideline. Endpoint users and developers, however, experience these actions as arbitrary interference and extraction of their network autonomy. Network researchers see a fundamental breakdown in the internet's architectural principles. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators and state surveillance agencies are clear beneficiaries (d near 0.0) as they gain control and data extraction capabilities. Endpoint users, application developers, and network researchers are victims (d near 1.0) as they bear the costs of unpredictability, broken functionality, and compromised privacy. IETF standard bodies are excluded, as their formal authority is bypassed by the de facto power of middlebox deployment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has atrophied from coordinating end-to-end communication to enabling middlebox control. The original coordination function of RFC 9293 is now largely a cover story for the extraction of control by middlebox operators. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting the coercive and extractive nature of the actual deployed network behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_authority_locus,
    'Does the ultimate authority for TCP behavior reside in formal specifications (RFCs) or in the deployed network infrastructure (middleboxes)?',
    'Empirical observation of network behavior vs. RFC compliance, and analysis of policy enforcement mechanisms. If RFC compliance is consistently overridden by middlebox actions, the authority lies with the latter.',
    'If authority is de facto with middleboxes, RFC 9293 is a Snare (as described). If RFCs regain enforcement power, it could shift towards a Rope or Tangled Rope, with clearer coordination functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_authority_locus, empirical, 'Ambiguity over where TCP''s ''true'' specification authority lies.').

omega_variable(
    middlebox_necessity_vs_opportunism,
    'To what extent are middlebox functions (e.g., NAT, firewalls) genuinely necessary for network operation and security, versus opportunistic leveraging of network position for control and extraction?',
    'Technical analysis of alternative architectures (e.g., end-to-end encryption, IPv6 deployment) that could achieve security/functionality without violating TCP semantics, combined with policy analysis of middlebox deployment motivations.',
    'If functions are largely opportunistic, the extractiveness and suppression metrics are accurate. If genuinely necessary, a portion of the extraction might be reclassified as a coordination cost, potentially shifting the constraint towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_opportunism, conceptual, 'Distinguishing necessary middlebox functions from opportunistic ones.').

omega_variable(
    endpoint_resistance_potential,
    'What is the true potential for endpoint users and application developers to resist or bypass middlebox interference through technical means (e.g., VPNs, E2E encryption, new protocols)?',
    'Deployment and adoption rates of resistance technologies, and analysis of middlebox countermeasures. If resistance is consistently suppressed, the current suppression metric is accurate. If effective bypasses become widespread, suppression would decrease.',
    'Increased endpoint resistance would lower the effective suppression and extractiveness, potentially shifting the constraint towards a Piton if middlebox operators cannot adapt, or a Tangled Rope if a new coordination emerges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endpoint_resistance_potential, empirical, 'The actual capacity of endpoints to resist middlebox control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, tcp_strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, tcp_optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_end_to_end_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the RFC 9293 TCP specification kernel, focusing on the de facto authority of middleboxes. It highlights how the deployed network reality overrides formal standards, influencing other readings of TCP's nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
