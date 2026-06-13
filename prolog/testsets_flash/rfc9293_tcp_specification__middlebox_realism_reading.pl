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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Specification (Middlebox Realism Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   This constraint describes the de facto reality of TCP operation, where
 *   RFC 9293 (the ideal specification) is frequently overridden or modified
 *   by deployed middleboxes. The specification, in this reading, functions as
 *   an aspirational document rather than an enforceable standard, with real
 *   network behavior being path-dependent and shaped by the middlebox
 *   population. This creates a Tangled Rope dynamic where the specification
 *   nominally coordinates, but middlebox operators extract control and impose
 *   costs on endpoints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.65).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.75).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Specification (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '19a88e92-f2ee-4042-80ff-e7106d1b325b').
narrative_ontology:cs_kernel_codification('19a88e92-f2ee-4042-80ff-e7106d1b325b', fixed_text).
narrative_ontology:cs_authority_grounding('19a88e92-f2ee-4042-80ff-e7106d1b325b', distributed).
narrative_ontology:cs_reading_relation('19a88e92-f2ee-4042-80ff-e7106d1b325b', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('19a88e92-f2ee-4042-80ff-e7106d1b325b', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('19a88e92-f2ee-4042-80ff-e7106d1b325b', foundational, deployed_network_defines_protocol).
narrative_ontology:cs_axiom_status(deployed_network_defines_protocol, holdable).
narrative_ontology:cs_axiom_grounding('19a88e92-f2ee-4042-80ff-e7106d1b325b', deployed_network_defines_protocol, empirically_contingent).
narrative_ontology:cs_axiom('19a88e92-f2ee-4042-80ff-e7106d1b325b', secondary, middlebox_modifications_are_de_facto_standard).
narrative_ontology:cs_axiom_status(middlebox_modifications_are_de_facto_standard, holdable).
narrative_ontology:cs_axiom_grounding('19a88e92-f2ee-4042-80ff-e7106d1b325b', middlebox_modifications_are_de_facto_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('19a88e92-f2ee-4042-80ff-e7106d1b325b', end_to_end_principle_ideal).
narrative_ontology:cs_drift_state('19a88e92-f2ee-4042-80ff-e7106d1b325b', contemporary_internet_deployment, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('19a88e92-f2ee-4042-80ff-e7106d1b325b', '').
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

% Publishes RFCs like 9293, aiming to define interoperable network protocols. In this reading, their specification authority is aspirational, often overridden by deployed network realities, particularly middleboxes. They continue to publish, but their ability to enforce 'ideal' behavior is limited.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_engineering_task_force_ietf, agenda_setter,
    institutional, generational, constrained, global).

% Deploy and operate network devices (firewalls, NATs, proxies, traffic shapers) that inspect and modify TCP packets. They benefit from the de facto power to shape network behavior, often for security, policy enforcement, or revenue generation, even if it violates RFC specifications. Their actions are the 'real' TCP.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Leverage middlebox capabilities for monitoring and control of network traffic. They benefit from the ability of middleboxes to intercept and modify TCP streams, which provides a mechanism for surveillance and censorship that bypasses endpoint control and RFC intent.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Experience degraded performance, unexpected connection resets, or privacy violations due to middlebox interference. They have no direct control over the network path their traffic takes and are largely unaware of the specific modifications occurring.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users, payer,
    powerless, immediate, trapped, global).

% Must design applications to be robust against unpredictable TCP behavior caused by middleboxes, rather than relying on the clean abstraction promised by RFCs. This increases development complexity and limits innovation, as they cannot assume standard protocol semantics.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Struggle to conduct experiments or deploy new protocols that assume strict adherence to TCP specifications, as middleboxes often break these assumptions. They bear the cost of a less predictable and harder-to-innovate network, while also observing and documenting the discrepancy.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, network_researchers, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RFC 9293 specification attempts to coordinate endpoint behavior to ensure reliable, interoperable data transfer across a heterogeneous network.
% TRANSFER_FUNCTION: This reading highlights a transfer of control and autonomy from network endpoints (users, applications) to middlebox operators, who gain the ability to inspect, modify, and filter traffic, often for their own policy or economic objectives.
% ABSENT_VOICES: The original architects of TCP and proponents of end-to-end transparency are largely absent from the operational decision-making that deploys middleboxes. They would argue for strict adherence to protocol specifications and minimal network interference.
% DISAPPEARANCE_RATIONALE: If the de facto middlebox-driven TCP behavior vanished overnight, and all network devices strictly adhered to RFC 9293, many security, policy, and business models built on middlebox functionality would collapse. Network traffic would flow more freely, but existing enforcement mechanisms would cease to function, leading to a significant reorganization of network operations and security paradigms.
% FOUNDING_PROBLEM: The original TCP specification aimed to provide a robust, reliable, and interoperable transport layer for the nascent internet, allowing any two endpoints to communicate predictably.
% FOUNDING_PROBLEM_CORROBORATION: The IETF and network researchers attest that the founding problem of reliable communication is still live, but its solution is undermined by middlebox behavior. Middlebox operators and state agencies argue that their interventions are necessary to solve new problems (security, policy, national interest) that the original specification did not foresee; this claim is contested by endpoint users and application developers who experience the negative consequences.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).

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
 *   Extractiveness is high (0.65) because middleboxes impose significant costs (performance degradation, privacy loss, reduced interoperability) on endpoints without their consent. Suppression is also high (0.75) as endpoints have limited to no ability to bypass middlebox interference, and the middlebox ecosystem actively suppresses alternative, RFC-compliant paths. The theater ratio (0.4) reflects that while the IETF continues to publish and update specifications, a substantial portion of network 'compliance' is performative, with actual behavior diverging significantly due to middlebox actions.
 *
 * PERSPECTIVAL GAP:
 *   The IETF and network researchers experience this as a degradation of a coordination mechanism, where their authority is eroded. Middlebox operators, however, experience it as a functional necessity or a legitimate exercise of control, benefiting from the ability to shape traffic. Endpoint users and application developers experience it as an opaque, extractive force that undermines the network's reliability and predictability.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators and state surveillance agencies are clear beneficiaries (d near 0.0) as they gain control and data. Endpoint users and application developers are victims (d near 1.0) as they bear the costs of non-compliant network behavior. The IETF, while nominally the agenda-setter, is a constrained actor whose specifications are often ignored, placing them closer to a payer/observer role in this specific dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reliable, interoperable TCP) has not atrophied, but the *authority* to enforce that mandate has shifted from the specification to the deployed network infrastructure. This classification as Tangled Rope prevents mislabeling it as a pure Snare (which would ignore the original coordination intent) or a Rope (which would ignore the asymmetric extraction and enforcement by middleboxes). It highlights the ongoing tension between specification and reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_vs_deployment_authority,
    'To what extent does the IETF''s specification authority genuinely influence deployed network behavior, versus merely documenting an ideal?',
    'Empirical studies tracking the adoption rate of new RFC features versus the prevalence of middlebox-induced deviations, and analysis of policy changes in major network operators.',
    'If influence is high, the constraint leans more towards a Rope (coordination with some friction); if influence is low, it confirms the Tangled Rope/Snare classification, highlighting the gap between stated intent and operational reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_vs_deployment_authority, empirical, 'The actual locus of authority in TCP behavior.').

omega_variable(
    middlebox_necessity_vs_opportunism,
    'Are middlebox interventions primarily driven by genuine security/functional necessity, or by opportunistic leveraging of network control for other gains (e.g., surveillance, traffic shaping for revenue)?',
    'Detailed analysis of middlebox functionality, comparing claimed security benefits against documented RFC violations and the presence of alternative, less intrusive solutions. Policy analysis of operator incentives.',
    'If necessity dominates, the extraction might be re-evaluated as a higher, but legitimate, coordination cost. If opportunism dominates, the Snare-like aspects of the Tangled Rope are amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_opportunism, conceptual, 'Motivation behind middlebox deployment and behavior.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''middlebox realism'' reading of the RFC 9293 TCP specification kernel?',
    'Comparison with other readings (strict invariance, optimization latitude) to ensure distinct structural properties and ε values. Verification that this reading''s core premise (deployed middleboxes shape TCP more than RFCs) is consistently applied.',
    'If misidentified, the classification would be inaccurate, potentially conflating distinct constraints. Correct identification ensures the framework models the specific structural claim being made.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensures this constraint is a distinct, ε-invariant reading of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1989, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1989, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(rfc9_tr_t1999, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1999, 0.2).
narrative_ontology:measurement(rfc9_tr_t2009, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2009, 0.3).
narrative_ontology:measurement(rfc9_tr_t2019, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(rfc9_tr_t2023, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1989, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1989, 0.2).
narrative_ontology:measurement(rfc9_be_t1999, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1999, 0.4).
narrative_ontology:measurement(rfc9_be_t2009, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement(rfc9_be_t2019, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(rfc9_be_t2023, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1989, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement(rfc9_su_t1999, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement(rfc9_su_t2009, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2009, 0.65).
narrative_ontology:measurement(rfc9_su_t2019, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(rfc9_su_t2023, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'rfc9293_tcp_specification' kernel. Other readings include 'strict_invariance_reading' and 'optimization_latitude_reading', which model different interpretations of TCP specification authority and behavior.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
