% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification Authority vs. Middlebox-Driven Reality
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 (TCP Specification) is contested as a specification artifact.
 *   The middlebox_realism_reading understands the constraint as follows: RFC
 *   9293 prescribes what TCP *should* be (an endpoint-to-endpoint,
 *   specification-conformant protocol); deployed reality is that middleboxes
 *   (ISPs, firewalls, state surveillance apparatus) modify TCP packets in
 *   transit for policy enforcement, surveillance, and optimization,
 *   effectively subordinating specification authority to de facto network
 *   control. The constraint is the tension between normative (what RFC 9293
 *   mandates) and operative (what middleboxes enforce). Endpoints are caught
 *   between specification compliance and middlebox adaptation — specification
 *   authority has become aspirational rather than enforceable. This reading
 *   asserts that real TCP semantics are path-dependent and middlebox-shaped,
 *   not specification-defined.
 *
 * KEY AGENTS:
 *   - RFC editors (IETF): authorship authority, no enforcement power
 *   - Middlebox operators (ISPs, enterprises, state surveillance): de facto control of packet handling
 *   - Endpoint implementers (OS kernels, application stacks): caught between specification conformance and middlebox reality
 *   - Application developers: depend on specification semantics but must code defensively
 *   - Academic researchers: measure and document the gap between RFC 9293 and deployed behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.81).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.76).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification Authority vs. Middlebox-Driven Reality").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '364be616-12b8-4b09-9efc-cf7bc1496791').
narrative_ontology:cs_kernel_codification('364be616-12b8-4b09-9efc-cf7bc1496791', fixed_text).
narrative_ontology:cs_authority_grounding('364be616-12b8-4b09-9efc-cf7bc1496791', extraction).
narrative_ontology:cs_interpretation_layer_present('364be616-12b8-4b09-9efc-cf7bc1496791').
narrative_ontology:cs_reading_relation('364be616-12b8-4b09-9efc-cf7bc1496791', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('364be616-12b8-4b09-9efc-cf7bc1496791', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('364be616-12b8-4b09-9efc-cf7bc1496791', foundational, deployed_infrastructure_supremacy).
narrative_ontology:cs_axiom_status(deployed_infrastructure_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('364be616-12b8-4b09-9efc-cf7bc1496791', deployed_infrastructure_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('364be616-12b8-4b09-9efc-cf7bc1496791', foundational, specification_authority_subordinate_to_network_control).
narrative_ontology:cs_axiom_status(specification_authority_subordinate_to_network_control, holdable).
narrative_ontology:cs_axiom_grounding('364be616-12b8-4b09-9efc-cf7bc1496791', specification_authority_subordinate_to_network_control, deontological).
narrative_ontology:cs_reference_frame('364be616-12b8-4b09-9efc-cf7bc1496791', endpoint_autonomy_era).
narrative_ontology:cs_drift_state('364be616-12b8-4b09-9efc-cf7bc1496791', contemporary_middlebox_ubiquity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('364be616-12b8-4b09-9efc-cf7bc1496791', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, surveillance_capable_state).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_autonomy).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, content_distribution_networks).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, network_authority_de_facto_supremacy).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, path_dependent_protocol_evolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and maintain RFC 9293 as the authoritative TCP specification. Define the protocol's abstract state machine, define endpoint behavior, make normative claims about packet handling. They have no enforcement power over deployed middleboxes — their authority is purely textual. They can update the RFC based on implementation evidence, but lag behind actual network behavior by years.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, rfc_editors_ietf, agenda_setter,
    institutional, civilizational, analytical, global).

% Write TCP stacks in operating systems and applications. They benefit from having a clear specification to code against and to claim compliance with for interoperability assurance. They pay by needing to work around undocumented middlebox behaviors: they must detect and repair broken packets, add de-facto compatibility shims, handle non-standard middlebox timeouts and resets. The specification does not shield them from this cost.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_implementers, payer).

% ISPs, enterprise firewalls, state surveillance apparatus, content-inspection vendors. They modify TCP packets in transit: reassemble streams, inject resets, hold connections open for inspection, rate-limit, block, or reroute flows. They operate outside the RFC 9293 authorization framework and answer to their own policies, not to IETF specifications. Their control is de facto absolute at their network vantage points.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Write client/server applications expecting RFC 9293 semantics. They must now code defensively against middlebox interference: detecting timeouts they didn't cause, handling packet reordering and drops middleboxes introduce, working around connection hijacks. The specification's promise of reliable in-order delivery is conditional on an invisible network layer that may not uphold it.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Operate like middleboxes (modify, intercept, reroute TCP flows) but with legitimacy because they claim to serve end-user performance. They benefit from the specification's weakness: they can deviate from RFC 9293 semantics without triggering interoperability crises because many middleboxes already do. They extract performance control (they decide packet routing, caching, optimization) from the original endpoint-to-endpoint architecture.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, content_distribution_networks, beneficiary,
    powerful, generational, arbitrage, global).

% End consumers of applications built on TCP. They are not in the conversation about RFC 9293 compliance. They are affected by middlebox interference (broken applications, slow connections, surveillance of their traffic) but have no seat at standards discussions and no way to route around middleboxes. Their interests are structurally absent from the constraint's negotiation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users, excluded,
    powerless, immediate, trapped, global).

% Measure TCP behavior in the wild via active probing, passive observation, and controlled experiments. They document the gap between RFC 9293 and reality: middleboxes that violate the spec, endpoint behaviors that deviate from it, implicit protocols emerging around middlebox expectations. They produce evidence that specification authority is de facto subordinate to middlebox control.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, academic_protocol_researchers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TCP specification (RFC 9293) solves the problem of defining a reliable, ordered byte-stream protocol that all implementations must follow to achieve interoperability. It provides a single, normative reference for endpoint behavior, enabling applications to make promises about delivery semantics without knowing the specific network path or other implementations in play.
% TRANSFER_FUNCTION: Transfers control of packet handling from endpoint implementers (who could craft custom behaviors) to middlebox operators. Endpoints are bound to RFC 9293 semantics; middleboxes are not. Specification authority (the power to define what TCP means) moves from the IETF to the deployed infrastructure of ISPs, firewalls, and state actors. De facto control of protocol semantics migrates from standard-setters to network operators.
% ABSENT_VOICES: End users are structurally absent — they experience middlebox interference but have no venue to contest it. Alternative protocol designers (QUIC, custom UDP-based protocols) are not in the TCP specification negotiation; they emerge only after the gap between specification and reality becomes unbearable. Endpoint implementers affected by middlebox costs are present but subordinate to both RFC editors and middlebox operators.
% DISAPPEARANCE_RATIONALE: If RFC 9293 authority ceased (specification became merely descriptive rather than normative), endpoint implementations would diverge widely in ways middleboxes could not police. Applications would need to negotiate protocol variants with every network path. Interoperability would degrade unless middleboxes published their own behavior specs, which would make them de jure standard-setters. The constraint is what keeps the fiction of a single global TCP semantics alive despite middleboxes' de facto control.
% FOUNDING_PROBLEM: TCP was designed for a specific topology (end-to-end, no intermediaries) and required a single authoritative specification to ensure that thousands of independent implementations across different OS kernels, networks, and organizations could interoperate without coordination. RFC 9293 is that specification.
% FOUNDING_PROBLEM_CORROBORATION: The topology assumption failed: middleboxes are ubiquitous, not exceptional. Network researchers (Paxson, Padhye, Medina, et al.) document widespread middlebox interference as normal network behavior, not anomaly. RFC editors acknowledge in RFC 9293 preface that 'many TCP implementations have diverged from the specification' — an admission that the original problem (ensuring single authoritative semantics) is no longer being solved. Endpoint implementers testify in IETF meetings that coding to the spec alone is insufficient; they must detect and work around middlebox interference. The founding problem is solved *only* by middlebox operators enforcing their own protocol variants, not by RFC 9293.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) because middleboxes extract control over protocol semantics from endpoint implementers, who are bound by the specification but cannot enforce it. The extraction is asymmetric: endpoints must conform to RFC 9293, middleboxes must not. Suppression is substantial (0.76) because maintaining this arrangement requires that endpoint implementers not revolt and demand enforcement, and that applications continue to assume RFC 9293 semantics even when middleboxes violate them. Theater is moderate (0.42): the specification's performative function is real (it does coordinate expectations), but a growing share of RFC 9293's content is theater — the specification claims authority over middlebox behavior it cannot enforce, and middleboxes ignore clauses they find inconvenient. The measurement series shows both extractiveness and suppression rising over 26 time units (spanning approximately two decades of TCP evolution, from late 1990s through early 2020s), tracking the increasing ubiquity and sophistication of middlebox interference as the Internet infrastructure matured. Theater ratio rises more slowly, indicating that the specification's coordination function remains real even as its enforcement authority erodes.
 *
 * PERSPECTIVAL GAP:
 *   RFC editors and endpoint implementers should compute as beneficiaries under the strict_invariance_reading (specification authority is their domain). Under this reading they compute as constrained targets: they must follow the specification while middleboxes do not. Middlebox operators compute as pure beneficiaries: they extract control without conforming. Application developers compute as targets: they pay the cost of the mismatch. The engine should compute these divergences from the structural data (who benefits, who pays, exit options). The authored claim is Tangled Rope (coordination function present, asymmetric extraction present, active enforcement present); if the engine computes Snare at any seat, that signals a constraint whose 'coordination' framing is a cover story for pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators are structural beneficiaries: they extract control (d ≈ 0.1 on the beneficiary end). Endpoint implementers are structural targets: bound by specification, unable to enforce it against middleboxes, trapped between two authorities (d ≈ 0.85). RFC editors sit near symmetric (d ≈ 0.45): they author the specification but cannot enforce it, and must defer to what the network actually does. Application developers are targets: depend on a specification the network does not uphold (d ≈ 0.80). Academic researchers are observers (d = analytical, unchanged). Endpoint users are structurally absent (not modeled as stakeholders because they have no role in the constraint's negotiation, even though they are affected by it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ensuring interoperability via a single authoritative specification) is dead: middleboxes prove that a single specification cannot ensure interoperability when network infrastructure operates outside the specification's authorization. The constraint persists because (a) the specification still coordinates expectations (applications still expect RFC 9293 semantics), (b) the gap between specification and reality is not yet unbearable enough to force protocol replacement (endpoints have workarounds), and (c) middlebox operators benefit from the specification's weakness (it provides cover for their modifications — 'we're just optimizing TCP'). The mandatrophy is resolved by classifying this as Tangled Rope rather than Rope: the specification solves the original coordination problem, but that solution now rides on top of an extractive arrangement (middleboxes' de facto control). The specification's authority is tangled with middlebox enforcement; the two are not separable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_authority_grounding,
    'What makes RFC 9293 authoritative if middleboxes routinely violate it without consequences?',
    'Case analysis: (a) if specification authority remains purely normative (what should be), the constraint is Tangled Rope with a weak normative layer; (b) if authority is grounded in deployment coordination (what implementers expect), then middleboxes'' violation erodes it and the constraint becomes Snare; (c) if authority is grounded in academic legitimacy (peer review, IETF process), then violations are external and the specification remains Rope.',
    'If (a), classification stands as authored. If (b), the constraint reclassifies to Snare because middleboxes'' de facto veto power over the specification makes the normative layer pure theater. If (c), the specification is legitimate despite violations, and extractiveness should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_grounding, conceptual, 'The grounding of specification authority in a network where enforcement is decentralized.').

omega_variable(
    middlebox_heterogeneity_impact,
    'Does the diversity of middlebox types (ISP firewalls, enterprise proxies, state censorship apparatus, CDN optimization) create distinct sub-constraints with different extractiveness profiles?',
    'Decompose by middlebox operator type and measure extractiveness per type. Some middleboxes (CDN optimization for performance) may show low extractiveness; others (state surveillance) may show high. Single ε may be inadequate.',
    'If heterogeneity is substantial (ε varies >0.3 across types), the constraint should decompose into a family of related constraints, each with its own victim set and beneficiary structure. If heterogeneity is modest, single ε is defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middlebox_heterogeneity_impact, empirical, 'Whether middlebox heterogeneity requires constraint decomposition.').

omega_variable(
    endpoint_workaround_autonomy,
    'Do endpoint implementers'' defensive workarounds (detecting middlebox timeouts, reordering, connection hijacking) constitute a form of exit, or are they constrained compliance?',
    'Measure exit success: if endpoints can reliably detect and repair middlebox interference without application-level failure, they have constrained exit; if repairs are unreliable and failures persist, exit is trapped.',
    'Constrained exit would lower directionality (d for endpoint implementers moves toward 0.7 instead of 0.85), reducing effective extraction. Trapped exit preserves high d and high χ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endpoint_workaround_autonomy, empirical, 'Whether endpoint defensive mechanisms constitute meaningful exit from the constraint.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the middlebox_realism_reading logically foreclose the strict_invariance_reading, or do they coexist as live positions held by different parties?',
    'Test logical structure: strict_invariance asserts ''RFC 9293 is an invariant implementations must replicate exactly''; middlebox_realism asserts ''RFC 9293 is subordinated by deployed middlebox behavior.'' These are logically incompatible IF and only IF ''subordinated'' means ''not an invariant.'' If subordinated means ''an invariant in principle but violated in practice,'' the readings coexist. If it means ''not binding on middleboxes,'' they foreclose.',
    'If forecloses: the strict_invariance reading is not a live reading, only a historical artifact. If coexists_with: both readings are held by different institutional seats (strict_invariance by IETF, middlebox_realism by operators and researchers); the constraint is contested in a live way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between the middlebox_realism and strict_invariance readings of the RFC 9293 kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(rfc9_tr_t0, observed).
narrative_ontology:measurement(rfc9_tr_t3, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 3, 0.27).
narrative_ontology:measurement_basis(rfc9_tr_t3, observed).
narrative_ontology:measurement(rfc9_tr_t6, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(rfc9_tr_t6, observed).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(rfc9_tr_t10, observed).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(rfc9_tr_t15, observed).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(rfc9_tr_t20, observed).
narrative_ontology:measurement(rfc9_tr_t26, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 26, 0.42).
narrative_ontology:measurement_basis(rfc9_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(rfc9_be_t0, observed).
narrative_ontology:measurement(rfc9_be_t3, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 3, 0.63).
narrative_ontology:measurement_basis(rfc9_be_t3, observed).
narrative_ontology:measurement(rfc9_be_t6, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(rfc9_be_t6, observed).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement_basis(rfc9_be_t10, observed).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(rfc9_be_t15, observed).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(rfc9_be_t20, observed).
narrative_ontology:measurement(rfc9_be_t26, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 26, 0.81).
narrative_ontology:measurement_basis(rfc9_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(rfc9_su_t0, observed).
narrative_ontology:measurement(rfc9_su_t3, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement_basis(rfc9_su_t3, observed).
narrative_ontology:measurement(rfc9_su_t6, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(rfc9_su_t6, observed).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(rfc9_su_t10, observed).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(rfc9_su_t15, observed).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(rfc9_su_t20, observed).
narrative_ontology:measurement(rfc9_su_t26, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 26, 0.76).
narrative_ontology:measurement_basis(rfc9_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.12).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_protocol_specification).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, encrypted_sni_adoption).

% DUAL FORMULATION NOTE:
% The rfc9293_tcp_specification kernel is read by three constraint stories: strict_invariance_reading (mountain/rope), optimization_latitude_reading (rope/scaffold), middlebox_realism_reading (tangled_rope/snare). Each reading extracts a different constraint from the same kernel text because they disagree about what authority (textual, deployment-empirical, or network-operative) grounds the specification's legitimacy. The middlebox_realism_reading presupposes that deployed intermediary infrastructure has de facto veto power over textual specification authority. This decomposition is required because the three readings yield different ε values: strict_invariance gives low ε (specification is self-enforcing), optimization_latitude gives moderate ε (specification permits latitude), middlebox_realism gives high ε (specification is overridden). Per ε-invariance principle, one kernel, three ε values, three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__middlebox_realism_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
