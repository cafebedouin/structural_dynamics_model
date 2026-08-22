% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__strict_invariance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP State Machine Strict Invariance Coordination
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 is the IETF's definitive specification of TCP state machine
 *   behavior. This constraint story instantiates the STRICT INVARIANCE
 *   READING: the specification is an invariant that all implementations MUST
 *   replicate exactly to preserve global interoperability. Under this
 *   reading, any implementation deviation or middlebox reinterpretation is a
 *   specification violation that threatens the coordination. The constraint's
 *   type is pure Rope: genuine coordination problem (interoperability without
 *   negotiation), no extractiveness (beneficiaries and payers are the same
 *   global set — implementers forgo latitude, the network gains certainty).
 *   The measurement series and metrics reflect this pure coordination: very
 *   low extractiveness (0.08 at interval end, rising only as middleboxes
 *   accumulate deployed modifications that the strict reading treats as
 *   external pressure, not as a function of the constraint itself), low
 *   theater (conformance testing is genuinely measuring state-machine
 *   compliance), low suppression (implementation choice to conform, not
 *   coerced). This is NOT the optimization_latitude_reading (which would show
 *   higher extractiveness by framing the latitude forfeiture as a cost) or
 *   the middlebox_realism_reading (which would show middlebox deviation as
 *   central to the constraint's operation, not extraneous pressure). This
 *   reading treats RFC 9293 as the definition of the coordinate system;
 *   sibling readings treat it as one interpretation among others.
 *
 * KEY AGENTS:
 *   - endpoint_implementers_strict_reading: Benefit from and replicate the invariant state machine; zero modification path
 *   - network_operators_path_dependent: Constrained by invariance reading to not modify state behavior; their deployed modifications violate the coordinate frame
 *   - application_developers_assume_invariance: Benefit when they can assume identical RFC 9293 behavior globally
 *   - standards_authority_ietf: Sets and maintains the specification as the canonical coordinate frame
 *   - protocol_testers_certification: Measure conformance to state machine; analytical seat
 *   - legacy_middlebox_deployments: Excluded from this reading's coordination frame; would dispute the strict interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.12).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP State Machine Strict Invariance Coordination").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '370e06aa-3f6d-41c1-909e-42151d8139a5').
narrative_ontology:cs_kernel_codification('370e06aa-3f6d-41c1-909e-42151d8139a5', fixed_text).
narrative_ontology:cs_authority_grounding('370e06aa-3f6d-41c1-909e-42151d8139a5', expertise).
narrative_ontology:cs_interpretation_layer_present('370e06aa-3f6d-41c1-909e-42151d8139a5').
narrative_ontology:cs_reading_relation('370e06aa-3f6d-41c1-909e-42151d8139a5', rfc9293_tcp_specification__optimization_latitude_reading, forecloses).
narrative_ontology:cs_reading_relation('370e06aa-3f6d-41c1-909e-42151d8139a5', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('370e06aa-3f6d-41c1-909e-42151d8139a5', foundational, specification_state_machine_identity_necessary).
narrative_ontology:cs_axiom_status(specification_state_machine_identity_necessary, holdable).
narrative_ontology:cs_axiom_grounding('370e06aa-3f6d-41c1-909e-42151d8139a5', specification_state_machine_identity_necessary, empirically_contingent).
narrative_ontology:cs_axiom('370e06aa-3f6d-41c1-909e-42151d8139a5', foundational, endpoint_conformance_is_primary_over_deployed_practice).
narrative_ontology:cs_axiom_status(endpoint_conformance_is_primary_over_deployed_practice, holdable).
narrative_ontology:cs_axiom_grounding('370e06aa-3f6d-41c1-909e-42151d8139a5', endpoint_conformance_is_primary_over_deployed_practice, conventional).
narrative_ontology:cs_reference_frame('370e06aa-3f6d-41c1-909e-42151d8139a5', rfc9293_canonical_state_machine).
narrative_ontology:cs_drift_state('370e06aa-3f6d-41c1-909e-42151d8139a5', contemporary_middlebox_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('370e06aa-3f6d-41c1-909e-42151d8139a5', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, global_tcp_interoperability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, endpoint_implementers_strict_reading).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_developers_assume_invariance).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, network_operators_path_dependent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement RFC 9293 state machine as specified. They benefit from predictable, deterministic behavior across all TCP stacks: packets sent from a peer in a known state will be handled by a known algorithm. When all implementers replicate the same invariant, end-to-end interoperability is guaranteed without negotiation or middleware translation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, endpoint_implementers_strict_reading, beneficiary,
    organized, generational, constrained, global).

% Operate deployed middleboxes (firewalls, proxies, NAT gateways, load balancers) that modify or reinterpret TCP state transitions to handle non-ideal path conditions (asymmetry, congestion, DDoS, NAT). Under strict invariance reading, every such modification is a specification violation and a potential interoperability hazard. Their practical network operation often requires deviations; strict reading treats those as constraints on their freedom.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, network_operators_path_dependent, payer,
    institutional, biographical, constrained, global).

% Build applications that rely on RFC 9293 behavior being identical on all endpoints. They benefit when they can assume a packet received in state X will invoke exactly the handling specified for state X, without variation. Strict invariance means they can reason about protocol guarantees once, globally, rather than per-OS or per-implementation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_developers_assume_invariance, beneficiary,
    organized, biographical, mobile, global).

% Maintains RFC 9293 as the authoritative specification. Under strict reading, the authority asserts that the state machine specified IS the constraint — implementations must conform, not reinterpret. The IETF does not enforce this directly but sets the standard against which conformance is measured.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, standards_authority_ietf, agenda_setter,
    institutional, generational, analytical, global).

% Run conformance test suites verifying implementations replicate the state machine. Under strict reading, test failures indicate specification violation; under optimization reading, the same behavior might pass a functional test (correct output) despite taking a different state path. Testers are analytical observers: they measure conformance but do not set it.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_testers_certification, observer,
    powerful, biographical, analytical, global).

% Have been deployed for years with state-machine modifications that work in practice but violate strict RFC 9293 reading. They operate outside the conversation the strict reading frames (interoperability via specification identity); if they were brought into the conversation, they would argue for optimization latitude or path-dependent readings instead.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, legacy_middlebox_deployments, excluded,
    institutional, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, deterministic algorithm for endpoint TCP behavior so implementations across vendors, operating systems, and networks can interoperate without middleware translation or per-peer negotiation: when both endpoints replicate the RFC 9293 state machine identically, any packet from state X invokes the same handling on both sides.
% TRANSFER_FUNCTION: The constraint transfers implementation latitude (dropped by implementers) to interoperability certainty (collected by the global TCP ecosystem). Implementers forgo performance optimizations that would deviate from the state machine; the entire network benefits from eliminable ambiguity.
% ABSENT_VOICES: Network operators with deployed path-dependent middleboxes are structurally excluded from the coordination frame of strict invariance reading — they would argue for freedom to modify state transitions for load balancing, congestion handling, or security. Standards-compliance practitioners who believe optimization latitude is compatible with interoperability are also excluded.
% DISAPPEARANCE_RATIONALE: If RFC 9293 strict invariance disappeared (implementations were permitted arbitrary state-machine variations), TCP interoperability would fracture into per-vendor dialects. Applications could not assume behavior; middleboxes would need per-implementation translation; the global end-to-end model would collapse into negotiated, path-dependent protocols. The internet's reliable transport would shift from identity-based coordination to capability-based (what does THIS implementation do?) discovery.
% FOUNDING_PROBLEM: In the 1980s-1990s, TCP implementations varied widely in non-critical details; networks suffered interoperability failures. RFC 9293 was standardized to specify a canonical state machine so implementers would converge on identical behavior, eliminating the need for per-peer negotiation or middleware translation.
% FOUNDING_PROBLEM_CORROBORATION: Internet architecture literature (RFC 1958, Clark et al. on end-to-end principle) attests the founding problem remains critical: specification identity is the foundation of global interoperability. Network operators and middleware vendors (outside the beneficiary set) attest the founding problem has been partially solved by deployed implementations but also constrain the evolution of the protocol via their path-dependent modifications — the problem is contested at the margins (what about network variation?) but not dead at the core.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).
:- end_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the constraint is pure coordination: implementers choose to conform, and they benefit from the global interoperability that conformance produces. The cost (forgoing optimization latitude) is borne by the same agents who collect the benefit (deterministic behavior). No party systematically extracts from another — the constraint lives at the Rope ideal point. Suppression is low (0.12) because conformance is enforced by certification testing and market expectation, not by coercive means; an implementer CAN deviate if they choose (constrained exit, not trapped exit). Theater is minimal (0.05) because conformance testing genuinely measures state machine identity, and the specification's function IS conformance measurement. The measurement series show slight rise over the interval: suppression rises as deployed middlebox modifications accumulate and the ecosystem has to decide whether to treat them as specification violations (strict reading) or as acceptable network evolution (middlebox realism reading). The constraint type should NOT waver — it remains Rope throughout — but the pressure on the coordinate frame from deployed path-dependent behavior does increase over time. This is modeled as rising suppression_requirement (the effort needed to maintain strict invariance in the face of accumulated middleware pressure) and slight extractiveness rise (as operators face choices about which reading to adopt). The divergence between rising external pressure and stable constraint classification is exactly what temporal measurement is for.
 *
 * PERSPECTIVAL GAP:
 *   All seats should compute the same type: Rope. The IETF sees a specification they set; implementers see a target to conform to; developers see a guarantee they rely on; network operators see a constraint they live within (but benefit from). Under the strict invariance reading, there is no divergence because the reading itself asserts that invariance IS the constraint, period. The tension appears when shifting to sibling readings: the middlebox_realism_reading would show network operators computing Snare (they are constrained by a coordinate frame they did not set and cannot modify for path-dependent reasons). The optimization_latitude_reading would show implementers computing Rope with freedom (same coordination, but with latitude to vary the internal path). This reading explicitly forecloses those reframings — it asserts the state machine IS invariant, and all seats must compute from that fixed anchor.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has no target/payer in the extractive sense. All stakeholders are beneficiaries of the invariance: endpoint implementers benefit from knowing other endpoints will follow the same state machine, application developers benefit from being able to assume identical behavior, network operators benefit from interoperable transport (even as the strict reading constrains their middleware freedom). The only structural asymmetry is agenda-setter vs. beneficiary: the IETF (or whoever maintains the specification) is the agenda-setter — they set what the invariant IS — but they do not extract anything. They are not powerless (institutional power), but their structural role is to define the coordinate frame, not to collect rents. This is a key diagnostic for Rope: beneficiaries and payers align; no extractive asymmetry. In the middlebox_realism_reading, network operators would shift from beneficiary to payer (they would bear a cost: constraint on their freedom to modify). In this reading, they remain beneficiaries of global TCP interoperability, even as their specific technologies (middleboxes) are excluded from the strict invariance frame.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint. The founding problem (interoperability via specification identity) remains live. Implementations have continued to replicate RFC 9293; the constraint's function has not atrophied. What has happened is that deployed middleboxes have accumulated path-dependent modifications that the strict reading treats as external pressure (not as the constraint evolving). The strict reading holds: the coordinate frame is invariant, and deviations are deviations. A mandatrophy reading would arise if implementations stopped following RFC 9293 while administrators maintained it theatrically — that has not occurred. The accumulated pressure from middlebox realism is instead a signal that the kernel (RFC 9293's authority and interpretation) is contested — a structural fact about the reading, not a mandatrophy claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_identity_necessity,
    'Is exact state machine identity logically necessary for TCP interoperability, or is functional equivalence (same observable behavior) sufficient?',
    'Empirical testing: deploy two TCP implementations that differ in internal state paths but produce identical packet behavior; run a full interoperability suite. If the suite passes, state identity is not necessary. If subtle failures occur, state identity is necessary.',
    'If functional equivalence is sufficient, this reading recedes toward optimization_latitude and extractiveness rises (implementers are constrained more than necessary). If state identity is necessary, the constraint remains Rope (coordination requires specification identity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_identity_necessity, empirical, 'Whether RFC 9293 state identity is a logical requirement or a useful convention.').

omega_variable(
    middlebox_modification_violates_or_adapts,
    'Do deployed middlebox modifications that deviate from RFC 9293 state machine constitute specification violations that threaten interoperability, or are they legitimate adaptations that preserve end-to-end correctness under path variation?',
    'Internet topology measurement: trace middlebox modifications in deployed networks and measure interoperability correlation. If networks with fewer middlebox deviations have fewer interoperability incidents, modifications violate the guarantee. If correlation is weak, modifications adapt without breaking guarantees.',
    'If modifications violate: strict reading holds, suppression requirement rises, and the constraint becomes contested (middleboxes push against it). If modifications adapt: middlebox_realism_reading gains coherence, and this reading''s treatment of them as violations is a false positive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middlebox_modification_violates_or_adapts, empirical, 'Whether middlebox deviation from RFC 9293 is pathological or functional.').

omega_variable(
    authority_of_specification_under_contest,
    'When RFC 9293 (an IETF specification) and deployed network practice (middlebox modifications) diverge, which has authority over what the constraint IS?',
    'Institutional analysis: trace how standards bodies, vendors, and operators adjudicate conflicts between specification and deployment practice. If the IETF or vendors prioritize specification, authority is formal. If deployed practice drives vendor behavior, authority is empirical.',
    'If specification authority is formal and enduring, this strict reading correctly centers RFC 9293 as the coordinate frame. If deployed practice has become authoritative, the middlebox_realism_reading is the operational ground truth, and this reading is aspirational but not effective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_of_specification_under_contest, conceptual, 'Whether RFC 9293 or deployed practice grounds the authority structure for TCP conformance.').

omega_variable(
    kernel_reading_contestation_signal,
    'Does the existence of three competing readings (strict_invariance, optimization_latitude, middlebox_realism) indicate that RFC 9293 kernel is genuinely ambiguous, or that one reading is correct and the others are motivated misreadings?',
    'Hermeneutic analysis: close-read RFC 9293 for language that supports or undermines each reading. Cross-check against historical IETF working group discussions and implementation behavior. If the text supports multiple readings with equal textual warrant, the kernel is ambiguous. If only one reading is supported, others are motivated deviations.',
    'If ambiguous: all three readings are live; strict_invariance is defensible but not uniquely true; the constraint is contested. If the text supports one reading uniquely: strict_invariance (or another) is correct; others are errors or opportunistic reinterpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_signal, conceptual, 'Whether the RFC 9293 kernel is genuinely multiply interpretable or whether one reading has superior textual warrant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(rfc9_tr_t0, observed).
narrative_ontology:measurement(rfc9_tr_t8, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 8, 0.03).
narrative_ontology:measurement_basis(rfc9_tr_t8, observed).
narrative_ontology:measurement(rfc9_tr_t16, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 16, 0.04).
narrative_ontology:measurement_basis(rfc9_tr_t16, observed).
narrative_ontology:measurement(rfc9_tr_t24, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t24, observed).
narrative_ontology:measurement(rfc9_tr_t32, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t32, observed).
narrative_ontology:measurement(rfc9_tr_t40, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(rfc9_be_t0, observed).
narrative_ontology:measurement(rfc9_be_t8, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 8, 0.07).
narrative_ontology:measurement_basis(rfc9_be_t8, observed).
narrative_ontology:measurement(rfc9_be_t16, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t16, observed).
narrative_ontology:measurement(rfc9_be_t24, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t24, observed).
narrative_ontology:measurement(rfc9_be_t32, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t32, observed).
narrative_ontology:measurement(rfc9_be_t40, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t0, observed).
narrative_ontology:measurement(rfc9_su_t8, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 8, 0.1).
narrative_ontology:measurement_basis(rfc9_su_t8, observed).
narrative_ontology:measurement(rfc9_su_t16, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement_basis(rfc9_su_t16, observed).
narrative_ontology:measurement(rfc9_su_t24, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t24, observed).
narrative_ontology:measurement(rfc9_su_t32, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t32, observed).
narrative_ontology:measurement(rfc9_su_t40, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% RFC 9293 TCP specification is a contested kernel. This story (strict_invariance_reading) treats the state machine as an invariant coordinate frame. Sibling stories (optimization_latitude_reading, middlebox_realism_reading) interpret the same kernel differently. All three are linked via network.affects_constraints. Each story authors its own ε, beneficiary/victim structure, and omega variables around the reading-specific structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
