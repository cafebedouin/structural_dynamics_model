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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP State Machine Invariance (Strict Interoperability Reading)
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 specifies a deterministic TCP state machine that all
 *   implementations must replicate to guarantee global interoperability. This
 *   story instantiates the strict_invariance_reading: the specification is a
 *   binding invariant that implementers must follow exactly, and any
 *   deviation (whether by optimization, middlebox modification, or
 *   performance trade-off) is a protocol breach. This reading contrasts with
 *   two sibling readings: the optimization_latitude_reading (which treats RFC
 *   9293 as specifying outcomes and permitting implementation latitude for
 *   performance), and the middlebox_realism_reading (which treats the
 *   specification as aspirational and acknowledges that deployed TCP is
 *   shaped by middlebox population and path conditions). The strict reading
 *   is instantiated as pure Rope — a genuine coordination function with no
 *   extractiveness, benefiting all participants equally via the guarantee of
 *   interoperable behavior. The constraint's persistence is entirely
 *   voluntary; implementations adopt it because interoperability is more
 *   valuable than marginal optimization gains.
 *
 * KEY AGENTS:
 *   - interoperability_participants: institutional implementers (OS vendors, network stack designers) that adopt RFC 9293 strictly and benefit from knowing every other implementation does the same
 *   - application_developers: beneficiaries who write once and deploy everywhere, relying on the state machine invariance to deliver identical semantics
 *   - strict_spec_implementers: institutional payers who bear the engineering cost of strict compliance (testing, validation, rejection of optimizations that break invariance)
 *   - middlebox_operators: excluded parties (network appliances that modify TCP state) whose operational needs are not accommodated by the strict reading
 *   - performance_optimizers: excluded researchers and implementers who prioritize improvements outside the state machine bounds
 *   - standards_body_ietf: agenda setter; enforces the interpretation and adjusts the specification when needed
 *   - protocol_verification_community: observers who measure compliance and gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.05).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP State Machine Invariance (Strict Interoperability Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '27426bad-7774-42d0-8588-49f23770ed03').
narrative_ontology:cs_kernel_codification('27426bad-7774-42d0-8588-49f23770ed03', fixed_text).
narrative_ontology:cs_authority_grounding('27426bad-7774-42d0-8588-49f23770ed03', expertise).
narrative_ontology:cs_interpretation_layer_present('27426bad-7774-42d0-8588-49f23770ed03').
narrative_ontology:cs_reading_relation('27426bad-7774-42d0-8588-49f23770ed03', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('27426bad-7774-42d0-8588-49f23770ed03', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('27426bad-7774-42d0-8588-49f23770ed03', foundational, state_machine_syntactic_invariance_required).
narrative_ontology:cs_axiom_status(state_machine_syntactic_invariance_required, holdable).
narrative_ontology:cs_axiom_grounding('27426bad-7774-42d0-8588-49f23770ed03', state_machine_syntactic_invariance_required, instrumental).
narrative_ontology:cs_axiom('27426bad-7774-42d0-8588-49f23770ed03', secondary, interoperability_necessitates_zero_tolerance).
narrative_ontology:cs_axiom_status(interoperability_necessitates_zero_tolerance, holdable).
narrative_ontology:cs_axiom_grounding('27426bad-7774-42d0-8588-49f23770ed03', interoperability_necessitates_zero_tolerance, empirically_contingent).
narrative_ontology:cs_reference_frame('27426bad-7774-42d0-8588-49f23770ed03', strict_state_machine_invariance).
narrative_ontology:cs_drift_state('27426bad-7774-42d0-8588-49f23770ed03', contemporary_deployed_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27426bad-7774-42d0-8588-49f23770ed03', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, interoperability_participants).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, strict_spec_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, strict_spec_implementers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implementations (operating systems, network stacks, hypervisors) that build TCP to the RFC 9293 specification and benefit from the predictable behavior of every other implementation that does the same. They receive the coordination benefit: any application can run on any stack and behave identically, without reimplementing TCP logic.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, interoperability_participants, beneficiary,
    organized, generational, mobile, universal).

% Write applications that depend on TCP semantics without needing to model or handle each implementation's quirks. They write once, run everywhere; the invariant state machine is what makes that possible.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_developers, beneficiary,
    organized, biographical, mobile, universal).

% Implement TCP strictly to RFC 9293 specifications. They bear the engineering cost of strict compliance (no optimizations outside the spec bounds, no shortcuts that break the invariant, extensive testing against the state machine). They also benefit from knowing that other implementers meet the same burden.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, strict_spec_implementers, payer,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, strict_spec_implementers, beneficiary).

% Operate network appliances (firewalls, proxies, traffic shapers) that observe or modify TCP flows in the middle of the network. The strict invariance reading treats any state-modifying middlebox action as a protocol violation. They are excluded from the coordination framework and would argue for latitude to adapt TCP behavior to local network conditions.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_operators, excluded,
    institutional, biographical, trapped, global).

% Implementers and researchers who prioritize performance improvements that deviate from the strict state machine (reordering state transitions, batching operations, non-standard timer behaviors). The strict reading treats these as violations. They would argue for a more permissive semantic boundary.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, performance_optimizers, excluded,
    powerful, generational, constrained, universal).

% Maintains RFC 9293 and adjudicates interpretation disputes. Under the strict reading, the IETF's authority is to enforce the invariance mandate and resist pressure to loosen it. Under competing readings, the IETF's role is more permissive (endorsing optimization latitude or acknowledging middlebox reality).
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, standards_body_ietf, agenda_setter,
    institutional, generational, analytical, universal).

% Researchers using formal methods, testing frameworks, and monitoring to verify whether deployed TCP implementations match the RFC 9293 state machine. They measure the gap between specification and practice.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_verification_community, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global TCP interoperability problem: enables any endpoint to communicate reliably with any other endpoint using identical semantics without negotiation or reimplementation. The strict invariance guarantee is what makes the coordination possible — if implementations differ in state machine behavior, the reliable byte stream semantics diverge.
% TRANSFER_FUNCTION: Moves implementation engineering burden from each actor individually (each stack would need to handle quirks of every other stack) to the collective burden of maintaining strict specification compliance. No extraction of resources from one party to another; the transfer is of predictability and coordination cost.
% ABSENT_VOICES: Middlebox operators and performance optimization communities are structurally excluded by the strict reading's zero-tolerance framing. They would argue that the specification should permit implementation latitude and acknowledge deployed reality. The reading does not give them a seat in the coordination framework.
% DISAPPEARANCE_RATIONALE: If the strict invariance constraint disappeared (TCP implementations ceased treating RFC 9293 as binding), interoperability would fragment: implementations would optimize independently, middleboxes would modify state machines, and applications would need to detect and work around implementation-specific behaviors. The TCP ecosystem would reorganize around de-facto standards (what major implementations actually do) rather than a single written specification.
% FOUNDING_PROBLEM: Early TCP implementations diverged in state machine details, causing interoperability failures and necessitating per-pair workarounds. RFC 9293 (originally RFC 793) codified an invariant state machine to guarantee that all implementations behave identically, enabling applications to write once and run on any stack.
% FOUNDING_PROBLEM_CORROBORATION: The strict reading attests the founding problem persists: any deviation from the state machine breaks the interoperability guarantee for edge-case scenarios. The middlebox-realism and optimization-latitude readings attest the problem is substantially solved (most applications work across most implementations) and the strict reading's zero-tolerance stance is an over-specification that ignores deployed reality. Protocol verification research from outside the standardization body documents divergences between RFC 9293 and actual implementations, supporting the contested reading.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness is near-zero (0.05 at interval end) because the coordination function is purely additive — no party gains at another's expense. The benefit distribution is universal and symmetric: the predictability of interoperability is worth more to every participant than whatever marginal optimization each forgoes. Suppression is minimal (0.08) because there is no coercive enforcement machinery required; the invariance is self-enforcing via network effects (any implementation that deviates finds itself incompatible with the rest). Theater is negligible (0.02) because the specification function is real and actively maintained — there is no performative gap. Accessibility collapse is high (0.78) because once the benefits of interoperability are understood, the alternative (not following the specification) becomes unattractive; the constraint is what the ecosystem converges on voluntarily. Resistance is low (0.12) because the strict reading aligns with the interests of most participants, though excluded parties (middlebox operators, performance researchers) constitute a real opposing pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   All beneficiaries (interoperability_participants, application_developers) derive d near 0.0 (full beneficiaries) because they receive the pure coordination benefit without bearing extraction cost. The strict_spec_implementers are technically payers (they bear engineering cost to maintain compliance) but also beneficiaries (they benefit from knowing the cost is shared by all competitors), so they sit at d≈0.35 (slightly beneficiary-tilted because the coordination benefit exceeds the compliance burden). Excluded parties (middleboxes, optimizers) are outside the directionality derivation because they are not participants in the constraint as authored; they would have high d if they were forced into compliance, but the strict reading leaves them out of scope. The IETF as agenda_setter has directionality near 0.5 (symmetric) because it bears the cost of maintaining the standard while receiving the benefit of a functioning global coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (TCP implementation divergence causing interoperability failures) was live at RFC 793's publication (1981) and remains contested today. The strict reading asserts it is still live: any deviation threatens interoperability in edge cases. The optimization-latitude and middlebox-realism readings assert it is substantially dead: most applications work across most implementations most of the time, and the strict invariance is an over-specification. The measurement series shows negligible extractiveness growth over the 1981-2024 interval, stable suppression, and stable low theater — consistent with a Rope that has not degraded into Piton. The constraint persists because the coordination benefit remains real, not because of inertia or theatrical maintenance. The contested founding-problem status is the signal: whether the founding problem is live or dead determines whether the constraint is active (Rope) or zombie (Piton masked as Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_vs_deployed_reality_divergence,
    'How much have actual TCP implementations diverged from the RFC 9293 state machine in deployed networks?',
    'Protocol verification studies measuring implementation compliance via formal testing, state machine tracing on real deployments, and middlebox modification discovery.',
    'If divergence is high, the strict reading''s premise (implementations must replicate the state machine exactly) is falsified by reality, and the constraint shifts from pure Rope toward Piton (specification maintained theatrically while practice deviates). If divergence is low, the strict reading''s coordination framing holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_vs_deployed_reality_divergence, empirical, 'The empirical gap between RFC 9293 specification and deployed TCP behavior.').

omega_variable(
    kernel_reading_contestation_extent,
    'Which reading of the RFC 9293 kernel commands the IETF''s actual enforcement authority and resource allocation?',
    'Document review of IETF working group charters, standards-track discussions, and implementation guidance. Interview or observational study of standards body decision-making on TCP errata and clarifications.',
    'If the strict invariance reading commands the authority, the constraint persists as pure Rope with the IETF as enforcer. If the middlebox-realism or optimization-latitude readings increasingly command resources, the constraint shifts toward Piton (the strict reading becomes a vestigial, unenforced ideal). This determines whether the three readings describe a live contest or a settled hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_extent, empirical, 'Which reading of the TCP specification kernel controls actual standards body authority.').

omega_variable(
    semantic_vs_syntactic_invariance_boundary,
    'Is the RFC 9293 invariant best understood as a syntactic state machine (implementers must follow the exact state transitions) or as a semantic boundary (implementers must produce identical byte-stream behavior, with state machine as one possible implementation path)?',
    'Formal analysis of RFC 9293 language and intent (historical RFC drafts, IETF meeting minutes, TCP implementer interviews). Experimental deployment of performance optimizations that preserve semantics but violate syntax, measuring interoperability impact.',
    'If syntactic: the strict reading holds and deviations are violations. If semantic: performance optimizations that preserve behavior are allowable, the optimization-latitude reading gains ground, and the constraint becomes less zero-tolerance. The boundary defines what counts as a breach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_vs_syntactic_invariance_boundary, conceptual, 'Whether the TCP specification''s invariance is syntactic or semantic.').

omega_variable(
    middlebox_modification_legitimacy,
    'Under what conditions, if any, is middlebox modification of TCP state machine justified by network conditions (congestion, path asymmetry, security threats)?',
    'Case study analysis of deployed middlebox modifications (traffic shaping, connection rate limiting, payload inspection, window adjustment) and their impact on end-to-end reliability. Regulatory or institutional analysis of whether network operators have legitimate authority to modify transit flows.',
    'The strict reading treats all middlebox modification as violation. If legitimate conditions exist, the reading becomes one position in a trade-off (interoperability guarantee vs. operational necessity) rather than an absolute rule. This affects whether the constraint type is Rope (pure coordination) or Tangled Rope (coordination with extractive enforcement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middlebox_modification_legitimacy, conceptual, 'The legitimacy of middlebox modification in the TCP stack.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.01).
narrative_ontology:measurement(rfc9_tr_t1995, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1995, 0.01).
narrative_ontology:measurement(rfc9_tr_t2005, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2005, 0.02).
narrative_ontology:measurement(rfc9_tr_t2015, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2015, 0.02).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2024, 0.02).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.02).
narrative_ontology:measurement(rfc9_be_t1995, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1995, 0.03).
narrative_ontology:measurement(rfc9_be_t2005, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2005, 0.04).
narrative_ontology:measurement(rfc9_be_t2015, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1981, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1981, 0.05).
narrative_ontology:measurement(rfc9_su_t1995, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1995, 0.06).
narrative_ontology:measurement(rfc9_su_t2005, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2005, 0.07).
narrative_ontology:measurement(rfc9_su_t2015, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2015, 0.08).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.05).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, tcp_middlebox_modification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, tcp_performance_optimization__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% The RFC 9293 TCP specification kernel decomposes into three structurally distinct constraint stories corresponding to three readings: strict_invariance_reading (this story: pure Rope, zero-tolerance invariance), optimization_latitude_reading (Tangled Rope: coordination via semantic bounds with latitude for performance optimization, extracts engineering cost from strict implementers), and middlebox_realism_reading (Piton: specification maintained but increasingly overridden by deployed reality, middlebox modification and path-dependence are the actual constraint). Each reading has different beneficiary/victim structure, different claimed_type, and different ε. They share the same kernel (RFC 9293 text) but instantiate different constraints from it. The three stories are linked via network.affects_constraints to model that challenge to one reading (e.g., empirical evidence of widespread implementation divergence) affects the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
