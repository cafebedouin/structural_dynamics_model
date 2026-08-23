% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: RFC 9293 TCP Invariant State Machine (Strict Invariance Reading)
 *   domain: technological/network_protocol
 *
 * SUMMARY:
 *   RFC 9293 (and its predecessors RFC 793, RFC 1122, RFC 7414) specifies the
 *   TCP state machine as an invariant: every conforming implementation must
 *   replicate the same state transitions, timer behaviors, and packet
 *   processing rules. This strict invariance reading holds that the
 *   specification is the coordination anchor — deviation is not
 *   'optimization' or 'adaptation,' it is violation. The constraint is a pure
 *   Rope: it solves a genuine collective-action problem (independent
 *   implementation interoperability) with minimal coercive overhead;
 *   participants are net beneficiaries; alternatives (other transports,
 *   non-compliant stacks) are not suppressed. The extractiveness is near-zero
 *   (0.03) — the cost of conformance is the price of admission to the
 *   interoperability club, not extraction by a beneficiary. Suppression is
 *   low (0.15) — no one is forced to use TCP; QUIC, UDP, and other transports
 *   exist. Theater is negligible (0.05) — the specification does real
 *   coordination work. The measurement series shows remarkable stability over
 *   four decades.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.03).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.15).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP Invariant State Machine (Strict Invariance Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "technological/network_protocol").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '8c1d0068-7225-409c-ae5d-8f3f4d83d264').
narrative_ontology:cs_kernel_codification('8c1d0068-7225-409c-ae5d-8f3f4d83d264', formalized).
narrative_ontology:cs_authority_grounding('8c1d0068-7225-409c-ae5d-8f3f4d83d264', expertise).
narrative_ontology:cs_interpretation_layer_present('8c1d0068-7225-409c-ae5d-8f3f4d83d264').
narrative_ontology:cs_reading_relation('8c1d0068-7225-409c-ae5d-8f3f4d83d264', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c1d0068-7225-409c-ae5d-8f3f4d83d264', rfc9293_tcp_specification__middlebox_realism_reading, forecloses).
narrative_ontology:cs_axiom('8c1d0068-7225-409c-ae5d-8f3f4d83d264', foundational, specification_is_authoritative).
narrative_ontology:cs_axiom_status(specification_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('8c1d0068-7225-409c-ae5d-8f3f4d83d264', specification_is_authoritative, conventional).
narrative_ontology:cs_axiom('8c1d0068-7225-409c-ae5d-8f3f4d83d264', foundational, deviation_is_violation).
narrative_ontology:cs_axiom_status(deviation_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('8c1d0068-7225-409c-ae5d-8f3f4d83d264', deviation_is_violation, conventional).
narrative_ontology:cs_reference_frame('8c1d0068-7225-409c-ae5d-8f3f4d83d264', rfc9293_invariant_state_machine).
narrative_ontology:cs_drift_state('8c1d0068-7225-409c-ae5d-8f3f4d83d264', contemporary_middlebox_deployment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c1d0068-7225-409c-ae5d-8f3f4d83d264', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, protocol_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, os_vendors_stack_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, os_vendors_stack_implementers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, tcp_interoperability_guarantee).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, specification_authority).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, invariant_state_machine_correctness).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, independent_implementation_convergence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the TCP specification through the IETF standards process. Produce RFCs, errata, and bis documents. Their authority derives from the voluntary consensus of the technical community. They do not enforce compliance; interoperability pressure does.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, ietf_tcp_maintainers, agenda_setter,
    institutional, generational, analytical, global).

% Implement the TCP state machine in operating system kernels (Linux, Windows, BSD, etc.). They benefit from a single authoritative specification that eliminates pairwise compatibility negotiation. They bear the cost of exact conformance testing and maintenance. Exit means abandoning the dominant transport protocol, which is practically infeasible for general-purpose OS vendors.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, os_vendors_stack_implementers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, os_vendors_stack_implementers, payer).

% Build applications assuming TCP's reliable, ordered byte-stream semantics. They benefit from the guarantee that any compliant stack will behave identically. They can switch to other transports (QUIC, SCTP, UDP-based) with effort, so exit is mobile. Their reliance on the specification is total for TCP-based applications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_developers, beneficiary,
    organized, biographical, mobile, global).

% Build network appliances (firewalls, NATs, load balancers, optimizers) that modify TCP headers, state, or timing in-path. This reading classifies their modifications as specification violations. They would argue that deployed middleboxes define the real protocol. Their exit from this reading's framework means accepting the 'violation' label or changing product behavior.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_vendors, excluded,
    organized, biographical, constrained, global).

% Operate the routers, links, and infrastructure that carry TCP traffic. They benefit from predictable, standard-compliant endpoint behavior that makes network management tractable. They also deploy middleboxes, creating a tension captured by the middlebox_realism_reading. Exit from TCP is not viable; the constraint is the substrate of their operation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).

% Use applications that run over TCP. They benefit from 'it just works' interoperability. They have no direct relationship to the specification and can switch applications or networks freely (arbitrage-grade exit at the user level). They are the ultimate beneficiaries of the coordination.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, end_users, beneficiary,
    powerless, biographical, arbitrage, global).

% Developers of QUIC, SCTP, and other transports. They observe the TCP specification as a coordination anchor they must either interoperate with or displace. They do not pay TCP's conformance costs nor collect its interoperability benefits directly. Their analytical seat tracks whether TCP's invariant model remains the coordination standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, competing_transport_protocols, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, exact state machine specification so that any two independently developed implementations can interoperate without prior coordination, pairwise negotiation, or runtime discovery of each other's behavioral quirks.
% TRANSFER_FUNCTION: Moves the burden of interoperability from pairwise implementation negotiation to specification compliance: each implementer invests in exact conformance to the invariant state machine, and in return gains universal connectivity with all other compliant implementations.
% ABSENT_VOICES: Middlebox vendors and network appliance manufacturers who modify TCP behavior in-path (header rewriting, state manipulation, timing alteration). They are structurally excluded by this reading's premise that such modification is a specification violation rather than a legitimate adaptation. They would argue that the deployed middlebox population *is* the real protocol.
% DISAPPEARANCE_RATIONALE: If the invariant specification vanished overnight, implementations would diverge within release cycles. Interoperability would fracture into version-specific compatibility matrices. The internet would reorganize around either a new coordination point (a successor specification) or a regime of bilateral compatibility layers and middlebox-driven de facto standards — the very fragmentation the specification was created to prevent.
% FOUNDING_PROBLEM: Early TCP implementations (pre-RFC 793, pre-RFC 1122) diverged in state machine behavior, retransmission logic, and window management in ways that broke interoperability between independently developed stacks. A single authoritative state machine was needed to allow independent implementations to work together without coordination.
% FOUNDING_PROBLEM_CORROBORATION: The IETF standards process itself, decades of multi-vendor interoperability testing (e.g., TCP/TCP interop events), and the documented history of pre-standardization fragmentation (the 'TCP wars' of the 1980s) all corroborate this from outside any single beneficiary. No single vendor or implementer controls this corroboration.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The claimed type (rope) and metrics align: near-zero extractiveness, low suppression, negligible theater, moderate accessibility collapse (TCP dominance makes alternatives harder but not impossible), near-zero resistance (no one opposes the specification itself). The specification emerged from a coordination need, not extraction. The beneficiary set is broad and symmetric — implementers, applications, operators, users all gain. No party collects rents from the constraint's operation. The 'victims' mentioned in the kernel delta are not extraction victims; they are parties harmed when *others* violate the rope — a different structural category.
 *
 * PERSPECTIVAL GAP:
 *   The middlebox_realism_reading would compute a radically different seat map: middlebox vendors become beneficiaries (their modifications are 'real TCP'), end users become payers (middlebox breakage causes silent failures), and the specification becomes a snare (enforcing an ideal that doesn't match deployed reality). The optimization_latitude_reading would compute a third map: implementers gain latitude (beneficiary), but application developers lose guarantee precision (payer). The engine computes these divergences from the same structural data under different reading framings.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (implementers, applications, operators, users) sit at d ≈ 0.0–0.2: the constraint subsidizes them by providing a coordination good they could not produce alone. The excluded middlebox vendors sit at d ≈ 0.8–0.9: the constraint's logic treats their core business (in-path modification) as violation. But this is not extraction — it is the coordination function drawing a boundary. The agenda setters (IETF maintainers) sit at d ≈ 0.1: they bear maintenance costs but gain legitimacy and standards authority. No seat experiences high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interoperability of independent implementations) remains live. The specification has not atrophied into a piton — it continues to do coordination work. It has not become a snare — no party extracts from it. It has not become a tangled rope — there is no asymmetric extraction layered on the coordination. It remains a rope, albeit one under practice_drift pressure from middlebox deployments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strict_invariance_reading a distinct constraint with its own ε, or a framing of the same constraint as the sibling readings?',
    'Apply the ε-invariance test: if measuring the constraint under this reading yields ε ≈ 0.03 (coordination cost) while the middlebox_realism_reading yields ε ≈ 0.4 (extraction by middlebox vendors from endpoints), they are different constraints. The test confirms distinct ε values — this is a distinct constraint story.',
    'Confirms this JSON represents a valid ε-invariant constraint story per DP-001. The sibling readings require their own JSON files with their own ε, stakeholders, and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this reading instantiates a distinct constraint with ε ≈ 0.03').

omega_variable(
    middlebox_violation_vs_adaptation,
    'Are middlebox modifications specification violations (this reading) or legitimate adaptations that define the real protocol (middlebox_realism_reading)?',
    'Empirical: measure the fraction of TCP connections that traverse at least one modifying middlebox, and the fraction of those modifications that cause observable interoperability failure. If modification is near-universal and mostly benign, the middlebox_realism_reading gains empirical ground. If modification is rare or failure-prone, this reading''s violation framing holds.',
    'If modifications are universal and benign, this reading''s suppression metric understates the constraint''s effective suppression (it suppresses the *actual* protocol). If modifications are failure-prone, this reading''s coordination function is vindicated and the middlebox_realism_reading describes a degraded state, not a valid alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_violation_vs_adaptation, empirical, 'Whether middlebox modifications are violations or the real protocol').

omega_variable(
    optimization_latitude_boundary,
    'Where is the boundary between permissible optimization (optimization_latitude_reading) and specification violation (this reading)?',
    'Catalog specific implementation behaviors: RACK loss detection, TLP, BBR congestion control, SYN cookie variations, timestamp options. Determine which are explicitly permitted by RFC 9293 (or its bis successors) and which rely on ''semantic bounds'' not in the invariant state machine. The boundary is where implementation choice affects on-wire interoperability with other compliant stacks.',
    'A wide latitude boundary makes this reading''s ''zero tolerance'' claim empirically false (implementations already vary widely without breaking interoperability). A narrow boundary vindicates this reading''s strictness. The boundary location determines whether optimization_latitude_reading coexists_with or forecloses this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_latitude_boundary, conceptual, 'Where permissible optimization ends and violation begins').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_strict_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.02).
narrative_ontology:measurement(rfc9293_strict_tr_t1989, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1989, 0.03).
narrative_ontology:measurement(rfc9293_strict_tr_t1999, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1999, 0.04).
narrative_ontology:measurement(rfc9293_strict_tr_t2009, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(rfc9293_strict_tr_t2014, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2014, 0.05).
narrative_ontology:measurement(rfc9293_strict_tr_t2024, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9293_strict_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.02).
narrative_ontology:measurement(rfc9293_strict_be_t1989, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1989, 0.02).
narrative_ontology:measurement(rfc9293_strict_be_t1999, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1999, 0.03).
narrative_ontology:measurement(rfc9293_strict_be_t2009, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2009, 0.03).
narrative_ontology:measurement(rfc9293_strict_be_t2014, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2014, 0.03).
narrative_ontology:measurement(rfc9293_strict_be_t2024, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2024, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_strict_su_t1981, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1981, 0.05).
narrative_ontology:measurement(rfc9293_strict_su_t1989, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1989, 0.1).
narrative_ontology:measurement(rfc9293_strict_su_t1999, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1999, 0.12).
narrative_ontology:measurement(rfc9293_strict_su_t2009, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2009, 0.15).
narrative_ontology:measurement(rfc9293_strict_su_t2014, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2014, 0.15).
narrative_ontology:measurement(rfc9293_strict_su_t2024, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, quic_transport_protocol).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, tcp_congestion_control_algorithms).

% DUAL FORMULATION NOTE:
% This constraint is the strict_invariance_reading of the rfc9293_tcp_specification kernel. The kernel decomposes into three constraint stories with distinct ε values: this reading (ε ≈ 0.03, rope), optimization_latitude_reading (ε ≈ 0.15, tangled_rope — coordination with implementation latitude extraction), and middlebox_realism_reading (ε ≈ 0.4, snare — middlebox vendors extract from endpoints via in-path modification). The ε-invariance principle requires separate stories; the label 'TCP specification' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__strict_invariance_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
