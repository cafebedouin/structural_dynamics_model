% ============================================================================
% CONSTRAINT STORY: rfc9293_state_machine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_state_machine, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_state_machine
 *   human_readable: TCP State Machine Constraints (RFC 9293)
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The TCP state machine, as defined in RFC 9293 and its predecessors (RFC
 *   793), is a finite automaton governing the lifecycle of a Transmission
 *   Control Protocol connection from initial handshake (SYN, SYN-ACK) through
 *   data exchange and termination (FIN, FIN-ACK, TIME-WAIT). The constraint
 *   is presented here as a candidate mountain: a logical and physical
 *   structure that emerges necessarily from the problem of providing
 *   reliable, ordered delivery over unreliable packet-switched networks.
 *   Every TCP implementation — in Linux, Windows, BSD, embedded systems —
 *   must implement functionally equivalent state machines or fail to
 *   interoperate. The state sequence is not arbitrary: reordering states or
 *   omitting transitions breaks the guarantees (no data loss, no duplication,
 *   in-order delivery) that TCP provides to applications. This analysis
 *   examines whether the state machine is a natural law (mountain), a
 *   coordinated standard (rope), or a sunset-candidate (scaffold as QUIC
 *   adoption accelerates).
 *
 * KEY AGENTS:
 *   - Endpoint Implementation: Powerless/trapped — must implement states correctly or break protocol guarantees
 *   - Application Developer: Moderate/constrained — cannot violate state transitions without encountering undefined behavior or connection failures
 *   - Standardization Body (IETF/IANA): Institutional/arbitrage — stabilizes the standard; benefits all implementers equally
 *   - Analytical Observer: Civilizational/analytical — examines whether the state machine is logically necessary or historically contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_state_machine, 0.08).
domain_priors:suppression_score(rfc9293_state_machine, 0.02).
domain_priors:theater_ratio(rfc9293_state_machine, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_state_machine, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_state_machine, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(rfc9293_state_machine, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_state_machine, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_state_machine, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_state_machine, mountain).
narrative_ontology:human_readable(rfc9293_state_machine, "TCP State Machine Constraints (RFC 9293)").
narrative_ontology:topic_domain(rfc9293_state_machine, "technological/institutional").

domain_priors:emerges_naturally(rfc9293_state_machine).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDPOINT IMPLEMENTATION (MOUNTAIN) — TCP implementations are logically trapped in state transitions defined by the RFC. No endpoint can deviate from valid state sequences without breaking the protocol. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.09. The constraint appears as an immutable logical law from the implementation perspective.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a formal protocol perspective, the TCP state machine is a minimal finite automaton (13 states + transition rules) that solves the reliable ordered delivery problem over unreliable packet-switched networks. The state sequence is logically necessary, not contingent. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. This is a natural law of network protocols.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARDIZATION BODY (MOUNTAIN) — IANA/IETF sees the state machine as a fixed reference specification. Deviations require new RFCs (e.g., RFC 9293 obsoletes RFC 793). The standard's role is to document what works, not to enforce change. From the standardizer's view, the constraint is an emergent property of decades of accumulated implementation experience — stable enough to encode as immutable law. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Negative extraction: stabilizing the standard benefits all implementers equally.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: APPLICATION DEVELOPER (MOUNTAIN) — Developers cannot build reliable networked applications by ignoring the TCP state machine. Attempts to violate it result in undefined behavior, connection resets, or data loss. The constraint is experienced as a technical law — you can study it, work within it, but not around it. d≈0.80, f(d)≈1.13, σ=1.0 → χ≈0.09. High power deficit makes the mountain seem restrictive, but the restriction solves a real coordination problem (reliable delivery).
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_state_machine_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(rfc9293_state_machine, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_state_machine, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rfc9293_state_machine, ExtMetricName, E),
    domain_priors:suppression_score(rfc9293_state_machine, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rfc9293_state_machine),
    narrative_ontology:constraint_metric(rfc9293_state_machine, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rfc9293_state_machine, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rfc9293_state_machine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The TCP state machine imposes no asymmetric cost on any implementer. All endpoints follow the same rules. The constraint does not extract resources from one group for another's benefit — it enables coordination that benefits all parties symmetrically. Suppression (0.02): Minimal. While implementations are 'trapped' in valid state sequences, this is not suppression in the DR sense — there is no hidden alternative foreclosed by coercion. The 'trap' is logical, not coercive: invalid state transitions cause immediate, observable failure (connection reset, data loss). Transparency is maximal. Theater ratio (0.15): Very low. The state machine is functionally pure. Each state and transition serves a specific purpose in the reliable delivery protocol. There is minimal performative content — debugging tools (tcpdump, netstat) expose the actual state directly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint presents minimal perspectival gap across all four observer positions. All perspectives classify as mountain because all stakeholders experience the same logical necessity. Endpoint implementations see a logical law (cannot deviate). Developers see a technical law (violation causes failure). The standardization body sees an emergent stable property of the problem (document what works). The analytical observer sees a mathematical necessity (finite automaton for reliable delivery). The constraint is perspectively invariant — a hallmark of true mountains. This uniformity is evidence FOR the mountain classification: if all stakeholders experienced different constraint types, it would suggest the 'constraint' was actually multiple distinct phenomena (per the ε-invariance principle). Here, the single ε (0.08) and uniform classification across all perspectives indicate a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives have minimal directionality because there is no extraction. Endpoint implementation: d≈0.95 (trapped in logic) but f(d)≈1.42 × 0.08 × 0.8 (local scope) → χ≈0.09. Beneficiary/victim distinction does not apply: all implementers are equally constrained and equally enabled. Application developer: d≈0.80 (constrained but not trapped) → χ≈0.09. Standardization body: d≈0.05 (beneficiary in the sense that it stabilizes the standard and benefits all) → χ≈-0.01. Analytical observer: d≈0.72 (neutral observer) → χ≈0.09. The consistency of χ across different d values — all around 0.08-0.09 — is characteristic of mountains: the constraint's effective extractiveness is stable regardless of which perspective you occupy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_variance,
    'Do TCP implementations exhibit measurable deviations from RFC 9293 state machine in practice (Linux, Windows, BSD kernel variants)?',
    'Packet capture and state tracking across implementations; comparison of actual connection lifecycles against RFC specification',
    'If variance < 0.5%: mountain classification confirmed. If variance > 2%: constraint may be rope (coordinated approximation) not mountain (strict law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_variance, empirical, 'Degree of implementation variance from RFC specification').

omega_variable(
    necessity_vs_convention,
    'Are the 13 states and their transitions logically necessary for reliable ordered delivery, or are they conventional choices that could be replaced by alternative finite automata with equivalent guarantees?',
    'Formal proof that alternative state machines cannot achieve TCP guarantees; analysis of whether specific state transitions (e.g., TIME-WAIT) are essential or historical artifacts',
    'If necessary: mountain classification stands. If conventional: constraint is rope (coordinated standard) optimized by history but not physically immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_convention, conceptual, 'Whether state machine is logically necessary or conventionally optimal').

omega_variable(
    quic_replacement_status,
    'Does QUIC provide equivalent reliability guarantees with a structurally simpler state machine, suggesting RFC 9293 is not the minimum for solving the coordination problem?',
    'Formal equivalence proof of TCP vs QUIC state spaces; analysis of whether QUIC''s complexity is lower or merely distributed differently',
    'If QUIC simpler: TCP state machine becomes scaffold (sunset clause: migration to QUIC) rather than mountain. If QUIC equally complex: TCP is the natural solution for the problem class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quic_replacement_status, empirical, 'Structural simplicity of QUIC relative to TCP').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_state_machine, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_tr_t0, rfc9293_state_machine, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rfc9293_tr_t15, rfc9293_state_machine, theater_ratio, 15, 0.15).
narrative_ontology:measurement(rfc9293_tr_t30, rfc9293_state_machine, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(rfc9293_be_t0, rfc9293_state_machine, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(rfc9293_be_t15, rfc9293_state_machine, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(rfc9293_be_t30, rfc9293_state_machine, base_extractiveness, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_state_machine, enforcement_mechanism).
narrative_ontology:affects_constraint(rfc9293_state_machine, congestion_control_algorithms).
narrative_ontology:affects_constraint(rfc9293_state_machine, tcp_timeout_exponential_backoff).
narrative_ontology:affects_constraint(rfc9293_state_machine, retransmission_queue_management).

% DUAL FORMULATION NOTE:
% The TCP state machine is upstream of all TCP-dependent constraints (congestion control, timeout handling, retransmission logic). These downstream constraints operate within the state space defined by RFC 9293. If the state machine is truly a mountain, these dependents are constrained by natural law. If the state machine is actually a rope (coordinated convention), the dependents are contingent on that convention and may be revisable via QUIC migration or next-generation protocols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
