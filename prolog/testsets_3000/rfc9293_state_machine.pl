% ============================================================================
% CONSTRAINT STORY: rfc9293_state_machine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: technological/protocol_specification
 *
 * SUMMARY:
 *   The TCP state machine defined in RFC 9293 governs the lifecycle of every
 *   TCP connection: SYN-RECEIVED → ESTABLISHED → FIN-WAIT-1 → FIN-WAIT-2 →
 *   TIME-WAIT → CLOSED. This constraint is invariant across all
 *   implementations, operating systems, and network topologies. It emerges
 *   from the mathematical structure of stateful connection negotiation over
 *   unreliable packet delivery, not from policy, convention, or institutional
 *   agreement. The state machine is an immutable logical structure — no
 *   alternative path exists from SYN to ESTABLISHED that maintains
 *   correctness. Deviating from the prescribed transitions results in
 *   immediate incompatibility with compliant peers. The constraint exhibits
 *   zero degrees of freedom for all agent positions: endpoints must implement
 *   it exactly, standards bodies can only codify it, and open-source
 *   communities converge on it through network effects. The theater ratio
 *   (0.15) reflects minimal performative content — the state machine is
 *   purely functional with no ritual or ceremonial overhead. The
 *   extractiveness (0.08) is extremely low because the constraint imposes no
 *   asymmetric burden: all agents face identical logical requirements
 *   regardless of power position.
 *
 * KEY AGENTS:
 *   - TCP Endpoint (Client): Logical actor (analytical/analytical) — bound by the state machine at the protocol level; no exit option
 *   - TCP Endpoint (Server): Logical actor (analytical/analytical) — bound by the state machine at the protocol level; no exit option
 *   - TCP Implementation (Stack): Logical actor (powerless/trapped) — must execute the state machine exactly or fail interoperability; zero freedom
 *   - Standards Body (IETF/RFC Authority): Institutional actor (institutional/arbitrage) — codifies the immutable structure; cannot negotiate or modify it
 *   - Open-Source Developer Community: Organized actor (organized/constrained) — implements the constraint; constrained by network effects and compatibility requirements
 *   - Analytical Observer (Formal Verification): Symbolic actor (analytical/analytical) — can prove the state machine's logical necessity and invariance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_state_machine, 0.08).
domain_priors:suppression_score(rfc9293_state_machine, 0.03).
domain_priors:theater_ratio(rfc9293_state_machine, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_state_machine, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_state_machine, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(rfc9293_state_machine, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_state_machine, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_state_machine, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_state_machine, mountain).
narrative_ontology:human_readable(rfc9293_state_machine, "TCP State Machine Constraints (RFC 9293)").
narrative_ontology:topic_domain(rfc9293_state_machine, "technological/protocol_specification").

domain_priors:emerges_naturally(rfc9293_state_machine).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTOCOL SPECIFICATION (MOUNTAIN) — The TCP state machine emerges from the mathematical requirements of stateful connection negotiation over unreliable packet delivery. The constraint is invariant across all implementations: SYN-SENT must precede ESTABLISHED; TIME-WAIT must follow FIN; transitions follow the state diagram deterministically. No degree of freedom exists — the state machine is an immutable logical structure, not a convention or policy choice.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENDPOINT IMPLEMENTATION (MOUNTAIN) — A TCP stack must implement the state machine exactly or fail interoperability testing. No alternative path exists from SYN-RECEIVED to ESTABLISHED except through the prescribed transitions. The constraint is absolute from the implementation perspective: deviation results in immediate packet rejection by compliant peers. The endpoint experiences zero degrees of freedom.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STANDARDS BODY (MOUNTAIN) — RFC 9293 codifies an already-immutable logical structure. The standards body cannot negotiate, compromise, or alter the fundamental state transitions without breaking compatibility with decades of deployed infrastructure. The constraint pre-exists the standard and is recovered by specification, not created by it. Authority over the document confers no authority over the underlying structure.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: OPEN-SOURCE COMMUNITY (MOUNTAIN) — Organized developers cannot implement alternative state machines and remain part of the TCP ecosystem. Every major TCP implementation (Linux kernel, BSD, Windows, Go runtime) converges on the same state diagram because deviation yields non-interoperability. The constraint is self-enforcing through network effects: deviate and your implementation is incompatible. No sunset clause exists — the structure is permanent as long as TCP is used.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(organized),
            time_horizon(generational),
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
 *   Extractiveness (0.08): Extremely low. The TCP state machine imposes identical constraints on all agents — no agent benefits at the expense of another. The constraint is purely structural, not distributive. There is no extraction because there is no asymmetry: endpoints, stacks, standards bodies all face the same logical necessity. The minimal non-zero value (0.08 vs 0.00) reflects that formal specification carries small institutional overhead (RFC document maintenance, version control, obsolescence procedures), but this is negligible relative to the constraint's logical content. Suppression (0.03): Minimal. Agents cannot avoid the state machine, but suppression is not meaningful because there are no alternatives to suppress — the state machine is not competing with other mechanisms; it is the only mechanism that solves the problem of stateful connection negotiation. Agents experience the constraint as mathematical necessity, not as coercion. Theater ratio (0.15): Very low. The state machine has minimal performative content — it is purely functional. The small non-zero value (0.15 vs 0.00) reflects that RFC specification documents contain explanatory prose and rationale sections that exceed the bare logical minimum, but the core specification is direct and functional.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives unanimously classify the constraint as Mountain. There is no perspectival gap because the constraint exhibits zero degrees of freedom across all observation contexts. The endpoint, the standards body, the developer community, and the formal logician all see the same immutable structure. This consensus is itself evidence of the mountain classification: constraints that appear identical from radically different perspectives (powerless vs institutional, immediate vs civilizational, trapped vs arbitrage) are typically natural laws. The absence of a perspectival gap is the hallmark of universality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for this constraint because there are no beneficiaries or victims — the constraint is purely structural, not distributive. All agents occupy identical logical positions relative to the state machine. The sigmoid function f(d) does not apply because d does not exist: there is no extraction flow from any agent to any other. The constraint is invariant under all permutations of agent power, time horizon, exit options, and spatial scope because the logical structure is universal. This is the defining characteristic of a mountain: it imposes identical constraints on all agents, with no asymmetry in burden or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE MOUNTAIN: The TCP state machine resolves mandatrophy trivially because it exhibits zero extraction. There is no risk of mislabeling coordination as extraction or vice versa: the constraint is neither. It is purely structural, imposing identical logical requirements on all agents without creating asymmetric benefit or burden. All perspectives converge on Mountain classification, confirming the absence of extractive or coordinative dynamics. The constraint serves as a baseline reference for what a true natural law looks like in the technological domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optional_rfc_extensions,
    'Do optional TCP extensions (TCP_USER_TIMEOUT, TCP_KEEPALIVE, SACK, window scaling) represent genuine extensions to the state machine or merely additional transitions that preserve the core constraint?',
    'Formal verification showing that all RFC extensions map onto or extend the RFC 9293 core without contradicting any state transition rules; identification of any extension that would require modification to the base state diagram',
    'If extensions preserve the core: mountain classification confirmed across temporal horizons. If any extension fundamentally alters state transitions: constraint decomposes into multiple stories (core mountain + extension-specific rope/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optional_rfc_extensions, empirical, 'Whether TCP extensions alter or merely extend the base state machine').

omega_variable(
    congestion_control_decoupling,
    'Are TCP congestion control algorithms (Reno, CUBIC, BBR) logically decoupled from the state machine constraint, or do they impose additional hidden state transitions that constrain the state machine''s freedom?',
    'State diagram analysis showing whether congestion control modifies legal state transitions or only affects timing/data-delivery within transitions; identification of any congestion state (e.g., timeout recovery) that would add vertices to the core graph',
    'If fully decoupled: TCP state machine is pure mountain (zero extraction, universal). If coupled: congestion control is a separate tangled_rope constraint (extraction via window scaling, retransmission delays) layered onto the mountain. Story should decompose into rfc9293_state_machine (mountain) and tcp_congestion_control (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_control_decoupling, conceptual, 'Whether congestion control is decoupled from state transitions').

omega_variable(
    middlebox_constraint_interaction,
    'Do network middleboxes (firewalls, NAT, proxies) that intercept or modify TCP state transitions constitute violations of the RFC 9293 constraint or legitimate extensions to it?',
    'Analysis of middlebox behavior: do they replay compliant state transitions transparently, or do they rewrite headers/sequences in ways that modify the endpoint state machine logic? Examination of whether middleboxes create new failure modes not predicted by the state diagram.',
    'If transparent replay: mountain constraint holds end-to-end, middleboxes are orthogonal. If rewriting occurs: a secondary constraint (middlebox_interception) emerges at tangled_rope or snare level, decomposing the network into endpoint mountains + middlebox extraction. Story should add network.affects_constraints link to middlebox_interception if high confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_constraint_interaction, empirical, 'Whether middleboxes violate or legitimately extend the state machine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_state_machine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_sm_tr_t0, rfc9293_state_machine, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tcp_sm_tr_t25, rfc9293_state_machine, theater_ratio, 25, 0.14).
narrative_ontology:measurement(tcp_sm_tr_t50, rfc9293_state_machine, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(tcp_sm_be_t0, rfc9293_state_machine, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(tcp_sm_be_t25, rfc9293_state_machine, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(tcp_sm_be_t50, rfc9293_state_machine, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_state_machine, information_standard).
narrative_ontology:affects_constraint(rfc9293_state_machine, tcp_congestion_control).
narrative_ontology:affects_constraint(rfc9293_state_machine, middlebox_interception).

% DUAL FORMULATION NOTE:
% RFC 9293 state machine is a pure structural constraint. Congestion control algorithms (CUBIC, BBR) and middlebox interception represent separate constraints that operate *within* the state machine's framework but add extraction or hybrid dynamics. The decomposition is: (1) rfc9293_state_machine (mountain, ε=0.08), (2) tcp_congestion_control (tangled_rope, ε≈0.35, because window-scaling creates asymmetric information advantage), (3) middlebox_interception (snare, ε≈0.52, because NAT/firewall state rewriting captures traffic flow control). All three are linked: the state machine is upstream; the others operate within it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
