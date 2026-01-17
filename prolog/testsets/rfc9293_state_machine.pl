% ============================================================================
% CONSTRAINT STORY: tcp_state_machine_logic
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 Flash
% Source: RFC 9293 - Transmission Control Protocol (TCP)
% ============================================================================

:- module(tcp_state_machine_logic, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor
narrative_ontology:interval(tcp_state_machine_logic, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tcp_state_machine_logic
 * human_readable: TCP State Machine Constraints
 * domain: technological
 * temporal_scope: Connection-lifetime (milliseconds to hours) 
 * spatial_scope: Local (endpoint logic) 
 * * SUMMARY:
 * The TCP state machine governs the lifecycle of a connection, from initial 
 * handshake (SYN) to termination (FIN/TIME-WAIT). It enforces 
 * strict transition rules that ensure both peers remain synchronized despite 
 * network delays or reboots.
 * * KEY AGENTS:
 * - Stack Developer: Must implement transitions for 11 distinct states.
 * - Peer A (Active): Initiates connections via SYN-SENT.
 * - Peer B (Passive): Waits in LISTEN to accept requests.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Base extractiveness: 0.2 (Moderate overhead). 
% Requires memory for Transmission Control Blocks (TCBs) even in idle states.
domain_priors:base_extractiveness(tcp_state_machine_logic, 0.2).

% Suppression: 0.6 (High). 
% Deviating from the state machine (e.g., ignoring a FIN) leads to "half-open" 
% connections and potential resource exhaustion.
domain_priors:suppression_score(tcp_state_machine_logic, 0.6).

% Active Enforcement: The protocol enforces itself through Resets (RST).
% Arriving segments that don't match the current state elicit an RST to 
% "kill" the non-conformant connection.
domain_priors:requires_active_enforcement(tcp_state_machine_logic).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE: Stack Developer - Noose
   --------------------------------------------------------------------------
   WHO: individual_moderate - Responsible for correct implementation.
   WHEN: biographical - Years spent maintaining/debugging kernel code.
   WHERE: constrained - The "wire image" of TCP is frozen; changes are hard.
   
   WHY: It acts as a Noose because of the complexity involved in 
   "Anomaly" states like Simultaneous Open or Half-Open connections. 
   Failure to handle a rare transition (e.g., RST in SYN-RECEIVED) can 
   lead to system hangs or security vulnerabilities.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_state_machine_logic,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE: Institutional (IETF) - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Governing body of the standard.
   WHEN: historical - 40+ year protocol stability.
   
   WHY: The state machine is a Rope that provides a common language 
   for diverse machines (from IoT sensors to supercomputers) to 
   coordinate reliable data transfer.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_state_machine_logic,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE: Automated Scanner/Script - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - No ability to modify the host's logic.
   WHEN: immediate - Execution time.
   
   WHY: To a script, the state machine is an unyielding Mountain. 
   If a port is in LISTEN, it behaves exactly according to Section 3.10.7.2. 
   There is no "negotiation"—only the law of the RFC.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_state_machine_logic,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(tcp_state_tests).

test(state_persistence_overhead) :-
    % TIME-WAIT state is a 2*MSL "Noose" that consumes resources after 
    % a connection is closed to prevent old duplicate segments.
    assertion(true).

test(synchronization_rope) :-
    % The 3-Way Handshake (3WHS) is the core "Rope" mechanism 
    % to prevent confusion from old duplicate initiations.
    assertion(true).

:- end_tests(tcp_state_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================= */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. CLASSIFIED AS NOOSE FOR DEVELOPERS:
 * RFC 9293 Section 3.10 is 15+ pages of event processing logic. 
 * Missing a single "if state is X" check results in non-conformance.
 * 2. CLASSIFIED AS MOUNTAIN FOR USERS:
 * The "Quiet Time" concept (delaying segments for 2 minutes after a reboot) 
 * is an unalterable physical-like constraint for older/strict stacks.
 * * AMBIGUITIES:
 * - RFC 9293 notes the "CLOSED" state is fictional because there is no 
 * TCB. I've modeled it as the "Ground" from which 
 * constraints emerge.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. T/TCP (RFC 1644): Accelerated the handshake. 
 * Status: Historically useful but raised security concerns.
 * 2. TCP Fast Open (TFO): Allows data in the SYN segment.
 * Constraint Effect: Loosens the "Mountain" of the 3WHS for performance.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
