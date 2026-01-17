% ============================================================================
% CONSTRAINT STORY: tcp_rfc9293_interoperability
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 Flash
% Source: RFC 9293 - Transmission Control Protocol (TCP)
% ============================================================================

:- module(tcp_rfc9293_interoperability, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for Python extraction
narrative_ontology:interval(tcp_rfc9293_interoperability, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tcp_rfc9293_interoperability
 * human_readable: TCP Interoperability & Reliability Requirements
 * domain: technological
 * temporal_scope: Decades (Modern Internet era)
 * * SUMMARY:
 * RFC 9293 codifies the requirements for TCP. While often perceived as a 
 * "natural law" of the internet (a Mountain), it is actually a strictly 
 * enforced coordination mechanism. The "interoperability" requirement 
 * ensures that any implementation MUST follow the state machine and 
 * header rules, or be actively "cut" from the network via Reset (RST) segments.
 * * KEY AGENTS:
 * - IETF (Institutional): The rule-maker.
 * - Application Developer (Moderate): Must implement the "MUSTs".
 * - Network Application (Powerless): Trapped by the host's stack logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Refining the "False Mountain")
   ========================================================================== */

% Base extractiveness: 0.2. 
% (Header overhead, TCB memory, and processing "tax" for reliability).
domain_priors:base_extractiveness(tcp_rfc9293_interoperability, 0.2).

% Suppression: 0.7. 
% High suppression of alternative behaviors. Middleboxes and stacks 
% actively drop or reset non-conformant packets.
domain_priors:suppression_score(tcp_rfc9293_interoperability, 0.7).

% Active Enforcement: YES.
% The protocol enforces itself through state-mismatch RSTs and timeouts.
domain_priors:requires_active_enforcement(tcp_rfc9293_interoperability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Network Application - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   EXIT: trapped
   
   WHY: For an application, TCP is a Noose. It cannot change the 
   3-way handshake or the TIME-WAIT delay. It is "trapped" within the 
   OS implementation's strict adherence to RFC 9293. If the app tries 
   to ignore the state machine, the connection is killed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_rfc9293_interoperability,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: IETF / Protocol Architect - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   EXIT: mobile
   
   WHY: To the IETF, the RFC is a Rope—a tool designed to tie disparate 
   systems together into a functional whole. They have the power to 
   update (Obsolete RFC 793) or extend (TCP Fast Open) the constraint.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_rfc9293_interoperability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: OS Stack Developer - Noose
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   EXIT: constrained
   
   WHY: The developer is "noosed" by the legacy of the wire image. 
   Even minor deviations from the state machine (Section 3.10) create 
   interoperability failures that are blamed on the developer.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_rfc9293_interoperability,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(tcp_interop_v2_tests).

test(perspectival_gap) :-
    % Validation of the "Rope/Noose" gap between IETF and User
    constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, noose, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, rope, context(agent_power(institutional), _, _, _)).

test(enforcement_check) :-
    % If active enforcement is required, the constraint cannot be a Mountain.
    domain_priors:requires_active_enforcement(tcp_rfc9293_interoperability),
    \+ constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, mountain, _).

:- end_tests(tcp_interop_v2_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================= */

/**
 * LLM REFINEMENT NOTES:
 * 1. Resolved "Type 1 False Mountain": 
 * Previous versions incorrectly labeled the protocol a "Mountain" for 
 * applications. While apps perceive it as unchangeable, the existence 
 * of RST mechanisms and IETF updates proves it is an institutionally 
 * maintained "Noose" or "Rope".
 * 2. Module Uniqueness:
 * Module name set to 'tcp_rfc9293_interoperability' to avoid 
 * collision with tax code test sets.
 * 3. Perspectival Coverage:
 * Provided required classifications for both individual_powerless 
 * and institutional agents.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE "POST-TCP" REALITY (QUIC):
 * UDP-based protocols like QUIC attempt to turn the TCP "Noose" back 
 * into a "Rope" for developers by moving the state machine into 
 * user-space. This allows for faster evolution, effectively 
 * shifting the exit_options from 'constrained' to 'mobile'.
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================

