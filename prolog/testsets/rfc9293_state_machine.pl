% ============================================================================
% CONSTRAINT STORY: rfc9293_state_machine
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: RFC 9293 - Transmission Control Protocol (TCP)
% ============================================================================

:- module(constraint_rfc9293_state_machine, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: rfc9293_state_machine
 * human_readable: TCP State Machine Constraints
 * domain: technological/institutional
 * temporal_scope: Connection-lifetime (milliseconds to hours) 
 * spatial_scope: Local (endpoint logic) 
 * 
 * SUMMARY:
 * The TCP state machine governs the lifecycle of a connection, from initial 
 * handshake (SYN) to termination (FIN/TIME-WAIT). It enforces 
 * strict transition rules that ensure both peers remain synchronized despite 
 * network delays or reboots.
 * 
 * KEY AGENTS:
 * - Stack Developer (Individual Moderate): Must implement transitions for 11 distinct states.
 * - IETF (Institutional): The governing body of the standard.
 * - Automated Scanner/Script (Individual Powerless): No ability to modify the host's logic.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(rfc9293_state_machine, 0, 10).
narrative_ontology:constraint_claim(rfc9293_state_machine, mountain).

% Base extractiveness: 0.2 (Moderate overhead). 
% Requires memory for Transmission Control Blocks (TCBs) even in idle states.
domain_priors:base_extractiveness(rfc9293_state_machine, 0.2).

% Suppression: 0.6 (High). 
% Deviating from the state machine (e.g., ignoring a FIN) leads to "half-open" 
% connections and potential resource exhaustion.
domain_priors:suppression_score(rfc9293_state_machine, 0.6).

% Enforcement: The protocol enforces itself through Resets (RST).
% Arriving segments that don't match the current state elicit an RST to 
% "kill" the non-conformant connection.
domain_priors:requires_active_enforcement(rfc9293_state_machine).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rfc9293_state_machine, protocol_stability).
constraint_victim(rfc9293_state_machine, developer_effort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: AUTOMATED SCANNER/SCRIPT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless (No ability to modify the host's logic)
   WHEN: immediate (Execution time)
   WHERE: trapped (By the deterministic response of the target host)
   
   WHY THIS CLASSIFICATION:
   To a script, the state machine is an unyielding 'Mountain'. 
   If a port is in LISTEN, it behaves exactly according to Section 3.10.7.2. 
   There is no "negotiation"—only the law of the RFC, a fixed and predictable landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rfc9293_state_machine,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: INSTITUTIONAL (IETF) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Governing body of the standard)
   WHEN: historical (40+ year protocol stability)
   WHERE: mobile (Can update and deprecate standards over time)
   
   WHY THIS CLASSIFICATION:
   For the IETF, the state machine is a 'Rope' that provides a common language 
   for diverse machines (from IoT sensors to supercomputers) to 
   coordinate reliable data transfer across the global internet. It is a tool for
   ensuring interoperability and stability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rfc9293_state_machine,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: STACK DEVELOPER - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Responsible for correct implementation)
   WHEN: biographical (Years spent maintaining/debugging kernel code)
   WHERE: constrained (The "wire image" of TCP is frozen; changes are hard)
   
   WHY THIS CLASSIFICATION:
   For a developer, the state machine acts as a 'Snare' because of the complexity
   involved in "Anomaly" states like Simultaneous Open or Half-Open connections. 
   Failure to handle a rare transition can lead to system hangs or security
   vulnerabilities, strangling the developer with intricate, high-stakes details.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rfc9293_state_machine,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rfc9293_state_machine_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rfc9293_state_machine, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_state_machine, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_state_machine, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(rfc9293_state_machine_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. CLASSIFICATION RATIONALE:
 *    - Automated Scanner (Mountain): An immutable set of rules to probe.
 *    - IETF (Rope): A tool for global interoperability.
 *    - Stack Developer (Snare): A complex, high-stakes implementation challenge.
 * 
 * 2. CORE INSIGHT: The TCP state machine is a 'Mountain' of logical necessity
 *    that, when codified by an institution (IETF), becomes a 'Rope' for global
 *    coordination. However, for those tasked with implementing its intricate
 *    details, it can feel like a 'Snare' of complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the conceptual versus actual existence of the 'CLOSED' state.
 */

omega_variable(
    closed_state_reality,
    "Is the TCP 'CLOSED' state a true non-existent state (a conceptual 'Mountain') or a simplification masking underlying resource release complexities (a procedural 'Rope')?",
    resolution_mechanism("Detailed analysis of operating system kernel implementations of TCP termination routines and associated resource management across various platforms."),
    impact("If Mountain: The state machine model is fundamentally complete. If Rope: Resource management beyond the state machine's explicit transitions is critical for robustness."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: TCP Fast Open (TFO)
 *    Viability: Allows application data to be sent in the initial SYN segment, improving latency for short-lived connections.
 *    Suppression: Faces challenges with network middlebox interference and security considerations for initial data, limiting its widespread adoption.
 *
 * CONCLUSION:
 * The core TCP state machine remains the dominant 'Rope' for reliable internet
 * communication. While alternatives exist to optimize specific use cases,
 * their suppression due to security and deployment complexities reinforces
 * the enduring necessity of the foundational state machine.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/rfc9293_state_machine].
 * 2. Multi-perspective: ?- multi_index_report(rfc9293_state_machine).
 * 3. Run tests: ?- run_tests(rfc9293_state_machine_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */