% ============================================================================
% CONSTRAINT STORY: tcp_rfc9293_interoperability
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: RFC 9293 - Transmission Control Protocol (TCP)
% ============================================================================

:- module(constraint_tcp_rfc9293_interoperability, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: tcp_rfc9293_interoperability
 * human_readable: TCP Interoperability & Reliability Requirements
 * domain: technological
 * temporal_scope: Decades (Modern Internet era)
 * spatial_scope: Global
 * 
 * SUMMARY:
 * RFC 9293 codifies the requirements for TCP. While often perceived as a 
 * "natural law" of the internet (a Mountain), it is actually a strictly 
 * enforced coordination mechanism. The "interoperability" requirement 
 * ensures that any implementation MUST follow the state machine and 
 * header rules, or be actively "cut" from the network via Reset (RST) segments.
 * 
 * KEY AGENTS:
 * - IETF (Institutional): The rule-maker, views it as a coordination tool.
 * - Application Developer (Individual Moderate): Must implement the "MUSTs", experiences it as a constraint.
 * - Network Application (Individual Powerless): Trapped by the host's stack logic.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(tcp_rfc9293_interoperability, 0, 10).
narrative_ontology:constraint_claim(tcp_rfc9293_interoperability, rope).
narrative_ontology:human_readable(tcp_rfc9293_interoperability, "TCP Interoperability & Reliability Requirements").
narrative_ontology:topic_domain(tcp_rfc9293_interoperability, "technological").

% Base extractiveness: 0.2. 
% (Header overhead, TCB memory, and processing "tax" for reliability).
domain_priors:base_extractiveness(tcp_rfc9293_interoperability, 0.2).

% Suppression: 0.7. 
% High suppression of alternative behaviors. Middleboxes and stacks 
% actively drop or reset non-conformant packets.
domain_priors:suppression_score(tcp_rfc9293_interoperability, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(tcp_rfc9293_interoperability, extractiveness, 0.2).
narrative_ontology:constraint_metric(tcp_rfc9293_interoperability, suppression_requirement, 0.7).

% Active Enforcement: YES.
% The protocol enforces itself through state-mismatch RSTs and timeouts.
domain_priors:requires_active_enforcement(tcp_rfc9293_interoperability).

% Beneficiaries: All internet users (reliable connections).
narrative_ontology:constraint_beneficiary(tcp_rfc9293_interoperability, global).
% Victims: None (overhead is a design trade-off, not targeted extraction).
narrative_ontology:constraint_victim(tcp_rfc9293_interoperability, none).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: NETWORK APPLICATION - Snare
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate
   WHERE: trapped
   
   WHY: For an application, TCP is a Snare. It cannot change the 
   3-way handshake or the TIME-WAIT delay. It is "trapped" within the 
   OS implementation's strict adherence to RFC 9293. If the app tries 
   to ignore the state machine, the connection is killed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_rfc9293_interoperability,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: IETF / PROTOCOL ARCHITECT - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: mobile
   
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: OS STACK DEVELOPER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: constrained
   
   WHY: The developer is bound by the legacy of the wire image, which feels
   like a Snare. However, they also benefit from the coordination of a clear
   spec, which acts as a Rope. The combination is a Tangled Rope: a useful
   tool that also severely constrains innovation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tcp_rfc9293_interoperability,
    tangled_rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tcp_interop_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_rfc9293_interoperability, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(tcp_interop_tests).

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
 * 1. RECLASSIFICATION: Changed the OS Stack Developer from 'Snare' to 'Tangled Rope'. This better captures the dual nature of the RFC: it's a constraint (snare) but also a valuable coordination standard (rope).
 * 
 * 2. PERSPECTIVE SELECTION: Maintained the three core perspectives (rule-maker, rule-follower, rule-user) as they clearly demonstrate the perspectival nature of a technical standard.
 * 
 * 3. AMBIGUITIES: The main ambiguity is whether the protocol's overhead is "extractive." I've classified it as a non-extractive trade-off for reliability, hence `victim(none)`.
 * 
 * 4. CONFIDENCE: High. The dynamics of technical standards like RFCs are a classic example of perspectival constraint classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

/**
 * No significant Omega variables identified. The system's behavior is
 * well-defined and its uncertainties are empirical (e.g., specific
 * implementation bugs) rather than fundamental.
 */

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * The primary alternative to TCP's rigid structure is to move transport
 * logic to the application layer over UDP.
 * 
 * ALTERNATIVE 1: UDP-based protocols (e.g., QUIC)
 *    Viability: Highly viable, now standardized as RFC 9000.
 *    Suppression: Not suppressed, but adoption is slowed by the massive installed base of TCP-optimized hardware and middleboxes. This demonstrates the "gravity" of a successful Rope.
 *    
 * CONCLUSION: The rise of QUIC shows that while the TCP constraint is powerful, it is not a 'Mountain'. It's a very successful 'Rope' whose own success creates the friction that makes it hard to replace, giving it Mountain-like characteristics for many agents.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/tcp_rfc9293_interoperability].
 * 2. Multi-perspective: ?- multi_index_report(tcp_rfc9293_interoperability).
 * 3. Run tests: ?- run_tests(tcp_interop_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Functional coordination mechanism — primarily substantive
domain_priors:theater_ratio(tcp_rfc9293_interoperability, 0.13).
narrative_ontology:constraint_metric(tcp_rfc9293_interoperability, theater_ratio, 0.13).

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (rfc9293_interoperability)
% ============================================================================

omega_variable(
    omega_tcp_ossification,
    "Will middlebox ossification permanently prevent TCP extension deployment, effectively converting the protocol from a Rope into a Mountain?",
    "Longitudinal measurement of TCP option negotiation success rates across diverse network paths.",
    "If ossified: TCP becomes an immutable Mountain. If extensible: Remains a living Rope that can adapt.",
    confidence_without_resolution(medium)
).
