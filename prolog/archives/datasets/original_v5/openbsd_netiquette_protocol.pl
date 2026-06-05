% ============================================================================
% CONSTRAINT STORY: openbsd_netiquette_protocol
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "OpenBSD’s Guide to Netiquette" by cafebedouin.org
% ============================================================================

:- module(constraint_openbsd_netiquette_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: openbsd_netiquette_protocol
 * human_readable: OpenBSD Netiquette Protocol
 * domain: technological/social
 * temporal_scope: Contemporary / Digital Era
 * spatial_scope: Global / Online Mailing Lists
 * * SUMMARY:
 * This constraint represents a distillation of rigorous online communication standards, 
 * originally for OpenBSD mailing lists, that prioritize content clarity and technical 
 * preparation over formatting and social convenience. It mandates that 
 * contributors "do their homework" and strip away formatting distractions to move 
 * discussions forward efficiently.
 * * KEY AGENTS:
 * - The Contributor: The individual seeking to join the discussion who must 
 * adhere to strict "plain text" and "homework" requirements.
 * - The Community Maintainer: The institutional authority that enforces topic 
 * relevance and "homework" status.
 * - The Newcomer (Novice): An agent who risks social liquidation (being ignored 
 * or rebuked) if they fail to provide relevant information.
 * * NARRATIVE ARC:
 * Communication is framed as a functional "battle" for clarity. The 
 * protocol acts as a "Rope" for the community to coordinate technical progress 
 * but functions as a "Snare" for those who attempt to "vent" or "be lazy" 
 * without preparation.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(openbsd_netiquette_protocol, 0, 10).
narrative_ontology:constraint_claim([openbsd_netiquette_protocol], [informational_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Moderate-Low (0.3). It extracts a "homework tax" from the user 
% to preserve the collective bandwidth of the group.
domain_priors:base_extractiveness(openbsd_netiquette_protocol, 0.3).

% Suppression score (0.0-1.0)
% Rationale: Moderate-High (0.6). It explicitly suppresses rich-text formatting, 
% off-topic banter, and "irrelevant signatures" to focus on content.
domain_priors:suppression_score(openbsd_netiquette_protocol, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, extractiveness, 0.3).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, suppression_requirement, 0.6).

% Enforcement requirements
% Requires active enforcement by the mailing list moderators and community.
domain_priors:requires_active_enforcement(openbsd_netiquette_protocol).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(openbsd_netiquette_protocol, technical_clarity). % The goal of the discussion
constraint_victim(openbsd_netiquette_protocol, casual_expression). % Those who prefer "lazy" or formatted chat

% Metrics required for Section 1 of the Executive Summary
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PREPARED CONTRIBUTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "do homework" and format text)
   WHEN: biographical (Long-term career/participation in technical communities)
   WHERE: mobile (Can navigate across different technical lists)
   SCOPE: global (Applies to "any kind of communication and for life")
   
   WHY THIS CLASSIFICATION:
   For the disciplined user, the protocol is a "Rope"—a functional coordination 
   mechanism that ensures their "important information" is received and respected 
   without "formatting getting in the way".
   
   NARRATIVE EVIDENCE:
   "It’s good advice for any kind of communication and for life... Help others 
   to understand".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    openbsd_netiquette_protocol,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNPREPARED NOVICE - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Target of "homework" enforcement)
   WHEN: immediate (Short-term failure to get a response/help)
   WHERE: trapped (Bounded by the "standard of doing" required by the list)
   SCOPE: local (The specific mailing list interaction)
   
   WHY THIS CLASSIFICATION:
   For the person who hasn't "done their homework," the protocol is a "Snare." 
   The community's "standard of excellence" extracts their social standing and 
   strangles their request if it doesn't meet the "plain text" or "useful 
   subject" criteria.
   
   NARRATIVE EVIDENCE:
   "Do your homework before writing... Include important information... Trim out 
   anything that does not move the discussion forward".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    openbsd_netiquette_protocol,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(openbsd_netiquette_protocol, E),
    E > 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TECHNICAL MAINTAINER - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over the mailing list page)
   WHEN: civilizational (The enduring "physics" of technical discourse)
   WHERE: analytical (Observer of what "moves the discussion forward")
   SCOPE: global (Standards for "communicating online")
   
   WHY THIS CLASSIFICATION:
   To the maintainer, netiquette is a "Mountain"—an unchangeable natural law of 
   productive communities. Without these fixed "physics" (staying on topic, 
   respecting differences), the system would collapse into "nonsense".
   
   NARRATIVE EVIDENCE:
   "Using only plain text is extreme... But, the idea that formatting should 
   not get in the way of content is good".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    openbsd_netiquette_protocol,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(openbsd_netiquette_protocol_tests).

test(multi_perspective_netiquette) :-
    % Contributor (Moderate) sees Rope
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, Type1, context(individual_moderate, biographical, mobile, global)),
    % Novice (Powerless) sees Snare
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, Type2, context(powerless, immediate, trapped, local)),
    % Maintainer (Institutional) sees Mountain
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, Type3, context(institutional, civilizational, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_netiquette) :-
    % Shows that the unprepared (powerless) experience higher extraction of time/social standing
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(openbsd_netiquette_protocol, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(openbsd_netiquette_protocol, ContextModerate, E2),
    E1 > E2.

test(time_immutability_homework) :-
    % In the immediate horizon, "doing homework" is an unchangeable Mountain (fact)
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(openbsd_netiquette_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.3):
 * Reasoning: I chose a moderate-low score because while the protocol 
 * extracts "homework" labor, it is presented as a beneficial tool for 
 * clarity. 
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the Contributor (Rope) who gains clarity, with the Novice 
 * (Snare) who is "trapped" by the expectation of preparation.
 * * 3. CLASSIFICATION RATIONALE:
 * Maintainer -> Mountain: The source frames the rules as "excellent" 
 * distillations of necessary reality.
 * * 4. AMBIGUITIES:
 * The text notes plain text is "extreme outside of email" but still 
 * "good advice". I resolved this by keeping the scope 
 * biographical/global for the general advice but local/trapped for 
 * the specific enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    netiquette_barrier_intent,
    "Is the 'homework' requirement a functional necessity for clarity (Mountain) or an elitist tool for gatekeeping/exclusion (Snare)?",
    resolution_mechanism("Audit of community growth vs. problem-resolution speed in 'high-homework' vs 'low-homework' lists"),
    impact("If necessity: Technical Mountain. If gatekeeping: Social Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Modern "Sanitized" UI / Rich Text
 * Viability: Standard in 95% of digital communication.
 * Suppression: Explicitly rejected by OpenBSD as "getting in the way of content".
 * * ALTERNATIVE 2: Unfiltered Social Ventilation (Chaos)
 * Viability: Common in casual groups.
 * Suppression: Rejected in favor of "moving the discussion forward".
 * * CONCLUSION:
 * The existence of Alternative 1 (Rich Text) makes the OpenBSD protocol a 
 * definitive "Rope" for technical coordination, as it rejects the 
 * "Snare" of visual clutter in favor of informational speed.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_openbsd_netiquette_protocol].
% Multi-perspective: ?- constraint_indexing:multi_index_report(openbsd_netiquette_protocol).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
