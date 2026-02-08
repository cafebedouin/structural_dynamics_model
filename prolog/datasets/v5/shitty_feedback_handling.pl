% ============================================================================
% CONSTRAINT STORY: shitty_feedback_handling
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Mandy Brown / cafebedouin.org, "Accept, Reframe, Or Reject"
% ============================================================================

:- module(constraint_shitty_feedback_handling, []).

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
 * 
 * constraint_id: shitty_feedback_handling
 * human_readable: The Protocol for Handling Shitty Feedback
 * domain: social/professional
 * temporal_scope: Contemporary / Ongoing
 * spatial_scope: Organizational and Interpersonal Environments
 * 
 * SUMMARY:
 * "Shitty feedback" is a ubiquitous byproduct of social systems where feedback skills are 
 * rare. The constraint identifies the three actions available to a receiver—Accept, 
 * Reframe, or Reject—to process this external noise without losing agency.
 * 
 * KEY AGENTS:
 * - The Subordinate (Individual Powerless): Subject to a supervisor's evaluation.
 * - The Peer (Individual Moderate): Engages in feedback with a colleague of equal standing.
 * - The Corporation (Institutional): Implements a 360-degree feedback system.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(shitty_feedback_handling, 0, 10).
narrative_ontology:constraint_claim(shitty_feedback_handling, rope).

% Base extractiveness: Moderate (0.4)
% Rationale: Shitty feedback extracts emotional energy, time, and potentially 
% identity if the receiver feels forced to defend themselves or accept invalidity.
domain_priors:base_extractiveness(shitty_feedback_handling, 0.4).

% Suppression: Moderate (0.3)
% Rationale: The "natural" alternative of defensiveness is visible but actively 
% discouraged as a "rarely necessary" and ineffective reaction.
domain_priors:suppression_score(shitty_feedback_handling, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(shitty_feedback_handling, extractiveness, 0.4).
narrative_ontology:constraint_metric(shitty_feedback_handling, suppression_requirement, 0.3).

% Enforcement: Requires active mental maintenance by the receiver.
domain_priors:requires_active_enforcement(shitty_feedback_handling).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(shitty_feedback_handling, the_receiver). % Preserves agency
constraint_victim(shitty_feedback_handling, emotional_energy). % Cognitive load of processing

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SUBORDINATE - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Subject to supervisor's evaluation)
   WHEN: immediate (High-stakes professional feedback)
   WHERE: constrained (Must maintain employment relationship)
   
   WHY THIS CLASSIFICATION:
   In a power imbalance, feedback from a supervisor acts as a 'Snare'. The 
   'Reject' option is functionally removed, forcing a performative 'Accept' 
   response ("I’ll do better") to maintain one's job, regardless of feedback quality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shitty_feedback_handling,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SELF-AWARE PEER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Managing personal growth with a colleague)
   WHEN: biographical (Developing lifelong feedback skills)
   WHERE: mobile (Can choose to reframe or reject input from an equal)
   
   WHY THIS CLASSIFICATION:
   For an agent with exit options and self-awareness, the protocol is a 'Rope'—a 
   beneficial tool to coordinate with the external world's perspective without 
   being crushed by it, filtering noise to find value.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shitty_feedback_handling,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CORPORATION - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Implementing a 360-review system)
   WHEN: generational (Managing the talent pool over years)
   WHERE: arbitrage (Can use feedback data to make promotion/firing decisions)
   
   WHY THIS CLASSIFICATION:
   For the corporation, a 360-review system is a 'Tangled Rope'. It's a 'Rope'
   for coordinating performance data across the organization, but it becomes
   'Tangled' because the low quality of feedback generates noise, politics, and
   demotivates employees, creating an extractive drag on the system it's meant to improve.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shitty_feedback_handling,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(shitty_feedback_handling_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(shitty_feedback_handling, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(shitty_feedback_handling_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Corporation' to represent how feedback,
 *    even shitty feedback, is institutionalized as a 'Tangled Rope'—a tool for
 *    coordination that has negative, extractive side effects.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Subordinate (Snare): Power forces acceptance.
 *    - Peer (Rope): A filter for personal growth.
 *    - Corporation (Tangled Rope): A useful data-gathering tool that also creates organizational drag.
 * 
 * 3. EXTRACTIVENESS (0.4): This score reflects the cognitive and emotional load
 *    required to process worthless or malicious commentary, which is a real cost
 *    even if no money changes hands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether bad feedback is an accident or a feature.
 */

omega_variable(
    shitty_feedback_intent,
    "Is shitty feedback a byproduct of low skill and poor incentives (Mountain/Noise) or a predatory attempt to destabilize the subject (Snare)?",
    resolution_mechanism("Audit of a feedback giver's history vs. the receiver's subsequent performance metrics and well-being."),
    impact("If noise: protocol is a Rope. If predatory: protocol must shift to pure Rejection/Snare-avoidance."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Defensiveness / Counter-Criticism
 *    Viability: This is the default, "natural" human reaction to perceived attack.
 *    Suppression: Discouraged by the protocol as "rarely necessary" and ineffective for being "taken seriously".
 * 
 * CONCLUSION:
 * The protocol of 'Accept, Reframe, Reject' is a learned 'Rope' that an agent uses to
 * manage the environmental 'Mountain' of constant, low-quality feedback. It suppresses
 * our natural defensive reaction in favor of a more strategic, self-protective one.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/shitty_feedback_handling].
 * 2. Multi-perspective: ?- multi_index_report(shitty_feedback_handling).
 * 3. Run tests: ?- run_tests(shitty_feedback_handling_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */