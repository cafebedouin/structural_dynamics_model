% ============================================================================
% CONSTRAINT STORY: shitty_feedback_handling
% ============================================================================
% Generated: 2026-05-22
% Model: Gemini 2.0 Flash
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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: shitty_feedback_handling
 * human_readable: The Protocol of Shitty Feedback
 * domain: social/professional
 * temporal_scope: Contemporary / Ongoing
 * spatial_scope: Organizational and Interpersonal Environments
 * * SUMMARY:
 * "Shitty feedback" is a ubiquitous byproduct of social systems where feedback skills are 
 * rare and incentives for care are low. The constraint identifies the three 
 * actions available to a receiver—Accept, Reframe, or Reject—to process this external 
 * noise without losing agency or becoming defensive.
 * * KEY AGENTS:
 * - The Feedback Giver: Often unskilled, context-blind, or acting out their own issues.
 * - The Receiver (Subject): The agent tasked with filtering noise to find idiosyncratic value.
 * - The Supervisor: An institutional power player whose feedback often demands a performative "Accept" 
 * status regardless of its inherent quality.
 * * NARRATIVE ARC:
 * Feedback begins as an extractive force (noise/criticism) that threatens the subject's 
 * time and self-concept. By applying the "Accept, Reframe, Reject" protocol, the subject 
 * transforms the interaction from a potential "Noose" of social judgment into a "Rope" 
 * for personal growth or a neutralized "Mountain" of irrelevant data.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(shitty_feedback_handling_interval, 0, 10).

% Updated to valid v3.1 ontology pillar: [rope]
narrative_ontology:constraint_claim(shitty_feedback_handling, rope).

% Base extractiveness: Moderate (0.4)
% Rationale: Shitty feedback extracts emotional energy, time, and potentially 
% identity if the receiver feels forced to defend themselves or accept invalidity.
domain_priors:base_extractiveness(shitty_feedback_handling, 0.4).

% Suppression: Moderate (0.3)
% Rationale: The "natural" alternative of defensiveness is visible but actively 
% discouraged as a "rarely necessary" and ineffective reaction.
domain_priors:suppression_score(shitty_feedback_handling, 0.3).

% Enforcement: Requires active mental maintenance.
domain_priors:requires_active_enforcement(shitty_feedback_handling).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(shitty_feedback_handling, extractiveness, 0.4).
narrative_ontology:constraint_metric(shitty_feedback_handling, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(shitty_feedback_handling, the_receiver). % Preserves agency/sanity
constraint_beneficiary(shitty_feedback_handling, the_giver).    % Feeling "heard" costs nothing
constraint_victim(shitty_feedback_handling, the_ego).           % Defensiveness is suppressed

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SUBORDINATE - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to supervisor's evaluation)
   WHEN: immediate (High-stakes professional feedback)
   WHERE: constrained (Must maintain employment relationship)
   SCOPE: local (Workplace context)
   
   WHY THIS CLASSIFICATION:
   In an institutional setting, feedback from a supervisor acts as a "Noose" because 
   the power asymmetry often forces an "Accept" or "I'll do better" response even 
   if the feedback is contextless or shitty.
   
   NARRATIVE EVIDENCE:
   "there are situations where you have to do something different, such as the 
   supervisor at work example... a simple: 'I’ll do better,' will often suffice".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shitty_feedback_handling,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(shitty_feedback_handling, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SELF-ACTUALIZED SUBJECT - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Managing personal growth and social interactions)
   WHEN: biographical (Developing lifelong feedback skills)
   WHERE: mobile (Can choose to reframe or reject input)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For an agent with exit options and self-awareness, the protocol is a "Rope"—a 
   beneficial tool to coordinate with the external world's perspective without 
   being crushed by it.
   
   NARRATIVE EVIDENCE:
   "We all have different values... and we need to reframe input to make it 
   valuable in light of our idiosyncrasies".
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
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the systemic nature of feedback)
   WHEN: historical (The long-term lack of institutional incentives for feedback)
   WHERE: analytical (Unconstrained observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The observer sees that "shitty feedback is out there" as an unchangeable 
   feature of human systems due to the difficulty of the skill and lack 
   of incentives. It is a social "Mountain" of noise.
   
   NARRATIVE EVIDENCE:
   "giving feedback is difficult and most people are terrifically bad at it... 
   the systems we occupy do not incentivize the effort".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shitty_feedback_handling,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(shitty_feedback_handling_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(shitty_feedback_handling, T1, context(individual_powerless, immediate, constrained, local)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, T2, context(individual_moderate, biographical, mobile, local)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_scaling) :-
    % Institutional feedback (Powerless context) is more extractive than peer feedback.
    constraint_indexing:extractiveness_for_agent(shitty_feedback_handling, context(individual_powerless, immediate, constrained, local), E1),
    constraint_indexing:extractiveness_for_agent(shitty_feedback_handling, context(individual_moderate, biographical, mobile, local), E2),
    E1 >= E2.

test(time_immutability) :-
    % In the short term/immediate, bad feedback feels like a problem to solve (Rope/Noose).
    % In the historical/analytical view, it's just an environment fact (Mountain).
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(shitty_feedback_handling_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2025-05-22
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.4):
 * Reasoning: The text highlights that feedback is often contextless noise that wastes the 
 * receiver's time and ego. The extraction is the cognitive load required to 
 * process "worthless" commentary from strangers or unskilled peers.
 * * 2. SUPPRESSION SCORE (0.3):
 * Reasoning: The text explicitly mentions defensiveness as the first reaction. 
 * The protocol suppresses this natural impulse in favor of a performative "Politeness" 
 * that protects the subject's long-term utility.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Subordinate (Noose) because institutional power changes the "Reject" option 
 * into a liability. Chose Subject (Rope) for the core "Accept/Reframe/Reject" utility. 
 * Chose Analyst (Mountain) to reflect the systemic ubiquity mentioned in the text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction (potential) constraints:
omega_variable(
    shitty_feedback_handling_intent,
    "Is shitty feedback a byproduct of low skill (Mountain/Noise) or a predatory attempt to destabilize the subject (Noose)?",
    resolution_mechanism("Audit of feedback giver's history vs. receiver's subsequent performance metrics"),
    impact("If noise: protocol is a Rope. If predatory: protocol must shift to pure Rejection/Noose-avoidance."),
    confidence_without_resolution(medium)
).

omega_variable(
    reframing_ceiling,
    "Can any contextless feedback be reframed into value, or is there a 'truth floor' below which feedback is absolute waste?",
    resolution_mechanism("Measure time-to-insight for reframing unsolicited stranger feedback"),
    impact("If value exists: Rope. If waste: Mountain of noise."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Defensiveness / Counter-Criticism
 * Viability: Our "first reaction" to shitty feedback.
 * Suppression: Discouraged as "rarely necessary" and ineffective for being "taken seriously".
 * Evidence: "I think the most important point is to not defend yourself".
 * * ALTERNATIVE 2: Blind Acceptance
 * Viability: Accepting the giver's context/values as superior to one's own idiosyncrasies.
 * Suppression: Rejected because "it’s rare for a person to know us well enough to give 
 * feedback that can simply be accepted".
 * * CONCLUSION:
 * The existence of "shitty feedback" as an environmental constant shifts the interaction 
 * from a potential meritocracy (where good feedback is rewarded) into a survival 
 * protocol (where noise must be managed).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/shitty_feedback_handling].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(shitty_feedback_handling).
 * 3. Run tests: ?- run_tests(shitty_feedback_handling_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
