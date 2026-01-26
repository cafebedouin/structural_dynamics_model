% ============================================================================
% CONSTRAINT STORY: sunk_cost_fallacy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Behavioral Economics / Arkes & Blumer (1985)
% ============================================================================

:- module(constraint_sunk_cost_fallacy, []).

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
 * * constraint_id: sunk_cost_fallacy
 * human_readable: The Sunk Cost Fallacy
 * domain: economic/social/cognitive
 * temporal_scope: Permanent (Human Decision Architecture)
 * spatial_scope: Global (Resource Allocation)
 * * SUMMARY:
 * The sunk cost fallacy is the tendency to continue an endeavor once an 
 * investment in money, effort, or time has been made, even when the current 
 * costs outweigh the likely future benefits. It is a "rear-view mirror" 
 * approach to decision-making that ignores future marginal utility in favor 
 * of emotional accounting for past losses.
 * * KEY AGENTS:
 * - The Entrenched Participant: An agent who feels they have "too much 
 * invested to quit" (e.g., a student in the wrong major or a failing project lead).
 * - The Rational Arbiter: An external observer who ignores past costs to 
 * focus solely on future ROI.
 * - The Systemic Exploiter: An institution (e.g., a subscription service or 
 * casino) that uses initial buy-ins to lock agents into further spending.
 * * NARRATIVE ARC:
 * The fallacy functions as a "Mountain" of cognitive inertia. To the manager, 
 * it is a "Rope" (the logic of commitment and persistence). However, for the 
 * individual trapped in a failing path, it becomes a "Snare," where every 
 * additional unit of effort invested makes the psychological cost of 
 * quitting higher, effectively strangling their future optionality.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(sunk_cost_interval, 0, 10).
narrative_ontology:constraint_claim(sunk_cost_fallacy, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts future "optionality" and "capital." It benefits 
% entities that profit from "lock-in" while the victim extracts only 
% emotional comfort from not admitting failure.
domain_priors:base_extractiveness(sunk_cost_fallacy, 0.5).

% Suppression score (0.0-1.0)
% Rationale: It suppresses the "Exit" alternative. The brain frames 
% quitting as "waste," making the rational choice (stopping) feel 
% morally or logically invisible.
domain_priors:suppression_score(sunk_cost_fallacy, 0.6).

% Enforcement requirements
domain_priors:emerges_naturally(sunk_cost_fallacy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(sunk_cost_fallacy, extractiveness, 0.5).
narrative_ontology:constraint_metric(sunk_cost_fallacy, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(sunk_cost_fallacy, [casinos, predatory_subscription_models, status_quo_bureaucrats]).
constraint_victim(sunk_cost_fallacy, [unhappy_career_holders, failed_project_investors]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE BEHAVIORAL ECONOMIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer of human error)
   WHEN: civilizational (Observing an evolved cognitive heuristic)
   WHERE: trapped (Human neural pathways prioritize loss aversion)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, the fallacy is a Mountain. It is an unchangeable feature of 
   the human landscape. Regardless of the culture or era, humans show a 
   predictable biological resistance to "cutting losses" because our brains 
   are hard-wired for loss aversion over gain maximization.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sunk_cost_fallacy,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROJECT MANAGER - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping power over a team)
   WHEN: biographical (Achieving project completion targets)
   WHERE: arbitrage (Can frame "persistence" as a virtue to secure more funding)
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For the manager, "sunk cost" logic is often used as a Rope. It is a 
   coordination mechanism for persistence. By reminding the team or the 
   board of "how far we've come," they pull together the social and 
   financial resources needed to push through the "trough of sorrow" 
   toward a delayed finish line.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sunk_cost_fallacy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNHAPPY DOCTORAL STUDENT - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A subject trapped by their own history)
   WHEN: immediate (Daily experience of diminishing returns)
   WHERE: constrained (Cannot quit without losing years of social/financial identity)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the person seven years into a degree they no longer want, the fallacy 
   is a Snare. The "investment" they've made acts as the rope that 
   strangles their current freedom. They cannot leave because they cannot 
   face the "waste" of the past seven years, so they lose the *next* seven years too.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sunk_cost_fallacy,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(sunk_cost_fallacy_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(sunk_cost_fallacy, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy, T3, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(loss_aversion_extraction) :-
    % Verify that the fallback to the past extracts from the future.
    domain_priors:base_extractiveness(sunk_cost_fallacy, Score),
    Score >= 0.4.

test(horizon_immutability) :-
    % Short-term pressure (Immediate) makes it a Snare/Mountain; 
    % Civilizational view sees it as an invariant fact of the mind.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(sunk_cost_fallacy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: The fallacy extracts the most precious non-renewable resource: 
 * future time. It forces agents to pay for the past using the future.
 * * 2. PERSPECTIVE SELECTION:
 * The Manager (Rope) is a key perspective because "Commitment" is often 
 * the social/positive framing of a sunk cost fallacy.
 * * 3. NOOSE CLASSIFICATION:
 * The Snare emerges when the exit cost is not just financial, but 
 * identity-based. "Who am I if I am not the person who finished this?"
 * * 4. OMEGAS:
 * omega_variable(quitting_stigma,
 *	"To what degree is the 'Snare' caused by internal bias vs. external social shame?",
 *	resolution_mechanism("Comparative study of abandonment rates in anonymous vs. public commitments"),
 *	impact("If Internal: Mountain. If Social: Rope (changeable culture)."),
 *	confidence_without_resolution(medium)
 *	).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Marginal Utility Thinking
 * Viability: The standard economic "correct" answer. Focus only on 
 * the next dollar/hour spent.
 * Suppression: High. It is cognitively difficult because it requires 
 * overriding deep-seated emotional loss-aversion.
 * * CONCLUSION:
 * The existence of a rational alternative (Marginal Thinking) that is 
 * systematically suppressed by human emotion converts the Sunk Cost 
 * Fallacy from a simple mistake into a persistent "Snare" for the 
 * unexamined life.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_sunk_cost_fallacy].
 * 2. Report: ?- multi_index_report(sunk_cost_fallacy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
