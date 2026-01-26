% ============================================================================
% CONSTRAINT STORY: planning_fallacy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Kahneman, D., & Tversky, A. (1979) / Cognitive Science
% ============================================================================

:- module(constraint_planning_fallacy, []).

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
 * * constraint_id: planning_fallacy
 * human_readable: The Planning Fallacy
 * domain: economic/social/technological
 * temporal_scope: Permanent (Human Cognitive History)
 * spatial_scope: Global (Organizational Systems)
 * * SUMMARY:
 * The planning fallacy is a phenomenon in which predictions about how much time 
 * will be needed to complete a future task display an optimism bias and 
 * underestimate the time needed. This occurs even when the planner knows that 
 * similar tasks have taken longer in the past.
 * * KEY AGENTS:
 * - The Enthusiastic Planner: The agent who underestimates time to secure 
 * approval or maintain morale.
 * - The Exploited Worker: The agent forced to absorb the "time debt" 
 * through crunch or unpaid labor.
 * - The Resource Allocator: The institutional agent who uses low estimates 
 * to justify aggressive capital deployment.
 * * NARRATIVE ARC:
 * The constraint functions as a "Mountain" of inherent cognitive limitation. 
 * For the institution, it is a "Rope" (a coordination tool to get 
 * projects started). For the worker at the end of the deadline, it is a 
 * "Snare," as the "impossible" schedule extracts their health and time 
 * to compensate for the initial underestimation.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification
narrative_ontology:interval(planning_fallacy_interval, 0, 10).
narrative_ontology:constraint_claim(planning_fallacy, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "buffer time" and health from laborers to meet 
% unrealistic deadlines set by planners.
domain_priors:base_extractiveness(planning_fallacy, 0.5).

% Suppression score (0.0-1.0)
% Rationale: It suppresses "Reference Class Forecasting" and historical 
% data. Planners actively ignore "base rates" in favor of the specific 
% "internal" view of the current project.
domain_priors:suppression_score(planning_fallacy, 0.6).

% Enforcement: Emerges naturally from cognitive architecture but is 
% often institutionalized to force project initiation.
domain_priors:emerges_naturally(planning_fallacy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(planning_fallacy, extractiveness, 0.5).
narrative_ontology:constraint_metric(planning_fallacy, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
% Who systematically benefits from this constraint?
constraint_beneficiary(planning_fallacy, institutional_planners).
% Who systematically suffers extraction?
constraint_victim(planning_fallacy, [individual_contributors, developers, laborers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESEARCH PSYCHOLOGIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of cognitive limits)
   WHEN: civilizational (A permanent feature of human information processing)
   WHERE: trapped (Brain architecture is relatively fixed)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, the fallacy is a Mountain. It is an unchangeable feature 
   of the "Inside View." Humans are biologically prone to focus on the 
   specifics of their own plan rather than the statistics of the world.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planning_fallacy,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STARTUP FOUNDER - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping power over a new venture)
   WHEN: biographical (Achieving project milestones)
   WHERE: arbitrage (Can move between different funding sources)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the founder, the fallacy is a Rope. It is a coordination mechanism for 
   optimism. Without it, the daunting reality of a project might prevent it 
   from ever starting. It acts as a tether to a future success that 
   requires a "leap of faith" to initiate.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planning_fallacy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE JUNIOR DEVELOPER - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to the manager's schedule)
   WHEN: immediate (Today's "crunch" time)
   WHERE: constrained (Must deliver to keep the job)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the worker at the end of a project cycle, the fallacy is a Snare. 
   The initial underestimation by management creates an impossible 
   deadline that tightens over time, extracting their evening and 
   weekend hours to "fix" the schedule.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planning_fallacy,
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

:- begin_tests(planning_fallacy_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that different perspectives see the fallacy as different types.
 */
test(multi_perspective_variance) :-
    % Analytical sees Mountain
    constraint_indexing:constraint_classification(planning_fallacy, mountain, context(agent_power(analytical), _, _, _)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(planning_fallacy, rope, context(agent_power(institutional), _, _, _)),
    % Powerless sees Snare
    constraint_indexing:constraint_classification(planning_fallacy, snare, context(agent_power(individual_powerless), _, _, _)).

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that extraction experienced varies with agent power.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(planning_fallacy, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(planning_fallacy, ContextPowerful, Score2),
    Score1 > Score2.

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that mutability varies with time horizon.
 */
test(time_immutability_planning) :-
    % Immediate/Biographical + trapped = Mountain (unchangeable deadline)
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % Civilizational/Historical = Rope (we can learn to build systems that account for it)
    constraint_indexing:effective_immutability(civilizational, mobile, rope).

:- end_tests(planning_fallacy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: The fallacy extracts "surplus life" (unpaid overtime, stress) 
 * from those tasked with execution. It's a moderate score because it 
 * often also harms the institution through budget overruns.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Analytical (the graph), Institutional (the budgeter), and 
 * Individual Powerless (the "crunched" worker) to show the full range 
 * of "Mountain," "Rope," and "Snare."
 * * 3. CLASSIFICATION RATIONALE:
 * The "Rope" for founders is a crucial insight: if people were rational 
 * about project timelines, few large-scale innovations would be attempted.
 * * 4. AMBIGUITIES:
 * - Resolved the "natural" vs "enforced" distinction by identifying 
 * it as naturally emerging but institutionalized for gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_underestimation,
    "How much of the planning fallacy is biological bias (Mountain) versus strategic, deceptive underestimation (Snare) for funding?",
    resolution_mechanism("Compare private project estimates with public funding requests"),
    impact("If Strategic: It is a Snare of deception. If Biological: It is a Mountain of error."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Reference Class Forecasting (RCF)
 * Viability: High. Using historical data from similar projects to set 
 * timelines effectively eliminates the fallacy.
 * Suppression: High. Institutions often view RCF as "un-innovative" 
 * or "pessimistic," suppressing it to maintain high project approval rates.
 * * CONCLUSION:
 * The existence of a valid alternative (RCF) that is suppressed by 
 * institutional optimism confirms that for the laborer, the fallacy 
 * is a Snare, not a natural Mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_planning_fallacy].
 * 2. Multi-perspective: ?- multi_index_report(planning_fallacy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
