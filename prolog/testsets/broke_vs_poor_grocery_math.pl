% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Erynn Brook / cafebedouin.org, "The Difference Between Being Broke and Being Poor"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_broke_vs_poor_grocery_math, []).

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
 * * constraint_id: broke_vs_poor_grocery_math
 * human_readable: The Cognitive Load of Poverty (Grocery Math)
 * domain: economic/social/psychological
 * temporal_scope: Persistent (until the "moment" of realization)
 * spatial_scope: Local/Personal (The grocery store checkout)
 * * SUMMARY:
 * This constraint defines the distinction between "broke" (temporarily without funds but with incoming income) 
 * and "poor" (perpetually counting costs). The primary mechanism of the constraint is "grocery math"—the 
 * mandatory mental tallying of every item to ensure it does not exceed available funds. 
 * Breaking the constraint occurs only when the cognitive load is lifted and the subject realizes 
 * they "hadn't done the math".
 * * KEY AGENTS:
 * - The Poor Individual: The agent under high cognitive load, forced to "count the costs" of survival.
 * - The Broke Individual: An agent temporarily without cash but protected by the expectation of income, 
 * exempt from the "math".
 * - The Cashier/Grocery Belt: The physical environment that enforces the "moment" of realization or failure.
 * * NARRATIVE ARC:
 * Poverty functions as a constant psychological "math" problem. The transition to non-poverty 
 * is not marked by a specific bank balance, but by the cessation of this mental enforcement. 
 * While "broke" is a temporary Rope (coordination with future money), "poor" is a Noose 
 * that extracts cognitive bandwidth.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(broke_poor_interval, 0, 10).
narrative_ontology:constraint_claim([broke_vs_poor_grocery_math], [economic_straitjacket]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.8). Poverty extracts cognitive bandwidth, emotional peace, and 
% time by forcing the subject to calculate survival costs in real-time.
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.8).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.6). The alternative of "not counting" is invisible 
% and suppressed by the immediate risk of insufficient funds at the register.
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.6).

% Enforcement: Emerges naturally from scarcity but is actively enforced by the 
% checkout counter.
domain_priors:requires_active_enforcement(broke_vs_poor_grocery_math).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, 0.8).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(broke_vs_poor_grocery_math, financial_stability). % Non-poor status
constraint_victim(broke_vs_poor_grocery_math, cognitive_bandwidth). % Bandwidth consumed by "math"

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POOR INDIVIDUAL - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to immediate scarcity)
   WHEN: immediate (Must solve the "math" before reaching the cashier)
   WHERE: trapped (No conceptual exit from counting until income shifts)
   SCOPE: local (The grocery belt)
   
   WHY THIS CLASSIFICATION:
   For the poor person, the requirement to "count the costs" is a "Noose." It 
   constrains their attention and tightens their focus on survival, extracting 
   the ability to simply "load groceries" without fear.
   
   NARRATIVE EVIDENCE:
   "Everyone I’ve ever talked to who has been poor... has the same story... realized 
   I hadn’t done the math".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    broke_vs_poor_grocery_math,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(broke_vs_poor_grocery_math, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BROKE INDIVIDUAL - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has incoming income)
   WHEN: biographical (Temporary status within a life of income)
   WHERE: mobile (Alternatives like "waiting for payday" are visible)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the "broke" person, their financial state is a "Rope"—a temporary 
   coordination mechanism. Because "income is coming in," they are not 
   strangled by the requirement to count every cent; they have the 
   bandwidth to ignore the math.
   
   NARRATIVE EVIDENCE:
   "broke people know income is coming in... so still don't count".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    broke_vs_poor_grocery_math,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SYSTEMIC ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "story" of poverty)
   WHEN: civilizational (Fundamental state of scarcity vs. abundance)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the observer, the "grocery math" is a "Mountain"—an immutable law of 
   poverty. It is the defining feature that remains until 
   the systemic "moment" of change occurs.
   
   NARRATIVE EVIDENCE:
   "Everyone I’ve ever talked to... has the same story... grocery shopping".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    broke_vs_poor_grocery_math,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(multi_perspective_realization) :-
    % Poor (Powerless) sees Noose
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, noose, context(individual_powerless, immediate, trapped, local)),
    % Broke (Moderate) sees Rope
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, rope, context(individual_moderate, biographical, mobile, local)),
    % Difference is confirmed
    Type1 \= Type2.

test(cognitive_extraction_scaling) :-
    % Poor experience higher bandwidth extraction than those who are just "broke."
    constraint_indexing:extractiveness_for_agent(broke_vs_poor_grocery_math, context(individual_powerless, immediate, trapped, local), E1),
    constraint_indexing:extractiveness_for_agent(broke_vs_poor_grocery_math, context(individual_moderate, biographical, mobile, local), E2),
    E1 > E2.

:- end_tests(broke_vs_poor_grocery_math_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.8):
 * Reasoning: Poverty isn't just a lack of money; it's the extraction of 
 * cognitive "peace". The "math" is an enforced labor 
 * that the "broke" do not have to perform.
 * * 2. MANDATROPHY RESOLUTION:
 * Status: RESOLVED MANDATROPHY. The system is a Rope for the non-poor (who can 
 * ignore the math) but a Noose for the poor (who must count or risk 
 * register failure).
 * * 3. PERSPECTIVE SELECTION:
 * Contrast between "Poor" (Noose) and "Broke" (Rope) is the core insight 
 * of the source text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_arrival_certainty,
    "Is the expectation that 'income is coming in' a reliable Mountain (fact) 
     or a fragile Rope (hope) that could snap into poverty?",
    resolution_mechanism("Audit of income stability and social safety nets"),
    impact("If stable: 'Broke' is a Rope. If fragile: 'Broke' is a Noose in waiting."),
    confidence_without_resolution(medium)
).

omega_variable(
    broke_vs_poor_grocery_math_extraction_intent,
    "Is the 'math' requirement a functional necessity of limited funds (Mountain) 
     or an extractive systemic failure of the market (Noose)?",
    resolution_mechanism("Audit of grocery pricing volatility vs. base survival wages"),
    impact("If necessity: Financial Mountain. If systemic failure: Mandatrophy Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Abundance (Not Counting)
 * Viability: The "story of the moment they realized they were no longer poor".
 * Suppression: Actively suppressed by poverty; "counting" is the only way to avoid 
 * the register's rejection.
 * * ALTERNATIVE 2: Credit/Debt
 * Viability: Using debt to bypass the "math" at the register.
 * Suppression: Often unavailable to the poor, or creates a tighter Noose of extraction.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [broke_vs_poor_grocery_math].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
