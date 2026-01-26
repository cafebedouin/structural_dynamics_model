% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
 * 
 * constraint_id: broke_vs_poor_grocery_math
 * human_readable: The Cognitive Load of Poverty (Grocery Math)
 * domain: economic/social/psychological
 * temporal_scope: Persistent (until the "moment" of realization)
 * spatial_scope: Local/Personal (The grocery store checkout)
 * 
 * SUMMARY:
 * This constraint defines the distinction between "broke" (temporarily without funds
 * but with incoming income) and "poor" (perpetually counting costs). The primary 
 * mechanism is "grocery math"—the mandatory mental tallying of every item to ensure 
 * it does not exceed available funds. Breaking the constraint occurs only when the
 * cognitive load is lifted.
 * 
 * KEY AGENTS:
 * - The Poor Individual (Individual Powerless): Under high cognitive load.
 * - The Broke Individual (Individual Moderate): Temporarily without cash, but with income.
 * - The Social Worker / Welfare Agency (Institutional): Faces the systemic challenges of poverty.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(broke_vs_poor_grocery_math, 0, 10).
narrative_ontology:constraint_claim(broke_vs_poor_grocery_math, snare).

% Base extractiveness score (0.8): High. Poverty extracts cognitive bandwidth, 
% emotional peace, and time by forcing the subject to calculate survival costs 
% in real-time.
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.8).

% Suppression score (0.6): Moderate-High. The alternative of "not counting" is
% invisible and suppressed by the immediate risk of insufficient funds at the register.
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.6).

% Enforcement: Emerges naturally from scarcity but is actively enforced by the 
% checkout counter.
domain_priors:requires_active_enforcement(broke_vs_poor_grocery_math).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(broke_vs_poor_grocery_math, the_non_poor).
constraint_victim(broke_vs_poor_grocery_math, the_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POOR INDIVIDUAL - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to immediate scarcity)
   WHEN: immediate (Must solve the "math" before reaching the cashier)
   WHERE: trapped (No conceptual exit from counting until income shifts)
   
   WHY THIS CLASSIFICATION:
   For the poor person, the requirement to "count the costs" is a 'Snare'. It 
   constrains their attention and tightens their focus on survival, extracting 
   the ability to simply "load groceries" without fear. This constant mental
   burden is a form of cognitive oppression.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    broke_vs_poor_grocery_math,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BROKE INDIVIDUAL - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has incoming income)
   WHEN: biographical (Temporary status within a life of income)
   WHERE: mobile (Alternatives like "waiting for payday" are visible)
   
   WHY THIS CLASSIFICATION:
   For the "broke" person, their financial state is a 'Rope'—a temporary 
   coordination mechanism. Because "income is coming in," they are not 
   strangled by the requirement to count every cent; they have the 
   bandwidth to ignore the math, using their expected future income as a buffer.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOCIAL WORKER / WELFARE AGENCY - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Faces the systemic challenges of poverty)
   WHEN: civilizational (Poverty as an enduring social problem)
   WHERE: analytical (Observing broader economic forces)
   
   WHY THIS CLASSIFICATION:
   For a social worker or welfare agency, "grocery math" represents the systemic 
   failure that creates the 'Mountain' of poverty. It's an immutable, pervasive
   barrier that they constantly work against, acknowledging its deep structural roots.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    broke_vs_poor_grocery_math,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(broke_vs_poor_grocery_math_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'Social Worker / Welfare Agency' as
 *    the institutional agent. Their view of poverty as a systemic, structural
 *    'Mountain' aligns with their mandate to address its root causes.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Poor (Snare): Cognitive oppression and constant stress.
 *    - Broke (Rope): Temporary state with a clear path to resolution.
 *    - Social Worker (Mountain): An immutable, systemic problem they fight against.
 * 
 * 3. EXTRACTIVENESS (0.8): High, because poverty extracts cognitive bandwidth,
 *    emotional peace, and time, by forcing constant calculations for survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    broke_vs_poor_grocery_math_extraction_intent,
    "Is the 'grocery math' requirement a functional necessity of limited individual funds (Mountain) or an extractive systemic failure of the market (Snare)?",
    resolution_mechanism("Audit of grocery pricing volatility vs. base survival wages, and access to affordable, nutritious food for low-income populations."),
    impact("If necessity: Financial Mountain. If systemic failure: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Abundance (Not Counting)
 *    Viability: This is the state experienced by the "not poor."
 *    Suppression: Actively suppressed by poverty. The "math" is mandatory to avoid the humiliation of insufficient funds at checkout.
 * 
 * CONCLUSION:
 * The "grocery math" constraint is a visible manifestation of systemic poverty.
 * The suppression of the alternative of "not counting" is the core mechanism
 * that maintains the 'Snare' of cognitive load for the poor.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/broke_vs_poor_grocery_math].
 * 2. Multi-perspective: ?- multi_index_report(broke_vs_poor_grocery_math).
 * 3. Run tests: ?- run_tests(broke_vs_poor_grocery_math_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */