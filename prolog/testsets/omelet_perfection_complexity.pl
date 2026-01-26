% ============================================================================
% CONSTRAINT STORY: omelet_perfection_complexity
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Omelets, Perfection & Life" from cafebedouin.org
% ============================================================================

:- module(constraint_omelet_perfection_complexity, []).

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
 * * constraint_id: omelet_perfection_complexity
 * human_readable: The French Omelet Paradox (Chasing Perfection)
 * domain: social/psychological
 * temporal_scope: Perennial / Biographical
 * spatial_scope: Local (The Kitchen)
 * * SUMMARY:
 * This constraint explores the hidden complexity within seemingly simple tasks, using the 
 * classic French omelet as a proxy for life. It identifies that 
 * "simple things" only appear simple when one stops asking the right questions, 
 * revealing a "frustratingly, world-openingly complex" reality once technique is applied.
 * * KEY AGENTS:
 * - The Obsessive (Francis Lam): One who "chases perfection" and recognizes the 
 * depth of trivial tasks.
 * - The Layperson: A "rational person" who views the task as a "no-brainer" 
 * and settles for simple, often mediocre answers.
 * - The Chef (Skibitcky): The institutional authority who holds the "brain" 
 * of the student, enforcing the rigid laws of technique.
 * * NARRATIVE ARC:
 * An agent starts with a "no-brainer" view of a task but becomes "obsessed" upon 
 * discovering the importance of technique (heat, curds, "fudge factors"). 
 * This creates a cycle where the "Rope" of technique leads to a "Mountain" of 
 * complexity, eventually providing a "Rope" for general life-wisdom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(omelet_perfection_interval, 0, 10).
narrative_ontology:constraint_claim([omelet_perfection_complexity], [skill_acquisition]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Low (0.25). While the "obsession" extracts time and energy, the 
% result is "always a really tasty egg dish" that is better than the alternative.
domain_priors:base_extractiveness(omelet_perfection_complexity, 0.25).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.45). The "simple answer" suppresses the 
% "world-openingly complex" one until the subject starts asking questions.
domain_priors:suppression_score(omelet_perfection_complexity, 0.45).

% Enforcement requirements
% Mastery of the curds and roll "requires active enforcement" of technique.
domain_priors:requires_active_enforcement(omelet_perfection_complexity).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(omelet_perfection_complexity, extractiveness, 0.25).
narrative_ontology:constraint_metric(omelet_perfection_complexity, suppression_requirement, 0.45).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(omelet_perfection_complexity, complex_wisdom). % "World-opening" insight
constraint_victim(omelet_perfection_complexity, simple_answer). % Mediocre, un-questioned existence

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE OBSESSIVE PRACTITIONER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has agency to chase perfection and adjust technique)
   WHEN: biographical (Years of trying to make the "perfect omelet")
   WHERE: mobile (Uses "Boursin" or "low heat" to navigate difficulty)
   SCOPE: local (The personal kitchen)
   
   WHY THIS CLASSIFICATION:
   For the practitioner, the omelet technique is a "Rope"—a tool for coordination and 
   self-growth. It is a means to learn the "vital lesson" that things 
   are only simple when you stop seeing them clearly.
   
   NARRATIVE EVIDENCE:
   "enough to keep reteaching me this vital lesson... what you find... is that 
   even simple things can be wonderfully... complex".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    omelet_perfection_complexity,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNAWARE LAYPERSON - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Trapped by a "no-brainer" mindset)
   WHEN: immediate (The moment of throwing eggs in a pan)
   WHERE: trapped (Lacks the questions to see the hidden complexity)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the layperson, the "simple" view is a "Snare." It strangles the potential for 
   quality, leaving them to settle for the 90% of mediocrity they've "eaten in 
   [their] life" without realizing there is a "difference".
   
   NARRATIVE EVIDENCE:
   "I, like every other rational person, thought an omelet was something anyone 
   can make... No-brainer".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    omelet_perfection_complexity,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(omelet_perfection_complexity, S),
    S > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MASTER CHEF - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (Rules of the classic French tradition)
   WHEN: historical (The enduring standard of culinary "perfection")
   WHERE: analytical (The "right questions" and "good technique")
   SCOPE: global (A universal culinary standard)
   
   WHY THIS CLASSIFICATION:
   To the Chef, the requirements of the omelet are a "Mountain"—an unchangeable 
   standard of technique where curds, heat, and "rolling" are zero-degree-of-freedom 
   realities.
   
   NARRATIVE EVIDENCE:
   "Before Chef Skibitcky got ahold of my brain... it turns out it is pretty 
   hard and the key is having good technique".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    omelet_perfection_complexity,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(omelet_perfection_complexity_tests).

test(multi_perspective_omelet) :-
    % Obsessive sees Rope
    constraint_indexing:constraint_classification(omelet_perfection_complexity, T1, context(individual_moderate, biographical, mobile, local)),
    % Layperson sees Snare
    constraint_indexing:constraint_classification(omelet_perfection_complexity, T2, context(individual_powerless, immediate, trapped, local)),
    % Chef sees Mountain
    constraint_indexing:constraint_classification(omelet_perfection_complexity, T3, context(institutional, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(complexity_suppression_insight) :-
    % Suppression prevents the layperson from seeing the world-opening complexity
    domain_priors:suppression_score(omelet_perfection_complexity, S),
    S > 0.4.

:- end_tests(omelet_perfection_complexity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.25):
 * Reasoning: Low extraction because the "failures" are still highly valuable/tasty 
 * compared to the baseline life of the unobsessed.
 * 2. PERSPECTIVE SELECTION:
 * The contrast between the "No-brainer" (Snare) and the "World-opening" (Rope) 
 * mirrors the text's shift from rational person to obsessed practitioner.
 * 3. MANDATROPHY STATUS:
 * [RESOLVED MANDATROPHY]. While obsession can be extractive, the benefit 
 * (reteaching vital lessons) justifies the coordination effort for the 
 * biographical agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omelet_perfection_definition,
    "Is 'perfection' an objective Mountain of culinary physics or a 
     subjective Rope of narrative storytelling?",
    resolution_mechanism("Comparison of 'perfect' omelets across divergent cultures and chemical analyses of curds"),
    impact("If Mountain: Technique is a law. If Rope: Technique is a ritual."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Scramble
 * Viability: A valid "mistake" that is still tasty.
 * Suppression: Shunted as "settling" compared to the French classic.
 * * ALTERNATIVE 2: The "French Velveeta" (Boursin)
 * Viability: A "fudge factor" for the unobsessed.
 * Suppression: Initially shunted by "skeptical" traditionalists but accepted 
 * as a Rope for the lay person.
 * * CONCLUSION:
 * The existence of tasty "mistakes" makes the "perfection" a Rope of choice 
 * rather than a lethal Snare.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into system: ?- [constraint_omelet_perfection_complexity].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
