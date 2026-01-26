% ============================================================================
% CONSTRAINT STORY: ai_evaluators_matching
% Status: [RESOLVED MANDATROPHY]
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini 2.0 Flash
% Source: AI-Driven Recruitment / Automated Talent Assessment
% ============================================================================

:- module(ai_evaluators_matching, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * SUMMARY:
 * AI-Evaluators function as a Tangled Rope: they provide critical coordination by 
 * reducing hire time by 50% and improving talent matching by 67%. 
 * However, they extract "performance labor" from applicants and often function 
 * as a Black-Box Sieve that suppresses human discretion.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(ai_evaluators_matching, 0, 10).
narrative_ontology:constraint_claim(ai_evaluators_matching, tangled_rope).

% Base extractiveness (0.75): AI tools extract behavioral data and impose 
% "digital twin" optimization costs on applicants.
domain_priors:base_extractiveness(ai_evaluators_matching, 0.75).

% Suppression score (0.80): Human-only paths are increasingly non-viable for 
% initial gates in large-scale hiring.
domain_priors:suppression_score(ai_evaluators_matching, 0.80).

domain_priors:requires_active_enforcement(ai_evaluators_matching).

constraint_beneficiary(ai_evaluators_matching, large_scale_employers).
constraint_victim(ai_evaluators_matching, neurodivergent_or_atypical_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

% PERSPECTIVE: THE ANALYST - Tangled Rope
% Hybrid coordination (efficiency) + extraction (bias/opacity).
constraint_indexing:constraint_classification(
    ai_evaluators_matching,
    tangled_rope,
    context(agent_power(analytical), time_horizon(biographical), exit_options(mobile), spatial_scope(global))
) :-
    domain_priors:base_extractiveness(ai_evaluators_matching, E), E > 0.4,
    domain_priors:suppression_score(ai_evaluators_matching, S), S > 0.5.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE APPLICANT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to the AI evaluation)
   WHEN: immediate (During the application process)
   WHERE: trapped (No alternative to the AI screening)
   SCOPE: local (The specific job application)
   
   WHY THIS CLASSIFICATION:
   For the applicant, the AI evaluator is a "Snare." It's an opaque,
   non-negotiable barrier where their digital twin is judged by unknown
   criteria. The process extracts performance labor and can lead to
   rejection without clear feedback, strangling their opportunity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_evaluators_matching,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EMPLOYER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The company using the AI)
   WHEN: biographical (Hiring cycles)
   WHERE: arbitrage (Can choose between different AI vendors)
   SCOPE: regional (Their talent pool)
   
   WHY THIS CLASSIFICATION:
   For the employer, the AI evaluator is a "Rope." It's a powerful tool
   for coordinating the immense task of sifting through thousands of
   applicants, improving efficiency and matching quality. It helps them
   manage a complex market and achieve their hiring goals.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_evaluators_matching,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ai_evaluators_matching_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(ai_evaluators_matching, snare, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_evaluators_matching, rope, context(agent_power(institutional), _, _, _)),
    true.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(regional)),
    constraint_indexing:extractiveness_for_agent(ai_evaluators_matching, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(ai_evaluators_matching, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(ai_evaluators_matching_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. BASE EXTRACTIVENESS (0.75):
 *    Reasoning: High extraction due to the "performance labor" imposed on applicants and the opacity of the evaluation criteria.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Selected Analytical (Tangled Rope), Powerless (Snare), and Institutional (Rope) to capture the conflict between efficiency, applicant experience, and systemic bias.
 * 
 * 3. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        ai_evaluators_matching_extraction_intent,
 *        "Is the 0.75 extraction a functional necessity for market congestion or a predatory choice to reduce labor costs?",
 *        resolution_mechanism("Audit of hiring firm resource allocation for applicant feedback vs AI licensing costs"),
 *        impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
 *        confidence_without_resolution(medium)
 *    ).
 * 
 *    omega_variable(
 *        explainability_frontier,
 *        "Will XAI (Explainable AI) allow applicants to understand and contest rejections effectively?",
 *        resolution_mechanism("Tracking enforcement of EU AI Act Article 13 and NYC Law 144 audit transparency in 2026"),
 *        impact("If Yes: Tangled Rope untangles into a Rope. If No: It tightens into a Snare."),
 *        confidence_without_resolution(low)
 *    ).
 * 
 *    omega_variable(
 *        proxy_variable_suppression,
 *        "Can algorithms successfully strip 'proxy variables' (e.g., ZIP codes as race indicators) while maintaining matching accuracy?",
 *        resolution_mechanism("Regulatory review of Illinois HB-3773 and California FEHA bias-testing results in 2026"),
 *        impact("If Yes: Systemic bias decreases. If No: The 'Black Box' remains a source of automated discrimination."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Human-First Structured Review
 * Viability: Historically primary; ensures high nuance and empathy.
 * Suppression: Actively suppressed by firms due to high labor cost and the speed of AI filters.
 * * ALTERNATIVE 2: Open-Weight Evaluation
 * Viability: Allowing candidates to see the scoring weights to adjust their "digital twin".
 * Suppression: Rejected by vendors as a threat to proprietary IP and "gaming" of the system.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/ai_evaluators_matching].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(ai_evaluators_matching).
 * 
 * 3. Run tests:
 *    ?- run_tests(ai_evaluators_matching_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(ai_evaluators_matching).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(ai_evaluators_matching, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
