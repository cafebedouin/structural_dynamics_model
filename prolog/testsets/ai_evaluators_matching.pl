% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: ai_evaluators_matching
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: AI-Driven Recruitment / Automated Talent Assessment
% ============================================================================

:- module(constraint_ai_evaluators, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: ai_evaluators_matching
 * human_readable: AI-Evaluators (Automated Gatekeeping)
 * domain: technological/economic
 * temporal_scope: Immediate to Biographical
 * spatial_scope: Global
 * * SUMMARY:
 * AI-Evaluators are the algorithmic layer between applicants and matching 
 * markets. They use Large Language Models (LLMs), video analysis, and game-based 
 * assessments to rank candidates. This shifts the constraint from "Human 
 * Discretion" to "Mathematical Feature Optimization."
 * * KEY AGENTS:
 * - The Applicant: Must optimize their "digital twin" to pass the filter.
 * - The Hiring Firm: Offloads the high-congestion filtering to a black-box system.
 * - The Model Developer: Designs the weights and features that define "merit."
 * * NARRATIVE ARC:
 * The constraint functions as a "Black-Box Sieve." It solves the problem of 
 * market congestion (too many applicants) but introduces a Noose of 
 * opacity—where applicants are rejected for reasons they cannot see, 
 * contested, or understand.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(ai_evaluators_matching, 0, 10).
narrative_ontology:constraint_claim(ai_evaluators_matching, rope).

% Base extractiveness: High (0.75).
% AI systems extract behavioral data and "performance labor" from applicants, 
% often without compensation or feedback.
domain_priors:base_extractiveness(ai_evaluators_matching, 0.75).

% Suppression: High (0.8).
% Human review is increasingly suppressed/unavailable for early-stage filters; 
% the "AI-first" path is effectively the only gate.
domain_priors:suppression_score(ai_evaluators_matching, 0.8).

% Enforcement: Emerges naturally from technological efficiency gains.
domain_priors:emerges_naturally(ai_evaluators_matching).

% Beneficiaries: Large Corporations (efficiency), AI SaaS Providers.
constraint_beneficiary(ai_evaluators_matching, large_scale_employers).

% Victims: "Outlier" Candidates (highly capable but don't fit the model's 
% statistical patterns).
constraint_victim(ai_evaluators_matching, neurodivergent_or_atypical_applicants).

% Metrics
narrative_ontology:constraint_metric(ai_evaluators_matching, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_evaluators_matching, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE APPLICANT - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped (opaque criteria, no appeal)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the applicant, AI-Evaluators are a "Noose." They are forced to perform 
   for an invisible set of weights. Because the logic is often proprietary 
   or unexplainable, the candidate is trapped in a loop of "A/B testing" 
   their own personality just to get a human to look at their resume.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_evaluators_matching,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(ai_evaluators_matching, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RECRUITER - Rope
   --------------------------------------------------------------------------
   WHO: institutional/individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To a recruiter handling 5,000 applications for one role, AI is a "Rope." 
   It pulls them out of the "Mountain" of paperwork and provides a 
   manageable shortlist. It is a coordination tool that makes the 
   "Match" possible in an age of hyper-congestion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_eval_matching,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DATA SCIENTIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The scientist views the AI evaluator as a "Mountain" of statistical 
   probability. The model simply reflects the patterns in the training data. 
   If the data says "X correlates with Y," that is a feature of the 
   mathematical landscape, not a subjective choice.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_eval_matching,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(ai_eval_matching),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(ai_evaluator_tests).

test(opacity_noose) :-
    % Testing that 'trapped' applicants experience high-extractiveness Noose.
    constraint_indexing:constraint_classification(ai_evaluators_matching, noose, context(_, _, trapped, _)).

test(congestion_reduction_rope) :-
    % Testing that institutions see the Rope for managing congestion.
    constraint_indexing:constraint_classification(ai_eval_matching, rope, context(institutional, _, _, _)).

:- end_tests(ai_evaluator_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * I set extractiveness high (0.75) because AI-Evaluators require the 
 * "performance" of the applicant while providing almost zero transparency 
 * in return. This is the ultimate "Asymmetric Match."
 * * OMEGAS:
 * 1. Can AI ever be truly "explainable"? If not, the Noose classification 
 * for applicants is permanent.
 */

omega_variable(
    explainability_frontier,
    "Will neural networks become transparent enough to provide specific reasons for rejection?",
    resolution_mechanism("Tracking XAI (Explainable AI) research and legislative mandates (e.g., EU AI Act)"),
    impact("If Yes: The Noose becomes a Rope (candidates can adapt). If No: The Noose remains a black box."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ai_evaluators_matching, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ai_evaluators_matching, rope, agent_power(institutional)).
