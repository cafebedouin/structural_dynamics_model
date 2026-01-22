% ============================================================================
% CONSTRAINT STORY: section_469_c7_professional_threshold
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: 26 USC 469(c)(7) - Special rules for taxpayers in real property business
% ============================================================================

:- module(section_469_c7_professional_threshold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

narrative_ontology:interval(section_469_c7_professional_threshold, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: section_469_c7_professional_threshold
 * human_readable: The Real Estate Professional 750-Hour Gateway
 * domain: economic/legal
 * temporal_scope: Annual Tax Filing Cycle
 * spatial_scope: National (US Federal)
 * * SUMMARY:
 * Section 469(c)(7) provides an exception to the general rule that rental 
 * activities are per se passive. To 
 * qualify, a taxpayer must perform more than 50% of their personal services 
 * in real property trades and exceed 750 hours of service annually. This creates a binary state: one is either 
 * "Professional" (active losses allowed) or "Amateur" (losses disallowed).
 * * KEY AGENTS:
 * - Hybrid_Worker: An individual with a W-2 job and rental properties.
 * - Full_Time_Developer: An individual whose primary income is real estate.
 * - Auditor: The enforcer of the hourly log.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Base extractiveness: Moderate (0.5) 
% It forces a massive time commitment to unlock tax benefits.
domain_priors:base_extractiveness(section_469_c7_professional_threshold, 0.5).

% Suppression: Moderate-High (0.65)
% Services as an employee generally do not count toward the 750 hours 
% unless the employee is a 5-percent owner.
domain_priors:suppression_score(section_469_c7_professional_threshold, 0.65).

domain_priors:requires_active_enforcement(section_469_c7_professional_threshold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Hybrid Worker (W-2 + Rentals) - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless - Cannot satisfy the "50% of services" rule 
   because their primary job consumes their time.
   EXIT: trapped - Cannot reclassify income without quitting their job.
   
   WHY THIS CLASSIFICATION:
   The 750-hour rule is a Noose because it is mathematically impossible for 
   someone working a full-time (2,000 hour) job to spend >50% of their time 
   in real estate. It extracts tax revenue by 
   disallowing losses that are economically real but legally "passive."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    section_469_c7_professional_threshold,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(section_469_c7_professional_threshold, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Full-Time Developer - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has the career flexibility to hit thresholds.
   EXIT: mobile - Uses the "election to treat all interests as one activity" 
   to simplify the burden.
   
   WHY THIS CLASSIFICATION:
   For the professional, the rule is a Rope—a tool for coordination. It 
   separates "true" industry participants from "dabblers," rewarding the 
   former with the ability to use rental losses to offset other active 
   income.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    section_469_c7_professional_threshold,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:requires_active_enforcement(section_469_c7_professional_threshold),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Tax Court / Auditor - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: institutional - Enforces the strict hourly definitions.
   EXIT: analytical - Neutral application of the "material participation" tests.
   
   WHY THIS CLASSIFICATION:
   To the institution, the 750-hour and 50% thresholds are objective constants 
   of the tax universe. They are not "negotiable" 
   functions but rather the boundaries of the legal definitions of trade 
   and business.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    section_469_c7_professional_threshold,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(section_469_c7_professional_threshold, E),
    E >= 0.5,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(section_469_c7_tests).

test(threshold_binary_flip) :-
    % Test that changing power/exit status flips the constraint type
    constraint_indexing:constraint_classification(section_469_c7_professional_threshold, noose, context(agent_power(individual_powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(section_469_c7_professional_threshold, rope, context(agent_power(individual_moderate), _, mobile, _)).

test(employment_suppression_logic) :-
    % Employee services (non-owner) are suppressed/excluded
    domain_priors:suppression_score(section_469_c7_professional_threshold, S),
    S > 0.5.

:- end_tests(section_469_c7_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): This is higher than the general PAL rule because 
 * it demands "Personal Services". The state extracts 
 * either tax revenue OR 750 hours of human labor.
 * 2. PERSPECTIVE GAP: The friction here is most visible for the "Hybrid 
 * Worker" (W-2 + Real Estate). The code 
 * explicitly prevents them from "counting" their job time toward the 
 * real estate requirement unless they own 5% of the company.
 */


