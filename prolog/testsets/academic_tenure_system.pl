% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: General Academic Sociology / Labor Market Analysis
% ============================================================================

:- module(constraint_academic_tenure, []).

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
 * * constraint_id: academic_tenure_system
 * human_readable: The Academic Tenure and Promotion System
 * domain: social/economic
 * temporal_scope: 1915-Present (Modern AAUP Standards)
 * spatial_scope: National/Global (Research Universities)
 * * SUMMARY:
 * The tenure system is a contractual right to near-permanent employment granted 
 * after a rigorous probationary period. While designed to protect academic 
 * freedom, it creates a bifurcated labor market that strictly constrains the 
 * behavior, research choices, and mobility of pre-tenure faculty.
 * * KEY AGENTS:
 * - Assistant Professor: Individual powerless; must navigate the "up-or-out" 
 * probationary period.
 * - Tenured Faculty/Department Chair: Institutional; hold rule-making and 
 * evaluation power over junior colleagues.
 * - Adjunct/Non-Tenure Track Faculty: Trapped; excluded from the tenure 
 * mechanism while subject to its labor externalities.
 * * NARRATIVE ARC:
 * Tenure begins as a Rope (coordinating long-term research and institutional 
 * stability). For the junior faculty member, the "tenure clock" is a 
 * Mountain—an unchangeable 6-year countdown. As the "tenure-track" shrinks, 
 * the system tightens into a Noose, extracting extreme labor from junior 
 * faculty and excluding the "adjunctified" majority from security.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(tenure_track_cycle, 0, 10).
narrative_ontology:constraint_claim(academic_tenure_system, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.75. High extraction; the system relies on the hyper-productivity 
% of pre-tenure faculty to boost institutional rankings and grant revenue.
domain_priors:base_extractiveness(academic_tenure_system, 0.75).

% Suppression score (0.0-1.0)
% Rationale: 0.6. Alternatives (long-term contracts, portable security) are 
% visible but suppressed by the prestige and "locked-in" nature of the 
% current university model.
domain_priors:suppression_score(academic_tenure_system, 0.6).

% Enforcement requirements
% Requires active enforcement (Departmental reviews, external letters, 
% university provost approval).
domain_priors:requires_active_enforcement(academic_tenure_system).

% Metrics required for Executive Summary
narrative_ontology:constraint_metric(academic_tenure_system, extractiveness, 0.75).
narrative_ontology:constraint_metric(academic_tenure_system, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(academic_tenure_system, research_universities).
constraint_beneficiary(academic_tenure_system, tenured_senior_faculty).
constraint_victim(academic_tenure_system, pre_tenure_faculty_wellbeing).
constraint_victim(academic_tenure_system, risky_non_conformist_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ASSISTANT PROFESSOR - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to evaluation; cannot alter the clock.
   WHEN: biographical - The 6-7 year probationary window is all-encompassing.
   WHERE: trapped - Mobility is limited; failure to get tenure is often a 
         "death sentence" for a research career in that tier.
   SCOPE: local - Their specific department and its peculiar culture.
   
   WHY THIS CLASSIFICATION:
   For the junior scholar, the tenure requirements are a Mountain. They are 
   set in stone long before they arrive. One cannot "negotiate" with the 
   expected number of publications or the internal politics of the 
   senior committee.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    academic_tenure_system,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(academic_tenure_system),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: UNIVERSITY LEADERSHIP - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to grant security and define "excellence."
   WHEN: generational - Protecting the university's mission over decades.
   WHERE: mobile - Can adjust criteria or create non-tenure pipelines.
   SCOPE: national - Competing in the global market for elite talent.
   
   WHY THIS CLASSIFICATION:
   Leadership views tenure as a Rope—a functional coordination tool. It 
   guarantees that the best minds stay at the institution long-term and 
   provides the stability needed for "deep work" that market-based 
   employment might discourage.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    academic_tenure_system,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(academic_tenure_system, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LABOR ECONOMIST / REFORMER - Noose
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the broader academic labor market.
   WHEN: historical - Tracking the decline of tenure-track lines since 1970.
   WHERE: analytical - Not bound by the social prestige of the "club."
   SCOPE: global - Impact on the future of the teaching and research labor force.
   
   WHY THIS CLASSIFICATION:
   The reformer sees the Noose. The system extracts 60-80 hours of labor 
   per week from the most vulnerable faculty in exchange for a 
   diminishing chance at security. By "locking out" most new PhDs into 
   adjunct roles, it chokes the diversity and sustainability of the profession.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    academic_tenure_system,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(academic_tenure_system, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(academic_tenure_tests).

test(multi_perspective_gap) :-
    % Junior Faculty (Mountain) vs Admin (Rope) vs Reformer (Noose)
    constraint_indexing:constraint_classification(academic_tenure_system, T1, context(individual_powerless, biographical, trapped, local)),
    constraint_indexing:constraint_classification(academic_tenure_system, T2, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(academic_tenure_system, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(labor_extraction_check) :-
    domain_priors:base_extractiveness(academic_tenure_system, E),
    E > 0.7.

:- end_tests(academic_tenure_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.75): The tenure track is famously extractive. It 
 * front-loads the work of a 40-year career into 6 years, with the 
 * university capturing the "surplus value" of that output.
 * 2. CLASSIFICATION: I primary-labeled this as 'Rope' because, 
 * fundamentally, its original purpose (academic freedom) is a 
 * coordination good for democracy.
 */

omega_variable(
    academic_freedom_utility,
    "Does tenure still provide a unique protection for academic freedom that 
     standard multi-year contracts cannot?",
    resolution_mechanism("Compare censorship rates and research risk-taking in tenure-heavy vs. contract-heavy systems"),
    impact("If Yes: It is a vital Rope. If No: It is an extractive Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Rolling Multi-Year (e.g., 5-year) Renewable Contracts
 * Viability: High; provides security without the "binary" win/loss 
 * of tenure. Used in some international systems.
 * Suppression: Suppressed by the "gold standard" status of tenure and 
 * faculty resistance to losing permanent security.
 * * CONCLUSION:
 * The institutionalized status of tenure as the ONLY "real" job in academia 
 * turns it from a coordination Rope into an unyielding Mountain for new entrants.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [academic_tenure_system].
% 2. Analyze: ?- multi_index_report(academic_tenure_system).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
