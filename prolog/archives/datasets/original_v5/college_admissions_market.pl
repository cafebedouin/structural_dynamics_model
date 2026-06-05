% ============================================================================
% CONSTRAINT STORY: college_admissions_market
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Market Design / College Admissions Problem (Gale-Shapley variant)
% ============================================================================

:- module(constraint_college_admissions, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: college_admissions_market
 * human_readable: The College Admissions Match
 * domain: social/economic/technological
 * temporal_scope: Biographical (Single Application Cycle)
 * spatial_scope: National
 * * SUMMARY:
 * College admissions is a "many-to-one" matching market. Unlike the marriage 
 * problem, one side (colleges) has quotas. The constraint is defined by the 
 * tension between student preferences and university ranking criteria (GPA, 
 * SAT, extracurriculars).
 * * KEY AGENTS:
 * - The Applicant: Seeks the highest-prestige match within their reach.
 * - The Admissions Office: Seeks to optimize a "class profile" under quota constraints.
 * - The Ranking Algorithm: The invisible hand (US News, etc.) that influences agent preferences.
 * * NARRATIVE ARC:
 * The constraint functions as a "High-Stakes Filter." For students, it is 
 * a bottleneck where years of preparation are compressed into a single binary 
 * decision. For the system, it is a sorting mechanism to maintain social hierarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(college_admissions_market, 0, 10).
narrative_ontology:constraint_claim(college_admissions_market, rope).

% Base extractiveness: High (0.7).
% Elite colleges extract massive "signaling costs" (tuition, prep-work, emotional labor) 
% from thousands of applicants to select only a few.
domain_priors:base_extractiveness(college_admissions_market, 0.7).

% Suppression: Moderate (0.5).
% While "alternative" paths exist (trade schools, gap years), the social 
% cost of opting out of the elite match is high for certain classes.
domain_priors:suppression_score(college_admissions_market, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(college_admissions_market, extractiveness, 0.7).
narrative_ontology:constraint_metric(college_admissions_market, suppression_requirement, 0.5).

% Enforcement: Requires active enforcement (Deadlines, standardized testing).
domain_priors:requires_active_enforcement(college_admissions_market).

% Beneficiaries: Elite Institutions (endowment growth, prestige maintenance).
constraint_beneficiary(college_admissions_market, elite_universities).

% Victims: The "Waitlisted/Rejected" (who paid the signaling cost but received no match).
constraint_victim(college_admissions_market, unmatched_applicants).

% Metrics
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE REJECTED APPLICANT - Snare
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate (one-year cycle)
   WHERE: trapped (cannot easily change credentials after submission)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the student who spent years building a profile only to be rejected, 
   the matching logic is a "Snare." The sunk costs are high, and the 
   perceived lack of alternatives (at the same prestige level) feels 
   punitive and extractive.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    college_admissions_market,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(college_admissions_market, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LEGACY ADMIT - Rope
   --------------------------------------------------------------------------
   WHO: powerful
   WHEN: generational
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For an agent with "high signal" or legacy status, the system is a "Rope." 
   It is a coordination tool that confirms their existing status and 
   provides a reliable path to future elite opportunities.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    college_admissions_market,
    rope,
    context(
        agent_power(powerful),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNIVERSITY PROVOST - Mountain
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: analytical
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   The provost sees the admission cycle as a "Mountain." The quotas, 
   financial aid limits, and applicant volume are structural realities. 
   The algorithm (Gale-Shapley or similar) is just the "law" used to 
   manage this inevitable social sorting.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    college_admissions_market,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(regional)
    )
) :-
    domain_priors:requires_active_enforcement(college_admissions_market),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(college_admissions_tests).

test(prestige_asymmetry) :-
    % Powerful agents see the coordination (Rope); powerless see the extraction (Snare).
    constraint_indexing:constraint_classification(college_admissions_market, rope, context(powerful, _, _, _)),
    constraint_indexing:constraint_classification(college_admissions_market, snare, context(powerless, _, _, _)).

test(quota_immutability) :-
    % Institutional perspective views the constraint as a fixed Mountain.
    constraint_indexing:constraint_classification(college_admissions_market, mountain, context(institutional, _, _, _)).

:- end_tests(college_admissions_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Extractiveness is high (0.7) because college admissions requires years of 
 * pre-emptive labor (extracurriculars, testing) before the match even begins. 
 * This creates a massive "asymmetry" between those who pay the cost but 
 * aren't matched and the institutions that benefit from the large applicant pool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    prestige_valuation,
    "Will the market value of elite degrees decline due to skill-based hiring?",
    resolution_mechanism("Tracking hiring data of elite vs. non-elite grads over 10 years"),
    impact("If Yes: The 'Snare' loosens; students find 'Exit' options (Mountain -> Rope)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Lottery Admissions: (Decreases extraction/stress, but destroys prestige signal).
 * 2. Open Enrollment: (Eliminates the 'Snare' but creates 'Congestion').
 * * CONCLUSION:
 * The current system persists as a "Mountain" because elite institutions 
 * benefit from the "Snare" effect—it creates the scarcity that defines their value.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
