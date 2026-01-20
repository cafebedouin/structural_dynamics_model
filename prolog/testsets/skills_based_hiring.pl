% ============================================================================
% CONSTRAINT STORY: skills_based_hiring
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Market Design / Labor Economics / Skills-Based Hiring Trends
% ============================================================================

:- module(constraint_skills_based_hiring, []).

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
 * * constraint_id: skills_based_hiring
 * human_readable: Skills-Based Hiring (De-credentialing)
 * domain: economic/technological/social
 * temporal_scope: Biographical to Historical
 * spatial_scope: Global
 * * SUMMARY:
 * Skills-based hiring is a matching market shift where employers prioritize 
 * specific competencies and verified "proof of work" over traditional degree 
 * credentials. This moves the bottleneck from "Institutional Pedigree" 
 * (College Match) to "Granular Verification" (Skill Match).
 * * KEY AGENTS:
 * - The Non-Degree Applicant: Gains access to elite roles via technical proof.
 * - The Credentialed Elite: Faces devaluation of their institutional pedigree.
 * - The Automated Evaluator: AI/Platform tools that score specific skills.
 * * NARRATIVE ARC:
 * The constraint functions as a "Demolished Wall." By removing the degree 
 * requirement, the market "thins" the power of elite universities. However, 
 * it replaces the "Degree Noose" with a "Continuous Testing Noose," where 
 * agents must constantly prove their worth in a granular, high-frequency match.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(skills_based_hiring, 0, 10).
narrative_ontology:constraint_claim(skills_based_hiring, rope).

% Base extractiveness score (0.3)
% Lower than college admissions because it reduces the "pre-match" cost of 
% expensive degrees, though platform fees for certification may emerge.
domain_priors:base_extractiveness(skills_based_hiring, 0.3).

% Suppression score (0.2)
% Low suppression; it encourages a plurality of paths (bootcamps, self-taught).
domain_priors:suppression_score(skills_based_hiring, 0.2).

% Enforcement requirements
domain_priors:emerges_naturally(skills_based_hiring). % Driven by labor scarcity

% Metrics
narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, 0.3).
narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(skills_based_hiring, self_taught_experts).
constraint_victim(skills_based_hiring, mid_tier_liberal_arts_colleges).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NON-DEGREE APPLICANT - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (empowered by skill proof)
   WHEN: biographical
   WHERE: mobile (can work for multiple firms based on skill proof)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the self-taught or non-traditional student, skills-based hiring is 
   a "Rope." It is the first coordination mechanism that allows them to 
   climb into high-wage labor markets without the $100k "pay-to-play" 
   entry fee of a degree.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    skills_based_hiring,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(skills_based_hiring, S),
    S < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MID-TIER GRADUATE - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (pedigree is no longer a shield)
   WHEN: immediate
   WHERE: trapped (stuck with debt for a devalued credential)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For someone who relied on their degree for market access, this shift 
   is a "Noose." Their primary constraint (the degree) has been 
   demoted, forcing them into a high-competition skill-arena where 
   their "institutional signal" no longer grants them a safe exit or 
   premium wage.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    skills_based_hiring,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HR ALGORITHM - Mountain
   --------------------------------------------------------------------------
   WHO: institutional/analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the automated system, skills are "Mountains." Data is the only 
   reality. A candidate either has the verified Python competency or 
   they do not. The algorithm views the match as a cold, immutable 
   binary filter of capacity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    skills_based_hiring,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(skills_based_hiring),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(skills_hiring_tests).

test(pedigree_devaluation) :-
    % Testing that 'trapped' traditional grads experience the shift as a Noose.
    constraint_indexing:constraint_classification(skills_based_hiring, noose, context(_, _, trapped, _)).

test(access_rope) :-
    % Testing that mobile, skill-heavy agents see the Rope.
    constraint_indexing:constraint_classification(skills_based_hiring, rope, context(_, _, mobile, _)).

:- end_tests(skills_hiring_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Skills-based hiring is a "de-matching" of the traditional 
 * University-Employer pipeline. It reduces extraction by decreasing the 
 * "entry fee" (Degrees), but increases the "monitoring pressure" (Noose) 
 * as workers must continuously prove their skills in an AI-gated market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    skill_obsolescence_rate,
    "How fast will AI-driven tools make specific human skills obsolete?",
    resolution_mechanism("Tracking half-life of technical certifications in LinkedIn data"),
    impact("If High: The 'Rope' is constantly breaking; the 'Noose' of re-skilling tightens."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
