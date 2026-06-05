% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The Work Behind the Writing: On Writers and Their Day Jobs" by Ed Simon
% ============================================================================

:- module(constraint_necessary_day_job, []).

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
 * 
 * constraint_id: necessary_day_job
 * human_readable: The Necessary Day Job for Creatives
 * domain: economic/social
 * temporal_scope: 19th Century to 2026
 * spatial_scope: National (USA)
 * 
 * SUMMARY:
 * The "necessary day job" is the economic constraint where creative vocation is
 * decoupled from subsistence labor. Writers and artists must perform unrelated,
 * often "boring" or "sterile" work to fund their creative passions.
 * 
 * KEY AGENTS:
 * - The Trapped Artist (Individual Powerless): e.g., Herman Melville, working a stagnant job for decades.
 * - The Functional Artist (Individual Moderate): e.g., William Carlos Williams, whose job feeds their art.
 * - The Publisher/University (Institutional): The system that benefits from a pool of self-supported talent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(necessary_day_job, 0, 10).
narrative_ontology:constraint_claim(necessary_day_job, snare).

% Base extractiveness score (0.65): High, due to "abysmal pay" for adjuncts
% or the stagnant wages of Melville, where labor is extracted for survival,
% leaving little for creative work.
domain_priors:base_extractiveness(necessary_day_job, 0.65).

% Suppression score (0.7): High. Economic alternatives (living solely off one's art)
% are rare and suppressed by market indifference or winner-take-all dynamics.
domain_priors:suppression_score(necessary_day_job, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.65).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.7).

% Enforcement: Requires active enforcement (the need to pay bills).
domain_priors:requires_active_enforcement(necessary_day_job).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(necessary_day_job, employers).
constraint_victim(necessary_day_job, artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: HERMAN MELVILLE (THE TRAPPED ARTIST) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - Devastated by criticism, working without raises.
   WHEN: biographical - Nineteen years of service until retirement.
   WHERE: trapped - "Everlasting itch for things remote" while stuck at a desk.
   
   WHY THIS CLASSIFICATION:
   For Melville, the Customs House was an unchangeable 'Mountain'. He could not
   alter his pay or the market's rejection of his work; he could only
   endure it as a natural law of his life, a permanent feature of his reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    necessary_day_job,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: WILLIAM CARLOS WILLIAMS (THE FUNCTIONAL ARTIST) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - A professional with specialized skills (physician).
   WHEN: biographical - Career-long integration of job and art.
   WHERE: mobile - His job provides material and inspiration for his art.
   
   WHY THIS CLASSIFICATION:
   For Williams, the day job was a 'Rope': a functional coordination mechanism
   that provided both economic stability and the raw data/inspiration
   (patients, observations) required for his "real" work as a poet.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    necessary_day_job,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNIVERSITY SYSTEM (THE EMPLOYER) - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional - Hiring adjuncts to teach classes.
   WHEN: generational - A systemic reliance on cheap, flexible academic labor.
   WHERE: arbitrage - Can hire from a large pool of PhDs with few tenure-track options.
   
   WHY THIS CLASSIFICATION:
   For the university, adjunct labor is a 'Tangled Rope'. It's a 'Rope' because it
   allows the institution to offer a wide variety of courses flexibly and cheaply.
   It becomes 'Tangled' because this reliance creates a demoralized, underpaid
   workforce, damaging the institution's long-term pedagogical mission.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    necessary_day_job,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(day_job_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(necessary_day_job, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(day_job_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'University System' as the institutional
 *    agent. This captures the dynamic where the "day job" of adjuncting is a systemic
 *    feature that benefits the institution's budget while exploiting the artist/scholar.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Trapped Artist (Mountain): An inescapable economic reality.
 *    - Functional Artist (Rope): A source of stability and inspiration.
 *    - University (Tangled Rope): A useful but ultimately self-damaging labor strategy.
 * 
 * 3. EXTRACTIVENESS (0.65): High, reflecting the "abysmal pay" of adjuncting and
 *    the general principle that the day job extracts the artist's most valuable
 *    resource: time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the 'day job' is a net positive or negative for creativity.
 */

omega_variable(
    vocational_ecstasy_vs_labor,
    "Can the 'ecstasy' and creative material provided by a job (Rope) truly coexist with its 'soul-crushing' anonymity and time-theft (Snare)?",
    resolution_mechanism("Qualitative analysis of 'transcendence' vs. 'burnout' reports in the diaries and letters of artists with day jobs."),
    impact("If Rope dominates: The day job is a net positive for art. If Snare dominates: The day job is a net negative."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Independent Wealth / Patronage
 *    Viability: Historically the primary model for artists, but rare today.
 *    Suppression: Limited by socioeconomic starting conditions and the decline of traditional patronage.
 * 
 * ALTERNATIVE 2: The Tenure System
 *    Viability: Once a viable path for academic-writers in the post-WWII era.
 *    Suppression: Systemically eroded by the rise of adjunctification, making it unavailable to most.
 *
 * CONCLUSION:
 * The "necessary day job" constraint becomes more powerful as the alternatives are suppressed.
 * The decline of tenure and patronage forces more artists into the precarious and often
 * extractive position of balancing their art with unrelated labor.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/necessary_day_job].
 * 2. Multi-perspective: ?- multi_index_report(necessary_day_job).
 * 3. Run tests: ?- run_tests(day_job_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */