% ============================================================================
% CONSTRAINT STORY: castration_longevity_choice
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Blog Post "Ask Your Doctor: Is Castration Right for You?"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(castration_longevity_choice, []).

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
 * * constraint_id: castration_longevity_choice
 * human_readable: The Castration-Longevity Trade-off
 * domain: technological/social/biological
 * temporal_scope: Historical (Korean Eunuchs) to 2026 (Modern Longevity Research)
 * spatial_scope: Global (Medical Ethics and Biology)
 * * SUMMARY:
 * This constraint explores the hypothesis that male sex hormones decrease lifespan, 
 * positioning castration as a potential biological lever for longevity. 
 * While supported by historical data and animal studies (dogs), it is 
 * constrained by modern medical ethics, institutional disapproval, and 
 * social pathologization.
 * * KEY AGENTS:
 * - Longevity Seeker: (Individual Powerless/Moderate) The male volunteer considering 
 * biological sacrifice for time.
 * - Institutional Review Board (IRB): (Institutional) The gatekeepers of ethical 
 * medical experimentation.
 * - Biological Researcher: (Analytical) The observer of Korean eunuch and canine 
 * longevity data.
 * * NARRATIVE ARC:
 * The constraint functions as a biological trade-off where reproductive 
 * capability is "extracted" in exchange for additional years of life. 
 * Socially, it acts as a filter where seekers are redirected to psychiatric 
 * rather than surgical interventions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(castration_longevity_choice, 0, 10).
narrative_ontology:constraint_claim([castration_longevity_choice], [biological_intervention]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Castration is a near-total extraction of reproductive agency 
% and hormonal identity in exchange for a theoretical longevity gain.
domain_priors:base_extractiveness(castration_longevity_choice, 0.8).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: The option is socially suppressed via ridicule ("hilarious") 
% and institutional barriers that prevent formal study.
domain_priors:suppression_score(castration_longevity_choice, 0.5).

% Enforcement requirements
% Rationale: Requires active enforcement by IRBs and medical professionals 
% who would refer seekers to psychiatrists.
domain_priors:requires_active_enforcement(castration_longevity_choice).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(castration_longevity_choice, extractiveness, 0.8).
narrative_ontology:constraint_metric(castration_longevity_choice, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(castration_longevity_choice, the_extended_lifespan_subject). % Potential 19% increase
constraint_victim(castration_longevity_choice, reproductive_capability). % Permanent extraction

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LONGEVITY SEEKER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A man seeking to extend life but blocked by ethics/stigma)
   WHEN: biographical (The scale of a human life expectancy, ~75-89 years)
   WHERE: trapped (Bounded by aging biology and medical disapproval)
   SCOPE: local (Individual body)
   
   WHY THIS CLASSIFICATION:
   The seeker experiences this as a "Snare" because the biological cost is 
   extractive and the social path leads to pathologization rather than agency 
  . They are trapped between certain aging and social ridicule.
   
   NARRATIVE EVIDENCE:
   "The chances she’ll refer me to a good psychiatrist rather than a good 
   surgeon?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    castration_longevity_choice,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(castration_longevity_choice, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MEDICAL INSTITUTION (IRB) - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (The board defining ethics and safety)
   WHEN: historical (Rooted in long-standing medical ethics)
   WHERE: constrained (Bound by current standards of "beneficence")
   SCOPE: global (Western medical standards)
   
   WHY THIS CLASSIFICATION:
   For the institution, the ethical barrier to such a study is a "Mountain"—an 
   immovable wall based on current social and professional conditioning that 
   treats such elective procedures as unethical or "madness".
   
   NARRATIVE EVIDENCE:
   "Could an experiment of this sort even make it through an Institutional 
   Review Board... Is even considering a study like this ethical?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    castration_longevity_choice,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL ANALYST - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of Korean eunuch and dog data)
   WHEN: civilizational (Evolutionary trade-offs of sex hormones)
   WHERE: analytical (Unconstrained by social conditioning)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, castration is a "Rope"—a biological coordination mechanism 
   that allows an organism to bypass the lifespan-shortening effects of 
   sex hormones to achieve greater longevity.
   
   NARRATIVE EVIDENCE:
   "Our study supports the idea that male sex hormones decrease the lifespan 
   of men".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    castration_longevity_choice,
    rope,
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

:- begin_tests(castration_longevity_choice_tests).

test(multi_perspective_longevity) :-
    % Seeker sees Snare
    constraint_indexing:constraint_classification(castration_longevity_choice, snare, context(individual_powerless, biographical, trapped, local)),
    % Institution sees Mountain
    constraint_indexing:constraint_classification(castration_longevity_choice, mountain, context(institutional, historical, constrained, global)),
    % Analyst sees Rope
    constraint_indexing:constraint_classification(castration_longevity_choice, rope, context(analytical, civilizational, analytical, global)).

test(extraction_mandatrophy) :-
    % High extraction score (0.8) should trigger the Mandatrophy check
    domain_priors:base_extractiveness(castration_longevity_choice, E),
    E > 0.7.

test(social_suppression) :-
    % Alternatives (Calorie Restriction) are visible but this specific 
    % choice is socially suppressed.
    domain_priors:suppression_score(castration_longevity_choice, S),
    S >= 0.5.

:- end_tests(castration_longevity_choice_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: Elective castration is the ultimate biological extraction of 
 * future reproductive potential and current endocrine health.
 * * 2. PERSPECTIVE SELECTION:
 * The model chose to contrast the seeker (powerless against aging/social 
 * stigma) with the institution (gatekeepers of "ethics") and the analyst 
 * (objective observer of the longevity gain).
 * * 3. MANDATROPHY RESOLUTION:
 * The constraint is a Rope for longevity (the goal) but a Snare for the 
 * individual's reproductive and social status. This asymmetry is 
 * resolved by the high indexical variance between biological data 
 * and social pathologization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modern_longevity_translation,
    "Does the 14-19 year longevity benefit seen in historical eunuchs translate to modern males with high-quality healthcare and nutrition?",
    resolution_mechanism("Long-term cohort study of males castrated for prostate cancer or other medical reasons vs. matched controls"),
    impact("If yes: Longevity-Rope is validated. If no: Sacrifice is a Snare with no payout."),
    confidence_without_resolution(low)
).

omega_variable(
    castration_longevity_choice_extraction_intent,
    "Is the social redirection to psychiatry a functional protection of the subject (Mountain) or an extractive suppression of life-extension agency (Snare)?",
    resolution_mechanism("Audit of IRB decisions regarding life-extending biological modifications"),
    impact("If protection: Institutional Mountain. If suppression: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Calorie Restriction (CR)
 * Viability: Historically the "only scientific claim" for prolonging life 
 *.
 * Suppression: Bounded by the difficulty of long-term adherence in an 
 * environment "conducive to overfeeding".
 * * CONCLUSION:
 * The absence of viable, easy alternatives (CR is difficult, castration is 
 * stigmatized) shifts the biological reality of aging from a manageable Rope 
 * into an inescapable Mountain for most agents.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [castration_longevity_choice].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
