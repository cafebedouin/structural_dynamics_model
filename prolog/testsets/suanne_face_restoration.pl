% ============================================================================
% CONSTRAINT STORY: suanne_face_restoration
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Frazier, “On the Rez” & “Politeness is Two-Faced”
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_suanne_face_restoration, []).

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
 * constraint_id: suanne_face_restoration
 * human_readable: The Coup of Positive Politeness
 * domain: social/psychological
 * temporal_scope: 1988 (Biographical)
 * spatial_scope: Lead, South Dakota (Local)
 * 
 * SUMMARY:
 * This analysis evaluates SuAnne Marie Big Crow’s performance through the lens of 
 * Politeness Theory. The Lead fans’ mockery constituted a massive 
 * "Face-Threatening Act" (FTA). SuAnne’s "Coup of Peace" functioned 
 * as a masterful "Positive Politeness" strategy, restoring the team's Face by 
 * inviting the crowd into a shared "we-focused" identity.
 * 
 * KEY AGENTS:
 * - The Face-Victim (Individual Powerless): The team feeling the "Snare" of the face-attack.
 * - School Administration / Athletic Association (Institutional): Manages inter-school relations.
 * - The Face-Restorer (Individual Moderate): SuAnne, using competence as a "Positive Politeness" tool.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(suanne_face_restoration, 0, 10).
narrative_ontology:constraint_claim(suanne_face_restoration, tangled_rope).

% Base extractiveness: 0.75.
% The initial face-attack liquidates the team's social status 
% and dignity to feed the crowd's sense of superiority.
domain_priors:base_extractiveness(suanne_face_restoration, 0.75).

% Suppression score: 0.65.
% The mockery suppresses the "real" Lakota identity 
% behind "fake" caricatures. SuAnne’s act un-suppresses this truth.
domain_priors:suppression_score(suanne_face_restoration, 0.65).

% Enforcement: Requires active enforcement by the crowd and subsequent social shifts.
domain_priors:requires_active_enforcement(suanne_face_restoration).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(suanne_face_restoration, inter_community_peace).
constraint_victim(suanne_face_restoration, racial_mockery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISHONORED TEAM - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (A visiting team under racialized face-attack)
   WHEN: immediate (The pre-game "din" of harassment)
   WHERE: trapped (Caught in a hostile "standard of thinking")
   
   WHY THIS CLASSIFICATION:
   For the teammates, the crowd's mockery and taunts are a 'Snare.' These acts
   are "attacks on our positive face" that discuss identity in belittling ways,
   strangling the team's ability to act competently and their sense of belonging.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    suanne_face_restoration,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SCHOOL ADMINISTRATION / ATHLETIC ASSOCIATION - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages inter-school relations and sportsmanship)
   WHEN: historical (The long-term relationship between schools and communities)
   WHERE: arbitrage (Balances maintaining order with fostering positive relations)
   
   WHY THIS CLASSIFICATION:
   For a school administration or athletic association, this event is a 'Tangled Rope'.
   It highlights the potential for student-led conflict resolution ('Rope') and
   the creation of "lasting friendships." However, it is 'Tangled' by the institutional
   failure to prevent the initial hostile environment and the underlying racial tensions,
   representing a complex challenge to their mandate.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    suanne_face_restoration,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SUANNE (THE STRATEGIST) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "step into the jump-ball circle")
   WHEN: biographical (Drawing on Powwow skills and personal courage)
   WHERE: mobile (Re-mapping the "fake" tune to "the real thing")
   
   WHY THIS CLASSIFICATION:
   SuAnne uses "Positive Politeness" as a 'Rope'. By performing a "graceful and 
   show-offy" dance, she asserts her competence (Positive Face) while using the 
   jacket to "invite us all to play," restoring the "Face" of her team through 
   inclusion rather than retaliation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    suanne_face_restoration,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(suanne_face_restoration_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(suanne_face_restoration, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_face_restoration, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(suanne_face_restoration, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(suanne_face_restoration_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'School Administration / Athletic Association'
 *    as the institutional agent. For them, this event is a 'Tangled Rope', a
 *    complex challenge of managing conflict and fostering positive relations.
 *
 * 2. MANDATROPHY STATUS: The high extraction of the team's "Face" is 'RESOLVED'
 *    by the 'Rope' of SuAnne's performance, which masterfully transforms a hostile
 *    'Snare' into an inclusive social experience.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    - The Team (Snare): Overwhelmed by face-threatening acts.
 *    - Administration (Tangled Rope): Manages the conflict and its aftermath.
 *    - SuAnne (Rope): A tool for social restoration and inclusion.
 *
 * 4. CORE INSIGHT: The story is a powerful example of how a single agent's
 *    skillful use of a 'Rope' (Positive Politeness) can untangle a 'Snare' of
 *    social hostility and create a new, more positive 'Mountain' of shared understanding.
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
    suanne_face_restoration_intent,
    "Was the crowd's silence a biological recognition of 'the real thing' (Mountain), or a temporary strategic 'Rope' of confusion, allowing them to recalibrate their social strategy without losing face?",
    resolution_mechanism("Audit of long-term community shifts in Lead, South Dakota, post-SuAnne's performance, vs. communities with no such intervention."),
    impact("If Mountain: Authenticity is a social law with predictable effects. If Rope: It is a high-risk gamble that may not be replicable."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Retaliatory FTA (Aggression)
 *    Viability: Mocking the Lead fans back in a similar fashion.
 *    Suppression: Suppressed because it would "embarrass" the team and likely escalate the 'Snare' of hostility, leading to a worse outcome.
 *
 * CONCLUSION:
 * SuAnne's "Positive Politeness" was a masterful 'Rope' that navigated a
 * complex social dilemma. It actively suppressed the alternative of retaliation,
 * which would have tightened the 'Snare', and instead transformed a hostile
 * environment into one of mutual respect.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/suanne_face_restoration].
 * 2. Multi-perspective: ?- multi_index_report(suanne_face_restoration).
 * 3. Run tests: ?- run_tests(suanne_face_restoration_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */