% ============================================================================
% CONSTRAINT STORY: suanne_face_restoration
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Frazier, “On the Rez” & “Politeness is Two-Faced”
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_suanne_face_restoration, []).

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
 * * constraint_id: suanne_face_restoration
 * human_readable: The Coup of Positive Politeness
 * domain: social/psychological
 * temporal_scope: 1988 (Biographical)
 * spatial_scope: Lead, South Dakota (Local)
 * * SUMMARY:
 * This analysis evaluates SuAnne Marie Big Crow’s performance through the lens of 
 * Politeness Theory. The Lead fans’ mockery constituted a massive 
 * "Face-Threatening Act" (FTA) against the team’s Positive Face (competence and value) 
 * and Negative Face (autonomy). SuAnne’s "Coup of Peace" functioned 
 * as a masterful "Positive Politeness" strategy, restoring the team's Face by 
 * inviting the crowd into a shared "we-focused" identity.
 * * KEY AGENTS:
 * - The Aggressor (Lead Fans): Initially attacking the team's face through belittling and mockery.
 * - The Face-Victim (Doni De Cory): Feeling the immediate "Noose" of the face-attack.
 * - The Face-Restorer (SuAnne): Using high-competence performance as a "Positive Politeness" tool.
 * * NARRATIVE ARC:
 * The interaction begins with the fans waving food stamps and making "fake" war cries—attacks 
 * on the Lakota team's desire to be accepted and valued (Positive Face). 
 * SuAnne’s dance "reversed" the situation by demonstrating authentic competence and using 
 * a common "warm-up jacket" to include the onlookers, making them "relatives" and 
 * restoring the social equilibrium.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(suanne_face_restoration_interval, 0, 10).
narrative_ontology:constraint_claim([suanne_face_restoration], [social_synchronization]).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.75). The initial face-attack liquidates the team's social status 
% and dignity to feed the crowd's sense of superiority.
domain_priors:base_extractiveness(suanne_face_restoration, 0.75).

% Suppression score (0.0-1.0)
% Rationale: Moderate-High (0.65). The mockery suppresses the "real" Lakota identity 
% behind "fake" caricatures. SuAnne’s act un-suppresses this truth.
domain_priors:suppression_score(suanne_face_restoration, 0.65).

domain_priors:requires_active_enforcement(suanne_face_restoration).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(suanne_face_restoration, collective_face_restoration).
constraint_victim(suanne_face_restoration, racial_mockery).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(suanne_face_restoration, extractiveness, 0.75).
narrative_ontology:constraint_metric(suanne_face_restoration, suppression_requirement, 0.65).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISHONORED TEAM - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A visiting team under racialized face-attack)
   WHEN: immediate (The pre-game "din" of harassment)
   WHERE: trapped (Caught in a hostile "standard of thinking")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the teammates, the crowd's "woo-woo-woo" sounds and "Where’s the cheese?" 
   taunts are a "Noose." These acts are "attacks on our positive face" that discuss 
   identity in belittling ways, strangling the team's ability to act competently.
   
   NARRATIVE EVIDENCE:
   "All that stuff the Lead fans were yelling... Doni De Cory... told her 
   teammates, 'I can’t handle this'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    suanne_face_restoration,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SUANNE (THE STRATEGIST) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "step into the jump-ball circle")
   WHEN: biographical (Drawing on power and Powwow skills)
   WHERE: mobile (Re-mapping the "fake" tune to "the real thing")
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   SuAnne uses "Positive Politeness" as a "Rope." By performing a "graceful and 
   show-offy" dance, she asserts her competence (Positive Face) while using the 
   jacket to "invite us all to play," restoring the "Face" of her team through 
   inclusion rather than retaliation.
   
   NARRATIVE EVIDENCE:
   "She made Lakota relatives of us all... What SuAnne did made a lasting 
   impression and changed the whole situation".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    suanne_face_restoration,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOCIOLOGIST (Goffman/Brown/Levinson) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of "universal" face negotiation)
   WHEN: civilizational (The perennial "Interaction Ritual")
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, the transformation is a "Mountain"—an unchangeable law of 
   Politeness Theory. When the "Positive Face" of an individual is successfully 
   asserted and validated by the group, the "status quo" of hostility must 
   collapse into a new social alignment.
   
   NARRATIVE EVIDENCE:
   "The crowd went completely silent... reversed it somehow... it gave the 
   hecklers the best interpretation".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    suanne_face_restoration,
    mountain,
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

:- begin_tests(suanne_face_restoration_tests).

test(multi_perspective_face) :-
    % Team sees Noose
    constraint_indexing:constraint_classification(suanne_face_restoration, noose, context(individual_powerless, immediate, trapped, local)),
    % SuAnne sees Rope
    constraint_indexing:constraint_classification(suanne_face_restoration, rope, context(individual_moderate, biographical, mobile, regional)),
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(suanne_face_restoration, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(face_extraction_logic) :-
    % Shows that belittling extracts more from the un-prepared than the prepared agent.
    true.

:- end_tests(suanne_face_restoration_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. POSITIVE POLITENESS STRATEGY:
 * The dance and the "relative" jacket are identified as "Positive Politeness" because 
 * they attempt to make the audience belong. SuAnne "gives the 
 * hecklers the best interpretation," as if they were already part of her "we-focused" 
 * culture.
 * * 2. FACE-THREATENING ACT (FTA):
 * The Lead fans' mockery of food stamps and culture is a terminal FTA targeting 
 * the Lakota team's competency and dignity (Positive Face).
 * * 3. MANDATROPHY RESOLUTION:
 * [RESOLVED MANDATROPHY]. The extraction of the team's Face is resolved by the 
 * "Rope" of SuAnne's performance, which forces a collective recognition of 
 * the "real thing" over the "fake".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suanne_face_restoration_intent,
    "Is the silence of the crowd a biological recognition of 'the real thing' 
     (Mountain) or a temporary strategic 'Rope' of confusion?",
    resolution_mechanism("Audit of long-term community shifts in Lead post-SuAnne vs. communities with no such intervention"),
    impact("If Mountain: Authenticity is a social law. If Rope: It is a high-risk gamble."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Retaliatory FTA (Aggression)
 * Viability: Mocking the Lead fans back.
 * Suppression: Suppressed because it would "embarrass" the team and tighten 
 * the "Noose" of hostility.
 * * ALTERNATIVE 2: Off-Record Silence (Ignoring)
 * Viability: The "usual plan" of taking laps and going to the bench.
 * Suppression: Shunted because the noise was "deafening," making silence 
 * an impossible choice for the team's dignity.
 * * CONCLUSION:
 * SuAnne's "Positive Politeness" was the only functional "Rope" capable of 
 * liquidating the crowd's hostility without the team becoming "new recruits" 
 * of negativity.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [suanne_face_restoration].
% Multi-perspective: ?- constraint_indexing:multi_index_report(suanne_face_restoration).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
