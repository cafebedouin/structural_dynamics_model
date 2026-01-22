% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Politeness is Two-Faced" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

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
 * * constraint_id: politeness_face_negotiation
 * human_readable: The Face Negotiation Constraint (Politeness Theory)
 * domain: social/psychological
 * temporal_scope: Perennial / Universal
 * spatial_scope: Global / Interpersonal
 * * SUMMARY:
 * Politeness is a universal social constraint where individuals manage a "face"—a social self-image 
 * categorized into "Positive Face" (the desire for competence and acceptance) and "Negative Face" 
 * (the desire for autonomy and freedom from imposition). 
 * * KEY AGENTS:
 * - The Social Actor: Navigates "face-threatening acts" (FTAs) like orders, requests, or belittling.
 * - The Group Member: Seeks alignment between their self-image and the group's perspective (Positive Face).
 * - The Leader/Proselytizer: Acts that threaten the autonomy or status quo of others (Negative Face).
 * - The Analytical Observer: Sees the "Interaction Ritual" as a fundamental, unchangeable law of society.
 * * NARRATIVE ARC:
 * Interaction is a constant negotiation where even "compliments" or "thanks" can be attacks on autonomy 
 * or self-image. Individuals use strategies (Ropes) like "negative politeness" to 
 * avoid imposing on others, but social norms can become a "Noose" when they force a commitment to 
 * unwanted actions to save face.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(politeness_face_negotiation, 0, 10).
narrative_ontology:constraint_claim([politeness_face_negotiation], [social_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Moderate-High (0.65). The system extracts individual autonomy (Negative Face) 
% and forces self-image alignment (Positive Face). It requires individuals to "commit to 
% doing something we don’t want to do" to maintain social etiquette.
domain_priors:base_extractiveness(politeness_face_negotiation, 0.65).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.5). Politeness suppresses raw, "bald on-record" expression in favor 
% of face-saving strategies like "off-record" indirect communication.
domain_priors:suppression_score(politeness_face_negotiation, 0.5).

% Enforcement requirements
% Emerges naturally from human sociality but is actively enforced via social disapproval, 
% belittling, or exclusion when face-etiquette is violated.
domain_priors:requires_active_enforcement(politeness_face_negotiation).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(politeness_face_negotiation, social_cohesion). % Acceptance and value to others
constraint_beneficiary(politeness_face_negotiation, status_quo). % Negative face maintenance
constraint_victim(politeness_face_negotiation, individual_autonomy). % Negative face attacks (orders/requests)
constraint_victim(politeness_face_negotiation, raw_identity). % Suppression of topics like politics/sex/religion

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(politeness_face_negotiation, extractiveness, 0.65).
narrative_ontology:constraint_metric(politeness_face_negotiation, suppression_requirement, 0.5).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SOCIAL STRATEGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to choose among four politeness strategies)
   WHEN: biographical (Developing social competence over a lifetime)
   WHERE: mobile (Can navigate between "bald on-record" and "off-record" based on context)
   SCOPE: regional (Navigating different social groups)
   
   WHY THIS CLASSIFICATION:
   For the socially competent actor, face-negotiation is a "Rope." It is a functional tool used to 
   "make the person feel like they belong" or to "not impose" while achieving personal goals.
   
   NARRATIVE EVIDENCE:
   "There are four politeness strategies... Positive politeness attempts to make the person feel like they 
   belong. Negative politeness attempts to not impose on other people".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    politeness_face_negotiation,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MARGINALIZED/EXCLUDED - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Target of belittling or misreading of face)
   WHEN: immediate (The moment of a face attack)
   WHERE: trapped (Caught in a social interaction where the "status quo" is enforced)
   SCOPE: local (The immediate newsroom or newsroom/group context)
   
   WHY THIS CLASSIFICATION:
   For those whose face is "misread" or "belittled," politeness is a "Noose." The extraction 
   of their self-image is immediate and painful, and the social requirement to 
   "not impose" (Negative Face) is used to silence their identity.
   
   NARRATIVE EVIDENCE:
   "Attacks on our positive face from others include... belittling... and misreading the face of 
   others (e.g., calling a trans-man a 'she')".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    politeness_face_negotiation,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(politeness_face_negotiation, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOCIOLOGIST (Goffman/Brown/Levinson) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The "ancient and wise" observer of "some universals")
   WHEN: civilizational (A fundamental property of human language usage)
   WHERE: analytical (Observer stance)
   SCOPE: global (Politeness is universal)
   
   WHY THIS CLASSIFICATION:
   To the analyst, politeness theory describes a "Mountain"—a zero-degree-of-freedom social law. 
   Face negotiation is not a choice but an inescapable "Interaction Ritual" that 
   defines human sociality across all cultures.
   
   NARRATIVE EVIDENCE:
   "Politeness theory holds that politeness is universal... We present a 'face' to others in a 
   particular social context".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    politeness_face_negotiation,
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

:- begin_tests(politeness_face_negotiation_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates the Mandatrophy resolution: The system is a tool for the 
 * competent (Rope) but a trap for the marginalized (Noose).
 */
test(multi_perspective_face) :-
    % Strategist (Moderate) sees Rope
    constraint_indexing:constraint_classification(politeness_face_negotiation, rope, context(individual_moderate, biographical, mobile, regional)),
    % Marginalized (Powerless) sees Noose
    constraint_indexing:constraint_classification(politeness_face_negotiation, noose, context(individual_powerless, immediate, trapped, local)),
    % Sociologist (Analytical) sees Mountain
    constraint_indexing:constraint_classification(politeness_face_negotiation, mountain, context(analytical, civilizational, analytical, global)).

/**
 * TEST 2: Power-based extractiveness
 * Demonstrates that those without social status experience higher 
 * extraction of identity/autonomy.
 */
test(power_extractiveness_face) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextInstitutional = context(institutional, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(politeness_face_negotiation, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(politeness_face_negotiation, ContextInstitutional, E2),
    E1 > E2.

/**
 * TEST 3: Leadership/Proselytizing Impact
 * Demonstrates how attempting to change the status quo (Negative Face) triggers 
 * high extraction from the group.
 */
test(leadership_extraction) :-
    domain_priors:base_extractiveness(politeness_face_negotiation, E),
    E > 0.5.

:- end_tests(politeness_face_negotiation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.65):
 * Reasoning: Chose a high score because the text identifies politeness as a mechanism 
 * that extracts autonomy ("orders, requests, commitments we don't want") and 
 * identity ("discussing topics that inform identity... are attacks").
 * * 2. PERSPECTIVE SELECTION:
 * Model focused on the gap between the competent user (who uses face as a tool/Rope) 
 * and the individual whose identity is attacked by "polite" norms (Noose).
 * * 3. MANDATROPHY STATUS:
 * RESOLVED MANDATROPHY. While the extraction of autonomy is high (0.65), the 
 * constraint is a "Rope" for social cohesion. The "predatory" nature is indexed 
 * to power level and social competence.
 * * 4. AMBIGUITIES:
 * - The text mentions that "negative face... often involves maintenance of the status quo." 
 * I resolved this by marking the "Status Quo" as a beneficiary and the "Change Agent" 
 * (Leader/Proselytizer) as a victim of face-threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    face_universality_intent,
    "Is 'face' a functional biological necessity for social survival (Mountain) or a predatory cultural 'Noose' used to enforce the status quo?",
    resolution_mechanism("Audit of isolation outcomes for 'face-ignoring' vs. 'face-observing' individuals in diverse cultures"),
    impact("If necessity: Evolutionary Mountain. If cultural enforcement: Mandatrophy Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    honesty_face_tradeoff,
    "Does 'Postman's Protocol' regarding overrated honesty (Rope) actually preserve face, or does it facilitate the 95% nonsense Mountain?",
    resolution_mechanism("Longitudinal tracking of group efficacy in 'radical honesty' vs. 'politeness strategy' environments"),
    impact("If honesty wins: Politeness is a Noose. If face wins: Honesty is a Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Radical Honesty / Plain Speaking
 * Viability: Bypassing "face" to speak the raw truth.
 * Suppression: Explicitly identified as a "Face-Threatening Act" that brings "pain to 
 * others".
 * Evidence: "Do not place too high a value on honesty and plain speaking".
 * * ALTERNATIVE 2: "Solo Polyamory" / Identity Exit
 * Viability: Choosing to not engage in the binary of "marriage or loneliness".
 * Suppression: Rejected by the "Standard of thinking" of traditional girlhood fantasies.
 * * CONCLUSION:
 * The existence of Alternatives 1 and 2 (Dissent/Autonomy) shifts the "Interaction Ritual" 
 * from a potential Rope into a Noose for those whose "Being expresses itself" outside 
 * the group's "good opinion".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * * 1. Load: ?- [politeness_face_negotiation].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(politeness_face_negotiation).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
