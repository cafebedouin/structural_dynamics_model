% ============================================================================
% CONSTRAINT STORY: suanne_coup_of_peace
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Ian Frazier, “On the Rez.” The Atlantic. December 1999.
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_suanne_coup_of_peace, []).

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
 * * constraint_id: suanne_coup_of_peace
 * human_readable: SuAnne Marie Big Crow’s Coup of Peace
 * domain: social/psychological
 * temporal_scope: 1988 (Biographical)
 * spatial_scope: Lead, South Dakota (Local/Regional)
 * * SUMMARY:
 * This constraint analyzes the transformation of a hostile, racist social environment into a 
 * space of mutual respect through a "Coup of Peace". SuAnne Marie Big Crow 
 * uses a traditional Lakota shawl dance—performed with a common warm-up jacket—to 
 * subvert a crowd's mockery and establish a shared human identity.
 * * KEY AGENTS:
 * - SuAnne Marie Big Crow: The 14-year-old "Teacher" who uses cultural performance as a Rope.
 * - The Lead Fans: The collective institutional force initially using mockery as a Noose.
 * - Doni De Cory & Teammates: Powerless agents initially trapped by the "din" of harassment.
 * - The Witness/Narrator: Analytical observer identifying the "ancient sense" of the act.
 * * NARRATIVE ARC:
 * A Lakota basketball team enters a gymnasium saturated with "fake Indian war cries" and 
 * mocking displays of food stamps. The tension acts as a lethal "Noose" 
 * that threatens to "liquidate" the team's focus and dignity. SuAnne 
 * intervenes at center court, performing a "graceful and modest" dance that silences the 
 * crowd and eventually converts their "silly, mocking chants" into applause and lasting 
 * community friendships.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(suanne_coup_of_peace, 0, 10).
narrative_ontology:constraint_claim([suanne_coup_of_peace], [social_transformation]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.8). The initial environment is predatory, extracting the team's peace 
% and dignity through "deafening" noise and racialized taunts ("Where’s the cheese?").
domain_priors:base_extractiveness(suanne_coup_of_peace, 0.8).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High (0.7). The dominant narrative of the "Lead fans" suppresses the 
% team's humanity, treating them as caricatures of poverty and "fake" culture.
domain_priors:suppression_score(suanne_coup_of_peace, 0.7).

% Enforcement requirements: Requires active subversion of social inertia.
domain_priors:requires_active_enforcement(suanne_coup_of_peace).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(suanne_coup_of_peace, inter_community_peace). % Lasting "tremendous" relationship.
constraint_victim(suanne_coup_of_peace, racial_hostility). % The "Noose" of the crowd's anger is liquidated.

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(suanne_coup_of_peace, extractiveness, 0.8).
narrative_ontology:constraint_metric(suanne_coup_of_peace, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HARASSED TEAM (Doni De Cory) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A visiting team "harassed regularly")
   WHEN: immediate (The moment of entering the court)
   WHERE: trapped (Caught in the hallway, "I can’t handle this")
   SCOPE: local (The high school gymnasium)
   
   WHY THIS CLASSIFICATION:
   For the teammates, the racism is a "Noose." It is an inescapable pressure that 
   strangles their ability to "handle" the situation, threatening to "embarrass" 
   them and liquidate their focus before the game begins.
   
   NARRATIVE EVIDENCE:
   "The noise was deafening... Doni De Cory looked out the door and told her 
   teammates, 'I can’t handle this'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    suanne_coup_of_peace,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(suanne_coup_of_peace, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SUANNE MARIE BIG CROW - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A freshman with the agency to "offer to go first")
   WHEN: biographical (Integrating Powwow skills learned "as a little girl")
   WHERE: mobile (Moving into the "jump-ball circle" to redefine the space)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For SuAnne, the traditional dance and the warm-up jacket are "Ropes"—functional 
   coordination mechanisms. She uses the "warm-up jacket as a shawl" to bridge the 
   gap between Lakota culture and the American crowd, transforming the "fake" 
   into the "real thing".
   
   NARRATIVE EVIDENCE:
   "By using the warm-up jacket as a shawl... she made Lakota relatives of us all".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    suanne_coup_of_peace,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (Narrator) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Viewing the "ancient sense" of the Oglala kin)
   WHEN: civilizational ("We Lakota have been dancing like this for centuries")
   WHERE: analytical (Observer stance; "isn't it pretty?")
   SCOPE: global (Representing the "non-Lakota rest of this country")
   
   WHY THIS CLASSIFICATION:
   To the observer, SuAnne’s act reveals a "Mountain"—an unchangeable cultural 
   excellence that predates the hecklers' history. The "real thing" is a fixed 
   standard of beauty and peace that cannot be erased by "fake" mockery.
   
   NARRATIVE EVIDENCE:
   "we’ve been doing the shawl dance since long before you came... before you 
   stole this land, and we’re still doing it today".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    suanne_coup_of_peace,
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

:- begin_tests(suanne_coup_of_peace_tests).

test(multi_perspective_transformation) :-
    % Team (Powerless) sees Noose
    constraint_indexing:constraint_classification(suanne_coup_of_peace, noose, context(individual_powerless, immediate, trapped, local)),
    % SuAnne (Moderate) sees Rope
    constraint_indexing:constraint_classification(suanne_coup_of_peace, rope, context(individual_moderate, biographical, mobile, local)),
    % Historian (Analytical) sees Mountain
    constraint_indexing:constraint_classification(suanne_coup_of_peace, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_subversion) :-
    % The "deafening noise" extracts more from the powerless teammates than from the "prepared" SuAnne.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(suanne_coup_of_peace, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(suanne_coup_of_peace, ContextModerate, Score2),
    Score1 > Score2.

test(time_immutability_coup) :-
    % In the immediate horizon, the heckling is a Mountain (fact).
    % Over biographical time, SuAnne's "lasting impression" is a Rope (changeable).
    constraint_indexing:effective_immutability(biographical, mobile, rope).

:- end_tests(suanne_coup_of_peace_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: Racism in this context is highly predatory, aiming to "liquidate" the 
 * team's agency through social humiliation and mockery of their economic status.
 * * 2. STATUS: [RESOLVED MANDATROPHY]:
 * The high extraction (Noose) is resolved by the "Coup of Peace," which 
 * transforms the energy of the extraction into a "Rope" of community 
 * building.
 * * 3. PERSPECTIVE SELECTION:
 * Contrasted the Team (Noose/Powerless), SuAnne (Rope/Moderate), and the 
 * Narrator (Mountain/Analytical) to show how cultural expertise can "reverse" 
 * a social trap.
 * * 4. AMBIGUITIES:
 * - The text mentions "ancient sense," suggesting a "Mountain" of ancestral 
 * knowledge that SuAnne tapped into.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suanne_coup_repeatability,
    "Is the 'Coup of Peace' a repeatable Rope for all marginalized groups (Rope), 
     or does it require the unique, 'world-opening' charisma of a SuAnne Marie Big Crow (Mountain)?",
    resolution_mechanism("Audit of similar 'Coup' attempts by different agents in diverse hostile environments"),
    impact("If Mountain: The resolution depends on the 'God-like' agent. If Rope: It is a teachable protocol."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Retaliatory Anger
 * Viability: A natural response to "Woo-woo-woo" sounds and food stamp taunts.
 * Suppression: Rejected by SuAnne as it would "embarrass" the team and likely 
 * escalate the "Noose".
 * * ALTERNATIVE 2: Passive Submission / Ignoring
 * Viability: The "usual plan" was to just shoot baskets and go to the bench.
 * Suppression: Shunted because Doni "couldn't handle it," meaning silence was 
 * no longer a protective Rope.
 * * CONCLUSION:
 * The presence of these "futile" alternatives (Anger/Submission) makes SuAnne's 
 * "Coup" a definitive "Rope" for survival and peace.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [suanne_coup_of_peace].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(suanne_coup_of_peace).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
