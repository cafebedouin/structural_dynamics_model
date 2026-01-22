% ============================================================================
% CONSTRAINT STORY: empty_tomb_transformation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Life is a Series of Empty Tombs" by cafebedouin.org
% ============================================================================

:- module(empty_tomb_transformation, []).

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
 * * constraint_id: empty_tomb_transformation
 * human_readable: The Resurrection Cycle (Empty Tombs)
 * domain: religious/social/psychological
 * temporal_scope: Perennial / Biographical
 * spatial_scope: Internal / The Familial and Social Fabric
 * * SUMMARY:
 * Life is framed as a constant series of dying to old selves and being reborn into new lives. 
 * Each life stage or "chapter" concludes with a "tomb of transition" that must be exited by rolling back 
 * the stone of the past to emerge transformed.
 * * KEY AGENTS:
 * - The Transitioning Subject: The individual currently "stuck in a tomb" or undergoing rebirth.
 * - The Old Self: The "dead" version of the person that must be left in the record of transformation.
 * - The Pastor/Witness: The analytical observer who frames the "profound wisdom" of the cycle.
 * - The Liturgical Institution: The "Church" context that provides the resurrection story as a social scaffold.
 * * NARRATIVE ARC:
 * The individual moves through a "string of pearls" of transformations. The constraint 
 * functions as a requirement for death (the end of a chapter) to precede new life. 
 * Without the "resurrection," the agent remains trapped in a stagnant tomb of their previous identity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(empty_tomb_transformation, 0, 10).
narrative_ontology:constraint_claim([empty_tomb_transformation], [ontological_transformation]).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The process extracts the "old life" and involves "facing our own suffering". 
% While generative of "new life," the transition phase is a temporary liquidation of the self.
domain_priors:base_extractiveness(empty_tomb_transformation, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.4). The "tomb" phase suppresses the visibility of the new life. 
% Additionally, the "fairy tale" interpretation of resurrection often suppresses the "profound wisdom" 
% of the actual cycle of growth.
domain_priors:suppression_score(empty_tomb_transformation, 0.4).

% Enforcement requirements
% Emerges naturally from the human stages of experience and development.
domain_priors:emerges_naturally(empty_tomb_transformation).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(empty_tomb_transformation, extractiveness, 0.4).
narrative_ontology:constraint_metric(empty_tomb_transformation, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
% Beneficiary: The New Self (Reborn into a "new, different life").
constraint_beneficiary(empty_tomb_transformation, the_resurrected_self).
% Victim: The Old Self (Who must die and be left behind).
constraint_victim(empty_tomb_transformation, the_static_ego).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STUCK INDIVIDUAL - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Unable to "roll back the stone")
   WHEN: immediate (The present moment of being "stuck in a tomb")
   WHERE: trapped (Bounded by the transition with no visibility of new life)
   SCOPE: local (The immediate psychological state)
   
   WHY THIS CLASSIFICATION:
   For the individual unable to emerge, the cycle is a "Noose." The "tomb of transition" 
   becomes a static trap where life has ended but rebirth has not begun, extracting 
   the agent's hope and agency.
   
   NARRATIVE EVIDENCE:
   "Am I stuck in a tomb? ... The old us is dead... our suffering".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    empty_tomb_transformation,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(empty_tomb_transformation, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE REBORN AGENT - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Choosing to be "open to new life")
   WHEN: biographical (The "string of pearls" across the lifespan)
   WHERE: mobile (Emerged from the tomb; "roll back the stone")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the agent who successfully transitions, the cycle is a "Rope." It is a 
   functional coordination mechanism for growth, allowing them to use past 
   "deaths" as a record of transformation that informs their new life.
   
   NARRATIVE EVIDENCE:
   "like a string of pearls, the tombs leave a record of who we were and our 
   transformation".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    empty_tomb_transformation,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PASTOR/WIZARD - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Ancient and wise observer of the "profound wisdom")
   WHEN: civilizational (Resurrection as an "Easter" message across thousands of years)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the observer, this cycle is a "Mountain"—an unchangeable law of development. 
   One cannot have "new life" without the "death" of the old; it is an objective 
   requirement for human experience.
   
   NARRATIVE EVIDENCE:
   "life is a constant series of dying and being reborn... from one moment to 
   the next".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    empty_tomb_transformation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE CHURCH DOCTRINE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Providing the "resurrection story" as a framework)
   WHEN: generational (Maintaining the homily and ritual over years)
   WHERE: mobile (Using the story to help people "roll back the stone")
   SCOPE: regional (The Holy Name Cathedral community)
   
   WHY THIS CLASSIFICATION:
   For the institution, the story is a "Rope"—a tool used to coordinate the 
   community's response to suffering and change, transforming the "fairy tale" 
   into a functional map for resilience.
   
   NARRATIVE EVIDENCE:
   "resurrection story made a kooky fairy tale into profound wisdom... the 
   living Jesus and his message of peace".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    empty_tomb_transformation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(empty_tomb_transformation_tests).

test(multi_perspective_tomb) :-
    % Stuck (Powerless) sees Noose
    constraint_indexing:constraint_classification(empty_tomb_transformation, T1, context(individual_powerless, immediate, trapped, local)),
    % Reborn (Moderate) sees Rope
    constraint_indexing:constraint_classification(empty_tomb_transformation, T2, context(individual_moderate, biographical, mobile, local)),
    % Pastor (Analytical) sees Mountain
    constraint_indexing:constraint_classification(empty_tomb_transformation, T3, context(analytical, civilizational, analytical, global)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_tomb) :-
    % The powerless (stuck in the tomb) experience more extraction of vitality/hope.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(empty_tomb_transformation, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(empty_tomb_transformation, ContextModerate, Score2),
    Score1 > Score2.

test(time_immutability_transformation) :-
    % Over biographical time, transformation is a Rope (possible through choice).
    % Over civilizational time, the cycle itself is a Mountain (fact of life).
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(empty_tomb_transformation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: I chose a moderate score because the transition requires "dying" to the 
 * old self, which is an extraction of established identity and comfort.
 * * 2. PERSPECTIVE SELECTION:
 * I selected the "Stuck" individual (Noose) and the "Reborn" agent (Rope) to highlight 
 * the core pedagogical shift: moving from being a victim of transition to an 
 * author of resurrection.
 * * 3. CLASSIFICATION RATIONALE:
 * Individual Powerless → Noose: In the "immediate" moment of "suffering," the tomb is 
 * an inescapable death of the old self.
 * Analytical → Mountain: The observer sees the cycle as a fixed feature of human 
 * growth across "thousands of years".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resurrection_agency,
    "Is the ability to 'roll back the stone' an internal choice (Rope) or a 
     stochastic event that requires external grace (Mountain)?",
    resolution_mechanism("Longitudinal tracking of individuals in 'tombs of transition' with vs. without internal 'hope' metrics"),
    impact("If choice: Rebirth is a Rope. If stochastic: It is a Mountain/Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    tomb_duration_determinism,
    "Is the length of the 'tomb of transition' fixed by the nature of the chapter (Mountain) 
     or variable based on individual effort (Rope)?",
    resolution_mechanism("Comparison of transition times across diverse psychological 'deaths'"),
    impact("If fixed: The tomb is a Mountain. If variable: It is a Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Eternal Sameness (Stagnation)
 * Viability: Avoiding the "death" of a chapter to keep the old self.
 * Suppression: Suppressed by the text as a state of being "stuck in a tomb".
 * * ALTERNATIVE 2: Scientific Secular Materialism
 * Viability: Viewing life as a linear accumulation of data rather than a series of rebirths.
 * Suppression: Shunted by the Pastor as a "fairy tale" (the literal view) that misses 
 * the "profound wisdom" of the cycle.
 * * CONCLUSION:
 * The existence of Alternative 1 (Stagnation) makes the "Resurrection" a functional 
 * Rope for those who wish to live. Without the "Stone rolling,"sameness becomes 
 * a terminal Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [empty_tomb_transformation].
% Multi-perspective: ?- constraint_indexing:multi_index_report(empty_tomb_transformation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
