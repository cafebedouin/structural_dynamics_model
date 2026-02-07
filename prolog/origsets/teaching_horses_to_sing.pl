% ============================================================================
% CONSTRAINT STORY: teaching_horses_to_sing
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Parable of Teaching Horses to Sing" from cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_teaching_horses_to_sing, []).

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
 * * constraint_id: teaching_horses_to_sing
 * human_readable: The Sing-or-Die Gambit (Purchasing Time)
 * domain: political/social
 * temporal_scope: Perennial / Folklore
 * spatial_scope: Local (Royal Stable) to Regional (The Kingdom)
 * * SUMMARY:
 * This constraint defines a state where an individual under a death sentence purchases 
 * a one-year stay of execution by promising an impossible outcome: teaching a horse 
 * to sing. It represents the strategic use of time as a 
 * "buffer" against stochastic mortality events.
 * * KEY AGENTS:
 * - The Thief (The Prisoner): An agent with no immediate power who uses an absurd 
 * claim to transform a terminal "Snare" into a temporal "Rope".
 * - The Monarch (The Ruler): An institutional agent who enforces the extraction 
 * (imprisonment and labor) while holding the power of life and death.
 * - The Stablehand (The Cynic): An analytical observer who sees only the 
 * biological impossibility of the task.
 * * NARRATIVE ARC:
 * The thief is "captured and hauled" before a monarch for execution. 
 * He introduces a year-long constraint (teaching the stallion to sing) to replace 
 * the immediate "Snare" of the gallows. While the task is 
 * physically impossible, the thief gambles on the "Mountain" of time—betting 
 * that the king, the horse, or himself might die within the year.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(teaching_horses_to_sing_interval, 0, 10).
narrative_ontology:constraint_claim([teaching_horses_to_sing], [existential_arbitrage]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.75). The Monarch extracts a year of the thief's life, liberty, 
% and hope through imprisonment and the requirement of daily singing. 
% Terminal extraction (death) is merely deferred.
domain_priors:base_extractiveness(teaching_horses_to_sing, 0.75).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.4). The immediate "Snare" of execution is suppressed by 
% the Monarch's bemusement, but the biological reality that "horses can't sing" 
% remains visible to the cynical stablehand.
domain_priors:suppression_score(teaching_horses_to_sing, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(teaching_horses_to_sing, extractiveness, 0.75).
narrative_ontology:constraint_metric(teaching_horses_to_sing, suppression_requirement, 0.4).

% Enforcement requirements
% Requires active enforcement by the Monarch and the Royal guards/stablehands.
domain_priors:requires_active_enforcement(teaching_horses_to_sing).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
% Beneficiary: The Monarch (Gains amusement and a year of labor/singing).
constraint_beneficiary(teaching_horses_to_sing, the_monarch).
% Victim: The Thief (Gives up a year of liberty and remains under threat of death).
constraint_victim(teaching_horses_to_sing, the_thief).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE THIEF (THE PRISONER) - Rope
   --------------------------------------------------------------------------
   
   WHO: powerless (A captured thief under a death sentence)
   WHEN: biographical (Gaining "a whole year" he didn't have before)
   WHERE: mobile (He has successfully "purchased" time/exit from immediate death)
   SCOPE: local (The royal stable)
   
   WHY THIS CLASSIFICATION:
   For the thief, the impossible assignment is a "Rope"—a functional coordination 
   mechanism that allows him to navigate away from immediate execution. It is 
   a tool of "existential arbitrage" that leverages the passage of time.
   
   NARRATIVE EVIDENCE:
   "It gained me a whole year which I didn’t have before. A lot can happen in a year".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    teaching_horses_to_sing,
    rope,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    % The thief uses the impossible task as a coordination tool for survival.
    domain_priors:base_extractiveness(teaching_horses_to_sing, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STABLEHAND (THE CYNIC) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the biological nature of horses)
   WHEN: immediate (A year is insufficient to change the nature of a horse)
   WHERE: trapped (Bounded by the physical reality of the stable)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   To the stablehand, the task is a "Mountain"—an unchangeable natural law. 
   Biological reality dictates that "everyone knows horses can't sing," and 
   the gambit is viewed as "stupid" because it cannot alter this fact.
   
   NARRATIVE EVIDENCE:
   "I don’t see why you bother. Everyone knows horses can’t sing".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    teaching_horses_to_sing,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MONARCH (THE RULER) - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power of life and death)
   WHEN: biographical (Managing his "finest stallion" and prisoners)
   WHERE: mobile (He can change the rules or end the year at any time)
   SCOPE: regional (The Kingdom)
   
   WHY THIS CLASSIFICATION:
   For the Monarch, the year-long stay is a "Snare." It is an extractive mechanism 
   that keeps the thief in a state of controlled labor (singing) while maintaining 
   the terminal threat. The Monarch "owns" the time he has granted.
   
   NARRATIVE EVIDENCE:
   "You will be imprisoned in the royal stable... and in a year if he cannot sing, 
   you will be put to death".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    teaching_horses_to_sing,
    snare,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(teaching_horses_to_sing_tests).

test(multi_perspective_gambit) :-
    % Thief (Powerless/Mobile) sees a Rope
    constraint_indexing:constraint_classification(teaching_horses_to_sing, rope, context(powerless, biographical, mobile, local)),
    % Stablehand (Analytical/Trapped) sees a Mountain
    constraint_indexing:constraint_classification(teaching_horses_to_sing, mountain, context(analytical, immediate, trapped, local)),
    % Monarch (Institutional/Mobile) sees a Snare
    constraint_indexing:constraint_classification(teaching_horses_to_sing, snare, context(institutional, biographical, mobile, regional)),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_singing) :-
    % The thief experiences the extraction of his liberty (imprisonment)
    ContextPowerless = context(powerless, biographical, mobile, local),
    % The monarch experiences the amusement/labor
    ContextPowerful = context(institutional, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(teaching_horses_to_sing, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(teaching_horses_to_sing, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_gallows) :-
    % In the immediate horizon (execution day), death is a Mountain (fact).
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % In the biographical horizon (the year stay), survival is a Rope (changeable).
    constraint_indexing:effective_immutability(biographical, mobile, rope).

:- end_tests(teaching_horses_to_sing_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: Chose high extraction because the thief's entire existential security 
 * is liquidated for a year of imprisonment and required labor (singing). 
 * The Monarch "owns" this year.
 * * 2. STATUS: [RESOLVED MANDATROPHY]
 * Reasoning: While the Monarch's extraction is high (Snare), the Thief 
 * experiences it as a Rope—a purchased tool for coordination with time and 
 * possibility. This perspectival variance resolves the 
 * apparent predatory nature of the year stay.
 * * 3. PERSPECTIVE SELECTION:
 * The Thief (Rope) illustrates the arbitrage of time; the Stablehand (Mountain) 
 * illustrates biological limits; the Monarch (Snare) illustrates institutional 
 * containment.
 * * 4. AMBIGUITIES:
 * - The outcome of the singing horse: I resolved this as an Omega variable 
 * of stochastic survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stochastic_survival_event,
    "Will the king, the horse, or the thief die within the year (Rope), or is the 
    death sentence an inescapable Mountain?",
    resolution_mechanism("Observation of the passage of time over the 365-day period"),
    impact("If a death occurs: The stay was a Rope. If no death occurs: The sing-or-die 
    clause is a terminal Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    equine_vocal_plasticity,
    "Is it biologically impossible for a horse to sing (Mountain) or can a year 
    of 'singing to the horse' reveal an unknown cognitive Rope?",
    resolution_mechanism("Biological and behavioral audit of equine vocalizations post-training"),
    impact("If singing is possible: The task was a Rope. If impossible: It was a 
    Mountain/Deception."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Immediate Execution
 * Viability: The standard protocol for a captured thief.
 * Suppression: Actively suppressed by the Thief's absurd claim and the Monarch's bemusement.
 * * ALTERNATIVE 2: Escaping the Stable
 * Viability: A logical attempt for a thief imprisoned with a horse.
 * Suppression: Not explicitly mentioned but suppressed by the requirement 
 * to "sing to the horse every day".
 * * CONCLUSION:
 * The existence of Alternative 1 (Death) makes the impossible task a "Rope" 
 * of existential value. Without the threat of the gallows, the stable is 
 * merely a Snare of labor.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system:
% ?- [teaching_horses_to_sing].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
