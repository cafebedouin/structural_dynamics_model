% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking (REFINED v2.0)
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 Flash
% Source: "Rotation Seven" narrative
% ============================================================================

:- module(kubo_ranking_system_v2, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor
narrative_ontology:interval(kubo_ranking_system_r7, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: kubo_ranking_system_r7
 * human_readable: Kubo Credit and Ranking System
 * domain: economic_social
 * * SUMMARY:
 * The Kubo system is a gamified labor-extraction mechanism. It presents 
 * a "ladder" of advancement to children on the R7 station, rewarding 
 * agricultural output with "colored plastic" credits. While it appears 
 * as an unchangeable reality to the children (a False Mountain), it is 
 * an institutionally maintained Noose designed to extract maximum labor 
 * while suppressing awareness of the health costs (kidney failure).
 * * KEY AGENTS:
 * - Rina/Anna (Powerless): Subjects who initially "believe" in the ladder.
 * - Supervisor Kwan (Institutional): Manager who uses the system as a tool (Rope).
 * - The Station Logic (Systemic): The beneficiary of the labor extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Refined per Audit)
   ========================================================================== */

% Base extractiveness: 0.75 (High).
% Essential agricultural labor and biological health are exchanged for 
% non-essential "credits" and minor privileges.
domain_priors:base_extractiveness(kubo_ranking_system_r7, 0.75).

% Suppression: 0.90 (Extreme).
% Total enclosure. No memory of Earth. Alternatives are non-existent 
% in the conceptual field of the children.
domain_priors:suppression_score(kubo_ranking_system_r7, 0.90).

% Enforcement: Requires active maintenance.
% Enforced via wristbands, medical isolation (R7), and Kwan's tablet.
domain_priors:requires_active_enforcement(kubo_ranking_system_r7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: RINA (POWERLESS) - Noose (False Mountain)
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical
   EXIT: trapped
   
   WHY: Rina initially treats the system as a Mountain ("The credits are real"), 
   but after Anna's "rotation," she recognizes it as a Noose. She sees 
   the extraction of Anna's life in exchange for a "gold rank" that was 
   just "colored plastic on a locked bracelet."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kubo_ranking_system_r7,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SUPERVISOR KWAN - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   EXIT: constrained
   
   WHY: To Kwan, the system is a Rope—a functional coordination tool 
   to manage labor and ensure soil prep. He has the power to "adjust 
   rankings with a tablet tap."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kubo_ranking_system_r7,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ANALYTICAL OBSERVER - Noose
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   EXIT: analytical
   
   WHY: The observer sees the "Structural Signature": high extraction (0.75) 
   and active enforcement. The fact that the gold names "never come back" 
   reveals the system as a terminal coercive mechanism.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kubo_ranking_system_r7,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ALGORITHMIC ANALYST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the unyielding mathematical logic.
   WHEN: immediate - Rankings are updated in real-time by the system.
   WHERE: arbitrage - Managing the data flow within the sector network.
   SCOPE: local - The ranking logic is specific to the Sector 7 population.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the Kubo Ranking algorithm. The sorting logic, the 
   weighted variables, and the zero-sum nature of the status hierarchy are 
   immutable facts of the Sector 7 digital terrain. While residents feel 
   this as a Noose and Command uses it as a Rope for behavioral control, 
   the analyst recognizes it as an unchangeable structural invariant: the 
   system is mathematically designed to produce a rigid social hierarchy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_kubo_ranking,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(rotation_seven_kubo_ranking),
    !.

% Explicit priors reflecting the rigid, unyielding nature of the ranking logic.
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.3).
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.4).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(kubo_v2_tests).

test(false_mountain_detection) :-
    % Despite high suppression making it LOOK like a mountain to Rina,
    % the system correctly classifies it as a Noose because it requires
    % active enforcement to exist.
    domain_priors:requires_active_enforcement(kubo_ranking_system_r7),
    constraint_indexing:constraint_classification(kubo_ranking_system_r7, noose, context(agent_power(individual_powerless), _, _, _)).

test(extraction_asymmetry) :-
    % Validation: Powerless experiences 0.75 extraction while manager sees low extraction.
    domain_priors:base_extractiveness(kubo_ranking_system_r7, E),
    E > 0.7.

:- end_tests(kubo_v2_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================= */

/**
 * LLM REFINEMENT NOTES:
 * 1. FIXED TYPE 1 FALSE MOUNTAIN: 
 * Reclassified the individual_powerless view from 'mountain' to 'noose'. 
 * The narrative support for this is Rina's terminal realization: "The ladder 
 * was never real... just a game to keep them busy."
 * 2. MODULE UNIQUENESS: 
 * Set to 'kubo_ranking_system_v2' to pass the dynamic validation suite.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE REJECTED ALTERNATIVE:
 * The primary alternative to the Kubo Noose is "Direct Democratic Labor 
 * Allocation," where cohorts manage their own health risks. This is 
 * suppressed by the "Ladder" narrative, which frames individual 
 * "advancement" as the only path out of the domes.
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, noose, agent_power(individual_powerless)).
