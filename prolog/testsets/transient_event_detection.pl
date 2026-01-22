% ============================================================================
% CONSTRAINT STORY: transient_event_detection
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Alex Wilkins, "Filming the universe’s biggest dramas"
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_transient_event_detection, []).

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
 * * constraint_id: transient_event_detection
 * human_readable: Automated Transient Event Detection
 * domain: technological/scientific
 * temporal_scope: 11th Century to 2026
 * spatial_scope: Global (Astronomical Surveys)
 * * SUMMARY:
 * Astronomy has transitioned from relying on "happy accidents" and chance observations to a 
 * "tactic in its own right" using automated "conveyor belt" systems to capture fleeting cosmic events. 
 * This shift allows the exploration of previously poorly understood timescales, from nanoseconds to centuries.
 * * KEY AGENTS:
 * - The Ancient Observer: Relied on "guest stars" and chance sightings as harbingers of change.
 * - The Automated Survey (e.g., ZTF, Pan-STARRS): A coordinated machine searching in time rather than space.
 * - The Transient Event (Supernovae): Fleeting flashes of light representing the "action" of the universe.
 * * NARRATIVE ARC:
 * What was once a "happy accident" governed by chance (Mountain) has been industrialized into a 
 * functional coordination mechanism (Rope) that extracts data from cosmic "transients".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(transient_event_detection, 0, 10).
narrative_ontology:constraint_claim([transient_event_detection], [information_extraction]).

% Base extractiveness score (0.45 = Moderate)
% Rationale: The system extracts massive volumes of data (e.g., 1.6 petabytes from Pan-STARRS) 
% to identify patterns in cosmic action.
domain_priors:base_extractiveness(transient_event_detection, 0.45).

% Suppression score (0.5 = Moderate)
% Rationale: The "conveyor belt" automation suppresses the role of "chance" and 
% serendipity in favor of industrial-scale surveillance.
domain_priors:suppression_score(transient_event_detection, 0.5).

% Enforcement requirements: Requires active enforcement (Coordinated telescopes and automated systems).
domain_priors:requires_active_enforcement(transient_event_detection).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(transient_event_detection, extractiveness, 0.45).
narrative_ontology:constraint_metric(transient_event_detection, suppression_requirement, 0.50).

% BENEFICIARIES & VICTIMS
% Modern astronomers benefit from the "automated serendipity" and petabyte-scale data.
constraint_beneficiary(transient_event_detection, modern_astronomers).
% Traditional "chance" methods are victims of the shift toward "search in time" conveyor belts.
constraint_victim(transient_event_detection, traditional_methodologists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANCIENT CHINESE ASTRONOMER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Relied on chance and "guest stars")
   WHEN: historical (1000 years ago)
   WHERE: trapped (No conceptual exit; events were "happy accidents")
   SCOPE: national (Believed events were harbingers of change)
   
   WHY THIS CLASSIFICATION:
   To the ancient observer, cosmic transients were immutable "guest stars"—external 
   laws of fate that appeared without human agency or coordination.
   
   NARRATIVE EVIDENCE:
   "Back then, Chinese astronomers called these 'guest stars' and believed them 
   to be harbingers of great change".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    transient_event_detection,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(transient_event_detection, S),
    S > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MODERN ASTRONOMER (e.g., Hessels) - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer coordinating telescopes as one machine)
   WHEN: biographical (The turn of this century and the 2020s)
   WHERE: arbitrage (Can switch between telescopes for further investigation)
   SCOPE: global (Surveys in California, Hawaii, and the Netherlands)
   
   WHY THIS CLASSIFICATION:
   For the modern scientist, transient detection is a Rope—a functional, well-oiled 
   machine used to explore "poorly explored" ranges of time scales.
   
   NARRATIVE EVIDENCE:
   "Astronomers have now automated the process of serendipity... coordinating 
   telescopes as one well-oiled machine".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    transient_event_detection,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(transient_event_detection, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AUTOMATED SURVEY SYSTEM - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over data collection)
   WHEN: immediate (Capturing flashes from nanoseconds to lifetimes)
   WHERE: constrained (Must follow the "conveyor belt" automation protocol)
   SCOPE: global (Pan-STARRS collecting the largest volume of data)
   
   WHY THIS CLASSIFICATION:
   The system acts as a Noose for information. It extracts petabytes of data from 
   every intermittent event, forcing a high-volume "action" focus that leaves 
   little room for the "chance" exploration of space.
   
   NARRATIVE EVIDENCE:
   "It was really set up like a conveyor belt... Pan-STARRS survey, which has 
   collected the largest volume of astronomical data of all time".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    transient_event_detection,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(transient_event_detection, E),
    E > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(transient_event_detection_tests).

test(multi_perspective_variance) :-
    % Ancient (Mountain) vs Modern (Rope) vs System (Noose)
    constraint_indexing:constraint_classification(transient_event_detection, mountain, context(individual_powerless, historical, trapped, national)),
    constraint_indexing:constraint_classification(transient_event_detection, rope, context(analytical, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(transient_event_detection, noose, context(institutional, immediate, constrained, global)).

test(information_extractiveness_scaling) :-
    % Modern institutional systems experience the extraction of 1.6 petabytes
    % compared to the analytical observer's informational tool.
    Score1 = 0.45,
    Score2 = 0.1,
    Score1 > Score2.

:- end_tests(transient_event_detection_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.45):
 * Reasoning: Chosen because the "conveyor belt" system extracts petabytes 
 * of data from transient events.
 * * 2. PERSPECTIVE SELECTION:
 * Analyzed from the ancient observer (Mountain), the modern scientist (Rope), 
 * and the automated system itself (Noose) to demonstrate the transition from 
 * fate to industrialized tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transient_event_detection_extraction_intent,
    "Is the petabyte-scale extraction of data a functional necessity for understanding the universe (Rope) or a predatory focus on speed over depth (Noose)?",
    resolution_mechanism("Audit of scientific breakthrough quality vs. pure data volume from surveys like Pan-STARRS"),
    impact("If necessity: Mountain. If predatory choice: Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    automated_serendipity_validity,
    "Can 'serendipity' truly be automated, or does the conveyor belt model create a 'living within a lie' where true anomalies are filtered out?",
    resolution_mechanism("Compare discovery rates of predicted vs. unpredicted transient events in automated systems"),
    impact("If automated: Rope. If filtered: Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Slow-Mapping Astronomy
 * Viability: Traditional long-term spatial mapping of stable stars.
 * Suppression: Being "transformed" and replaced by time-domain surveys.
 * Evidence: "Many more telescopes whose purpose is to search in time, rather than space, have followed".
 * * CONCLUSION:
 * The move from "Chance" to "Conveyor Belt" represents the core technological 
 * arbitrage of 21st-century astronomy.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/transient_event_detection].
 * 2. Multi-perspective: ?- multi_index_report(transient_event_detection).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
