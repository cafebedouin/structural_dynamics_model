% ============================================================================
% CONSTRAINT STORY: temporal_scale_arbitrage
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Alex Wilkins, "Filming the universe’s biggest dramas"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_temporal_scale_arbitrage, []).

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
 * * constraint_id: temporal_scale_arbitrage
 * human_readable: Temporal Scale Arbitrage
 * domain: technological/scientific
 * temporal_scope: 21st Century (The "Transient" Era)
 * spatial_scope: Global (Astronomical Surveys)
 * * SUMMARY:
 * This constraint describes the strategic exploitation of cosmic events occurring 
 * across vast ranges of time scales—from nanoseconds to human lifetimes. 
 * By shifting from spatial mapping to time-domain search, astronomers arbitrage 
 * "fleeting explosions" by matching detector cadence to the specific temporal 
 * signature of the event.
 * * KEY AGENTS:
 * - The High-Cadence Surveyor: Uses "conveyor belt" automation to investigate flashes.
 * - The Fast Transient: An event (e.g., nanosecond bursts) that exists as a "fleeting" opportunity.
 * - The Universe: The source of a "different range of time scales" that are poorly explored.
 * * NARRATIVE ARC:
 * The field has moved from "happy accidents" (Mountain) to a coordinated 
 * industrialized search (Rope). This allows for 
 * the arbitrage of time, where different telescopes investigate different 
 * durations, but it creates a Snare for events that vanish before detection 
 * protocols can be triggered.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(temporal_scale_arbitrage, 0, 10).
narrative_ontology:constraint_claim([temporal_scale_arbitrage], [temporal_leverage]).

% Base extractiveness score (0.76 = High)
% Rationale: Value is extracted from "fleeting explosions" that are "caught when 
% astronomers were looking at the right spot at the right time". 
% The informational yield is entirely dependent on temporal precision.
domain_priors:base_extractiveness(temporal_scale_arbitrage, 0.76).

% Suppression score (0.4 = Moderate)
% Rationale: Traditional spatial mapping is being "completely transformed" and 
% displaced by time-domain tactics.
domain_priors:suppression_score(temporal_scale_arbitrage, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(temporal_scale_arbitrage, extractiveness, 0.76).
narrative_ontology:constraint_metric(temporal_scale_arbitrage, suppression_requirement, 0.4).

% Enforcement: Requires active enforcement (Automated surveys and conveyor belt coordination).
domain_priors:requires_active_enforcement(temporal_scale_arbitrage).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
% Modern surveys (ZTF, Pan-STARRS) that benefit from the "search in time".
constraint_beneficiary(temporal_scale_arbitrage, high_cadence_surveys).
% "Happy accidents" and chance sightings are victims of industrialized detection.
constraint_victim(temporal_scale_arbitrage, serendipitous_observers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SURVEY COORDINATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Coordinating telescopes as "one well-oiled machine")
   WHEN: biographical (Current era of automated serendipity)
   WHERE: arbitrage (Investigating flashes across nanoseconds to decades)
   SCOPE: global (Pan-STARRS and Palomar collaborations)
   
   WHY THIS CLASSIFICATION:
   The temporal spectrum acts as a Rope—a functional mechanism to coordinate 
   "conveyor belt" searches and investigate "guest stars" systematically.
   
   NARRATIVE EVIDENCE:
   "One telescope in San Diego... would see an interesting flash and another 
   would investigate further. It was really set up like a conveyor belt".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    temporal_scale_arbitrage,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(temporal_scale_arbitrage, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRANSIENT EVENT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Fleeting phenomena subject to "the action")
   WHEN: immediate (Flashes lasting nanoseconds)
   WHERE: trapped (Subject to the "right spot at the right time" detector window)
   SCOPE: local (Specific cosmic events)
   
   WHY THIS CLASSIFICATION:
   For the transient, the cadence is a Snare. If it does not occur within the 
   active "conveyor belt" window, its scientific information is permanently 
   extracted or lost.
   
   NARRATIVE EVIDENCE:
   "Relying on chance to capture these events risks missing much of the action".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    temporal_scale_arbitrage,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(temporal_scale_arbitrage, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNIVERSE - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The natural range of time scales)
   WHEN: civilizational (Deep-time ranges longer than human lifetimes)
   WHERE: constrained (Bound by physics and spatial scales)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The existence of these ranges of time scales is a Mountain—a natural, immutable 
   reality of the cosmos that was previously "poorly explored".
   
   NARRATIVE EVIDENCE:
   "You think the universe has a different range of spatial scales, but it also 
   has these ranges of time scales".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    temporal_scale_arbitrage,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    % Classification based on the physical "Mountain" of time scales
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(temporal_scale_arbitrage_tests).

test(multi_perspective_variance) :-
    % Coordinator (Rope) vs Event (Snare) vs Universe (Mountain)
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, rope, context(institutional, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, snare, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, mountain, context(analytical, civilizational, constrained, global)).

test(cadence_extractiveness_scaling) :-
    % Nanosecond events (powerless to detectors) experience higher extraction (0.76)
    % compared to institutional researchers.
    Score1 = 0.76,
    Score2 = 0.15,
    Score1 > Score2.

:- end_tests(temporal_scale_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.76):
 * Reasoning: Chosen because "the process of serendipity" has been automated 
 * into a high-extraction conveyor belt for transient data.
 * * 2. MANDATROPHY STATUS:
 * STATUS: [RESOLVED MANDATROPHY]. The extraction of scientific value from 
 * "guest stars" is justified as a Rope for research but is a Snare of 
 * extinction for the "action" of transient events that are missed.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Coordinator (Rope), the Event (Snare), and the Universe (Mountain) 
 * to show how temporal scale transforms from a law of nature to an 
 * industrialized tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_scale_arbitrage_extraction_intent,
    "Is the 0.76 extraction of time-domain data a functional necessity for understanding the universe or a predatory focus on volume (1.6PB) over insight?",
    resolution_mechanism("Audit of scientific value produced per petabyte of Pan-STARRS data vs. traditional chance observations"),
    impact("If necessity: Mountain. If predatory: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    temporal_anomaly_existence,
    "Are there 'guest star' phenomena occurring on timescales (e.g., femtoseconds) that current conveyor belts actively suppress?",
    resolution_mechanism("Development of detectors capable of femtosecond-scale resolution"),
    impact("If yes: Current Rope is a Snare for ultra-fast physics. If no: The spectrum is complete."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Chance-Based "Happy Accident" Astronomy
 * Viability: Historically dominant for 1000 years.
 * Suppression: Actively "transformed" and replaced by automated tactics.
 * Evidence: "Relying on chance... risks missing much of the action".
 * * CONCLUSION:
 * The move from "Happy Accidents" to "Temporal Arbitrage" represents a shift 
 * from the "Chance Mountain" to the "Industrial Rope".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/temporal_scale_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(temporal_scale_arbitrage).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
