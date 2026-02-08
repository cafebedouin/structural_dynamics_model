% ============================================================================
% CONSTRAINT STORY: mars_rover_navigational_autonomy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Autonomy for Mars Rovers: Past, Present, and Future (Bajracharya et al., 2008)
% ============================================================================

:- module(constraint_mars_autonomy, []).

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
 * * constraint_id: mars_rover_navigational_autonomy
 * human_readable: Mars Surface Navigational Autonomy (AutoNav)
 * domain: technological
 * temporal_scope: 1997-2004 (Sojourner to Spirit/Opportunity)
 * spatial_scope: Martian Surface / JPL Control
 * * SUMMARY:
 * Navigational autonomy on Mars is constrained by the "speed-of-light" latency 
 * (Mountain) and the high risk of irrecoverable hardware loss (Snare). To 
 * overcome these, NASA implemented GESTALT and AutoNav—functional coordination 
 * mechanisms (Ropes) that allow rovers to sense hazards and plan paths 
 * without constant human intervention.
 * * KEY AGENTS:
 * - The Rover (Sojourner/Spirit): Individual powerless; subject to the 
 * physical terrain and the "safety logic" of its own code.
 * - Rover Planners (JPL): Institutional; define the "cost maps" and 
 * safety thresholds that govern what the rover perceives as an obstacle.
 * - Robotics Researcher: Analytical; evaluates the efficiency of 
 * autonomous navigation vs. human-commanded "blind driving."
 * * NARRATIVE ARC:
 * Autonomy evolved from a simple 2D hazard avoidance (Sojourner) to complex 
 * 3D stereo-vision path planning (Spirit). While intended as a Rope to 
 * increase mission velocity, the software's conservative safety limits 
 * can function as a Snare, "extracting" mission time by causing the rover 
 * to stop or retreat from navigable but "scary" terrain.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(mars_autonomy_evolution, 0, 10).
narrative_ontology:constraint_claim(mars_rover_navigational_autonomy, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.4. Moderate; autonomous cycles "extract" significant 
% onboard CPU time and battery power (computational cost), potentially 
% reducing time available for science instruments.
domain_priors:base_extractiveness(mars_rover_navigational_autonomy, 0.4).

% Suppression score (0.0-1.0)
% Rationale: 0.5. The system suppresses high-risk maneuvers. If the 
% "safety margin" is too high, it suppresses the visibility of valid 
% navigation paths.
domain_priors:suppression_score(mars_rover_navigational_autonomy, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(mars_rover_navigational_autonomy, extractiveness, 0.4).
narrative_ontology:constraint_metric(mars_rover_navigational_autonomy, suppression_requirement, 0.5).

% Enforcement requirements
% Requires active enforcement (Onboard hazard avoidance software 
% triggers a stop if safety constraints are violated).
domain_priors:requires_active_enforcement(mars_rover_navigational_autonomy).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(mars_rover_navigational_autonomy, mission_longevity).
constraint_beneficiary(mars_rover_navigational_autonomy, navigational_safety).
constraint_victim(mars_rover_navigational_autonomy, traverse_speed).
constraint_victim(mars_rover_navigational_autonomy, cpu_availability_for_science).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ROVER (SPIRIT) - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Cannot alter its own code; must obey the 
         hazard avoidance logic.
   WHEN: immediate - Tactical millisecond-by-millisecond wheel control.
   WHERE: trapped - Bound by the physical geometry of Martian rocks and slopes.
   SCOPE: local - Immediate sensor range (1-5 meters).
   
   WHY THIS CLASSIFICATION:
   For the rover, the software's safety parameters (e.g., "stop if tilt > 20°") 
   are as unchangeable as the Martian gravity. It has zero degrees of 
   freedom to ignore its own "Fear" of a hazard.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mars_rover_navigational_autonomy,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(mars_rover_navigational_autonomy),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ROVER PLANNERS (JPL) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to adjust "cost" weights and path heuristics.
   WHEN: biographical - Planning the 90-day (and beyond) mission arc.
   WHERE: mobile - Can send "overrides" or change the software's caution level.
   SCOPE: regional - Planning traverse routes across kilometers of Gusev Crater.
   
   WHY THIS CLASSIFICATION:
   Planners view autonomy as a Rope—a functional coordination tool. If the 
   rover is too conservative, they can "loosen" the rope by adjusting 
   thresholds to allow more daring drives, or "tighten" it in dangerous areas.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    mars_rover_navigational_autonomy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(mars_rover_navigational_autonomy, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MISSION EFFICIENCY ANALYST - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the ratio between "thinking" and "driving."
   WHEN: historical - Comparing Pathfinder's 100m traverse to Spirit's kilometers.
   WHERE: analytical - Free from the operational "fear" of losing the rover.
   SCOPE: global - Impact on the future of planetary exploration.
   
   WHY THIS CLASSIFICATION:
   The analyst sees the Snare. Onboards like Spirit spent hours "thinking" 
   (computing stereo vision) for every minute of driving. This extraction 
   of time/power chokes the potential science yield, particularly when the 
   rover gets "stuck" in a computational loop due to ambiguous terrain.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mars_rover_navigational_autonomy,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(mars_rover_navigational_autonomy, S),
    S > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(mars_autonomy_tests).

test(autonomy_utility_variance) :-
    % Institutional planners see a tool (Rope), local rover sees a limit (Mountain)
    constraint_indexing:constraint_classification(mars_rover_navigational_autonomy, T1, context(institutional, biographical, mobile, regional)),
    constraint_indexing:constraint_classification(mars_rover_navigational_autonomy, T2, context(powerless, immediate, trapped, local)),
    T1 \= T2.

test(computational_extraction) :-
    % Extraction of battery/time is significant for Mars missions
    domain_priors:base_extractiveness(mars_rover_navigational_autonomy, E),
    E > 0.3.

:- end_tests(mars_autonomy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): I chose this to reflect the "Tax" on mission 
 * time. As noted in the Bajracharya paper, autonomous navigation is 
 * significantly slower than blind driving because the rover must stop, 
 * image, and think.
 * 2. CLASSIFICATION: I primary-labeled this as a 'Rope' because it 
 * coordinated the transition from 100-meter missions (Sojourner) to 
 * 10-kilometer missions (Spirit/Opportunity).
 * 3. PERSPECTIVE: The "Analytical" view of the Snare reflects the 
 * 'Replication Crisis' in autonomy: the more safe we make it, the less 
 * productive it becomes.
 */

omega_variable(
    autonomy_trust_threshold,
    "At what point does human 'Trust' in the algorithm allow the Rope 
     to slacken enough to become a speed-enhancing tool vs. a speed-choking limit?",
    resolution_mechanism("Comparative analysis of traverse speed in 'AutoNav' vs. 'Directed' modes on MER"),
    impact("If Directed is consistently faster: AutoNav is a Snare. 
            If AutoNav allows more meters per sol: It is a Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Blind Driving (Direct Sequencing)
 * Viability: Used extensively for "safe" flat terrain. High speed, low 
 * onboard computation.
 * Suppression: Suppressed in "unknown" or "hazardous" terrain because 
 * the 20-minute latency (Mountain) makes it an operational Snare.
 * * ALTERNATIVE 2: Continuous Drive (Spirit/Opportunity upgrade)
 * Viability: Later mission upgrades allowed for imaging during wheel motion.
 * * CONCLUSION:
 * The existence of "Blind Driving" as a faster but riskier alternative 
 * highlights that Autonomy is a chosen Rope—we trade time (Extraction) 
 * for the avoidance of a permanent mission-ending Mountain (a crash).
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [mars_rover_navigational_autonomy].
% 2. Analyze: ?- multi_index_report(mars_rover_navigational_autonomy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, snare, agent_power(powerless)).

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Functional coordination mechanism — primarily substantive
domain_priors:theater_ratio(mars_rover_navigational_autonomy, 0.13).
narrative_ontology:constraint_metric(mars_rover_navigational_autonomy, theater_ratio, 0.13).
