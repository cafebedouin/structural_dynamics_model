% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: NASA Mars Climate Orbiter Project Failure Report (Azka Naheem, 2020)
% ============================================================================

:- module(constraint_mco_unit_discontinuity, []).

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
 * * constraint_id: mco_unit_system_discontinuity
 * human_readable: Persistence of Imperial Units in a Metric World
 * domain: technological/institutional
 * temporal_scope: 1960s - Present
 * spatial_scope: Global Science & Engineering (esp. US)
 * * SUMMARY:
 * The continued use of the Imperial unit system in a global scientific
 * community that has standardized on Metric is a classic Piton. It is a
 * legacy coordination system (a Rope) that has lost its function but
 * persists through cultural inertia. This creates constant friction, risk, and
 * catastrophic failures, like the Mars Climate Orbiter loss, which serves
 * as the canonical example of this Piton failing under load.
 * * KEY AGENTS:
 * - Navigation Engineer (JPL): Individual powerless; subject to the data 
 * provided by the upstream software interface.
 * - Software Architect (Lockheed): Institutional; established the original 
 * coding standards and unit choices for the SM_FORCES file.
 * - Mishap Investigation Board: Analytical; post-hoc observer identifying 
 * the systemic "Requirements Engineering" failure.
 * * NARRATIVE ARC:
 * The Imperial system was once a functional Rope for regional coordination.
 * In the context of global science, it has become a Piton. It provides no
 * coordinating benefit over the global standard (Metric) and introduces
 * high-friction costs and catastrophic risk. It persists only through inertia.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(mco_failure_trajectory, 0, 10).
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3. Low-to-moderate. The constraint extracts cognitive energy
% through constant conversion requirements and introduces a high risk of
% catastrophic failure (like the MCO loss).
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.1. Very low. The Metric system is not actively suppressed;
% the Imperial system simply persists through cultural and educational inertia.
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.1).

% Resistance score (0.0-1.0)
% Rationale: 0.8. Very high. The global scientific community actively
% resists the use of Imperial units, and failures like the MCO create
% immense pushback and demand for standardization.
domain_priors:resistance_score(mco_unit_system_discontinuity, 0.8).


% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.3).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.1).

% Make the constraint "evolve" by showing resistance increasing over time
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, resistance, 0.5, 0). % Initial friction
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, resistance, 0.8, 10). % High resistance after major failures

% Formally model the viable alternative
narrative_ontology:intent_viable_alternative(mco_failure_trajectory,
    'The Metric System (SI)',
    'The global standard for science and engineering, which simplifies collaboration and reduces conversion errors.').

% BENEFICIARIES & VICTIMS
constraint_beneficiary(mco_unit_system_discontinuity, us_aerospace_legacy_tooling).
constraint_victim(mco_unit_system_discontinuity, planetary_science_community).
constraint_victim(mco_unit_system_discontinuity, us_taxpayers).


/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JPL NAVIGATION ENGINEER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Bound by the incoming data stream; lacks 
         visibility into the upstream calculation logic.
   WHEN: immediate - Tactical focus on orbital insertion maneuvers.
   WHERE: trapped - Bound by the constraints of the existing "Ground Software" 
         architecture.
   SCOPE: local - Specific mission trajectory and thruster burns.
   
   WHY THIS CLASSIFICATION:
   To the engineer receiving the data, the software interface is a Mountain. 
   They assume the "Force" value provided is in the standard SI units 
   required by the system. There is no "choice" to interpret the number 
   differently; the math is presented as a natural law of the interface.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mco_unit_system_discontinuity,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(mco_unit_system_discontinuity),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA PROJECT MANAGEMENT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to establish ICDs and set engineering standards.
   WHEN: biographical - Planning the multi-year mission lifecycle.
   WHERE: mobile - Authority to bridge silos between Lockheed and JPL.
   SCOPE: national - Managing the US Mars exploration program.
   
   WHY THIS CLASSIFICATION:
   Management views the Interface Control Document (ICD) as a Rope—a 
   functional coordination mechanism that allows two different 
   organizations to work on the same hardware. They believe the 
   "system" is coordinating the teams effectively through these documents.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    mco_unit_system_discontinuity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(mco_unit_system_discontinuity, E),
    E < 0.95, % Invisibility of the extraction to the manager
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MISHAP INVESTIGATION BOARD - Piton
   --------------------------------------------------------------------------
   
   WHO: analytical - Post-failure body identifying the root cause.
   WHEN: historical - Evaluating the failure in the context of NASA's 
         "Faster, Better, Cheaper" era.
   WHERE: analytical - Not bound by the operational constraints of the mission.
   SCOPE: global - A case study in systems engineering failure.
   
   WHY THIS CLASSIFICATION:
   The Board identifies the use of Imperial units as a Piton. It was a
   legacy Rope that lost its function in a global, metric-first scientific
   project. Its persistence through inertia created a high-risk,
   low-benefit situation that ultimately failed under load, destroying the
   orbiter.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mco_unit_system_discontinuity,
    piton,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(mco_unit_system_discontinuity, S),
    S < 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(multi_perspective_asymmetry) :-
    % Engineer (Mountain) vs Manager (Rope) vs Investigator (Snare)
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, T1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(high_extraction_signature) :-
    domain_priors:base_extractiveness(mco_unit_system_discontinuity, E),
    E > 0.8.

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.9): The report highlights a total loss of the 
 * spacecraft ($125M). In Deferential Realism, technical failures that 
 * destroy capital through "silent" silos are high-extraction Snares.
 * 2. SUPPRESSION (0.7): I chose a high score because the error was 
 * effectively suppressed by a lack of end-to-end testing and the 
 * separation of software teams.
 * 3. PERSPECTIVE: The "Navigation Engineer" is the classic powerless 
 * agent who treats the "API" as a Mountain (law of the world).
 */

omega_variable(
    automated_verification_cost,
    "Would the cost of implementing automated unit-checking have 
     outweighed the risk of mission failure?",
    resolution_mechanism("Comparison of modern NASA mission requirements vs. 1999 standards"),
    impact("If Yes: The silo was a temporary Rope. If No: It was a 
            structural Snare from inception."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Standardized Unit Testing (Automated)
 * Viability: Modern software practices (unit test suites) would have 
 * caught the type/unit mismatch instantly.
 * Suppression: Suppressed by "Faster, Better, Cheaper" budget 
 * constraints and a lack of software engineering rigor.
 * * ALTERNATIVE 2: End-to-End System Simulation
 * Viability: Simulating the mission from the perspective of the ground 
 * station would have revealed the orbital deviation before arrival.
 * * CONCLUSION:
 * The existence of proven engineering alternatives (Simulation/Unit Testing) 
 * that were suppressed for cost reasons confirms the classification of the 
 * unit discontinuity as a Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [mco_unit_system_discontinuity].
% 2. Analyze: ?- multi_index_report(mco_unit_system_discontinuity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
