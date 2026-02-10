% ============================================================================
% CONSTRAINT STORY: martian_signal_latency
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: NASA JPL / Planetary Science Mission Operations
% ============================================================================

:- module(constraint_martian_latency, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: martian_signal_latency
 * human_readable: Martian Signal Latency (One-Way Light Time)
 * domain: technological/scientific
 * temporal_scope: 1965-Present
 * spatial_scope: Interplanetary (Earth-Mars)
 * * SUMMARY:
 * Martian signal latency is the absolute delay in communication between Earth 
 * and Mars caused by the speed of light. Ranging from 3 to 22 minutes depending 
 * on orbital positions, this constraint dictates the architecture of all 
 * deep-space missions, rendering real-time teleoperation of assets impossible.
 * * KEY AGENTS:
 * - Rover Driver (JPL): Individual powerless; must wait for "next sol" to 
 * see the results of a command.
 * - Mission Architect: Institutional; designs the "Rope" of autonomy to 
 * bypass the "Snare" of latency.
 * - Radio Physicist: Analytical; measures the constraint as a fundamental 
 * property of the universe.
 * * NARRATIVE ARC:
 * Latency is the ultimate Mountain—a natural law that cannot be negotiated. 
 * In the early days of exploration, it was a Snare that threatened to destroy 
 * any lander that couldn't think for itself. To survive, NASA wove the 
 * Rope of autonomy, transforming a terminal physical limit into a 
 * predictable coordination rhythm for mission operations.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(mariner_to_msl_era, 0, 10).
narrative_ontology:constraint_claim(martian_signal_latency, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.1. Low; the speed of light does not "extract" value in a 
% coercive sense, but it imposes a high "opportunity cost" on mission 
% duration and real-time scientific discovery.
domain_priors:base_extractiveness(martian_signal_latency, 0.1).

% Suppression score (0.0-1.0)
% Rationale: 1.0. Absolute; signal latency suppresses the very possibility 
% of real-time remote control. There are no known physical alternatives 
% to electromagnetic wave propagation.
domain_priors:suppression_score(martian_signal_latency, 1.0).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(martian_signal_latency, extractiveness, 0.1).
narrative_ontology:constraint_metric(martian_signal_latency, suppression_requirement, 1.0).

% Enforcement requirements
% Emerges naturally from the physics of the universe.
domain_priors:emerges_naturally(martian_signal_latency).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(martian_signal_latency, autonomy_software_development).
narrative_ontology:constraint_victim(martian_signal_latency, real_time_human_intuition).
narrative_ontology:constraint_victim(martian_signal_latency, mission_cadence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ROVER DRIVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Bound by the laws of physics; cannot "drive" 
         fast enough to outrun the delay.
   WHEN: immediate - Tactical focus on the current wheel-turn or hazard.
   WHERE: trapped - Bound by the Earth-Mars distance.
   SCOPE: local - Focused on the immediate traverse of the rover.
   
   WHY THIS CLASSIFICATION:
   For the driver, the delay is a Mountain. It is an unchangeable, 
   non-negotiable fact of their workday. They cannot "negotiate" with 
   the speed of light to get a faster image back from the surface.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    martian_signal_latency,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(martian_signal_latency),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MISSION MANAGER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design the mission architecture and 
         software capabilities.
   WHEN: biographical - Planning the multi-year mission arc.
   WHERE: mobile - Can choose to invest in more autonomy or slower 
         "directed" sequences.
   SCOPE: national - Managing the US planetary exploration budget.
   
   WHY THIS CLASSIFICATION:
   Management views the latency as a Rope. It is the core coordination 
   mechanism that dictates how they structure their teams (e.g., Sol-path 
   planning cycles). They "weave" the mission's software around the delay, 
   using it as a structural anchor for the entire project.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    martian_signal_latency,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(martian_signal_latency, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MARS COLONIST (FUTURE) - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency on Mars but is socially/technologically 
         bound to Earth for resources/data.
   WHEN: biographical - Lifetime spent on another planet.
   WHERE: constrained - High cost of communication; cannot "call home" for help.
   SCOPE: global - Impact on interplanetary civilization.
   
   WHY THIS CLASSIFICATION:
   For a future colonist, the 20-minute lag is a Snare. It extracts 
   human connection and chokes off the ability to receive emergency 
   instructions from Earth in real-time. It separates the two worlds 
   into distinct, often asynchronous realities.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    martian_signal_latency,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(martian_signal_latency, S),
    S > 0.8,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(martian_latency_tests).

test(multi_perspective_variance) :-
    % Driver (Mountain) vs Manager (Rope) vs Colonist (Snare)
    constraint_indexing:constraint_classification(martian_signal_latency, T1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(martian_signal_latency, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(martian_signal_latency, T3, context(individual_moderate, biographical, constrained, global)),
    T1 \= T2, T2 \= T3.

test(absolute_suppression) :-
    % Latency should always show maximum suppression of real-time control
    domain_priors:suppression_score(martian_signal_latency, S),
    S =:= 1.0.

:- end_tests(martian_latency_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (1.0): This is the defining characteristic of 
 * the speed of light. It is the only constraint in this system where 
 * suppression is legally and physically absolute.
 * 2. CLASSIFICATION: I primary-labeled this as a 'Mountain'. While 
 * mission managers treat it as a 'Rope' for planning, they are 
 * effectively coordinating within a prison of physics.
 * 3. EXTRACTIVENESS: I set this low (0.1) because the universe is not 
 * "extractive" in a moral sense, though the *result* of the constraint 
 * is the extraction of time.
 */

omega_variable(
    quantum_entanglement_comms,
    "Will quantum entanglement eventually provide a non-extractive Rope 
     that bypasses the light-speed Mountain?",
    resolution_mechanism("Monitor theoretical physics for proof of FTL information transfer"),
    impact("If Yes: Signal latency was a temporary Snare. If No: It is an 
            eternal Mountain of the physical world."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: On-Site Human Presence
 * Viability: Sending humans to Mars eliminates the latency constraint 
 * for local operations, though it creates a new "Mountain" of life 
 * support.
 * Suppression: Suppressed by current launch technology, budget, and 
 * radiation risk thresholds.
 * * ALTERNATIVE 2: Advanced AI Autonomy
 * Viability: Moving the "Decision Brain" to the rover (e.g., AutoNav) 
 * allows the mission to ignore the latency Mountain for local safety.
 * * CONCLUSION:
 * The existence of Autonomy as a viable alternative shifts the 
 * experience of the latency from a terminal Snare (mission death) 
 * to a manageable Rope (planned delay).
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [martian_signal_latency].
% 2. Analyze: ?- multi_index_report(martian_signal_latency).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

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
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(martian_signal_latency, 0.02).
narrative_ontology:constraint_metric(martian_signal_latency, theater_ratio, 0.02).
