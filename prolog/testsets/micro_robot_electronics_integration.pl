% ============================================================================
% CONSTRAINT STORY: micro_robot_electronics_integration
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Hanson et al., "Electrokinetic propulsion for electronically 
%         integrated microscopic robots," PNAS (2025).
% ============================================================================

:- module(constraint_micro_robot_integration, []).

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
 * * constraint_id: micro_robot_electronics_integration
 * human_readable: The Structural Barrier to Microrobot Electronics Integration
 * domain: technological/engineering
 * temporal_scope: 2020-2025 (Current Research Era)
 * spatial_scope: Microscopic (Sub-millimeter scale)
 * * SUMMARY:
 * This constraint represents the fundamental "gap" in microscopic robotics: 
 * the inability to integrate semiconductor microelectronics with existing 
 * propulsion platforms. While macroscopic robots benefit from 
 * unified electronic control, microscopic platforms are traditionally 
 * bifurcated between mechanically robust propulsion and intelligent 
 * electronic circuitry, "limiting their potential for intelligence".
 * * KEY AGENTS:
 * - The Microrobot (EK-Robot): Individual powerless; subject to the 
 * physical laws of electrokinetic flows and photovoltaic control.
 * - The Research Engineer (Hanson/Miskin): Institutional; rule-making 
 * agents seeking to "upgrade" particle platforms into integrated robots.
 * - The Analytical Observer (PNAS Reviewer): Evaluates the efficiency of 
 * coordinated swarming and waypoint navigation.
 * * NARRATIVE ARC:
 * The lack of integration functioned as a Snare for the field, choking off 
 * the development of autonomous microrobotic intelligence. The 
 * introduction of electrokinetic propulsion (ek-robots) serves as a Rope—a 
 * functional coordination mechanism that allows propulsion and electronics 
 * to share a single semiconductor platform.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for ID extraction
narrative_ontology:interval(ek_robot_integration_cycle, 0, 10).
narrative_ontology:constraint_claim(micro_robot_electronics_integration, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.2. Low; the constraint is a technical barrier that "extracts" 
% scientific potential and research funding without a functional yield.
domain_priors:base_extractiveness(micro_robot_electronics_integration, 0.2).

% Suppression score (0.0-1.0)
% Rationale: 0.6. Moderate/High; existing propulsion methods (chemically 
% propelled, magnetic, etc.) often "suppress" the inclusion of on-board 
% logic because they are incompatible with standard lithographic processes.
domain_priors:suppression_score(micro_robot_electronics_integration, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(micro_robot_electronics_integration, extractiveness, 0.2).
narrative_ontology:constraint_metric(micro_robot_electronics_integration, suppression_requirement, 0.6).

% Enforcement requirements
% Emerges naturally from the physical limits of scaling semiconductor 
% fabrication to fluidic environments.
domain_priors:emerges_naturally(micro_robot_electronics_integration).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(micro_robot_electronics_integration, mechanical_simplicity).
narrative_ontology:constraint_victim(micro_robot_electronics_integration, autonomous_microrobotic_intelligence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MICROROBOT - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Bound by the physics of the Reynolds number and 
         fluidic drag; cannot alter its own circuitry once fabricated.
   WHEN: immediate - Tactical millisecond response to optical control pulses.
   WHERE: trapped - Bound by the liquid environment of the test chamber.
   SCOPE: local - Navigating micrometer-scale waypoints.
   
   WHY THIS CLASSIFICATION:
   For the bot, the relationship between current ($I$) and speed ($v$) is a 
   Mountain ($v \propto I$). It is an unchangeable law of its world. If the 
   circuitry isn't integrated, it remains a passive particle, unable to 
   negotiate its trajectory.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    micro_robot_electronics_integration,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(micro_robot_electronics_integration),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RESEARCH ENGINEER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power to design the lithographic 
         masks and optical control loops.
   WHEN: biographical - Planning the multi-year development of 
         "smart" microscopic swarms.
   WHERE: mobile - Can pivot between different electrokinetic geometries 
         (two-diode vs. four-diode bots) .
   SCOPE: regional - Managing the lab-scale "swarm" environment.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the electrokinetic model is a Rope. It is a functional 
   coordination tool that links electronics with propulsion. They "weave" 
   the control signals (lasers) to synchronize the movement of independent 
   agents toward a waypoint.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    micro_robot_electronics_integration,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(micro_robot_electronics_integration, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the field's previous stagnation.
   WHEN: historical - Looking at the 20-year history of self-propelled particles.
   WHERE: analytical - Free from the design constraints of a specific lab.
   SCOPE: global - Evaluating the worldwide search for microscopic intelligence.
   
   WHY THIS CLASSIFICATION:
   The investigator identifies the lack of integration as a Snare. By 
   separating control from propulsion, the field "choked" its own ability 
   to scale up to complex, autonomous tasks. The "extraction" was the loss 
   of intelligence potential in favor of purely mechanical particles.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    micro_robot_electronics_integration,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(micro_robot_electronics_integration, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(micro_robot_integration_tests).

test(multi_perspective_conflict) :-
    % Bot (Mountain) vs Scientist (Rope) vs Historian (Snare)
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, T1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(extraction_threshold_check) :-
    % Extraction of intelligence potential is moderate but noticeable
    domain_priors:base_extractiveness(micro_robot_electronics_integration, E),
    E > 0.15.

:- end_tests(micro_robot_integration_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.2): I chose a low score because the constraint isn't 
 * actively hostile, but passively extractive of the field's "intelligent 
 * potential".
 * 2. PERSPECTIVE: The microrobot's view as 'Mountain' highlights the 
 * unyielding nature of the fluid physics (ek-flows) it must obey to 
 * achieve "one body length per second".
 * 3. SNARE LOGIC: The analytical view recognizes that mechanical-only 
 * platforms "choked" the path to intelligence, a signature of the Snare.
 */

omega_variable(
    intelligence_scaling_bottleneck,
    "Does the current requirement for off-board optical control (lasers) 
     simply create a new Snare of external dependency?",
    resolution_mechanism("Evaluation of bots with fully autonomous on-board decision logic"),
    impact("If Yes: The system remains a Snare. If No: It has successfully 
            transformed into a fully internal Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Optical Tweezers
 * Viability: Highly precise for single particles[cite: 51].
 * Suppression: Suppressed by the need for massive, expensive off-board 
 * infrastructure which cannot scale to autonomous swarms.
 * * ALTERNATIVE 2: Chemically Propelled Janus Particles
 * Viability: Excellent for raw speed and robustness.
 * Suppression: Actively suppressed by scientists seeking electronics 
 * integration, as chemistry is difficult to "program" digitally.
 * * CONCLUSION:
 * The existence of mechanical-only alternatives (Janus particles) proves that 
 * the lack of electronics was a choice—a choice that functioned as a Snare 
 * for the development of "smart" microrobots.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [micro_robot_electronics_integration].
% 2. Analyze: ?- multi_index_report(micro_robot_electronics_integration).

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
% Extraction is substantive — the constraint's costs are real, not theatrical
domain_priors:theater_ratio(micro_robot_electronics_integration, 0.15).
narrative_ontology:constraint_metric(micro_robot_electronics_integration, theater_ratio, 0.15).
