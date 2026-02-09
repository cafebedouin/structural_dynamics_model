% ============================================================================
% CONSTRAINT STORY: fittss_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Paul Fitts (1954) / Human-Computer Interaction (HCI)
% ============================================================================

:- module(constraint_fittss_law, []).

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
 * * constraint_id: fittss_law
 * human_readable: Fitts’s Law (Speed-Accuracy Tradeoff)
 * domain: technological/social
 * temporal_scope: Permanent (Biological/Motor Constraints)
 * spatial_scope: Global (Human-Machine Interaction)
 * * SUMMARY:
 * Fitts’s Law is a predictive model of human movement that states the time 
 * required to rapidly move to a target area is a function of the ratio 
 * between the distance to the target and the width of the target. It 
 * represents the fundamental biological limit of the human motor system’s 
 * information capacity.
 * * KEY AGENTS:
 * - The Ergonomist: Analytical observer mapping the logarithmic decay of 
 * precision as speed increases.
 * - The UI Designer: Institutional agent using the law as a "Rope" to 
 * coordinate efficient interface layouts (e.g., placing the "Save" button 
 * in a high-access area).
 * - The Repetitive Task Worker: Individual powerless agent whose physical 
 * "output" is extracted by a system that demands maximum speed at the 
 * edge of their biological accuracy.
 * * NARRATIVE ARC:
 * Fitts's Law functions as a "Mountain"—a fixed feature of human neuro-muscular 
 * hardware. For the 
 * industrial engineer, it is a "Rope" used to pull a factory floor toward 
 * peak efficiency. 
 * However, for the data-entry clerk or assembly worker, it becomes a "Snare" 
 * as the system's "Efficiency" requirement strangles their physical health, 
 * leading to the extraction of their long-term motor vitality (RSI) 
 *.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================= */

% Required for structural identification
narrative_ontology:interval(fittss_law_interval, 0, 10).
narrative_ontology:constraint_claim(fittss_law, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The law allows institutions to extract "speed" 
% from human agents by optimizing targets. If pushed too hard, it extracts 
% physical health (motor strain).
domain_priors:base_extractiveness(fittss_law, 0.4).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). The law suppresses "Infinite Precision." Humans 
% cannot move infinitely fast with infinite accuracy; the law makes the 
% alternative of "perfect fast movement" physically invisible.
domain_priors:suppression_score(fittss_law, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fittss_law, extractiveness, 0.4).
narrative_ontology:constraint_metric(fittss_law, suppression_requirement, 0.7).

% Enforcement requirements
% Emerges naturally from the human nervous system and motor-control loops.
domain_priors:emerges_naturally(fittss_law).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
% Beneficiary: Institutional managers and UI architects who gain speed metrics.
narrative_ontology:constraint_beneficiary(fittss_law, [industrial_engineers, ui_designers]).
% Victim: Individual workers subject to repetitive motion strain.
narrative_ontology:constraint_victim(fittss_law, [assembly_line_workers, data_entry_clerks]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXPERIMENTAL PSYCHOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the human biological substrate.
   WHEN: civilizational - Viewing human motor capacity as an evolutionary constant.
   WHERE: trapped - The nervous system's processing speed is a fixed point.
   SCOPE: global - Universal across all humans.
   
   WHY THIS CLASSIFICATION:
   To the scientist, Fitts’s Law is a Mountain. It is an unchangeable 
   feature of human "hardware." Regardless of culture or technology, the 
   mathematical relationship between target size, distance, and time 
   remains a fixed peak in the topography of human capability.
   
   NARRATIVE EVIDENCE:
   Fitts's original 1954 experiments showed a constant "Index of Performance" 
   measured in bits per second, suggesting a hard-wired bandwidth for 
   human motion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fittss_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRODUCT DESIGNER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design tools and environments.
   WHEN: biographical - Creating products that users will master over years.
   WHERE: arbitrage - Can shift button locations or sizes to optimize workflow.
   SCOPE: national - Mass-market software or hardware standards.
   
   WHY THIS CLASSIFICATION:
   For the designer, the law is a Rope. It is a vital coordination 
   mechanism. By using Fitts's Law to place common actions in "easy" 
   reach (like the corners of a screen), they "pull" the user toward 
   an efficient and satisfying experience, using the constraint as 
    a tether for usability.
   
   NARRATIVE EVIDENCE:
   The "Magic Pixels" at the corners of a screen have infinite width in 
   one dimension (you can't overshoot them), making them the most 
   efficient "Rope" for UI navigation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fittss_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(fittss_law, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WAREHOUSE PICKER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to high-frequency quotas.
   WHEN: immediate - Today’s production target and picking rate.
   WHERE: constrained - High cost to ignoring the quota (termination).
   SCOPE: local - Immediate physical workstation/aisle.
   
   WHY THIS CLASSIFICATION:
   For the worker, the law is a Snare. The employer has optimized the 
   workspace (the Rope) for maximum throughput, but the worker must 
   now operate at the very edge of their biological limit for 10 hours. 
   The extraction of every millisecond strangles their comfort, eventually 
   leading to injury. The "Efficiency" is a trap they cannot exit.
   
   NARRATIVE EVIDENCE:
   "I have to hit the scanner perfectly every 3 seconds or the system flags 
   me; the distance is too far for that speed, and my wrist is starting 
   to burn."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fittss_law,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(fittss_law, E),
    E > 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(fittss_law_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(fittss_law, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(fittss_law, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(fittss_law, snare, context(powerless, immediate, constrained, local)).

test(power_extractiveness_motor) :-
    % Powerless workers feel the total extraction of their physical health (Snare).
    % Institutional designers use the law for coordination (Rope).
    ContextPowerless = context(powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(fittss_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(fittss_law, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_biology) :-
    % Civilizational scale = Mountain
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(fittss_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: Fitts's Law extracts "precision." By forcing humans to 
 * trade accuracy for speed, the beneficiary (manager) extracts more 
 * output per second.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Psychologist (Law), the Designer (Tool), and the 
 * Warehouse Worker (Victim) to highlight the indexical shift from 
 * "Math" to "Design" to "Extraction".
 * * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: It's an unchangeable biological limit.
 * Institutional → Rope: It's used as a "nudge" for efficiency.
 * Powerless → Snare: It's a performance trap that leads to RSI.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    haptic_augmentation_limits,
    "Can haptic feedback or neural interfaces untie the Fitts's Snare (Rope) 
    by bypassing the traditional motor-loop (Mountain)?",
    resolution_mechanism("Long-term study of BMI (Brain-Machine Interface) 
    accuracy vs. traditional mouse/limb input"),
    impact("If Rope: Human throughput can expand beyond current biological 
    bandwidth. If Mountain: The law is fundamental to all consciousness-to-matter 
    translations."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * * ALTERNATIVE 1: Automated Robotic Assistance (Cobots)
 * Viability: High. Robots aren't subject to Fitts's Law in the same 
 * way; they can move with high speed and high precision.
 * Suppression: Moderate. Often rejected in low-margin industries 
 * because human labor (subject to the Snare) is currently cheaper 
 * than robotic CAPEX.
 * * CONCLUSION:
 * The existence of mechanical "Exits" (Automation) that are suppressed 
 * by labor costs confirms that for the warehouse worker, Fitts’s Law is a 
 * Snare of economic necessity, not just a biological Mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_fittss_law].
 * 2. Multi-perspective: ?- multi_index_report(fittss_law).
 */

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
domain_priors:theater_ratio(fittss_law, 0.05).
narrative_ontology:constraint_metric(fittss_law, theater_ratio, 0.05).
