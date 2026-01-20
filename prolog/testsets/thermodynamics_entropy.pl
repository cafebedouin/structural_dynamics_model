% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Second Law of Thermodynamics / Statistical Mechanics
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
 * * constraint_id: thermodynamics_entropy
 * human_readable: The Second Law of Thermodynamics (Entropy)
 * domain: technological
 * temporal_scope: Permanent (Universal Physical Law)
 * spatial_scope: Global (Universal)
 * * SUMMARY:
 * Entropy is a measure of molecular disorder within a system and represents 
 * the inevitable decay of concentrated energy into unusable heat. The Second 
 * Law of Thermodynamics dictates that in any isolated system, entropy must 
 * increase over time, imposing a fundamental "arrow of time" and a hard limit 
 * on the efficiency of any engine or biological process.
 * * KEY AGENTS:
 * - The Cosmologist: Analytical observer tracking the "Heat Death" of the universe.
 * - The Industrial Engineer: Institutional agent attempting to maximize 
 * efficiency while fighting the "entropy tax" of friction and heat loss.
 * - The Biological Organism: Individual agent trapped in a local struggle to 
 * maintain order (negentropy) by extracting energy from the environment.
 * * NARRATIVE ARC:
 * Entropy functions as a "Mountain" of cosmic reality—the ultimate physical 
 * boundary. To the engineer, it is a "Rope" for coordination (defining the 
 * Carnot limit for engines). However, for the aging organism, it becomes 
 * a "Noose" that slowly strangles the ability to maintain internal order, 
 * leading to the inevitable extraction of life itself.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(thermodynamics_entropy_interval, 0, 10).
narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.8). Entropy is the ultimate extractor, turning 
% usable energy into unusable waste and eventually extracting the 
% structural integrity of all physical systems.
domain_priors:base_extractiveness(thermodynamics_entropy, 0.8).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). The Second Law suppresses the possibility of 
% perpetual motion or "free energy." These alternatives are not just 
% invisible; they are physically forbidden by the system of reality.
domain_priors:suppression_score(thermodynamics_entropy, 0.7).

% Enforcement requirements
% Emerges naturally from the statistical probability of microstates.
domain_priors:emerges_naturally(thermodynamics_entropy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.8).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(thermodynamics_entropy, [none, time_itself]).
constraint_victim(thermodynamics_entropy, [all_biological_life, complex_machinery]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE THEORETICAL PHYSICIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the fundamental laws of the universe.
   WHEN: civilizational - Viewing the timeline from the Big Bang to Heat Death.
   WHERE: trapped - Logic and physics cannot bypass the statistical odds of disorder.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the physicist, entropy is a Mountain. It is an unchangeable feature of 
   the universe's hardware. It is the geometric slope of time itself. You 
   cannot "innovate" your way past the Second Law; you can only map its 
   peaks and valleys.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    thermodynamics_entropy,
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
   PERSPECTIVE 2: THE ENERGY ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design energy grids and power plants.
   WHEN: biographical - Managing infrastructure over a 50-year career.
   WHERE: arbitrage - Can shift between energy sources to manage entropy.
   SCOPE: national - Continental energy systems.
   
   WHY THIS CLASSIFICATION:
   For the institutional engineer, the laws of thermodynamics are a Rope. 
   They act as a coordination mechanism for efficiency. By understanding 
   the Carnot cycle and heat transfer, they "pull" as much work as 
   possible out of the system, using the constraint to standardize 
   technology and prevent waste.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    thermodynamics_entropy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(thermodynamics_entropy, E),
    E > 0.5, % High extraction requires high-level coordination
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LIVING ORGANISM - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the biological "tax" of aging.
   WHEN: immediate - Today's need for calories to maintain bodily order.
   WHERE: constrained - Cannot exit the requirements of biological metabolism.
   SCOPE: local - Immediate internal physiology.
   
   WHY THIS CLASSIFICATION:
   For the individual organism, entropy is a Noose. Life is a constant 
   struggle to "pump out" disorder (Schrödinger's negentropy). As time 
   passes, the difficulty of maintaining internal order increases. The 
   Noose of decay slowly tightens, eventually extracting the 
   agent's vitality until the system collapses into equilibrium (death).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    thermodynamics_entropy,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(thermodynamics_entropy, E),
    E > 0.7, % Severe extraction of health and life
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(thermodynamics_entropy, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(thermodynamics_entropy, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(thermodynamics_entropy, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability) :-
    % Civilizational view of physical laws = Mountain
    constraint_indexing:effective_immutability_for_context(context(analytical, civilizational, trapped, global), mountain).

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Entropy is the ultimate "extractive" force. It takes high-quality energy 
 * and extracts its utility, leaving only low-grade heat. It is the tax 
 * that every physical process must pay to exist in time.
 * * 2. SUPPRESSION SCORE (0.7):
 * The law suppresses "Perpetual Motion." While people can imagine it, 
 * the physical reality of the system suppresses it so effectively 
 * that it remains a fantasy.
 * * 3. PERSPECTIVE SELECTION:
 * The Observer (Mountain) sees the cosmic trend. The Engineer (Rope) 
 * sees the efficiency limit. The Organism (Noose) sees the decay of self.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heat_death_timeline,
    "Is the universe truly a closed system that will reach maximum entropy (Mountain), or is there an unknown 'exit' via quantum fluctuations or multiversal dynamics (Rope)?",
    resolution_mechanism("Resolution of the relationship between general relativity and quantum information theory"),
    impact("If Mountain: Absolute termination of all agency. If Rope: Long-term survival might be possible via arbitrage."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * Were there alternatives to this constraint that were suppressed?
 * * ALTERNATIVE 1: Steady-State Universe
 * Viability: Historically proposed (Hoyle), where matter is created 
 * to maintain density as the universe expands.
 * Suppression: High. Suppressed by empirical evidence (CMB radiation) 
 * favoring the Big Bang model and irreversible entropy.
 * * CONCLUSION:
 * Since physical evidence suppresses the Steady-State alternative, Entropy 
 * remains a Mountain for the universe, but for life itself, it functions 
 * as a Noose by suppressing the possibility of eternal order.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_thermodynamics_entropy].
 * 2. Multi-perspective: ?- multi_index_report(thermodynamics_entropy).
 * 3. Run tests: ?- run_tests(thermodynamics_entropy_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
