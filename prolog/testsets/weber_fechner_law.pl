% ============================================================================
% CONSTRAINT STORY: weber_fechner_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Psychophysics / Weber (1834) / Fechner (1860)
% ============================================================================

:- module(constraint_weber_fechner_law, []).

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
 * * constraint_id: weber_fechner_law
 * human_readable: Weber-Fechner Law (Logarithmic Perception)
 * domain: technological/social
 * temporal_scope: Permanent (Biological Sensory Architecture)
 * spatial_scope: Global (Human Perception)
 * * SUMMARY:
 * The Weber-Fechner law describes the relationship between the physical intensity 
 * of a stimulus and its perceived intensity. It states that the perceived change 
 * in a stimulus is proportional to the relative change, not the absolute change, 
 * resulting in a logarithmic rather than linear sensory experience.
 * * KEY AGENTS:
 * - The Psychophysicist (Analytical): Observer mapping the logarithmic 
 * curve of human sensation and the "Just Noticeable Difference" (JND).
 * - The Marketing Executive (Institutional): Uses the law to coordinate pricing 
 * and branding (e.g., hiding price increases or weight reductions below the JND).
 * - The Sensory Subject (Individual Powerless): Subject to the biological 
 * inability to perceive absolute changes at high intensities.
 * * NARRATIVE ARC:
 * The law functions as a "Mountain" of biological reality—an evolved efficiency 
 * in sensory hardware. For the designer, it is a "Rope" for coordination (creating 
 * volume knobs or lighting systems that feel "natural"). However, for the consumer, 
 * it becomes a "Noose" when used by institutions to extract value by reducing 
 * product quality or quantity in increments that remain psychologically invisible.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural anchor
narrative_ontology:interval(weber_fechner_interval, 0, 10).
narrative_ontology:constraint_claim(weber_fechner_law, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The law allows for the extraction of "value" 
% by hiding small negative changes from the consumer while maximizing 
% the perception of small positive changes in marketing.
domain_priors:base_extractiveness(weber_fechner_law, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It suppresses "Linear Realism." Humans find it 
% extremely difficult to perceive physical reality linearly, making the 
% objective "Truth" of a stimulus invisible compared to the perceived sensation.
domain_priors:suppression_score(weber_fechner_law, 0.5).

% Enforcement requirements
% Emerges naturally from neural firing rates and sensory compression.
domain_priors:emerges_naturally(weber_fechner_law).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(weber_fechner_law, extractiveness, 0.4).
narrative_ontology:constraint_metric(weber_fechner_law, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(weber_fechner_law, [advertising_agencies, industrial_designers]).
constraint_victim(weber_fechner_law, [uninformed_consumers, classical_utility_models]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEUROSCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the biological sensory substrate.
   WHEN: civilizational - Viewing the evolution of the mammalian nervous system.
   WHERE: trapped - Sensory compression is a fixed feature of neural hardware.
   SCOPE: global - Universal to the human species.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the law is a Mountain. It is an unchangeable feature of 
   the brain's data compression. No amount of training can allow a human to 
   hear 100dB as exactly twice as loud as 50dB; the logarithmic response 
   is a fixed peak in our biological topography.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
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
   PERSPECTIVE 2: THE UI/UX DESIGNER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design interfaces and sensory experiences.
   WHEN: biographical - Creating products that last for decades.
   WHERE: arbitrage - Can choose to use logarithmic or linear scales to influence behavior.
   SCOPE: national - Digital and physical product ecosystems.
   
   WHY THIS CLASSIFICATION:
   For the designer, the law is a Rope. It is a vital coordination mechanism. 
   By aligning a volume slider or a brightness control with the logarithmic 
   nature of human perception, they "pull" the user into a state of comfort 
   and intuitive control, using the constraint to create a bridge of usability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(weber_fechner_law, E),
    E > 0.2, % Managed as a tool for usability
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUPERMARKET SHOPPER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to "shrinkflation" and price masking.
   WHEN: immediate - Today's grocery bill and perception of "value."
   WHERE: constrained - High cost to verify absolute weights/volumes of every item.
   SCOPE: local - Immediate household purchasing.
   
   WHY THIS CLASSIFICATION:
   For the consumer, the law is often a Noose. Corporations exploit the 
   "Just Noticeable Difference" to extract value—reducing the contents 
   of a bag of chips by 5% or raising a price by a small percentage that 
   falls below the sensory threshold. The biological "fuzziness" of 
   perception becomes a trap that strangles their purchasing power invisibly.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(weber_fechner_law, E),
    E > 0.3, % Extraction of value through sensory blindness
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(weber_fechner_law_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(weber_fechner_law, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(weber_fechner_law, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(weber_fechner_law, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_perception) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(weber_fechner_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(weber_fechner_law, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(weber_fechner_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): It extracts the ability to maintain linear 
 * economic utility. The law allows for "stealth extraction" of value 
 * in consumer markets.
 * 2. PERSPECTIVE SELECTION: Chose the Scientist (Nature), the Designer 
 * (Utility), and the Shopper (Extraction) to highlight the shift from 
 * biological fact to economic exploitation.
 * 3. NOOSE LOGIC: Specifically focuses on "Shrinkflation" as a manifestation 
 * of the Noose where biological limits are used as a predatory trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precision_training_limit,
    "Can humans be trained to bypass Weber-Fechner 'blindness' through 
    comparative analytical tools (Rope), or is the logarithmic floor 
    absolute (Mountain)?",
    resolution_mechanism("Long-term sensory training studies using 
    real-time digital feedback to linearize perception"),
    impact("If Rope: Consumers can untie the Noose of shrinkflation. 
    If Mountain: We require institutional regulation (External Rope) to 
    protect against the biological trap."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Stevens' Power Law
 * Viability: High. A more generalized version of the relationship that 
 * accounts for different sensory modalities (e.g., electric shock is 
 * super-linear).
 * Suppression: Low. It is a scientific refinement, not a competitor.
 * * CONCLUSION:
 * Since there is no "Linear Sensation" alternative for humans, the 
 * constraint remains a Mountain of nature. The "Noose" arises purely 
 * from the social exploitation of this biological peak.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_weber_fechner_law].
 * 2. Multi-perspective: ?- multi_index_report(weber_fechner_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
