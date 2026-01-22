% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The Extraordinary Intruding on the Ordinary" by cafebedouin.org
% ============================================================================

:- module(extraordinary_narrative_shift, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor: IDs for Python script extraction
narrative_ontology:interval(extraordinary_narrative_shift, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: extraordinary_narrative_shift
 * human_readable: The Story of the Extraordinary
 * domain: social/psychological
 * temporal_scope: Perennial / Historical
 * spatial_scope: Global / Cognitive
 * * SUMMARY:
 * The distinction between "extraordinary" and "ordinary" is not a physical property but a function of frequency, 
 * quantity, and the narrative framing we apply to experience. The constraint identifies that 
 * repetitive, commonplace existence truly defines our experience, and "extraordinary" moments are simply 
 * ordinary ones with a different story attached.
 * * KEY AGENTS:
 * - The Sherpa: Repetitive laborer for whom the "extraordinary" summit is a daily ordinary.
 * - Bernard Moitessier: A sailor who abandoned an "extraordinary" race to live the "ordinary" journey.
 * - The Historical Judge: An observer whose context (e.g., 200 years ago vs. 200 years hence) determines 
 * the extraordinariness of sailors vs. astronauts.
 * * NARRATIVE ARC:
 * The constraint functions as a psychological filter. Agents either use stories as "Ropes" to 
 * imbue life with uniqueness or find themselves trapped in a "Noose" of repetitive labor that 
 * others mislabel as extraordinary. True agency is found in identifying with the ordinary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Moderate (0.4)
% Rationale: Narrative framing "extracts" the raw, lived presence of the moment to serve a "branding" 
% or "unique" story. It also extracts labor (Sherpa) for the sake of another's "extraordinary" story.
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.4).

% Suppression: Moderate (0.4)
% Rationale: The "extraordinary" narrative suppresses the value of the "ordinary" repetition, 
% making the bulk of life feel like it doesn't "matter" unless it fits a specific narrative.
domain_priors:suppression_score(extraordinary_narrative_shift, 0.4).

% Enforcement: Emerges naturally from human storytelling and cognitive frequency bias.
domain_priors:emerges_naturally(extraordinary_narrative_shift).

% ASYMMETRY METRIC
% Beneficiaries: The Storyteller / The Unique Subject.
% Victims: The Lived Experience / The Commonplace Repetition.
constraint_beneficiary(extraordinary_narrative_shift, the_storyteller).
constraint_victim(extraordinary_narrative_shift, lived_experience).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.4).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.4).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SHERPA (Repetitive Labor) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to the frequency and volume of work)
   WHEN: immediate (Daily repetitive task)
   WHERE: trapped (Bound by the physical necessity of the mountain)
   SCOPE: local (The physical pinnacle)
   
   WHY THIS CLASSIFICATION:
   For the Sherpa, the "extraordinary" summit of Everest is a "Noose." The narrative of the 
   tourist's "one minute at the pinnacle" extracts the Sherpa's daily labor while 
   rendering their experience invisible to the "Extraordinary" story.
   
   NARRATIVE EVIDENCE:
   "If you were a Sherpa climbing Mt. Everest every day... What would the value of 
   summiting Everest be to you?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Classification logic: Powerless + Immediate + Trapped = Noose in an extractive context.
    domain_priors:base_extractiveness(extraordinary_narrative_shift, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: BERNARD MOITESSIER (The Solo Sailor) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has agency to abandon the race)
   WHEN: biographical (Changing his personal life story mid-journey)
   WHERE: mobile (Abandons the competitive race for the experience)
   SCOPE: global (Sailing around the world)
   
   WHY THIS CLASSIFICATION:
   For Moitessier, the "Ordinary" repetition of sailing becomes a "Rope." He rejects the 
   extractive race narrative to coordinate his existence with the "journey" rather 
   than the "pinnacle," making his experience a functional tool for selfhood.
   
   NARRATIVE EVIDENCE:
   "he decided that the race was less important than the journey of the race... 
   abandoned the race".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HISTORICAL ANALYST (Astronauts vs Sailors) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of civilizational shifts)
   WHEN: historical (200-year horizons)
   WHERE: analytical (Observer stance)
   SCOPE: global (The species-level experience)
   
   WHY THIS CLASSIFICATION:
   To the analyst, these narratives are "Mountains"—fixed social facts of the 
   historical moment. The extraordinariness of a sailor vs. an astronaut is an 
   unchangeable law dictated by the availability and frequency of the role at 
   that point in time.
   
   NARRATIVE EVIDENCE:
   "The answer depends on the perspective of the person judging, usually from within 
   the context of the historical moment".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    % Logic: Long horizon + Observer = Mountain.
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(multi_perspective_extraordinary) :-
    % Test that different perspectives yield different classifications
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type2, context(individual_moderate, biographical, mobile, global)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type3, context(analytical, historical, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_extraordinary) :-
    % Powerless experience more extraction ( Sherpa labor)
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(individual_moderate, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(extraordinary_narrative_shift, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(extraordinary_narrative_shift, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_narrative) :-
    % Long horizon (historical) treats the shift as a fact/Mountain
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(extraordinary_narrative_shift_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.4):
 * Reasoning: The narrative centers on how stories extract presence from lived experience. 
 * The Sherpa example illustrates how one's labor is extracted to serve another's 
 * peak-experience narrative.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Sherpa (Noose), Moitessier (Rope), and Analyst (Mountain) to demonstrate the transition 
 * from labor to agency to historical law.
 * * 3. CLASSIFICATION RATIONALE:
 * Sherpa -> Noose: They are "trapped" in the repetition of a task they cannot frame as unique.
 * Moitessier -> Rope: He uses the "ordinary" repetition as a tool to bypass a competitive "Noose".
 * Analyst -> Mountain: Sees the "shift" in roles (astronaut/sailor) as unchangeable terrain of history.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory extraction intent omega:
omega_variable(
    extraordinary_narrative_shift_extraction_intent,
    "Is the narrative of 'extraordinary' moments a byproduct of human storytelling (Mountain) or a predatory choice to drive consumption and competition (Noose)?",
    resolution_mechanism("Audit of peak-experience marketing spend vs. individual reported life-satisfaction metrics"),
    impact("If necessity: Evolutionary Mountain. If predatory choice: Mandatrophy Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    repetition_threshold,
    "What is the specific 'frequency, quantity and volume' threshold where an extraordinary experience permanently collapses into the ordinary?",
    resolution_mechanism("Neurological monitoring of dopamine response during repeated high-novelty stimuli"),
    impact("If low threshold: Extraordinary is a fragile Rope. If high threshold: It is a durable Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Identifing with the Ordinary
 * Viability: Moitessier proves this is a real alternative to the race.
 * Suppression: Suppressed by "everyone wanting to believe they are unique".
 * Evidence: "To identify with the ordinary... truly defines our experience".
 * * ALTERNATIVE 2: Changing the Story
 * Viability: Deciding to tell a different narrative to highlight unique status.
 * Suppression: Often suppressed by historical context (e.g. inability to be an astronaut 200 years ago).
 * * CONCLUSION:
 * The existence of Alternative 1 shifts the "Extraordinary Story" from a necessary Rope into a 
 * potential Noose if the agent ignores their own "ordinary" lived experience for a fiction.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [extraordinary_narrative_shift].
 * 2. Multi-perspective: ?- multi_index_report(extraordinary_narrative_shift).
 * 3. Run tests: ?- run_tests(extraordinary_narrative_shift_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
