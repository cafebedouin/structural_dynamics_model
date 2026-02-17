% ============================================================================
% CONSTRAINT STORY: amish_technological_renunciation
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: "What Does It Mean To Be Amish?" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_amish_technological_renunciation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: amish_technological_renunciation
 * human_readable: The Television Test (Amish Renunciation)
 * domain: social/technological/religious
 * temporal_scope: Contemporary / Perennial
 * spatial_scope: Regional (Amish Country) to Global (Mass Culture)
 * * SUMMARY:
 * This constraint defines "being Amish" as the collective volitional discipline to renounce 
 * technology recognized as harmful to community integrity. It highlights the 
 * "Amish Test": the gap between intellectually recognizing a technology's negative impact 
 * and the actual willingness to sacrifice its convenience.
 * * KEY AGENTS:
 * - Amos (The Amish Man): An agent embodying the coordinate discipline of the Ordnung.
 * - The Tourist (Mass Culture Subject): An agent who recognizes technological harm but lacks the agency to exit.
 * - The Amish Church (Institutional): The rule-making body that maintains the community's technological boundaries.
 * - The Analytical Observer: An observer of the "stochastic" nature of human habit and renunciation.
 * * NARRATIVE ARC:
 * When tourists are asked if television is harmful, they agree; when asked if they would 
 * give it up, none raise their hands. For the tourist, technology is a 
 * "Snare" of dependency. For the Amish, the same renunciation is a "Rope"—the 
 * primary coordination mechanism for their distinct identity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(amish_technological_renunciation, 0, 10).
narrative_ontology:constraint_claim(amish_technological_renunciation, snare).
narrative_ontology:human_readable(amish_technological_renunciation, "The Television Test (Amish Renunciation)").

% Base Properties
% Rationale: High extraction (0.8). Mass technology siphons volitional agency 
% from tourists.
domain_priors:base_extractiveness(amish_technological_renunciation, 0.80).
domain_priors:suppression_score(amish_technological_renunciation, 0.75).
domain_priors:theater_ratio(amish_technological_renunciation, 0.12). % Functional focus

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(amish_technological_renunciation, extractiveness, 0.8).
narrative_ontology:constraint_metric(amish_technological_renunciation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(amish_technological_renunciation, theater_ratio, 0.12).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(amish_technological_renunciation).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, community_integrity).
narrative_ontology:constraint_victim(amish_technological_renunciation, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */


/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TOURIST - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subjects "trapped" by technological habit.
   WHEN: immediate - The "present moment" of the Television Test.
   WHERE: trapped - Intellectually aware of harm but unable to exit.
   SCOPE: global - Participants in the mass culture.
   
   WHY THIS CLASSIFICATION:
   For the tourist, technology is a Snare. They acknowledge its "negative impact" 
   but their agency has been liquidated; they are "trapped" by convenience 
   and cannot raise their hands to quit, even when harm is identified.
   
   NARRATIVE EVIDENCE:
   "How many of you think that television... has a negative impact? ... no hands 
   were raised [to give it up]".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AMISH CHURCH (ORDNUNG) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - The collective rule-shaping body of the community.
   WHEN: generational - Preserving identity through successive lineages.
   WHERE: mobile - Actively choosing to exist outside mass technological norms.
   SCOPE: regional - The specific geography of Amish life.
   
   WHY THIS CLASSIFICATION:
   For the institutional community, renunciation is a Rope. It is the primary 
   coordination mechanism that allows the group to be "more than the individuals," 
   using the "willingness to give up" as a tool for social cohesion.
   
   NARRATIVE EVIDENCE:
   "That is what it means to be Amish" — defining identity through the tool 
   of collective sacrifice.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the "Television Test" dynamics.
   WHEN: civilizational - Viewing the perennial human struggle with habit.
   WHERE: analytical - External observer stance.
   SCOPE: global - Universal human behavior.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the "Television Test" reveals a Mountain. The gap between 
   knowing harm and acting to stop it appears as a fixed law of human inertia. 
   Amish renunciation is seen as a rare biological/social exception to a 
   natural law of consumption.
   
   NARRATIVE EVIDENCE:
   The silent group of tourists facing the unyielding fact of their own 
   dependency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(amish_technological_renunciation_tests).

test(multi_perspective_variance) :-
    % Tourist sees Snare, Church sees Rope, Analyst sees Mountain
    constraint_indexing:constraint_classification(amish_technological_renunciation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amish_technological_renunciation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(amish_technological_renunciation, mountain, context(agent_power(analytical), _, _, _)).

test(power_extractiveness_scaling) :-
    % The powerless tourist experiences higher "habit-extraction" than the disciplined community member.
    ContextTourist = context(powerless, immediate, trapped, global),
    ContextAmish = context(institutional, generational, mobile, regional),
    constraint_indexing:extractiveness_for_agent(amish_technological_renunciation, ContextTourist, E1),
    constraint_indexing:extractiveness_for_agent(amish_technological_renunciation, ContextAmish, E2),
    E1 > E2.

test(habit_immutability_threshold) :-
    % Demonstrates that in the immediate horizon, technological dependency appears as a Mountain.
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(amish_technological_renunciation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-23
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: I chose high extraction because the text emphasizes that tourists 
 * are "liquidated" of their agency. They *believe* the tech is harmful but 
 * cannot stop, indicating a high extraction of volitional life.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the "trapped" Tourist (Snare) with the "institutional" Ordnung 
 * (Rope) to highlight the perspectival gap between habit and discipline.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The high extraction of convenience is 
 * resolved as a functional tool (Rope) for preservation within the Amish index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    amish_extraction_intent,
    "Is the renunciation of technology a functional necessity for social survival or a predatory restriction of individual potential?",
    resolution_mechanism("Audit of mental health and social cohesion metrics in Amish communities vs. high-tech mass culture."),
    impact("If necessity: Mountain (natural law). If predatory: Snare (Mandatrophy)."),
    confidence_without_resolution(medium)
).

omega_variable(
    technological_agency_threshold,
    "Is the 'willingness to give up' a biological limit of the human brain (Mountain) or a learnable cultural skill (Rope)?",
    resolution_mechanism("Longitudinal tracking of 'digital detox' success rates in individuals moving to intentional communities."),
    impact("If limit: Mass culture is a terminal Snare. If skill: Amish life is a teachable Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Better Programming" (Reformism)
 * Viability: The belief that technology can be fixed through content reform.
 * Suppression: Explicitly rejected in the source: "the problems of society 
 * will not be solved by advocating for better programming".
 * * ALTERNATIVE 2: Pure Theological Belief
 * Viability: Religious identity without lifestyle change.
 * Suppression: Rejected by Amos as failing to capture "what it means to be Amish".
 * * CONCLUSION:
 * The active suppression of "low-cost" alternatives (Reform/Pure Belief) 
 * shifts the Amish renunciation from a mere choice to a definitive Rope for 
 * community survival.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Remains low as the Amish renunciation remains a functional 
% discipline rather than a performative one.
narrative_ontology:measurement(amish_tr_t0, amish_technological_renunciation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(amish_tr_t5, amish_technological_renunciation, theater_ratio, 5, 0.11).
narrative_ontology:measurement(amish_tr_t10, amish_technological_renunciation, theater_ratio, 10, 0.12).

% Extraction: Tracking the rising "volitional cost" of renunciation as 
% mass-culture technologies become more pervasive and harder to exit.
narrative_ontology:measurement(amish_ex_t0, amish_technological_renunciation, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(amish_ex_t5, amish_technological_renunciation, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(amish_ex_t10, amish_technological_renunciation, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [amish_technological_renunciation].
% Report: ?- constraint_indexing:multi_index_report(amish_technological_renunciation).
% Tests: ?- run_tests(amish_technological_renunciation_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
