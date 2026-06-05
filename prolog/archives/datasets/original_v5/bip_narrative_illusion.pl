% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The Dream Within The Dream"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

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
 * * constraint_id: bip_narrative_illusion
 * human_readable: The Black Iron Prison (BIP) and Sensory Optimization
 * domain: philosophical/social/religious
 * temporal_scope: Contemporary / Perennial
 * spatial_scope: Global / Existential
 * * SUMMARY:
 * This constraint defines the world as a "Black Iron Prison" (BIP)—a "puny cell" created by malevolent elites and 
 * corporations that keep humans "slaves to Empire". It operates through a "bombardment of sensory 
 * stimulus" that forces individuals to cultivate an "analytical understanding" of the world, optimizing 
 * behaviors to "get things done" while suppressing the "good heart". 
 * * KEY AGENTS:
 * - The Worldly Person (Subject): Viewed as a "slave to Empire," trapped in sense experience and judgment.
 * - The Corporate Elite/Shareholder: The "covert coven" whose interests are served by the "college of corporations".
 * - The Spiritual Practitioner: One who uses experience as a "mirror" or "telescope" to transcend the illusion.
 * * NARRATIVE ARC:
 * Modern life creates a tension where individuals are unceasingly bombarded by stimulus, forcing them into a state 
 * of "doing and thinking". This analytical standard becomes a prison that chains humans to "lesser versions" 
 * of themselves. The only escape is to "tear through the veil of Maya" and transcend the narrative 
 * fictions that create the world.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bip_narrative_illusion_interval, 0, 10).
narrative_ontology:constraint_claim([bip_narrative_illusion], [ontological_suppression]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.85). The system extracts the "Being" and "Heart" of the individual to serve the "interests 
% of business leaders and shareholders" through the standard of "doing and thinking".
domain_priors:base_extractiveness(bip_narrative_illusion, 0.85).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High (0.75). The system suppresses "warmth" (heart) and spiritual redemption (PTG) in favor of 
% "competence" (mind) and "analytical understanding".
domain_priors:suppression_score(bip_narrative_illusion, 0.75).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.85).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.75).

% Enforcement requirements
% Requires active enforcement (Corporate/Imperial elites) and natural emergence (Sensory bombardment).
domain_priors:requires_active_enforcement(bip_narrative_illusion).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(bip_narrative_illusion, business_leaders_and_shareholders). %
constraint_victim(bip_narrative_illusion, the_good_heart). %
constraint_victim(bip_narrative_illusion, lived_experience). % Reduced to a "sliver of a sliver"

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE WORLDLY PERSON (SUBORDINATE) - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (A "slave to Empire" unmoored from truth)
   WHEN: biographical (A life from "birth to death" resembling a dream)
   WHERE: trapped (Bounded by the "puny cell" of sense experience)
   SCOPE: local (Immediate sensory stimulus and judgment)
   
   WHY THIS CLASSIFICATION:
   For the worldly subject, the BIP is a "Snare." The standard of "doing and thinking" tightens around their 
   identity, extracting their "Being" and leaving them "chained to lesser versions" of themselves.
   
   NARRATIVE EVIDENCE:
   "modern life creates a tension... sensory stimulus that can take a long time for us to get free... 
   our standard is one of doing and thinking".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    bip_narrative_illusion,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(bip_narrative_illusion, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE ELITE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power within the "college of corporations")
   WHEN: historical (Systemic governance of money and elite interests)
   WHERE: mobile (Shaping the environment to optimize behavior)
   SCOPE: global (Worldwide network of linked corporations)
   
   WHY THIS CLASSIFICATION:
   For the institutional player, the "analytical understanding" of the world is a "Rope"—a functional 
   coordination mechanism used to "get things done" and serve shareholder interests.
   
   NARRATIVE EVIDENCE:
   "The world itself is a college of corporations linked together by money and serving only the interests 
   of their business leaders and shareholders".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bip_narrative_illusion,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SPIRITUAL SEEKER (THE ANALYST) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "astronomical magnitudes" of the within)
   WHEN: civilizational/civilizational (Viewing life as "one long dream")
   WHERE: analytical (Observer stance; "unmoored" from sense experience)
   SCOPE: global (Transcending the limits of the universe)
   
   WHY THIS CLASSIFICATION:
   To the practitioner, the BIP is a "Mountain"—an unchangeable, insubstantial dream that is "unreal, untrue". 
   It is the fundamental terrain of Samsara that must be recognized before it can be transcended.
   
   NARRATIVE EVIDENCE:
   "all experience... is as insubstantial as our dreams. All of it is unreal, untrue. It is an unceasing, 
   luminous, magnificent, and illusionary display".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    bip_narrative_illusion,
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

:- begin_tests(bip_narrative_illusion_tests).

test(multi_perspective_illusion) :-
    % Worldly Subject (Powerless) sees Snare
    constraint_indexing:constraint_classification(bip_narrative_illusion, snare, context(powerless, _, _, _)),
    % Corporate Elite (Institutional) sees Rope
    constraint_indexing:constraint_classification(bip_narrative_illusion, rope, context(institutional, _, _, _)),
    % Spiritual Seeker (Analytical) sees Mountain
    constraint_indexing:constraint_classification(bip_narrative_illusion, mountain, context(analytical, _, _, _)).

test(power_extractiveness_bip) :-
    % The powerless victim experiences the full 0.85 extraction of heart and soul.
    ContextPowerless = context(powerless, biographical, trapped, local),
    ContextInstitutional = context(institutional, historical, mobile, global),
    constraint_indexing:extractiveness_for_agent(bip_narrative_illusion, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(bip_narrative_illusion, ContextInstitutional, Score2),
    Score1 > Score2.

test(time_immutability_dream) :-
    % In the immediate biographical timeframe, the dream feels real (Snare).
    % Over a civilizational analytical horizon, it is seen as unchangeable unreality (Mountain).
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.85):
 * Reasoning: The text identifies the BIP as a state of slavery to Empire and money. 
 * The system extracts the "Being" and "Heart" of individuals, turning them into fictions.
 * * 2. SUPPRESSION SCORE (0.75):
 * Reasoning: The system actively suppresses the "heart" in favor of "mind" (competence). 
 * It replaces "lived experience" with narrative fiction and sensory bombardment.
 * * 3. PERSPECTIVE SELECTION:
 * Contrast between the "Worldly Person" (Snare), the "Corporate Elite" (Rope), and the 
 * "Spiritual Seeker" (Mountain) highlights the ontological trap of the BIP.
 * * 4. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The high extraction (0.85) for the subject is justified by the 
 * institutional "Rope" of optimization used by elites to govern the "college of corporations".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bip_extraction_intent,
    "Is the sensory bombardment a byproduct of technological growth (Mountain) or an intentional strategy 
     by the 'covert coven' (Snare)?",
    resolution_mechanism("Audit of elite resource allocation toward behavior optimization vs. human well-being"),
    impact("If necessity: Evolutionary Mountain. If choice: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    self_transcendence_efficacy,
    "Can consciousness truly 'tear through the veil' (Rope) or is the 'red pill' just another dream within 
     a dream (Mountain)?",
    resolution_mechanism("Longitudinal tracking of practitioners claiming 'nirvana' vs. their sensory constraints"),
    impact("If possible: Transcendence is a Rope. If impossible: The BIP is a terminal Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Palm Tree Garden (PTG)
 * Viability: The "spiritual redemption" mentioned as the binary opposite of the BIP.
 * Suppression: Actively suppressed by sensory stimulus and the "college of corporations".
 * Evidence: "The world is a prison from which we need to free ourselves".
 * * ALTERNATIVE 2: The "Good Heart" (Warmth)
 * Viability: Prioritizing "warmth" over "competence" as a metric for human judging.
 * Suppression: Shunted by the environment's demand for "doing and thinking".
 * * CONCLUSION:
 * The existence of the PTG and the "Good Heart" makes the current BIP a "Snare." Because these 
 * alternatives are visible but actively shunted by the "analytical understanding," the 
 * system remains predatory.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_bip_narrative_illusion].
% Multi-perspective: ?- multi_index_report(bip_narrative_illusion).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
