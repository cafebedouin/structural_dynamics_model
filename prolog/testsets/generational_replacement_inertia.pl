% ============================================================================
% CONSTRAINT STORY: generational_replacement_inertia
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The Primary Human Problem" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_generational_replacement_inertia, []).

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
 * * constraint_id: generational_replacement_inertia
 * human_readable: Generational Cognitive Inertia
 * domain: social/psychological
 * temporal_scope: Perennial / Generational
 * spatial_scope: Global / Civilizational
 * * SUMMARY:
 * This constraint posits that human thoughts are trapped in "well-worn pathways" that rarely change, 
 * leading to a state where societies only evolve as a function of generational replacement. 
 * It identifies the "Primary Human Problem" as the duality of being "good" but also "self-centered, 
 * lazy and stubborn," making the "breaking of chains" a biological rather than intellectual process.
 * * KEY AGENTS:
 * - The Individual (The Stuck Thinker): Caught in static mental routes where "advice is useless".
 * - The New Generation: The carrier of "new ideas" that gain currency only as older pathways fade.
 * - The Wise Observer: One who recognizes that "under stress, everyone will regress" and seeks absolution through self-forgiveness.
 * * NARRATIVE ARC:
 * Progress is constrained by the "static, unchanging" routes of the current generation's mind. 
 * While individuals may seek "nourishment" or "absolution," the broader social evolution is 
 * gated by the "Mountain" of biological replacement, where "new pathways" are embraced by new 
 * agents rather than adopted by the old.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(generational_replacement_interval, 0, 10).
narrative_ontology:constraint_claim([generational_replacement_inertia], [cognitive_lock_in]).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.75). The constraint extracts the agency and potential growth of 
% existing individuals, liquidating their capacity for change in favor of 
% "generational replacement".
domain_priors:base_extractiveness(generational_replacement_inertia, 0.75).

% Suppression score (0.0-1.0)
% Rationale: High (0.8). "Advice is useless" and "fools won't heed it," actively 
% suppressing the visibility of alternative pathways for the current generation.
domain_priors:suppression_score(generational_replacement_inertia, 0.8).

% Enforcement: Emerges naturally from human psychology ("stubborn") and biology.
domain_priors:emerges_naturally(generational_replacement_inertia).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(generational_replacement_inertia, future_generations). % Gain currency for new ideas
constraint_victim(generational_replacement_inertia, individual_growth). % Individual thoughts "rarely change"

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(generational_replacement_inertia, extractiveness, 0.75).
narrative_ontology:constraint_metric(generational_replacement_inertia, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL THINKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Slaves to "self-centered, lazy and stubborn" nature)
   WHEN: immediate (The daily cycle of "well-worn pathways")
   WHERE: trapped (Caught in routes that are "static, unchanging")
   SCOPE: local (Personal thought environment)
   
   WHY THIS CLASSIFICATION:
   For the individual, cognitive inertia is a "Noose." Their thoughts are "abbreviated versions" 
   of their being, and they are trapped in a cycle where they seek "corroboration" over advice, 
   effectively strangling their own potential for evolution.
   
   NARRATIVE EVIDENCE:
   "Our thoughts... are constantly changing. Cycling through the well-worn pathways, 
   but the routes are static, unchanging".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    generational_replacement_inertia,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(generational_replacement_inertia, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NEW GENERATION - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "create new pathways")
   WHEN: generational (The timescale of social evolution)
   WHERE: mobile (Moving into the currency of new ideas)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   For the incoming cohort, the replacement cycle is a "Rope." It is the functional 
   mechanism that "breaks the chains" of the old society, allowing for the 
   nourishment of new ideas that the previous generation could not understand.
   
   NARRATIVE EVIDENCE:
   "Societies break these chains, evolve only as a function of generational 
   replacement. New ideas gain currency as they are embraced by new generations".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    generational_replacement_inertia,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WISE ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Ancient and wise observer of "human problems")
   WHEN: civilizational (Viewing the totality of human history)
   WHERE: analytical (Observer stance; "gods of ruins")
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, the primary problem is a "Mountain"—an unchangeable natural law. 
   The duality of human nature (good/stubborn) and the futility of advice are 
   fixed terrains that must be "faced" rather than "fixed".
   
   NARRATIVE EVIDENCE:
   "Most people are good. Most people are also self-centered, lazy and stubborn... 
   The incomprehensible is invisible, nonexistent".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    generational_replacement_inertia,
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

:- begin_tests(generational_replacement_inertia_tests).

test(multi_perspective_inertia) :-
    % Stuck Individual (Powerless) sees Noose
    constraint_indexing:constraint_classification(generational_replacement_inertia, noose, context(individual_powerless, immediate, trapped, local)),
    % Future Generation (Moderate) sees Rope
    constraint_indexing:constraint_classification(generational_replacement_inertia, rope, context(individual_moderate, generational, mobile, global)),
    % Sage (Analytical) sees Mountain
    constraint_indexing:constraint_classification(generational_replacement_inertia, mountain, context(analytical, civilizational, analytical, global)).

test(power_extractiveness_inertia) :-
    % The individual thinker experiences high extraction of their potential growth.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, generational, mobile, global),
    constraint_indexing:extractiveness_for_agent(generational_replacement_inertia, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(generational_replacement_inertia, ContextModerate, E2),
    E1 > E2.

test(time_immutability_inertia) :-
    % Over immediate horizons, mental pathways are a Mountain (fact).
    % Over generational horizons, they are a Rope (changing).
    constraint_indexing:effective_immutability(generational, mobile, rope).

:- end_tests(generational_replacement_inertia_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: I chose high extraction because the text frames the inability to change 
 * as a terminal condition where "advice is useless" and growth is purely 
 * biological/generational rather than volitional. 
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the "Stuck Thinker" (Noose) who cannot change their well-worn pathways 
 * with the "Future Generation" (Rope) for whom the same mechanism is the only 
 * way to "break the chains".
 * * 3. STATUS: [RESOLVED MANDATROPHY]
 * Reasoning: High extraction for the individual is justified by the functional 
 * "Rope" of social evolution through generational replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_replacement_inertia_extraction_intent,
    "Is the inability for individual thoughts to change a biological necessity (Mountain) or an extractive psychological Noose?",
    resolution_mechanism("Audit of cognitive plasticity in 'nourishing' vs 'poisonous' environments"),
    impact("If necessity: Evolutionary Mountain. If choice: Mandatrophy Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    self_absolution_efficacy,
    "Can 'self-forgiveness' truly break the 'Noose' of corruption (Rope), or is 'surgery' on the environment the only Mountain?",
    resolution_mechanism("Comparison of internal well-being in 'corrupt' vs 'burned down' arenas"),
    impact("If self-forgiveness works: Personal Rope. If surgery required: Systemic Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Intellectual Persuasion (Advice)
 * Viability: The common hope that sharing information changes minds.
 * Suppression: Explicitly shunted as "useless" because fools won't heed it.
 * Evidence: "No one wants advice. They want corraboration".
 * * ALTERNATIVE 2: Systemic Reform
 * Viability: Fixing corrupt arenas through policy.
 * Suppression: Rejected in favor of "surgery" or "burning them to the ground".
 * * CONCLUSION:
 * The absence of these alternatives (Persuasion/Reform) makes the "Generational 
 * Replacement" the only functional Rope, while individual cognitive change 
 * remains a Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [generational_replacement_inertia].
% Multi-perspective: ?- constraint_indexing:multi_index_report(generational_replacement_inertia).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
