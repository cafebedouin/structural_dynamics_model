% ============================================================================
% CONSTRAINT STORY: the_churn_systemic_upheaval
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: The Expanse (TV Series), S1E7 Dialogue
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(the_churn_systemic_upheaval, []).

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
 * * constraint_id: the_churn_systemic_upheaval
 * human_readable: The Churn (Systemic Collapse and Rebirth)
 * domain: political/social/economic
 * temporal_scope: Periods of transition ("when the rules of the game change")
 * spatial_scope: Systemic/Global ("When the jungle tears itself down")
 * * SUMMARY:
 * "The Churn" represents a period of systemic instability where established social and political rules 
 * dissolve and reconfigure. It is characterized by high mortality for "little people" and 
 * survivors alike, rendering individual effort or identity "meaningless" in the face of macro-scale 
 * structural shifts.
 * * KEY AGENTS:
 * - The Veteran Survivor (Amos): Accepts the churn as a natural, inevitable law.
 * - The Disposable Subject (Spy): A "loose end" liquidated by the shifting rules.
 * - The Systemic Architect (Baltimore Boss): Defines the "game" and identifies the transition.
 * * NARRATIVE ARC:
 * The churn functions as a "game" of survival where the environment ("jungle") aggressively 
 * restructures itself. In this state, agents are "caught up" in a process where life or death 
 * is a statistical byproduct of the system's re-building phase.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(the_churn_systemic_upheaval, 0, 10).
narrative_ontology:constraint_claim([the_churn_systemic_upheaval], [systemic_collapse]).

% Base extractiveness score (0.9 = High)
% Rationale: The churn is maximally extractive; it consumes the lives of those "caught up" in it, 
% often treating death as a "loose end" or a trivial outcome of structural change.
domain_priors:base_extractiveness(the_churn_systemic_upheaval, 0.9).

% Suppression score (0.8 = High)
% Rationale: During the churn, the previous "rules of the game" are torn down, suppressing 
% any alternative stable reality or moral agency.
domain_priors:suppression_score(the_churn_systemic_upheaval, 0.8).

% Enforcement: Emerges naturally
% Rationale: It is described as a force of nature ("Like water's wet. Sky's up").
domain_priors:emerges_naturally(the_churn_systemic_upheaval).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, extractiveness, 0.9).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(the_churn_systemic_upheaval, the_new_system). % "builds itself into somethin' new"
constraint_victim(the_churn_systemic_upheaval, loose_ends). % "Guys like you and me, we end up dead"

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE VETERAN SURVIVOR (Amos) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A skilled agent who "happens to live through it")
   WHEN: civilizational (Views the cycle of the "jungle" as a constant)
   WHERE: analytical (Disconnected from personal meaning/emotion)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the survivor, the churn is a "Mountain." It is an unchangeable natural law comparable 
   to physics ("Like water's wet. Sky's up"). Meaning is irrelevant; only existence remains.
   
   NARRATIVE EVIDENCE:
   "It has nothing to do with me. We’re just caught up in the churn, is all".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    the_churn_systemic_upheaval,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DISPOSABLE SUBJECT (The Spy) - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "loose end" with no agency)
   WHEN: immediate (Facing death "either way this plays out")
   WHERE: trapped (No escape from the liquidation)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the victim, the churn is a "Snare." It is a coercive trap where their death is 
   predetermined and "doesn't really mean anything" to the system. 
   They are "caught up" in a process they cannot influence or understand.
   
   NARRATIVE EVIDENCE:
   "Either way this plays out, you’re dead... You’re a loose end. Nothing personal".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    the_churn_systemic_upheaval,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SYSTEMIC ARCHITECT (The Boss) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (The one who defines the "rules of the game")
   WHEN: historical (Managing the transition between old and new)
   WHERE: mobile (Positioned to observe and use the transition)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the institutional player, the churn is a "Rope"—a functional, albeit violent, 
   mechanism for clearing out "loose ends" and building "somethin' new". 
   It is the means by which the "game" of survival is updated.
   
   NARRATIVE EVIDENCE:
   "he called it the churn, when the rules of the game change".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    the_churn_systemic_upheaval,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(the_churn_systemic_upheaval_tests).

test(multi_perspective_upheaval) :-
    % Survivor (Moderate) sees Mountain
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, mountain, context(individual_moderate, _, _, _)),
    % Victim (Powerless) sees Snare
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare, context(individual_powerless, _, _, _)),
    % Architect (Institutional) sees Rope
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope, context(institutional, _, _, _)).

test(extraction_scaling) :-
    % Extraction is high (0.9), confirming the "liquidation" aspect.
    domain_priors:base_extractiveness(the_churn_systemic_upheaval, E),
    E > 0.7.

:- end_tests(the_churn_systemic_upheaval_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.9):
 * The churn extracts the ultimate resource—life—without meaning or compensation.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the "Survivor" (Amos) to show the transition to Mountain-view (radical acceptance), 
 * the "Victim" to show the Snare (liquidation), and the "Boss" to show the Rope (strategic utility).
 * * 3. MANDATROPHY RESOLUTION:
 * The constraint is [RESOLVED MANDATROPHY] because while it is a lethal "Snare" for 
 * individuals, it is indexed as a necessary "Mountain" or "Rope" for the system's 
 * continuation into "somethin' new".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    the_churn_meaning_resolution,
    "Does living through the churn truly 'mean nothing' (Mountain), or does it create 
     new social capital (Rope)?",
    resolution_mechanism("Longitudinal tracking of 'churn' survivors' role in the subsequent 'somethin' new' system"),
    impact("If meaning exists: The Mountain becomes a Rope. If no meaning: It remains a cold Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    the_churn_extraction_intent,
    "Is the liquidation of 'loose ends' a byproduct of structural collapse (Mountain) 
     or an intentional predatory strategy by 'Bosses' (Snare)?",
    resolution_mechanism("Audit of 'Boss' resource preservation vs. 'loose end' survival rates during the churn"),
    impact("If intent exists: Mandatrophy Snare. If byproduct: Physical Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Stable Institutional Maintenance
 * Viability: The state before "the jungle tears itself down".
 * Suppression: Explicitly rejected when "the rules of the game change".
 * * ALTERNATIVE 2: Meritocratic Survival
 * Viability: The idea that "having life all figured out" or being "talented" helps.
 * Suppression: Dismissed by Amos—survival is a matter of "happening to live through it".
 * * CONCLUSION:
 * The churn actively suppresses both Stability and Meritocracy, leaving only the 
 * raw "Mountain" of existence.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [the_churn_systemic_upheaval].
% Multi-perspective: ?- multi_index_report(the_churn_systemic_upheaval).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
