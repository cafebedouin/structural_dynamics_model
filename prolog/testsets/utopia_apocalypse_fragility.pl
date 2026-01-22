% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Apocalypse is the Suburb of Utopia"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_utopia_apocalypse_fragility, []).

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
 * * constraint_id: utopia_apocalypse_fragility
 * human_readable: The Utopia-Apocalypse Cliff-Edge
 * domain: social/political/philosophical
 * temporal_scope: Ontological/Continuous
 * spatial_scope: Global/Existential
 * * SUMMARY:
 * This constraint describes the inherent fragility of social order (Utopia), which rests on 
 * a "cliff-edge" and can transition into Apocalypse through slight shifts in structure, 
 * personnel, or culture. It posits that life is a stochastic migration 
 * from youthful promise to inescapable limits and transformation.
 * * KEY AGENTS:
 * - The Utopian Strategist: One who uses coalition-building and science to solve problems.
 * - The Victim of Calamity: The individual pushed from safety into horror by systemic shifts.
 * - The Existential Realist: The observer who recognizes the secondary nature of truth to stories.
 * * NARRATIVE ARC:
 * Social safety is presented as a "land of the possible" that is inherently impermanent 
 *. The constraint operates as a "fell wind" that transforms a stable 
 * environment into a catastrophic confrontation with fear, where truth is objectified 
 * and common sense is absent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(utopia_apocalypse_fragility_interval, 0, 10).
narrative_ontology:constraint_claim([utopia_apocalypse_fragility], [stochastic_instability]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Apocalypse extracts safety, honorable status, and truth, replacing 
% them with "calamity," "horror," and "alienation".
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.7).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Truth is actively suppressed by "stories and opinion," "objectification," 
% and "prejudices," making it "secondary".
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.6).

% Enforcement requirements:
% The shift "happens as we migrate" and comes like a "fell wind," suggesting it 
% emerges naturally from stochastic shifts.
domain_priors:emerges_naturally(utopia_apocalypse_fragility).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, 0.7).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
% Beneficiary: The "New Order" built after the jungle (Utopia) tears itself down.
constraint_beneficiary(utopia_apocalypse_fragility, systemic_transformation).
% Victim: The "little people" who suffer at the hand of calamity.
constraint_victim(utopia_apocalypse_fragility, individual_human_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UTOPIAN STRATEGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional power level; rule-shapers using coalition and science.
   WHEN: generational time horizon; solving long-term problems.
   WHERE: mobile exit options; choosing the "land of the possible."
   SCOPE: global spatial scope.
   
   WHY THIS CLASSIFICATION:
   For those in a stable society, the cliff-edge is a "Rope"—a tool to be 
   managed through determination and science to maintain the "greener grass" 
   of safety.
   
   NARRATIVE EVIDENCE:
   "Utopia is a place with kind and reasonable people using coalition-building, 
   science and determination to solve their problems".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    utopia_apocalypse_fragility,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE VICTIM OF CALAMITY - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless; one defined by physical and historical limits.
   WHEN: immediate time horizon; the moment of shift to calamity.
   WHERE: trapped exit options; "No one escapes transformation."
   SCOPE: local spatial scope.
   
   WHY THIS CLASSIFICATION:
   For the individual facing the "fell wind," the fragility is a "Noose." 
   They are confronted by fear, destitution, and defeat in an unsympathetic 
   environment where their strength is insufficient.
   
   NARRATIVE EVIDENCE:
   "Our destiny, in part, is to confront what we fear. Alone, insane, 
   destitute and defeated".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    utopia_apocalypse_fragility,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXISTENTIAL REALIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical power level; observer of "glacial shifts."
   WHEN: civilizational time horizon; life as "historical moment."
   WHERE: analytical exit options; observer stance.
   SCOPE: global spatial scope.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the precarious nature of existence is a "Mountain." 
   Impermanence and the secondary nature of truth are unchangeable laws 
   of the "stochastic" universe.
   
   NARRATIVE EVIDENCE:
   "The land of the possible has many paths, and we can know only one. 
   Everything’s stochastic and impermanent".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    utopia_apocalypse_fragility,
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

:- begin_tests(utopia_apocalypse_fragility_tests).

test(multi_perspective_fragility) :-
    % Institutional sees Rope
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope, context(institutional, generational, mobile, global)),
    % Powerless sees Noose
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, noose, context(individual_powerless, immediate, trapped, local)),
    % Analytical sees Mountain
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, mountain, context(analytical, civilizational, analytical, global)).

test(power_extractiveness_fragility) :-
    % The powerless experience the full extraction of "calamity" and "defeat."
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, generational, mobile, global),
    constraint_indexing:extractiveness_for_agent(utopia_apocalypse_fragility, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(utopia_apocalypse_fragility, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_fragility) :-
    % Glacial shifts over civilizational horizons appear as Mountain.
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS MADE BY MODEL:
 * * 1. BASE EXTRACTIVENESS (0.7):
 * Reasoning: Chose high because the transition described is not a mere 
 * inconvenience but a total "catastrophe" that strips agents of safety, 
 * sanity, and truth. This triggers the Mandatrophy 
 * status, necessitating an indexed resolution.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Institutional (Strategist) to show the "Rope" of maintenance, 
 * Individual Powerless (Victim) to show the "Noose" of collapse, and 
 * Analytical (Realist) to show the "Mountain" of ontological fragility.
 * * 3. CLASSIFICATION RATIONALE:
 * Individual Powerless → Noose: Trapped in the "stink of fear" with no 
 * exit from transformation.
 * Analytical → Mountain: Viewing the "glacial shifts" of history where 
 * transformation is inevitable for all.
 * * 4. AMBIGUITIES IN SOURCE MATERIAL:
 * - The efficacy of "belief" (smaller than a mustard seed) to change the 
 * system remains an irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    utopia_apocalypse_fragility_extraction_intent,
    "Is the shift from safety to calamity a functional necessity of stochastic systems or a predatory result of human failure?",
    resolution_mechanism("Audit of personnel and cultural shifts preceding the fell wind's arrival"),
    impact("If necessity: Mountain. If predatory choice: Noose/Mandatrophy."),
    confidence_without_resolution(medium)
).

% Additional Omega for narrative ambiguity:
omega_variable(
    mustard_seed_potency,
    "Can the strength of individual belief influence the underlying stochastic structure of the cliff-edge?",
    resolution_mechanism("Long-term tracking of social outcomes based on belief-strength metrics in fragile systems"),
    impact("If potent: The cliff-edge is a Rope. If impotent: It is a fixed Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Static Utopia
 * Viability: Presented as the "hope" for "greener grass".
 * Suppression: Rejected because everything is "stochastic and impermanent".
 * * ALTERNATIVE 2: Scientific Common Sense
 * Viability: A tool for solving problems in Utopia.
 * Suppression: Suppressed in the Apocalypse by "gullibility" and "alienation".
 * * CONCLUSION:
 * The existence of stochastic shifts turns the "Rope" of science into a "Noose" 
 * for those caught in the "horror" when the underlying structure changes.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [utopia_apocalypse_fragility].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
