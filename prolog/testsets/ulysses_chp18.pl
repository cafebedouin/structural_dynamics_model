% ============================================================================
% CONSTRAINT STORY: molly_affirmation_cycle
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 18 (Penelope)
% ============================================================================

:- module(molly_affirmation_cycle, []).

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
 * * constraint_id: molly_affirmation_cycle
 * human_readable: The Rhythmic Affirmation (Molly’s Stream)
 * domain: biological/social/psychological
 * temporal_scope: June 17, 1904 (Pre-dawn)
 * spatial_scope: 7 Eccles Street, Dublin (The Bed)
 * * SUMMARY:
 * This constraint represents the fluid, unpunctuated consciousness of Molly Bloom 
 * as she reconciles her day and her marriage. It functions as a tool for 
 * emotional coordination, moving from the "night hours" of doubt to the 
 * "morning hours" of affirmation.
 * * KEY AGENTS:
 * - Molly Bloom: Individual moderate navigating a "Rope" of memory and desire.
 * - Leopold Bloom: Individual powerless subject to Molly's domestic demands and 
 * final acceptance (Noose/Rope).
 * - The Catholic Church (Mrs. Riordan's path): Institutional force viewing 
 * female morality as an immutable law (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(molly_affirmation_id, 0, 10).
% Fixed: Changed 'cyclical_affirmation' to 'rope' to satisfy schema
narrative_ontology:constraint_claim(molly_affirmation_cycle, rope).

% Metrics: Extractiveness (0.4 - Emotional labor) and Suppression (0.5 - Boundary breakdown)
domain_priors:base_extractiveness(molly_affirmation_cycle, 0.4).
domain_priors:suppression_score(molly_affirmation_cycle, 0.5).
domain_priors:emerges_naturally(molly_affirmation_cycle).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(molly_affirmation_cycle, extractiveness, 0.4).
narrative_ontology:constraint_metric(molly_affirmation_cycle, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(molly_affirmation_cycle, bloom_leopold_marriage).
constraint_victim(molly_affirmation_cycle, social_orthodoxy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: MOLLY BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Molly uses her internal monologue as a "Rope"—a functional coordination 
   tool to weave her disparate memories (Gibraltar, Bloom, Boylan) into a 
   single, coherent affirmation of her life.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    molly_affirmation_cycle,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LEOPOLD BLOOM - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - "Ask to get his breakfast in bed with a couple 
   of eggs".
   WHY: From Bloom's current state of sleep and domestic dependence, Molly's 
   affirmation and its associated demands are a "Noose"—he is bound by 
   her decisions and her "Yes" without active agency in this chapter.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    molly_affirmation_cycle,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SOCIAL ORTHODOXY (Mrs. Riordan) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "Pious because no man would look at her twice" 
  .
   WHY: From the institutional perspective of 1904 Catholic morality, the 
   laws of marriage and female propriety are a "Mountain"—immutable and 
   divinely ordained laws that Molly's "stream" technically violates.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    molly_affirmation_cycle,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(molly_affirmation_tests).

test(multi_perspective_penelope) :-
    constraint_indexing:constraint_classification(molly_affirmation_cycle, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(molly_affirmation_cycle, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_rhythm_scaling) :-
    % Institutional view sees the moral code as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(molly_affirmation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'cyclical_affirmation' mismatch. Declaring the 
 * claim as 'rope' allows the audit to identify how Molly's internal 
 * coordination (Rope) creates a "Stalemate" with the external 
 * institutional 'Mountain' of social expectations.
 */

omega_variable(
    molly_intent_sincerity,
    "Is the final 'Yes' a reconciliation with Bloom (Rope) or an undifferentiated 
    affirmation of all sensation (Mountain)?",
    resolution_mechanism("Analysis of the transition from Gibraltar memories to the bed in Eccles Street"),
    impact("If reconciliation: Rope. If biological sensation: Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================\
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Outright Departure: Mentioned as "as well him as another". 
 * Rejected/Suppressed by Molly's emotional history and her logistical 
 * domestic needs.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp18, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp18, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp18, noose, agent_power(individual_powerless)).
