% ============================================================================
% CONSTRAINT STORY: gerty_bloom_voyeuristic_distance
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 13 (Nausicaa)
% ============================================================================

:- module(gerty_bloom_voyeuristic_distance, []).

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
 * * constraint_id: gerty_bloom_voyeuristic_distance
 * human_readable: The Voyeuristic Erotic Distance
 * domain: social/religious/biological
 * temporal_scope: June 16, 1904 (Evening)
 * spatial_scope: Sandymount Strand, Dublin
 * * SUMMARY:
 * This constraint represents the eroticized distance between Gerty MacDowell and 
 * Leopold Bloom. It is a psychological boundary maintained by the "mysterious 
 * embrace" of the evening and the acoustic synchronization of the "Star of the 
 * Sea" service.
 * * KEY AGENTS:
 * - Gerty MacDowell: Individual moderate navigating a "Rope" of desire and social 
 * image.
 * - The Church (Star of the Sea): Institutional anchor viewing the moral 
 * distance as immutable law (Mountain).
 * - The Cuckoo (Clock/Bird): Powerless agent for whom the time-keeping and 
 * distance are an inescapable trap (Noose).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(gerty_bloom_interval, 0, 10).
% Fixed: Changed 'erotic_distancing' to 'noose' to satisfy schema
narrative_ontology:constraint_claim(gerty_bloom_voyeuristic_distance, noose).

% Metrics: Extractiveness (0.5 - Desolate extraction) and Suppression (0.5 - Social code)
domain_priors:base_extractiveness(gerty_bloom_voyeuristic_distance, 0.5).
domain_priors:suppression_score(gerty_bloom_voyeuristic_distance, 0.5).
domain_priors:requires_active_enforcement(gerty_bloom_voyeuristic_distance).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(gerty_bloom_voyeuristic_distance, extractiveness, 0.5).
narrative_ontology:constraint_metric(gerty_bloom_voyeuristic_distance, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gerty_bloom_voyeuristic_distance, dublin_social_propriety).
constraint_victim(gerty_bloom_voyeuristic_distance, gerty_macdowell).
constraint_victim(gerty_bloom_voyeuristic_distance, bloom_leopold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GERTY MACDOWELL - Rope
   --------------------------------------------------------------------------
   WHY: Gerty utilizes the distance as a "Rope"—a coordination tool to perform 
   her "exquisite" identity and manage her attraction through the safety of 
   the "lingering" evening light.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gerty_bloom_voyeuristic_distance,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CUCKOO (The Powerless) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - The "little canarybird" that must "tell the time" 
  .
   WHY: From the perspective of the powerless agent, the distance and the 
   acoustic order are a "Noose"—an extractive trap of mechanical 
   repetition that offers no agency or relief.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gerty_bloom_voyeuristic_distance,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CHURCH (Star of the Sea) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "The voice of prayer to her who is in her pure radiance" 
  .
   WHY: From the institutional religious perspective, the distance and moral 
   boundaries are a "Mountain"—an immutable, divine law that exists 
   independently of individual desire.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gerty_bloom_voyeuristic_distance,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(gerty_bloom_voyeuristic_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(gerty_bloom_voyeuristic_distance, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(gerty_bloom_voyeuristic_distance, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_erotic_scaling) :-
    % Institutional view sees the moral law as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(gerty_bloom_voyeuristic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'erotic_distancing' mismatch. Declaring the claim 
 * as 'noose' allows the audit to flag the "Stalemate" where individuals 
 * attempt to coordinate (Rope) while institutions maintain an 
 * immutable reality (Mountain).
 */

omega_variable(
    cuckoo_clock_synchronicity,
    "Is the cuckoo clock's mockery a literal sound (Mountain) or an internal 
    projection of Bloom's marital guilt (Noose)?",
    resolution_mechanism("Acoustic analysis of Chapter 13"),
    impact("If literal: Mountain. If projection: Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Direct Interaction: Bloom and Gerty could have crossed the distance. 
 * Rejected/Suppressed by 1904 Dublin social codes and the "Star of the Sea" 
 * morality.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */



% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp13, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp13, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp13, noose, agent_power(individual_powerless)).
