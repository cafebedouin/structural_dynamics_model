% ============================================================================
% CONSTRAINT STORY: lestrygonian_metabolism_chp8
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 8 (Lestrygonians)
% ============================================================================

:- module(constraint_lestrygonian_metabolism_chp8, []).

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
 * * constraint_id: lestrygonian_metabolism_chp8
 * human_readable: The Metabolic Stream (Hunger and Predation)
 * domain: economic/biological
 * temporal_scope: June 16, 1904 (Noon)
 * spatial_scope: Dublin (Davy Byrne's, The Burton, and the Streets)
 * * SUMMARY:
 * This constraint represents the inescapable biological and economic cycle of 
 * consumption. It is the "stream of life" where one is either the eater or 
 * the eaten, manifesting as the predatory extraction of hunger.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating his hunger via "cheese sandwich" (Rope).
 * - The Horses/Jugginses: Individual powerless agents trapped in the cycle of work and feeding (Snare).
 * - Nature/The Stream: Institutional force viewing consumption as immutable law (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(lestrygonian_metabolism_chp8_interval, 0, 10).
narrative_ontology:constraint_claim(lestrygonian_metabolism_chp8, snare).

% Metrics: Extractiveness (0.6 - Predatory) and Suppression (0.4 - Natural)
domain_priors:base_extractiveness(lestrygonian_metabolism_chp8, 0.6).
domain_priors:suppression_score(lestrygonian_metabolism_chp8, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(lestrygonian_metabolism_chp8, extractiveness, 0.6).
narrative_ontology:constraint_metric(lestrygonian_metabolism_chp8, suppression_requirement, 0.4).
domain_priors:emerges_naturally(lestrygonian_metabolism_chp8).

% Explicit metric hooks to prevent auto-imputation
% BENEFICIARIES & VICTIMS
constraint_beneficiary(lestrygonian_metabolism_chp8, predator_class).
constraint_victim(lestrygonian_metabolism_chp8, bloom_leopold).
constraint_victim(lestrygonian_metabolism_chp8, the_poor_of_dublin).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom views his own hunger as a "Rope"—a functional coordination 
   problem he solves through a modest lunch, avoiding the "animal" 
   scenes at the Burton.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lestrygonian_metabolism_chp8,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE "JUGGINSES" (Horses) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - "Damn all they know or care... noses stuck in nosebags".
   WHY: For the laboring animals and the destitute, the metabolic cycle is a 
   "Snare"—an extractive, inescapable trap of work for food.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lestrygonian_metabolism_chp8,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL NECESSITY - Mountain
   --------------------------------------------------------------------------
   WHO: institutional/analytical - The "Stream of Life".
   WHY: From the perspective of biology, the cycle of "everybody eating 
   everyone else" is an immutable "Mountain"—a natural law.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lestrygonian_metabolism_chp8,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(global))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(lestrygonian_metabolism_chp8_tests).

test(multi_perspective_metabolism) :-
    constraint_indexing:constraint_classification(lestrygonian_metabolism_chp8, T1, context(individual_moderate, _, _, _)),
    constraint_indexing:constraint_classification(lestrygonian_metabolism_chp8, T2, context(powerless, _, _, _)),
    T1 = rope, T2 = snare.

test(extractiveness_check) :-
    % predatory systems should have high base extractiveness
    domain_priors:base_extractiveness(lestrygonian_metabolism_chp8, E),
    E >= 0.6.

:- end_tests(lestrygonian_metabolism_chp8_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to fix the 'metabolic_cycle' mismatch. By declaring the claim 
 * as 'snare', we highlight how the biological 'Mountain' actually functions 
 * as an extractive trap for the lower classes of Dublin.
 */

omega_variable(
    metempsychosis_immutability,
    "Is the 'stream of life' a biological Mountain or a spiritual Snare (Karma)?",
    resolution_mechanism("Verification of the 'Met him pike hoses' theory in Chapter 17"),
    impact("If Mountain: Suffering is a natural fact. If Snare: It is a breakable cycle."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Communal Kitchens: Imagined by Bloom but suppressed by the "all for 
 * number one" ethos of the city.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp08, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp08, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp08, snare, agent_power(powerless)).
