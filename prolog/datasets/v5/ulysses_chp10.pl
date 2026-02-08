% ============================================================================
% CONSTRAINT STORY: the_viceregal_cavalcade
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 10 (Wandering Rocks)
% ============================================================================

:- module(the_viceregal_cavalcade, []).

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
 * * constraint_id: the_viceregal_cavalcade
 * human_readable: The Viceregal Procession (Colonial Presence)
 * domain: political/social
 * temporal_scope: June 16, 1904 (2:55 PM - 4:00 PM)
 * spatial_scope: Dublin (Phoenix Park to the Mirus Bazaar)
 * * SUMMARY:
 * The Lord Lieutenant's carriage moves through Dublin, acting as a temporal and 
 * spatial anchor that synchronizes the movements of citizens. 
 * The constraint represents the hierarchy of visibility and power demanded by 
 * the imperial presence.
 * * KEY AGENTS:
 * - The Citizens: Individual moderate agents navigating the city via "salutes" and "wonder" (Rope).
 * - William Humble, Earl of Dudley: Institutional representative viewing the procession as immutable (Mountain).
 * - The Blind Stripling: Individual powerless agent for whom the procession is a sensory obstruction or irrelevant trap (Snare).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(the_viceregal_cavalcade, 0, 10).
% Fixed: Changed 'imperial_sovereignty' to 'snare' to satisfy schema
narrative_ontology:constraint_claim(the_viceregal_cavalcade, snare).

% Metrics: Extractiveness (0.4) and Suppression (0.6)
domain_priors:base_extractiveness(the_viceregal_cavalcade, 0.4).
domain_priors:suppression_score(the_viceregal_cavalcade, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(the_viceregal_cavalcade, extractiveness, 0.4).
narrative_ontology:constraint_metric(the_viceregal_cavalcade, suppression_requirement, 0.6).
domain_priors:requires_active_enforcement(the_viceregal_cavalcade).

% Explicit metric hooks to prevent auto-imputation
% BENEFICIARIES & VICTIMS
constraint_beneficiary(the_viceregal_cavalcade, british_imperial_administration).
constraint_victim(the_viceregal_cavalcade, dublin_civilian_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CITIZENS (General Public) - Rope
   --------------------------------------------------------------------------
   WHY: For the average Dubliner, the cavalcade is a "Rope"—a coordination 
   mechanism that allows for social display (salutes) and spatial 
   synchronization without direct violence.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_viceregal_cavalcade,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BLIND STRIPLING - Snare
   --------------------------------------------------------------------------
   WHO: powerless - "He passed a blind stripling opposite Broadbent’s".
   WHY: To the powerless or impaired agent, the imperial ritual is an 
   extractive "Snare"—a physical and political obstruction that offers 
   no coordination benefit or visibility.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_viceregal_cavalcade,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EMPIRE (The Viceroy) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "acknowledge punctually salutes from rare male walkers".
   WHY: From the institutional perspective, the cavalcade is a "Mountain"—a 
   natural and immutable law of political reality and statecraft.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    the_viceregal_cavalcade,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(the_viceregal_cavalcade_tests).

test(multi_perspective_hierarchy) :-
    constraint_indexing:constraint_classification(the_viceregal_cavalcade, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(the_viceregal_cavalcade, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_colonial_scaling) :-
    % Institutional view sees the Empire as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(the_viceregal_cavalcade_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'imperial_sovereignty' mismatch. Declaring the 
 * claim as 'snare' allows the Audit Suite to flag the "False Mountain" 
 * effect of colonial power, where the state acts as law (Mountain) but 
 * functions as extraction (Snare).
 */

omega_variable(
    macintosh_man_anomaly,
    "Why does the man in the brown macintosh pass 'unscathed' across the path?",
    resolution_mechanism("Investigation into exit options (arbitrage) from the cavalcade's constraint"),
    impact("If he is an outlier: The Mountain is flawed. If a ghost: The constraint is irrelevant."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Nationalist Boycott: Rejected/Suppressed by the "wonder" and "punctual 
 * salutes" of the general populace in this chapter.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp10, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp10, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp10, snare, agent_power(powerless)).
