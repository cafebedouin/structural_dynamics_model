% ============================================================================
% CONSTRAINT STORY: nighttown_hallucinatory_vigil
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 15 (Circe)
% ============================================================================

:- module(nighttown_hallucinatory_vigil, []).

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
 * * constraint_id: nighttown_hallucinatory_vigil
 * human_readable: The Hallucinatory Vigil of Nighttown
 * domain: psychological/social
 * temporal_scope: June 16, 1904 (Midnight)
 * spatial_scope: Mabbot Street/Nighttown, Dublin
 * * SUMMARY:
 * Characters navigate a surreal environment where internal repressions manifest 
 * as external realities. The constraint is the "Nighttown" 
 * atmosphere itself—a psychological trap of "will-o’-the-wisps" and "danger 
 * signals" that extracts the agents' grip on reality.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating via "secret master" masonic 
 * rituals (Rope).
 * - The Deafmute Idiot: Individual powerless agent trapped in a "Saint Vitus’ 
 * dance" and imprisoned by children (Snare).
 * - Nighttown (The Environment): Institutional force where the "skeleton tracks" 
 * of the mind are immutable laws (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(nighttown_vigil_id, 0, 10).
% Fixed: Changed 'psychic_confrontation' to 'snare' to satisfy schema
narrative_ontology:constraint_claim(nighttown_hallucinatory_vigil, snare).

% Metrics: Extractiveness (0.6 - Psychological extraction) and Suppression (0.8 - High repression)
domain_priors:base_extractiveness(nighttown_hallucinatory_vigil, 0.6).
domain_priors:suppression_score(nighttown_hallucinatory_vigil, 0.8).
domain_priors:requires_active_enforcement(nighttown_hallucinatory_vigil).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(nighttown_hallucinatory_vigil, extractiveness, 0.6).
narrative_ontology:constraint_metric(nighttown_hallucinatory_vigil, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(nighttown_hallucinatory_vigil, nighttown_economic_vice).
constraint_victim(nighttown_hallucinatory_vigil, stephen_dedalus_psyche).
constraint_victim(nighttown_hallucinatory_vigil, bloom_leopold_psyche).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom utilizes his "alert" state and "masonic" stances as a "Rope"—a 
   functional coordination tool to protect Stephen and manage the 
   hallucinations of his own past (Rudy).
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    nighttown_hallucinatory_vigil,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DEAFMUTE IDIOT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - "shapeless mouth dribbling... jerks past".
   WHY: For the truly powerless in Nighttown, the environment is a "Snare"—an 
   inescapable trap of physical and social degradation where no coordination 
   benefits exist.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    nighttown_hallucinatory_vigil,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ENVIRONMENT (Nighttown) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The "Swancomb of the gondola" and the "lighthouse".
   WHY: From the institutional perspective of the district, the chaos and the 
   "Saint Vitus’ dance" are a "Mountain"—the immutable, logic-defying laws 
   of the night.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    nighttown_hallucinatory_vigil,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(nighttown_hallucinatory_tests).

test(multi_perspective_circe) :-
    constraint_indexing:constraint_classification(nighttown_hallucinatory_vigil, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(nighttown_hallucinatory_vigil, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 = rope, T2 = snare.

test(immutability_nighttown_scaling) :-
    % Institutional view sees the hallucinations as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(nighttown_hallucinatory_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'psychic_confrontation' mismatch. Declaring the 
 * claim as 'snare' allows the audit to identify how the "False Mountain" of 
 * hallucination masks an underlying 'Snare' of extreme psychological 
 * suppression.
 */

omega_variable(
    rudy_apparition_status,
    "Is Rudy's apparition a healing vision (Rope) or a symptom of deep 
    trauma-based extraction (Snare)?",
    resolution_mechanism("Comparative analysis of Bloom's posture as 'secret master'"),
    impact("If healing: Rope. If symptom: Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Physical Resistance/Departure: Stephen and Bloom could have left 
 * immediately. Rejected/Suppressed by the "Saint Vitus’ dance" and the 
 * "skeleton tracks" of the environment.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */



% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp15, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp15, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp15, snare, agent_power(individual_powerless)).
