% ============================================================================
% CONSTRAINT STORY: bloom_acoustic_seduction_vigil
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 11 (Sirens)
% ============================================================================

:- module(bloom_acoustic_seduction_vigil, []).

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
 * * constraint_id: bloom_acoustic_seduction_vigil
 * human_readable: The Acoustic Seduction Vigil (The Cuckold's Hour)
 * domain: psychological/social
 * temporal_scope: June 16, 1904 (4:00 PM)
 * spatial_scope: The Ormond Hotel, Dublin
 * * SUMMARY:
 * Leopold Bloom experiences the seductive power of music (the "Sirens") while 
 * acutely aware that Blazes Boylan is visiting Molly. The 
 * constraint acts as an emotional "decoy" that extracts Bloom's attention 
 * through trilling notes and acoustic lures.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating a "Rope" of artistic appreciation and distraction.
 * - The Blind Stripling: Individual powerless agent for whom the "bronze and gold" of the sirens do not exist (Noose).
 * - The Ormond Hotel: Institutional setting viewing the acoustic order as an immutable law (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_sirens_interval, 0, 10).
% Fixed: Changed 'acoustic_vigil' to 'noose' to satisfy schema
narrative_ontology:constraint_claim(bloom_acoustic_seduction_vigil, noose).

% Metrics: Extractiveness (0.6 - Sensory trap) and Suppression (0.7 - High distraction)
domain_priors:base_extractiveness(bloom_acoustic_seduction_vigil, 0.6).
domain_priors:suppression_score(bloom_acoustic_seduction_vigil, 0.7).
domain_priors:requires_active_enforcement(bloom_acoustic_seduction_vigil).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(bloom_acoustic_seduction_vigil, extractiveness, 0.6).
narrative_ontology:constraint_metric(bloom_acoustic_seduction_vigil, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_acoustic_seduction_vigil, ormond_bar_social_fabric).
constraint_victim(bloom_acoustic_seduction_vigil, bloom_emotional_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom utilizes the music as a "Rope"—a functional coordination tool 
   to manage his acute distress by focusing on technical and aesthetic 
   qualities ("vibrato," "seven last words").
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_acoustic_seduction_vigil,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BLIND STRIPLING - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - "He did not see... Nor Ben nor Bob nor Tom".
   WHY: For the powerless, sensory-impaired agent, the seductive "order" of 
   the hotel is a "Noose"—an extractive trap of exclusion where the 
   coordination benefits of the "Siren" song are entirely absent.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_acoustic_seduction_vigil,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ORMOND ORDER - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The "Sirens" (barmaids) and the "gallant" drinkers.
   WHY: From the institutional perspective of the bar, the acoustic ritual 
   and social hierarchy are a "Mountain"—an immutable law of Dublin 
   hospitality and performance.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_acoustic_seduction_vigil,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_acoustic_seduction_tests).

test(multi_perspective_sirens) :-
    constraint_indexing:constraint_classification(bloom_acoustic_seduction_vigil, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_acoustic_seduction_vigil, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 = rope, T2 = noose.

test(immutability_acoustic_scaling) :-
    % Institutional view sees the Siren song as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(bloom_acoustic_seduction_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'acoustic_vigil' mismatch. Mapping the claim 
 * to 'noose' allows the audit to detect if the music's "decoy" effect 
 * creates a "False Mountain" of aesthetic escape that masks a "Noose" 
 * of emotional extraction.
 */

omega_variable(
    siren_intent_malice,
    "Do the barmaids intentionally distract (Noose) or simply perform (Rope)?",
    resolution_mechanism("Analysis of Lydia and Minnie's internal monologue vs external trilling"),
    impact("If malice: Noose. If performance: Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Immediate Confrontation: Bloom could have followed Boylan's car. 
 * Rejected/Suppressed by his "sluggish" temperament and the hotel's 
 * "acoustic lure".
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp11, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp11, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp11, noose, agent_power(individual_powerless)).
