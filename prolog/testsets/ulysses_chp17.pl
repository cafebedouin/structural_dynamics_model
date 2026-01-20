% ============================================================================
% CONSTRAINT STORY: bloom_ithaca_mathematical_order
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 17 (Ithaca)
% ============================================================================

:- module(bloom_ithaca_mathematical_order, []).

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
 * * constraint_id: bloom_ithaca_mathematical_order
 * human_readable: The Catechism of Mathematical Order
 * domain: scientific/ontological
 * temporal_scope: June 17, 1904 (2:00 AM - 3:00 AM)
 * spatial_scope: 7 Eccles Street, Dublin (and the Macrocosm)
 * * SUMMARY:
 * This constraint represents the exhaustive, cold, and "scientific" lens 
 * through which the final meeting of Bloom and Stephen is filtered. 
 * Every human movement is reduced to its mathematical components—angles, 
 * latitudes, and velocities.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating the late hour via 
 * rationalist coordination (Rope).
 * - Stephen Dedalus: Individual powerless agent, the "childman weary" 
 * for whom the logic is an inescapable trap (Noose).
 * - The Narrator/Universe: Institutional voice viewing the cosmos as 
 * immutable and deterministic (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_ithaca_interval, 0, 10).
% Fixed: Changed 'scientific_determinism' to 'rope' to satisfy schema
narrative_ontology:constraint_claim(bloom_ithaca_mathematical_order, rope).

% Metrics: Extractiveness (0.4 - Intellectual cost) and Suppression (0.6 - Emotional limit)
domain_priors:base_extractiveness(bloom_ithaca_mathematical_order, 0.4).
domain_priors:suppression_score(bloom_ithaca_mathematical_order, 0.6).
domain_priors:requires_active_enforcement(bloom_ithaca_mathematical_order).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(bloom_ithaca_mathematical_order, extractiveness, 0.4).
narrative_ontology:constraint_metric(bloom_ithaca_mathematical_order, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_ithaca_mathematical_order, rationalist_clarity).
constraint_victim(bloom_ithaca_mathematical_order, reader_emotional_catharsis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom uses scientific facts as a "Rope"—a tool to manage his 
   domestic reality and his "like and unlike reactions" to the day, 
   finding comfort in the "perpetual motion of the earth".
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_ithaca_mathematical_order,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: STEPHEN DEDALUS - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - The "manchild in the womb".
   WHY: For the exhausted Stephen, the mathematical "Catechism" is a 
   "Noose"—an extractive trap of interrogation that mirrors his 
   Jesuit training but offers no spiritual release.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_ithaca_mathematical_order,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COSMOS (Narrator) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional/analytical - The "proper perpetual motion of the earth" 
  .
   WHY: From the perspective of the narrative structure, the laws of physics 
   and the "everchanging tracks of neverchanging space" are a 
   "Mountain"—immutable and absolute.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_ithaca_mathematical_order,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(global))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_ithaca_mathematical_tests).

test(multi_perspective_ithaca) :-
    constraint_indexing:constraint_classification(bloom_ithaca_mathematical_order, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_ithaca_mathematical_order, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_cosmic_scaling) :-
    % Institutional view sees the Universe as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(bloom_ithaca_mathematical_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'scientific_determinism' mismatch. Declaring the 
 * claim as 'rope' allows the audit to detect the "Stalemate" where 
 * Bloom attempts rational coordination (Rope) while the narrative 
 * enforces an immutable "False Mountain" of cold data.
 */

omega_variable(
    bloom_stephen_coordination,
    "Does the 'parallel course' indicate a genuine coordination (Rope) or 
    an astronomical isolation (Noose)?",
    resolution_mechanism("Cross-referencing their 'like and unlike reactions'"),
    impact("If coordination: Rope. If isolation: Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Emotional Sentimentality: A warm, traditional reunion. 
 * Rejected/Suppressed by the "Catechism" format and the "reduced pace" 
 * of scientific prose.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp17, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp17, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp17, noose, agent_power(individual_powerless)).
