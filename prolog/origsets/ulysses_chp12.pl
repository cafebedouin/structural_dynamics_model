% ============================================================================
% CONSTRAINT STORY: bloom_xenophobic_exclusion
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 12 (Cyclops)
% ============================================================================

:- module(bloom_xenophobic_exclusion, []).

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
 * * constraint_id: bloom_xenophobic_exclusion
 * human_readable: Ethnonationalist Exclusion (The Citizen's Creed)
 * domain: political/social
 * temporal_scope: June 16, 1904 (Afternoon)
 * spatial_scope: Barney Kiernan’s Pub, Dublin
 * * SUMMARY:
 * Leopold Bloom is subjected to a violent, xenophobic constraint enforced by 
 * "the citizen" and a chorus of pub-goers. This constraint defines 
 * "Irishness" through an exclusionary lens that targets Bloom's Jewish heritage 
 * and lack of ethnonationalist fervor.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate attempting to coordinate through reason (Rope).
 * - The Citizen: Institutional agent for whom ethnonationalism is an immutable law (Mountain).
 * - Garryowen (The Dog): Individual powerless recipient of the Citizen's violent environment (Snare).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_exclusion_id, 0, 10).
% Fixed: Changed 'exclusionary_identity' to 'snare' to satisfy schema
narrative_ontology:constraint_claim(bloom_xenophobic_exclusion, snare).

% Metrics: Extractiveness (0.6 - Violent extraction) and Suppression (0.7 - High social pressure)
domain_priors:base_extractiveness(bloom_xenophobic_exclusion, 0.6).
domain_priors:suppression_score(bloom_xenophobic_exclusion, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bloom_xenophobic_exclusion, extractiveness, 0.6).
narrative_ontology:constraint_metric(bloom_xenophobic_exclusion, suppression_requirement, 0.7).
domain_priors:requires_active_enforcement(bloom_xenophobic_exclusion).

% Explicit metric hooks to prevent auto-imputation
% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_xenophobic_exclusion, ethnonationalist_cohesion).
constraint_victim(bloom_xenophobic_exclusion, bloom_leopold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom treats the debate as a "Rope"—a functional if dangerous 
   coordination tool where he argues for "love" and "nation" to find a 
   middle ground. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_xenophobic_exclusion,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: GARRYOWEN (The Powerless) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - The dog "bloody well worth to tear him limb from limb".
   WHY: From the perspective of the powerless agent caught in the fray, the 
   xenophobic violence is a "Snare"—an inescapable trap of predatory 
   hostility.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_xenophobic_exclusion,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CITIZEN - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The enforcer of the "creed".
   WHY: To the Citizen, his ethnonationalist boundaries are a "Mountain"—an 
   immutable natural law of blood and soil.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_xenophobic_exclusion,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_xenophobic_exclusion_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(bloom_xenophobic_exclusion, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_xenophobic_exclusion, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_xenophobic_scaling) :-
    % Institutional view sees the Citizen's law as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(bloom_xenophobic_exclusion_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the schema mismatch. Declaring the claim as 'snare' 
 * allows the audit to identify the "False Mountain" of the Citizen's rhetoric, 
 * which masks a 'Snare' of literal physical extraction and social entrapment 
 *.
 */

omega_variable(
    bloom_prophetic_ascent,
    "Is Bloom's 'Elijah' ascent a transfiguration (Rope) or narrative mockery (Snare)?",
    resolution_mechanism("Analysis of the 'Abba! Adonai!' cry"),
    impact("If transfiguration: Bloom finds an exit option. If mockery: The Snare is total."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Silence/Assimilation: Bloom could have stayed quiet to avoid the 
 * Citizen's wrath. Rejected/Suppressed by Bloom's "gesticulating" need to 
 * define himself.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp12, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp12, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp12, snare, agent_power(powerless)).
