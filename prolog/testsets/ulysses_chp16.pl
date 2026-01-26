% ============================================================================
% CONSTRAINT STORY: bloom_samaritan_paternal_care
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 16 (Eumaeus)
% ============================================================================

:- module(bloom_samaritan_paternal_care, []).

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
 * * constraint_id: bloom_samaritan_paternal_care
 * human_readable: The Samaritan Paternal Obligation
 * domain: social/emotional
 * temporal_scope: June 17, 1904 (1:00 AM - 2:00 AM)
 * spatial_scope: Beaver street and the Cabman's Shelter, Dublin
 * * SUMMARY:
 * Leopold Bloom feels a self-imposed "duty" to protect the exhausted and 
 * "unsteady" Stephen Dedalus. This constraint operates in a 
 * narcotic state of early morning fatigue, extracting energy from Bloom to 
 * maintain social "propriety" and Stephen's safety.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating via "orthodox Samaritan" 
 * logic (Rope).
 * - Stephen Dedalus: Individual powerless agent, "unsteady" and "e.d.ed" 
 * (exhausted), for whom the care is an inescapable trap (Snare).
 * - Social Orthodoxy: Institutional force viewing paternal duty as an 
 * immutable law of "propriety" (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_samaritan_id, 0, 10).
% Fixed: Changed 'social_stewardship' to 'snare' to satisfy schema
narrative_ontology:constraint_claim(bloom_samaritan_paternal_care, snare).

% Metrics: Extractiveness (0.5 - High fatigue cost) and Suppression (0.6 - Social duty)
domain_priors:base_extractiveness(bloom_samaritan_paternal_care, 0.5).
domain_priors:suppression_score(bloom_samaritan_paternal_care, 0.6).
domain_priors:requires_active_enforcement(bloom_samaritan_paternal_care).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(bloom_samaritan_paternal_care, extractiveness, 0.5).
narrative_ontology:constraint_metric(bloom_samaritan_paternal_care, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_samaritan_paternal_care, stephen_dedalus_safety).
constraint_victim(bloom_samaritan_paternal_care, bloom_leopold_vitality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom views his care for Stephen as a "Rope"—a functional coordination 
   mechanism to navigate the "rub" of the late hour and ensure their 
   mutual "ablutions" and safety.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_samaritan_paternal_care,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: STEPHEN DEDALUS - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - "His mind was not exactly what you would call 
   wandering but a bit unsteady".
   WHY: From Stephen's exhausted perspective, Bloom's "Samaritan" care is 
   a "Snare"—he is effectively "kidnapped" by kindness and social obligation 
   without the agency to refuse.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_samaritan_paternal_care,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SOCIAL ORTHODOXY - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The "propriety" and "orthodox Samaritan fashion" 
   of 1904 Dublin.
   WHY: From the institutional perspective of social norms, the duty to 
   protect the "pale" and "unsteady" is an immutable fact of 
   civilized behavior—a Mountain.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_samaritan_paternal_care,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_samaritan_paternal_tests).

test(multi_perspective_eumaeus) :-
    constraint_indexing:constraint_classification(bloom_samaritan_paternal_care, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_samaritan_paternal_care, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 = rope, T2 = snare.

test(immutability_samaritan_scaling) :-
    % Institutional view sees the social code as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(bloom_samaritan_paternal_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'social_stewardship' mismatch. Mapping the claim 
 * to 'snare' allows the audit to detect if Bloom's "duty" is an act of 
 * genuine coordination (Rope) or a "Snare" that extracts Stephen's autonomy 
 * through unsolicited care.
 */

omega_variable(
    stephen_dissent_silence,
    "Is Stephen's silence at the end of the chapter an exhausted assent (Rope) 
    or a suppressed dissent (Snare)?",
    resolution_mechanism("Investigation into the 'unsteady' silence vs the ballad singing"),
    impact("If exhaustion: Mountain. If dissent: Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Immediate Separation: Bloom could have sent Stephen home in a 
 * "conveyance" alone. Rejected/Suppressed by Bloom's "duty" and his 
 * "soapsuddy" sense of companionship.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp16, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp16, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp16, snare, agent_power(individual_powerless)).
