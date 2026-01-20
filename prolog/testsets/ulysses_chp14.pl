% ============================================================================
% CONSTRAINT STORY: gestation_the_wombfruit
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 14 (Oxen of the Sun)
% ============================================================================

:- module(gestation_the_wombfruit, []).

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
 * * constraint_id: gestation_the_wombfruit
 * human_readable: Biological Gestation and the Tribute of Solicitude
 * domain: biological/social
 * temporal_scope: June 16, 1904 (10:00 PM)
 * spatial_scope: National Maternity Hospital, Holles Street, Dublin
 * * SUMMARY:
 * This constraint represents the inescapable biological process of human birth—the 
 * "proliferent continuance" of the species. It governs the physical 
 * presence of the characters at the hospital and serves as the moral anchor 
 * for the "tribute of its solicitude".
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate observing the process with "paternal care" (Rope).
 * - The Hospital/Nature: Institutional force viewing gestation as an immutable law (Mountain).
 * - The Wombfruit (The Unborn): Powerless recipient of the "nature’s incorrupted benefaction" (Noose).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(gestation_wombfruit_id, 0, 10).
% Fixed: Changed 'biological_destiny' to 'mountain' to satisfy schema
narrative_ontology:constraint_claim(gestation_the_wombfruit, mountain).

% Metrics: Extractiveness (0.3 - Natural cost) and Suppression (0.4 - Low re-direction)
domain_priors:base_extractiveness(gestation_the_wombfruit, 0.3).
domain_priors:suppression_score(gestation_the_wombfruit, 0.4).
domain_priors:emerges_naturally(gestation_the_wombfruit).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(gestation_the_wombfruit, extractiveness, 0.3).
narrative_ontology:constraint_metric(gestation_the_wombfruit, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gestation_the_wombfruit, proliferent_continuance).
constraint_victim(gestation_the_wombfruit, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom views the event as a "Rope"—a coordination point where he can 
   provide "stewardship" and process his own grief over Rudy through 
   social empathy. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gestation_the_wombfruit,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE WOMBFRUIT (The Powerless) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - The "quickening" that has no agency.
   WHY: For the unborn, the biological process is a "Noose"—an extractive 
   trap of development where they are bound by "nature’s boon" without 
   consent or coordination.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gestation_the_wombfruit,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MEDICAL/MORAL DOCTRINE - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "The most in doctrine erudite".
   WHY: From the institutional perspective, gestation is a "Mountain"—an 
   immutable natural and theological law that "no nature’s boon can 
   contend against".
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gestation_the_wombfruit,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(gestation_the_wombfruit_tests).

test(multi_perspective_gestation) :-
    constraint_indexing:constraint_classification(gestation_the_wombfruit, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(gestation_the_wombfruit, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_gestation_scaling) :-
    % Institutional/Nature view sees birth as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(gestation_the_wombfruit_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate the 'biological_destiny' mismatch. Standardizing the 
 * claim to 'mountain' allows the audit to evaluate if the "Natural Law" of 
 * the hospital acts as a 'Rope' (coordination for birth) or a 
 * 'Noose' (theological trap).
 */

omega_variable(
    dowie_business_proposition,
    "Is Alexander J Christ Dowie's 'corking fine business proposition' a 
    spiritual Rope or a financial Noose?",
    resolution_mechanism("Investigation of the 'triple extract of infamy' monologue"),
    impact("If spiritual: Rope. If financial: Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. "Sinned against the light" (Prevention): Mentioned as an option. 
 * Rejected/Suppressed by the "most in doctrine erudite" as a "sign of 
 * original evil".
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp14, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp14, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp14, noose, agent_power(individual_powerless)).
