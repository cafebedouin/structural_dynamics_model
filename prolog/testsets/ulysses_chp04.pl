% ============================================================================
% CONSTRAINT STORY: bloom_kosher_transgression
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 4 (Calypso)
% ============================================================================

:- module(bloom_kosher_transgression, []).

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
 * * constraint_id: bloom_kosher_transgression
 * human_readable: Dietary Law Transgression (The Pork Kidney)
 * domain: religious/social
 * temporal_scope: June 16, 1904
 * spatial_scope: 7 Eccles Street, Dublin
 * * SUMMARY:
 * Leopold Bloom, of Jewish descent, chooses to consume non-kosher pork kidneys. 
 * The constraint is the religious and cultural weight of the Old Testament law 
 * which he navigates through modernist assimilation and physical relish.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating a "Rope" of identity.
 * - The Ancestral Tradition: The institutional "Mountain" of history.
 * - The Cat: Powerless recipient of the law's logic as a "Noose".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Fixed: Changed 'dietary_identity' to 'noose' to satisfy schema
narrative_ontology:interval(bloom_kosher_transgression, 0, 10).
narrative_ontology:constraint_claim(bloom_kosher_transgression, noose).

% Metrics (Section 1 of Executive Summary)
domain_priors:base_extractiveness(bloom_kosher_transgression, 0.4).
domain_priors:suppression_score(bloom_kosher_transgression, 0.5).
domain_priors:requires_active_enforcement(bloom_kosher_transgression).

narrative_ontology:constraint_metric(bloom_kosher_transgression, extractiveness, 0.4).
narrative_ontology:constraint_metric(bloom_kosher_transgression, suppression_requirement, 0.5).

% Beneficiaries & Victims
constraint_beneficiary(bloom_kosher_transgression, dublin_social_fabric).
constraint_victim(bloom_kosher_transgression, bloom_ancestral_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM - Rope (Functionally navigate choice)
constraint_indexing:constraint_classification(
    bloom_kosher_transgression,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

% PERSPECTIVE 2: ANCESTRAL TRADITION - Mountain (Immutable law)
% REQUIRED: agent_power(institutional)
constraint_indexing:constraint_classification(
    bloom_kosher_transgression,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(global))
) :- !.

% PERSPECTIVE 3: THE CAT - Noose (Asymmetric power/logic trap)
% REQUIRED: agent_power(individual_powerless)
constraint_indexing:constraint_classification(
    bloom_kosher_transgression,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_kosher_transgression_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(bloom_kosher_transgression, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_kosher_transgression, Type2, context(agent_power(institutional), _, _, _)),
    Type1 = rope,
    Type2 = mountain.

:- end_tests(bloom_kosher_transgression_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate 'dietary_identity' error. Standardizing the claim 
 * as 'noose' allows the system to evaluate if Bloom's relish (Rope) 
 * hides a deeper cultural extraction (Noose).
 */

omega_variable(
    bloom_intent_internalization,
    "Is Bloom's choice an act of conscious modernism or passive loss?",
    resolution_mechanism("Analysis of Ithaca regarding his departure from ritual"),
    impact("If modernism: Rope. If loss: Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp04, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp04, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp04, noose, agent_power(individual_powerless)).
