% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: dual_masters_dublin_1904
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 1 (Telemachus)
% ============================================================================

:- module(dual_masters_dublin_1904, []).

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
 * * constraint_id: dual_masters_dublin_1904
 * human_readable: The Dual Servitude (Empire and Church)
 * domain: political/religious
 * temporal_scope: June 16, 1904 (8:00 AM)
 * spatial_scope: The Martello Tower, Sandycove
 * * SUMMARY:
 * Stephen Dedalus defines his existence as a servant of two masters: the 
 * Imperial British state and the Holy Roman Catholic Church. 
 * This constraint represents the colonial and theological extraction of 
 * Irish identity.
 * * KEY AGENTS:
 * - Stephen Dedalus: Individual moderate navigating a "Noose" of inherited 
 * servitude.
 * - The Milk Woman: Individual powerless agent, a "messenger from the secret 
 * morning" who ignores her own language (Noose).
 * - Haines: Institutional representative for whom the Empire is a 
 * "Mountain" of historical necessity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(telemachus_morning, 0, 10).
narrative_ontology:constraint_claim(dual_masters_dublin_1904, noose).

% Metrics: Extractiveness (0.8 - Extreme colonial cost) and Suppression (0.8 - High)
domain_priors:base_extractiveness(dual_masters_dublin_1904, 0.8).
domain_priors:suppression_score(dual_masters_dublin_1904, 0.8).
domain_priors:requires_active_enforcement(dual_masters_dublin_1904).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(dual_masters_dublin_1904, extractiveness, 0.8).
narrative_ontology:constraint_metric(dual_masters_dublin_1904, suppression_requirement, 0.8).

% Institutional perspective: The Empire/Church as a civilizing coordination tool.
constraint_indexing:constraint_classification(dual_masters_dublin_1904, rope, agent_power(institutional)).

% Base priors for the Institutional Master
domain_priors:base_extractiveness(dual_masters_dublin_1904, 0.4).
domain_priors:suppression_score(dual_masters_dublin_1904, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dual_masters_dublin_1904, british_empire).
constraint_beneficiary(dual_masters_dublin_1904, catholic_church).
constraint_victim(dual_masters_dublin_1904, stephen_dedalus).
constraint_victim(dual_masters_dublin_1904, ireland_national_spirit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: STEPHEN DEDALUS - Noose
   --------------------------------------------------------------------------
   WHY: Stephen views the dual servitude as a "Noose"—an extractive trap 
   that demands his spiritual and political loyalty while mocking his 
   artistic and personal freedom.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    dual_masters_dublin_1904,
    noose,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MILK WOMAN - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - "A wandering crone... lowly form of an immortal" 
  .
   WHY: From the perspective of the powerless native, the constraint is 
   a "Noose"—she pays tribute to the "Saxon" (Haines) and the "Loud voice" 
   of the church without recognizing her own agency.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    dual_masters_dublin_1904,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: HAINES (The Empire) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "It seems history is to blame".
   WHY: From the institutional perspective of the colonizer, the dual 
   servitude is a "Mountain"—an immutable natural law of history for 
   which no individual is personally responsible.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    dual_masters_dublin_1904,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(dual_masters_tests).

test(multi_perspective_telemachus) :-
    constraint_indexing:constraint_classification(dual_masters_dublin_1904, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(dual_masters_dublin_1904, T2, context(agent_power(institutional), _, _, _)),
    T1 = noose, T2 = mountain.

test(extractiveness_colonial_check) :-
    % Colonial systems must have very high base extractiveness
    domain_priors:base_extractiveness(dual_masters_dublin_1904, E),
    E >= 0.8.

:- end_tests(dual_masters_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to stop auto-imputation. Declaring the claim as 'noose' allows 
 * the audit to identify the "Stalemate" between Stephen's acute awareness 
 * of the trap (Noose) and Haines's defense of it as natural law 
 * (Mountain).
 */

omega_variable(
    milk_woman_immortality,
    "Is the milk woman a 'messenger from the secret morning' (immortal) or 
    a victim of economic extraction (Noose)?",
    resolution_mechanism("Analysis of her reaction to the Irish language in later chapters"),
    impact("If immortal: The land is a Rope. If victim: The land is a Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. INSTITUTIONAL PERSPECTIVE (REPAIR)
   ========================================================================== */

/**
 * THE INSTITUTIONAL ROPE:
 * From the perspective of the British Empire (Haines/Mulligan) and the 
 * Church, the servitude is a coordination mechanism. 
 * - The Empire: Provides the "Saxon" efficiency, global trade, and 
 * administrative standardization (The Martello Tower itself is a defensive Rope).
 * - The Church: Provides the moral and metaphysical "Christine" architecture 
 * that sustains the soul.
 */

constraint_indexing:constraint_classification(
    dual_masters_dublin_1904, 
    rope, 
    agent_power(institutional)
).

domain_priors:base_extractiveness(dual_masters_dublin_1904, 0.4).
domain_priors:suppression_score(dual_masters_dublin_1904, 0.3).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Hellenism (Buck Mulligan): A way to escape the "Jesuit" vs "Saxon" 
 * binary. Rejected/Suppressed by Stephen as 
 * a "mockery".
 * 2. Irish Language Revival: Mentioned by the Milk Woman. 
 * Suppressed by the immediate utility of English ("Saxon").
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp01, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp01, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp01, noose, agent_power(individual_powerless)).
