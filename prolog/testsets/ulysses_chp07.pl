% ============================================================================
% CONSTRAINT STORY: bloom_aeolus_advertisement_circuit
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 7 (Aeolus)
% ============================================================================

:- module(bloom_aeolus_advertisement, []).

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
 * * constraint_id: bloom_aeolus_advertisement_circuit
 * human_readable: The Advertisement Canvassing Cycle (Aeolus)
 * domain: economic/social
 * temporal_scope: June 16, 1904 (Mid-morning)
 * spatial_scope: Dublin (Press District)
 * * SUMMARY:
 * Leopold Bloom navigates the "Heart of the Hibernian Metropolis" to secure a 
 * three-month advertising renewal for Alexander Keyes. 
 * The constraint is the economic requirement to mediate between client and 
 * publisher in a chaotic, dismissive institutional environment.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate canvasser seeking coordination (Rope).
 * - Myles Crawford: Institutional editor viewing the economic reality as immutable (Mountain).
 * - Newsboys: Powerless urchins for whom the cycle is a mocking trap (Snare).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_aeolus_interval, 0, 10).
narrative_ontology:constraint_claim(bloom_aeolus_advertisement_circuit, snare).

% Metrics: Extractiveness (0.4) and Suppression (0.5)
domain_priors:base_extractiveness(bloom_aeolus_advertisement_circuit, 0.4).
domain_priors:suppression_score(bloom_aeolus_advertisement_circuit, 0.5).
domain_priors:requires_active_enforcement(bloom_aeolus_advertisement_circuit).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(bloom_aeolus_advertisement_circuit, extractiveness, 0.4).
narrative_ontology:constraint_metric(bloom_aeolus_advertisement_circuit, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_aeolus_advertisement_circuit, myles_crawford).
constraint_beneficiary(bloom_aeolus_advertisement_circuit, alexander_keyes).
constraint_victim(bloom_aeolus_advertisement_circuit, bloom_leopold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom views the canvassing circuit as a functional coordination tool 
   that provides a living through "pars" and "renewals".
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_aeolus_advertisement_circuit,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MYLES CRAWFORD - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The editor who "rules the world" of the press.
   WHY: Crawford treats the paper's economic necessities as irrelevant noise 
   compared to historical "afflatus".
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_aeolus_advertisement_circuit,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: NEWSBOYS - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - "young guttersnipes" mocking the ritual.
   WHY: From the street level, Bloom's circuit is an extractive, ridiculous 
   trapping ritual that makes him a target for mockery.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_aeolus_advertisement_circuit,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_aeolus_advertisement_tests).

test(multi_perspective_aeolus) :-
    constraint_indexing:constraint_classification(bloom_aeolus_advertisement_circuit, T1, context(individual_moderate, _, _, _)),
    constraint_indexing:constraint_classification(bloom_aeolus_advertisement_circuit, T2, context(institutional, _, _, _)),
    T1 = rope, T2 = mountain.

test(time_immutability_aeolus) :-
    % Historical/Institutional view sees the paper's culture as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(bloom_aeolus_advertisement_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to eliminate auto-imputation and ensure the 'Mountain' vs 'Snare' 
 * perspectival gap is audited. 
 */

omega_variable(
    crawford_alcohol_influence,
    "Is Crawford's refusal to change (Mountain) institutional or alcoholic whim?",
    resolution_mechanism("Comparative analysis of sober vs drunk decision-making"),
    impact("If whim: Snare. If policy: Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Direct Steady Employment: Rejected due to dwindling practice.
 * 2. Political Entry: Suppressed by Bloom's identity as a 'Semite'.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp07, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp07, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp07, snare, agent_power(individual_powerless)).
