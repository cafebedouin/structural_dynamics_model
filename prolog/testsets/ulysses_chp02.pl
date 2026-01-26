% ============================================================================
% CONSTRAINT STORY: history_nightmare_1904
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 2 (Nestor)
% ============================================================================

:- module(constraint_history_nightmare_1904, []).

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
 * * constraint_id: history_nightmare_1904
 * human_readable: The Nightmare of History (The Providential Trap)
 * domain: political/social/economic
 * temporal_scope: June 16, 1904 (10:00 AM)
 * spatial_scope: Mr. Deasy's School, Dalkey
 * * SUMMARY:
 * Stephen Dedalus famously defines history as a "nightmare" from which he is 
 * trying to awake. This constraint represents the 
 * static, crushing weight of the past—manifesting as personal debt, 
 * institutional antisemitism, and the rote repetition of schoolroom lessons.
 * * KEY AGENTS:
 * - Stephen Dedalus: Individual moderate navigating a "Snare" of historical 
 * and financial debt.
 * - Mr. Deasy: Institutional representative for whom history is a "Mountain" 
 * moving toward a divine goal.
 * - Cyril Sargent: Individual powerless agent, a student "failing" to grasp 
 * the repetition, trapped in the "Snare" of the present.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(nestor_afternoon, 0, 10).
narrative_ontology:constraint_claim(history_nightmare_1904, snare).

% Metrics: Extractiveness (0.7 - Debt and exclusion) and Suppression (0.7 - High)
domain_priors:base_extractiveness(history_nightmare_1904, 0.7).
domain_priors:suppression_score(history_nightmare_1904, 0.7).
domain_priors:requires_active_enforcement(history_nightmare_1904).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(history_nightmare_1904, extractiveness, 0.7).
narrative_ontology:constraint_metric(history_nightmare_1904, suppression_requirement, 0.7).

% Institutional perspective: Mr. Deasy's view of history as a moral ledger.
constraint_indexing:constraint_classification(history_nightmare_1904, rope, agent_power(institutional)).

% Analytical perspective: History as an unchangeable sequence of facts.
constraint_indexing:constraint_classification(history_nightmare_1904, mountain, agent_power(analytical)).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(history_nightmare_1904, institutional_status_quo).
constraint_beneficiary(history_nightmare_1904, debt_holders).
constraint_victim(history_nightmare_1904, stephen_dedalus).
constraint_victim(history_nightmare_1904, the_powerless_student).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: STEPHEN DEDALUS - Snare
   --------------------------------------------------------------------------
   WHY: For Stephen, history is a "Snare"—a series of "ousted possibilities" 
   that have been "branded" and "fettered" by time, extracting his freedom 
   to act and saddling him with "agenbite of inwit".
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    history_nightmare_1904,
    snare,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CYRIL SARGENT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - "Ugly and futile... a snail's bed".
   WHY: From the perspective of the struggling student, the educational and 
   historical order is an inescapable "Snare" of failure and repetition 
   without any coordination benefit.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    history_nightmare_1904,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MR. DEASY (The School/State) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "All history moves towards one great goal".
   WHY: From the institutional perspective, history is a "Mountain"—an 
   immutable, teleological law of progress that exists as a manifestation 
   of divine will.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    history_nightmare_1904,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(history_nightmare_tests).

test(multi_perspective_nestor) :-
    constraint_indexing:constraint_classification(history_nightmare_1904, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(history_nightmare_1904, T2, context(agent_power(institutional), _, _, _)),
    T1 = snare, T2 = mountain.

test(extractiveness_debt_check) :-
    % Systems involving significant personal debt should score high
    domain_priors:base_extractiveness(history_nightmare_1904, E),
    E >= 0.7.

:- end_tests(history_nightmare_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * This refactor eliminates Global Repair by hardening the metrics. The 
 * audit can now cleanly detect the "False Mountain" of Deasy's teleology, 
 * which Stephen correctly identifies as a 'Nightmare' (Snare).
 */

omega_variable(
    history_teleology,
    "Is history moving toward a 'goal' (God) or is it a recursive loop of 
    ruin?",
    resolution_mechanism("Analysis of the 'livid final flame' vision vs the school gate reality"),
    impact("If goal: Mountain. If ruin: Snare."),
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
 * 1. Financial Autonomy: Stephen could pay his debts and leave. 
 * Suppression: Actively suppressed by his inability to manage the "shillings 
 * and pence" Mr. Deasy counts for him.
 * 2. Academic Success (Sargent): The student could master the lesson. 
 * Suppression: Suppressed by his "lean neck" and "ugly" futility in 
 * the face of the system.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */



% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp02, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp02, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp02, snare, agent_power(individual_powerless)).
