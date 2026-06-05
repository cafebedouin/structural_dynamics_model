% ============================================================================
% CONSTRAINT STORY: us_congress_2026_midterms
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_us_congress_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: gerrymandering_incumbency_2026
 * human_readable: Structural Inertia of Congressional Control
 * domain: political
 * * SUMMARY:
 * This constraint represents the combination of hyper-partisan redistricting 
 * and the structural advantages of incumbency that dictate the 2026 US 
 * Midterm outcome. It acts as a filter that converts national sentiment into 
 * localized, predictable outcomes, often decoupling the popular will from 
 * seat distribution.
 * * KEY AGENTS:
 * - The Voter: Subject (Powerless)
 * - Party Leadership: Beneficiary (Institutional)
 * - Election Forecaster: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction due to the "safe seat" phenomenon reducing competitive choice.
domain_priors:base_extractiveness(gerrymandering_incumbency_2026, 0.68). 
domain_priors:suppression_score(gerrymandering_incumbency_2026, 0.75).   
domain_priors:theater_ratio(gerrymandering_incumbency_2026, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(gerrymandering_incumbency_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(gerrymandering_incumbency_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gerrymandering_incumbency_2026, theater_ratio, 0.4).
domain_priors:requires_active_enforcement(gerrymandering_incumbency_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Voters in non-competitive districts feel "trapped" by lines drawn to negate their influence.
constraint_indexing:constraint_classification(gerrymandering_incumbency_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the parties, these constraints provide stability, funding predictability, and coordination.
constraint_indexing:constraint_classification(gerrymandering_incumbency_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects that while the system extracts choice, it also prevents total state collapse via stability.
constraint_indexing:constraint_classification(gerrymandering_incumbency_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(gerrymandering_incumbency_2026, E), E >= 0.50,
    domain_priors:suppression_score(gerrymandering_incumbency_2026, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_congress_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gerrymandering_incumbency_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gerrymandering_incumbency_2026, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(gerrymandering_incumbency_2026, E),

    E >= 0.46. % Confirms high-extraction Snare/Tangled logic for 2026 structural barriers.

:- end_tests(us_congress_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 2026 midterms are governed by "The Great Sort" and the 2020 redistricting cycle. 
 * For a powerless individual, this is a Snare: their vote has marginal impact on 
 * overall control due to geographical constraints. For the Institutional Power (Parties), 
 * it is a Rope: a coordination mechanism to allocate resources to the 10-15% of 
 * competitive seats that actually decide Congress.
 * * [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved by recognizing that the "extraction" (voter apathy/lack of choice) 
 * is the byproduct of a "coordination" success (party stability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_midterm_delta,
    'Will the "Presidential Party Penalty" exceed the structural insulation of gerrymandered seats?',
    'Analysis of the generic ballot margin vs seat-swings in D+3/R+3 districts post-election.',
    'If True: Major party flip (Control Shift). If False: Status quo/Minority gain (Gridlock).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gerrymandering_incumbency_2026, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
