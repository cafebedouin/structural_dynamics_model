% ============================================================================
% CONSTRAINT STORY: rafah_crossing_lifeline
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_rafah_crossing, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rafah_crossing_lifeline
 * human_readable: The Rafah Bottleneck
 * domain: political/geographical
 * * SUMMARY:
 * The Rafah crossing serves as a "literal lifeline" for Palestinians needing 
 * medical treatment. Under the 2026 peace plan, it has reopened as a 
 * highly restricted, multi-layered security bottleneck.
 * * KEY AGENTS:
 * - The Patient: Subject (Powerless) - Individuals like Mohammed Mahdi seeking 
 * medical exit.
 * - The Oversight: Beneficiary (Institutional) - Multi-entity security (Israel, 
 * Egypt, EU, PA).
 * - The UN Auditor: Auditor (Analytical) - Tracking the 18,500 people needing 
 * medical evacuation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is low (not a rent-seeking toll), but Suppression is extreme.
domain_priors:base_extractiveness(rafah_crossing_lifeline, 0.15). 
domain_priors:suppression_score(rafah_crossing_lifeline, 0.95).   
domain_priors:theater_ratio(rafah_crossing_lifeline, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rafah_crossing_lifeline, extractiveness, 0.15).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, theater_ratio, 0.4).

% Binary flags
domain_priors:requires_active_enforcement(rafah_crossing_lifeline).
narrative_ontology:has_sunset_clause(rafah_crossing_lifeline). % Tied to peace plan milestones

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (MOUNTAIN)
% Due to the "bottleneck" and 50-person daily limit, it remains an immovable limit.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, mountain, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE ARCHITECT (SCAFFOLD)
% Viewed by authorities as a temporary, high-security bridge to demilitarization.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, scaffold, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% Detects the coordination function (medical evacuation) despite the friction.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rafah_tests).

test(scaffold_validation) :-
    % Verify it is classified as a Scaffold due to the peace plan sunset clause.
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, scaffold, _),
    narrative_ontology:has_sunset_clause(rafah_crossing_lifeline).

test(suppression_intensity) :-
    domain_priors:suppression_score(rafah_crossing_lifeline, S),

    S > 0.90. % Reflects the multi-layered security bottleneck.

:- end_tests(rafah_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Perspectival Gap" is defined by time and throughput. To a patient with 
 * a life-threatening wound, the crossing is a Mountain due to the 50-person 
 * cap. To the peace plan architects, it is a Scaffold—a temporary 
 * necessity to test Hamas's "bluff" on demilitarization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hamas_disarmament,
    'Will Hamas fulfill the requirements to disarm and demilitarize?',
    'Direct verification by the European border-monitoring team.',
    'If False: The Scaffold collapses back into a Snare/Closed border.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rafah_crossing_lifeline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as political "resignation" replaces functional speed.
narrative_ontology:measurement(rafah_tr_t0, rafah_crossing_lifeline, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rafah_tr_t5, rafah_crossing_lifeline, theater_ratio, 5, 0.25).
narrative_ontology:measurement(rafah_tr_t10, rafah_crossing_lifeline, theater_ratio, 10, 0.40).

% Extraction: Remains low as the goal is security, not rent-seeking.
narrative_ontology:measurement(rafah_ex_t0, rafah_crossing_lifeline, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rafah_ex_t10, rafah_crossing_lifeline, base_extractiveness, 10, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
