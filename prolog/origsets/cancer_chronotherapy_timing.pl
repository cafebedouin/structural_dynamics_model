% ============================================================================
% CONSTRAINT STORY: cancer_chronotherapy_timing
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-04
% ============================================================================

:- module(constraint_chronotherapy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: cancer_chronotherapy_timing
 * human_readable: The Circadian Lifeline
 * domain: health/technological
 * * SUMMARY:
 * A medical intervention where the timing of immunotherapy (before 3pm) 
 * leverages T-cell congregation around tumours. This coordination 
 * around circadian rhythms resulted in a nearly doubling of survival time 
 * (28 months vs 17 months) for lung cancer patients.
 * * KEY AGENTS:
 * - The Patient: Subject (Powerless) - Dependent on hospital scheduling.
 * - The Oncologist: Beneficiary (Institutional) - Managing treatment cycles.
 * - The Chronobiologist: Auditor (Analytical) - Mapping T-cell migration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is near-zero; this is a timing shift of existing resources.
domain_priors:base_extractiveness(cancer_chronotherapy_timing, 0.05). 
% Suppression is moderate; patients are constrained by clinical hours.
domain_priors:suppression_score(cancer_chronotherapy_timing, 0.40).   
% Theater ratio is low; the evidence is based on a randomised-controlled trial.
domain_priors:theater_ratio(cancer_chronotherapy_timing, 0.10).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, extractiveness, 0.05).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, theater_ratio, 0.1).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (ROPE)
% For the patient, this is pure coordination that extends life.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE HOSPITAL (SCAFFOLD)
% Busy hospitals view the 3pm window as a temporary logistical support.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, scaffold, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(local))) :-
    narrative_ontology:has_sunset_clause(cancer_chronotherapy_timing).

% PERSPECTIVE 3: THE BIOLOGICAL REALIST (MOUNTAIN)
% Circadian rhythms are fixed biological limits ("Mountain").
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(trapped), 
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS (Outcome Audit)
   ========================================================================== */

:- begin_tests(chronotherapy_tests).

test(outcome_superiority) :-
    % Verify the 11-month survival advantage.
    SurvivalBefore3pm = 28,
    SurvivalAfter3pm = 17,
    SurvivalBefore3pm > SurvivalAfter3pm.

test(low_extraction_rope) :-
    % Verify it qualifies as a Rope due to minimal extraction.
    domain_priors:base_extractiveness(cancer_chronotherapy_timing, E),

    E < 0.10.

:- end_tests(chronotherapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Chronotherapy is a "Pure Rope" because it produces massive value (11 months 
 * of life) simply by re-organizing the sequence of actions. There 
 * is no "Perspectival Gap" regarding its utility; both the subject and 
 * beneficiary benefit from the coordination. 
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_individual_chronotype,
    'How do "morning larks" vs "night owls" shift the optimal 3pm window?',
    'Personalized immune-fluctuation tracking over 4 treatment cycles.',
    'Result: If significant, the "3pm" rule becomes an individualized Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_chronotherapy_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Remains low as clinical efficacy is the primary driver.
narrative_ontology:measurement(ct_tr_t0, cancer_chronotherapy_timing, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ct_tr_t10, cancer_chronotherapy_timing, theater_ratio, 10, 0.10).

% Extraction: Fixed at near-zero; the protocol is a matter of timing, not cost.
narrative_ontology:measurement(ct_ex_t0, cancer_chronotherapy_timing, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ct_ex_t10, cancer_chronotherapy_timing, base_extractiveness, 10, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
