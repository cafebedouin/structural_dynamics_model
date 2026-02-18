% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(academic_tenure_system, []).

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
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: academic_tenure_system
 * human_readable: Academic Tenure System
 * domain: economic/social
 * * SUMMARY:
 * A system that coordinates the protection of academic freedom (Rope) while 
 * asymmetrically extracting hyper-productivity from junior faculty (Snare). 
 * * KEY AGENTS:
 * - Junior Scholar: Subject (Powerless)
 * - University Leadership: Beneficiary (Institutional)
 * - Higher-Ed Sociologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(academic_tenure_system, 0.75). %
domain_priors:suppression_score(academic_tenure_system, 0.60).   %
domain_priors:theater_ratio(academic_tenure_system, 0.52).       % Detected Piton drift

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(academic_tenure_system, extractiveness, 0.75).
narrative_ontology:constraint_metric(academic_tenure_system, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(academic_tenure_system, theater_ratio, 0.52).

% Constraint classification claim
narrative_ontology:constraint_claim(academic_tenure_system, tangled_rope).
narrative_ontology:human_readable(academic_tenure_system, "Academic Tenure System").
narrative_ontology:topic_domain(academic_tenure_system, "economic/social").

% Constraint metric facts used by classification engine
domain_priors:requires_active_enforcement(academic_tenure_system).

% Beneficiaries and Victims
narrative_ontology:constraint_beneficiary(academic_tenure_system, research_universities). %
narrative_ontology:constraint_beneficiary(academic_tenure_system, tenured_senior_faculty). %
narrative_ontology:constraint_victim(academic_tenure_system, pre_tenure_faculty_wellbeing). %
narrative_ontology:constraint_victim(academic_tenure_system, adjunct_labor_pool). %

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR SCHOLAR (SNARE)
constraint_indexing:constraint_classification(academic_tenure_system, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))). %

% PERSPECTIVE 2: UNIVERSITY LEADERSHIP (ROPE)
constraint_indexing:constraint_classification(academic_tenure_system, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))). %

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))). %

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_tenure_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_tenure_system, T1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_tenure_system, T2, context(agent_power(institutional), _, _, _)),
    T1 \= T2. %

:- end_tests(academic_tenure_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score of 0.75 reflects the "up-or-out" pressure where 
 * institutional ranking is built on the uncompensated surplus labor of 
 * pre-tenure researchers. The theater_ratio (0.52) indicates a drift toward 
 * Piton-like behavior, where administrative metrics (H-index, grant totals) 
 * begin to supersede the actual coordination of academic freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_tenure_extraction_intent,
    'Is the 0.75 extraction a functional necessity for intellectual rigor or a predatory bypass for senior labor?',
    'Audit of research quality vs. senior faculty teaching loads',
    'If necessity: Mountain. If predatory: Snare/Mandatrophy.',
    confidence_without_resolution(medium)
). %

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_tenure_system, 0, 10). %

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Proxy goals replacing real coordination)
narrative_ontology:measurement(tenure_tr_t0, academic_tenure_system, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tenure_tr_t5, academic_tenure_system, theater_ratio, 5, 0.38).
narrative_ontology:measurement(tenure_tr_t10, academic_tenure_system, theater_ratio, 10, 0.52).

% Extraction over time (Intensification of pre-tenure labor requirements)
narrative_ontology:measurement(tenure_ex_t0, academic_tenure_system, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(tenure_ex_t5, academic_tenure_system, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(tenure_ex_t10, academic_tenure_system, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
