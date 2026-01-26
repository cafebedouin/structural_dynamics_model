% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% Status: [RESOLVED MANDATROPHY]
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini 2.0 Flash
% Source: General Academic Sociology / Labor Market Analysis
% ============================================================================

:- module(academic_tenure_system, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * SUMMARY:
 * The academic tenure system is a Tangled Rope that coordinates the protection of 
 * academic freedom (Rope) while asymmetrically extracting 6–7 years of hyper-productivity 
 * from junior faculty (Snare). It creates a bifurcated market where the "track" 
 * relies on the "adjunctified" majority to remain viable.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(academic_tenure_system, 1915, 2026).
narrative_ontology:constraint_claim(academic_tenure_system, tangled_rope).

% Base extractiveness (0.75): High reliance on pre-tenure labor to sustain institutional 
% rankings and research output.
domain_priors:base_extractiveness(academic_tenure_system, 0.75).

% Suppression score (0.60): Non-tenure security models exist but are socially 
% and institutionally devalued.
domain_priors:suppression_score(academic_tenure_system, 0.60).

domain_priors:requires_active_enforcement(academic_tenure_system).

constraint_beneficiary(academic_tenure_system, research_universities).
constraint_beneficiary(academic_tenure_system, tenured_senior_faculty).
constraint_victim(academic_tenure_system, pre_tenure_faculty_wellbeing).
constraint_victim(academic_tenure_system, adjunct_labor_pool).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE JUNIOR SCHOLAR - Snare
   WHO: individual_powerless | EXIT: trapped
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_tenure_system,
    snare,
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))
) :-
    domain_priors:base_extractiveness(academic_tenure_system, E), E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HIGHER-ED SOCIOLOGIST - Tangled Rope
   WHO: analytical | EXIT: analytical
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_tenure_system,
    tangled_rope,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :-
    domain_priors:base_extractiveness(academic_tenure_system, E), E > 0.4,
    domain_priors:suppression_score(academic_tenure_system, S), S > 0.5,
    domain_priors:requires_active_enforcement(academic_tenure_system),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: UNIVERSITY LEADERSHIP - Rope
   WHO: institutional | EXIT: mobile
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_tenure_system,
    rope,
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(national))
) :-
    domain_priors:requires_active_enforcement(academic_tenure_system),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(academic_tenure_system_tests).

test(multi_perspective_variance) :-
    % Junior Faculty (Snare) vs Admin (Rope) vs Sociologist (Tangled Rope)
    constraint_indexing:constraint_classification(academic_tenure_system, T1, context(individual_powerless, biographical, trapped, local)),
    constraint_indexing:constraint_classification(academic_tenure_system, T2, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(academic_tenure_system, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3, T1 \= T3.

:- end_tests(academic_tenure_system_tests).

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_tenure_extraction_intent,
    "Is the 0.75 extraction a functional necessity for intellectual rigor or a predatory bypass for senior labor?",
    resolution_mechanism("Audit of research quality vs. senior faculty teaching loads across departments"),
    impact("If necessity: Mountain. If predatory: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
