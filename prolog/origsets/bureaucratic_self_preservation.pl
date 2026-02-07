% ============================================================================
% CONSTRAINT STORY: bureaucratic_self_preservation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_bureaucratic_self_preservation, []).

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
 * * constraint_id: bureaucratic_self_preservation
 * human_readable: The Inertial Office
 * domain: political
 * * SUMMARY:
 * A phenomenon where an administrative body prioritizes its own survival and 
 * budget growth over its original mission. It creates a "Piton" effect where 
 * the constraint outlives its utility.
 * * KEY AGENTS:
 * - The Applicant: Subject (Powerless) - Navigating dead-end paperwork.
 * - The Bureau Chief: Beneficiary (Institutional) - Maintaining headcounts.
 * - The Auditor: Auditor (Analytical) - Observing the efficiency decay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(bureaucratic_self_preservation, 0.72). % High extraction (Snare/Piton)
domain_priors:suppression_score(bureaucratic_self_preservation, 0.80).   % High: monopoly on service
domain_priors:theater_ratio(bureaucratic_self_preservation, 0.85).       % High: signals Piton status

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bureaucratic_self_preservation, extractiveness, 0.72).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, theater_ratio, 0.85).

% Binary flags
domain_priors:requires_active_enforcement(bureaucratic_self_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual sees a predatory trap of red tape.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution justifies the friction as essential "due process" or coordination.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Detection of inertial maintenance of a non-functional constraint.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_self_preservation_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the powerless, Rope for the institution.
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope, context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Verify the analytical observer sees the Piton due to high theater_ratio.
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(bureaucratic_self_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base_extractiveness is set to 0.72 because the bureaucracy absorbs 
 * significant public resources relative to its output.
 * * [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved by the High Theater Ratio (0.85). This confirms the 
 * constraint is no longer a "Rope" for society, but a "Piton" maintained by 
 * institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High extraction (> 0.46) requires omega resolution.
omega_variable(
    omega_bureaucratic_drift,
    'Is the complexity required by law, or created by the office for budget justification?',
    'Audit of legislative mandates vs. internal procedural expansion.',
    'If Law: It is a Scaffold/Rope; If Office: It is a Snare/Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(bureaucratic_self_preservation, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
