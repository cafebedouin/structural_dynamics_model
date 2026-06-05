% ============================================================================
% CONSTRAINT STORY: emergency_oversight_bureau
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_emergency_oversight_bureau, []).

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
 * * constraint_id: emergency_oversight_bureau
 * human_readable: The Crisis Scaffold
 * domain: political
 * * SUMMARY:
 * A temporary administrative body created to manage a specific recovery period. 
 * While it exerts high control (suppression), it includes a mandatory 
 * expiration date to prevent it from hardening into a permanent Snare.
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Undergoing temporary mandatory vetting.
 * - The Coordinator: Architect (Organized) - Managing the sunset transition.
 * - The Historian: Auditor (Analytical) - Monitoring for "Scaffold-to-Piton" drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(emergency_oversight_bureau, 0.48). % High extraction, but justified by coordination.
domain_priors:suppression_score(emergency_oversight_bureau, 0.75).   % High: emergency mandates.
domain_priors:theater_ratio(emergency_oversight_bureau, 0.15).       % Low: The crisis function is currently active.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(emergency_oversight_bureau, extractiveness, 0.48).
narrative_ontology:constraint_metric(emergency_oversight_bureau, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(emergency_oversight_bureau, theater_ratio, 0.15).

% Binary flags
narrative_ontology:has_sunset_clause(emergency_oversight_bureau).      % Mandatory for Scaffold status.
domain_priors:requires_active_enforcement(emergency_oversight_bureau).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% During the crisis, the subject feels the high suppression as a trap.
constraint_indexing:constraint_classification(emergency_oversight_bureau, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE ARCHITECT (SCAFFOLD)
% The organized body views this as a temporary, essential support structure.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(emergency_oversight_bureau).

% PERSPECTIVE 3: THE BENEFICIARY (ROPE)
% From an institutional view, this is pure coordination to prevent total collapse.
constraint_indexing:constraint_classification(emergency_oversight_bureau, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_oversight_bureau_tests).

test(scaffold_validation) :-
    % Verify that the sunset clause triggers the Scaffold classification for organized power.
    constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold, context(agent_power(organized), _, _, _)).

test(perspectival_shift) :-
    % Verify it appears as a Snare over an immediate time horizon but a Rope over a historical one.
    constraint_indexing:constraint_classification(emergency_oversight_bureau, snare, context(_, time_horizon(immediate), _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, rope, context(_, time_horizon(historical), _, _)).

:- end_tests(emergency_oversight_bureau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction is high (0.48), which usually triggers a Snare. However, because 
 * `has_sunset_clause` is true, the system correctly identifies this as a Scaffold.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the explicit time-horizon decay. High suppression 
 * is tolerated because it is non-permanent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sunset_integrity,
    'Will the sunset clause be honored, or will an "emergency extension" convert this to a Piton?',
    'Verification of the bureau dissolution date vs. legislative amendments.',
    'If honored: Success of the Scaffold; If extended: Failure into a Snare/Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(emergency_oversight_bureau, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
