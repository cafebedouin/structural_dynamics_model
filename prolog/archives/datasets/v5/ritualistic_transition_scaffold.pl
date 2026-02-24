% ============================================================================
% CONSTRAINT STORY: ritualistic_transition_scaffold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_ritual_scaffold, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ritual_transition_scaffold
 * human_readable: The Habit-Building Scaffold
 * domain: social/institutional
 * * SUMMARY:
 * An intentional use of procedural theater to stabilize a chaotic organization. 
 * While the rituals themselves may have low utility, they act as a Scaffold 
 * to re-establish the baseline coordination (Rope) needed for future growth.
 * * KEY AGENTS:
 * - The New Hire: Subject (Powerless) - Finding safety in the new "routine."
 * - The Change Manager: Architect (Organized) - Designing the sunset-bound rituals.
 * - The Efficiency Auditor: Auditor (Analytical) - Monitoring the Scaffold-to-Rope decay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ritual_transition_scaffold, 0.35). % Moderate: Labor is used for habit-building, not pure extraction.
domain_priors:suppression_score(ritual_transition_scaffold, 0.60).   % Moderate: Participation is mandatory but justified by crisis.
domain_priors:theater_ratio(ritual_transition_scaffold, 0.72).       % High: The rituals are theatrical by design.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ritual_transition_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(ritual_transition_scaffold, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ritual_transition_scaffold, theater_ratio, 0.72).

% Binary flags
narrative_ontology:has_sunset_clause(ritual_transition_scaffold).    % Required for Scaffold status.
domain_priors:requires_active_enforcement(ritual_transition_scaffold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISORIENTED WORKER (ROPE)
% To someone lost in chaos, the theater provides a "Rope" of predictability.
constraint_indexing:constraint_classification(ritual_transition_scaffold, rope, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CHANGE ARCHITECT (SCAFFOLD)
% The architect views this as a temporary support structure that must expire.
constraint_indexing:constraint_classification(ritual_transition_scaffold, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(ritual_transition_scaffold).

% PERSPECTIVE 3: THE EXTERNAL AUDITOR (TANGLED ROPE)
% Detects the hybrid: It is theater (extraction of time) but it works (coordination).
constraint_indexing:constraint_classification(ritual_transition_scaffold, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(ritual_transition_scaffold, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_scaffold_tests).

test(scaffold_trigger) :-
    % Verify the presence of a sunset clause triggers Scaffold classification.
    constraint_indexing:constraint_classification(ritual_transition_scaffold, scaffold, context(agent_power(organized), _, _, _)).

test(theatrical_rope) :-
    % Verify that for the subject, even theater acts as a Rope during high-entropy events.
    constraint_indexing:constraint_classification(ritual_transition_scaffold, rope, context(agent_power(powerless), _, _, _)).

:- end_tests(ritual_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Unlike the "Piton" variant, this theater is constructive. The theater_ratio 
 * is high (0.72) not because of decay, but because the forms are symbols of 
 * order used to "trick" the system back into coordination.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the `has_sunset_clause`. This ensures the 
 * "Theater" is a tool for transition (Scaffold) rather than an eternal 
 * predatory trap (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scaffold_hardening,
    'Will the rituals be abandoned once stability is reached, or will they harden into a Piton?',
    'Observation of the theater_ratio 12 months post-stability.',
    'If abandoned: Success of the Scaffold; If hardened: Failure into a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(ritual_transition_scaffold, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
