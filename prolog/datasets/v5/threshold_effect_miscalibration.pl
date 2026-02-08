% ============================================================================
% CONSTRAINT STORY: threshold_effect_miscalibration
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(threshold_effect_miscalibration, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: threshold_effect_miscalibration
 * human_readable: The Invisible Cliff Trap
 * domain: environmental/social/technological
 * * SUMMARY:
 * A scenario where a system is managed based on linear models that fail to 
 * account for non-linear "tipping points." The "Rope" of institutional 
 * management provides a sense of safety and coordination until the miscalibrated 
 * threshold is crossed, instantly transforming the environment into a Snare.
 * * KEY AGENTS:
 * - Local Stakeholder: Subject (Powerless)
 * - Policy Modeler: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the miscalibration allows the institution to 
% siphons the subject's long-term safety into short-term operational growth.
domain_priors:base_extractiveness(threshold_effect_miscalibration, 0.84).
domain_priors:suppression_score(threshold_effect_miscalibration, 0.68).
domain_priors:theater_ratio(threshold_effect_miscalibration, 0.72). % Piton threshold (> 0.70)

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(threshold_effect_miscalibration, extractiveness, 0.84).
narrative_ontology:constraint_metric(threshold_effect_miscalibration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(threshold_effect_miscalibration, theater_ratio, 0.72).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a system that appears stable but is nearing an 
% unpriced catastrophic transition.
constraint_indexing:constraint_classification(threshold_effect_miscalibration, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the linear model as a Rope—a necessary simplification 
% for large-scale coordination and planning.
constraint_indexing:constraint_classification(threshold_effect_miscalibration, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: the "risk management" framework 
% is an inert spike that no longer maps to reality.
constraint_indexing:constraint_classification(threshold_effect_miscalibration, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(threshold_effect_miscalibration, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.84) and suppression (0.68) trigger the hybrid signature.
constraint_indexing:constraint_classification(threshold_effect_miscalibration, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(threshold_effect_miscalibration, E), E >= 0.50,
    domain_priors:suppression_score(threshold_effect_miscalibration, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(threshold_effect_miscalibration_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless but Rope for the institution.
    constraint_indexing:constraint_classification(threshold_effect_miscalibration, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(threshold_effect_miscalibration, rope, context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection.
    constraint_indexing:constraint_classification(threshold_effect_miscalibration, piton, context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(threshold_effect_miscalibration, E),

    E >= 0.70.

:- end_tests(threshold_effect_miscalibration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic exploitation of systemic blindness.
 
 * * PERSPECTIVAL GAP:
 * The Local Stakeholder feels a Snare because they bear the risk of a 
 * "cliff" the system ignores. The Policy Modeler sees a Rope because the 
 * linear model provides the coordination needed for budget cycles.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the coordination is no longer functional relative 
 * to real risks (Theater 0.72); the system is an inert spike siphoning 
 * 0.84 of the subject's risk-adjusted surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_threshold_visibility,
    'Can the tipping point be identified before it is crossed (Snare vs Mountain)?',
    'Auditing the delta between predictive non-linear models and actual system state.',
    'If visible: Snare of policy. If invisible: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(threshold_effect_miscalibration, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
