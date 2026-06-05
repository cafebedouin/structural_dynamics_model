% ============================================================================
% CONSTRAINT STORY: rule_update_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(rule_update_failure, []).

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
 * * constraint_id: rule_update_failure
 * human_readable: Obsolete Protocol Enforcement
 * domain: technological/social
 * * SUMMARY:
 * This constraint occurs when a system continues to enforce a behavioral rule 
 * that no longer serves its original purpose due to a change in environment. 
 * The update mechanism has failed, turning a former 'Rope' into a 'Snare'.
 * * KEY AGENTS:
 * - End User: Subject (Powerless)
 * - System Maintainer: Beneficiary (Institutional)
 * - Protocol Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.72) because the obsolete rule costs time/energy for no gain.
domain_priors:base_extractiveness(rule_update_failure, 0.72). 
domain_priors:suppression_score(rule_update_failure, 0.58).
domain_priors:theater_ratio(rule_update_failure, 0.75). % High theater/inertia triggers Piton.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rule_update_failure, extractiveness, 0.72).
narrative_ontology:constraint_metric(rule_update_failure, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rule_update_failure, theater_ratio, 0.75).

% Required for Scaffold classification later in the lifecycle.
narrative_ontology:has_sunset_clause(rule_update_failure). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Users are trapped by a rule that serves no purpose, effectively a predatory snare.
constraint_indexing:constraint_classification(rule_update_failure, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the rule as a coordination 'Rope' that maintains stability.
constraint_indexing:constraint_classification(rule_update_failure, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% Because a sunset clause exists, it can be viewed as a temporary Scaffold.
constraint_indexing:constraint_classification(rule_update_failure, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(rule_update_failure).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.75) > 0.70 triggers Piton: a non-functional inertial spike.
constraint_indexing:constraint_classification(rule_update_failure, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(rule_update_failure, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================= */

:- begin_tests(rule_update_failure_tests).

test(perspectival_gap) :-
    % Verify Snare (Powerless) vs Rope (Institutional).
    constraint_indexing:constraint_classification(rule_update_failure, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rule_update_failure, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_threshold) :-
    % Ensure high theater ratio results in Piton classification.
    constraint_indexing:constraint_classification(rule_update_failure, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(rule_update_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) represents the friction of an un-updated protocol.
 * The high theater_ratio (0.75) indicates that the institution performs the 
 * "act" of enforcement despite the underlying function being atrophied.
 * * PERSPECTIVAL GAP:
 * The Individual feels a Snare because they pay the cost; the Institution 
 * feels a Rope because it keeps the system legible to them.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Scaffold/Piton overlap. The Tangled Rope signature is 
 * present, but the audit identifies it as a Piton because the theater ratio 
 * confirms that the coordination function is performative, not functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required because extraction (0.72) > 0.46.
omega_variable(
    omega_update_lag,
    'Is the failure due to technical debt (Mountain) or gatekeeping (Snare)?',
    'A/B testing the removal of the rule in an isolated regional node.',
    'If system stability holds: Snare. If system fails: Mountain (hidden dependency).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(rule_update_failure, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
