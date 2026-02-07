% ============================================================================
% CONSTRAINT STORY: overfitting_to_frameworks
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(overfitting_to_frameworks, []).

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
 * * constraint_id: overfitting_to_frameworks
 * human_readable: The Rigidity of the Map
 * domain: technological/cognitive/organizational
 * * SUMMARY:
 * This constraint occurs when an organization or agent optimizes their 
 * behavior so tightly to a specific evaluative or technical framework that 
 * they lose the ability to perceive or respond to external reality. The 
 * framework becomes a "Snare" for internal agents while appearing as a 
 * necessary "Rope" for institutional consistency.
 * * KEY AGENTS:
 * - Junior Implementer: Subject (Powerless)
 * - Framework Architect: Beneficiary (Institutional)
 * - Systems Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as the system siphons adaptive capacity into 
% hyper-specialized compliance with the internal map.
domain_priors:base_extractiveness(overfitting_to_frameworks, 0.78). 
domain_priors:suppression_score(overfitting_to_frameworks, 0.65). 
domain_priors:theater_ratio(overfitting_to_frameworks, 0.82). % High theater: focus on framework "scores" over actual outcomes.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(overfitting_to_frameworks, extractiveness, 0.78).
narrative_ontology:constraint_metric(overfitting_to_frameworks, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(overfitting_to_frameworks, theater_ratio, 0.82).

% Not a scaffold; this represents a systemic decay toward brittle rigidity.
% narrative_ontology:has_sunset_clause(overfitting_to_frameworks). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The implementer is trapped: following the framework leads to failure, 
% but diverging leads to professional extraction/punishment.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the framework as a vital Rope for maintaining 
% coordination and quality standards across a distributed organization.
constraint_indexing:constraint_classification(overfitting_to_frameworks, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the framework has atrophied 
% into an inertial spike that exists purely for self-validation.
constraint_indexing:constraint_classification(overfitting_to_frameworks, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(overfitting_to_frameworks, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.78) and high theater (0.82) as a Tangled Rope.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(overfitting_to_frameworks, E), E >= 0.50,
    domain_priors:suppression_score(overfitting_to_frameworks, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfitting_to_frameworks_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict for the powerless and institutional indices.
    constraint_indexing:constraint_classification(overfitting_to_frameworks, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfitting_to_frameworks, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio correctly triggers Piton classification.
    constraint_indexing:constraint_classification(overfitting_to_frameworks, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(overfitting_to_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the 'Mandatrophy' threshold where 
 * institutional focus has shifted from external value to internal metrics.
 * * PERSPECTIVAL GAP:
 * 
 * The Junior Implementer feels a Snare because they are penalized for 
 * "doing the right thing" if it violates the framework. The Architect 
 * sees a Rope because the framework ensures that everyone is speaking 
 * the same language and following the same script.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by identifying the system as a Piton for analytical auditors. 
 * While the framework appears to provide "coordination" (Rope), its high 
 * theater and extractive cost indicate it is an inert spike of logic 
 * that prevents the system from adapting to new information.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_framework_permeability,
    'Can the framework integrate "outside" signals, or is it a closed loop (Snare vs Mountain)?',
    'Auditing the rate of change in framework parameters relative to market volatility.',
    'If static: Mountain of Rigidity. If adaptive: Snare of Control.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(overfitting_to_frameworks, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
