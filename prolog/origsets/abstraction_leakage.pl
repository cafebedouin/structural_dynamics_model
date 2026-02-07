% ============================================================================
% CONSTRAINT STORY: abstraction_leakage
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(abstraction_leakage, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:measurement/5.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: abstraction_leakage
 * human_readable: The Law of Leaky Abstractions
 * domain: technological/cognitive
 * * SUMMARY:
 * This constraint represents the failure of a simplified interface to hide the 
 * underlying complexity of a system. When the abstraction "leaks," the subject 
 * is forced to manage the low-level reality they were promised would be 
 * automated. This creates a Snare where the subject pays for the convenience 
 * of the abstraction but bears the full cognitive load of the complexity.
 * * KEY AGENTS:
 * - Application Developer: Subject (Powerless)
 * - Cloud Infrastructure Provider: Beneficiary (Institutional)
 * - Systems Reliability Engineer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.74) because the "leaked" complexity requires specialized 
% labor that the abstraction was intended to eliminate, siphoning margins.
domain_priors:base_extractiveness(abstraction_leakage, 0.74). 
domain_priors:suppression_score(abstraction_leakage, 0.55). 
domain_priors:theater_ratio(abstraction_leakage, 0.68). % High theater: The "simple" UI remains while the backend burns.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(abstraction_leakage, extractiveness, 0.74).
narrative_ontology:constraint_metric(abstraction_leakage, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(abstraction_leakage, theater_ratio, 0.68).

% This is an inherent property of complex software, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(abstraction_leakage). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The developer is trapped: they cannot fix the leak (it's in the provider's 
% black box), but they are responsible for the resulting downtime.
constraint_indexing:constraint_classification(abstraction_leakage, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The provider views the abstraction as a Rope—it is the only way to coordinate 
% millions of users onto a standardized infrastructure stack.
constraint_indexing:constraint_classification(abstraction_leakage, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.74) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(abstraction_leakage, E), E >= 0.50,
    domain_priors:suppression_score(abstraction_leakage, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_leakage_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict for the powerless and institutional indices.
    constraint_indexing:constraint_classification(abstraction_leakage, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_leakage, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy_check) :-
    % Ensure extraction (0.74) triggers the mandatory resolution logic.
    domain_priors:base_extractiveness(abstraction_leakage, E),

    E > 0.70.

:- end_tests(abstraction_leakage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) reflects the 'Mandatrophy' threshold where 
 * the abstraction no longer provides a net reduction in complexity.
 * * PERSPECTIVAL GAP:
 * 
 * The Developer feels a Snare because they are billed for "Serverless" 
 * while debugging server-specific TCP timeouts. The Provider sees a Rope 
 * because the abstraction allows for mass-scale coordination of resources.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This recognizes that while 
 * the leakage is extractive, the abstraction layer is still a necessary 
 * coordination artifact for the modern digital economy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_abstraction_fidelity,
    'Is the leak due to poor engineering (Snare) or fundamental logic limits (Mountain)?',
    'Comparison of leakage rates across multiple distinct architecture implementations.',
    'If universal: Mountain of Complexity. If isolated: Snare of Implementation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(abstraction_leakage, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(abstraction_leakage_tr_t0, abstraction_leakage, theater_ratio, 0, 0.5).
narrative_ontology:measurement(abstraction_leakage_tr_t5, abstraction_leakage, theater_ratio, 5, 0.6).
narrative_ontology:measurement(abstraction_leakage_tr_t10, abstraction_leakage, theater_ratio, 10, 0.68).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(abstraction_leakage_ex_t0, abstraction_leakage, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(abstraction_leakage_ex_t5, abstraction_leakage, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(abstraction_leakage_ex_t10, abstraction_leakage, base_extractiveness, 10, 0.74).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
