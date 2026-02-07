% ============================================================================
% CONSTRAINT STORY: civilizational_maintenance_debt
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_civilizational_maintenance_debt, []).

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
 * * constraint_id: civilizational_maintenance_debt
 * human_readable: The Crumbling Foundation
 * domain: technological
 * * SUMMARY:
 * The growing gap between the infrastructure required to sustain modern living 
 * standards and the actual resources allocated to maintain them. It acts as 
 * a "Mountain" of physical decay that extracts future prosperity to pay for 
 * present stagnation.
 * * KEY AGENTS:
 * - The Resident: Subject (Powerless) - Lives with failing grids and bridges.
 * - The Utility Provider: Beneficiary (Institutional) - Extracts fees while deferring repairs.
 * - The Systems Engineer: Auditor (Analytical) - Measures the entropy/debt ratio.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(civilizational_maintenance_debt, 0.62). % High extraction from future generations.
domain_priors:suppression_score(civilizational_maintenance_debt, 0.40).   % Moderate: alternatives exist but are expensive.
domain_priors:theater_ratio(civilizational_maintenance_debt, 0.30).       % Low: The physical decay is non-theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(civilizational_maintenance_debt, extractiveness, 0.62).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, theater_ratio, 0.3).

% Binary flags
domain_priors:requires_active_enforcement(civilizational_maintenance_debt).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the failure of the power grid is an immutable limit.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view the "debt" as a coordination tool—prioritizing capital for growth over upkeep.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes it as a hybrid: it allows current society to function (coordination) by stealing from the future (extraction).
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(civilizational_maintenance_debt, E), E >= 0.50,
    domain_priors:suppression_score(civilizational_maintenance_debt, S), S > 0.35.

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% When the infrastructure is gone, but the tax for its "maintenance" remains.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(civilizational_maintenance_debt, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_maintenance_debt_tests).

test(perspectival_gap) :-
    % Verify variance: Mountain for the powerless, Rope for the institution.
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope, context(agent_power(institutional), _, _, _)).

test(tangled_detection) :-
    % Verify the analytical observer identifies the hybrid extraction/coordination signature.
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(civilizational_maintenance_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extraction is 0.62 because the "debt" represents a massive transfer of 
 * risk and cost to future populations who cannot opt out.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by distinguishing physical entropy (Mountain) 
 * from fiscal policy (Snare). For the individual with no exit, it is physically 
 * a Mountain, but analytically it is a Tangled Rope of mismanaged coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High-extraction constraints (> 0.46) require omega_variable.
omega_variable(
    omega_collapse_threshold,
    'At what point does the maintenance debt trigger a non-linear systemic collapse?',
    'Empirical stress-testing of civilizational nodes vs. historical entropy models.',
    'If early: The constraint is a Snare; If late: The constraint is a manageable Scaffold.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(civilizational_maintenance_debt, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
