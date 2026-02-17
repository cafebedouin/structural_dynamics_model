% ============================================================================
% CONSTRAINT STORY: civilizational_maintenance_debt
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

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
 * standards and the actual resources allocated to maintain them. It acts as a
 * system of deferred costs that extracts future prosperity to pay for present
 * consumption and stagnation.
 * * KEY AGENTS:
 * - The Resident: Subject (Powerless) - Lives with failing grids, crumbling bridges, and contaminated water.
 * - The Utility Provider/Fiscal Authority: Beneficiary (Institutional) - Extracts fees/taxes while deferring capital-intensive repairs.
 * - The Systems Engineer: Auditor (Analytical) - Measures the entropy/debt ratio.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(civilizational_maintenance_debt, 0.62). % High extraction from future generations.
domain_priors:suppression_score(civilizational_maintenance_debt, 0.40).   % Structural property (raw, unscaled). Alternatives (off-grid) are expensive/impractical.
domain_priors:theater_ratio(civilizational_maintenance_debt, 0.30).       % Low: The physical decay is non-theatrical and its effects are direct.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(civilizational_maintenance_debt, extractiveness, 0.62).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
% It's often framed as a necessary, unavoidable consequence of physical laws (entropy).
narrative_ontology:constraint_claim(civilizational_maintenance_debt, tangled_rope).
narrative_ontology:human_readable(civilizational_maintenance_debt, "The Crumbling Foundation").

% Binary flags
domain_priors:requires_active_enforcement(civilizational_maintenance_debt). % Enforcement via regulation, fee structures, and fiscal policy.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, current_fiscal_planners).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, future_generations).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, current_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the powerless individual, the failure of the power grid is a trap. The high base
% extraction (0.62) is amplified by powerlessness (π=1.5), resulting in an
% effective extraction χ of 0.93. This is a Snare, not a Mountain.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view the "debt" as a coordination tool—prioritizing capital for growth over upkeep.
% Effective extraction is suppressed by institutional power (π=-0.2), making it feel like pure coordination.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes it as a hybrid: it allows current society to function (coordination) by
% extracting massive value from the future (asymmetric extraction).
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_maintenance_debt_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the powerless, Rope for the institution.
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(civilizational_maintenance_debt),
    narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, _),
    narrative_ontology:constraint_victim(civilizational_maintenance_debt, _).

test(threshold_validation_high_extraction) :-
    % Verify the base extraction meets the threshold for a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(civilizational_maintenance_debt, ExtMetricName, E),
    E >= 0.46.

:- end_tests(civilizational_maintenance_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint was regenerated to fix multiple structural errors.
 * 1. The subject's perspective was changed from Mountain to Snare. While the effects (a failing bridge) feel like a physical limit, the cause is a policy choice with high extraction (0.62), which defines a Snare.
 * 2. The invalid Piton classification was removed, as the theater_ratio (0.30) is far below the required 0.70 threshold.
 * 3. Beneficiary and Victim declarations were added, which are required for the Tangled Rope classification to derive its coordination and asymmetric extraction properties.
 * 4. Temporal measurement data was added, as required for any constraint with base extraction > 0.46, to model the accumulation of this "debt" over time.
 *
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by distinguishing physical entropy (a potential Mountain) from the fiscal and political policies that manage it. For the individual with no exit, the system is a Snare created by policy. Analytically, it is a Tangled Rope: a system of coordination (keeping society running today) that is inextricably linked with severe, asymmetric extraction (from future generations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_civilizational_maintenance_debt,
    'At what point does the maintenance debt trigger a non-linear systemic collapse, rather than gradual decay?',
    'Empirical stress-testing of critical infrastructure nodes combined with historical analysis of civilizational decline models.',
    'If the collapse threshold is near: the constraint is an acute Snare. If it is distant: the constraint is a chronic, but manageable, Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(civilizational_maintenance_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the "debt" accumulating
% over a generational interval, starting as a manageable issue and growing into
% a severe extraction mechanism.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low as the problem is physical, not performative):
narrative_ontology:measurement(civilizational_maintenance_debt_tr_t0, civilizational_maintenance_debt, theater_ratio, 0, 0.25).
narrative_ontology:measurement(civilizational_maintenance_debt_tr_t5, civilizational_maintenance_debt, theater_ratio, 5, 0.28).
narrative_ontology:measurement(civilizational_maintenance_debt_tr_t10, civilizational_maintenance_debt, theater_ratio, 10, 0.30).

% Extraction over time (the deferred cost grows significantly):
narrative_ontology:measurement(civilizational_maintenance_debt_ex_t0, civilizational_maintenance_debt, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(civilizational_maintenance_debt_ex_t5, civilizational_maintenance_debt, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(civilizational_maintenance_debt_ex_t10, civilizational_maintenance_debt, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is fundamentally about allocating societal resources.
narrative_ontology:coordination_type(civilizational_maintenance_debt, resource_allocation).

% Network relationships (structural influence edges)
% The state of core infrastructure directly impacts the reliability of supply chains.
narrative_ontology:affects_constraint(civilizational_maintenance_debt, supply_chain_resilience).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */