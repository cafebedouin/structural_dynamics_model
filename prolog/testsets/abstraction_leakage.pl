% ============================================================================
% CONSTRAINT STORY: abstraction_leakage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_abstraction_leakage, []).

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
    constraint_indexing:constraint_classification/3.

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

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(abstraction_leakage, extractiveness, 0.74).
narrative_ontology:constraint_metric(abstraction_leakage, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(abstraction_leakage, theater_ratio, 0.68).

% The abstraction claims to be a pure coordination/simplification tool.
narrative_ontology:constraint_claim(abstraction_leakage, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification.
% Enforcement is through the platform's terms of service and technical lock-in.
domain_priors:requires_active_enforcement(abstraction_leakage).

% Structural property derivation hooks for Tangled Rope:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(abstraction_leakage, cloud_infrastructure_provider).
narrative_ontology:constraint_victim(abstraction_leakage, application_developer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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
% Detects the combination of a genuine coordination function (beneficiary exists)
% with high, asymmetric extraction (victim exists) and active enforcement.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_leakage_tests).

test(perspectival_gap_subject_beneficiary) :-
    % Verify the Snare vs Rope conflict for the powerless and institutional indices.
    constraint_indexing:constraint_classification(abstraction_leakage, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_leakage, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify that all three required properties for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(abstraction_leakage, _),
    narrative_ontology:constraint_victim(abstraction_leakage, _),
    domain_priors:requires_active_enforcement(abstraction_leakage).

:- end_tests(abstraction_leakage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) reflects the 'Mandatrophy' threshold where
 * the abstraction no longer provides a net reduction in complexity, instead
 * imposing the cost of debugging its failures onto the user. The suppression
 * score (0.55) represents the high cost and difficulty of migrating away from
 * the provider's ecosystem (technical lock-in).
 *
 * The classification is Tangled Rope because it possesses all three required
 * properties:
 * 1. A genuine coordination function (beneficiary: provider standardizing infrastructure).
 * 2. Asymmetric extraction (victim: developer paying for a leaky service).
 * 3. Active enforcement (terms of service, API contracts, technical lock-in).
 *
 * PERSPECTIVAL GAP:
 * The Developer feels a Snare because they are billed for "Serverless"
 * while debugging server-specific TCP timeouts. The Provider sees a Rope
 * because the abstraction allows for mass-scale coordination of resources.
 *
 * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This recognizes that while
 * the leakage is extractive, the abstraction layer is still a necessary
 * coordination artifact for the modern digital economy, preventing a
 * misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_abstraction_leakage_fidelity,
    'Is the leak due to poor engineering (Snare) or fundamental logic limits (Mountain)?',
    'Comparison of leakage rates across multiple distinct architecture implementations.',
    'If universal: Mountain of Complexity. If isolated: Snare of Implementation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(abstraction_leakage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(abstraction_leakage_tr_t0, abstraction_leakage, theater_ratio, 0, 0.50).
narrative_ontology:measurement(abstraction_leakage_tr_t5, abstraction_leakage, theater_ratio, 5, 0.60).
narrative_ontology:measurement(abstraction_leakage_tr_t10, abstraction_leakage, theater_ratio, 10, 0.68).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(abstraction_leakage_ex_t0, abstraction_leakage, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(abstraction_leakage_ex_t5, abstraction_leakage, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(abstraction_leakage_ex_t10, abstraction_leakage, base_extractiveness, 10, 0.74).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The abstraction acts as a standardized infrastructure layer.
narrative_ontology:coordination_type(abstraction_leakage, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */