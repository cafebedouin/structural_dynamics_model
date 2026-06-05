% ============================================================================
% CONSTRAINT STORY: cholesterol_pill_cost
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_cholesterol_pill_cost, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cholesterol_pill_cost
 * human_readable: Cost of Experimental Cholesterol Pill
 * domain: economic
 * * SUMMARY:
 * An experimental cholesterol pill shows promise in reducing bad cholesterol.
 * However, the high cost of the medication could create a barrier to access, particularly for vulnerable populations.
 * This constraint models the extractiveness imposed on patients by the cost, and the suppression of alternative affordable treatments.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Pharmaceutical Company: Beneficiary (Institutional)
 * - Healthcare System: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cholesterol_pill_cost, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cholesterol_pill_cost, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cholesterol_pill_cost, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cholesterol_pill_cost, extractiveness, 0.6).
narrative_ontology:constraint_metric(cholesterol_pill_cost, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cholesterol_pill_cost, theater_ratio, 0.1).

% Binary flags
% narrative_ontology:has_sunset_clause(cholesterol_pill_cost).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(cholesterol_pill_cost). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, pharmaceutical_company).
narrative_ontology:constraint_victim(cholesterol_pill_cost, patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
% NOTE: This may be upgraded to 'organized' power if a critical mass of victims exists.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives.  Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cholesterol_pill_cost, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(cholesterol_pill_cost, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(cholesterol_pill_cost).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(cholesterol_pill_cost, piton, 
%     context(agent_power(analytical), 
%             time_horizon(civilizational), 
%             exit_options(arbitrage), 
%             spatial_scope(universal))) :-
%     domain_priors:theater_ratio(cholesterol_pill_cost, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cholesterol_pill_cost_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cholesterol_pill_cost, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(cholesterol_pill_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high cost of the experimental cholesterol pill, combined with the potential lack of affordable alternatives, creates a Snare for patients who need it but cannot afford it. The pharmaceutical company, from an institutional perspective, views the cost as necessary for recouping R&D investments and ensuring future innovation (Rope).
 *
 * The Perspectival Gap arises from the asymmetry in power and access to resources.  Patients are largely powerless to negotiate the price, while the company has institutional power to set it. This asymmetry justifies the TANGLED ROPE classification for the Analytical Observer perspective (extraction + coordination).
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling the constraint as pure extraction by acknowledging the pharmaceutical company's coordination function in developing and providing the pill. The high price, while extractive, is also argued to be a necessary incentive for continued innovation. However, the active enforcement of patent protection and the suppression of generic alternatives must be part of the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cholesterol_pill_cost,
    'What is the actual cost of production and a reasonable profit margin for the pharmaceutical company?',
    'Independent cost analysis and industry benchmarking.',
    'If the actual cost is significantly lower, the constraint leans towards a pure Snare. If the profit margin is reasonable given R&D risks, it supports the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cholesterol_pill_cost, 0, 10).

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
narrative_ontology:measurement(cholesterol_pill_cost_tr_t0, cholesterol_pill_cost, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cholesterol_pill_cost_tr_t5, cholesterol_pill_cost, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cholesterol_pill_cost_tr_t10, cholesterol_pill_cost, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cholesterol_pill_cost_ex_t0, cholesterol_pill_cost, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cholesterol_pill_cost_ex_t5, cholesterol_pill_cost, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cholesterol_pill_cost_ex_t10, cholesterol_pill_cost, base_extractiveness, 10, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */