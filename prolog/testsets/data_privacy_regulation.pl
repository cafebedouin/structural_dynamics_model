% ============================================================================
% CONSTRAINT STORY: data_privacy_regulation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-01-23
% ============================================================================

:- module(constraint_data_privacy_regulation, []).

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
 * * constraint_id: data_privacy_regulation
 * human_readable: Data Privacy Regulation (e.g., GDPR)
 * domain: political/economic/social/technological
 * * SUMMARY:
 * Data privacy regulations like GDPR aim to protect user data but also create compliance burdens and potential restrictions on data use. This creates a tension between individual rights, business interests, and government oversight.
 * * KEY AGENTS:
 * - User: Subject (Powerless)
 * - Corporation: Beneficiary (Institutional)
 * - Regulator: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(data_privacy_regulation, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(data_privacy_regulation, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(data_privacy_regulation, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(data_privacy_regulation, extractiveness, 0.55).
narrative_ontology:constraint_metric(data_privacy_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(data_privacy_regulation, theater_ratio, 0.20).

% Binary flags
% narrative_ontology:has_sunset_clause(data_privacy_regulation).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(data_privacy_regulation). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(data_privacy_regulation, corporation).
narrative_ontology:constraint_victim(data_privacy_regulation, user).

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
constraint_indexing:constraint_classification(data_privacy_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(data_privacy_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(data_privacy_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_privacy_regulation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(data_privacy_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_privacy_regulation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(data_privacy_regulation, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(data_privacy_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The user feels trapped because their data is collected and used, often without explicit consent or understanding. The corporation benefits from the data flow, viewing the regulation as a necessary (but sometimes burdensome) step to maintain public trust and data quality. From an analytical perspective, it's a tangled rope because it has both coordination (increased consumer confidence, data standardization) and extraction (compliance costs, limitations on data use). The perspectival gap stems from the differing power dynamics and access to information.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the regulation as pure extraction by explicitly recognizing the coordination function it provides (e.g., increased consumer confidence in data handling practices), even though it also entails extraction (compliance costs for businesses). Without considering this dual nature, the system might incorrectly classify it as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_data_privacy_regulation,
    'Does the regulation effectively protect user data, or does it primarily create compliance theater?',
    'Empirical studies on data breaches and user awareness after regulation implementation.',
    'If effective: Tangled Rope. If theater: Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(data_privacy_regulation, 0, 10).

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
narrative_ontology:measurement(data_privacy_regulation_tr_t0, data_privacy_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(data_privacy_regulation_tr_t5, data_privacy_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(data_privacy_regulation_tr_t10, data_privacy_regulation, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(data_privacy_regulation_ex_t0, data_privacy_regulation, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(data_privacy_regulation_ex_t5, data_privacy_regulation, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(data_privacy_regulation_ex_t10, data_privacy_regulation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */