% ============================================================================
% CONSTRAINT STORY: cancer_prevention
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-28
% ============================================================================

:- module(constraint_cancer_prevention, []).

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
 * * constraint_id: cancer_prevention
 * human_readable: Preventable Cancer Risk Factors
 * domain: social
 * * SUMMARY:
 * A global report indicates that 40% of cancers are preventable through lifestyle changes and public health initiatives. This constraint focuses on the systemic factors that make these preventable cancers a reality despite available knowledge. It involves the tension between individual choices, systemic influences (e.g., availability of unhealthy foods, lack of access to healthcare), and the role of public health in mitigating these risks.
 * * KEY AGENTS:
 * - Individual: Subject (Powerless) - Limited access to resources and information, subject to marketing pressures.
 * - Public Health Organizations: Beneficiary (Institutional) - Responsible for disseminating information and implementing preventative measures.
 * - Analytical Epidemiologist: Auditor (Analytical) - Analyzes data to identify risk factors and effective interventions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cancer_prevention, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cancer_prevention, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cancer_prevention, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cancer_prevention, extractiveness, 0.55).
narrative_ontology:constraint_metric(cancer_prevention, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(cancer_prevention, theater_ratio, 0.30).

% Binary flags
% narrative_ontology:has_sunset_clause(cancer_prevention).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cancer_prevention). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cancer_prevention, public_health_organizations).
narrative_ontology:constraint_victim(cancer_prevention, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as an immutable limit or predatory trap.
% NOTE: This may be upgraded to 'organized' power if a critical mass of victims exists.
constraint_indexing:constraint_classification(cancer_prevention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cancer_prevention, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(cancer_prevention, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(cancer_prevention).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(cancer_prevention, piton, 
%     context(agent_power(analytical), 
%             time_horizon(civilizational), 
%             exit_options(arbitrage), 
%             spatial_scope(universal))) :-
%     domain_priors:theater_ratio(cancer_prevention, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_prevention_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cancer_prevention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_prevention, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cancer_prevention, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(cancer_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base_extractiveness is set to 0.55 because there is a significant cost to individuals and society from preventable cancers. The suppression_score is 0.70 because factors like targeted advertising, lack of access to healthy food, and insufficient public health messaging suppress healthier choices.
 *
 * The perspectives differ because individuals may feel trapped by their circumstances (Snare), while public health institutions aim to provide a rope (Rope) to pull people out of this situation. From an analytical viewpoint, this is a Tangled Rope because there is an extraction element for certain industries (tobacco, processed food) but there's also an undeniable coordination element required from public health bodies.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling coordination as pure extraction by accounting for the genuine public health benefits alongside the asymmetric extraction of the disease-promoting industries. It explicitly models the tension between these two opposing forces. If this were labeled just a "Snare" the coordination aspect would be lost and the system could wrongly encourage interventions to remove the constraint altogether (e.g., remove all food regulations), which would do more harm than good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cancer_prevention,
    'To what extent are individual choices truly free, vs. determined by systemic factors (economic, social, environmental)?',
    'Longitudinal studies tracking health outcomes based on socioeconomic status and access to resources.',
    'If individual choices are largely constrained, public policy interventions become more crucial. If choices are more independent, educational and awareness campaigns may suffice.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cancer_prevention, 0, 10).

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
narrative_ontology:measurement(cancer_prevention_tr_t0, cancer_prevention, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cancer_prevention_tr_t5, cancer_prevention, theater_ratio, 5, 0.30).
narrative_ontology:measurement(cancer_prevention_tr_t10, cancer_prevention, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cancer_prevention_ex_t0, cancer_prevention, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cancer_prevention_ex_t5, cancer_prevention, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(cancer_prevention_ex_t10, cancer_prevention, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */