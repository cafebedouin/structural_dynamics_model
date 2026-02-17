% ============================================================================
% CONSTRAINT STORY: cancer_prevention
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
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
 * * constraint_id: cancer_prevention
 * human_readable: Systemic Barriers to Preventable Cancer Risk Reduction
 * domain: social
 * * SUMMARY:
 * A global report indicates that 40% of cancers are preventable through lifestyle changes and public health initiatives. This constraint focuses on the systemic factors that make these preventable cancers a reality despite available knowledge. It involves the tension between individual choices, systemic influences (e.g., availability of unhealthy foods, lack of access to healthcare), and the role of public health in mitigating these risks.
 * * KEY AGENTS:
 * - Vulnerable Populations: Subject (Powerless) - Limited access to resources and information, subject to marketing pressures.
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

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cancer_prevention, tangled_rope).
narrative_ontology:human_readable(cancer_prevention, "Systemic Barriers to Preventable Cancer Risk Reduction").

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

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
    (E =< 0.15 ; E >= 0.46). % Ensures it's either a low-extraction Rope or high-extraction Snare/Tangled.

test(tangled_rope_properties) :-
    % Verify that the necessary structural properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(cancer_prevention, _),
    narrative_ontology:constraint_victim(cancer_prevention, _),
    domain_priors:requires_active_enforcement(cancer_prevention).

:- end_tests(cancer_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base_extractiveness is set to 0.55 because there is a significant cost (in health and economic terms) to individuals and society from preventable cancers. The suppression_score is 0.70 because factors like targeted advertising of unhealthy products, food deserts, and insufficient public health messaging actively suppress healthier choices. The theater_ratio is low (0.30) because while there is some performative public health messaging, the underlying systemic drivers are functional and impactful, not merely theatrical.
 *
 * The perspectives differ starkly. Individuals feel trapped by their circumstances (Snare), while public health institutions see their role as providing a coordination mechanism (Rope) to improve outcomes. From an analytical viewpoint, this is a Tangled Rope because there is an extraction element benefiting certain industries (e.g., tobacco, processed food) but also an undeniable coordination function required from public health bodies.
 *
 * The Piton classification was removed as it is inconsistent with the low theater_ratio and the active, functional nature of the constraint.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling coordination as pure extraction. It explicitly models the tension between the genuine public health benefits and the asymmetric extraction by disease-promoting industries. If this were labeled just a "Snare", the coordination aspect would be lost, and the system could wrongly encourage interventions to remove the constraint altogether (e.g., remove all food regulations), which would do more harm than good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cancer_prevention,
    'To what extent are individual choices truly free, vs. determined by systemic factors (economic, social, environmental)?',
    'Longitudinal studies tracking health outcomes based on socioeconomic status and access to resources, controlling for genetic predispositions.',
    'If choices are largely constrained, public policy interventions (taxation, regulation) become more crucial. If choices are more independent, educational campaigns may suffice.',
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
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cancer_prevention_tr_t0, cancer_prevention, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cancer_prevention_tr_t5, cancer_prevention, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cancer_prevention_tr_t10, cancer_prevention, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cancer_prevention_ex_t0, cancer_prevention, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cancer_prevention_ex_t5, cancer_prevention, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(cancer_prevention_ex_t10, cancer_prevention, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(cancer_prevention, resource_allocation).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(cancer_prevention, public_health_funding).
narrative_ontology:affects_constraint(cancer_prevention, food_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */