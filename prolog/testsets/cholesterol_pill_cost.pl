% ============================================================================
% CONSTRAINT STORY: cholesterol_pill_cost
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
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
 * * constraint_id: cholesterol_pill_cost
 * human_readable: Cost of Patented Cholesterol Medication
 * domain: economic
 * * SUMMARY:
 * An experimental cholesterol pill shows promise in reducing bad cholesterol.
 * However, its high price, protected by patent law, creates a significant barrier to access, particularly for vulnerable populations.
 * This constraint models the extractiveness imposed on patients by the cost, and the suppression of affordable alternatives via active patent enforcement.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Pharmaceutical Company: Beneficiary (Institutional)
 * - Healthcare System Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cholesterol_pill_cost, 0.7). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cholesterol_pill_cost, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cholesterol_pill_cost, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cholesterol_pill_cost, extractiveness, 0.7).
narrative_ontology:constraint_metric(cholesterol_pill_cost, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cholesterol_pill_cost, theater_ratio, 0.2).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cholesterol_pill_cost, tangled_rope).
narrative_ontology:human_readable(cholesterol_pill_cost, "Cost of Patented Cholesterol Medication").

% Binary flags
% narrative_ontology:has_sunset_clause(cholesterol_pill_cost).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cholesterol_pill_cost). % Required for Tangled Rope

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
    (E =< 0.15 ; E >= 0.46). % Ensures it's either low-extraction or high-extraction Snare/Tangled.

test(tangled_rope_structural_properties) :-
    % Verify that the necessary structural properties for a Tangled Rope are present.
    domain_priors:requires_active_enforcement(cholesterol_pill_cost),
    narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, _),
    narrative_ontology:constraint_victim(cholesterol_pill_cost, _).

:- end_tests(cholesterol_pill_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high cost of the patented cholesterol pill, combined with the legal suppression of affordable alternatives, creates a Snare for patients who need it but cannot afford it. The pharmaceutical company, from an institutional perspective, views the pricing as a necessary coordination mechanism (Rope) to recoup R&D investments and fund future innovation.
 *
 * The Perspectival Gap arises from the asymmetry in power and access to resources. Patients are powerless to negotiate the price, while the company has institutional power to set it. This asymmetry, combined with a genuine coordination function (funding R&D) and a clear extraction mechanism (high price), justifies the TANGLED ROPE classification from the Analytical Observer perspective. The high suppression score (0.7) reflects the power of the patent system to eliminate alternatives.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. A simpler model might label this a pure Snare, ignoring the pharmaceutical company's coordination function in developing and distributing the medication. By requiring `constraint_beneficiary` (coordination), `constraint_victim` (asymmetric extraction), and `requires_active_enforcement` (patent law), the system correctly identifies this as a hybrid constraint. The high price is extractive, but it is enabled by a system that also incentivizes innovation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cholesterol_pill_cost,
    'What portion of the drug price is necessary for R&D recoupment versus what portion is pure rent-seeking?',
    'Independent, transparent audits of pharmaceutical R&D, marketing, and production costs.',
    'If R&D costs are a small fraction of the price, the constraint is functionally a Snare. If they are substantial, it strongly supports the Tangled Rope classification.',
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
% This models the drug's price increasing over its patent life, a common
% real-world phenomenon.
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
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The patent system is a mechanism to enforce exclusive rights, coordinating
% market behavior to incentivize R&D.
narrative_ontology:coordination_type(cholesterol_pill_cost, enforcement_mechanism).

% Network relationships (structural influence edges)
% The high cost of patented drugs directly impacts the financial solvency
% of public and private healthcare systems.
narrative_ontology:affects_constraint(cholesterol_pill_cost, healthcare_system_solvency).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */