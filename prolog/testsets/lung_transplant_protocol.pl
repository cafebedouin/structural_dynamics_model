% ============================================================================
% CONSTRAINT STORY: lung_transplant_protocol
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_lung_transplant_protocol, []).

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
 * * constraint_id: lung_transplant_protocol
 * human_readable: Lung Transplant Allocation Protocol
 * domain: social
 * * SUMMARY:
 * The lung transplant protocol governs the allocation of scarce donor lungs to patients in need. It involves a complex set of criteria to determine recipient eligibility and priority. This story examines the constraint on access to this life-saving procedure, which functions as both a necessary coordination mechanism and a source of severe extraction for those deemed ineligible.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Transplant Center: Beneficiary (Institutional)
 * - Bioethicist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(lung_transplant_protocol, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(lung_transplant_protocol, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(lung_transplant_protocol, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(lung_transplant_protocol, extractiveness, 0.55).
narrative_ontology:constraint_metric(lung_transplant_protocol, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(lung_transplant_protocol, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(lung_transplant_protocol, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(lung_transplant_protocol).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(lung_transplant_protocol). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(lung_transplant_protocol, transplant_centers).
narrative_ontology:constraint_victim(lung_transplant_protocol, patients_in_need).

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
constraint_indexing:constraint_classification(lung_transplant_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(lung_transplant_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% The canonical classifier identifies this as Tangled Rope due to the presence of
% coordination (beneficiary), asymmetric extraction (victim), and enforcement.
constraint_indexing:constraint_classification(lung_transplant_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lung_transplant_protocol_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lung_transplant_protocol, ExtMetricName, E),
    (E =< 0.15 ; E >= 0.46). % Ensures it's either low-extraction or high-extraction.

test(tangled_rope_prerequisites) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(lung_transplant_protocol),
    narrative_ontology:constraint_beneficiary(lung_transplant_protocol, _),
    narrative_ontology:constraint_victim(lung_transplant_protocol, _).

:- end_tests(lung_transplant_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Lung Transplant Protocol is a canonical Tangled Rope. While its primary purpose is coordination—matching scarce organs to recipients to maximize survival—it also creates severe, asymmetric extraction. Patients who do not meet the criteria are effectively denied a chance at life (high suppression, high extraction). Transplant centers benefit from maintaining standards that improve success rates, which contributes to their institutional reputation and funding. The patient feels trapped due to their powerlessness and the high barrier to exit (lack of alternatives), experiencing the protocol as a Snare. The institution sees it as a necessary triage mechanism, a Rope. The analytical view, accounting for both the coordination function and the enforced, asymmetric extraction, correctly classifies it as a Tangled Rope.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification correctly identifies the system as having both a coordination function (organ allocation) and asymmetric extraction (denying access). This prevents misclassification as a pure Snare, which would ignore the life-saving coordination aspect, or a pure Rope, which would ignore the coercive denial of care to a vulnerable population. The `constraint_claim` of 'coordination' is key for the Boltzmann engine to detect the gap between the system's self-description and its extractive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lung_transplant_protocol,
    'Are the current allocation criteria the MOST effective and equitable given resource constraints, or do they contain hidden biases?',
    'Comparative analysis of different allocation algorithms and their real-world outcomes (survival rates, quality of life, equity across demographic groups).',
    'If TRUE: Current system is optimized. If FALSE: Alternative criteria could save more lives or distribute organs more equitably, revealing the current extraction as partially unnecessary.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lung_transplant_protocol, 0, 10).

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
narrative_ontology:measurement(lung_transplant_protocol_tr_t0, lung_transplant_protocol, theater_ratio, 0, 0.10).
narrative_ontology:measurement(lung_transplant_protocol_tr_t5, lung_transplant_protocol, theater_ratio, 5, 0.15).
narrative_ontology:measurement(lung_transplant_protocol_tr_t10, lung_transplant_protocol, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(lung_transplant_protocol_ex_t0, lung_transplant_protocol, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(lung_transplant_protocol_ex_t5, lung_transplant_protocol, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lung_transplant_protocol_ex_t10, lung_transplant_protocol, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(lung_transplant_protocol, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(lung_transplant_protocol, Value).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(lung_transplant_protocol, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */