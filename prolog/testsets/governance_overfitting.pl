% ============================================================================
% CONSTRAINT STORY: governance_overfitting
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_governance_overfitting, []).

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
 * * constraint_id: governance_overfitting
 * human_readable: Hyper-Specific Compliance Lock-in
 * domain: political/technological
 * * SUMMARY:
 * This constraint occurs when a governance framework is so precisely tuned to
 * past edge cases that it becomes a rigid barrier to novel coordination.
 * It functions as a 'Snare' of red tape for innovators while acting as
 * a protective 'Rope' for entrenched incumbents, who often shaped the rules.
 * * KEY AGENTS:
 * - Startup Innovators: Subject (Powerless)
 * - Industry Incumbents: Beneficiary (Institutional)
 * - Regulatory Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(governance_overfitting, 0.74). % Snare extraction >= 0.46
domain_priors:suppression_score(governance_overfitting, 0.68).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(governance_overfitting, 0.45).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(governance_overfitting, extractiveness, 0.74).
narrative_ontology:constraint_metric(governance_overfitting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(governance_overfitting, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(governance_overfitting, tangled_rope).

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(governance_overfitting). % Required for Tangled Rope
narrative_ontology:constraint_beneficiary(governance_overfitting, industry_incumbents). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(governance_overfitting, startup_innovators). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the innovator, the hyper-specific rules are a predatory snare designed to stop growth.
constraint_indexing:constraint_classification(governance_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Incumbents view the overfitting as a necessary 'Rope' that ensures safety and standards.
constraint_indexing:constraint_classification(governance_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction/suppression with a coordination function triggers the 'Tangled Rope' classification.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_overfitting_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(governance_overfitting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_overfitting, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification) :-
    % Verify the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(governance_overfitting, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction meets the linter's v3.4 core limits for high-extraction types.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(governance_overfitting, ExtMetricName, E),
    E >= 0.46.

:- end_tests(governance_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high extraction score (0.74) represents the significant economic advantage conferred to incumbents who can navigate or have shaped the complex regulatory landscape, while imposing prohibitive costs on new entrants. The suppression score (0.68) reflects how these rules effectively block novel approaches that don't fit the pre-defined mold. The theater ratio (0.45) is moderate, as there is still a genuine (though over-engineered) safety function being performed.
 *
 * The Perspectival Gap is stark: for the incumbent (Beneficiary), the rules are a 'Rope' providing stability and predictable market conditions. For the startup (Subject), the same rules are a 'Snare' of compliance costs and bureaucratic hurdles that seem designed to prevent their success.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of Mandatrophy, where a system designed for coordination becomes pathologically extractive. The 'Tangled Rope' classification is critical for resolving this. It prevents the system from making a binary error: it's not a pure 'Snare' (as it does have a coordination function for incumbents) and it's not a pure 'Rope' (as the powerless perspective reveals immense, asymmetric extraction). By acknowledging both the coordination and the extraction, the Tangled Rope classification correctly models the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_governance_overfitting,
    'Is the overfitting a result of genuine edge-case safety concerns or deliberate regulatory capture by incumbents?',
    'Comparative analysis of compliance costs for new entrants vs. lobbying expenditures by incumbents over the regulation''s lifecycle.',
    'If capture-linked: Confirms Tangled Rope/Snare. If safety-linked: suggests a Mountain of Complexity that happens to benefit incumbents.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(governance_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraints (base_extractiveness > 0.46).
% Models the gradual capture and complexification of the regulatory framework.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(gov_overfit_tr_t0, governance_overfitting, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gov_overfit_tr_t5, governance_overfitting, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gov_overfit_tr_t10, governance_overfitting, theater_ratio, 10, 0.45).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(gov_overfit_ex_t0, governance_overfitting, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(gov_overfit_ex_t5, governance_overfitting, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gov_overfit_ex_t10, governance_overfitting, base_extractiveness, 10, 0.74).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a regulatory framework, making it an enforcement mechanism.
narrative_ontology:coordination_type(governance_overfitting, enforcement_mechanism).

% Network relationships (structural influence edges)
% This type of regulation directly creates barriers to entry for new players.
narrative_ontology:affects_constraint(governance_overfitting, market_entry_barriers).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */