% ============================================================================
% CONSTRAINT STORY: child_marriage
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_child_marriage, []).

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
 * * constraint_id: child_marriage
 * human_readable: Child Marriage
 * domain: social/religious
 * * SUMMARY:
 * Child marriage, especially in regions like sub-Saharan Africa, involves marrying off children, typically girls, before they reach adulthood. This practice stems from various factors, including poverty, cultural traditions, and perceived protection for girls. It severely limits the child's educational opportunities, exposes them to health risks associated with early pregnancy, and perpetuates cycles of poverty.
 * * KEY AGENTS:
 * - Girl: Subject (Powerless)
 * - Family/Community: Beneficiary (Institutional)
 * - UNICEF/NGOs: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(child_marriage, 0.75). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(child_marriage, 0.85).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(child_marriage, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(child_marriage, extractiveness, 0.75).
narrative_ontology:constraint_metric(child_marriage, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(child_marriage, theater_ratio, 0.20).

% Binary flags
% narrative_ontology:has_sunset_clause(child_marriage).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(child_marriage). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(child_marriage, family).
narrative_ontology:constraint_victim(child_marriage, child).

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
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(child_marriage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(child_marriage_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(child_marriage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(child_marriage, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(child_marriage, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(child_marriage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Child marriage is scored with high extractiveness and suppression. The powerless subject is trapped and the institutional beneficiary (family/community) often views it as a cultural norm or economic necessity. However, from an analytical perspective, it's clearly a snare, depriving children of their fundamental rights and opportunities. The perspectival gap arises from the conflicting values and power dynamics within the system. The "institutional" perspective is classified as a Piton as the original justification (economic or safety) has degraded over time.
 * * MANDATROPHY ANALYSIS:
 * This is not a Tangled Rope because while there is a coordination function (family and community upholding the tradition), the extraction on the child is overwhelming and there is no genuine coordination function for the child's benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_child_marriage,
    'To what extent is child marriage truly a choice versus a coerced outcome?',
    'Longitudinal studies on the perceived autonomy of young brides and their long-term life satisfaction.',
    'If a choice, the severity of the Snare is reduced; if coerced, the Snare classification is strengthened.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(child_marriage, 0, 10).

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
narrative_ontology:measurement(child_marriage_tr_t0, child_marriage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(child_marriage_tr_t5, child_marriage, theater_ratio, 5, 0.15).
narrative_ontology:measurement(child_marriage_tr_t10, child_marriage, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(child_marriage_ex_t0, child_marriage, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(child_marriage_ex_t5, child_marriage, base_extractiveness, 5, 0.73).
narrative_ontology:measurement(child_marriage_ex_t10, child_marriage, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */