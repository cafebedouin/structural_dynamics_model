% ============================================================================
% CONSTRAINT STORY: canal_panama_influence
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_canal_panama_influence, []).

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
 * * constraint_id: canal_panama_influence
 *   human_readable: Geopolitical Influence over Panama Canal
 *   domain: political
 * * SUMMARY:
 *   The Panama Canal, a crucial global trade route, is subject to a geopolitical struggle for influence between the US and China, with Swiss companies playing a key role. The constraint involves the exertion of influence to control and benefit from the canal's operations and strategic value.
 * * KEY AGENTS:
 * - Panama: Subject (Powerless)
 * - China/US: Beneficiary (Institutional)
 * - Global Trade System: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(canal_panama_influence, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(canal_panama_influence, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(canal_panama_influence, 0.3).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(canal_panama_influence, extractiveness, 0.6).
narrative_ontology:constraint_metric(canal_panama_influence, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(canal_panama_influence, theater_ratio, 0.3).

% Binary flags
% narrative_ontology:has_sunset_clause(canal_panama_influence).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(canal_panama_influence). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(canal_panama_influence, [china, usa]).
narrative_ontology:constraint_victim(canal_panama_influence, panama).

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
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(canal_panama_influence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canal_panama_influence_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(canal_panama_influence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canal_panama_influence, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(canal_panama_influence, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(canal_panama_influence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Panama Canal is viewed as a Snare by Panama due to the pressure and extraction from larger powers. The US and China perceive it as a Rope because it facilitates global trade and benefits their economies. From an analytical perspective, it is a Tangled Rope because it provides a coordination function for global trade, but simultaneously allows for asymmetric extraction of benefits by powerful nations, especially via influence over canal administration and related infrastructure. The suppression score is high because Panama's agency is significantly constrained in this dynamic.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction (Snare) because it acknowledges the coordination function of the canal for global trade. It also acknowledges that this coordination comes with an inherent level of coercion and influence being exerted on Panama, preventing a simple Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_canal_panama_influence,
    'Will Panama be able to diversify its economic dependence on the canal and assert greater sovereign control?',
    'Future economic diversification policies and diplomatic efforts by Panama.',
    'If True: Reduced asymmetric extraction; Canal as a Rope. If False: Continued Snare dynamic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(canal_panama_influence, 0, 10).

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
narrative_ontology:measurement(canal_panama_influence_tr_t0, canal_panama_influence, theater_ratio, 0, 0.2).
narrative_ontology:measurement(canal_panama_influence_tr_t5, canal_panama_influence, theater_ratio, 5, 0.3).
narrative_ontology:measurement(canal_panama_influence_tr_t10, canal_panama_influence, theater_ratio, 10, 0.3).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(canal_panama_influence_ex_t0, canal_panama_influence, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(canal_panama_influence_ex_t5, canal_panama_influence, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(canal_panama_influence_ex_t10, canal_panama_influence, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */