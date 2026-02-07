% ============================================================================
% CONSTRAINT STORY: israel_gaza_ceasefire_violation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_israel_gaza_ceasefire_violation, []).

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
 * * constraint_id: israel_gaza_ceasefire_violation
 * human_readable: Israel-Gaza Ceasefire Violation
 * domain: political
 * * SUMMARY:
 * Following a ceasefire agreement, Hamas's violation of the agreement by launching rockets into Israel triggers retaliatory strikes from Israel on Gaza. This cycle represents a constraint on sustainable peace, defined by the agreement's perceived utility and enforceability for each party.
 * * KEY AGENTS:
 * - Gazan Civilians: Subject (Powerless)
 * - Israeli Government: Beneficiary (Institutional)
 * - International Observers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(israel_gaza_ceasefire_violation, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(israel_gaza_ceasefire_violation, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(israel_gaza_ceasefire_violation, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, extractiveness, 0.6).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, theater_ratio, 0.2).

% Binary flags
domain_priors:requires_active_enforcement(israel_gaza_ceasefire_violation). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(israel_gaza_ceasefire_violation, israeli_citizens).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, gazan_civilians).

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
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_gaza_ceasefire_violation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(israel_gaza_ceasefire_violation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ceasefire agreement is a high-extraction constraint because it requires significant concessions from both sides and the violation leads to immediate detrimental consequences (retaliatory strikes). The suppression score is high due to the limited alternative options available to Gazan civilians and Hamas, given the blockade and ongoing conflict. The perspectival gap emerges because the Israeli government, with its institutional power, views the ceasefire as a necessary, albeit fragile, rope for managing the conflict and protecting its citizens. On the other hand, Gazan civilians, trapped in a cycle of violence and limited resources, experience the ceasefire (and its violation) as a snare.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling coordination as pure extraction by recognizing that the ceasefire agreement is intended to reduce violence and provide a framework for future negotiations (coordination function), even though it entails asymmetric extraction (disproportionate impact on Gazan civilians due to violations and subsequent Israeli responses). The active enforcement requirement underscores the need for external monitoring and pressure to maintain the agreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israel_gaza,
    'Is the ceasefire violation a strategic decision by Hamas, or a result of splinter group actions?',
    'Intelligence gathering and analysis of Hamas communication.',
    'If strategic, the ceasefire is less likely to hold. If splinter groups, greater external pressure on Hamas might be effective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(israel_gaza_ceasefire_violation, 0, 10).

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
narrative_ontology:measurement(israel_gaza_ceasefire_violation_tr_t0, israel_gaza_ceasefire_violation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(israel_gaza_ceasefire_violation_tr_t5, israel_gaza_ceasefire_violation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(israel_gaza_ceasefire_violation_tr_t10, israel_gaza_ceasefire_violation, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(israel_gaza_ceasefire_violation_ex_t0, israel_gaza_ceasefire_violation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(israel_gaza_ceasefire_violation_ex_t5, israel_gaza_ceasefire_violation, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(israel_gaza_ceasefire_violation_ex_t10, israel_gaza_ceasefire_violation, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */