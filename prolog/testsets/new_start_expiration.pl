% ============================================================================
% CONSTRAINT STORY: new_start_expiration
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_new_start_expiration, []).

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
 * * constraint_id: new_start_expiration
 * human_readable: New START Treaty Expiration
 * domain: political
 * * SUMMARY:
 * The New START treaty, limiting US and Russian nuclear arsenals, has expired. This removes a formal constraint on the number of deployed nuclear warheads and delivery systems each country can possess, increasing the potential for a renewed arms race.
 * * KEY AGENTS:
 * - US/Russia (Nuclear Arsenal Limit): Subject (Powerless)
 * - US/Russia (Nuclear Arms Industry): Beneficiary (Institutional)
 * - Arms Control Community (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(new_start_expiration, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(new_start_expiration, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(new_start_expiration, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(new_start_expiration, extractiveness, 0.6).
narrative_ontology:constraint_metric(new_start_expiration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(new_start_expiration, theater_ratio, 0.2).

% Binary flags
% narrative_ontology:has_sunset_clause(new_start_expiration).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(new_start_expiration). % Required for Tangled Rope
domain_priors:requires_active_enforcement(new_start_expiration). % Required because of the suppression and extraction scores.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(new_start_expiration, nuclear_arms_industry).
narrative_ontology:constraint_victim(new_start_expiration, global_security).

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
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(new_start_expiration, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(new_start_expiration).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(new_start_expiration, piton, 
%     context(agent_power(analytical), 
%             time_horizon(civilizational), 
%             exit_options(arbitrage), 
%             spatial_scope(universal))) :-
%     domain_priors:theater_ratio(new_start_expiration, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_start_expiration_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(new_start_expiration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_start_expiration, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(new_start_expiration, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(new_start_expiration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The expiration of the New START treaty is classified differently based on perspective. To global security, it's a Snare, removing constraints and increasing the risk of nuclear conflict, because of increased potential and less verification requirements. To the arms industry, it's a Rope, allowing for increased profits. Analytically, it's a Tangled Rope, with both coordination (limited arms race during the treaty) and extraction (profits for the arms industry). The base extractiveness is set to 0.6, and suppression to 0.7, as the treaty's expiration removes significant, but not total, limitations.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification captures both the coordination (arms limitation) and extraction (arms industry profits) aspects of the treaty's expiration. Without it, the system might mislabel the situation as purely extractiveness (Snare) or pure coordination (Rope), missing the complex interplay of benefits and drawbacks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_new_start_expiration,
    'Will the expiration of New START lead to a renewed arms race?',
    'Monitoring nuclear weapons development and deployment by the US and Russia.',
    'Increased global instability and risk of nuclear conflict if true; maintained stability if false.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(new_start_expiration, 0, 10).

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
narrative_ontology:measurement(new_start_expiration_tr_t0, new_start_expiration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(new_start_expiration_tr_t5, new_start_expiration, theater_ratio, 5, 0.3).
narrative_ontology:measurement(new_start_expiration_tr_t10, new_start_expiration, theater_ratio, 10, 0.4).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(new_start_expiration_ex_t0, new_start_expiration, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(new_start_expiration_ex_t5, new_start_expiration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(new_start_expiration_ex_t10, new_start_expiration, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */