% ============================================================================
% CONSTRAINT STORY: microwave_weapon_1
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_microwave_weapon_1, []).

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
 * * constraint_id: microwave_weapon_1
 * human_readable: Chinese Anti-Satellite Microwave Weapon
 * domain: technological/political
 * * SUMMARY:
 * China has developed a compact microwave weapon capable of disabling satellites. This technology poses a threat to other nations' space assets and alters the balance of power in space. The constraint involves the potential for coercion and control over orbital infrastructure.
 * * KEY AGENTS:
 * - Other Nations: Subject (Powerless)
 * - China: Beneficiary (Institutional)
 * - Analytical Observer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(microwave_weapon_1, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(microwave_weapon_1, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(microwave_weapon_1, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(microwave_weapon_1, extractiveness, 0.6).
narrative_ontology:constraint_metric(microwave_weapon_1, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(microwave_weapon_1, theater_ratio, 0.2).

% Binary flags
% narrative_ontology:has_sunset_clause(microwave_weapon_1).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(microwave_weapon_1). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(microwave_weapon_1, china).
narrative_ontology:constraint_victim(microwave_weapon_1, other_nations).

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
constraint_indexing:constraint_classification(microwave_weapon_1, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(microwave_weapon_1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(microwave_weapon_1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(microwave_weapon_1, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(microwave_weapon_1).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(microwave_weapon_1, piton, 
%     context(agent_power(analytical), 
%             time_horizon(civilizational), 
%             exit_options(arbitrage), 
%             spatial_scope(universal))) :-
%     domain_priors:theater_ratio(microwave_weapon_1, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microwave_weapon_1_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(microwave_weapon_1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microwave_weapon_1, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(microwave_weapon_1, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(microwave_weapon_1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Subject (Other Nations) perceives this as a Snare because they are relatively powerless to stop its development and deployment, and are trapped in a situation where their satellites are vulnerable. China sees this as a Rope, a tool for national security and potentially for establishing norms in space.
 *
 * The Analytical Observer sees this as a Tangled Rope. There is a coordination aspect: establishing some kind of order (albeit coercive) in space. There is also asymmetric extraction: China gains power and influence at the expense of other nations' security. Active enforcement is also required.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling this as pure extraction (Snare) because it acknowledges the potential coordination aspect, such as establishing rules for space activities, even if those rules are enforced through coercion. It also prevents misclassification as pure coordination by explicitly capturing the asymmetric distribution of power and the potential for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_microwave_weapon_1,
    'Will this technology be used primarily for defense or offense?',
    'Historical precedent and strategic doctrine analysis.',
    'If defense, it reinforces a regional power balance; if offense, it creates a global instability.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(microwave_weapon_1, 0, 10).

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
narrative_ontology:measurement(microwave_weapon_1_tr_t0, microwave_weapon_1, theater_ratio, 0, 0.1).
narrative_ontology:measurement(microwave_weapon_1_tr_t5, microwave_weapon_1, theater_ratio, 5, 0.2).
narrative_ontology:measurement(microwave_weapon_1_tr_t10, microwave_weapon_1, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(microwave_weapon_1_ex_t0, microwave_weapon_1, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(microwave_weapon_1_ex_t5, microwave_weapon_1, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(microwave_weapon_1_ex_t10, microwave_weapon_1, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */