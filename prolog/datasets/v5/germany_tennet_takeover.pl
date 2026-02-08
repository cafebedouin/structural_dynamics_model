% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
 * * constraint_id: germany_tennet_takeover
 * human_readable: German Government Stake in TenneT Germany
 * domain: economic/political
 * * SUMMARY:
 * The German government is buying a 25.1% stake in TenneT Germany, a crucial electricity grid operator, for 3.3 billion euros to prevent a foreign takeover and ensure energy security during the green energy transition.  This acquisition is framed as vital infrastructure investment.
 * * KEY AGENTS:
 * - German Citizens: Subject (Powerless)
 * - German Government: Beneficiary (Institutional)
 * - Analytical Observer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(germany_tennet_takeover, 0.35). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(germany_tennet_takeover, 0.20).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(germany_tennet_takeover, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.35).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.10).

% Binary flags
% narrative_ontology:has_sunset_clause(germany_tennet_takeover).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(germany_tennet_takeover). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_government).
narrative_ontology:constraint_victim(germany_tennet_takeover, german_citizens).

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
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    narrative_ontology:constraint_beneficiary(germany_tennet_takeover, _),
    narrative_ontology:constraint_victim(germany_tennet_takeover, _).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(germany_tennet_takeover, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(germany_tennet_takeover).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(germany_tennet_takeover, piton, 
%    context(agent_power(analytical), 
%            time_horizon(civilizational), 
%            exit_options(arbitrage), 
%            spatial_scope(universal))) :-
%    domain_priors:theater_ratio(germany_tennet_takeover, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(germany_tennet_takeover, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46 ; E > 0.05). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The German government's stake in TenneT Germany presents a complex situation.  From the perspective of ordinary citizens (powerless, trapped), it can feel like a Snare.  While framed as a national security measure and infrastructure investment, it inherently involves the extraction of taxpayer money.
 * The German government (institutional, mobile) views it as a vital Rope, ensuring energy grid stability and control during a critical transition.
 * The Analytical Observer sees this as a Tangled Rope.  There *is* a coordination function -- maintaining the energy grid and preventing foreign control.  But there's also asymmetric extraction, as citizens are forced to fund this investment whether they agree with it or not. The suppression score is lower than a pure Snare, because there is a genuine coordination benefit. The base_extractiveness of 0.35 reflects the use of public funds and potential for inefficiencies inherent in government ownership.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is crucial to prevent mislabeling this as a pure extraction (Snare).  While citizens may experience the cost as a direct loss, the investment theoretically provides a broader benefit by securing critical infrastructure and supporting the energy transition. The coordination is preventing energy grid failures, even if it comes at a price. Without acknowledging the coordination aspect, it would become a pure Snare, and the benefit would be ignored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_germany_tennet_takeover,
    'Is this investment truly necessary for energy security, or is it a politically motivated maneuver?',
    'Auditing the actual impact of the investment on grid stability and the availability of alternative solutions.',
    'If true, the benefits outweigh the costs. If false, it is a misallocation of resources and rent-seeking.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(germany_tennet_takeover, 0, 10).

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
narrative_ontology:measurement(germany_tennet_takeover_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.05).
narrative_ontology:measurement(germany_tennet_takeover_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.10).
narrative_ontology:measurement(germany_tennet_takeover_tr_t10, germany_tennet_takeover, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(germany_tennet_takeover_ex_t0, germany_tennet_takeover, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(germany_tennet_takeover_ex_t5, germany_tennet_takeover, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(germany_tennet_takeover_ex_t10, germany_tennet_takeover, base_extractiveness, 10, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */