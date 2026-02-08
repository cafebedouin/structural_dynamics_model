% ============================================================================
% CONSTRAINT STORY: trump_critical_minerals
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_trump_critical_minerals, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trump_critical_minerals
 * human_readable: Trump Critical Minerals Stockpile Project
 * domain: economic/political
 * * SUMMARY:
 * The Trump administration initiated a project to stockpile critical minerals, aiming to reduce reliance on foreign suppliers, particularly China. This involves government subsidies and potential regulatory changes to incentivize domestic mining and processing. This creates a constraint on market access and resource control.
 * * KEY AGENTS:
 * - Domestic Miners: Subject (Powerless/Constrained initially, potentially Powerful/Mobile with subsidies)
 * - Foreign Suppliers (China): Victim (Powerless)
 * - US Government: Beneficiary (Institutional/Powerful)
 * - Environmental groups: Victim (Powerless)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trump_critical_minerals, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(trump_critical_minerals, 0.55).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(trump_critical_minerals, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(trump_critical_minerals, extractiveness, 0.65).
narrative_ontology:constraint_metric(trump_critical_minerals, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(trump_critical_minerals, theater_ratio, 0.2).

% Constraint classification claim
narrative_ontology:constraint_claim(trump_critical_minerals, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(trump_critical_minerals). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(trump_critical_minerals, us_government).
narrative_ontology:constraint_victim(trump_critical_minerals, foreign_suppliers).
narrative_ontology:constraint_victim(trump_critical_minerals, environmental_groups).

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
constraint_indexing:constraint_classification(trump_critical_minerals, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(trump_critical_minerals, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_critical_minerals_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(trump_critical_minerals, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_critical_minerals, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trump_critical_minerals, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(trump_critical_minerals_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Subject (domestic miners initially) perceives a Snare because they are constrained by the existing reliance on foreign suppliers and lack the capital to compete. The Beneficiary (US Government) views it as a Rope, coordinating resources to ensure national security. The Analytical Observer sees a Tangled Rope: there's a coordination function (national security), but it involves asymmetric extraction (disadvantaging foreign suppliers and, potentially, domestic consumers through higher prices).
 * The high base extractiveness and suppression reflect the government intervention in the market and the limitations placed on alternative supply sources.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction by acknowledging the legitimate coordination function of national security. It avoids a pure "Snare" label that would ignore this aspect, while still capturing the extractive effects on other actors.  The US government derives benefit at the cost of other actors, especially China.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trump_critical_minerals,
    'Is the long-term economic benefit to the US greater than the cost imposed on foreign suppliers and the environmental impact of domestic mining?',
    'Comprehensive cost-benefit analysis including environmental externalities and geopolitical consequences.',
    'If True: Justified national security measure. If False: Rent-seeking and economic inefficiency.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_critical_minerals, 0, 10).

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
narrative_ontology:measurement(trump_critical_minerals_tr_t0, trump_critical_minerals, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trump_critical_minerals_tr_t5, trump_critical_minerals, theater_ratio, 5, 0.2).
narrative_ontology:measurement(trump_critical_minerals_tr_t10, trump_critical_minerals, theater_ratio, 10, 0.3).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(trump_critical_minerals_ex_t0, trump_critical_minerals, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(trump_critical_minerals_ex_t5, trump_critical_minerals, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(trump_critical_minerals_ex_t10, trump_critical_minerals, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */