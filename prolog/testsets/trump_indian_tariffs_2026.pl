% ============================================================================
% CONSTRAINT STORY: trump_indian_tariffs_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_trump_indian_tariffs_2026, []).

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
 * * constraint_id: trump_indian_tariffs_2026
 * human_readable: Trump's Tariff Reduction on India (2026)
 * domain: economic
 * * SUMMARY:
 * In 2026, Trump reduces tariffs on Indian goods to 18% after Modi agrees to reduce oil purchases from Russia. This is a quid-pro-quo arrangement where economic concessions are exchanged for political alignment, seemingly benefiting both countries at the expense of Russia.
 * * KEY AGENTS:
 * - Indian Exporters: Subject (Powerless initially, potentially Organized)
 * - US Consumers/Importers: Beneficiary (Institutional)
 * - Russia: Victim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trump_indian_tariffs_2026, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(trump_indian_tariffs_2026, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(trump_indian_tariffs_2026, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, theater_ratio, 0.20).

% Binary flags
% narrative_ontology:has_sunset_clause(trump_indian_tariffs_2026).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(trump_indian_tariffs_2026). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, "US Consumers/Importers").
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, "Russia").

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
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(trump_indian_tariffs_2026, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(trump_indian_tariffs_2026).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(trump_indian_tariffs_2026, piton,
%     context(agent_power(analytical),
%             time_horizon(civilizational),
%             exit_options(arbitrage),
%             spatial_scope(universal))) :-
%     domain_priors:theater_ratio(trump_indian_tariffs_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_indian_tariffs_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trump_indian_tariffs_2026, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(trump_indian_tariffs_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores are assigned as follows:
 *  - extractiveness: 0.55. The tariffs are a form of economic extraction for the US, and the reduction only partially mitigates that. This is a fairly high level.
 *  - suppression: 0.70. India is pressured into changing its foreign policy, limiting its options for oil sourcing. Russia faces limited options for selling oil.
 *  - theater_ratio: 0.20. There is a real change in policy and trade flows, so relatively low theater.
 *
 * The perspectival gap exists because Indian exporters initially see the tariffs as a snare (limiting their market access). However, from the US perspective (consumers/importers), the reduced tariffs offer a rope of benefit by offering cheaper imported goods. The analytical observer sees it as a Tangled Rope because it facilitates US economic coordination while extracting from Russia through geopolitical pressure.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification ensures that the system doesn't mislabel this as pure extraction (Snare). The benefit to US consumers and businesses represents a coordination function that offsets the extraction from Russia. The classification takes into account that active enforcement (of the implicit agreement to limit Russian oil purchases) is required to maintain the agreement, further supporting the Tangled Rope classification. If India decided to renege on its implicit agreement with the US, the benefit to US consumers and businesses from lower Indian tariffs would stop.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trump_indian_tariffs_2026,
    'Will India abide by the implicit agreement to reduce Russian oil purchases?',
    'Monitor Indian oil import data from Russia over time.',
    'If True: US benefits economically and geopolitically, Russia is weakened. If False: The US gains only limited economic benefits, agreement collapses.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_indian_tariffs_2026, 0, 10).

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
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t0, trump_indian_tariffs_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t5, trump_indian_tariffs_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t10, trump_indian_tariffs_2026, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t0, trump_indian_tariffs_2026, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t5, trump_indian_tariffs_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t10, trump_indian_tariffs_2026, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */