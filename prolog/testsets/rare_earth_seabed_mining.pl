% ============================================================================
% CONSTRAINT STORY: rare_earth_seabed_mining
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_rare_earth_seabed_mining, []).

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
 * * constraint_id: rare_earth_seabed_mining
 * human_readable: Deep-Sea Rare Earth Mining
 * domain: economic/technological/environmental
 * * SUMMARY:
 * Japan retrieves rare-earth-bearing seabed sediment in a deep-sea test near Minamitorishima Island. This represents a constraint due to the potential extraction of resources, environmental impact, and the international legal and geopolitical implications of deep-sea mining. The constraint involves balancing resource acquisition with environmental protection and international norms.
 * * KEY AGENTS:
 * - Marine Ecosystem: Subject (Powerless)
 * - Japan/Mining Companies: Beneficiary (Institutional)
 * - International Community/Scientists: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rare_earth_seabed_mining, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(rare_earth_seabed_mining, 0.45).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(rare_earth_seabed_mining, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, 0.65).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, theater_ratio, 0.2).

% Binary flags
% narrative_ontology:has_sunset_clause(rare_earth_seabed_mining).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(rare_earth_seabed_mining). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, japan).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, marine_ecosystem).

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
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :- domain_priors:requires_active_enforcement(rare_earth_seabed_mining).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(rare_earth_seabed_mining, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(rare_earth_seabed_mining).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(rare_earth_seabed_mining, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_seabed_mining_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rare_earth_seabed_mining, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(rare_earth_seabed_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.65 because of the direct extraction of rare earth elements from the seabed. The suppression score is 0.45 reflecting the suppression of alternative, potentially less environmentally damaging sourcing methods, and the limited voice of the marine ecosystem in the decision-making process. The theater ratio is low (0.2) as the activity is functionally focused on resource extraction, with limited symbolic or performative aspects.
 * The perspectival gap arises because the marine ecosystem (powerless) experiences the mining as a Snare, a potentially destructive and inescapable trap. Japan (institutional) views it as a Rope, an essential means to secure resources. The analytical observer sees it as a Tangled Rope, a mix of resource extraction and potential coordination (e.g., through regulations and international agreements).
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction (Snare) because it acknowledges the coordination function. The mining activity is governed by regulations and international agreements. The existence of a beneficiary (Japan) and a victim (marine ecosystem) in addition to the need for active enforcement are all properties confirmed by the classifier. The analytical observer recognizes that seabed mining requires regulations and oversight to mitigate environmental damage, making it a Tangled Rope rather than a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rare_earth_seabed_mining,
    'What is the long-term impact of seabed mining on the marine ecosystem?',
    'Long-term ecological studies and monitoring of mined areas',
    'If high negative impact: reclassification as Snare; If minimal impact: reclassification as Rope or Tangled Rope with lower extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rare_earth_seabed_mining, 0, 10).

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
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t0, rare_earth_seabed_mining, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t5, rare_earth_seabed_mining, theater_ratio, 5, 0.2).
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t10, rare_earth_seabed_mining, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t0, rare_earth_seabed_mining, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t5, rare_earth_seabed_mining, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t10, rare_earth_seabed_mining, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */