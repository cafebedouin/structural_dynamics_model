% ============================================================================
% CONSTRAINT STORY: perseverance_rover_autonomy
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_perseverance_rover_autonomy, []).

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
 * * constraint_id: perseverance_rover_autonomy
 * human_readable: Perseverance Rover Autonomy
 * domain: technological
 * * SUMMARY:
 * The Perseverance rover achieving autonomous exploration on Mars presents a constraint on human control. This is a case of distributed control where some decision-making is delegated to an AI agent on Mars, limiting the immediate intervention capabilities of Earth-based scientists.
 * * KEY AGENTS:
 * - The Rover: Subject (Powerless) - acting on its programming
 * - NASA Scientists: Beneficiary (Institutional) - enabling more efficient exploration
 * - Mars Environment: Auditor (Analytical) - the context in which the autonomy is deployed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(perseverance_rover_autonomy, 0.25). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(perseverance_rover_autonomy, 0.15).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(perseverance_rover_autonomy, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(perseverance_rover_autonomy, extractiveness, 0.25).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, theater_ratio, 0.05).

% Binary flags
% narrative_ontology:has_sunset_clause(perseverance_rover_autonomy).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(perseverance_rover_autonomy). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, nasa_scientists).
% narrative_ontology:constraint_victim(perseverance_rover_autonomy, [victim_group]).

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
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(perseverance_rover_autonomy, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(perseverance_rover_autonomy).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(perseverance_rover_autonomy, piton,
%    context(agent_power(analytical),
%            time_horizon(civilizational),
%            exit_options(arbitrage),
%            spatial_scope(universal))) :-
%    domain_priors:theater_ratio(perseverance_rover_autonomy, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_rover_autonomy_tests).

test(perspectival_consistency) :-
    % Verify there is no perspectival gap because this is rope-only
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless = TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(perseverance_rover_autonomy, ExtMetricName, E),
    E =< 0.15. % Ensures it's a low-extraction Rope.

:- end_tests(perseverance_rover_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perseverance rover's autonomy is classified as a Rope because it facilitates exploration and data collection, despite limiting immediate human control. The base extractiveness is low (0.25) as the primary function is enabling, not extracting, resources or value *from* other actors. The suppression score is also low (0.15) because while it reduces direct control, it opens up greater possibilities for data collection. There is perspectival consistency as even the rover (considered "powerless" as acting on its programming) benefits from the arrangement, as it is enabled to explore more efficiently. This does not qualify as a Tangled Rope because while the scientists benefit, there isn't a clear asymmetric extraction of resources. The Mars environment doesn't qualify as a victim because the rover is collecting data that enhances human knowledge with limited alteration of the environment. It is *not* a Snare as the alternative (humans controlling every move from Earth) would result in considerably less data collection and scientific progress.
 * * MANDATROPHY ANALYSIS:
 * The Rope classification prevents mislabeling this as pure extraction because it considers the coordination benefits (more efficient exploration) alongside the limited reduction in direct control.  It acknowledges that the autonomous system, while imposing a constraint on direct intervention, primarily functions to enable more efficient data collection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_perseverance_rover_autonomy,
    'Will the autonomous decision-making of the rover lead to unintended environmental consequences or ethical dilemmas?',
    'Long-term monitoring of the rover's environmental impact and ongoing ethical analysis of its decision-making processes.',
    'If true: Rover autonomy presents environmental/ethical challenges, requiring stricter controls or limitations. If false: Rover autonomy expands scientific progress with minimal negative consequences.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(perseverance_rover_autonomy, 0, 10).

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
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t0, perseverance_rover_autonomy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t5, perseverance_rover_autonomy, theater_ratio, 5, 0.05).
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t10, perseverance_rover_autonomy, theater_ratio, 10, 0.05).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t0, perseverance_rover_autonomy, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t5, perseverance_rover_autonomy, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t10, perseverance_rover_autonomy, base_extractiveness, 10, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */