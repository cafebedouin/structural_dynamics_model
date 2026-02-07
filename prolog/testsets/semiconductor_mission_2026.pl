% ============================================================================
% CONSTRAINT STORY: semiconductor_mission_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_semiconductor_mission_2026, []).

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
 * * constraint_id: semiconductor_mission_2026
 * human_readable: Indian Semiconductor Mission 2.0
 * domain: economic
 * * SUMMARY:
 * The Indian government's Semiconductor Mission 2.0 aims to foster domestic semiconductor manufacturing capabilities by providing financial incentives and infrastructure support. It expands the scope of the initial mission to include equipment, materials, and intellectual property, intending to reduce reliance on foreign chip suppliers and create a self-sufficient ecosystem.
 * * KEY AGENTS:
 * - Domestic Chip Manufacturers: Subject (Powerless)
 * - Government (Ministry of Electronics and IT): Beneficiary (Institutional)
 * - Global Tech Industry/Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(semiconductor_mission_2026, 0.48). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(semiconductor_mission_2026, 0.55).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(semiconductor_mission_2026, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(semiconductor_mission_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(semiconductor_mission_2026, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(semiconductor_mission_2026, theater_ratio, 0.30).

% Binary flags
% narrative_ontology:has_sunset_clause(semiconductor_mission_2026).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(semiconductor_mission_2026). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(semiconductor_mission_2026, indian_economy).
narrative_ontology:constraint_victim(semiconductor_mission_2026, global_chip_suppliers).

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
constraint_indexing:constraint_classification(semiconductor_mission_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(semiconductor_mission_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(semiconductor_mission_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(semiconductor_mission_2026, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(semiconductor_mission_2026).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(semiconductor_mission_2026, piton,
%    context(agent_power(analytical),
%            time_horizon(civilizational),
%            exit_options(arbitrage),
%            spatial_scope(universal))) :-
%    domain_priors:theater_ratio(semiconductor_mission_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_mission_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(semiconductor_mission_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_mission_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(semiconductor_mission_2026, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(semiconductor_mission_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * I assigned a base extractiveness of 0.48 because the mission does extract resources from taxpayers to subsidize the industry. The suppression score is 0.55 because the mission limits the choices of domestic manufacturers to use only government approved programs, and may indirectly disadvantage non-participating foreign suppliers. The theater ratio is low (0.30) because the mission seems genuinely aimed at building capacity, not just performative. The perspectival gap arises because domestic manufacturers may perceive it as a necessary intervention, but they also face limitations and compliance burdens (Snare), whereas the government views it as a strategic investment (Rope). Global chip suppliers, initially beneficiaries of the Indian market, now find their access curtailed, making them victims of asymmetric extraction. The analytical observer classifies it as Tangled Rope due to its mix of coordination (domestic capacity building) and extraction (from global suppliers and taxpayers).
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling coordination as pure extraction by explicitly acknowledging both the coordination function (building domestic semiconductor capacity) and the asymmetric extraction (potentially harming global chip suppliers and burdening taxpayers). Without considering both aspects, the system might incorrectly classify it as a simple Rope (pure coordination) or a Snare (pure extraction). The 'requires_active_enforcement' flag indicates that the mission needs continued government intervention to maintain its goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_semiconductor_mission_2026,
    'Will the mission foster genuine innovation or merely create dependence on government subsidies?',
    'Longitudinal analysis of patent filings and technological advancements by participating companies.',
    'If true, strengthens domestic industry; if false, wastes taxpayer money and stifles competition.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(semiconductor_mission_2026, 0, 10).

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
narrative_ontology:measurement(semiconductor_mission_2026_tr_t0, semiconductor_mission_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(semiconductor_mission_2026_tr_t5, semiconductor_mission_2026, theater_ratio, 5, 0.30).
narrative_ontology:measurement(semiconductor_mission_2026_tr_t10, semiconductor_mission_2026, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(semiconductor_mission_2026_ex_t0, semiconductor_mission_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(semiconductor_mission_2026_ex_t5, semiconductor_mission_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(semiconductor_mission_2026_ex_t10, semiconductor_mission_2026, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */