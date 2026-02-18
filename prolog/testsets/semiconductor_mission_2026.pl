% ============================================================================
% CONSTRAINT STORY: semiconductor_mission_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-08
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
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(semiconductor_mission_2026, tangled_rope).
narrative_ontology:human_readable(semiconductor_mission_2026, "Indian Semiconductor Mission 2.0").
narrative_ontology:topic_domain(semiconductor_mission_2026, "economic").

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
% Domestic manufacturers face high compliance burdens and dependency, feeling trapped.
constraint_indexing:constraint_classification(semiconductor_mission_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The government views this as essential coordination for national strategic goals.
constraint_indexing:constraint_classification(semiconductor_mission_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The policy has both a genuine coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(semiconductor_mission_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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

test(tangled_rope_properties) :-
    % A tangled_rope classification requires these three structural properties.
    narrative_ontology:constraint_beneficiary(semiconductor_mission_2026, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(semiconductor_mission_2026, _), % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(semiconductor_mission_2026).

:- end_tests(semiconductor_mission_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * I assigned a base extractiveness of 0.48 because the mission extracts resources from taxpayers to subsidize the industry. The suppression score is 0.55 because the mission limits the choices of domestic manufacturers to use only government approved programs, and may indirectly disadvantage non-participating foreign suppliers. The theater ratio is low (0.30) because the mission seems genuinely aimed at building capacity, not just performative. The perspectival gap arises because domestic manufacturers may perceive it as a necessary intervention, but they also face limitations and compliance burdens (Snare), whereas the government views it as a strategic investment (Rope). Global chip suppliers, initially beneficiaries of the Indian market, now find their access curtailed, making them victims of asymmetric extraction. The analytical observer classifies it as Tangled Rope due to its mix of coordination (domestic capacity building) and extraction (from global suppliers and taxpayers).
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
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(semiconductor_mission_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */