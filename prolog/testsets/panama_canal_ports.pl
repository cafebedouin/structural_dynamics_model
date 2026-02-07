% ============================================================================
% CONSTRAINT STORY: panama_canal_ports
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_panama_canal_ports, []).

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
 * * constraint_id: panama_canal_ports
 * human_readable: Panama Canal Port Control
 * domain: political/economic
 * * SUMMARY:
 * The US and China are vying for control of ports along the Panama Canal. This competition creates a strategic chokepoint with implications for global trade and security, potentially granting undue influence to the controlling party. The situation presents a complex interplay of economic interests and geopolitical power.
 * * KEY AGENTS:
 * - Panama: Subject (Powerless)
 * - China/US: Beneficiary (Institutional)
 * - Global Trade: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(panama_canal_ports, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(panama_canal_ports, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(panama_canal_ports, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(panama_canal_ports, extractiveness, 0.65).
narrative_ontology:constraint_metric(panama_canal_ports, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(panama_canal_ports, theater_ratio, 0.20).

% Binary flags
% narrative_ontology:has_sunset_clause(panama_canal_ports).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(panama_canal_ports). % Required for Tangled Rope
domain_priors:requires_active_enforcement(panama_canal_ports). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(panama_canal_ports, china_us).
narrative_ontology:constraint_victim(panama_canal_ports, panama).

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
constraint_indexing:constraint_classification(panama_canal_ports, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(panama_canal_ports, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(panama_canal_ports, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(panama_canal_ports, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(panama_canal_ports).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(panama_canal_ports, piton, 
%    context(agent_power(analytical), 
%            time_horizon(civilizational), 
%            exit_options(arbitrage), 
%            spatial_scope(universal))) :-
%    domain_priors:theater_ratio(panama_canal_ports, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(panama_canal_ports_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(panama_canal_ports, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(panama_canal_ports, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(panama_canal_ports, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(panama_canal_ports_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Panama Canal port control is classified as a Snare from Panama's perspective (powerless, trapped), as they perceive limited alternatives and high extraction.  China/US (institutional) view it as a Rope, facilitating global trade (coordination). The Analytical Observer sees it as a Tangled Rope because there is genuine coordination for international trade, but also asymmetric extraction and potential for geopolitical coercion against Panama and other nations. Active enforcement is required to maintain the status quo and suppress competing interests. The perspectival gap arises from the vastly different power dynamics between the involved parties.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling coordination as pure extraction by acknowledging the coordination benefits for global trade while simultaneously highlighting the asymmetric extraction and potential coercion experienced by Panama. This distinguishes it from a pure "Rope" scenario where benefits are more evenly distributed. The need for active enforcement distinguishes it from a purely voluntary coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_panama_canal_ports,
    'Will Panama retain genuine sovereignty over its canal ports, or will foreign powers exert undue influence?',
    'Longitudinal monitoring of contractual agreements, enforcement mechanisms, and political lobbying efforts.',
    'True: Panama maintains control, promoting fair trade. False: Foreign powers dominate, leading to economic exploitation and geopolitical instability.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(panama_canal_ports, 0, 10).

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
narrative_ontology:measurement(panama_canal_ports_tr_t0, panama_canal_ports, theater_ratio, 0, 0.10).
narrative_ontology:measurement(panama_canal_ports_tr_t5, panama_canal_ports, theater_ratio, 5, 0.15).
narrative_ontology:measurement(panama_canal_ports_tr_t10, panama_canal_ports, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(panama_canal_ports_ex_t0, panama_canal_ports, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(panama_canal_ports_ex_t5, panama_canal_ports, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(panama_canal_ports_ex_t10, panama_canal_ports, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */