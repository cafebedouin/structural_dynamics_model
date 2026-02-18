% ============================================================================
% CONSTRAINT STORY: panama_canal_ports
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
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
 * * constraint_id: panama_canal_ports
 * human_readable: Panama Canal Port Control
 * domain: political/economic
 * * SUMMARY:
 * The US and China are vying for control of ports along the Panama Canal. This competition creates a strategic chokepoint with implications for global trade and security, granting undue influence to the controlling party. The situation presents a complex interplay of economic interests and geopolitical power.
 * * KEY AGENTS:
 * - Panama: Subject (Powerless)
 * - China/US: Beneficiary (Institutional)
 * - Global Trade Auditors: Auditor (Analytical)
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

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(panama_canal_ports, tangled_rope).
narrative_ontology:human_readable(panama_canal_ports, "Panama Canal Port Control").
narrative_ontology:topic_domain(panama_canal_ports, "political/economic").

% Binary flags
% narrative_ontology:has_sunset_clause(panama_canal_ports).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(panama_canal_ports). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(panama_canal_ports, global_powers).
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
% This constraint is not a scaffold; it has no sunset clause and is not intended
% to be temporary.
% constraint_indexing:constraint_classification(panama_canal_ports, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(panama_canal_ports).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% This constraint is not a piton. It is actively functional and enforced, not
% inertial or theatrical. The theater_ratio is low (0.20), well below the
% piton threshold of 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(panama_canal_ports_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(panama_canal_ports, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(panama_canal_ports, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation_is_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(panama_canal_ports, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction Snare/Tangled Rope.

test(tangled_rope_conditions_met) :-
    % A tangled_rope classification requires these three structural properties.
    domain_priors:requires_active_enforcement(panama_canal_ports),
    narrative_ontology:constraint_beneficiary(panama_canal_ports, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(panama_canal_ports, _). % derives has_asymmetric_extraction

:- end_tests(panama_canal_ports_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Panama Canal port control is classified as a Snare from Panama's perspective (powerless, trapped), as they perceive limited alternatives and high extraction of sovereignty and economic potential. The global powers (institutional beneficiary) view it as a Rope, a vital coordination mechanism for global trade. The Analytical Observer classifies it as a Tangled Rope because it possesses both a genuine coordination function (facilitating international trade) and clear asymmetric extraction, backed by geopolitical power (active enforcement). The high extraction (0.65) and suppression (0.70) scores reflect the strategic importance and coercive potential of controlling this chokepoint. The low theater ratio (0.20) indicates this is an active, functional system, not a performative or inertial one.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. It prevents the system from mislabeling the genuine coordination benefits (smoother global shipping) as the constraint's sole purpose, which would incorrectly classify it as a pure Rope. It correctly identifies that this coordination is coupled with significant, asymmetric extraction from Panama and requires active geopolitical maneuvering (enforcement) to maintain, distinguishing it from a purely voluntary or equitable system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_panama_canal_ports,
    'To what extent can Panama leverage the competition between foreign powers to retain sovereignty versus being exploited by it?',
    'Longitudinal analysis of port management contracts, revenue sharing agreements, and instances of political pressure or intervention.',
    'High leverage: Panama negotiates favorable terms, becoming a Scaffold for its own development. Low leverage: Foreign powers dominate, solidifying the constraint as a Snare.',
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
% Required for high-extraction constraints (base_extractiveness > 0.46).
% The data models the intensification of geopolitical competition over the interval.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(panama_canal_ports_tr_t0, panama_canal_ports, theater_ratio, 0, 0.10).
narrative_ontology:measurement(panama_canal_ports_tr_t5, panama_canal_ports, theater_ratio, 5, 0.15).
narrative_ontology:measurement(panama_canal_ports_tr_t10, panama_canal_ports, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(panama_canal_ports_ex_t0, panama_canal_ports, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(panama_canal_ports_ex_t5, panama_canal_ports, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(panama_canal_ports_ex_t10, panama_canal_ports, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(panama_canal_ports, global_infrastructure).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(panama_canal_ports, 0.1).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(panama_canal_ports, global_shipping_lanes).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */