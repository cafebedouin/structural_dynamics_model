% ============================================================================
% CONSTRAINT STORY: microwave_weapon_1
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-25
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

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(microwave_weapon_1, tangled_rope).
narrative_ontology:human_readable(microwave_weapon_1, "Chinese Anti-Satellite Microwave Weapon").
narrative_ontology:topic_domain(microwave_weapon_1, "technological/political").

% Binary flags
% narrative_ontology:has_sunset_clause(microwave_weapon_1).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(microwave_weapon_1). % Required for Tangled Rope

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
    (E =< 0.15 -> false ; E >= 0.46). % Ensures it's a high-extraction Snare/Tangled.

test(tangled_rope_requirements_met) :-
    % Verify all three structural properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(microwave_weapon_1),
    narrative_ontology:constraint_beneficiary(microwave_weapon_1, _),
    narrative_ontology:constraint_victim(microwave_weapon_1, _).

:- end_tests(microwave_weapon_1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.6) and suppression (0.7) are high, reflecting the coercive potential of a strategic weapon system. For other nations (powerless, trapped), this is a Snare, as it creates a vulnerability they cannot easily escape. For its developer, China (institutional, mobile), it is a Rope—a tool for national security, strategic deterrence, and enforcing its interests in the space domain.
 *
 * The Analytical Observer classifies this as a Tangled Rope. This is critical because it captures the dual nature of the constraint. It has a genuine (if coercive) coordination function: establishing new rules and power dynamics in space (derived from `constraint_beneficiary`). It also has clear asymmetric extraction, benefiting one party at the expense of others' security (derived from `constraint_victim`). Finally, its strategic value depends on the credible threat of use, satisfying the `requires_active_enforcement` condition. The low theater ratio (0.2) confirms it is a functional, not performative, system.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification is essential here. A simpler model might classify this as a pure Snare, missing the strategic coordination aspect (establishing a coercive order). Conversely, classifying it as a Rope would ignore the high, asymmetric extraction imposed on other nations. Tangled Rope correctly identifies that it is a system of coercive coordination with unequal outcomes, preventing misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_microwave_weapon_1,
    'Will this technology be used primarily for defense (deterrence) or offense (first strike)?',
    'Observation of deployment patterns, declared strategic doctrine, and response to international treaties.',
    'If defensive, it may stabilize a regional power balance; if offensive, it creates global instability and incentivizes an arms race in space.',
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
narrative_ontology:measurement(microwave_weapon_1_tr_t5, microwave_weapon_1, theater_ratio, 5, 0.15).
narrative_ontology:measurement(microwave_weapon_1_tr_t10, microwave_weapon_1, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(microwave_weapon_1_ex_t0, microwave_weapon_1, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(microwave_weapon_1_ex_t5, microwave_weapon_1, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(microwave_weapon_1_ex_t10, microwave_weapon_1, base_extractiveness, 10, 0.6).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(microwave_weapon_1, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(microwave_weapon_1, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(microwave_weapon_1, global_gps_reliance).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */