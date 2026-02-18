% ============================================================================
% CONSTRAINT STORY: CG_IsraelGaza_20231012
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_cg_israelgaza_20231012, []).

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
 * * constraint_id: cg_israelgaza_20231012
 * human_readable: Israeli Blockade of Gaza Strip
 * domain: political
 * * SUMMARY:
 * The Israeli blockade of the Gaza Strip, implemented since 2007, restricts the movement of people and goods in and out of Gaza. This has severe humanitarian consequences, limiting access to essential resources and hindering economic development. The blockade is justified by Israel as a security measure to prevent the entry of weapons and materials that could be used for attacks.
 * * KEY AGENTS:
 * - Gazan Civilians: Subject (Powerless)
 * - Israeli Government: Beneficiary (Institutional)
 * - International Observers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cg_israelgaza_20231012, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cg_israelgaza_20231012, 0.80).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cg_israelgaza_20231012, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cg_israelgaza_20231012, extractiveness, 0.65).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cg_israelgaza_20231012, tangled_rope).
narrative_ontology:human_readable(cg_israelgaza_20231012, "Israeli Blockade of Gaza Strip").
narrative_ontology:topic_domain(cg_israelgaza_20231012, "political").

% Binary flags
% narrative_ontology:has_sunset_clause(cg_israelgaza_20231012).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cg_israelgaza_20231012). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cg_israelgaza_20231012, israeli_government).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, gazan_civilians).

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
constraint_indexing:constraint_classification(cg_israelgaza_20231012, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(cg_israelgaza_20231012, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(cg_israelgaza_20231012).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cg_israelgaza_20231012_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cg_israelgaza_20231012, ExtMetricName, E),
    (E =< 0.15 -> false ; E >= 0.46). % Ensures it's a high-extraction Snare/Tangled.

test(tangled_rope_conditions_met) :-
    % Verify that all structural conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(cg_israelgaza_20231012),
    narrative_ontology:constraint_beneficiary(cg_israelgaza_20231012, _),
    narrative_ontology:constraint_victim(cg_israelgaza_20231012, _).

:- end_tests(cg_israelgaza_20231012_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high extractiveness (0.65) and suppression (0.80) scores reflect the severe impact the blockade has on the lives and economic opportunities of Gazan civilians. The perspectival gap is stark: the Israeli government, as the institutional beneficiary, views the blockade as a necessary security coordination mechanism (Rope), while Gazans experience it as a coercive trap (Snare). The analytical observer classifies it as a Tangled Rope because it possesses both a genuine (from the beneficiary's perspective) coordination function (security) and a clear, asymmetric extractive effect on a distinct victim population. The low theater ratio (0.20) indicates this is an actively enforced and functional system, not a vestigial one.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. It prevents the system from mislabeling the blockade as pure extraction (Snare), which would ignore the stated security rationale that underpins its existence and institutional support. By acknowledging both the coordination claim and the extractive reality, the Tangled Rope model provides a more complete and stable analysis. The `requires_active_enforcement` fact underscores the continuous military and logistical effort needed to maintain the constraint, a hallmark of both Snares and Tangled Ropes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cg_israelgaza_20231012,
    'To what extent does the blockade effectively prevent weapons from entering Gaza versus primarily impacting civilian life?',
    'Declassified intelligence reports on intercepted shipments vs. UN economic and humanitarian impact assessments.',
    'If highly effective at stopping weapons, the coordination aspect is strengthened. If primarily impacting civilians with minimal effect on military capabilities, the Snare classification becomes more dominant.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cg_israelgaza_20231012, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% The interval models the period from 2007 to the present.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cg_israelgaza_20231012_tr_t0, cg_israelgaza_20231012, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cg_israelgaza_20231012_tr_t5, cg_israelgaza_20231012, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cg_israelgaza_20231012_tr_t10, cg_israelgaza_20231012, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cg_israelgaza_20231012_ex_t0, cg_israelgaza_20231012, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cg_israelgaza_20231012_ex_t5, cg_israelgaza_20231012, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cg_israelgaza_20231012_ex_t10, cg_israelgaza_20231012, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(cg_israelgaza_20231012, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(cg_israelgaza_20231012, 0.1).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(cg_israelgaza_20231012, international_aid_delivery).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */