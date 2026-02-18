% ============================================================================
% CONSTRAINT STORY: india_semi_mission
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_india_semi_mission, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: india_semi_mission
 * human_readable: India Semiconductor Mission 2.0
 * domain: economic
 * * SUMMARY:
 * The India Semiconductor Mission 2.0 aims to boost the domestic chip industry through financial incentives, infrastructure development, and skill training. This constraint involves significant government investment and regulation, creating potential for both coordination and extraction.
 * * KEY AGENTS:
 * - Local Chip Designers: Subject (Powerless/Organized)
 * - Government of India: Beneficiary (Institutional)
 * - Global Semiconductor Market: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(india_semi_mission, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(india_semi_mission, 0.50).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(india_semi_mission, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(india_semi_mission, extractiveness, 0.65).
narrative_ontology:constraint_metric(india_semi_mission, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(india_semi_mission, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(india_semi_mission, tangled_rope).
narrative_ontology:human_readable(india_semi_mission, "India Semiconductor Mission 2.0").
narrative_ontology:topic_domain(india_semi_mission, "economic").

% Binary flags
domain_priors:requires_active_enforcement(india_semi_mission). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(india_semi_mission, indian_economy).
narrative_ontology:constraint_victim(india_semi_mission, local_chip_designers).

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
constraint_indexing:constraint_classification(india_semi_mission, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(india_semi_mission, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_semi_mission_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(india_semi_mission, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_semi_mission, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(india_semi_mission, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

test(tangled_rope_properties) :-
  narrative_ontology:constraint_beneficiary(india_semi_mission, _),
  narrative_ontology:constraint_victim(india_semi_mission, _),
  domain_priors:requires_active_enforcement(india_semi_mission).

:- end_tests(india_semi_mission_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high base extractiveness reflects the potential for rent-seeking and regulatory capture within the semiconductor industry. The powerless chip designers may feel trapped by regulations favoring large corporations, while the government views the mission as a vital coordinating effort. The analytical perspective identifies it as a Tangled Rope due to the coordination function (boosting the economy) and asymmetric extraction (potentially harming small designers). The suppression score reflects the barriers to entry and the potential for the mission to stifle innovation by smaller players. The perspectival gap arises from the unequal distribution of benefits and burdens.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the semiconductor mission as pure extraction (Snare). While there's potential for exploitation, the mission also aims to provide a coordination function by attracting investment and developing the domestic semiconductor industry. A Snare classification would overlook the intended coordination benefits and exaggerate the negative impacts. The beneficiaries and victims are explicitly labeled to satisfy the coordination and asymmetric extraction conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_india_semi_mission,
    'Will the incentives primarily benefit large, established firms, or will they foster innovation among smaller players?',
    'Tracking the distribution of subsidies and the number of new semiconductor startups created',
    'If large firms: rent-seeking, low innovation. If small players: more dynamism and innovation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(india_semi_mission, 0, 10).

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
narrative_ontology:measurement(india_semi_mission_tr_t0, india_semi_mission, theater_ratio, 0, 0.10).
narrative_ontology:measurement(india_semi_mission_tr_t5, india_semi_mission, theater_ratio, 5, 0.20).
narrative_ontology:measurement(india_semi_mission_tr_t10, india_semi_mission, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(india_semi_mission_ex_t0, india_semi_mission, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(india_semi_mission_ex_t5, india_semi_mission, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(india_semi_mission_ex_t10, india_semi_mission, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */