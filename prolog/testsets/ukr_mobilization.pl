% ============================================================================
% CONSTRAINT STORY: ukr_mobilization
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_ukr_mobilization, []).

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
 * * constraint_id: ukr_mobilization
 * human_readable: Ukrainian Mobilization Law
 * domain: political
 * * SUMMARY:
 * The Ukrainian government is attempting to mobilize citizens to fight against the Russian invasion. This involves restrictions on movement, compulsory service, and potential penalties for evasion. The law aims to bolster defenses but faces public resistance and concerns about human rights.
 * * KEY AGENTS:
 * - Ukrainian Citizens: Subject (Powerless)
 * - Ukrainian Government: Beneficiary (Institutional)
 * - International Observers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ukr_mobilization, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(ukr_mobilization, 0.75).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(ukr_mobilization, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(ukr_mobilization, extractiveness, 0.65).
narrative_ontology:constraint_metric(ukr_mobilization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ukr_mobilization, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(ukr_mobilization, tangled_rope).
narrative_ontology:human_readable(ukr_mobilization, "Ukrainian Mobilization Law").
narrative_ontology:topic_domain(ukr_mobilization, "political").

% Binary flags
domain_priors:requires_active_enforcement(ukr_mobilization). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(ukr_mobilization, ukrainian_government).
narrative_ontology:constraint_victim(ukr_mobilization, ukrainian_citizens).

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
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(ukr_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(ukr_mobilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukr_mobilization_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ukr_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukr_mobilization, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ukr_mobilization, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction Snare/Tangled.

:- end_tests(ukr_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Ukrainian Mobilization Law is classified as a Snare from the perspective of Ukrainian citizens, as they are forced into military service with limited options.
 * The Ukrainian government views it as a Rope, essential for coordinating the war effort.
 * From an analytical perspective, it's a Tangled Rope because it serves a coordination function (defense) but also involves significant extraction and coercion. The base extractiveness is high (0.65) due to the severity of the obligations. The suppression score (0.75) reflects the lack of alternatives for those being mobilized.
 *
 * The Perspectival Gap arises because the government prioritizes national defense, while citizens may prioritize personal safety and freedom.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the mobilization as pure extraction (Snare). The government aims to defend the country, a coordination function. However, it also extracts labor and freedom from its citizens, which is asymmetric. The `requires_active_enforcement` flag acknowledges the coercion involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ukr_mobilization,
    'Will the mobilization law ultimately strengthen or weaken Ukrainian society in the long run?',
    'Longitudinal study of social and economic impacts post-conflict.',
    'If strengthens: Increased national resilience; If weakens: Demographic and economic decline.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ukr_mobilization, 0, 10).

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
narrative_ontology:measurement(ukr_mobilization_tr_t0, ukr_mobilization, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ukr_mobilization_tr_t5, ukr_mobilization, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ukr_mobilization_tr_t10, ukr_mobilization, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ukr_mobilization_ex_t0, ukr_mobilization, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ukr_mobilization_ex_t5, ukr_mobilization, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ukr_mobilization_ex_t10, ukr_mobilization, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */