% ============================================================================
% CONSTRAINT STORY: cmr_001
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-26
% ============================================================================

:- module(constraint_cmr_001, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cmr_001
 * human_readable: Critical Minerals Reserve
 * domain: economic
 * * SUMMARY:
 * The US government establishes a $12 billion critical minerals reserve to reduce reliance on China and other nations for strategic resources. This initiative aims to secure domestic supply chains and enhance national security. It represents an attempt to shift from relying on existing global supply chains, perceived as risky, to building domestic capacity, with the goal of mitigating geopolitical risk.
 * * KEY AGENTS:
 * - US Manufacturers: Subject (Powerless)
 * - US Government: Beneficiary (Institutional)
 * - Global Supply Chain Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cmr_001, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cmr_001, 0.40).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cmr_001, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cmr_001, extractiveness, 0.55).
narrative_ontology:constraint_metric(cmr_001, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(cmr_001, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(cmr_001, tangled_rope).
narrative_ontology:human_readable(cmr_001, "Critical Minerals Reserve").

% Binary flags
% narrative_ontology:has_sunset_clause(cmr_001).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cmr_001). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(cmr_001, us_government).
narrative_ontology:constraint_victim(cmr_001, global_mineral_suppliers).

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
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cmr_001, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(cmr_001, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cmr_001_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cmr_001, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cmr_001, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cmr_001, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(cmr_001_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The US manufacturers, as "powerless" subjects, see the minerals reserve as a snare because it limits their options. While it aims to provide them with a secure supply, it potentially introduces new regulations and limits access to cheaper international sources, thereby "trapping" them within the domestic system, at least in the short term. The US government, as the "institutional" beneficiary, views it as a rope, ensuring resource access for national security and strategic advantage. The Analytical Observer sees this as a Tangled Rope because it possesses BOTH a genuine coordination function (securing supply chains) AND asymmetric extraction (potentially harming global suppliers and increasing costs for some domestic actors). The Perspectival Gap arises from the differing power dynamics and exit options. The manufacturers have limited exit options, while the government has the power to shape the market to its advantage.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling this coordination as pure extraction by acknowledging the genuine coordination function in securing mineral supplies and national security. However, it also recognizes the potential for asymmetric extraction by disadvantaging foreign suppliers or certain domestic actors. This avoids the oversimplification of viewing it solely as a snare or solely as a rope. The active enforcement (requires_active_enforcement) aspect reflects the government's role in maintaining this system through policy and funding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cmr_001,
    'Is the long-term effect of the reserve to increase or decrease the competitiveness of US manufacturers?',
    'A longitudinal study of US manufacturers' cost structure and market share over the next decade.',
    'If competitiveness increases, it will solidify the rope classification. If it decreases, the snare classification will be reinforced.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cmr_001, 0, 10).

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
narrative_ontology:measurement(cmr_001_tr_t0, cmr_001, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cmr_001_tr_t5, cmr_001, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cmr_001_tr_t10, cmr_001, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cmr_001_ex_t0, cmr_001, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cmr_001_ex_t5, cmr_001, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cmr_001_ex_t10, cmr_001, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */