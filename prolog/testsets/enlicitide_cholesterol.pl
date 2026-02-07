% ============================================================================
% CONSTRAINT STORY: enlicitide_cholesterol
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_enlicitide_cholesterol, []).

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
 * * constraint_id: enlicitide_cholesterol
 * human_readable: Pharmaceutical Regulation of Cholesterol
 * domain: technological
 * * SUMMARY:
 * The availability and regulation of the experimental drug enlicitide, which significantly lowers "bad" cholesterol, presents a constraint.  Access to this medication is controlled by pharmaceutical companies and regulatory bodies, impacting patient access and choice. The constraint lies in the controlled access and potential cost barriers, affecting patients' ability to manage their cholesterol effectively.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Pharmaceutical Company: Beneficiary (Institutional)
 * - Healthcare System: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(enlicitide_cholesterol, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(enlicitide_cholesterol, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(enlicitide_cholesterol, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(enlicitide_cholesterol, extractiveness, 0.6).
narrative_ontology:constraint_metric(enlicitide_cholesterol, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(enlicitide_cholesterol, theater_ratio, 0.1).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(enlicitide_cholesterol, pharmaceutical_company).
narrative_ontology:constraint_victim(enlicitide_cholesterol, patients).

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
constraint_indexing:constraint_classification(enlicitide_cholesterol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(enlicitide_cholesterol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(enlicitide_cholesterol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(enlicitide_cholesterol_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(enlicitide_cholesterol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(enlicitide_cholesterol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(enlicitide_cholesterol, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(enlicitide_cholesterol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The powerless patient perceives the constraint as a Snare due to limited access and high costs associated with a potentially life-saving medication. The pharmaceutical company, from an institutional perspective, sees it as a Rope, representing a necessary process for drug development and regulation, ensuring safety and efficacy while also recouping investment. The analytical observer sees it as a tangled rope because it has both a coordination function and extracts value from patients.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the regulation as pure extraction by acknowledging the essential coordination function of pharmaceutical regulation in ensuring drug safety and efficacy, even though it inevitably leads to extraction through pricing and access controls.  Without this perspective, it would be easy to label this a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_enlicitide_cholesterol,
    'Will alternative cholesterol-lowering treatments become more readily available and affordable?',
    'Longitudinal studies of healthcare access and pricing trends.',
    'If True: Constraint weakens, becomes less of a Snare. If False: Constraint strengthens, remains a Snare or potentially becomes a Piton if the drug becomes obsolete but regulations persist.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(enlicitide_cholesterol, 0, 10).

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
narrative_ontology:measurement(enlicitide_cholesterol_tr_t0, enlicitide_cholesterol, theater_ratio, 0, 0.05).
narrative_ontology:measurement(enlicitide_cholesterol_tr_t5, enlicitide_cholesterol, theater_ratio, 5, 0.08).
narrative_ontology:measurement(enlicitide_cholesterol_tr_t10, enlicitide_cholesterol, theater_ratio, 10, 0.1).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(enlicitide_cholesterol_ex_t0, enlicitide_cholesterol, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(enlicitide_cholesterol_ex_t5, enlicitide_cholesterol, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(enlicitide_cholesterol_ex_t10, enlicitide_cholesterol, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */